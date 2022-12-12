!> @file
!> @brief NUOPC based ESMF interface module for multi-grid wave model.
!>
!> @author T. J. Campell
!> @author J. Meixner
!> @author A. J. van der Westhuysen
!> @date   09-Aug-2017
!>

#include "w3macros.h"
!/
!/ ------------------------------------------------------------------- /
!/ Macros for ESMF logging
!/
#define FILENAME "wmesmfmd.ftn"
#define CONTEXT  line=__LINE__,file=FILENAME,method=METHOD
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT
!/
!/ ------------------------------------------------------------------- /
!/ Define real kind for data passed through ESMF interface
!/
#if defined(ESMF_R8)
#define ESMF_KIND_RX ESMF_KIND_R8
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R8
#else
#define ESMF_KIND_RX ESMF_KIND_R4
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R4
#endif
!/
!/ ------------------------------------------------------------------- /
!/ Macro for enabling using W3OUTG to calculate export fields
!/
#define USE_W3OUTG_FOR_EXPORT___disabled
!/
!/ ------------------------------------------------------------------- /
!/ Macros for enabling test output
!/
#define TEST_WMESMFMD___disabled
#define TEST_WMESMFMD_GETIMPORT___disabled
#define TEST_WMESMFMD_SETEXPORT___disabled
#define TEST_WMESMFMD_CREATEIMPGRID___disabled
#define TEST_WMESMFMD_CREATEEXPGRID___disabled
#define TEST_WMESMFMD_SETUPIMPBMSK___disabled
#define TEST_WMESMFMD_SETUPIMPMMSK___disabled
#define TEST_WMESMFMD_CHARNK___disabled
#define TEST_WMESMFMD_ROUGHL___disabled
#define TEST_WMESMFMD_BOTCUR___disabled
#define TEST_WMESMFMD_RADSTR2D___disabled
#define TEST_WMESMFMD_STOKES3D___disabled
#define TEST_WMESMFMD_PSTOKES___disabled
#define TEST_WMESMFMD_READFROMFILE___disabled

!/ ------------------------------------------------------------------- /
!>
!> @brief National Unified Prediction Capability (NUOPC) based
!>  Earth System Modeling Framework (ESMF) interface module for
!>  multi-grid wave model.
!>
!> @details All module variables and types are scoped private by default.
!>  The private module variables and types are not listed in this section.
!>
!> @author T. J. Campell
!> @author J. Meixner
!> @author A. J. van der Westhuysen
!> @date   09-Aug-2017
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>  National Oceanic and Atmospheric Administration.  All rights
!>  reserved.  WAVEWATCH III is a trademark of the NWS.
!>  No unauthorized use without permission.
!>
module WMESMFMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |        T. J. Campbell, NRL        |
  !/                  |         J. Meixner, NCEP          |
  !/                  |     A. J. van der Westhuysen      |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         09-Aug-2017 |
  !/                  +-----------------------------------+
  !/
  !/    20-Jan-2017 : Origination.                        ( version 6.02 )
  !/    09-Aug-2017 : Add ocean forcing export fields     ( version 6.03 )
  !/    28-Feb-2018 : Modifications for unstruc meshes    ( version 6.06 )
  !/
  !/    Copyright 2009-2014 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     National Unified Prediction Capability (NUOPC) based
  !     Earth System Modeling Framework (ESMF) interface module for
  !     multi-grid wave model.
  !
  !  2. Variables and types :
  !
  !     All module variables and types are scoped private by default.
  !     The private module variables and types are not listed in this section.
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  3. Subroutines and functions :
  !
  !     All module subroutines and functions are scoped private by default.
  !
  !      Name             Type   Scope    Description
  !     -----------------------------------------------------------------
  !      SetServices      Subr.  Public   Wave model ESMF Set Services
  !     -----------------------------------------------------------------
  !      InitializeP0     Subr.  Private  NUOPC/ESMF Initialize phase 0
  !      InitializeP1     Subr.  Private  NUOPC/ESMF Initialize phase 1
  !      InitializeP3     Subr.  Private  NUOPC/ESMF Initialize phase 3
  !      Finalize         Subr.  Private  NUOPC/ESMF Finalize
  !      DataInitialize   Subr.  Private  NUOPC/ESMF Data Initialize
  !      ModelAdvance     Subr.  Private  NUOPC/ESMF Model Advance
  !      GetImport        Subr.  Private  Get fields from import state
  !      SetExport        Subr.  Private  Set fields from export state
  !      CreateImpGrid    Subr.  Private  Create ESMF grid for import
  !      CreateExpGrid    Subr.  Private  Create ESMF grid for export
  !      CreateImpMesh    Subr.  Private  Create ESMF mesh for import
  !      CreateExpMesh    Subr.  Private  Create ESMF mesh for export
  !      SetupImpBmsk     Subr.  Private  Setup background blending mask
  !      BlendImpField    Subr.  Private  Blend import field with background
  !      SetupImpMmsk     Subr.  Private  Setup merging mask
  !      FieldFill        Subr.  Private  Fill ESMF field
  !      FieldGather      Subr.  Private  Gather ESMF field
  !      FieldIndex       Func.  Private  Return field index
  !      PrintTimers      Subr.  Private  Print wallclock timers
  !      CalcDecomp       Subr.  Private  Calculate a 2D processor layout
  !      GetEnvValue      Subr.  Private  Get value of env. variable
  !      GetZlevels       Subr.  Private  Get z-levels from file for SDC
  !      CalcCharnk       Subr.  Private  Calculate Charnock for export
  !      CalcRoughl       Subr.  Private  Calculate roughness length for export
  !      CalcBotcur       Subr.  Private  Calculate wave-bottom currents for export
  !      CalcRadstr2D     Subr.  Private  Calculate 2D radiation stresses for export
  !      CalcStokes3D     Subr.  Private  Calculate 3D Stokes drift current for export
  !      CalcPStokes      Subr.  Private  Calculate partitioned Stokes drift for export
  !      ReadFromFile     Subr.  Private  Read input file
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !     See subroutine documentation.
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !     See subroutine documentation.
  !
  !     !/MPI   Switch for enabling Message Passing Interface API
  !     !/SHRD  Switch for shared memory architecture
  !     !/DIST  Switch for distributed memory architecture
  !     !/ST3   WAM 4+ input and dissipation.
  !     !/ST4   Ardhuin et al. (2009, 2010)
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  !/
  !/ Use associated modules
  !/
  ! --- ESMF Module
  use ESMF

  ! --- NUOPC modules
  use NUOPC
  use NUOPC_Model, parent_SetServices => SetServices

  ! --- WW3 modules
  use CONSTANTS
  use WMINITMD, only: WMINIT, WMINITNML
  use WMWAVEMD, only: WMWAVE
  use WMFINLMD, only: WMFINL
  use WMMDATMD
  use W3GDATMD
  use W3IDATMD
  use W3ODATMD
  use W3WDATMD
  use W3ADATMD
  use W3TIMEMD
  use WMUPDTMD, only: WMUPD2
  use W3UPDTMD, only: W3UINI
#ifdef W3_ST3
  use W3SRC3MD, only: W3SPR3
#endif
#ifdef W3_ST4
  use W3SRC4MD, only: W3SPR4
#endif
  use W3IOGOMD, only: W3OUTG
#ifdef W3_SCRIP
  use WMSCRPMD, only: get_scrip_info_structured
#endif
  !/
  !/ Specify default data typing
  !/
  implicit none
  !/
  !/ Include MPI definitions
  !/
#ifdef W3_MPI
  include "mpif.h"
#endif
  !/
  !/ Specify default accessibility
  !/
  private
  save
  !/
  !/ Public module methods
  !/
  public SetServices, SetVM
  !/
  !/ Private module parameters
  !/
  ! --- Default Mask Convention for import/export fields
  INTEGER, PARAMETER :: DEFAULT_MASK_WATER =  0 !< DEFAULT_MASK_WATER
  INTEGER, PARAMETER :: DEFAULT_MASK_LAND  =  1 !< DEFAULT_MASK_LAND

  ! --- Miscellaneous
  integer, parameter :: stdo = 6 !< stdo
  type(ESMF_VM) :: vm            !< vm
  integer :: lpet                !< lpet
  integer :: npet                !< npet
  integer :: verbosity           !< verbosity
  logical :: realizeAllExport = .false.           !< realizeAllExport
  integer :: maskValueWater = DEFAULT_MASK_WATER  !< maskValueWater
  integer :: maskValueLand  = DEFAULT_MASK_LAND   !< maskValueLand
  integer              :: nz    !< nz Number of z-levels for SDC
  real(4), allocatable :: zl(:) !< zl Array of z-levels for SDC
  character(256)       :: zlfile = 'none' !< zlfile File containing z-levels for SDC
  character(ESMF_MAXSTR) :: msg           !< msg
  real(ESMF_KIND_RX) :: zeroValue         !< zeroValue
  real(ESMF_KIND_RX) :: missingValue      !< missingValue
  real(ESMF_KIND_RX) :: fillValue         !< fillValue
  !
  ! --- Timing
  integer, parameter :: numwt=10          !< numwt
  character(32) :: wtnam(numwt)           !< wtnam
  integer       :: wtcnt(numwt)           !< wtcnt
  real(8)       :: wtime(numwt)           !< wtime
  !
  ! --- Import fields
  type(ESMF_ArraySpec)          :: impArraySpec2D        !< impArraySpec2D
  type(ESMF_StaggerLoc)         :: impStaggerLoc         !< impStaggerLoc
  type(ESMF_Index_Flag)         :: impIndexFlag         !< impIndexFlag
  type(ESMF_Grid)               :: impGrid         !< impGrid
  integer                       :: impGridID         !< impGridID
  logical                       :: impGridIsLocal         !< impGridIsLocal
  integer, parameter            :: impHaloWidth = 3         !< impHaloWidth
  integer                       :: impHaloLWidth(2)         !< impHaloLWidth
  integer                       :: impHaloUWidth(2)         !< impHaloUWidth
  type(ESMF_RouteHandle)        :: impHaloRH         !< impHaloRH
  type(ESMF_Field)              :: impMask         !< impMask
  logical                       :: noActiveImpFields         !< noActiveImpFields
  integer                       :: numImpFields         !< numImpFields
  character(64), allocatable    :: impFieldName(:)         !< impFieldName
  character(128), allocatable   :: impFieldStdName(:)         !< impFieldStdName
  logical, allocatable          :: impFieldInitRqrd(:)         !< impFieldInitRqrd
  logical, allocatable          :: impFieldActive(:)         !< impFieldActive
  type(ESMF_Field), allocatable :: impField(:)         !< impField
  !
  ! --- Background import fields
  character(10), allocatable    :: mbgFieldName(:)     !< mbgFieldName
  character(128), allocatable   :: mbgFieldStdName(:)  !< mbgFieldStdName
  logical, allocatable          :: mbgFieldActive(:)         !< mbgFieldActive
  type(ESMF_Field), allocatable :: mbgField(:)         !< mbgField
  type(ESMF_Field), allocatable :: bmskField(:)         !< bmskField
  !
  ! --- Unstructured import meshes
  type(ESMF_Mesh)               :: impMesh         !< impMesh
  !      integer                       :: impMeshID         !< impMeshID
  !      logical                       :: impMeshIsLocal         !< impMeshIsLocal
  !
  ! --- Export fields
  type(ESMF_ArraySpec)          :: expArraySpec2D         !< expArraySpec2D
  type(ESMF_ArraySpec)          :: expArraySpec3D         !< expArraySpec3D
  type(ESMF_StaggerLoc)         :: expStaggerLoc         !< expStaggerLoc
  type(ESMF_Index_Flag)         :: expIndexFlag         !< expIndexFlag
  type(ESMF_Grid)               :: expGrid         !< expGrid
  integer                       :: expGridID = 1         !< expGridID
  logical                       :: expGridIsLocal         !< expGridIsLocal
  integer, parameter            :: expHaloWidth = 3         !< expHaloWidth
  integer                       :: expHaloLWidth(2)         !< expHaloLWidth
  integer                       :: expHaloUWidth(2)         !< expHaloUWidth
  type(ESMF_RouteHandle)        :: expHaloRH         !< expHaloRH
  type(ESMF_Field)              :: expMask         !< expMask
  logical                       :: noActiveExpFields         !< noActiveExpFields
  integer                       :: numExpFields         !< numExpFields
  character(64), allocatable    :: expFieldName(:)         !< expFieldName
  character(128), allocatable   :: expFieldStdName(:)         !< expFieldStdName
  integer, allocatable          :: expFieldDim(:)         !< expFieldDim
  logical, allocatable          :: expFieldActive(:)         !< expFieldActive
  type(ESMF_Field), allocatable :: expField(:)         !< expField
  !
  ! --- Unstructured export meshes
  type(ESMF_Mesh)               :: expMesh         !< expMesh
  integer                       :: expMeshID         !<  expMeshID
  logical                       :: expMeshIsLocal         !< expMeshIsLocal
  !
  ! --- Native field stuff
  type(ESMF_ArraySpec)  :: natArraySpec1D         !< natArraySpec1D
  type(ESMF_ArraySpec)  :: natArraySpec2D         !< natArraySpec2D
  type(ESMF_ArraySpec)  :: natArraySpec3D         !< natArraySpec3D
  type(ESMF_StaggerLoc) :: natStaggerLoc         !< natStaggerLoc
  type(ESMF_Index_Flag) :: natIndexFlag         !< natIndexFlag
  type(ESMF_Grid)       :: natGrid         !< natGrid
  integer               :: natGridID         !< natGridID
  logical               :: natGridIsLocal         !< natGridIsLocal
  type(ESMF_RouteHandle):: n2eRH         !< n2eRH
  !
  ! --- Mediator
  logical        :: med_present = .false.         !< med_present
  character(256) :: flds_scalar_name = ''         !< flds_scalar_name
  integer        :: flds_scalar_num = 0         !< flds_scalar_num
  ! flds_scalar_index_nx and flds_scalar_index_nx are domain
  ! metadata that allows CMEPS to convert a mesh back to 2d
  ! space for mediator restart and history outputs
  integer        :: flds_scalar_index_nx = 0         !< flds_scalar_index_nx
  integer        :: flds_scalar_index_ny = 0         !< flds_scalar_index_ny
  ! --- Memory Profiling
  logical        :: profile_memory = .false.         !< profile_memory
  !
  ! --- Coupling stuff for non completely overlapped domains
  logical                       :: merge_import = .false.         !< merge_import
  logical, allocatable          :: mmskCreated(:)         !< mmskCreated
  type(ESMF_Field), allocatable :: mmskField(:)         !< mmskField
  type(ESMF_Field), allocatable :: mdtField(:)         !< mdtField
  !/
  !/ ------------------------------------------------------------------- /

contains

  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "SetServices"
  !>
  !> @brief Wave model ESMF set services.
  !>
  !> @param      gcomp Gridded component.
  !> @param[out] rc    Return code.
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  subroutine SetServices ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Wave model ESMF set services.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp   Type   I/O Gridded component
    !       rc      Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name            Type   Module    Description
    !     ----------------------------------------------------------------
    !      InitializeP0    Subr.  WMESMFMD  Wave model NUOPC/ESMF Initialize phase 0
    !      InitializeP1    Subr.  WMESMFMD  Wave model NUOPC/ESMF Initialize phase 1
    !      InitializeP3    Subr.  WMESMFMD  Wave model NUOPC/ESMF Initialize phase 3
    !      Finalize        Subr.  WMESMFMD  Wave model NUOPC/ESMF Finalize
    !      DataInitialize  Subr.  WMESMFMD  Wave model NUOPC/ESMF Data Initialize
    !      ModelAdvance    Subr.  WMESMFMD  Wave model NUOPC/ESMF Model Advance
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    !NONE
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS

    ! --- Initialize wallclock timers

    wtnam( 1) = 'InitializeP0'
    wtnam( 2) = 'InitializeP1'
    wtnam( 3) = 'InitializeP3'
    wtnam( 4) = 'DataInitialize'
    wtnam( 5) = 'ModelAdvance'
    wtnam( 6) = 'Finalize'
    wtnam( 7) = 'GetImport'
    wtnam( 8) = 'SetExport'
    wtnam( 9) = 'FieldGather'
    wtnam(10) = 'FieldFill'
    wtcnt( :) = 0
    wtime( :) = 0d0
    !
    ! -------------------------------------------------------------------- /
    ! 1.  NUOPC model component will register the generic methods
    !
    call NUOPC_CompDerive(gcomp, parent_SetServices, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Set model entry points
    !
    ! --- Initialize - phase 0 (requires use of ESMF method)

    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    ! --- Set entry points for initialize methods

    ! >= IPDv03 supports satisfying inter-model data dependencies and
    ! the transfer of ESMF Grid & Mesh objects between Model and/or
    ! Mediator components during initialization
    ! IPDv03p1: advertise import & export fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    ! IPDv03p2: unspecified by NUOPC -- not required
    ! IPDv03p3: realize import & export fields
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
         phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    ! IPDv03p4: relevant for TransferActionGeomObject=="accept"
    ! IPDv03p5: relevant for TransferActionGeomObject=="accept"
    ! IPDv03p6: check compatibility of fields connected status
    ! IPDv03p7: handle field data initialization

    !
    ! -------------------------------------------------------------------- /
    ! 3.  Register specializing methods
    !
    ! --- Model initialize export data method

    call NUOPC_CompSpecialize(gcomp, specLabel=label_DataInitialize, &
         specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    ! --- Model checkImport method (overriding default)

    call ESMF_MethodRemove(gcomp, label_CheckImport, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call NUOPC_CompSpecialize(gcomp, specLabel=label_CheckImport, &
         specRoutine=NUOPC_NoOp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    ! --- Model advance method

    call NUOPC_CompSpecialize(gcomp, specLabel=label_Advance, &
         specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    ! --- Model finalize method

    call NUOPC_CompSpecialize(gcomp, specLabel=label_Finalize, &
         specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
    rc = ESMF_SUCCESS
    !/
    !/ End of SetServices ------------------------------------------------ /
    !/
  end subroutine SetServices
  !/ ------------------------------------------------------------------- /

#undef METHOD
#define METHOD "InitializeP0"
  !>
  !> @brief  Initialize wave model (phase 0).
  !>
  !> @details Define the NUOPC Initialize Phase Mapping.
  !>
  !> @param gcomp    Gridded component.
  !> @param impState Import state.
  !> @param expState Export state.
  !> @param extClock External clock.
  !> @param[out] rc  Return code.
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  subroutine InitializeP0 ( gcomp, impState, expState, extClock, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Initialize wave model (phase 0)
    !     * Define the NUOPC Initialize Phase Mapping
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       impState  Type   I/O Import state
    !       expState  Type   I/O Export state
    !       extClock  Type   I   External clock
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State)    :: impState
    type(ESMF_State)    :: expState
    type(ESMF_Clock)    :: extClock
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    character(ESMF_MAXSTR) :: valueString
    integer, parameter :: iwt=1
    real(8) :: wstime, wftime
    logical :: isPresent, isSet
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! Determine verbosity
    !
    call NUOPC_CompAttributeGet(gcomp, name='Verbosity', &
         value=valueString, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    verbosity = ESMF_UtilString2Int( valueString, &
         specialStringList=(/'high','max '/), &
         specialValueList=(/    255,   255/), rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered InitializeP0', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! Define initialization phases
    ! * switch to IPDv03 by filtering all other phaseMap entries
    !
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
         acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! Check if coupled with CMEPS mediator or not
    !
    call NUOPC_CompAttributeGet(gcomp, name="mediator_present", &
         value=valueString, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if (isPresent .and. isSet) then
      read(valueString,*) med_present
      call ESMF_LogWrite(trim(cname)//': mediator_present = '// &
           trim(valueString), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    end if
    !
    ! -------------------------------------------------------------------- /
    ! Set memory profiling
    !
    call NUOPC_CompAttributeGet(gcomp, name="ProfileMemory", &
         value=valueString, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if (isPresent .and. isSet) then
      read(valueString,*) profile_memory
      call ESMF_LogWrite(trim(cname)//': profile_memory = '// &
           trim(valueString), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    end if

    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving InitializeP0', ESMF_LOGMSG_INFO)
    !/
    !/ End of InitializeP0 ----------------------------------------------- /
    !/
  end subroutine InitializeP0
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "InitializeP1"
  !>
  !> @brief  Initialize wave model (phase 1).
  !>
  !> @details Advertise fields in import and export states.
  !>
  !> @param gcomp    Gridded component.
  !> @param impState Import state.
  !> @param expState Export state.
  !> @param extClock External clock.
  !> @param[out] rc  Return code.
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine InitializeP1 ( gcomp, impState, expState, extClock, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/    09-Aug-2017 : Add ocean forcing export fields     ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Initialize wave model (phase 1)
    !     * Advertise fields in import and export states.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       impState  Type   I/O Import state
    !       expState  Type   I/O Export state
    !       extClock  Type   I   External clock
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name            Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMINIT          Subr. WMINITMD Wave model initialization
    !      WMINITNML       Subr. WMINITMD Wave model initialization
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     ----------------------------------------------------------------
    !      1.  Initialization necessary for driver
    !        a General I/O: (implicit in WMMDATMD)
    !        b MPI environment
    !        c Identifying output to "screen" unit
    !      2.  Initialization of all wave models / grids
    !      3.  Advertise import fields
    !      4.  Advertise export fields
    !     ----------------------------------------------------------------
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State)    :: impState
    type(ESMF_State)    :: expState
    type(ESMF_Clock)    :: extClock
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    integer, parameter :: iwt=2
    real(8) :: wstime, wftime
    integer :: idsi, idso, idss, idst, idse
    integer :: mpiComm = -99
    logical :: configIsPresent
    type(ESMF_Config) :: config
    character(ESMF_MAXSTR) :: wrkdir = '.'
    character(ESMF_MAXSTR) :: preamb = '.'
    character(ESMF_MAXSTR) :: ifname = 'ww3_multi.inp'
    logical :: lsep_ss = .true.
    logical :: lsep_st = .true.
    logical :: lsep_se = .true.
    character(ESMF_MAXSTR) :: attstr
    integer(ESMF_KIND_I4) :: yy,mm,dd,h,m,s
    type(ESMF_Time) :: ttmp, cttmp
    type(ESMF_TimeInterval) :: tstep, etstep
    integer :: i, j, n, istep, imod, jmod
    integer, allocatable :: cplmap(:,:)
    logical :: includeObg, includeAbg, includeIbg
    character(256) :: cvalue, logmsg
    logical :: isPresent, isSet
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered InitializeP1', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! Query mediator specific attributes
    !
    if (med_present) then
      call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", &
           value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (isPresent .and. isSet) then
        flds_scalar_name = trim(cvalue)
        call ESMF_LogWrite(trim(cname)//': flds_scalar_name = '// &
             trim(flds_scalar_name), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      end if

      call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldCount", &
           value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (isPresent .and. isSet) then
        flds_scalar_num = ESMF_UtilString2Int(cvalue, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        if (verbosity.gt.0) then
          write(logmsg,*) flds_scalar_num
          call ESMF_LogWrite(trim(cname)//': flds_scalar_num = '// &
               trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
      end if

      call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNX", &
           value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (isPresent .and. isSet) then
        flds_scalar_index_nx = ESMF_UtilString2Int(cvalue, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        if (verbosity.gt.0) then
          write(logmsg,*) flds_scalar_index_nx
          call ESMF_LogWrite(trim(cname)//': flds_scalar_index_nx = '// &
               trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
      end if

      call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldIdxGridNY", &
           value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (isPresent .and. isSet) then
        flds_scalar_index_ny = ESMF_UtilString2Int(cvalue, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        if (verbosity.gt.0) then
          write(logmsg,*) flds_scalar_index_ny
          call ESMF_LogWrite(trim(cname)//': flds_scalar_index_ny = '// &
               trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
      end if

      call NUOPC_CompAttributeGet(gcomp, name="mask_value_water", &
           value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (isPresent .and. isSet) then
        maskvaluewater = ESMF_UtilString2Int(cvalue, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        if (verbosity.gt.0) then
          write(logmsg,*) maskvaluewater
          call ESMF_LogWrite(trim(cname)//': mask_value_water = '// &
               trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
      end if

      call NUOPC_CompAttributeGet(gcomp, name="mask_value_land", &
           value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (isPresent .and. isSet) then
        maskvalueland = ESMF_UtilString2Int(cvalue, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        if (verbosity.gt.0) then
          write(logmsg,*) maskvalueland
          call ESMF_LogWrite(trim(cname)//': mask_value_land = '// &
               trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
      end if
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Initialization necessary for driver
    !
    ! 1.a Set global flag indicating that model is an ESMF Component
    !
    is_esmf_component = .true.
    zeroValue = real(0,ESMF_KIND_RX)
    missingValue = real(0,ESMF_KIND_RX)
    fillValue = real(9.99e20,ESMF_KIND_RX)
    !
    !
    ! 1.b Get MPI environment from ESMF VM and set WW3 MPI related variables
    !
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_VMGet(vm, petCount=npet, localPet=lpet, &
         mpiCommunicator=mpiComm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    nmproc = npet
    improc = lpet + 1
    nmpscr = 1
    if ( improc .eq. nmpscr ) write (*,900)
    !
    ! 1.c Get background model info
    !
#if defined(COAMPS)
    call ESMF_AttributeGet(gcomp, name="OcnBackground", &
         value=attstr, defaultValue="none", &
         convention="COAMPS", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    includeObg = trim(attstr).eq."model"
    call ESMF_AttributeGet(gcomp, name="AtmBackground", &
         value=attstr, defaultValue="none", &
         convention="COAMPS", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    includeAbg = trim(attstr).eq."model"
    call ESMF_AttributeGet(gcomp, name="IceBackground", &
         value=attstr, defaultValue="none", &
         convention="COAMPS", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    includeIbg = trim(attstr).eq."model"
    call ESMF_AttributeGet(gcomp, name="MissingValue", &
         value=missingValue, defaultValue=real(0,ESMF_KIND_RX), &
         convention="COAMPS", purpose="General", rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#else
    includeObg = .false.
    includeAbg = .false.
    includeIbg = .false.
#endif
    !
    ! 1.d Config input
    !
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (configIsPresent) then
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      ! working directory
      call ESMF_ConfigGetAttribute(config, wrkdir, &
           label=trim(cname)//'_work_dir:', default='.', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      ! I/O options
      call ESMF_ConfigGetAttribute(config, ifname, &
           label=trim(cname)//'_input_file_name:', &
           default='ww3_multi.inp', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_ConfigGetAttribute(config, lsep_ss, &
           label=trim(cname)//'_stdo_output_to_file:', default=.false., rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_ConfigGetAttribute(config, lsep_st, &
           label=trim(cname)//'_test_output_to_file:', default=.false., rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_ConfigGetAttribute(config, lsep_se, &
           label=trim(cname)//'_error_output_to_file:', default=.false., rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      ! export grid id
      call ESMF_ConfigGetAttribute(config, expGridID, &
           label=trim(cname)//'_export_grid_id:', default=1, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      ! realize all export flag
      call ESMF_ConfigGetAttribute(config, realizeAllExport, &
           label=trim(cname)//'_realize_all_export:', default=.false., rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      ! grid mask convention
      call ESMF_ConfigGetAttribute(config, maskValueWater, &
           label='mask_value_water:', default=DEFAULT_MASK_WATER, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_ConfigGetAttribute(config, maskValueLand, &
           label='mask_value_land:', default=DEFAULT_MASK_LAND, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      ! z-level file
      call ESMF_ConfigGetAttribute(config, zlfile, &
           label=trim(cname)//'_zlevel_exp_file:', default='none', rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !     preamb = trim(wrkdir)//'/'
    preamb = trim(preamb)//'/' !TODO: have separate paths for .inp, logs and data?
    !
    ! 1.e Set internal start/stop time from external start/stop time
    !

    call ESMF_ClockGet(extClock, startTime=ttmp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_ClockGet(extClock, currTime=cttmp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! Adjust internal start time to currTime in case of delayed start
    !
    if ( cttmp.gt.ttmp ) ttmp=cttmp
    call ESMF_TimeGet(ttmp, yy=yy,mm=mm,dd=dd,h=h,m=m,s=s, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    stime(1) = 10000*yy + 100*mm + dd
    stime(2) = 10000*h  + 100*m  + s

    call ESMF_ClockGet(extClock, stopTime=ttmp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_TimeGet(ttmp, yy=yy,mm=mm,dd=dd,h=h,m=m,s=s, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    etime(1) = 10000*yy + 100*mm + dd
    etime(2) = 10000*h  + 100*m  + s
    !
    ! 1.f Identify available unit numbers
    ! Each ESMF_UtilIOUnitGet is followed by an OPEN statement for that
    ! unit so that subsequent ESMF_UtilIOUnitGet calls do not return the
    ! the same unit.  After getting all the available unit numbers, close
    ! the units since they will be opened within WMINIT.
    !
    call ESMF_UtilIOUnitGet(idsi); open(unit=idsi, status='scratch');
    call ESMF_UtilIOUnitGet(idso); open(unit=idso, status='scratch');
    call ESMF_UtilIOUnitGet(idss); open(unit=idss, status='scratch');
    call ESMF_UtilIOUnitGet(idst); open(unit=idst, status='scratch');
    call ESMF_UtilIOUnitGet(idse); open(unit=idse, status='scratch');
    close(idsi); close(idso); close(idss); close(idst); close(idse);
    !
    ! 1.g Get merging option for regional coupling that domians does not
    ! overlap complately. This will blend the data coming from forcing with
    ! the data coming from coupling.
    !
    call NUOPC_CompAttributeGet(gcomp, name="merge_import", &
         value=attstr, isPresent=isPresent, isSet=isSet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (isPresent .and. isSet) then
      if (trim(attstr) .eq. '.true.') then
        merge_import = .true.
      end if
    end if
    if (verbosity.gt.0) then
      write(logmsg,'(l)') merge_import
      call ESMF_LogWrite(trim(cname)//': merge_import = '// &
           trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    end if
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Initialization of all wave models / grids
    !
    ! 2.a Call into WMINIT
    !
    if ( .not.lsep_ss ) idss = stdo
    if ( .not.lsep_st ) idst = stdo
    if ( .not.lsep_se ) idse = stdo
    if ( trim(ifname).eq.'ww3_multi.nml' ) then
      call wminitnml ( idsi, idso, idss, idst, idse, trim(ifname), &
           mpicomm, preamb=preamb )
    else
      call wminit ( idsi, idso, idss, idst, idse, trim(ifname), &
           mpicomm, preamb=preamb )
    endif
    !
    ! 2.b Check consistency between internal timestep and external
    !     timestep (coupling interval)
    !
    call ESMF_ClockGet(extClock, timeStep=etstep, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 2.c Trap unsupported CPL input forcing settings
    !
    if ( any(inpmap.lt.0) ) then
      if ( nrgrd.gt.1 ) then
        if ( any(inpmap.eq.-999) ) then
          write (msg,'(a)') 'CPL input forcing defined on a '// &
               'native grid is not supported with multiple model grids'
          if ( improc .eq. nmpscr ) write (idse,'(a)') trim(msg)
          call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
        endif
      endif
      allocate (cplmap(nrgrd,jfirst:8), stat=rc)
      if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
      jmod = minval(inpmap)
      cplmap = inpmap
      where ( inpmap.lt.0 ) cplmap = jmod
      if ( any(inpmap.ne.cplmap) ) then
        write (msg,'(a)') 'All CPL input forcing must be '// &
             'defined on the same grid'
        if ( improc .eq. nmpscr ) write (idse,'(a)') trim(msg)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif
      deallocate (cplmap, stat=rc)
      if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Initialize import field list
    !
    istep_import: do istep = 1, 2

      if ( istep.eq.2 ) then
        allocate ( impFieldName(numImpFields), &
             impFieldStdName(numImpFields), &
             impFieldInitRqrd(numImpFields), &
             impFieldActive(numImpFields), &
             impField(numImpFields), &
             stat=rc )
        if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
        allocate ( mbgFieldName(numImpFields), &
             mbgFieldStdName(numImpFields), &
             mbgFieldActive(numImpFields), &
             mbgField(numImpFields), &
             bmskField(numImpFields), &
             stat=rc )
        if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
        if (merge_import) then
          allocate (mmskCreated(numImpFields))
          allocate (mmskField(numImpFields))
          allocate (mdtField(numImpFields))
          mmskCreated(:) = .false.
        end if
        impFieldActive(:) = .false.
      endif
      i = 0

      i = i + 1
      if ( istep.eq.2 ) then
        j = 1
        impFieldActive(i)   = any(inpmap(:,j).lt.0)
        impFieldName(i)     = 'seahgt'
        impFieldStdName(i)  = 'sea_surface_height_above_sea_level'
        impFieldInitRqrd(i) = .true.
        mbgFieldActive(i)   = impFieldActive(i).and.includeObg
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        j = 2
        impFieldActive(i)   = any(inpmap(:,j).lt.0)
        impFieldName(i)     = 'uucurr'
        impFieldStdName(i)  = 'surface_eastward_sea_water_velocity'
        impFieldInitRqrd(i) = .true.
        mbgFieldActive(i)   = impFieldActive(i).and.includeObg
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        j = 2
        impFieldActive(i)   = any(inpmap(:,j).lt.0)
        impFieldName(i)     = 'vvcurr'
        impFieldStdName(i)  = 'surface_northward_sea_water_velocity'
        impFieldInitRqrd(i) = .true.
        mbgFieldActive(i)   = impFieldActive(i).and.includeObg
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        j = 3
        impFieldActive(i)   = any(inpmap(:,j).lt.0)
        impFieldName(i)     = 'uutrue'
        impFieldStdName(i)  = 'eastward_wind_at_10m_height'
        impFieldInitRqrd(i) = .true.
        mbgFieldActive(i)   = impFieldActive(i).and.includeAbg
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        j = 3
        impFieldActive(i)   = any(inpmap(:,j).lt.0)
        impFieldName(i)     = 'vvtrue'
        impFieldStdName(i)  = 'northward_wind_at_10m_height'
        impFieldInitRqrd(i) = .true.
        mbgFieldActive(i)   = impFieldActive(i).and.includeAbg
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        j = 4
        impFieldActive(i)   = any(inpmap(:,j).lt.0)
        impFieldName(i)     = 'seaice'
        impFieldStdName(i)  = 'sea_ice_concentration'
        impFieldInitRqrd(i) = .true.
        mbgFieldActive(i)   = impFieldActive(i).and.includeIbg
      endif

      numImpFields = i
    enddo istep_import

    noActiveImpFields = all(.not.impFieldActive)

    do i = 1,numImpFields
      mbgFieldName(i) = 'mbg_'//trim(impFieldName(i))
      mbgFieldStdName(i) = 'mbg_'//trim(impFieldStdName(i))
    enddo
    !
    ! -------------------------------------------------------------------- /
    ! 4.  Initialize export field list
    !
    istep_export: do istep = 1, 2

      if ( istep.eq.2 ) then
        allocate ( expFieldName(numExpFields), &
             expFieldStdName(numExpFields), &
             expFieldDim(numExpFields), &
             expFieldActive(numExpFields), &
             expField(numExpFields), &
             stat=rc )
        if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
        expFieldActive(:) = .false.
      endif
      i = 0

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'charno'
        expFieldStdName(i) = 'wave_induced_charnock_parameter'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'z0rlen'
        expFieldStdName(i) = 'wave_z0_roughness_length'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'uscurr'
        expFieldStdName(i) = 'eastward_stokes_drift_current'
        expFieldDim(i)     = 3
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'vscurr'
        expFieldStdName(i) = 'northward_stokes_drift_current'
        expFieldDim(i)     = 3
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'x1pstk'
        expFieldStdName(i) = 'eastward_partitioned_stokes_drift_1'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'y1pstk'
        expFieldStdName(i) = 'northward_partitioned_stokes_drift_1'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'x2pstk'
        expFieldStdName(i) = 'eastward_partitioned_stokes_drift_2'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'y2pstk'
        expFieldStdName(i) = 'northward_partitioned_stokes_drift_2'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'x3pstk'
        expFieldStdName(i) = 'eastward_partitioned_stokes_drift_3'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'y3pstk'
        expFieldStdName(i) = 'northward_partitioned_stokes_drift_3'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'wbcuru'
        expFieldStdName(i) = 'eastward_wave_bottom_current'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'wbcurv'
        expFieldStdName(i) = 'northward_wave_bottom_current'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'wbcurp'
        expFieldStdName(i) = 'wave_bottom_current_period'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'wavsuu'
        expFieldStdName(i) = 'eastward_wave_radiation_stress'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'wavsuv'
        expFieldStdName(i) = 'eastward_northward_wave_radiation_stress'
        expFieldDim(i)     = 2
      endif

      i = i + 1
      if ( istep.eq.2 ) then
        expFieldName(i)    = 'wavsvv'
        expFieldStdName(i) = 'northward_wave_radiation_stress'
        expFieldDim(i)     = 2
      endif

      if (med_present) then
        i = i + 1
        if ( istep.eq.2 ) then
          expFieldName(i)    = trim(flds_scalar_name)
          expFieldStdName(i) = trim(flds_scalar_name)
          expFieldDim(i)     = 1
        endif
      endif

      numExpFields = i
    enddo istep_export

    noActiveExpFields = all(.not.expFieldActive)
    !
    ! -------------------------------------------------------------------- /
    ! 5.  Advertise import fields
    !
    ! 5.a Advertise active import fields
    !
    n = 0
    do i = 1,numImpFields
      if (.not.impFieldActive(i)) cycle
      n = n + 1
      call NUOPC_Advertise(impState, &
           trim(impFieldStdName(i)), name=trim(impFieldName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (.not.mbgFieldActive(i)) cycle
      n = n + 1
      call NUOPC_Advertise(impState, &
           trim(mbgFieldStdName(i)), name=trim(mbgFieldName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    enddo
    !
    ! 5.b Report advertised import fields
    !
    write(msg,'(a,i0,a)') trim(cname)// &
         ': List of advertised import fields(',n,'):'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,a5,a,a10,a3,a)') trim(cname)// &
         ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    n = 0
    do i = 1,numImpFields
      if (.not.impFieldActive(i)) cycle
      n = n + 1
      write(msg,'(a,i5,a,a10,a3,a)') trim(cname)//': ',n, &
           ' ',trim(impFieldName(i)),' ',trim(impFieldStdName(i))
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      if (.not.mbgFieldActive(i)) cycle
      n = n + 1
      write(msg,'(a,i5,a,a10,a3,a)') trim(cname)//': ',n, &
           ' ',trim(mbgFieldName(i)),' ',trim(mbgFieldStdName(i))
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    enddo
    !
    ! -------------------------------------------------------------------- /
    ! 6.  Advertise export fields
    !
    ! 6.a Advertise all export fields
    !
    do i = 1,numExpFields
      call NUOPC_Advertise(expState, &
           trim(expFieldStdName(i)), name=trim(expFieldName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    enddo
    !
    ! 6.b Report advertised export fields
    !
    write(msg,'(a,i0,a)') trim(cname)// &
         ': List of advertised export fields(',numExpFields,'):'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,a5,a,a10,a3,a)') trim(cname)// &
         ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    do i = 1,numExpFields
      write(msg,'(a,i5,a,a10,a3,a)') trim(cname)//': ',i, &
           ' ',trim(expFieldName(i)),' ',trim(expFieldStdName(i))
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    enddo
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving InitializeP1', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! Formats
    !
900 format (/15x,'     *** WAVEWATCH III Multi-grid shell ***    '/ &
         15x,'================================================='/)
    !/
    !/ End of InitializeP1 ----------------------------------------------- /
    !/
  end subroutine InitializeP1
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "InitializeP3"
  !>
  !> @brief Initialize wave model (phase 3).
  !>
  !> @details Realize fields in import and export states.
  !>
  !> @param gcomp    Gridded component.
  !> @param impState Import state.
  !> @param expState Export state.
  !> @param extClock External clock.
  !> @param[out] rc  Return code.
  !>
  !> @author T. J. Campbell
  !> @author A. J. van der Westhuysen
  !> @date 09-Aug-2017
  !>
  subroutine InitializeP3 ( gcomp, impState, expState, extClock, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |     A. J. van der Westhuysen      |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/    09-Aug-2017 : Update 3D export field setup        ( version 6.03 )
    !/    28-Feb-2018 : Modifications for unstruc meshes    ( version 6.06 )
    !/
    !  1. Purpose :
    !
    !     Initialize wave model (phase 3)
    !     * Realize fields in import and export states.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       impState  Type   I/O Import state
    !       expState  Type   I/O Export state
    !       extClock  Type   I   External clock
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name            Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMINIT          Subr. WMINITMD Wave model initialization
    !      WMINITNML       Subr. WMINITMD Wave model initialization
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    type(ESMF_State)    :: impState
    type(ESMF_State)    :: expState
    type(ESMF_Clock)    :: extClock
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    integer, parameter :: iwt=3
    real(8) :: wstime, wftime
    integer :: i1, i2, i3, i, n
    logical :: isConnected
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_Grid) :: grid_scalar
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered InitializeP3', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Realize active import fields
    !
    ! 1.a Create ESMF grid for import fields
    !
    if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
      write(msg,'(a)') trim(cname)// &
           ': Creating import grid for Reg/Curvilinear Mode'
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      call CreateImpGrid( gcomp, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    elseif (GTYPE.eq.UNGTYPE) then
      write(msg,'(a)') trim(cname)// &
           ': Creating import mesh for Unstructured Mode'
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      call CreateImpMesh( gcomp, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! 1.b Create import fields and realize
    !
    n = 0
    do i = 1,numImpFields
      if (.not.impFieldActive(i)) cycle
      n = n + 1
      if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
        impField(i) = ESMF_FieldCreate( impGrid, impArraySpec2D, &
             totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
             staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, &
             name=trim(impFieldName(i)), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call FieldFill( impField(i), zeroValue, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      elseif (GTYPE.eq.UNGTYPE) then
        impField(i) = ESMF_FieldCreate( impMesh, &
             typekind=ESMF_TYPEKIND_RX, name=trim(impFieldName(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call FieldFill( impField(i), zeroValue, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
      call NUOPC_Realize( impState, impField(i), rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
        if (merge_import) then
          mmskField(i) = ESMF_FieldCreate( impGrid, impArraySpec2D, &
               totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
               staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, &
               name='mmsk_'//trim(impFieldName(i)), rc=rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          call FieldFill( mmskField(i), zeroValue, rc=rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          mdtField(i) = ESMF_FieldCreate( impGrid, impArraySpec2D, &
               totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
               staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, &
               name='mdt_'//trim(impFieldName(i)), rc=rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          call FieldFill( mdtField(i), zeroValue, rc=rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
        if (.not.mbgFieldActive(i)) cycle
        n = n + 1
        mbgField(i) = ESMF_FieldCreate( impGrid, impArraySpec2D, &
             totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
             staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, &
             name=trim(mbgFieldName(i)), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call FieldFill( mbgField(i), zeroValue, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call NUOPC_Realize( impState, mbgField(i), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        bmskField(i) = ESMF_FieldCreate( impGrid, impArraySpec2D, &
             totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
             staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, &
             name='bmsk_'//trim(impFieldName(i)), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
    enddo
    !
    ! 1.c Report realized import fields
    !
    write(msg,'(a,i0,a)') trim(cname)// &
         ': List of realized import fields(',n,'):'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,a5,a,a10,a3,a)') trim(cname)// &
         ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    n = 0
    do i = 1,numImpFields
      if (.not.impFieldActive(i)) cycle
      n = n + 1
      write(msg,'(a,i5,a,a10,a3,a)') trim(cname)//': ',n, &
           ' ',trim(impFieldName(i)),' ',trim(impFieldStdName(i))
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      if (.not.mbgFieldActive(i)) cycle
      n = n + 1
      write(msg,'(a,i5,a,a10,a3,a)') trim(cname)//': ',n, &
           ' ',trim(mbgFieldName(i)),' ',trim(mbgFieldStdName(i))
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    enddo
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Realize active export fields
    !
    ! 2.a Set connected export fields as active and remove unconnected
    !     If realizeAllExport, then set all fields as active and realize.
    !
    do i = 1,numExpFields
      isConnected = NUOPC_IsConnected(expState, &
           expFieldName(i), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      expFieldActive(i) = isConnected .or. realizeAllExport
      if (expFieldActive(i)) noActiveExpFields = .false.
      if (.not.expFieldActive(i)) then
        call ESMF_StateRemove(expState, (/expFieldName(i)/), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        write(msg,fmt="(a,l)") trim(cname)//': '//trim(expFieldName(i)), expFieldActive(i)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      endif
    enddo
    !
    ! 2.b Create ESMF grid for export fields
    !
    if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
      write(msg,'(a)') trim(cname)// &
           ': Creating export grid for Reg/Curvilinear Mode'
      call CreateExpGrid( gcomp, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    elseif (GTYPE.eq.UNGTYPE) then
      write(msg,'(a)') trim(cname)// &
           ': Creating export mesh for Unstructured Mode'
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      call CreateExpMesh( gcomp, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! 2.c Create active export fields and realize
    !
    n = 0
    do i = 1,numExpFields
      if (.not.expFieldActive(i)) cycle
      n = n + 1
      if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
        if (trim(expFieldName(i)) == trim(flds_scalar_name)) then
          distgrid = ESMF_DistGridCreate(minIndex=(/1/), maxIndex=(/1/), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          grid_scalar = ESMF_GridCreate(distgrid, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          expField(i) = ESMF_FieldCreate(grid_scalar, typekind=ESMF_TYPEKIND_R8, &
               name=trim(expFieldName(i)), ungriddedLBound=(/1/), &
               ungriddedUBound=(/flds_scalar_num/), gridToFieldMap=(/2/), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        else
          if ( expFieldDim(i).eq.3 ) then
            expField(i) = ESMF_FieldCreate( expGrid, expArraySpec3D, &
                 totalLWidth=expHaloLWidth, totalUWidth=expHaloUWidth, &
                 gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/nz/), &
                 staggerLoc=expStaggerLoc, name=trim(expFieldName(i)), rc=rc )
            if (ESMF_LogFoundError(rc, PASSTHRU)) return
          else
            expField(i) = ESMF_FieldCreate( expGrid, expArraySpec2D, &
                 totalLWidth=expHaloLWidth, totalUWidth=expHaloUWidth, &
                 staggerLoc=expStaggerLoc, name=trim(expFieldName(i)), rc=rc )
            if (ESMF_LogFoundError(rc, PASSTHRU)) return
          endif
          call FieldFill( expField(i), zeroValue, rc=rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        endif
      elseif (GTYPE.eq.UNGTYPE) then
        expField(i) = ESMF_FieldCreate( expMesh, &
             typekind=ESMF_TYPEKIND_RX, name=trim(expFieldName(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call FieldFill( expField(i), zeroValue, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
      call NUOPC_Realize( expState, expField(i), rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    enddo
    !
    ! 2.d Report realized export fields
    !
    write(msg,'(a,i0,a)') trim(cname)// &
         ': List of realized export fields(',n,'):'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,a5,a,a10,a3,a)') trim(cname)// &
         ': ','index',' ','name',' ','standardName'
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    n = 0
    do i = 1,numExpFields
      if (.not.expFieldActive(i)) cycle
      n = n + 1
      write(msg,'(a,i5,a,a10,a3,a)') trim(cname)//': ',n, &
           ' ',trim(expFieldName(i)),' ',trim(expFieldStdName(i))
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    enddo
    !
    ! 2.e Set W3OUTG flags needed for calculating export fields
    !
#ifdef USE_W3OUTG_FOR_EXPORT
    call w3seto ( expGridID, mdse, mdst )

    i1 = FieldIndex( expFieldName, 'uscurr', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( expFieldName, 'vscurr', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) .and. &
         expFieldActive(i2) ) then
      flogr2(6,8) = .true.  !Spectrum of surface Stokes drift
      if ( us3df(1) .le. 0 ) then
        msg = trim(cname)//': Stokes drift export using W3OUTG'// &
             ' requires setting US3D=1 (ww3_grid.inp: OUTS namelist)'
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif
    endif

    i1 = FieldIndex( expFieldName, 'wbcuru', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( expFieldName, 'wbcurv', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i3 = FieldIndex( expFieldName, 'wbcurp', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) .and. &
         expFieldActive(i2) .and. &
         expFieldActive(i3) ) then
      flogr2(7,1) = .true.  !Near bottom rms amplitudes
      flogr2(7,2) = .true.  !Near bottom rms velocities
    endif

    i1 = FieldIndex( expFieldName, 'wavsuu', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( expFieldName, 'wavsuv', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i3 = FieldIndex( expFieldName, 'wavsvv', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) .and. &
         expFieldActive(i2) .and. &
         expFieldActive(i3) ) then
      flogr2(6,1) = .true.  !Radiation stresses
    endif
#endif
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving InitializeP3', ESMF_LOGMSG_INFO)
    !/
    !/ End of InitializeP3 ----------------------------------------------- /
    !/
  end subroutine InitializeP3
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "Finalize"
  !>
  !> @brief Finalize wave model.
  !>
  !> @param gcomp   Gridded component.
  !> @param[out] rc Return code.
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine Finalize ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/    09-Aug-2017 : Add clean up of local allocations   ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Finalize wave model
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WMFINL    Subr. WMFINLMD Wave model finalization
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    integer, parameter :: iwt=6
    real(8) :: wstime, wftime
    integer :: i
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered Finalize', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Finalize the wave model
    !
    call wmfinl
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Clean up ESMF data structures
    !
    ! 2.a Import field and grid stuff
    !
    if ( .not.noActiveImpFields ) then

      do i = 1,numImpFields
        if (.not.impFieldActive(i)) cycle
        call ESMF_FieldDestroy(impField(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        if (merge_import) then
          call ESMF_FieldDestroy(mdtField(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          call ESMF_FieldDestroy(mmskField(i), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
        if (.not.mbgFieldActive(i)) cycle
        call ESMF_FieldDestroy(mbgField(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call ESMF_FieldDestroy(bmskField(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      enddo

      if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
        call ESMF_FieldHaloRelease(impHaloRH, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return

        call ESMF_GridDestroy(impGrid, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return

      elseif (GTYPE.eq.UNGTYPE) then
        !AW          call ESMF_GridDestroy(impMesh, rc=rc)
        !AW          if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif

    endif

    deallocate (impFieldName, &
         impFieldStdName, &
         impFieldInitRqrd, &
         impFieldActive, &
         impField, &
         stat=rc)
    if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return

    deallocate (mbgFieldName, &
         mbgFieldStdName, &
         mbgFieldActive, &
         mbgField, &
         bmskField, &
         stat=rc)
    if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return

    if (merge_import) then
      deallocate(mmskCreated, &
           mmskField, &
           mdtField, &
           stat=rc)
      if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return
    end if
    !
    ! 2.b Export field and grid stuff
    !
    if ( .not.noActiveExpFields ) then

      do i = 1,numExpFields
        if (.not.expFieldActive(i)) cycle
        call ESMF_FieldDestroy(expField(i), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      enddo

      if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
        call ESMF_FieldHaloRelease(expHaloRH, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return

        call ESMF_GridDestroy(expGrid, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return

      elseif (GTYPE.eq.UNGTYPE) then
        !AW          call ESMF_GridDestroy(expMesh, rc=rc)
        !AW          if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif

    endif

    deallocate (expFieldName, &
         expFieldStdName, &
         expFieldDim, &
         expFieldActive, &
         expField, &
         stat=rc)
    if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return
    !
    ! 2.c Native field and grid stuff
    !
    if ( .not.noActiveExpFields ) then

      call ESMF_FieldRedistRelease(n2eRH, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      call ESMF_GridDestroy(natGrid, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

    endif
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Clean up locally allocated data structures
    !
    if (allocated(zl)) then
      deallocate (zl, stat=rc)
      if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    call PrintTimers(trim(cname), wtnam, wtcnt, wtime)
    rc = ESMF_SUCCESS
    if ( improc .eq. nmpscr ) write (*,999)
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving Finalize', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! Formats
    !
999 format(//'  End of program '/                                   &
         ' ========================================'/           &
         '          WAVEWATCH III Multi-grid shell '/)
    !/
    !/ End of Finalize --------------------------------------------------- /
    !/
  end subroutine Finalize
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "DataInitialize"
  !>
  !> @brief  Initialize wave model export data
  !>
  !> @param gcomp   Gridded component.
  !> @param[out] rc Return code.
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  subroutine DataInitialize ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Initialize wave model export data
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name            Type  Module   Description
    !     ----------------------------------------------------------------
    !      GetImport       Subr. WMESMFMD Wave model get import fields
    !      SetExport       Subr. WMESMFMD Wave model set export fields
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    integer, parameter :: iwt=4
    real(8) :: wstime, wftime
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: currTime
    logical :: fldUpdated, allUpdated
    integer :: i, imod
    logical :: local
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered DataInitialize', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Check that required import fields show correct time stamp
    !
    if (med_present) then
      allUpdated = .true.
    else
      call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      allUpdated = .true.
      do i = 1,numImpFields
        if (.not.impFieldActive(i)) cycle
        if (impFieldInitRqrd(i)) then
          fldUpdated = NUOPC_IsAtTime(impField(i), currTime, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          if (fldUpdated) then
            write(msg,'(a,a10,a,a13)') trim(cname)//': ', &
                 trim(impFieldName(i)),': inter-model data dependency: ', &
                 'SATISFIED'
          else
            allUpdated = .false.
            write(msg,'(a,a10,a,a13)') trim(cname)//': ', &
                 trim(impFieldName(i)),': inter-model data dependency: ', &
                 'NOT SATISFIED'
          endif
        else
          write(msg,'(a,a10,a,a13)') trim(cname)//': ', &
               trim(impFieldName(i)),': inter-model data dependency: ', &
               'NOT  REQUIRED'
        endif
        if (verbosity.gt.0) call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (improc.eq.nmpscr) write(*,'(a)') trim(msg)
        ! background
        if (.not.mbgFieldActive(i)) cycle
        if (impFieldInitRqrd(i)) then
          fldUpdated = NUOPC_IsAtTime(mbgField(i), currTime, rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          if (fldUpdated) then
            write(msg,'(a,a10,a,a13)') trim(cname)//': ', &
                 trim(mbgFieldName(i)),': inter-model data dependency: ', &
                 'SATISFIED'
          else
            allUpdated = .false.
            write(msg,'(a,a10,a,a13)') trim(cname)//': ', &
                 trim(mbgFieldName(i)),': inter-model data dependency: ', &
                 'NOT SATISFIED'
          endif
        else
          write(msg,'(a,a10,a,a13)') trim(cname)//': ', &
               trim(mbgFieldName(i)),': inter-model data dependency: ', &
               'NOT  REQUIRED'
        endif
        if (verbosity.gt.0) call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        if (improc.eq.nmpscr) write(*,'(a)') trim(msg)
      enddo
    endif
    !
    ! If not all import dependencies are satisfied, then return
    !
    if (.not.allUpdated) goto 1
    !
    ! -------------------------------------------------------------------- /
    ! 2.  All import dependencies are satisfied, so finish initialization
    !
    ! 2.a Report all import dependencies are satisfied
    !
    write(msg,'(a)') trim(cname)// &
         ': all inter-model data dependencies SATISFIED'
    if (verbosity.gt.0) call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    if (improc.eq.nmpscr) write(*,'(a)') trim(msg)
    !
    ! 2.b Setup background blending mask for each import field
    !
    do i = 1,numImpFields
      if (.not.impFieldActive(i)) cycle
      if (.not.mbgFieldActive(i)) cycle
      call SetupImpBmsk(bmskField(i), impField(i), missingValue, rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    enddo
    !
    ! 2.c Get import fields
    !
    call GetImport(gcomp, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 2.d Finish initialization (compute initial state), if not restart
    !
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
    !
    ! 2.e Set export fields
    !
    call SetExport(gcomp, rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 2.f Set Updated Field Attribute to "true", indicating to the
    !     generic code to set the timestamp for these fields
    !
    do i = 1,numExpFields
      if (.not.expFieldActive(i)) cycle
      call NUOPC_SetAttribute(expField(i), name="Updated", &
           value="true", rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    enddo
    !
    ! 2.g Set InitializeDataComplete Attribute to "true", indicating to the
    !     generic code that all inter-model data dependencies are satisfied
    !
    call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", &
         value="true", rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
1   rc = ESMF_SUCCESS
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving DataInitialize', ESMF_LOGMSG_INFO)
    !/
    !/ End of DataInitialize --------------------------------------------- /
    !/
  end subroutine DataInitialize
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "ModelAdvance"
  !>
  !> @brief Advance wave model in time.
  !>
  !> @param      gcomp Gridded component.
  !> @param[out] rc    Return code.
  !>
  !> @author T. J. Campbell   @date 20-Jan-2017
  !>
  subroutine ModelAdvance ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Advance wave model in time
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      GetImport Subr. WMESMFMD Wave model get import fields
    !      SetExport Subr. WMESMFMD Wave model set export fields
    !      WMWAVE    Subr. WMWAVEMD Wave model run
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    integer, parameter :: iwt=5
    real(8) :: wstime, wftime
    integer :: i, stat, imod, tcur(2)
    integer, allocatable :: tend(:,:)
    integer(ESMF_KIND_I4) :: yy,mm,dd,h,m,s
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: currTime, stopTime
    real :: delt
    logical :: lerr

    type(ESMF_Time) :: startTime
    type(ESMF_TimeInterval)    :: timeStep
    character(len=256)         :: msgString

    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered ModelAdvance', ESMF_LOGMSG_INFO)
    if(profile_memory) call ESMF_VMLogMemInfo('Entering WW3 '// &
         'Model_ADVANCE: ')

    allocate (tend(2,nrgrd), stat=rc)
    if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Advance model to requested end time
    !
    ! 1.a Get component clock
    !
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 1.b Report
    !
    if ( improc .eq. nmpscr ) then
      write(*,'(///)')
      call ESMF_ClockPrint(clock, options="currTime", &
           preString="-->Advancing "//TRIM(cname)//" from: ")
      call ESMF_ClockPrint(clock, options="stopTime", &
           preString="-----------------> to: ")
    endif

    if (profile_memory) then
      call ESMF_ClockPrint(clock, options="currTime", &
           preString="------>Advancing WAV from: ", &
           unit=msgString, rc=rc)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)

      call ESMF_ClockGet(clock, startTime=startTime, &
           currTime=currTime, &
           timeStep=timeStep, rc=rc)

      call ESMF_TimePrint(currTime + timeStep, &
           preString="--------------------------------> to: ", &
           unit=msgString, rc=rc)
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO)
    endif
    !
    ! 1.c Check internal current time with component current time
    !
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_TimeGet(currTime, yy=yy,mm=mm,dd=dd,h=h,m=m,s=s, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    lerr=.false.
    do imod = 1,nrgrd
      tcur(1) = 10000*yy + 100*mm + dd
      tcur(2) = 10000*h  + 100*m  + s
      call w3setw ( imod, mdse, mdst )
      delt = dsec21 ( time, tcur )
      if ( abs(delt).gt.0 ) then
        lerr=.true.
        write(msg,'(a,i2,a,2(a,i8,a,i8,a))') &
             'Wave model grid ',imod,': ', &
             'Internal time (',time(1),'.',time(2),') /= ', &
             'Component time (',tcur(1),'.',tcur(2),')'
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
      endif
    enddo
    if (lerr) then
      rc = ESMF_FAILURE
      return
    endif
    !
    ! 1.d Set end time of this advance
    !
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_TimeGet(stopTime, yy=yy,mm=mm,dd=dd,h=h,m=m,s=s, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    do imod = 1,nrgrd
      tend(1,imod) = 10000*yy + 100*mm + dd
      tend(2,imod) = 10000*h  + 100*m  + s
    enddo
    !
    ! 1.e Get import fields
    !
    call GetImport(gcomp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 1.f Advance model
    !
    if(profile_memory) call ESMF_VMLogMemInfo("Entering WW3 Run : ")
    call wmwave ( tend )
    if(profile_memory) call ESMF_VMLogMemInfo("Entering WW3 Run : ")
    !
    ! 1.g Set export fields
    !
    call SetExport(gcomp, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
    deallocate (tend, stat=rc)
    if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving ModelAdvance', ESMF_LOGMSG_INFO)
    if(profile_memory) call ESMF_VMLogMemInfo('Leaving WW3 '// &
         'Model_ADVANCE: ')
    !/
    !/ End of ModelAdvance ----------------------------------------------- /
    !/
  end subroutine ModelAdvance
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "GetImport"
  !>
  !> @brief Get import fields and put in internal data structures.
  !>
  !> @param      gcomp Gridded component.
  !> @param[out] rc    Return code.
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  subroutine GetImport ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Get import fields and put in internal data structures
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
#ifdef W3_MPI
    USE WMMDATMD, ONLY: IMPROC
#endif
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    !AW ---TEST-TEST-TEST---------------------------
    character(500) :: msg
    integer :: k
    !AW ---TEST-TEST-TEST---------------------------
    integer, parameter :: iwt=7
    real(8) :: wstime, wftime
    integer :: i1, i2, i3, j, imod, jmod
    logical, save :: firstCall = .true.
    integer :: tcur(2), tend(2)
    integer(ESMF_KIND_I4) :: yy,mm,dd,h,m,s
    type(ESMF_Clock) :: clock
    type(ESMF_Time) :: currTime, stopTime
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_GETIMPORT)
    type(ESMF_State) :: dumpState
    integer, save :: timeSlice = 1
#endif
    real(ESMF_KIND_RX), pointer :: rptr(:,:)
    integer :: iy, ix
    integer :: elb(2), eub(2)
    character(len=3) :: fieldName
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    if ( noActiveImpFields ) return
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered GetImport', ESMF_LOGMSG_INFO)
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_GETIMPORT)
    call NUOPC_ModelGet(gcomp, importState=dumpState, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call NUOPC_Write(dumpState, overwrite=.true., &
         fileNamePrefix="field_"//trim(cname)//"_import1_", &
         timeslice=timeSlice, relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#endif
    !
    ! -------------------------------------------------------------------- /
    ! Set time stamps using currTime and stopTime
    !
    call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_ClockGet(clock, currTime=currTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_TimeGet(currTime, yy=yy,mm=mm,dd=dd,h=h,m=m,s=s, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    tcur(1) = 10000*yy + 100*mm + dd
    tcur(2) = 10000*h  + 100*m  + s
    call ESMF_ClockGet(clock, stopTime=stopTime, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_TimeGet(stopTime, yy=yy,mm=mm,dd=dd,h=h,m=m,s=s, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    tend(1) = 10000*yy + 100*mm + dd
    tend(2) = 10000*h  + 100*m  + s
    !
    ! -------------------------------------------------------------------- /
    ! Water levels
    !
    j = 1
    i1 = FieldIndex( impFieldName, 'seahgt', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = i1
    if ( impFieldActive(i1) ) then
      call w3setg ( impGridID, mdse, mdst )
      call w3seti ( impGridID, mdse, mdst )
      if (firstCall) then
        tln = tcur
      else
        tln = tend
      endif
      tfn(:,j) = tln
      if ( mbgFieldActive(i1) ) then
        call BlendImpField( impField(i1), mbgField(i1), bmskField(i1), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
      call FieldGather( impField(i1), nx, ny, wlev, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do imod = 1,nrgrd
        call w3setg ( imod, mdse, mdst )
        call w3setw ( imod, mdse, mdst )
        call w3seti ( imod, mdse, mdst )
        call w3seto ( imod, mdse, mdst )
        call wmsetm ( imod, mdse, mdst )
#ifdef W3_MPI
        if ( mpi_comm_grd .eq. mpi_comm_null ) cycle
#endif
        jmod = inpmap(imod,j)
        if ( jmod.lt.0 .and. jmod.ne.-999 ) then
          call wmupd2( imod, j, jmod, rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        endif
      enddo
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Currents
    !
    j = 2
    i1 = FieldIndex( impFieldName, 'uucurr', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( impFieldName, 'vvcurr', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( impFieldActive(i1) ) then
      call w3setg ( impGridID, mdse, mdst )
      call w3seti ( impGridID, mdse, mdst )
      if (firstCall) then
        tcn = tcur
      else
        tc0 = tcn
        cx0 = cxn
        cy0 = cyn
        tcn = tend
      endif
      tfn(:,j) = tcn
      if ( mbgFieldActive(i1) ) then
        call BlendImpField( impField(i1), mbgField(i1), bmskField(i1), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call BlendImpField( impField(i2), mbgField(i2), bmskField(i2), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
      call FieldGather( impField(i1), nx, ny, cxn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call FieldGather( impField(i2), nx, ny, cyn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (firstCall) then
        tc0 = tcn
        cx0 = cxn
        cy0 = cyn
      endif
      do imod = 1,nrgrd
        call w3setg ( imod, mdse, mdst )
        call w3setw ( imod, mdse, mdst )
        call w3seti ( imod, mdse, mdst )
        call wmsetm ( imod, mdse, mdst )
#ifdef W3_MPI
        if ( mpi_comm_grd .eq. mpi_comm_null ) cycle
#endif
        jmod = inpmap(imod,j)
        if ( jmod.lt.0 .and. jmod.ne.-999 ) then
          call wmupd2( imod, j, jmod, rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        endif
      enddo
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Winds
    !
    j = 3
    i1 = FieldIndex( impFieldName, 'uutrue', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( impFieldName, 'vvtrue', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( impFieldActive(i1) ) then
      call w3setg ( impGridID, mdse, mdst )
      call w3seti ( impGridID, mdse, mdst )

      if (firstCall) then
        twn = tcur
      else
        tw0 = twn
        wx0 = wxn
        wy0 = wyn
        twn = tend
      endif
      tfn(:,j) = twn
      if ( mbgFieldActive(i1) ) then
        call BlendImpField( impField(i1), mbgField(i1), bmskField(i1), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call BlendImpField( impField(i2), mbgField(i2), bmskField(i2), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
      if (merge_import) then
        ! read wave input
        fieldName = 'WND'
        call ReadFromFile(fieldName, mdtField(i1), mdtField(i2), tcur, tend, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        ! create merge mask
        if (.not. firstCall) then
          call SetupImpMmsk(mmskField(i1), impField(i1), fillValue, mmskCreated(i1), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
          call SetupImpMmsk(mmskField(i2), impField(i2), fillValue, mmskCreated(i2), rc=rc)
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        end if
        ! blend data, mask is all zero initially (use all data)
        call BlendImpField( impField(i1), mdtField(i1), mmskField(i1), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call BlendImpField( impField(i2), mdtField(i2), mmskField(i2), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      end if
      call FieldGather( impField(i1), nx, ny, wxn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call FieldGather( impField(i2), nx, ny, wyn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if (firstCall) then
        tw0 = twn
        wx0 = wxn
        wy0 = wyn
#ifdef W3_WRST
        ! The WRST switch saves the values of wind in the
        ! restart file and then uses the wind for the first
        ! time step here.  This is needed when coupling with
        ! an atm model that does not have 10m wind speeds at
        ! initialization.  If there is no restart, wind is zero
        wxn = WXNwrst !replace with values from restart
        wyn = WYNwrst
        wx0 = WXNwrst
        wy0 = WYNwrst
        do imod = 1,nrgrd
          call w3setg ( imod, mdse, mdst )
          call w3setw ( imod, mdse, mdst )
          call w3seti ( imod, mdse, mdst )
          call wmsetm ( imod, mdse, mdst )
#ifdef W3_MPI
          if ( mpi_comm_grd .eq. mpi_comm_null ) cycle
#endif
          INPUTS(IMOD)%TW0(:) = INPUTS(impGridID)%TW0(:)
          INPUTS(IMOD)%TFN(:,3) = INPUTS(impGridID)%TFN(:,3)
          wxn = WXNwrst !replace with values from restart
          wyn = WYNwrst
          wx0 = WXNwrst
          wy0 = WYNwrst
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        enddo
#endif
      endif

#ifdef W3_WRST
      if ( ((twn(1)-tw0(1))*1000000+((twn(2)-tw0(2)))) .le. 0  ) then
        !If the time of the field is still initial time, replace
        !with restart field
        wxn = WXNwrst !replace with values from restart
        wyn = WYNwrst
      else    !twn>tw0
#endif
        do imod = 1,nrgrd
          call w3setg ( imod, mdse, mdst )
          call w3setw ( imod, mdse, mdst )
          call w3seti ( imod, mdse, mdst )
          call wmsetm ( imod, mdse, mdst )
#ifdef W3_MPI
          if ( mpi_comm_grd .eq. mpi_comm_null ) cycle
#endif
          jmod = inpmap(imod,j)
          if ( jmod.lt.0 .and. jmod.ne.-999 ) then
            call wmupd2( imod, j, jmod, rc )
            if (ESMF_LogFoundError(rc, PASSTHRU)) return
          endif
        enddo
#ifdef W3_WRST
      endif  !if ( twn-tw0 .le. 0  )
#endif
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Sea ice concentration
    !
    j = 4
    i1 = FieldIndex( impFieldName, 'seaice', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = i1
    if ( impFieldActive(i1) ) then
      call w3setg ( impGridID, mdse, mdst )
      call w3seti ( impGridID, mdse, mdst )
      if (firstCall) then
        tin = tcur
      else
        tin = tend
      endif
      tfn(:,j) = tin
      if ( mbgFieldActive(i1) ) then
        call BlendImpField( impField(i1), mbgField(i1), bmskField(i1), rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
      call FieldGather( impField(i1), nx, ny, icei, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do imod = 1,nrgrd
        call w3setg ( imod, mdse, mdst )
        call w3setw ( imod, mdse, mdst )
        call w3seti ( imod, mdse, mdst )
        call wmsetm ( imod, mdse, mdst )
#ifdef W3_MPI
        if ( mpi_comm_grd .eq. mpi_comm_null ) cycle
#endif
        jmod = inpmap(imod,j)
        if ( jmod.lt.0 .and. jmod.ne.-999 ) then
          call wmupd2( imod, j, jmod, rc )
          if (ESMF_LogFoundError(rc, PASSTHRU)) return
        endif
      enddo
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_GETIMPORT)
    call NUOPC_Write(dumpState, overwrite=.true., &
         fileNamePrefix="field_"//trim(cname)//"_import2_", &
         timeslice=timeSlice, relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    firstCall = .false.
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving GetImport', ESMF_LOGMSG_INFO)
    !/
    !/ End of GetImport -------------------------------------------------- /
    !/
  end subroutine GetImport
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "SetExport"
  !>
  !> @brief Set export fields from internal data structures.
  !>
  !> @param      gcomp Gridded component
  !> @param[out] rc    Return code
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine SetExport ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/    09-Aug-2017 : Add ocean forcing export fields     ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Set export fields from internal data structures
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    integer, parameter :: iwt=8
    real(8) :: wstime, wftime
    integer :: i1, i2, i3, i4, i5, i6
    logical :: flpart = .false., floutg = .false., floutg2 = .true.
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_SETEXPORT)
    type(ESMF_State) :: dumpState
    integer, save :: timeSlice = 1
#endif
    real(ESMF_KIND_R8), pointer :: farrayptr(:,:)
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    if ( noActiveExpFields ) return
    call ESMF_VMWtime(wstime)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered SetExport', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! Setup
    !
    call w3setg ( expGridID, mdse, mdst )
    call w3setw ( expGridID, mdse, mdst )
    call w3seta ( expGridID, mdse, mdst )
    call w3seti ( expGridID, mdse, mdst )
    call w3seto ( expGridID, mdse, mdst )
    call wmsetm ( expGridID, mdse, mdst )
#ifdef USE_W3OUTG_FOR_EXPORT
    if ( natGridIsLocal ) call w3outg( va, flpart, floutg, floutg2 )
#endif
    !
    ! -------------------------------------------------------------------- /
    ! Charnock
    !
    i1 = FieldIndex( expFieldName, 'charno', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) ) then
      call CalcCharnk( expField(i1), rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Surface Roughness
    !
    i1 = FieldIndex( expFieldName, 'z0rlen', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) ) then
      call CalcRoughl( expField(i1), rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Stokes Drift 3D
    !
    i1 = FieldIndex( expFieldName, 'uscurr', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( expFieldName, 'vscurr', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) .and. &
         expFieldActive(i2) ) then
      call CalcStokes3D( va, expField(i1), expField(i2), rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Partitioned Stokes Drift 3 2D fields
    !
    i1 = FieldIndex( expFieldName, 'x1pstk', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( expFieldName, 'y1pstk', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i3 = FieldIndex( expFieldName, 'x2pstk', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i4 = FieldIndex( expFieldName, 'y2pstk', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i5 = FieldIndex( expFieldName, 'x3pstk', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i6 = FieldIndex( expFieldName, 'y3pstk', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) .and. &
         expFieldActive(i2) .and. &
         expFieldActive(i3) .and. &
         expFieldActive(i4) .and. &
         expFieldActive(i5) .and. &
         expFieldActive(i6)  ) then
      call CalcPStokes( va, expField(i1), expField(i2), expField(i3), &
           expField(i4), expField(i5), expField(i6), rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Bottom Currents
    !
    i1 = FieldIndex( expFieldName, 'wbcuru', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( expFieldName, 'wbcurv', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i3 = FieldIndex( expFieldName, 'wbcurp', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) .and. &
         expFieldActive(i2) .and. &
         expFieldActive(i3) ) then
      call CalcBotcur( va, expField(i1), expField(i2), expField(i3), rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Radiation stresses 2D
    !
    i1 = FieldIndex( expFieldName, 'wavsuu', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i2 = FieldIndex( expFieldName, 'wavsuv', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    i3 = FieldIndex( expFieldName, 'wavsvv', rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expFieldActive(i1) .and. &
         expFieldActive(i2) .and. &
         expFieldActive(i3) ) then
      call CalcRadstr2D( va, expField(i1), expField(i2), expField(i3), rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! cpl_scalars - grid sizes
    !
    if (med_present) then
      i1 = FieldIndex( expFieldName, trim(flds_scalar_name), rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      if ( expFieldActive(i1) ) then
        call ESMF_FieldGet(expField(i1), farrayPtr=farrayptr, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        if (flds_scalar_index_nx > 0 .and. flds_scalar_index_nx < flds_scalar_num) then
          farrayptr(flds_scalar_index_nx,1) = dble(nx)
        endif
        if (flds_scalar_index_ny > 0 .and. flds_scalar_index_ny < flds_scalar_num) then
          farrayptr(flds_scalar_index_ny,1) = dble(ny)
        endif
      endif
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_SETEXPORT)
    call NUOPC_ModelGet(gcomp, exportState=dumpState, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call NUOPC_Write(dumpState, overwrite=.true., &
         fileNamePrefix="field_"//trim(cname)//"_export_", &
         timeslice=timeSlice, relaxedFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    rc = ESMF_SUCCESS
    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving SetExport', ESMF_LOGMSG_INFO)
    !/
    !/ End of SetExport -------------------------------------------------- /
    !/
  end subroutine SetExport
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CreateImpGrid"
  !>
  !> @brief Create ESMF grid for import fields.
  !>
  !> @param      gcomp Gridded component
  !> @param[out] rc    Return code
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  subroutine CreateImpGrid ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Create ESMF grid for import fields
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    type(ESMF_VM) :: vm
    character(ESMF_MAXSTR) :: cname
    integer :: nproc, nxproc, nyproc
    integer, parameter :: lde = 0
    integer :: lpet, ldecnt
    integer :: ix, iy, isea, jsea, irec, ubx, uby
    integer :: elb(2), eub(2), elbc(2), eubc(2)
    integer :: tlb(2), tub(2)
    integer(ESMF_KIND_I4), pointer :: iptr(:,:)
    real(ESMF_KIND_RX), pointer :: rptrx(:,:), rptry(:,:)
    real(ESMF_KIND_RX), pointer :: rptr(:,:)
    real(ESMF_KIND_R8), allocatable :: xgrd_center(:)
    real(ESMF_KIND_R8), allocatable :: ygrd_center(:)
    real(ESMF_KIND_R8), allocatable :: xgrd_corner(:,:)
    real(ESMF_KIND_R8), allocatable :: ygrd_corner(:,:)
    logical, allocatable :: land_sea(:)
    integer, allocatable :: grid_dims(:)
    integer :: grid_size, grid_corners, grid_rank
    type(ESMF_Field) :: tmpField
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    if ( noActiveImpFields ) return
    call ESMF_GridCompGet(gcomp, name=cname, vm=vm, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_VMGet(vm, localPet=lpet, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered CreateImpGrid', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Setup
    !
    ! 1.a Set grid pointers
    !
    impGridID = minval(inpmap)
    if ( impGridID.eq.-999 ) impGridID = 1
    call w3setg ( impGridID, mdse, mdst )
    call w3seti ( impGridID, mdse, mdst )
    call w3seto ( impGridID, mdse, mdst )
    if ( impGridID.gt.0 ) then
      call wmsetm ( impGridID, mdse, mdst )
      nproc = naproc
    else
      nproc = nmproc
    endif
    !
    ! 1.b Compute a 2D subdomain layout based on nproc
    !
    call CalcDecomp( nx, ny, nproc, impHaloWidth, .true., nxproc, nyproc, rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 1.c Set arraySpec, staggerLoc, and indexFlag for import fields
    !
    call ESMF_ArraySpecSet( impArraySpec2D, rank=2, &
         typekind=ESMF_TYPEKIND_RX, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    impStaggerLoc = ESMF_STAGGERLOC_CENTER
    impIndexFlag = ESMF_INDEX_GLOBAL
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Create ESMF grid for import with 2D subdomain layout
    !     Note that the ESMF grid layout is dim1=X, dim2=Y
    !
    ! 2.a Create ESMF import grid
    !
    select case (iclose)
    case (iclose_none)
      impGrid = ESMF_GridCreateNoPeriDim( &
           minIndex=(/ 1, 1/), &
           maxIndex=(/nx,ny/), &
           coordDep1=(/1,2/), &
           coordDep2=(/1,2/), &
           regDecomp=(/nxproc,nyproc/), &
           decompFlag=(/ESMF_DECOMP_BALANCED,ESMF_DECOMP_BALANCED/), &
           coordTypeKind=ESMF_TYPEKIND_RX, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           indexFlag=impIndexFlag, &
           name=trim(cname)//"_import_grid", rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    case (iclose_smpl)
      impGrid = ESMF_GridCreate1PeriDim( &
           periodicDim=1, &
           poleDim=2, &
           poleKindFlag=(/ESMF_POLEKIND_NONE,ESMF_POLEKIND_NONE/), &
           minIndex=(/ 1, 1/), &
           maxIndex=(/nx,ny/), &
           coordDep1=(/1,2/), &
           coordDep2=(/1,2/), &
           regDecomp=(/nxproc,nyproc/), &
           decompFlag=(/ESMF_DECOMP_BALANCED,ESMF_DECOMP_BALANCED/), &
           coordTypeKind=ESMF_TYPEKIND_RX, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           indexFlag=impIndexFlag, &
           name=trim(cname)//"_import_grid", rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    case default
      write(msg,'(a,i1,a)') 'Index closure ',iclose, &
           ' not supported for import grid'
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
      rc = ESMF_FAILURE
      return
    endselect
    !
    ! 2.b Add coordinate arrays and land/sea mask to import grid
    !
    call ESMF_GridAddCoord( impGrid, staggerLoc=impStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_GridAddItem( impGrid, ESMF_GRIDITEM_MASK, &
         staggerLoc=impStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 2.c Set flag to indicate that this processor has local import grid storage
    !
    call ESMF_GridGet( impGrid, localDECount=ldecnt, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    impGridIsLocal = ldecnt.gt.0
    !
    ! 2.d Get exclusive bounds (global index) for import grid
    !
    if ( impGridIsLocal ) then
      call ESMF_GridGet( impGrid, impStaggerLoc, lde, &
           exclusiveLBound=elb, exclusiveUBound=eub, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! 2.e Set halo widths for import fields
    !
    if ( impGridIsLocal ) then
      impHaloLWidth = (/impHaloWidth,impHaloWidth/)
      impHaloUWidth = (/impHaloWidth,impHaloWidth/)
      select case (iclose)
      case (iclose_none)
        if ( elb(1).eq.1  ) impHaloLWidth(1) = 0
        if ( elb(2).eq.1  ) impHaloLWidth(2) = 0
        if ( eub(1).eq.nx ) impHaloUWidth(1) = 0
        if ( eub(2).eq.ny ) impHaloUWidth(2) = 0
      case (iclose_smpl)
        if ( elb(2).eq.1  ) impHaloLWidth(2) = 0
        if ( eub(2).eq.ny ) impHaloUWidth(2) = 0
      endselect
    else
      impHaloLWidth = (/0,0/)
      impHaloUWidth = (/0,0/)
    endif
    !
    ! 2.f Set ESMF import grid coordinates
    !
    if ( impGridIsLocal ) then
      call ESMF_GridGetCoord( impGrid, 1, localDE=lde, &
           staggerLoc=impStaggerLoc, farrayPtr=rptrx, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_GridGetCoord( impGrid, 2, localDE=lde, &
           staggerLoc=impStaggerLoc, farrayPtr=rptry, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptrx(ix,iy) = xgrd(iy,ix)
          rptry(ix,iy) = ygrd(iy,ix)
        enddo
      enddo
      nullify(rptrx)
      nullify(rptry)
    endif
    !
    ! 2.g Set ESMF import grid land/sea mask values.
    !     Land/sea mask is fixed in time and based on excluded points only.
    !
    if ( impGridIsLocal ) then
      call ESMF_GridGetItem( impGrid, ESMF_GRIDITEM_MASK, localDE=lde, &
           staggerLoc=impStaggerLoc, farrayPtr=iptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          if ( mapsta(iy,ix).ne.0 ) then
            iptr(ix,iy) = maskValueWater
          else
            iptr(ix,iy) = maskValueLand
          endif
        enddo
      enddo
    endif
    !
    ! 2.h Set ESMF import grid corner coordinates
    !
#ifdef W3_SCRIP
    call ESMF_GridAddCoord( impGrid, &
         staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    ! Calculate grid coordinates with help of SCRIP module
    ! It does not return coordinates of top-most row and
    ! right-most column but ESMF expects it. So, top-most row
    ! and right-most column are theated specially in below
    call get_scrip_info_structured(impGridID, &
         xgrd_center, ygrd_center, xgrd_corner, ygrd_corner, &
         land_sea, grid_dims, grid_size, grid_corners, grid_rank)

    ! Add corner coordinates
    if ( impGridIsLocal ) then
      ! Retrieve pointers
      call ESMF_GridGetCoord( impGrid, 1, localDE=lde, &
           staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=rptrx, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_GridGetCoord( impGrid, 2, localDE=lde, &
           staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=rptry, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      ! Get exclusive bounds (global index) for import grid
      ! corner coordinates
      call ESMF_GridGet( impGrid, ESMF_STAGGERLOC_CORNER, lde, &
           exclusiveLBound=elbc, exclusiveUBound=eubc, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      ! Adjust upper bounds for specific PEs
      ubx = 0
      uby = 0
      if (eubc(1) == grid_dims(1)+1) ubx = -1
      if (eubc(2) == grid_dims(2)+1) uby = -1

      ! Fill coordinates
      do iy = elbc(2),eubc(2)+uby
        do ix = elbc(1),eubc(1)+ubx
          irec = (iy-1)*grid_dims(1)+ix
          rptrx(ix,iy) = real(xgrd_corner(1,irec), kind=ESMF_KIND_RX)
          rptry(ix,iy) = real(ygrd_corner(1,irec), kind=ESMF_KIND_RX)
        enddo
      enddo

      ! Fill data on top-most row
      if (eubc(2) == grid_dims(2)+1) then
        do ix = elbc(1),eubc(1)+ubx
          rptrx(ix,grid_dims(2)+1) = rptrx(ix,grid_dims(2))+ &
               (rptrx(ix,grid_dims(2))-rptrx(ix,grid_dims(2)-1))
          rptry(ix,grid_dims(2)+1) = rptry(ix,grid_dims(2))+ &
               (rptry(ix,grid_dims(2))-rptry(ix,grid_dims(2)-1))
        end do
      end if

      ! Fill data on right-most column
      if (eubc(1) == grid_dims(1)+1) then
        do iy = elbc(2),eubc(2)+uby
          rptrx(grid_dims(1)+1,iy) = rptrx(grid_dims(1),iy)+ &
               (rptrx(grid_dims(1),iy)-rptrx(grid_dims(1)-1,iy))
          rptry(grid_dims(1)+1,iy) = rptry(grid_dims(1),iy)+ &
               (rptry(grid_dims(1),iy)-rptry(grid_dims(1)-1,iy))
        end do
      end if

      ! Fill data on top-right corner, single point
      if (eubc(1) == grid_dims(1)+1 .and. eubc(2) == grid_dims(2)+1) then
        rptrx(grid_dims(1)+1,grid_dims(2)+1) = &
             rptrx(grid_dims(1)+1,grid_dims(2))
        rptry(grid_dims(1)+1,grid_dims(2)+1) = &
             rptry(grid_dims(1),grid_dims(2)+1)
      end if
    endif
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Create import field mask and routeHandle halo update
    !
    ! 3.a Create field for import grid land/sea mask.
    !
    impMask = ESMF_FieldCreate( impGrid, impArraySpec2D, &
         totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
         staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, &
         name='mask', rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 3.b Store import field halo routeHandle
    !
    call ESMF_FieldHaloStore( impMask, routeHandle=impHaloRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 3.c Set import field land/sea mask values and update halos
    !
    if ( impGridIsLocal ) then
      call ESMF_FieldGet( impMask, localDE=lde, farrayPtr=rptr, &
           totalLBound=tlb, totalUBound=tub, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = iptr(ix,iy)
        enddo
      enddo
    endif

    call ESMF_FieldHalo( impMask, impHaloRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_CREATEIMPGRID)
    write(msg,'(a,i6)')  '     impGridID: ',impGridID
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,l6)')  'impGridIsLocal: ',impGridIsLocal
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '        nx, ny: ',nx,ny
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') 'naproc, nmproc: ',naproc,nmproc
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,i6)')  '         nproc: ',nproc
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') 'nxproc, nyproc: ',nxproc,nyproc
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           elb: ',elb(:)
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           eub: ',eub(:)
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           tlb: ',tlb(:)
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           tub: ',tub(:)
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') ' impHaloLWidth: ',impHaloLWidth(:)
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') ' impHaloUWidth: ',impHaloUWidth(:)
    call ESMF_LogWrite(trim(cname)//': CreateImpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    call ESMF_FieldWrite( impMask, &
         "wmesmfmd_createimpgrid_import_mask.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    tmpField = ESMF_FieldCreate( impGrid, impArraySpec2D, &
         totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
         staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, &
         name='temp', rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( impGridIsLocal ) then
      call ESMF_FieldGet( tmpField, localDE=lde, farrayPtr=rptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    if ( impGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = xgrd(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createimpgrid_import_xgrd.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( impGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = ygrd(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createimpgrid_import_ygrd.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( impGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = hpfac(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createimpgrid_import_hpfac.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( impGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = hqfac(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createimpgrid_import_hqfac.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( impGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = lpet
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createimpgrid_import_dcomp.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( tmpField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#endif
    !
    rc = ESMF_SUCCESS
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving CreateImpGrid', ESMF_LOGMSG_INFO)
    !/
    !/ End of CreateImpGrid ---------------------------------------------- /
    !/
  end subroutine CreateImpGrid
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CreateExpGrid"
  !>
  !> @brief Create ESMF grid for export fields
  !>
  !> @param      gcomp Gridded component
  !> @param[out] rc    Return code
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  subroutine CreateExpGrid ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !       !/SHRD  Switch for shared / distributed memory architecture.
    !       !/DIST  Id.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    integer :: nproc, nxproc, nyproc
    integer, parameter :: lde = 0
    integer :: ldecnt
    integer :: ix, iy, isea, jsea, irec, k, ubx, uby
    integer :: elb(2), eub(2), elbc(2), eubc(2)
    integer :: tlb(2), tub(2)
    integer(ESMF_KIND_I4), pointer :: iptr(:,:)
    real(ESMF_KIND_RX), pointer :: rptrx(:,:), rptry(:,:)
    real(ESMF_KIND_RX), pointer :: rptr(:,:)
    real(ESMF_KIND_R8), allocatable :: xgrd_center(:)
    real(ESMF_KIND_R8), allocatable :: ygrd_center(:)
    real(ESMF_KIND_R8), allocatable :: xgrd_corner(:,:)
    real(ESMF_KIND_R8), allocatable :: ygrd_corner(:,:)
    logical, allocatable :: land_sea(:)
    integer, allocatable :: grid_dims(:)
    integer :: grid_size, grid_corners, grid_rank
    integer :: arbIndexCount
    integer, allocatable :: arbIndexList(:,:)
    type(ESMF_Field) :: nField, eField
    type(ESMF_Field) :: tmpField
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    if ( noActiveExpFields ) return
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered CreateExpGrid', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Setup
    !
    ! 1.a Set grid pointers
    !
    !TODO: only export from one grid
    !expGridID set from config input (default = 1)
    call w3setg ( expGridID, mdse, mdst )
    call w3setw ( expGridID, mdse, mdst )
    call w3seta ( expGridID, mdse, mdst )
    call w3seti ( expGridID, mdse, mdst )
    call w3seto ( expGridID, mdse, mdst )
    call wmsetm ( expGridID, mdse, mdst )
    natGridID = expGridID
    nproc = naproc
    !
    ! 1.b Compute a 2D subdomain layout based on nproc
    !
    call CalcDecomp( nx, ny, nproc, expHaloWidth, .true., nxproc, nyproc, rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 1.c Set arraySpec, staggerLoc, and indexFlag for export fields
    !
    call ESMF_ArraySpecSet( expArraySpec2D, rank=2, &
         typekind=ESMF_TYPEKIND_RX, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_ArraySpecSet( expArraySpec3D, rank=3, &
         typekind=ESMF_TYPEKIND_RX, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    expStaggerLoc = ESMF_STAGGERLOC_CENTER
    expIndexFlag = ESMF_INDEX_GLOBAL
    !
    ! 1.d Set arraySpec, staggerLoc, and indexFlag for native fields
    !
    call ESMF_ArraySpecSet( natArraySpec2D, rank=1, &
         typekind=ESMF_TYPEKIND_RX, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_ArraySpecSet( natArraySpec3D, rank=2, &
         typekind=ESMF_TYPEKIND_RX, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    natStaggerLoc = ESMF_STAGGERLOC_CENTER
    natIndexFlag = ESMF_INDEX_DELOCAL
    !
    ! 1.e Get z-levels for 3D export fields
    !
    call GetZlevels( rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(msg,'(a)') 'input z-level file: '//trim(zlfile)
    call ESMF_LogWrite(trim(cname)//': '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a)') 'table of z-levels'
    call ESMF_LogWrite(trim(cname)//': '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a)') '   index         z'
    call ESMF_LogWrite(trim(cname)//': '//trim(msg), ESMF_LOGMSG_INFO)
    do k=1,nz
      write(msg,'(i8,1f10.2)') k, zl(k)
      call ESMF_LogWrite(trim(cname)//': '//trim(msg), ESMF_LOGMSG_INFO)
    enddo
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Create ESMF grid for export with 2D subdomain layout
    !     Note that the ESMF grid layout is dim1=X, dim2=Y
    !
    ! 2.a Create ESMF export grid
    !
    select case (iclose)
    case (iclose_none)
      expGrid = ESMF_GridCreateNoPeriDim( &
           minIndex=(/ 1, 1/), &
           maxIndex=(/nx,ny/), &
           coordDep1=(/1,2/), &
           coordDep2=(/1,2/), &
           regDecomp=(/nxproc,nyproc/), &
           decompFlag=(/ESMF_DECOMP_BALANCED,ESMF_DECOMP_BALANCED/), &
           coordTypeKind=ESMF_TYPEKIND_RX, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           indexFlag=expIndexFlag, &
           name=trim(cname)//"_export_grid", rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    case (iclose_smpl)
      expGrid = ESMF_GridCreate1PeriDim( &
           periodicDim=1, &
           poleDim=2, &
           poleKindFlag=(/ESMF_POLEKIND_NONE,ESMF_POLEKIND_NONE/), &
           minIndex=(/ 1, 1/), &
           maxIndex=(/nx,ny/), &
           coordDep1=(/1,2/), &
           coordDep2=(/1,2/), &
           regDecomp=(/nxproc,nyproc/), &
           decompFlag=(/ESMF_DECOMP_BALANCED,ESMF_DECOMP_BALANCED/), &
           coordTypeKind=ESMF_TYPEKIND_RX, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           indexFlag=expIndexFlag, &
           name=trim(cname)//"_export_grid", rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    case default
      write(msg,'(a,i1,a)') 'Index closure ',iclose, &
           ' not supported for export grid'
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
      rc = ESMF_FAILURE
      return
    endselect
    !
    ! 2.b Add coordinate arrays and land/sea mask to export grid
    !
    call ESMF_GridAddCoord( expGrid, staggerLoc=expStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_GridAddItem( expGrid, ESMF_GRIDITEM_MASK, &
         staggerLoc=expStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 2.c Set flag to indicate that this processor has local export grid storage
    !
    call ESMF_GridGet( expGrid, localDECount=ldecnt, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    expGridIsLocal = ldecnt.gt.0
    !
    ! 2.d Get exclusive bounds (global index) for export grid
    !
    if ( expGridIsLocal ) then
      call ESMF_GridGet( expGrid, expStaggerLoc, lde, &
           exclusiveLBound=elb, exclusiveUBound=eub, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! 2.e Set halo widths for export fields
    !
    if ( expGridIsLocal ) then
      expHaloLWidth = (/expHaloWidth,expHaloWidth/)
      expHaloUWidth = (/expHaloWidth,expHaloWidth/)
      select case (iclose)
      case (iclose_none)
        if ( elb(1).eq.1  ) expHaloLWidth(1) = 0
        if ( elb(2).eq.1  ) expHaloLWidth(2) = 0
        if ( eub(1).eq.nx ) expHaloUWidth(1) = 0
        if ( eub(2).eq.ny ) expHaloUWidth(2) = 0
      case (iclose_smpl)
        if ( elb(2).eq.1  ) expHaloLWidth(2) = 0
        if ( eub(2).eq.ny ) expHaloUWidth(2) = 0
      endselect
    else
      expHaloLWidth = (/0,0/)
      expHaloUWidth = (/0,0/)
    endif
    !
    ! 2.f Set ESMF export grid coordinate
    !
    if ( expGridIsLocal ) then
      call ESMF_GridGetCoord( expGrid, 1, localDE=lde, &
           staggerLoc=expStaggerLoc, farrayPtr=rptrx, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_GridGetCoord( expGrid, 2, localDE=lde, &
           staggerLoc=expStaggerLoc, farrayPtr=rptry, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptrx(ix,iy) = xgrd(iy,ix)
          rptry(ix,iy) = ygrd(iy,ix)
        enddo
      enddo
    endif
    !
    ! 2.g Set ESMF export grid land/sea mask values.
    !     Land/sea mask is fixed in time and based on excluded points only.
    !
    if ( expGridIsLocal ) then
      call ESMF_GridGetItem( expGrid, ESMF_GRIDITEM_MASK, localDE=lde, &
           staggerLoc=expStaggerLoc, farrayPtr=iptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          if ( mapsta(iy,ix).ne.0 ) then
            iptr(ix,iy) = maskValueWater
          else
            iptr(ix,iy) = maskValueLand
          endif
        enddo
      enddo
    endif
    !
    ! 2.h Set ESMF export grid corner coordinates
    !
#ifdef W3_SCRIP
    call ESMF_GridAddCoord( expGrid, &
         staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    ! Calculate grid coordinates with help of SCRIP module
    ! It does not return coordinates of top-most row and
    ! right-most column but ESMF expects it. So, top-most row
    ! and right-most column are theated specially in below
    call get_scrip_info_structured(expGridID, &
         xgrd_center, ygrd_center, xgrd_corner, ygrd_corner, &
         land_sea, grid_dims, grid_size, grid_corners, grid_rank)

    ! Add corner coordinates
    if ( impGridIsLocal ) then
      ! Retrieve pointers
      call ESMF_GridGetCoord( expGrid, 1, localDE=lde, &
           staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=rptrx, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_GridGetCoord( expGrid, 2, localDE=lde, &
           staggerLoc=ESMF_STAGGERLOC_CORNER, farrayPtr=rptry, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      ! Get exclusive bounds (global index) for export grid
      ! corner coordinates
      call ESMF_GridGet( impGrid, ESMF_STAGGERLOC_CORNER, lde, &
           exclusiveLBound=elbc, exclusiveUBound=eubc, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      ! Adjust upper bounds for specific PEs
      ubx = 0
      uby = 0
      if (eubc(1) == grid_dims(1)+1) ubx = -1
      if (eubc(2) == grid_dims(2)+1) uby = -1

      ! Fill coordinates
      do iy = elbc(2),eubc(2)+uby
        do ix = elbc(1),eubc(1)+ubx
          irec = (iy-1)*grid_dims(1)+ix
          rptrx(ix,iy) = real(xgrd_corner(1,irec), kind=ESMF_KIND_RX)
          rptry(ix,iy) = real(ygrd_corner(1,irec), kind=ESMF_KIND_RX)
        enddo
      enddo

      ! Fill data on top-most row
      if (eubc(2) == grid_dims(2)+1) then
        do ix = elbc(1),eubc(1)+ubx
          rptrx(ix,grid_dims(2)+1) = rptrx(ix,grid_dims(2))+ &
               (rptrx(ix,grid_dims(2))-rptrx(ix,grid_dims(2)-1))
          rptry(ix,grid_dims(2)+1) = rptry(ix,grid_dims(2))+ &
               (rptry(ix,grid_dims(2))-rptry(ix,grid_dims(2)-1))
        end do
      end if

      ! Fill data on right-most column
      if (eubc(1) == grid_dims(1)+1) then
        do iy = elbc(2),eubc(2)+uby
          rptrx(grid_dims(1)+1,iy) = rptrx(grid_dims(1),iy)+ &
               (rptrx(grid_dims(1),iy)-rptrx(grid_dims(1)-1,iy))
          rptry(grid_dims(1)+1,iy) = rptry(grid_dims(1),iy)+ &
               (rptry(grid_dims(1),iy)-rptry(grid_dims(1)-1,iy))
        end do
      end if

      ! Fill data on top-right corner, single point
      if (eubc(1) == grid_dims(1)+1 .and. eubc(2) == grid_dims(2)+1) then
        rptrx(grid_dims(1)+1,grid_dims(2)+1) = &
             rptrx(grid_dims(1)+1,grid_dims(2))
        rptry(grid_dims(1)+1,grid_dims(2)+1) = &
             rptry(grid_dims(1),grid_dims(2)+1)
      end if
    end if
#endif
    !
    ! -------------------------------------------------------------------- /
    ! 3.  Create export field mask and routeHandle halo update
    !
    ! 3.a Create field for export grid land/sea mask.
    !
    expMask = ESMF_FieldCreate( expGrid, expArraySpec2D, &
         totalLWidth=expHaloLWidth, totalUWidth=expHaloUWidth, &
         staggerLoc=expStaggerLoc, indexFlag=expIndexFlag, &
         name='mask', rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 3.b Store export field halo routeHandle
    !
    call ESMF_FieldHaloStore( expMask, routeHandle=expHaloRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 3.c Set export field land/sea mask values and update halos
    !
    if ( expGridIsLocal ) then
      call ESMF_FieldGet( expMask, localDE=lde, farrayPtr=rptr, &
           totalLBound=tlb, totalUBound=tub, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = iptr(ix,iy)
        enddo
      enddo
    endif

    call ESMF_FieldHalo( expMask, expHaloRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! 4.  Create ESMF grid with arbitrary domain decomposition to match
    !     the native domain decomposition of the non-excluded points
    !     Note that the native grid layout is dim1=X, dim2=Y
    !     Note that coordinates and mask are not needed since this
    !     grid is only used to define fields for a redist operation
    !
    ! 4.a Set flag to indicate that this processor has local native grid storage
    !
    natGridIsLocal = iaproc .gt. 0 .and. iaproc .le. naproc
    !
    ! 4.b Setup arbitrary sequence index list
    !
    do ipass = 1,2
      if (ipass.eq.2) then
        allocate (arbIndexList(arbIndexCount,2), stat=rc)
        if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
      endif
      arbIndexCount = 0
      ! list local native grid non-excluded points
      if ( natGridIsLocal ) then
        do jsea = 1,nseal
#ifdef W3_DIST
          isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
          isea = jsea
#endif
          arbIndexCount = arbIndexCount+1
          if (ipass.eq.2) then
            ix = mapsf(isea,1)
            iy = mapsf(isea,2)
            ! native grid layout: dim1=X, dim2=Y
            arbIndexList(arbIndexCount,1) = ix
            arbIndexList(arbIndexCount,2) = iy
          endif
        enddo
      endif
      ! list local export grid excluded points
      if ( expGridIsLocal ) then
        do iy = elb(2),eub(2)
          do ix = elb(1),eub(1)
            if ( mapsta(iy,ix).ne.0 ) cycle ! skip non-excluded point
            arbIndexCount = arbIndexCount+1
            if (ipass.eq.2) then
              ! native grid layout: dim1=X, dim2=Y
              arbIndexList(arbIndexCount,1) = ix
              arbIndexList(arbIndexCount,2) = iy
            endif
          enddo
        enddo
      endif
    enddo !ipass
    !
    ! 4.c Create ESMF native grid
    !
    select case (iclose)
    case (iclose_none)
      natGrid = ESMF_GridCreateNoPeriDim( &
           minIndex=(/ 1, 1/), &
           maxIndex=(/nx,ny/), &
           coordDep1=(/ESMF_DIM_ARB,ESMF_DIM_ARB/), &
           coordDep2=(/ESMF_DIM_ARB,ESMF_DIM_ARB/), &
           arbIndexCount=arbIndexCount, &
           arbIndexList=arbIndexList, &
           coordTypeKind=ESMF_TYPEKIND_RX, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           name=trim(cname)//"_native_grid", rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    case (iclose_smpl)
      natGrid = ESMF_GridCreate1PeriDim( &
           periodicDim=1, &
           poleDim=2, &
           poleKindFlag=(/ESMF_POLEKIND_NONE,ESMF_POLEKIND_NONE/), &
           minIndex=(/ 1, 1/), &
           maxIndex=(/nx,ny/), &
           coordDep1=(/ESMF_DIM_ARB,ESMF_DIM_ARB/), &
           coordDep2=(/ESMF_DIM_ARB,ESMF_DIM_ARB/), &
           arbIndexCount=arbIndexCount, &
           arbIndexList=arbIndexList, &
           coordTypeKind=ESMF_TYPEKIND_RX, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           name=trim(cname)//"_native_grid", rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    case default
      write(msg,'(a,i1,a)') 'Index closure ',iclose, &
           ' not supported for native grid'
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
      rc = ESMF_FAILURE
      return
    endselect
    !
    ! 4.d Deallocate arbitrary sequence index list
    !
    deallocate (arbIndexList, stat=rc)
    if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! 5.  Create route handle for redist between native grid domain
    !     decomposition and the export grid domain decomposition
    !
    ! 5.a Create temporary fields
    !
    nField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    eField = ESMF_FieldCreate( expGrid, expArraySpec2D, &
         totalLWidth=expHaloLWidth, totalUWidth=expHaloUWidth, &
         staggerLoc=expStaggerLoc, indexFlag=expIndexFlag, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 5.b Store route handle
    !
    call ESMF_FieldRedistStore( nField, eField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! 5.c Clean up
    !
    call ESMF_FieldDestroy( nField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( eField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !
    ! -------------------------------------------------------------------- /
    ! Post
    !
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_CREATEEXPGRID)
    write(msg,'(a,i6)')  '     expGridID: ',expGridID
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,l6)')  'expGridIsLocal: ',expGridIsLocal
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '        nx, ny: ',nx,ny
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') 'naproc, nmproc: ',naproc,nmproc
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,i6)')  '         nproc: ',nproc
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') 'nxproc, nyproc: ',nxproc,nyproc
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           elb: ',elb(:)
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           eub: ',eub(:)
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           tlb: ',tlb(:)
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') '           tub: ',tub(:)
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') ' expHaloLWidth: ',expHaloLWidth(:)
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    write(msg,'(a,2i6)') ' expHaloUWidth: ',expHaloUWidth(:)
    call ESMF_LogWrite(trim(cname)//': CreateExpGrid: '//trim(msg), ESMF_LOGMSG_INFO)
    call ESMF_FieldWrite( expMask, &
         "wmesmfmd_createexpgrid_export_mask.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    tmpField = ESMF_FieldCreate( expGrid, expArraySpec2D, &
         totalLWidth=expHaloLWidth, totalUWidth=expHaloUWidth, &
         staggerLoc=expStaggerLoc, indexFlag=expIndexFlag, &
         name='temp', rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expGridIsLocal ) then
      call ESMF_FieldGet( tmpField, localDE=lde, farrayPtr=rptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    if ( expGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = xgrd(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createexpgrid_export_xgrd.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = ygrd(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createexpgrid_export_ygrd.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = hpfac(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createexpgrid_export_hpfac.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = hqfac(iy,ix)
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createexpgrid_export_hqfac.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if ( expGridIsLocal ) then
      do iy = elb(2),eub(2)
        do ix = elb(1),eub(1)
          rptr(ix,iy) = lpet
        enddo
      enddo
    endif
    call ESMF_FieldWrite( tmpField, &
         "wmesmfmd_createexpgrid_export_dcomp.nc", overwrite=.true., rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( tmpField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#endif
    !
    rc = ESMF_SUCCESS
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving CreateExpGrid', ESMF_LOGMSG_INFO)
    !/
    !/ End of CreateExpGrid ---------------------------------------------- /
    !/
  end subroutine CreateExpGrid
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CreateImpMesh"
  !>
  !> @brief Create ESMF mesh (unstructured) for import fields.
  !>
  !> @details Create an ESMF Mesh for import using the unstructured mesh
  !>  description in W3GDATMD. At present, this import mesh is not
  !>  domain decomposed, but instead is defined on PET 0 only. (In
  !>  future, when the unstructured mesh will run on domain decomposition,
  !>  we will use that decomposition.)
  !>
  !> @param      gcomp Gridded component
  !> @param[out] rc    Return code
  !>
  !> @author A. J. van der Westhuysen  @date 28-Feb-2018
  !>
  subroutine CreateImpMesh ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |     A. J. van der Westhuysen      |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         28-FEB_2018 |
    !/                  +-----------------------------------+
    !/
    !/    28-Feb-2018 : Origination.                        ( version 6.06 )
    !/
    !  1. Purpose :
    !
    !     Create ESMF mesh (unstructured) for import fields
    !
    !  2. Method :
    !
    !     Create an ESMF Mesh for import using the unstructured mesh description
    !     in W3GDATMD. At present, this import mesh is not domain decomposed,
    !     but instead is defined on PET 0 only. (In future, when the unstructured
    !     mesh will run on domain decomposition, we will use that decomposition.)
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_PDLIB
    use yowNodepool, only: npa, iplg, nodes_global
    use yowElementpool, only: ne, ielg, INE
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    character(128) :: msg
    integer :: nproc, nxproc, nyproc, n, nfac, irp
    real    :: gr, rp, pr, diff
    integer, parameter :: lde = 0
    integer :: ldecnt
    integer :: i, j, pos, ix, iy
    integer(ESMF_KIND_I4), pointer :: iptr(:,:)
    real(ESMF_KIND_RX), pointer :: rptrx(:,:), rptry(:,:)
    real(ESMF_KIND_RX), pointer :: rptr(:,:)
    type(ESMF_Field) :: tmpField
    integer(ESMF_KIND_I4), allocatable :: nodeIds(:)
    real(ESMF_KIND_R8), allocatable    :: nodeCoords(:)
    integer(ESMF_KIND_I4), allocatable :: nodeOwners(:)
    integer(ESMF_KIND_I4), allocatable :: elemIds(:)
    integer(ESMF_KIND_I4), allocatable :: elemTypes(:)
    integer(ESMF_KIND_I4), allocatable :: elemConn(:)
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    if ( noActiveImpFields ) return
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered CreateImpMesh', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    ! 1.  Setup
    !
    ! 1.a Set grid pointers
    !
    impGridID = minval(inpmap)
    if ( impGridID.eq.-999 ) impGridID = 1
    call w3setg ( impGridID, mdse, mdst )
    call w3seti ( impGridID, mdse, mdst )
    call w3seto ( impGridID, mdse, mdst )
    if ( impGridID.gt.0 ) then
      call wmsetm ( impGridID, mdse, mdst )
      nproc = naproc
    else
      nproc = nmproc
    endif
    !
    ! 1.b Set arraySpec, staggerLoc, and indexFlag for import fields
    !
    !      call ESMF_ArraySpecSet( impArraySpec2D, rank=2, &
    !        typekind=ESMF_TYPEKIND_RX, rc=rc )
    !      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !      impStaggerLoc = ESMF_STAGGERLOC_CENTER
    !      impIndexFlag = ESMF_INDEX_GLOBAL
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Create ESMF mesh for import, currently without domain decomposition
    !     Note that the ESMF grid layout is dim1=X, dim2=Y
    !
    ! 2.a Create ESMF import mesh
    !
    ! Allocate and fill the node id array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(nodeIds(NX))
      do i = 1,NX
        nodeIds(i)=i
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: The global id's of the nodes resident on this processor
      !        -------------------------------------------------------------------
      !        Allocate global node ids, including ghost nodes (npa=np+ng)
      allocate(nodeIds(npa))
      do i = 1,npa
        nodeIds(i)=iplg(i)
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, nodeIds=', &
    !          ESMF_LOGMSG_INFO)
    !         do i = 1,npa
    !         write(msg,*) trim(cname)//': nodeIds(i)',i, &
    !          ' ',nodeIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, nodeIds=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NX
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',nodeIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(nodeCoords(2*NX))
      do i = 1,NX
        do j = 1,2
          pos=2*(i-1)+j
          if (j == 1) then
            nodeCoords(pos) = xgrd(1,i)
          else
            nodeCoords(pos) = ygrd(1,i)
          endif
        enddo
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Physical coordinates of the nodes
      !        -------------------------------------------------------------------
      allocate(nodeCoords(2*npa))
      do i = 1,npa
        do j = 1,2
          pos=2*(i-1)+j
          if ( j == 1) then
            nodeCoords(pos) = xgrd(1,iplg(i))
          else
            nodeCoords(pos) = ygrd(1,iplg(i))
          endif
        enddo
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, nodeCoords=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(2*npa)
    !        write(msg,*) trim(cname)//': nodeCoords(i)',i, &
    !          ' ',nodeCoords(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, nodeCoords=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(2*NX)
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',nodeCoords(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the node owner array.
    ! Since this mesh is all on PET 0, it’s just set to all 0.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(nodeOwners(NX))
      nodeOwners=0 ! everything on PET 0
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Processor that owns the node
      !        -------------------------------------------------------------------
      allocate(nodeOwners(npa))
      nodeOwners=nodes_global(iplg(1:npa))%domainID-1
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, nodeOwners=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,npa
    !        write(msg,*) trim(cname)//': nodeOwners(i)',i, &
    !          ' ',nodeOwners(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, nodeOwners=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NX
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',nodeOwners(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the element id array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(elemIds(NTRI))
      do i = 1,NTRI
        elemIds(i)=i
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: The global id's of the elements resident on this processor
      !        -------------------------------------------------------------------
      allocate(elemIds(ne))
      do i = 1,ne
        elemIds(i)=ielg(i)
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, elemIds=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,ne
    !        write(msg,*) trim(cname)//': elemIds(i)',i, &
    !          ' ',elemIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, elemIds=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NTRI
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',elemIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the element topology type array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(elemTypes(NTRI))
      do i = 1,NTRI
        elemTypes(i)=ESMF_MESHELEMTYPE_TRI
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Topology of the given element (one of ESMF_MeshElement)
      !        -------------------------------------------------------------------
      allocate(elemTypes(ne))
      do i = 1,ne
        elemTypes(i)=ESMF_MESHELEMTYPE_TRI
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateImpM, elemTypes=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,ne
    !        write(msg,*) trim(cname)//': elemTypes(i)',i, &
    !          ' ',elemTypes(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateImpM, elemTypes=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NTRI
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',elemTypes(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the element connection type array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(elemConn(3*NTRI))
      do i = 1,NTRI
        do j = 1,3
          pos=3*(i-1)+j
          elemConn(pos)=TRIGP(j,i)
        enddo
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Connectivity table. The number of entries should
      !        be equal to the number of nodes in the given topology. The indices
      !        should be the local index (1 based) into the array of nodes that
      !        was declared with MeshAddNodes.
      !        -------------------------------------------------------------------
      !        > INE is local element array. it stores the local node IDs
      !        > first index from 1 to 3.
      !        > second index from 1 to ne.
      allocate(elemConn(3*ne))
      do i = 1,ne
        do j = 1,3
          pos=3*(i-1)+j
          elemConn(pos)=INE(j,i)
        enddo
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, elemConn=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(3*ne)
    !        write(msg,*) trim(cname)//': elemConn(i)',i, &
    !          ' ',elemConn(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, elemConn=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(3*NTRI)
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',elemConn(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    impMesh = ESMF_MeshCreate( parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)

    call ESMF_LogWrite(trim(cname)//': In CreateImpMesh, created impMesh', &
         ESMF_LOGMSG_INFO)
    !
    rc = ESMF_SUCCESS
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving CreateImpMesh', ESMF_LOGMSG_INFO)
    !/
    !/ End of CreateImpMesh ---------------------------------------------- /
    !/
  end subroutine CreateImpMesh
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CreateExpMesh"
  !>
  !> @brief Create ESMF mesh (unstructured) for export fields.
  !>
  !> @details Create an ESMF Mesh for export using the unstructured mesh
  !>  description in W3GDATMD. At present, this export mesh is not domain
  !>  decomposed, but instead is defined on PET 0 only. (In future, when the
  !>  unstructured mesh will run on domain decomposition, we will use that
  !>  decomposition.)
  !>
  !>  Since the internal parallel data is currently stored accross grid points
  !>  in a "card deck" fashion, we will define an intermediate native grid, as
  !>  is done for regular/curvilinear grids, and perform an ESMF regrid to the
  !>  export mesh. This code segment is taken from T. J. Campbell, and
  !>  modified to 1D, because the internal data structure for unstructred
  !>  meshes is an array with dimensions [NX,NY=1].
  !>
  !> @param      gcomp Gridded component
  !> @param[out] rc    Return code
  !>
  !> @author A. J. van der Westhuysen  @date 28-Feb-2018
  !>
  subroutine CreateExpMesh ( gcomp, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |     A. J. van der Westhuysen      |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         28-FEB-2018 |
    !/                  +-----------------------------------+
    !/
    !/    28-Feb-2018 : Origination.                        ( version 6.06 )
    !/
    !  1. Purpose :
    !
    !     Create ESMF mesh (unstructured) for export fields
    !
    !  2. Method :
    !
    !     Create an ESMF Mesh for export using the unstructured mesh description
    !     in W3GDATMD. At present, this export mesh is not domain decomposed,
    !     but instead is defined on PET 0 only. (In future, when the unstructured
    !     mesh will run on domain decomposition, we will use that decomposition.)
    !
    !     Since the internal parallel data is currently stored accross grid points
    !     in a "card deck" fashion, we will define an intermediate native grid, as
    !     is done for regular/curvilinear grids, and perform an ESMF regrid to the
    !     export mesh. This code segment is taken from T. J. Campbell, and
    !     modified to 1D, because the internal data structure for unstructred
    !     meshes is an array with dimensions [NX,NY=1].
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       gcomp     Type   I/O Gridded component
    !       rc        Int.   O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !       !/SHRD  Switch for shared / distributed memory architecture.
    !       !/DIST  Id.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_PDLIB
    use yowNodepool, only: npa, iplg, nodes_global
    use yowElementpool, only: ne, ielg, INE
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    character(128) :: msg
    integer :: nproc, nxproc, nyproc, n, nfac, irp
    real    :: gr, rp, pr, diff
    integer, parameter :: lde = 0
    integer :: ldecnt
    integer :: i, j, pos, ix, iy, isea, jsea, iproc
    integer :: elb(2), eub(2)
    integer(ESMF_KIND_I4), pointer :: iptr(:,:)
    real(ESMF_KIND_RX), pointer :: rptrx(:,:), rptry(:,:)
    real(ESMF_KIND_RX), pointer :: rptr(:,:)
    integer :: arbIndexCount
    integer, allocatable :: arbIndexList(:,:)
    type(ESMF_Field) :: nField, eField
    type(ESMF_Field) :: tmpField
    integer(ESMF_KIND_I4), allocatable :: nodeIds(:)
    real(ESMF_KIND_R8), allocatable    :: nodeCoords(:)
    integer(ESMF_KIND_I4), allocatable :: nodeOwners(:)
    integer(ESMF_KIND_I4), allocatable :: elemIds(:)
    integer(ESMF_KIND_I4), allocatable :: elemTypes(:)
    integer(ESMF_KIND_I4), allocatable :: elemConn(:)
    !
    ! -------------------------------------------------------------------- /
    ! Prep
    !
    rc = ESMF_SUCCESS
    if ( noActiveExpFields ) return
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': entered CreateExpMesh', ESMF_LOGMSG_INFO)
    !
    ! -------------------------------------------------------------------- /
    !     Set flag to indicate that this processor has local native grid storage
    !
    natGridIsLocal = iaproc .gt. 0 .and. iaproc .le. naproc

    ! 1.  Setup
    !
    ! 1.a Set grid pointers
    !
    expGridID = 1 !TODO: only export from grid 1
    call w3setg ( expGridID, mdse, mdst )
    call w3setw ( expGridID, mdse, mdst )
    call w3seta ( expGridID, mdse, mdst )
    call w3seti ( expGridID, mdse, mdst )
    call w3seto ( expGridID, mdse, mdst )
    call wmsetm ( expGridID, mdse, mdst )
    natGridID = expGridID
    nproc = naproc
    !
    ! 1.b Set arraySpec, staggerLoc, and indexFlag for native fields.
    !     NOTE: For unstructured meshes the native grid is a 1D array (NY=1)
    !
    call ESMF_ArraySpecSet( natArraySpec1D, rank=1, &
         typekind=ESMF_TYPEKIND_RX, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    natStaggerLoc = ESMF_STAGGERLOC_CENTER
    natIndexFlag = ESMF_INDEX_DELOCAL
    !
    ! -------------------------------------------------------------------- /
    ! 2.  Create ESMF mesh for export
    !
    ! 2.a Create ESMF export mesh
    !
    ! Allocate and fill the node id array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(nodeIds(NX))
      do i = 1,NX
        nodeIds(i)=i
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: The global id's of the nodes resident on this processor
      !        -------------------------------------------------------------------
      !        Allocate global node ids, including ghost nodes (npa=np+ng)
      allocate(nodeIds(npa))
      do i = 1,npa
        nodeIds(i)=iplg(i)
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, nodeIds=', &
    !          ESMF_LOGMSG_INFO)
    !         do i = 1,npa
    !         write(msg,*) trim(cname)//': nodeIds(i)',i, &
    !          ' ',nodeIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, nodeIds=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NX
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',nodeIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill node coordinate array.
    ! Since this is a 2D Mesh the size is 2x the
    ! number of nodes.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(nodeCoords(2*NX))
      do i = 1,NX
        do j = 1,2
          pos=2*(i-1)+j
          if (j == 1) then
            nodeCoords(pos) = xgrd(1,i)
          else
            nodeCoords(pos) = ygrd(1,i)
          endif
        enddo
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Physical coordinates of the nodes
      !        -------------------------------------------------------------------
      allocate(nodeCoords(2*npa))
      do i = 1,npa
        do j = 1,2
          pos=2*(i-1)+j
          if ( j == 1) then
            nodeCoords(pos) = xgrd(1,iplg(i))
          else
            nodeCoords(pos) = ygrd(1,iplg(i))
          endif
        enddo
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, nodeCoords=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(2*npa)
    !        write(msg,*) trim(cname)//': nodeCoords(i)',i, &
    !          ' ',nodeCoords(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, nodeCoords=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(2*NX)
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',nodeCoords(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the node owner array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(nodeOwners(NX))
      nodeOwners=0 ! TODO: For now, export everything via PET 0
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Processor that owns the node
      !        -------------------------------------------------------------------
      allocate(nodeOwners(npa))
      nodeOwners=nodes_global(iplg(1:npa))%domainID-1
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, nodeOwners=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,npa
    !        write(msg,*) trim(cname)//': nodeOwners(i)',i, &
    !          ' ',nodeOwners(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, nodeOwners=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NX
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',nodeOwners(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the element id array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(elemIds(NTRI))
      do i = 1,NTRI
        elemIds(i)=i
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: The global id's of the elements resident on this processor
      !        -------------------------------------------------------------------
      allocate(elemIds(ne))
      do i = 1,ne
        elemIds(i)=ielg(i)
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, elemIds=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,ne
    !        write(msg,*) trim(cname)//': elemIds(i)',i, &
    !          ' ',elemIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, elemIds=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NTRI
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',elemIds(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the element topology type array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(elemTypes(NTRI))
      do i = 1,NTRI
        elemTypes(i)=ESMF_MESHELEMTYPE_TRI
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Topology of the given element (one of ESMF_MeshElement)
      !        -------------------------------------------------------------------
      allocate(elemTypes(ne))
      do i = 1,ne
        elemTypes(i)=ESMF_MESHELEMTYPE_TRI
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, elemTypes=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,ne
    !        write(msg,*) trim(cname)//': elemTypes(i)',i, &
    !          ' ',elemTypes(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, elemTypes=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,NTRI
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',elemTypes(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    ! Allocate and fill the element connection type array.
#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      allocate(elemConn(3*NTRI))
      do i = 1,NTRI
        do j = 1,3
          pos=3*(i-1)+j
          elemConn(pos)=TRIGP(j,i)
        enddo
      enddo
#ifdef W3_PDLIB
    else
      !        -------------------------------------------------------------------
      !        ESMF Definition: Connectivity table. The number of entries should
      !        be equal to the number of nodes in the given topology. The indices
      !        should be the local index (1 based) into the array of nodes that
      !        was declared with MeshAddNodes.
      !        -------------------------------------------------------------------
      !        > INE is local element array. it stores the local node IDs
      !        > first index from 1 to 3.
      !        > second index from 1 to ne.
      allocate(elemConn(3*ne))
      do i = 1,ne
        do j = 1,3
          pos=3*(i-1)+j
          elemConn(pos)=INE(j,i)
        enddo
      enddo
    endif
    !
    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, elemConn=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(3*ne)
    !        write(msg,*) trim(cname)//': elemConn(i)',i, &
    !          ' ',elemConn(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo
#endif

    !      call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, elemConn=', &
    !          ESMF_LOGMSG_INFO)
    !      do i = 1,(3*NTRI)
    !        write(msg,*) trim(cname)//': ',i, &
    !          ' ',elemConn(i)
    !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    !      enddo

    expMesh = ESMF_MeshCreate( parametricDim=2,spatialDim=2, &
         nodeIds=nodeIds, nodeCoords=nodeCoords, &
         nodeOwners=nodeOwners, elementIds=elemIds,&
         elementTypes=elemTypes, elementConn=elemConn, &
         rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    deallocate(nodeIds)
    deallocate(nodeCoords)
    deallocate(nodeOwners)
    deallocate(elemIds)
    deallocate(elemTypes)
    deallocate(elemConn)
    !
    !     Set flag to indicate that this processor has local export grid storage
    !
    !AW      if (lpet == 0) then
    !AW         call ESMF_GridGet( expMesh, localDECount=ldecnt, rc=rc )
    !AW         if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !AW         expGridIsLocal = ldecnt.gt.0
    !AW      endif

    !
    ! -------------------------------------------------------------------- /
    ! 3.  Create ESMF grid with arbitrary domain decomposition to match
    !     the native domain decomposition of the non-excluded points
    !     Note that the native grid layout is dim1=Y, dim2=X
    !     Note that coordinates and mask are not needed since this
    !     grid is only used to define fields for a redist operation
    !
    ! 3.a Set flag to indicate that this processor has local native grid storage
    !
    natGridIsLocal = iaproc .gt. 0 .and. iaproc .le. naproc

#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      !
      ! 3.b Setup arbitrary sequence index list
      !
      do ipass = 1,2
        if (ipass.eq.2) then
          allocate (arbIndexList(arbIndexCount,2), stat=rc)
          if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
        endif
        arbIndexCount = 0
        ! list local native grid non-excluded points
        if ( natGridIsLocal ) then
          do jsea = 1,nseal
#ifdef W3_DIST
            isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
            isea = jsea
#endif
            arbIndexCount = arbIndexCount+1
            if (ipass.eq.2) then
              ix = mapsf(isea,1)
              iy = mapsf(isea,2)
              ! native grid layout: dim1=Y, dim2=X
              arbIndexList(arbIndexCount,1) = iy
              arbIndexList(arbIndexCount,2) = ix
            endif
          enddo
        endif
      enddo !ipass
      !
      ! 3.c Create ESMF native grid
      !
      natGrid = ESMF_GridCreateNoPeriDim( &
           minIndex=(/ 1, 1/), &
           maxIndex=(/ny,nx/), &
           coordDep1=(/ESMF_DIM_ARB,ESMF_DIM_ARB/), &
           coordDep2=(/ESMF_DIM_ARB,ESMF_DIM_ARB/), &
           arbIndexCount=arbIndexCount, &
           arbIndexList=arbIndexList, &
           coordTypeKind=ESMF_TYPEKIND_RX, &
           coordSys=ESMF_COORDSYS_SPH_DEG, &
           name=trim(cname)//"_native_grid", rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      !
      ! 3.d Deallocate arbitrary sequence index list
      !
      deallocate (arbIndexList, stat=rc)
      if (ESMF_LogFoundDeallocError(rc, PASSTHRU)) return
      !
      ! -------------------------------------------------------------------- /
      ! 4.  Create route handle for redist between native grid domain
      !     decomposition and the export grid domain decomposition
      !
      ! 4.a Create temporary fields
      !
      nField = ESMF_FieldCreate( natGrid, natArraySpec1D, &
           staggerLoc=natStaggerLoc, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      eField = ESMF_FieldCreate(expMesh, typekind=ESMF_TYPEKIND_RX, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      !
      ! 4.b Store route handle
      !
      call ESMF_FieldRedistStore( nField, eField, n2eRH, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      !
      ! 4.c Clean up
      !
      call ESMF_FieldDestroy( nField, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldDestroy( eField, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

#ifdef W3_PDLIB
    endif
#endif

    call ESMF_LogWrite(trim(cname)//': In CreateExpMesh, created expMesh', &
         ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS
    if (verbosity.gt.0) call ESMF_LogWrite(trim(cname)// &
         ': leaving CreateExpMesh', ESMF_LOGMSG_INFO)
    !/
    !/ End of CreateExpMesh ---------------------------------------------- /
    !/
  end subroutine CreateExpMesh
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "SetupImpBmsk"
  !>
  !> @brief Setup background blending mask field for an import field.
  !>
  !> @param bmskField   Blending mask field
  !> @param impField    Import field
  !> @param missingVal  Missing value
  !> @param rc          Return code
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine SetupImpBmsk( bmskField, impField, missingVal, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Setup background blending mask field for an import field
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       bmskField  Type   I/O blending mask field
    !       impField   Type   I   import field
    !       missingVal Real   I   missing value
    !       rc         Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_Field)      :: bmskField
    type(ESMF_Field)      :: impField
    real(ESMF_KIND_RX)    :: missingVal
    integer, optional     :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    real(ESMF_KIND_RX), parameter :: zero = 0.0
    real(ESMF_KIND_RX), parameter :: half = 0.5
    real(ESMF_KIND_RX), parameter ::  one = 1.0
    integer, parameter :: nsig = impHaloWidth-1
    integer, parameter :: niter = 10
    integer, parameter :: iter0 = 1-niter
    integer, parameter :: lde = 0
    integer :: iter, i, j, ii, jj, k, l
    integer :: elb(2), eub(2)
    integer :: tlb(2), tub(2)
    real(ESMF_KIND_RX), pointer :: mptr(:,:), dptr(:,:)
    type(ESMF_Field) :: cmskField
    real(ESMF_KIND_RX), pointer :: bmsk(:,:), cmsk(:,:)
    real(ESMF_KIND_RX) :: bsum, wsum
    real(ESMF_KIND_RX) :: wflt(-nsig:nsig,-nsig:nsig)
    character(ESMF_MAXSTR) :: fnm
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_SETUPIMPBMSK)
    integer :: timeSlice
#endif
    !
    ! -------------------------------------------------------------------- /
    ! Initialize filter
    !
    if (present(rc)) rc = ESMF_SUCCESS

    do l = -nsig,nsig
      do k = -nsig,nsig
        wflt(k,l) = exp( -half*( real(k,ESMF_KIND_RX)**2 &
             + real(l,ESMF_KIND_RX)**2 ) )
      enddo
    enddo
    !
    ! -------------------------------------------------------------------- /
    ! Set up fields and pointers
    !
    call ESMF_FieldGet( impField, name=fnm, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    cmskField = ESMF_FieldCreate( impGrid, impArraySpec2D, &
         totalLWidth=impHaloLWidth, totalUWidth=impHaloUWidth, &
         staggerLoc=impStaggerLoc, indexFlag=impIndexFlag, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if ( impGridIsLocal ) then
      call ESMF_FieldGet( impMask, localDE=lde, farrayPtr=mptr, &
           exclusiveLBound=elb, exclusiveUBound=eub, &
           totalLBound=tlb, totalUBound=tub, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( impField,  localDE=lde, farrayPtr=dptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( bmskField, localDE=lde, farrayPtr=bmsk, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( cmskField, localDE=lde, farrayPtr=cmsk, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Create blending mask
    !
    if ( impGridIsLocal ) then
      do j = elb(2),eub(2)
        do i = elb(1),eub(1)
          if ( nint(dptr(i,j)).eq.nint(missingVal) ) then
            bmsk(i,j) = zero
          else
            bmsk(i,j) = one
          endif
          cmsk(i,j) = bmsk(i,j)
        enddo
      enddo
    endif
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_SETUPIMPBMSK)
    timeSlice = 1
    call ESMF_FieldWrite( bmskField, &
         "wmesmfmd_setupimpbmsk_"//trim(fnm)//".nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#endif

    iter_loop: do iter = iter0,niter

      call ESMF_FieldHalo( bmskField, impHaloRH, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      if ( impGridIsLocal ) then

        j_loop: do j = elb(2),eub(2)
          i_loop: do i = elb(1),eub(1)
            if ( nint(mptr(i,j)).eq.maskValueLand ) cycle i_loop
            if ( nint(dptr(i,j)).eq.nint(missingVal) ) cycle i_loop

            if (iter.le.0) then

              ! initialize blending zone to zero
              l_loop0: do l = -1,1
                jj = j + l
                if ( jj.lt.tlb(2).or.jj.gt.tub(2) ) cycle l_loop0
                k_loop0: do k = -1,1
                  ii = i + k
                  if ( ii.lt.tlb(1).or.ii.gt.tub(1) ) cycle k_loop0
                  if ( nint(mptr(ii,jj)).eq.maskValueLand ) cycle k_loop0
                  if ( bmsk(ii,jj).eq.zero ) cmsk(i,j) = zero
                enddo k_loop0
              enddo l_loop0

            else

              ! iterate filter over blending zone
              bsum = zero
              wsum = zero
              l_loop: do l = -nsig,nsig
                jj = j + l
                if ( jj.lt.tlb(2).or.jj.gt.tub(2) ) cycle l_loop
                k_loop: do k = -nsig,nsig
                  ii = i + k
                  if ( ii.lt.tlb(1).or.ii.gt.tub(1) ) cycle k_loop
                  if ( nint(mptr(ii,jj)).eq.maskValueLand ) cycle k_loop
                  bsum = bsum + wflt(k,l)*bmsk(ii,jj)
                  wsum = wsum + wflt(k,l)
                enddo k_loop
              enddo l_loop
              if ( wsum.gt.zero ) cmsk(i,j) = bsum/wsum

            endif

          enddo i_loop
        enddo j_loop

        do j = elb(2),eub(2)
          do i = elb(1),eub(1)
            if ( nint(mptr(i,j)).eq.maskValueLand ) cycle
            bmsk(i,j) = cmsk(i,j)
          enddo
        enddo

      endif
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_SETUPIMPBMSK)
      timeSlice = timeSlice + 1
      call ESMF_FieldWrite( bmskField, &
           "wmesmfmd_setupimpbmsk_"//trim(fnm)//".nc", &
           overwrite=.true., timeSlice=timeSlice, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
#endif

    enddo iter_loop
    !
    ! -------------------------------------------------------------------- /
    ! Clean up
    !
    call ESMF_FieldDestroy( cmskField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    !/
    !/ End of SetupImpBmsk ----------------------------------------------- /
    !/
  end subroutine SetupImpBmsk
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "BlendImpField"
  !>
  !> @brief Blend import field with background field.
  !>
  !> @param[inout] impField Import field
  !> @param[in] mbgField    Import background field
  !> @param[in] bmskField   Blending mask field
  !> @param[inout] rc       Return code
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine BlendImpField( impField, mbgField, bmskField, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Blend import field with background field
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       impField   Type   I/O import field
    !       mbgField   Type   I   import background field
    !       bmskField  Type   I   blending mask field
    !       rc         Int    I/O Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_Field), intent(inout)  :: impField
    type(ESMF_Field), intent(in)     :: mbgField
    type(ESMF_Field), intent(in)     :: bmskField
    integer, optional, intent(inout) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    real(ESMF_KIND_RX), parameter ::  one = 1.0
    integer, parameter :: lde = 0
    integer :: i, j
    integer :: elb(2), eub(2)
    real(ESMF_KIND_RX), pointer :: mptr(:,:), dptr(:,:), sptr(:,:)
    real(ESMF_KIND_RX), pointer :: bmsk(:,:)
    !
    ! -------------------------------------------------------------------- /
    ! Get array pointers and bounds
    !
    if (present(rc)) rc = ESMF_SUCCESS

    if ( impGridIsLocal ) then
      call ESMF_FieldGet( impMask, localDE=lde, farrayPtr=mptr, &
           exclusiveLBound=elb, exclusiveUBound=eub, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( impField,  localDE=lde, farrayPtr=dptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( mbgField,  localDE=lde, farrayPtr=sptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( bmskField, localDE=lde, farrayPtr=bmsk, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Blend Fields
    !
    if ( impGridIsLocal ) then
      do j = elb(2),eub(2)
        do i = elb(1),eub(1)
          if ( nint(mptr(i,j)).eq.maskValueLand ) cycle
          dptr(i,j) = bmsk(i,j)*dptr(i,j) + (one-bmsk(i,j))*sptr(i,j)
        enddo
      enddo
    endif
    !/
    !/ End of BlendImpField ---------------------------------------------- /
    !/
  end subroutine BlendImpField
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "SetupImpMmsk"
  !>
  !> @brief Setup merging mask field for an import field for the cases
  !>  that model domains does not overlap completely.
  !>
  !> @param mmskField  Merging mask field
  !> @param impField   Import field
  !> @param fillVal    Fill value
  !> @param mskCreated Mask is created or not
  !> @param rc         Return code
  !>
  !> @author U. Turuncoglu  @date 18-May-2021
  !>
  subroutine SetupImpMmsk( mmskField, impField, fillVal, mskCreated, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           U. Turuncoglu           |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         18-May-2021 |
    !/                  +-----------------------------------+
    !/
    !/    18-May-2021 : Origination.                        ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Setup merging mask field for an import field for the cases that
    !     model domains does not overlap completely
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       mmskField   Type   I/O merging mask field
    !       impField    Type   I   import field
    !       fillVal     Real   I   fill value
    !       mskCreated  Log.   I/O mask is created or not
    !       rc          Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_Field)   :: mmskField
    type(ESMF_Field)   :: impField
    real(ESMF_KIND_RX) :: fillVal
    logical            :: mskCreated
    integer, optional  :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    integer, parameter :: lde = 0
    integer :: i, j
    integer :: elb(2), eub(2)
    integer :: tlb(2), tub(2)
    real(ESMF_KIND_RX), pointer :: mptr(:,:), dptr(:,:)
    real(ESMF_KIND_RX), pointer :: mmsk(:,:)
    character(ESMF_MAXSTR) :: fnm
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_SETUPIMPMMSK)
    integer, save :: timeSlice = 1
#endif
    !
    ! -------------------------------------------------------------------- /
    ! Check mask is created or not
    !
    if ( mskCreated ) return
    !
    ! -------------------------------------------------------------------- /
    ! Set up fields and pointers
    !

    call ESMF_FieldGet( impField, name=fnm, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if ( impGridIsLocal ) then
      call ESMF_FieldGet( impMask, localDE=lde, farrayPtr=mptr, &
           exclusiveLBound=elb, exclusiveUBound=eub, &
           totalLBound=tlb, totalUBound=tub, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( impField,  localDE=lde, farrayPtr=dptr, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( mmskField, localDE=lde, farrayPtr=mmsk, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Create merging mask
    !
    if ( impGridIsLocal ) then
      do j = elb(2),eub(2)
        do i = elb(1),eub(1)
          mmsk(i,j) = 0.0
          if (dptr(i,j).lt.fillVal) then
            mmsk(i,j) = 1.0
          end if
        enddo
      enddo
    endif
    !
    ! -------------------------------------------------------------------- /
    ! Set mask created flag
    !
    mskCreated = .true.
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_SETUPIMPMMSK)
    call ESMF_FieldWrite( mmskField, &
         "wmesmfmd_setupimpmmsk_"//trim(fnm)//".nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice+1
#endif
    !/
    !/ End of SetupImpMmsk ----------------------------------------------- /
    !/
  end subroutine SetupImpMmsk
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "FieldFill"
  !>
  !> @brief Fill ESMF field.
  !>
  !> @param field   ESMF field
  !> @param fillVal Fill value
  !> @param rc      Return code
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine FieldFill(field, fillVal, rc)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/    09-Aug-2017 : Remove mask parameter.              ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Fill ESMF Field
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       field      Type   I/O ESMF field
    !       fillVal    Real   I   fill value
    !       rc         Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_Field)                 :: field
    real(ESMF_KIND_RX)               :: fillVal
    integer,               optional  :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    integer :: ldecnt, lde, i, j, k
    integer :: rank
    integer :: lb1(1), ub1(1)
    integer :: lb2(2), ub2(2)
    integer :: lb3(3), ub3(3)
    real(ESMF_KIND_RX), pointer :: dptr1(:)
    real(ESMF_KIND_RX), pointer :: dptr2(:,:)
    real(ESMF_KIND_RX), pointer :: dptr3(:,:,:)
    integer, parameter :: iwt=10
    real(8) :: wstime, wftime
    !
    ! -------------------------------------------------------------------- /
    ! Fill Field
    !
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMWtime(wstime)

    call ESMF_FieldGet(field, localDECount=ldecnt, rank=rank, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    if (rank.ne.1.and.rank.ne.2.and.rank.ne.3) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
           msg='FieldFill: rank must be 1, 2 or 3')
      return ! bail out
    endif

    do lde=0,ldecnt-1

      if (rank.eq.1) then
        call ESMF_FieldGet(field, localDE=lde, farrayPtr=dptr1, &
             exclusiveLBound=lb1, exclusiveUBound=ub1, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      elseif (rank.eq.2) then
        call ESMF_FieldGet(field, localDE=lde, farrayPtr=dptr2, &
             exclusiveLBound=lb2, exclusiveUBound=ub2, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      else
        call ESMF_FieldGet(field, localDE=lde, farrayPtr=dptr3, &
             exclusiveLBound=lb3, exclusiveUBound=ub3, rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif

      if (rank.eq.1) then
        dptr1(lb1(1):ub1(1)) = fillVal
      elseif (rank.eq.2) then
        dptr2(lb2(1):ub2(1),lb2(2):ub2(2)) = fillVal
      else
        dptr3(lb3(1):ub3(1),lb3(2):ub3(2),lb3(3):ub3(3)) = fillVal
      endif

    enddo

    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1
    !/
    !/ End of FieldFill ------------------------------------------------- /
    !/
  end subroutine FieldFill
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "FieldGather"
  !>
  !> @brief All gather of ESMF field.
  !>
  !> @param field ESMF field
  !> @param n1    Dimension of output array
  !> @param n2    Dimension of output array
  !> @param fout  Global output array
  !> @param rc    Return code
  !>
  !> @author  T. J. Campbell
  !> @author  A. J. van der Westhuysen
  !> @date    20-Jan-2017
  !>
  subroutine FieldGather(field, n1, n2, fout, rc)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |     A. J. van der Westhuysen      |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/    27-Feb-2018 : Modification for use with UNGTYPE   ( version 6.06 )
    !/
    !  1. Purpose :
    !
    !     All gather of ESMF field
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       field      Type   I/O ESMF field
    !       n1,n2      Int    I   Dimensions of output array
    !       fout       R.A.   O   global output array
    !       rc         Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_PDLIB
    use yowNodepool, only: np, iplg
    use yowrankModule, only: rank
#endif
    !/
    implicit none
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    type(ESMF_Field)                 :: field
    integer                          :: n1, n2
    real                             :: fout(n1,n2)
    integer,               optional  :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    character(500) :: msg
    integer :: i, j, k, ir, ip, count
    real(ESMF_KIND_RX) :: floc(n1,n2)
    real(ESMF_KIND_RX) :: floc1d(n1), floc1dary(n1*n2)
#ifdef W3_PDLIB
    real(ESMF_KIND_R8), pointer :: fldptr(:)
#endif
    integer, parameter :: iwt=9
    real(8) :: wstime, wftime
    !
    ! -------------------------------------------------------------------- /
    ! Gather Field
    !
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_VMWtime(wstime)

    if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
      count = n1 * n2
      floc = 0.
      floc1dary = 0.
      call ESMF_FieldGather( field,  floc, rootPet=0, vm=vm, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do j=1,n2
        do i=1,n1
          floc1dary(i+(j-1)*n1) = floc(i,j)
        enddo
      enddo
      call ESMF_VMbroadcast( vm, bcstData=floc1dary, count=count, rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      do j=1,n2
        do i=1,n1
          fout(i,j) = floc1dary(i+(j-1)*n1)
        enddo
      enddo
    elseif (GTYPE.eq.UNGTYPE) then
      count = n1
      floc1d = 0.
      call ESMF_FieldGather( field,  floc1d, rootPet=0, vm=vm, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_VMbroadcast( vm, bcstData=floc1d, count=count, rootPet=0, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
#ifdef W3_PDLIB
      if ( LPDLIB .EQV. .FALSE. ) then
#endif
        do k = 1, n1
          fout(k,1) = floc1d(k)
        enddo
#ifdef W3_PDLIB
      else
        count = 0
        do ir = 1, npet
          do ip = 1, rank(ir)%np
            count = count+1
            fout(rank(ir)%iplg(ip),1) = floc1d(count)
            !             write(msg,*) trim(cname)//': count,ir,ip =',count, &
            !                ir,ip
            !             call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
          enddo
        enddo
      endif
#endif

#ifdef W3_PDLIB
      !      call ESMF_LogWrite(trim(cname)//': In FieldGather, fout(k,1)=', &
      !          ESMF_LOGMSG_INFO)
      !      do k = 1, n1
      !         write(msg,*) trim(cname)//': fout(k,1) =',k, &
      !          ' ',fout(k,1)
      !         call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
      !      enddo
#endif

    endif

    call ESMF_VMWtime(wftime)
    wtime(iwt) = wtime(iwt) + wftime - wstime
    wtcnt(iwt) = wtcnt(iwt) + 1

    !/
    !/ End of FieldGather ------------------------------------------------ /
    !/
  end subroutine FieldGather
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "FieldIndex"
  !>
  !> @brief Return index associated with field name.
  !>
  !> @param    fnameList Array of field names
  !> @param    fname     Field name
  !> @param    rc        Return code
  !> @returns  indx      Returned index of fname
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  function FieldIndex ( fnameList, fname, rc ) result (indx)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Return index associated with field name
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       fnameList StrA   I   Array of field names
    !       fname     Str    I   Field name
    !       rc        Int.   O   Return code
    !       indx      Int    I   Returned index of fname
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    character (len=*) :: fnameList(:)
    character (len=*) :: fname
    integer           :: rc
    integer           :: indx
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    integer       :: i, check
    !
    ! -------------------------------------------------------------------- /
    ! Find name in fnameList that matches fname
    !
    check = lbound(fnameList,1) - 1
    indx = check
    do i = lbound(fnameList,1),ubound(fnameList,1)
      if ( trim(fnameList(i)).eq.trim(fname) ) then
        indx = i
        exit
      endif
    enddo
    if ( indx.eq.check ) then
      call ESMF_LogSetError(ESMF_FAILURE, rcToReturn=rc, &
           msg='FieldIndex: input name ('//fname//') not in list')
    endif
    !/
    !/ End of FieldIndex ------------------------------------------------- /
    !/
  end function FieldIndex
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "PrintTimers"
  !>
  !> @brief Print wallclock timers to ESMF log file.
  !>
  !> @param cname  Name of component
  !> @param wtnam  Timer names
  !> @param wtcnt  Timer counts
  !> @param wtime  Timers
  !>
  !> @author T. J. Campbell  @date 20-Jan-2017
  !>
  subroutine PrintTimers ( cname, wtnam, wtcnt, wtime )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         20-Jan-2017 |
    !/                  +-----------------------------------+
    !/
    !/    20-Jan-2017 : Origination.                        ( version 6.02 )
    !/
    !  1. Purpose :
    !
    !     Print wallclock timers to ESMF log file
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       cname     Str    I   Name of component
    !       wtnam     Str    I   Timer names
    !       wtcnt     Int    I   Timer counts
    !       wtime     R8     I   Timers
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    character(*) :: cname
    character(*) :: wtnam(:)
    integer      :: wtcnt(:)
    real(8)      :: wtime(:)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(128) :: msg
    integer :: k
    !
    ! -------------------------------------------------------------------- /
    ! Print timers to ESMF log file
    !
    write(msg,1) trim(cname),"timer","count","time"
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    do k=lbound(wtcnt,1),ubound(wtcnt,1)
      write(msg,2) trim(cname),trim(wtnam(k)),wtcnt(k),wtime(k)
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
    enddo
    !
    ! -------------------------------------------------------------------- /
    ! Formats
    !
1   format(a,': wtime: ',a20,a10,a14)
2   format(a,': wtime: ',a20,i10,e14.6)
    !/
    !/ End of PrintTimers ------------------------------------------------ /
    !/
  end subroutine PrintTimers
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CalcDecomp"
  !>
  !> @brief Calculate a 2D processor layout
  !>
  !> @param[in]    nx       Grid dimension x
  !> @param[in]    ny       Grid dimension y
  !> @param[in]    nproc    Total processor count
  !> @param[in]    npmin    Min number of grid points per tile per direction
  !> @param[in]    adjust   Enable/disable adjusting proc count downward
  !> @param[out]   nxproc   Processor count in x-direction
  !> @param[out]   nyproc   Processor count in y-direction
  !> @param[inout] rc       Return code
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine CalcDecomp ( nx, ny, nproc, npmin, adjust, nxproc, nyproc, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Calculate a 2D processor layout
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       nx,ny     Int    I   Grid dimensions
    !       nproc     Int    I   Total processor count
    !       npmin     Int    I   Min number of grid points per tile per direction
    !       adjust    Log    I   Enable/disable adjusting proc count downward
    !       nxproc    Int    O   Processor count in x-direction
    !       nyproc    Int    O   Processor count in y-direction
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    integer, intent(in)    :: nx, ny
    integer, intent(in)    :: nproc
    integer, intent(in)    :: npmin
    logical, intent(in)    :: adjust
    integer, intent(out)   :: nxproc
    integer, intent(out)   :: nyproc
    integer, intent(inout) :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    integer, parameter :: k = 4
    integer :: mproc, n, nfac, irp
    real(k) :: gr, rp, pr, diff, npx, npy
    character(256) :: msg
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

    if ( nx.gt.ny ) then
      gr = real(nx,k)/real(ny,k)
    else
      gr = real(ny,k)/real(nx,k)
    endif

    mproc = nproc
    mproc_loop: do

      irp = int(sqrt(real(mproc,k)))
      diff = huge(gr)
      nfac = mproc
      do n = irp,mproc
        if ( mod(mproc,n).ne.0 ) cycle
        pr = real(n**2,k)/real(mproc,k)
        if ( abs(gr-pr) < diff ) then
          diff = abs(gr-pr)
          nfac = n
        endif
      enddo
      if ( nx.gt.ny ) then
        nxproc = nfac
        nyproc = mproc/nfac
      else
        nxproc = mproc/nfac
        nyproc = nfac
      endif

      npx = nx/real(nxproc,k)
      npy = ny/real(nyproc,k)
      if (.not.adjust) exit mproc_loop

      if ( npx.ge.npmin .and. npy.ge.npmin ) then
        exit mproc_loop
      else
        if ( mproc.gt.1 ) then
          mproc = mproc - 1
        else
          exit mproc_loop
        endif
      endif

    enddo mproc_loop

    if ( npx.lt.npmin .or. npy.lt.npmin ) then
      write(msg,'(a,7i6)') 'proc count is too large for grid size:', &
           nx,ny,npmin,nproc,mproc,nxproc,nyproc
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
      rc = ESMF_FAILURE
    endif
    !/
    !/ End of CalcDecomp ------------------------------------------------- /
    !/
  end subroutine CalcDecomp
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "GetEnvValue"
  !>
  !> @brief Get value of environment variable.
  !>
  !> @param cenv  Name of environment variable
  !> @param cval  Value of environment variable
  !> @param rc    Return code
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine GetEnvValue ( cenv, cval, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Get value of environment variable
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       cenv      Str    I   Name of environment variable
    !       cval      Str    O   Value of environment variable
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    character(*) :: cenv
    character(*) :: cval
    integer      :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(256) :: msg
    integer :: length, istat
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS
    call get_environment_variable( name=trim(cenv), value=cval, &
         length=length, trim_name=.false., status=istat )
    if (istat.lt.0) then
      ! The VALUE argument is present and has a length less than
      ! the significant length of the environment variable value.
      write(msg,'(a,i3,a)') "Length of input variable", &
           " is less than length of environment variable " &
           //trim(cenv)//" value (",length,")."
      call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
      rc = istat
    elseif (istat.gt.0) then
      ! 1: The specified environment variable NAME does not exist.
      ! 2: The processor does not support environment variables.
      !>2: Some other error condition occured.
      cval=" "
    endif
    if (length.eq.0) cval=" "
    !/
    !/ End of GetEnvValue ------------------------------------------------ /
    !/
  end subroutine GetEnvValue
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "GetZlevels"
  !>
  !> @brief Get array of z-levels from zlfile for SDC.
  !>
  !> @param rc  Return code
  !>
  !> @author T. J. Campbell  @date 09-Aug-2017
  !>
  subroutine GetZlevels ( rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Get array of z-levels from zlfile for SDC
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    integer      :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(256) :: msg
    integer :: k, iunit, ierr
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

    if (len_trim(zlfile).eq.0 .or. trim(zlfile) .eq. 'none') then

      nz = 1
      allocate(zl(nz), stat=rc)
      if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
      zl(1) = 0

    else

      call ESMF_UtilIOUnitGet(iunit, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      open(unit=iunit, file=trim(zlfile), form='formatted', &
           status='old', access='sequential', iostat=ierr)
      if (ierr.ne.0) then
        msg = "failed opening "//trim(zlfile)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif
      read(iunit, fmt=*, iostat=ierr) nz
      if (ierr.ne.0) then
        msg = "read nz failed: "//trim(zlfile)
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif
      allocate(zl(nz), stat=rc)
      if (ESMF_LogFoundAllocError(rc, PASSTHRU)) return
      do k=1,nz
        read(iunit, fmt=*, iostat=ierr) zl(k)
        if (ierr.ne.0) then
          msg = "read zl failed: "//trim(zlfile)
          call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_ERROR)
          rc = ESMF_FAILURE
          return
        endif
      enddo
      close(iunit)

    endif
    !/
    !/ End of GetZlevels ------------------------------------------------- /
    !/
  end subroutine GetZlevels
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CalcCharnk"
  !>
  !> @brief Calculate Charnock for export.
  !>
  !> @param chkField  2D Charnock export field
  !> @param rc        Return code
  !>
  !> @author T. J. Campbell
  !> @date   09-Aug-2017
  subroutine CalcCharnk ( chkField, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Calculate Charnock for export
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       chkField  Type   I/O 2D Charnock export field
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_Field) :: chkField
    integer          :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    real   , parameter :: zero  = 0.0
    logical, save :: firstCall = .true.
    integer :: isea, jsea
    real    :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr, &
         tauwx, tauwy, cd, z0, fmeanws, dlwmean
    logical :: llws(nspec)
    type(ESMF_Field) :: chknField
    real(ESMF_KIND_RX), pointer :: chkn(:)
    integer, save :: timeSlice = 1
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

    chknField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call FieldFill( chknField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if ( natGridIsLocal ) then

      call ESMF_FieldGet( chknField, farrayPtr=chkn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      jsea_loop: do jsea = 1,nseal
#ifdef W3_DIST
        isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
        isea = jsea
#endif
        if ( firstCall ) then
          charn(jsea) = zero
#ifdef W3_ST3
          llws(:) = .true.
          ustar = zero
          ustdr = zero
          tauwx = zero
          tauwy = zero
          call w3spr3( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
               emean, fmean, fmean1, wnmean, amax,         &
               u10(isea), u10d(isea), ustar, ustdr, tauwx, &
               tauwy, cd, z0, charn(jsea), llws, fmeanws )
#endif
#ifdef W3_ST4
          llws(:) = .true.
          ustar = zero
          ustdr = zero
          tauwx = zero
          tauwy = zero
          call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
               emean, fmean, fmean1, wnmean, amax,         &
               u10(isea), u10d(isea), ustar, ustdr, tauwx, &
               tauwy, cd, z0, charn(jsea), llws, fmeanws,  &
               dlwmean )
#endif
        endif !firstCall
        chkn(jsea) = charn(jsea)
      enddo jsea_loop

    endif !natGridIsLocal

    call ESMF_FieldRedist( chknField, chkField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call ESMF_FieldDestroy( chknField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    firstCall = .false.

#ifdef TEST_WMESMFMD_CHARNK
    call ESMF_FieldWrite( chkField, "wmesmfmd_charnk_chk.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    !/
    !/ End of CalcCharnk ------------------------------------------------- /
    !/
  end subroutine CalcCharnk
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CalcRoughl"
  !>
  !> @brief Calculate 2D wave roughness length for export.
  !>
  !> @param wrlField  2D roughness length export field
  !> @param rc        Return code
  !>
  !> @author T. J. Campbell
  !> @date   09-Aug-2017
  subroutine CalcRoughl ( wrlField, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Calculate 2D wave roughness length for export
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       wrlField  Type   I/O 2D roughness length export field
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    type(ESMF_Field) :: wrlField
    integer          :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    real   , parameter :: zero  = 0.0
    logical, save :: firstCall = .true.
    integer :: isea, jsea, ix, iy
    real    :: emean, fmean, fmean1, wnmean, amax, ustar, ustdr, &
         tauwx, tauwy, cd, z0, fmeanws, dlwmean
    logical :: llws(nspec)
    type(ESMF_Field) :: wrlnField
    real(ESMF_KIND_RX), pointer :: wrln(:)
    integer, save :: timeSlice = 1
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

    wrlnField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call FieldFill( wrlnField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if ( natGridIsLocal ) then

      call ESMF_FieldGet( wrlnField, farrayPtr=wrln, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      jsea_loop: do jsea = 1,nseal
#ifdef W3_DIST
        isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
        isea = jsea
#endif
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
        IF ( MAPSTA(IY,IX) .EQ. 1 ) THEN
          if ( firstCall ) then
            charn(jsea) = zero
#ifdef W3_ST3
            llws(:) = .true.
            ustar = zero
            ustdr = zero
            tauwx = zero
            tauwy = zero
            call w3spr3( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
                 emean, fmean, fmean1, wnmean, amax,         &
                 u10(isea), u10d(isea), ustar, ustdr, tauwx, &
                 tauwy, cd, z0, charn(jsea), llws, fmeanws )
#endif
#ifdef W3_ST4
            llws(:) = .true.
            ustar = zero
            ustdr = zero
            tauwx = zero
            tauwy = zero
            call w3spr4( va(:,jsea), cg(1:nk,isea), wn(1:nk,isea),   &
                 emean, fmean, fmean1, wnmean, amax,         &
                 u10(isea), u10d(isea), ustar, ustdr, tauwx, &
                 tauwy, cd, z0, charn(jsea), llws, fmeanws,  &
                 dlwmean )
#endif
          endif !firstCall
          wrln(jsea) = charn(jsea)*ust(isea)**2/grav
        endif
      enddo jsea_loop

    endif !natGridIsLocal

    call ESMF_FieldRedist( wrlnField, wrlField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call ESMF_FieldDestroy( wrlnField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    firstCall = .false.

#ifdef TEST_WMESMFMD_ROUGHL
    call ESMF_FieldWrite( wrlField, "wmesmfmd_roughl_wrl.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    !/
    !/ End of CalcRoughl ------------------------------------------------- /
    !/
  end subroutine CalcRoughl
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CalcBotcur"
  !>
  !> @brief Calculate wave-bottom currents for export.
  !>
  !> @details Madsen, O. S. (1994), ICCE.
  !>
  !> @param a         Input spectra (in par list to change shape)
  !> @param wbxField  WBC 2D eastward-component export field
  !> @param wbyField  WBC 2D northward-component export field
  !> @param wbpField  WBC 2D period export field
  !> @param rc        Return code
  !>
  !> @author T. J. Campbell
  !> @date   09-Aug-2017
  subroutine CalcBotcur ( a, wbxField, wbyField, wbpField, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Calculate wave-bottom currents for export
    !
    !  2. Method :
    !
    !   > Madsen, O. S. (1994), ICCE
    !
    !                          //
    !   U_bot       = sqrt( 2*|| S_ub dsig dtheta )
    !   (magnitude)          //
    !
    !                            //
    !                           || S_ub*sin(theta) dsig dtheta
    !                          //
    !   phi_b       = arctan( ---------------------------------- )
    !   (direction)              //
    !                           || S_ub*cos(theta) dsig dtheta
    !                          //
    !
    !                              //
    !                             || S_ub dsig dtheta
    !                            //
    !   U_bp        =  2*pi * ----------------------------
    !   (period)                 //
    !                           || sig*S_ub dsig dtheta
    !                          //
    !
    ! Where:
    !   S_ub(theta,k) = near-bottom orbital velocity spectrum
    !                 = ( sig^2 / sinh^2(kD) ) *  E(theta,k) / Cg
    !                 = ( sig^3 / sinh^2(kD) ) * Ac(theta,k) / Cg
    !   Ac(theta,k) = wave action density
    !   D = depth
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       a         Real   I   Input spectra (in par list to change shape)
    !       wbxField  Type   I/O WBC 2D eastward-component export field
    !       wbyField  Type   I/O WBC 2D northward-component export field
    !       wbpField  Type   I/O WBC 2D period export field
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    real             :: a(nth,nk,0:nseal)
    type(ESMF_Field) :: wbxField
    type(ESMF_Field) :: wbyField
    type(ESMF_Field) :: wbpField
    integer          :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    real(8), parameter :: zero  = 0.0
    real(8), parameter :: half  = 0.5
    real(8), parameter ::  one  = 1.0
    real(8), parameter ::  two  = 2.0
    ! kdmin = 1e-7: sinh(kdmin)**2 ~ 1e-14
    real(8), parameter :: kdmin = 1e-7
    ! kdmax = 18.0:  1/sinh(kdmax)**2 ~ 1e-14
    real(8), parameter :: kdmax = 18.0
    integer :: isea, jsea, ik, ith
    real(8) :: depth
    real(8) :: kd, fack, fkd, aka, akx, aky, abr, ubr, ubx, uby, dir
    real(8), allocatable :: sig2(:)
    type(ESMF_Field) :: wbxnField, wbynField, wbpnField
    real(ESMF_KIND_RX), pointer :: wbxn(:), wbyn(:), wbpn(:)
    integer, save :: timeSlice = 1
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

    wbxnField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    wbynField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    wbpnField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call FieldFill( wbxnField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( wbynField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( wbpnField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if ( natGridIsLocal ) then

      call ESMF_FieldGet( wbxnField, farrayPtr=wbxn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( wbynField, farrayPtr=wbyn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( wbpnField, farrayPtr=wbpn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      allocate( sig2(1:nk) )
      sig2(1:nk) = sig(1:nk)**2

      jsea_loop: do jsea = 1,nseal
#ifdef W3_DIST
        isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
        isea = jsea
#endif
        if ( dw(isea).le.zero ) cycle jsea_loop
        depth = max(dmin,dw(isea))
#ifdef USE_W3OUTG_FOR_EXPORT
        if ( aba(jsea).le.zero ) cycle jsea_loop
        if ( uba(jsea).le.zero ) cycle jsea_loop
        wbxn(jsea) = uba(jsea)*cos(ubd(jsea))
        wbyn(jsea) = uba(jsea)*sin(ubd(jsea))
        wbpn(jsea) = tpi*aba(jsea)/uba(jsea)
#else
        abr = zero
        ubr = zero
        ubx = zero
        uby = zero
        ik_loop: do ik = 1,nk
          aka = zero
          akx = zero
          aky = zero
          ith_loop: do ith = 1,nth
            aka = aka + a(ith,ik,jsea)
            akx = akx + a(ith,ik,jsea)*ecos(ith)
            aky = aky + a(ith,ik,jsea)*esin(ith)
          enddo ith_loop
          fack = dden(ik)/cg(ik,isea)
          kd = max(kdmin,min(kdmax,wn(ik,isea)*depth))
          fkd = fack/sinh(kd)**2
          abr = abr + aka*fkd
          ubr = ubr + aka*sig2(ik)*fkd
          ubx = ubx + akx*sig2(ik)*fkd
          uby = uby + aky*sig2(ik)*fkd
        enddo ik_loop
        if ( abr.le.zero .or. ubr.le.zero ) cycle jsea_loop
        abr = sqrt(two*abr)
        ubr = sqrt(two*ubr)
        dir = atan2(uby,ubx)
        wbxn(jsea) = ubr*cos(dir)
        wbyn(jsea) = ubr*sin(dir)
        wbpn(jsea) = tpi*abr/ubr
#endif
      enddo jsea_loop

      deallocate( sig2 )

    endif !natGridIsLocal

    call ESMF_FieldRedist( wbxnField, wbxField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( wbynField, wbyField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( wbpnField, wbpField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call ESMF_FieldDestroy( wbxnField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( wbynField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( wbpnField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

#ifdef TEST_WMESMFMD_BOTCUR
    call ESMF_FieldWrite( wbxField, "wmesmfmd_botcur_wbx.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( wbyField, "wmesmfmd_botcur_wby.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( wbpField, "wmesmfmd_botcur_wbp.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    !/
    !/ End of CalcBotcur ------------------------------------------------- /
    !/
  end subroutine CalcBotcur
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CalcRadstr2D"
  !>
  !> @brief Calculate 2D radiation stresses for export.
  !>
  !> @param[inout] a         Input spectra (in par list to change shape)
  !> @param[inout] sxxField  RS 2D eastward-component export field
  !> @param[inout] sxyField  RS 2D eastward-northward-component export field
  !> @param[inout] syyField  RS 2D northward-component field
  !> @param[inout] rc        Return code
  !>
  !> @author T. J. Campbell
  !> @date   09-Aug-2017
  !>
  subroutine CalcRadstr2D ( a, sxxField, sxyField, syyField, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |     A. J. van der Westhuysen      |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/    27-Feb-2018 : Modification for use with UNGTYPE   ( version 6.06 )
    !/
    !  1. Purpose :
    !
    !     Calculate 2D radiation stresses for export
    !
    !  2. Method :
    !
    ! Radiation stresses are defined as:
    !
    !                   //
    !   Sxx = rho grav || (N*cos^2(theta) + N - 1/2) * sig*Ac(theta,k)/Cg dsig dtheta
    !                 //
    !                   //
    !   Sxy = rho grav ||  N*sin(theta)*cos(theta)   * sig*Ac(theta,k)/Cg dsig dtheta
    !                 //
    !                   //
    !   Syy = rho grav || (N*sin^2(theta) + N - 1/2) * sig*Ac(theta,k)/Cg dsig dtheta
    !                 //
    !
    ! Where:
    !   rho = density of sea water
    !   grav = acceleration due to gravity
    !   Ac(theta,k) = wave action density
    !   N = Cg/C = ratio of group velocity and phase velocity
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       a         Real   I   Input spectra (in par list to change shape)
    !       sxxField  Type   I/O RS 2D eastward-component export field
    !       sxyField  Type   I/O RS 2D eastward-northward-component export field
    !       syyField  Type   I/O RS 2D northward-component export field
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_PDLIB
    use yowNodepool, only: np, iplg
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    real             :: a(nth,nk,0:nseal)
    type(ESMF_Field) :: sxxField
    type(ESMF_Field) :: sxyField
    type(ESMF_Field) :: syyField
    integer          :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    character(ESMF_MAXSTR) :: cname
    character(128) :: msg
    real(8), parameter :: zero  = 0.0
    real(8), parameter :: half  = 0.5
    real(8), parameter ::  one  = 1.0
    real(8), parameter ::  two  = 2.0
    integer :: isea, jsea, ik, ith
    real(8) :: sxxs, sxys, syys
    real(8) :: akxx, akxy, akyy, cgoc, facd, fack, facs
    type(ESMF_Field) :: sxxnField, sxynField, syynField
    real(ESMF_KIND_RX), pointer :: sxxn(:), sxyn(:), syyn(:)
    integer, save :: timeSlice = 1
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

    !     For regular and curvilinear grids the native grid has a 2D
    !     layout, whereas for unstructured meshes it is a 1D array
    if ( (GTYPE.eq.RLGTYPE).or.(GTYPE.eq.CLGTYPE) ) then
      sxxnField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
           staggerLoc=natStaggerLoc, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      sxynField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
           staggerLoc=natStaggerLoc, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      syynField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
           staggerLoc=natStaggerLoc, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
    elseif (GTYPE.eq.UNGTYPE) then
#ifdef W3_PDLIB
      if ( LPDLIB .EQV. .FALSE. ) then
#endif
        sxxnField = ESMF_FieldCreate( natGrid, natArraySpec1D, &
             staggerLoc=natStaggerLoc, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        sxynField = ESMF_FieldCreate( natGrid, natArraySpec1D, &
             staggerLoc=natStaggerLoc, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        syynField = ESMF_FieldCreate( natGrid, natArraySpec1D, &
             staggerLoc=natStaggerLoc, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
#ifdef W3_PDLIB
      endif
#endif
    endif

#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      call FieldFill( sxxnField, zeroValue, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call FieldFill( sxynField, zeroValue, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call FieldFill( syynField, zeroValue, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
#ifdef W3_PDLIB
    endif
#endif

    if ( natGridIsLocal ) then

#ifdef W3_PDLIB
      if ( LPDLIB .EQV. .FALSE. ) then
        !        Use auxiliary native grid/mesh to populate and redistribute data
#endif
        call ESMF_FieldGet( sxxnField, farrayPtr=sxxn, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call ESMF_FieldGet( sxynField, farrayPtr=sxyn, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call ESMF_FieldGet( syynField, farrayPtr=syyn, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
#ifdef W3_PDLIB
      else
        !        Use single domain-decomposed native mesh to populate and communicate data
        call ESMF_FieldGet( sxxField, farrayPtr=sxxn, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call ESMF_FieldGet( sxyField, farrayPtr=sxyn, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
        call ESMF_FieldGet( syyField, farrayPtr=syyn, rc=rc )
        if (ESMF_LogFoundError(rc, PASSTHRU)) return
      endif
#endif

      facd = dwat*grav
#ifdef W3_PDLIB
      if ( LPDLIB .EQV. .FALSE. ) then
#endif
        jsea_loop: do jsea = 1,nseal
#ifdef W3_DIST
          isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
          isea = jsea
#endif
          if ( dw(isea).le.zero ) cycle jsea_loop
#ifdef USE_W3OUTG_FOR_EXPORT
          sxxn(jsea) = sxx(jsea)
          sxyn(jsea) = sxy(jsea)
          syyn(jsea) = syy(jsea)
#else
          sxxs = zero
          sxys = zero
          syys = zero
          ik_loop: do ik = 1,nk
            akxx = zero
            akxy = zero
            akyy = zero
            cgoc = cg(ik,isea)*wn(ik,isea)/sig(ik)
            cgoc = min(one,max(half,cgoc))
            ith_loop: do ith = 1,nth
              akxx = akxx + (cgoc*(ec2(ith)+one)-half)*a(ith,ik,jsea)
              akxy = akxy + cgoc*esc(ith)*a(ith,ik,jsea)
              akyy = akyy + (cgoc*(es2(ith)+one)-half)*a(ith,ik,jsea)
            enddo ith_loop
            fack = dden(ik)/cg(ik,isea)
            sxxs = sxxs + akxx*fack
            sxys = sxys + akxy*fack
            syys = syys + akyy*fack
          enddo ik_loop
          facs = (one+fte/cg(nk,isea))*facd
          sxxn(jsea) = sxxs*facs
          sxyn(jsea) = sxys*facs
          syyn(jsea) = syys*facs
#endif
        enddo jsea_loop
#ifdef W3_PDLIB
      else
        jsea_loop2: do jsea = 1,np
          isea = iplg(jsea)
          !          if ( dw(isea).le.zero ) cycle jsea_loop
          sxxn(jsea) = sxx(jsea)
          sxyn(jsea) = sxy(jsea)
          syyn(jsea) = syy(jsea)
          !        write(msg,*) trim(cname)//' sxxn', sxxn(jsea)
          !        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO)
        enddo jsea_loop2
      endif
#endif

    endif !natGridIsLocal

#ifdef W3_PDLIB
    if ( LPDLIB .EQV. .FALSE. ) then
#endif
      call ESMF_FieldRedist( sxxnField, sxxField, n2eRH, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldRedist( sxynField, sxyField, n2eRH, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldRedist( syynField, syyField, n2eRH, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      call ESMF_FieldDestroy( sxxnField, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldDestroy( sxynField, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldDestroy( syynField, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
#ifdef W3_PDLIB
    endif
#endif

#ifdef TEST_WMESMFMD_RADSTR2D
    call ESMF_FieldWrite( sxxField, "wmesmfmd_radstr2d_sxx.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( sxyField, "wmesmfmd_radstr2d_sxy.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( syyField, "wmesmfmd_radstr2d_syy.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    !/
    !/ End of CalcRadstr2D ----------------------------------------------- /
    !/
  end subroutine CalcRadstr2D
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CalcStokes3D"
  !>
  !> @brief Calculate 3D Stokes drift current for export.
  !>
  !> @param a        Input spectra (in par list to change shape)
  !> @param usxField 3D SDC eastward-component export field
  !> @param usyField 3D SDC northward-component export field
  !> @param rc       Return code
  !>
  !> @author T. J. Campbell
  !> @date   09-Aug-2017
  !>
  subroutine CalcStokes3D ( a, usxField, usyField, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        T. J. Campbell, NRL        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Aug-2017 |
    !/                  +-----------------------------------+
    !/
    !/    09-Aug-2017 : Origination.                        ( version 6.03 )
    !/
    !  1. Purpose :
    !
    !     Calculate 3D Stokes drift current for export
    !
    !  2. Method :
    !
    !    Kenyon, K. E. (1969), J. Geophys. R., Vol 74, No 28, p 6991
    !
    !    U_vec(z)
    !             //
    !      = 2 g || ( F(f,theta) k_vec/C cosh(2k(D+z))/sinh(2kD) ) dsig dtheta
    !           //
    !
    !          //
    !      =  || (Ac(k,theta) sig^2 k_vec/Cg cosh(2k(D+z))/sinh^2(kD) ) dsig dtheta
    !        //
    !
    ! Where:
    !   Ac(k,theta) = wave action density
    !   k_vec = k*[cos(theta),sin(theta)]
    !   D = depth
    !   z = height (0 = mean sea level)
    !
    ! In deep water (kD large):  cosh(2k(D+z))/sinh^2(kD) --> 2*exp(2kz)
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       a         Real   I   Input spectra (in par list to change shape)
    !       usxField  Type   I/O 3D SDC eastward-component export field
    !       usyField  Type   I/O 3D SDC northward-component export field
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    implicit none
    real             :: a(nth,nk,0:nseal)
    type(ESMF_Field) :: usxField
    type(ESMF_Field) :: usyField
    integer          :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    real(8), parameter :: zero  = 0.0
    real(8), parameter :: half  = 0.5
    real(8), parameter ::  one  = 1.0
    real(8), parameter ::  two  = 2.0
    ! kdmin = 1e-7: sinh(kdmin)**2 ~ 1e-14
    real(8), parameter :: kdmin = 1e-7
    ! kdmax = 18.0:  cosh(2*(kdmax+kz))/sinh(kdmax)**2 - 2*exp(2*kz) < 1e-14
    real(8), parameter :: kdmax = 18.0
    ! kdmin & kdmax settings used in w3iogomd
    real(8), parameter :: kdmin_us3d = 1e-3
    real(8), parameter :: kdmax_us3d = 6.0
    integer :: isea, jsea, ik, ith, iz
    real(8) :: depth
    real(8) :: akx, aky, kd, kz, fac1, fac2, fac3
    real(8) :: uzx(nz), uzy(nz)
    real(8), allocatable :: fack(:)
    type(ESMF_Field) :: usxnField, usynField
    real(ESMF_KIND_RX), pointer :: usxn(:,:), usyn(:,:)
    integer, save :: timeSlice = 1
    ! Need this workaround to deal with ESMF_FieldCreate not setting up the
    ! Fortran arrays with the ungridded dimension as the first array dimension
#define ESMF_ARBSEQ_WORKAROUND
#ifdef ESMF_ARBSEQ_WORKAROUND
    type(ESMF_DistGrid) :: natDistGrid
    type(ESMF_Array) :: usxnArray, usynArray
#endif
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

#ifdef ESMF_ARBSEQ_WORKAROUND
    call ESMF_GridGet( natGrid, distGrid=natDistGrid, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    usxnArray = ESMF_ArrayCreate( natDistGrid, ESMF_TYPEKIND_RX, &
         distGridToArrayMap=(/2/), undistLBound=(/1/), undistUBound=(/nz/), rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    usxnField = ESMF_FieldCreate( natGrid, usxnArray, &
         gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/nz/), &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    usynArray = ESMF_ArrayCreate( natDistGrid, ESMF_TYPEKIND_RX, &
         distGridToArrayMap=(/2/), undistLBound=(/1/), undistUBound=(/nz/), rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    usynField = ESMF_FieldCreate( natGrid, usynArray, &
         gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/nz/), &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#else
    usxnField = ESMF_FieldCreate( natGrid, natArraySpec3D, &
         gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/nz/), &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    usynField = ESMF_FieldCreate( natGrid, natArraySpec3D, &
         gridToFieldMap=(/2,3/), ungriddedLBound=(/1/), ungriddedUBound=(/nz/), &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#endif

    call FieldFill( usxnField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( usynField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if ( natGridIsLocal ) then

      call ESMF_FieldGet( usxnField, farrayPtr=usxn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( usynField, farrayPtr=usyn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      allocate( fack(1:nk) )
      fack(1:nk) = dden(1:nk) * sig(1:nk)

      jsea_loop: do jsea = 1,nseal
#ifdef W3_DIST
        isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
        isea = jsea
#endif
        if ( dw(isea).le.zero ) cycle jsea_loop
        depth = max(dmin,dw(isea))
        uzx(:) = zero
        uzy(:) = zero
#ifdef USE_W3OUTG_FOR_EXPORT
        ik_loop: do ik = us3df(2),us3df(3)
          fac1 = tpiinv*dsii(ik)
          kd = max(kdmin_us3d,wn(ik,isea)*dw(isea))
          iz_loop: do iz = 1,nz
            if ( dw(isea)+zl(iz).le.zero ) cycle iz_loop
            kz = wn(ik,isea)*zl(iz)
            if ( kd .lt. kdmax_us3d ) then
              fac2 = fac1*cosh(two*max(zero,kd+kz))/cosh(two*kd)
            else
              fac2 = fac1*exp(two*kz)
            endif
            uzx(iz) = uzx(iz) + us3d(jsea,ik   )*fac2
            uzy(iz) = uzy(iz) + us3d(jsea,nk+ik)*fac2
          enddo iz_loop
        enddo ik_loop
#else
        ik_loop: do ik = 1,nk
          akx = zero
          aky = zero
          ith_loop: do ith = 1,nth
            akx = akx + a(ith,ik,jsea)*ecos(ith)
            aky = aky + a(ith,ik,jsea)*esin(ith)
          enddo ith_loop
          fac1 = fack(ik)*wn(ik,isea)/cg(ik,isea)
          kd = max(kdmin,wn(ik,isea)*depth)
          if ( kd .lt. kdmax ) then
            fac2 = fac1/sinh(kd)**2
          else
            fac2 = fac1*two
          endif
          akx = akx*fac2
          aky = aky*fac2
          iz_loop: do iz = 1,nz
            if ( depth+zl(iz).le.zero ) cycle iz_loop
            kz = wn(ik,isea)*zl(iz)
            if ( kd .lt. kdmax ) then
              fac3 = cosh(two*max(zero,kd+kz))
            else
              fac3 = exp(two*kz)
            endif
            uzx(iz) = uzx(iz) + akx*fac3
            uzy(iz) = uzy(iz) + aky*fac3
          enddo iz_loop
        enddo ik_loop
#endif
        usxn(:,jsea) = uzx(:)
        usyn(:,jsea) = uzy(:)
      enddo jsea_loop

      deallocate( fack )

    endif !natGridIsLocal

    call ESMF_FieldRedist( usxnField, usxField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( usynField, usyField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

#ifdef ESMF_ARBSEQ_WORKAROUND
    call ESMF_ArrayDestroy( usxnArray, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_ArrayDestroy( usynArray, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
#endif
    call ESMF_FieldDestroy( usxnField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( usynField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

#ifdef TEST_WMESMFMD_STOKES3D
    call ESMF_FieldWrite( usxField, "wmesmfmd_stokes3d_usx.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( usyField, "wmesmfmd_stokes3d_usy.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    !/
    !/ End of CalcStokes3D ----------------------------------------------- /
    !/
  end subroutine CalcStokes3D
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "CalcPStokes"
  !>
  !> @brief Calculate partitioned Stokes drift for export.
  !>
  !> @param a         Input spectra (in par list to change shape)
  !> @param p1xField
  !> @param p1yField
  !> @param p2xField
  !> @param p2yField
  !> @param p3xField
  !> @param p3yField
  !> @param rc       Return code
  !>
  !> @author J. Meixner  @date 29-Oct-2019
  !>
  subroutine CalcPStokes ( a, p1xField, p1yField, p2xField,   &
       p2yField, p3xField, p3yField, rc )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           J. Meixner              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         29-Oct-2019 |
    !/                  +-----------------------------------+
    !/
    !/    DD-MMM-YYYY : Origination.                        ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Calculate partitioned Stokes drift for export
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       a         Real   I   Input spectra (in par list to change shape)
    !       p1Field   Type   I/O
    !       p2Field   Type   I/O
    !       p3Field   Type   I/O
    !       rc        Int    O   Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3ADATMD, ONLY: USSP
    USE W3IOGOMD, ONLY: CALC_U3STOKES
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    real             :: a(nth,nk,0:nseal)
    type(ESMF_Field) :: p1xField,p2xField,p3xField
    type(ESMF_Field) :: p1yField,p2yField,p3yField
    integer          :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    !real(8) :: sxxs, sxys, syys
    type(ESMF_Field) :: p1xnField, p2xnField, p3xnField
    type(ESMF_Field) :: p1ynField, p2ynField, p3ynField
    real(ESMF_KIND_RX), pointer :: p1xn(:), p2xn(:), p3xn(:)
    real(ESMF_KIND_RX), pointer :: p1yn(:), p2yn(:), p3yn(:)
    integer, save :: timeSlice = 1
    integer :: isea,jsea
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS


    p1xnField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    p1ynField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    p2xnField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    p2ynField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    p3xnField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    p3ynField = ESMF_FieldCreate( natGrid, natArraySpec2D, &
         staggerLoc=natStaggerLoc, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call FieldFill( p1xnField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( p1ynField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( p2xnField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( p2ynField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( p3xnField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call FieldFill( p3ynField, zeroValue, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    if ( natGridIsLocal ) then

      call ESMF_FieldGet( p1xnField, farrayPtr=p1xn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( p1ynField, farrayPtr=p1yn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( p2xnField, farrayPtr=p2xn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( p2ynField, farrayPtr=p2yn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( p3xnField, farrayPtr=p3xn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      call ESMF_FieldGet( p3ynField, farrayPtr=p3yn, rc=rc )
      if (ESMF_LogFoundError(rc, PASSTHRU)) return

      call CALC_U3STOKES ( a , 2 )

      jsea_loop: do jsea = 1,nseal
#ifdef W3_DIST
        isea = iaproc + (jsea-1)*naproc
#endif
#ifdef W3_SHRD
        isea = jsea
#endif

        p1xn(jsea)=ussp(jsea,1)
        p1yn(jsea)=ussp(jsea,nk+1)
        p2xn(jsea)=ussp(jsea,2)
        p2yn(jsea)=ussp(jsea,nk+2)
        p3xn(jsea)=ussp(jsea,3)
        p3yn(jsea)=ussp(jsea,nk+3)
      enddo jsea_loop

    endif !natGridIsLocal

    call ESMF_FieldRedist( p1xnField, p1xField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( p1ynField, p1yField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( p2xnField, p2xField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( p2ynField, p2yField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( p3xnField, p3xField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldRedist( p3ynField, p3yField, n2eRH, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

    call ESMF_FieldDestroy( p1xnField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( p2xnField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( p3xnField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( p1ynField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( p2ynField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldDestroy( p3ynField, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return

#ifdef TEST_WMESMFMD_PSTOKES
    call ESMF_FieldWrite( p1xField, "wmesmfmd_pstokes_1x.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( p1yField, "wmesmfmd_pstokes_1y.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( p2xField, "wmesmfmd_pstokes_2x.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( p2yField, "wmesmfmd_pstokes_2y.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( p3xField, "wmesmfmd_pstokes_3x.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( p3yField, "wmesmfmd_pstokes_3y.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    !/
    !/ End of CalcPStokes ----------------------------------------------- /
    !/
  end subroutine CalcPStokes
  !/ ------------------------------------------------------------------- /
  !/
#undef METHOD
#define METHOD "ReadFromFile"
  !>
  !> @brief Read input file to fill unmapped point for regional applications.
  !>
  !> @param[inout] idfld  Field name
  !> @param[inout] fldwx  2D eastward-component of field
  !> @param[inout] fldwy  2D northward-component of field
  !> @param[in]    time0  Time stamp for current time
  !> @param[in]    timen  Time stamp for end time
  !> @param[inout] rc     Return code
  !>
  !> @author U. Turuncoglu
  !> @date 18-May-2021
  !>
  subroutine ReadFromFile (idfld, fldwx, fldwy, time0, timen, rc)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           U. Turuncoglu           |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         18-May-2021 |
    !/                  +-----------------------------------+
    !/
    !/    18-May-2021 : Origination.                        ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Read input file to fill unmapped point for regional applications
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       idfld     Str    I/O Field name
    !       fldwx     Type   I/O 2D eastward-component of field
    !       fldwy     Type   I/O 2D northward-component of field
    !       time0     Int    I   Time stamp for current time
    !       timen     Int    I   Time stamp for end time
    !       rc        Int    I/O Return code
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      NONE
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/
    USE W3FLDSMD, ONLY: W3FLDO, W3FLDG
    USE WMUNITMD, ONLY: WMUGET, WMUSET
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    character(len=3), intent(inout)  :: idfld
    type(ESMF_Field), intent(inout)  :: fldwx
    type(ESMF_Field), intent(inout)  :: fldwy
    integer, intent(in)              :: time0(2)
    integer, intent(in)              :: timen(2)
    integer, intent(inout), optional :: rc
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    integer :: ierr, tw0l(2), twnl(2), lb(2), ub(2)
    real :: wx0l(nx,ny), wy0l(nx,ny)
    real :: wxnl(nx,ny), wynl(nx,ny)
    real :: dt0l(nx,ny), dtnl(nx,ny)
    real(ESMF_KIND_RX), pointer :: dptr(:,:)
    integer :: mdse = 6
    integer :: mdst = 10
    integer, save :: mdsf
    character(256) :: logmsg
    logical :: flagsc = .false.
    integer, parameter :: lde = 0
    logical, save :: firstCall = .true.
    character(len=13) :: tsstr
    character(len=3) :: tsfld
    integer :: nxt, nyt, gtypet, filler(3), tideflag
#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_READFROMFILE)
    integer, save :: timeSlice = 1
#endif
    !
    ! -------------------------------------------------------------------- /
    !
    rc = ESMF_SUCCESS

    if (firstCall) then
      ! assign unit number for input file
      call wmuget(mdse, mdst, mdsf, 'INP')
      call wmuset(mdse, mdst, mdsf, .true., desc='Input data file')

      ! open file
      call w3fldo('READ', idfld, mdsf, mdst, mdse, nx, ny, gtype, ierr)
      if (ierr.ne.0) then
        write(logmsg,*) "Error in opening "//idfld//", iostat = ", ierr
        call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_ERROR)
        rc = ESMF_FAILURE
        return
      endif

      firstCall = .false.
    end if

    ! init variables
    wx0l = 0.0
    wy0l = 0.0
    dt0l = 0.0
    wxnl = 0.0
    wynl = 0.0
    dtnl = 0.0

    ! need to rewind to the begining of the file to access
    ! data of requested date correctly
    rewind(mdsf)

    ! read header information
    ! this was inside of w3fldo call but since we are opening file
    ! once and rewinding, the header need to be read
    read(mdsf, iostat=ierr) tsstr, tsfld, nxt, nyt, &
         gtypet, filler(1:2), tideflag

    ! read input
    call w3fldg('READ', idfld, mdsf, mdst, mdse, nx, ny, &
         nx, ny, time0, timen, tw0l, wx0l, wy0l, dt0l, twnl, &
         wxnl, wynl, dtnl, ierr, flagsc)

    ! fill fields with data belong to current time
    if ( impGridIsLocal ) then
      call ESMF_FieldGet(fldwx, localDE=lde, farrayPtr=dptr, &
           exclusiveLBound=lb, exclusiveUBound=ub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      dptr(lb(1):ub(1),lb(2):ub(2)) = wx0l(lb(1):ub(1),lb(2):ub(2))
      if (associated(dptr)) nullify(dptr)
      call ESMF_FieldGet(fldwy, localDE=lde, farrayPtr=dptr, &
           exclusiveLBound=lb, exclusiveUBound=ub, rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return
      dptr(lb(1):ub(1),lb(2):ub(2)) = wy0l(lb(1):ub(1),lb(2):ub(2))
      if (associated(dptr)) nullify(dptr)
    end if

#if defined(TEST_WMESMFMD) || defined(TEST_WMESMFMD_READFROMFILE)
    write(logmsg,*) 'time0 = ', time0(1), time0(2)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(logmsg,*) 'timen = ', timen(1), timen(2)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(logmsg,*) 'tw0 = ', tw0l(1), tw0l(2)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(logmsg,*) 'twn = ', twnl(1), twnl(2)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(logmsg,*) 'wx0 min, max = ', minval(wx0l), maxval(wx0l)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(logmsg,*) 'wy0 min, max = ', minval(wy0l), maxval(wy0l)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(logmsg,*) 'wxn min, max = ', minval(wxnl), maxval(wxnl)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    write(logmsg,*) 'wyn min, max = ', minval(wynl), maxval(wynl)
    call ESMF_LogWrite(trim(logmsg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( fldwx, &
         "wmesmfmd_read_wx0.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    call ESMF_FieldWrite( fldwy, &
         "wmesmfmd_read_wy0.nc", &
         overwrite=.true., timeSlice=timeSlice, rc=rc )
    if (ESMF_LogFoundError(rc, PASSTHRU)) return
    timeSlice = timeSlice + 1
#endif
    !/
    !/ End of ReadFromFile ------------------------------------------- /
    !/
  end subroutine ReadFromFile
  !/ ------------------------------------------------------------------- /
  !/
  !/ End of module WMESMFMD -------------------------------------------- /
  !/
end module WMESMFMD
