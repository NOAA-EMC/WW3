#include "macros.h"
!-------------------------------------------------------------------------------
! A test coupled application driver
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!-------------------------------------------------------------------------------

program esmApp

  use ESMF
  use NUOPC
  use FRONT_ESM, only: drmSS => SetServices
  use UTL

  implicit none

  integer                 :: rc, urc
  type(ESMF_GridComp)     :: gcomp
  integer                 :: argCount
  type(ESMF_Config)       :: config
  character(ESMF_MAXSTR)  :: configFile

  ! Initialize ESMF
  call ESMF_Initialize(logkindflag=ESMF_LOGKIND_MULTI, &
    defaultCalkind=ESMF_CALKIND_GREGORIAN, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Set log defaults
  call ESMF_LogSet(flush=.true.)

  ! Add required fields to NUOPC field dictionary
  call InitFieldDictionary(rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create the driver Component
  gcomp = ESMF_GridCompCreate(name='WW3ESM', rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Create & set config for the driver Component
  configFile = 'ww3_esmf.rc'
  call ESMF_UtilGetArgC(argCount, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (argCount.eq.1) then
    call ESMF_UtilGetArg(1, argValue=configFile, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  endif
  config = ESMF_ConfigCreate(rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_ConfigLoadFile(config, trim(configFile), rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  call ESMF_GridCompSet(gcomp, config=config, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! SetServices for the driver Component
  call ESMF_GridCompSetServices(gcomp, drmSS, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Initialize for the driver Component
  call ESMF_GridCompInitialize(gcomp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Run for the driver Component
  call ESMF_GridCompRun(gcomp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Call Finalize for the driver Component
  call ESMF_GridCompFinalize(gcomp, userRc=urc, rc=rc)
  if (ESMF_LogFoundError( rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)
  if (ESMF_LogFoundError(urc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Destroy the driver Component
  call ESMF_GridCompDestroy(gcomp, rc=rc)
  if (ESMF_LogFoundError(rc, PASSTHRU)) call ESMF_Finalize(endflag=ESMF_END_ABORT)

  ! Finalize ESMF
  call ESMF_Finalize()

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine DummyRoutine(rc)
    integer, intent(out) :: rc

    ! local variables
    ! none

    rc = ESMF_SUCCESS

  end subroutine

end program
