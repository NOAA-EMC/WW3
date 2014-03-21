!-------------------------------------------------------------------------------
! A test Wavewatch III coupled application driver
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   March 2014
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! ESMF macros for logging
!-------------------------------------------------------------------------------
#define FILENAME "esmApp.F90"
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


program esmApp

  use ESMF
  use NUOPC
  use ESM, only: drvSS => SetServices

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
  call SetFieldDictionary(rc=rc)
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
  call ESMF_GridCompSetServices(gcomp, drvSS, userRc=urc, rc=rc)
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

  subroutine SetFieldDictionary(rc)
    integer, intent(out) :: rc

    ! local variables
    integer, parameter :: maxFields = 50
    character(ESMF_MAXSTR) :: standardName(maxFields)
    character(ESMF_MAXSTR) :: canonicalUnits(maxFields)
    integer :: i, numFields
    logical :: isPresent

    rc = ESMF_SUCCESS

    i = 0
    ! ATM export
    i = i+1; standardName(i) = 'air_pressure_at_sea_level'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'eastward_wind_at_10m_height'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_wind_at_10m_height'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'magnitude_of_surface_downward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_downward_eastward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_downward_northward_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'air_temperature_at_2m_height'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'relative_humidity_at_2m_height'
             canonicalUnits(i)='1'
    i = i+1; standardName(i) = 'surface_downward_latent_heat_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_downward_sensible_heat_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_net_downward_shortwave_flux'
             canonicalUnits(i)='W m-2'
    i = i+1; standardName(i) = 'surface_net_downward_longwave_flux'
             canonicalUnits(i)='W m-2'
    ! OCN export
    i = i+1; standardName(i) = 'sea_surface_height_above_sea_level'
             canonicalUnits(i)='m'
    i = i+1; standardName(i) = 'sea_surface_temperature'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'sea_surface_salinity'
             canonicalUnits(i)='1e-3'
    i = i+1; standardName(i) = 'surface_eastward_sea_water_velocity'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'surface_northward_sea_water_velocity'
             canonicalUnits(i)='m s-1'
    ! WAV export
    i = i+1; standardName(i) = 'wave_induced_charnock_parameter'
             canonicalUnits(i)='1'
    i = i+1; standardName(i) = 'surface_total_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_eastward_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'surface_northward_wave_induced_stress'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'eastward_stokes_drift_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_stokes_drift_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'eastward_wave_bottom_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'northward_wave_bottom_current'
             canonicalUnits(i)='m s-1'
    i = i+1; standardName(i) = 'wave_bottom_current_radian_frequency'
             canonicalUnits(i)='rad s-1'
    i = i+1; standardName(i) = 'eastward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'northward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    numFields = i

    do i=1,numFields
      isPresent = NUOPC_FieldDictionaryHasEntry(trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry(trim(standardName(i)), &
          trim(canonicalUnits(i)), defaultLongName='none', defaultShortName='none', rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      isPresent = NUOPC_FieldDictionaryHasEntry('mbg_'//trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry('mbg_'//trim(standardName(i)), &
          trim(canonicalUnits(i)), defaultLongName='none', defaultShortName='none', rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    enddo

  end subroutine
  
end program  
