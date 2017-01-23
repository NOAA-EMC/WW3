#include "macros.h"
!-------------------------------------------------------------------------------
! A test coupled application utility module
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!-------------------------------------------------------------------------------

module UTL

  use ESMF
  use NUOPC

  implicit none
  save
  private

  public InitFieldDictionary
  public PrintTimers

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine InitFieldDictionary(rc)
    integer, intent(out) :: rc

    ! local variables
    integer, parameter :: maxFields = 50
    character(ESMF_MAXSTR) :: standardName(maxFields)
    character(ESMF_MAXSTR) :: canonicalUnits(maxFields)
    integer :: i, numFields
    logical :: isPresent

    rc = ESMF_SUCCESS

    i = 0
    ! ATM export fields
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
    ! OCN export fields
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
    ! WAV export fields
    i = i+1; standardName(i) = 'wave_z0_roughness_length'
             canonicalUnits(i)='m'
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
    i = i+1; standardName(i) = 'eastward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'eastward_northward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'northward_wave_radiation_stress'
             canonicalUnits(i)='Pa m'
    i = i+1; standardName(i) = 'eastward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'northward_wave_radiation_stress_gradient'
             canonicalUnits(i)='Pa'
    i = i+1; standardName(i) = 'wave_orbital_turbulence_production'
             canonicalUnits(i)='m2 s-3'
    i = i+1; standardName(i) = 'sea_floor_depth_below_sea_surface'
             canonicalUnits(i)='m'
    i = i+1; standardName(i) = 'air_sea_temperature_difference'
             canonicalUnits(i)='K'
    i = i+1; standardName(i) = 'bottom_friction_coefficient'
             canonicalUnits(i)='1'
    ! ICE export fields
    i = i+1; standardName(i) = 'sea_ice_concentration'
             canonicalUnits(i)='m'
    numFields = i

    do i=1,numFields
      ! normal fields
      isPresent = NUOPC_FieldDictionaryHasEntry(trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry(trim(standardName(i)), &
          trim(canonicalUnits(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      ! background fields (add mbg_ prefix)
      isPresent = NUOPC_FieldDictionaryHasEntry('mbg_'//trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry('mbg_'//trim(standardName(i)), &
          trim(canonicalUnits(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
      ! perturbation fields (add pert_ prefix)
      isPresent = NUOPC_FieldDictionaryHasEntry('pert_'//trim(standardName(i)), rc=rc)
      if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      if (.not.isPresent) then
        call NUOPC_FieldDictionaryAddEntry('pert_'//trim(standardName(i)), &
          trim(canonicalUnits(i)), rc=rc)
        if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
      endif
    enddo

  end subroutine

  !-----------------------------------------------------------------------------

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
