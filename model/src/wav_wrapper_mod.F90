!> @file wav_wrapper_mod
!!
!> A wrapper module for log functionality in UFS
!!
!> @details Contains public logging routines for UFS and
!! stub routines for CESM
!!
!> Denise.Worthen@noaa.gov
!> @date 01-08-2024
module wav_wrapper_mod

  use wav_kind_mod  , only : r8 => shr_kind_r8, r4 => shr_kind_r4, i4 => shr_kind_i4
  use wav_kind_mod  , only : CL => shr_kind_cl, CS => shr_kind_cs

  implicit none

  real(r8) :: wtime = 0.0

#ifdef CESMCOUPLED
contains
  ! Define stub routines that do nothing - they are just here to avoid
  ! having cppdefs in the main program
  subroutine ufs_settimer(timevalue)
    real(r8),    intent(inout) :: timevalue
  end subroutine ufs_settimer
  subroutine ufs_logtimer(nunit,times,tod,string,runtimelog,wtime0)
    integer,          intent(in) :: nunit
    integer(i4),      intent(in) :: times(2), tod
    character(len=*), intent(in) :: string
    logical,          intent(in) :: runtimelog
    real(r8),         intent(in) :: wtime0
  end subroutine ufs_logtimer
  subroutine ufs_file_setLogUnit(filename,nunit,runtimelog)
    character(len=*),  intent(in)  :: filename
    logical,           intent(in)  :: runtimelog
    integer,           intent(out) :: nunit
  end subroutine ufs_file_setLogUnit
  subroutine ufs_logfhour(msg,hour)
    character(len=*),  intent(in)  :: msg
    real(r8),          intent(in)  :: hour
  end subroutine ufs_logfhour
#else
contains
  subroutine ufs_settimer(timevalue)
    !> Set a time value
    !! @param[inout]    timevalue    a MPI time value
    !!
    !> Denise.Worthen@noaa.gov
    !> @date 01-08-2024

    real(r8),    intent(inout) :: timevalue
    real(r8)                   :: MPI_Wtime
    timevalue = MPI_Wtime()
  end subroutine ufs_settimer

  subroutine ufs_logtimer(nunit,times,tod,string,runtimelog,wtime0)
    !> Log a time interval
    !! @param[in]    nunit              the log file unit
    !! @param[in]    times              the ymd,hms time values
    !! @param[in]    tod                the elapsed seconds in the day
    !! @param[in]    string             a message string to log
    !! @param[in]    runtimelog         a logical to control the log function
    !! @param[in]    wtime0             an initial MPI time
    !!
    !> Denise.Worthen@noaa.gov
    !> @date 01-08-2024
    integer,          intent(in)    :: nunit
    integer(i4),      intent(in)    :: times(2),tod
    character(len=*), intent(in)    :: string
    logical,          intent(in)    :: runtimelog
    real(r8),         intent(in)    :: wtime0
    real(r8)                        :: MPI_Wtime, timevalue
    if (.not. runtimelog) return
    if (wtime0 > 0.) then
      timevalue = MPI_Wtime()-wtime0
      write(nunit,'(3i8,a,g14.7)')times,tod,' WW3 '//trim(string),timevalue
    end if
  end subroutine ufs_logtimer

  subroutine ufs_file_setLogUnit(filename,nunit,runtimelog)
    !> Create a log unit
    !! @param[in]    filename           the log filename
    !! @param[in]    runtimelog         a logical to control the log function
    !! @param[out]   nunit              the log file unit
    !!
    !> Denise.Worthen@noaa.gov
    !> @date 01-08-2024

    character(len=*),  intent(in)    :: filename
    logical,           intent(in)    :: runtimelog
    integer,           intent(out)   :: nunit
    if (.not. runtimelog) return
    open (newunit=nunit, file=trim(filename))
  end subroutine ufs_file_setLogUnit

  subroutine ufs_logfhour(msg,hour)
    !> Log the completion of model output
    !! @param[in]    msg                the log message
    !! @param[in]    hour               the forecast hour
    !!
    !> Denise.Worthen@noaa.gov
    !> @date 01-08-2024

    character(len=*), intent(in) :: msg
    real(r8),         intent(in) :: hour

    character(len=CS)            :: filename
    integer(r4)                  :: nunit

    write(filename,'(a,i3.3)')'log.ww3.f',int(hour)
    open(newunit=nunit,file=trim(filename))
    write(nunit,'(a)')'completed: ww3'
    write(nunit,'(a,f10.3)')'forecast hour:',hour
    write(nunit,'(a)')'valid time: '//trim(msg)
    close(nunit)
  end subroutine ufs_logfhour
#endif

end module wav_wrapper_mod
