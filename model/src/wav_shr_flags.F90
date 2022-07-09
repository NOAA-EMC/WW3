!> @file wav_shr_flags
!!
!> Shared flags matching compile time options
!!
!> @details Sets logical flags to according to compile time
!! options
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 07-01-2022
module wav_shr_flags

  implicit none

  logical, public :: debuginit_flag        !< @public a flag for "W3_DEBUGINIT"
  logical, public :: debugrun_flag         !< @public a flag for "W3_DEBUGRUN"
  logical, public :: debugio_flag          !< @public a flag for "W3_DEBUGIO"
  logical, public :: timings_flag          !< @public a flag for "W3_TIMINGS"

  logical, public :: couple_flag           !< @public a flag for "W3_COU"
  logical, public :: oasis_flag            !< @public a flag for "W3_OASIS"
  logical, public :: O7_flag               !< @public a flag for "W3_O7"
  logical, public :: t_flag                !< @public a flag for "W3_T"
  logical, public :: mgw_flag              !< @public a flag for "W3_MGW"
  logical, public :: mgp_flag              !< @public a flag for "W3_MGP"
  logical, public :: nl5_flag              !< @public a flag for "W3_NL5"
  logical, public :: ic1_flag              !< @public a flag for "W3_IC1"
  logical, public :: ic2_flag              !< @public a flag for "W3_IC2"
  logical, public :: is2_flag              !< @public a flag for "W3_IS2"
  logical, public :: ic3_flag              !< @public a flag for "W3_IC3"
  logical, public :: bt8_flag              !< @public a flag for "W3_BT8"
  logical, public :: bt9_flag              !< @public a flag for "W3_BT9"
  logical, public :: ic4_flag              !< @public a flag for "W3_IC4"
  logical, public :: ic5_flag              !< @public a flag for "W3_IC5"
  logical, public :: nco_flag              !< @public a flag for "W3_NCO"

  public :: initialize_flags
  public :: debuginit_msg

  contains

  subroutine initialize_flags

  ! initialize all flags false by default
  debuginit_flag = .false.
  debugrun_flag = .false.
  debugio_flag = .false.
  timings_flag = .false.

  couple_flag = .false.
  oasis_flag = .false.
  O7_flag = .false.
  t_flag = .false.
  mgw_flag = .false.
  mgp_flag = .false.
  nl5_flag = .false.
  ic1_flag = .false.
  ic2_flag = .false.
  is2_flag = .false.
  ic3_flag = .false.
  bt8_flag = .false.
  bt9_flag = .false.
  ic4_flag = .false.
  ic5_flag = .false.
  nco_flag = .false.

#ifdef W3_DEBUGINIT
    debuginit_flag = .true.
#endif
#ifdef W3_DEBUGRUN
    debugrun_flag = .true.
#endif
#ifdef W3_DEBUGIO
    debugio_flag = .true.
#endif
#ifdef W3_TIMINGS
    timings_flag = .true.
#endif
#ifdef W3_COU
    couple_flag = .true.
#endif
#ifdef W3_OASIS
    oasis_flag = .true.
#endif
#ifdef W3_O7
    O7_flag = .true.
#endif
#ifdef W3_T
    t_flag = .true.
#endif
#ifdef W3_MGW
    mgw_flag = .true.
#endif
#ifdef W3_MGP
    mgp_flag = .true.
#endif
#ifdef W3_NL5
    nl5_flag = .true.
#endif
#ifdef W3_IC1
      ic1_flag = .true.
#endif
#ifdef W3_IC2
      ic2_flag = .true.
#endif
#ifdef W3_IS2
      is2_flag = .true.
#endif
#ifdef W3_IC3
      ic3_flag = .true.
#endif
#ifdef W3_BT8
      bt8_flag = .true.
#endif
#ifdef W3_BT9
      bt9_flag = .true.
#endif
#ifdef W3_IC4
      ic4_flag = .true.
#endif
#ifdef W3_IC5
      ic5_flag = .true.
#endif
#ifdef W3_NCO
      nco_flag = .true.
#endif
  end subroutine initialize_flags

  !========================================================================
!> Write a message for debuginit if requested
!!
!! @details Writes a debug message
!!
!! @param[in]   unum               unit number
!! @param[in]   msg                debug message
!! @param[in]   lwrite             logical to control message writing
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 06-01-2022
   subroutine debuginit_msg(unum, msg, lwrite)

   integer         , intent(in)           :: unum
   character(len=*), intent(in)           :: msg
   logical         , intent(in)           :: lwrite

   if (.not. lwrite) return

   write(unum,'(a)') trim(msg)
   flush(unum)

   end subroutine debuginit_msg
end module wav_shr_flags
