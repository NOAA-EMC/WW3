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
  logical, public :: pdlib_flag            !< @public a flag for "W3_PDLIB"

  public :: initialize_flags

  interface print_logmsg
    module procedure print_logmsg_1line
    module procedure print_logmsg_2line
    module procedure print_logmsg_3line
    module procedure print_logmsg_4line
  end interface

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
  pdlib_flag = .false.

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
#ifdef W3_PDLIB
      pdlib_flag = .true.
#endif
  end subroutine initialize_flags

  !========================================================================
!> Write a 1 line message if requested
!!
!! @details Writes a one line message
!!
!! @param[in]   unum               unit number
!! @param[in]   msg1               one line message
!! @param[in]   lwrite             logical to control message writing
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 06-01-2022
   subroutine print_logmsg_1line(unum, msg1, lwrite)

   integer         , intent(in)           :: unum
   character(len=*), intent(in)           :: msg1
   logical         , intent(in)           :: lwrite

   if (.not. lwrite) return

   write(unum,'(a)') trim(msg1)
   flush(unum)

   end subroutine print_logmsg_1line

  !========================================================================
!> Write a 2 line message if requested
!!
!! @details Writes a two line message
!!
!! @param[in]   unum               unit number
!! @param[in]   msg1               first line of message
!! @param[in]   msg2               second line of message
!! @param[in]   lwrite             logical to control message writing
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 06-01-2022
   subroutine print_logmsg_2line(unum, msg1, msg2, lwrite)

   integer         , intent(in)           :: unum
   character(len=*), intent(in)           :: msg1
   character(len=*), intent(in)           :: msg2
   logical         , intent(in)           :: lwrite

   if (.not. lwrite) return

   write(unum,'(a)') trim(msg1)
   write(unum,'(a)') trim(msg2)
   flush(unum)

   end subroutine print_logmsg_2line

  !========================================================================
!> Write a 3 line message if requested
!!
!! @details Writes a three line message
!!
!! @param[in]   unum               unit number
!! @param[in]   msg1               first line of message
!! @param[in]   msg2               second line of message
!! @param[in]   msg3               third line of message
!! @param[in]   lwrite             logical to control message writing
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 06-01-2022
   subroutine print_logmsg_3line(unum, msg1, msg2, msg3, lwrite)

   integer         , intent(in)           :: unum
   character(len=*), intent(in)           :: msg1
   character(len=*), intent(in)           :: msg2
   character(len=*), intent(in)           :: msg3
   logical         , intent(in)           :: lwrite

   if (.not. lwrite) return

   write(unum,'(a)') trim(msg1)
   write(unum,'(a)') trim(msg2)
   write(unum,'(a)') trim(msg3)
   flush(unum)

   end subroutine print_logmsg_3line

  !========================================================================
!> Write a 4 line message if requested
!!
!! @details Writes a four line message
!!
!! @param[in]   unum               unit number
!! @param[in]   msg1               first line of message
!! @param[in]   msg2               second line of message
!! @param[in]   msg3               third line of message
!! @param[in]   msg4               forth line of message
!! @param[in]   lwrite             logical to control message writing
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 06-01-2022

   subroutine print_logmsg_4line(unum, msg1, msg2, msg3, msg4, lwrite)

   integer         , intent(in)           :: unum
   character(len=*), intent(in)           :: msg1
   character(len=*), intent(in)           :: msg2
   character(len=*), intent(in)           :: msg3
   character(len=*), intent(in)           :: msg4
   logical         , intent(in)           :: lwrite

   if (.not. lwrite) return

   write(unum,'(a)') trim(msg1)
   write(unum,'(a)') trim(msg2)
   write(unum,'(a)') trim(msg3)
   write(unum,'(a)') trim(msg4)
   flush(unum)

   end subroutine print_logmsg_4line
end module wav_shr_flags
