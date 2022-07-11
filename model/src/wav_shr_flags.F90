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

  logical, public :: w3_debuginit_flag        !< @public a flag for "W3_DEBUGINIT"
  logical, public :: w3_debugrun_flag         !< @public a flag for "W3_DEBUGRUN"
  logical, public :: w3_debugio_flag          !< @public a flag for "W3_DEBUGIO"
  logical, public :: w3_timings_flag          !< @public a flag for "W3_TIMINGS"

  logical, public :: w3_cou_flag              !< @public a flag for "W3_COU"
  logical, public :: w3_oasis_flag            !< @public a flag for "W3_OASIS"
  logical, public :: w3_o7_flag               !< @public a flag for "W3_O7"
  logical, public :: w3_t_flag                !< @public a flag for "W3_T"
  logical, public :: w3_s_flag                !< @public a flag for "W3_S"
  logical, public :: w3_mgw_flag              !< @public a flag for "W3_MGW"
  logical, public :: w3_mgp_flag              !< @public a flag for "W3_MGP"
  logical, public :: w3_nl5_flag              !< @public a flag for "W3_NL5"
  logical, public :: w3_ic1_flag              !< @public a flag for "W3_IC1"
  logical, public :: w3_ic2_flag              !< @public a flag for "W3_IC2"
  logical, public :: w3_is2_flag              !< @public a flag for "W3_IS2"
  logical, public :: w3_ic3_flag              !< @public a flag for "W3_IC3"
  logical, public :: w3_bt8_flag              !< @public a flag for "W3_BT8"
  logical, public :: w3_bt9_flag              !< @public a flag for "W3_BT9"
  logical, public :: w3_ic4_flag              !< @public a flag for "W3_IC4"
  logical, public :: w3_ic5_flag              !< @public a flag for "W3_IC5"
  logical, public :: w3_nco_flag              !< @public a flag for "W3_NCO"
  logical, public :: w3_pdlib_flag            !< @public a flag for "W3_PDLIB"
  logical, public :: w3_cesmcoupled_flag      !< @public a flag for "W3_CESMCOUPLED"
  logical, public :: w3_memcheck_flag         !< @public a flag for "W3_MEMCHECK"

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
    w3_debuginit_flag = .false.
    w3_debugrun_flag = .false.
    w3_debugio_flag = .false.
    w3_timings_flag = .false.

    w3_cou_flag = .false.
    w3_oasis_flag = .false.
    w3_o7_flag = .false.
    w3_t_flag = .false.
    w3_mgw_flag = .false.
    w3_mgp_flag = .false.
    w3_nl5_flag = .false.
    w3_ic1_flag = .false.
    w3_ic2_flag = .false.
    w3_is2_flag = .false.
    w3_ic3_flag = .false.
    w3_bt8_flag = .false.
    w3_bt9_flag = .false.
    w3_ic4_flag = .false.
    w3_ic5_flag = .false.
    w3_nco_flag = .false.
    w3_pdlib_flag = .false.
    w3_cesmcoupled_flag = .false.
    w3_memcheck_flag = .false.

#ifdef W3_DEBUGINIT
    w3_debuginit_flag = .true.
#endif
#ifdef W3_DEBUGRUN
    w3_debugrun_flag = .true.
#endif
#ifdef W3_DEBUGIO
    w3_debugio_flag = .true.
#endif
#ifdef W3_TIMINGS
    w3_timings_flag = .true.
#endif
#ifdef W3_COU
    w3_cou_flag = .true.
#endif
#ifdef W3_OASIS
    w3_oasis_flag = .true.
#endif
#ifdef W3_O7
    w3_o7_flag = .true.
#endif
#ifdef W3_T
    w3_t_flag = .true.
#endif
#ifdef W3_MGW
    w3_mgw_flag = .true.
#endif
#ifdef W3_MGP
    w3_mgp_flag = .true.
#endif
#ifdef W3_NL5
    w3_nl5_flag = .true.
#endif
#ifdef W3_IC1
    w3_ic1_flag = .true.
#endif
#ifdef W3_IC2
    w3_ic2_flag = .true.
#endif
#ifdef W3_IS2
    w3_is2_flag = .true.
#endif
#ifdef W3_IC3
    w3_ic3_flag = .true.
#endif
#ifdef W3_BT8
    w3_bt8_flag = .true.
#endif
#ifdef W3_BT9
    w3_bt9_flag = .true.
#endif
#ifdef W3_IC4
    w3_ic4_flag = .true.
#endif
#ifdef W3_IC5
    w3_ic5_flag = .true.
#endif
#ifdef W3_NCO
    w3_nco_flag = .true.
#endif
#ifdef W3_PDLIB
    w3_pdlib_flag = .true.
#endif
#ifdef W3_S
    w3_strace_flag = .true.
#endif
#ifdef W3_T
    w3_t_flag = .true.
#endif
#ifdef W3_CESMCOUPLED
    w3_cesmcoupled_flag = .true.
#endif
#ifdef W3_CESMCOUPLED
    w3_memcheck_flag = .true.
#endif
  end subroutine initialize_flags

  !========================================================================
!> Write a 1 line message if requested
!!
!> @details Writes a one line message
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
!> @details Writes a two line message
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
!> @details Writes a three line message
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
!> @details Writes a four line message
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
