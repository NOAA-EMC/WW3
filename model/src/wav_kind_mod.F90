!> @file wav_kind_mod
!!
!> Precision and kind constants
!!
!> @details Contains public definitions of variable types and constants
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
module wav_kind_mod

  !----------------------------------------------------------------------------
  ! precision/kind constants add data public
  !----------------------------------------------------------------------------
  public
  integer,parameter :: SHR_KIND_R8 = selected_real_kind(12) !< @public 8 byte real
  integer,parameter :: SHR_KIND_R4 = selected_real_kind( 6) !< @public 4 byte real
  integer,parameter :: SHR_KIND_RN = kind(1.0)              !< @public native real
  integer,parameter :: SHR_KIND_I8 = selected_int_kind (13) !< @public 8 byte integer
  integer,parameter :: SHR_KIND_I4 = selected_int_kind ( 6) !< @public 4 byte integer
  integer,parameter :: SHR_KIND_IN = kind(1)                !< @public native integer
  integer,parameter :: SHR_KIND_CS = 80                     !< @public short char
  integer,parameter :: SHR_KIND_CM = 160                    !< @public mid-sized char
  integer,parameter :: SHR_KIND_CL = 256                    !< @public long char
  integer,parameter :: SHR_KIND_CX = 512                    !< @public extra-long char
  integer,parameter :: SHR_KIND_CXX= 4096                   !< @public extra-extra-long char

end module wav_kind_mod
