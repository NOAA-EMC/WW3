module wav_wrapper_mod

#ifdef CESMCOUPLED
  use perf_mod     , only : t_startf, t_stopf, t_barrierf
  use shr_file_mod , only : shr_file_getlogunit, shr_file_setlogunit

#else
contains

  ! These are just stub routines put in place to remove

  subroutine shr_file_setLogUnit(nunit)
    integer, intent(in) :: nunit
    ! do nothing for this stub - its just here to replace
    ! having cppdefs in the main program
  end subroutine shr_file_setLogUnit
  subroutine shr_file_getLogUnit(nunit)
    integer, intent(in) :: nunit
    ! do nothing for this stub - its just here to replace
    ! having cppdefs in the main program
  end subroutine shr_file_getLogUnit

  subroutine t_startf(string)
    character(len=*) :: string
  end subroutine t_startf
  subroutine t_stopf(string)
    character(len=*) :: string
  end subroutine t_stopf
  subroutine t_barrierf(string, comm)
    character(len=*) :: string
    integer:: comm
  end subroutine t_barrierf
#endif

end module wav_wrapper_mod
