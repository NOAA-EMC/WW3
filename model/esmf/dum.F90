#include "macros.h"
!-------------------------------------------------------------------------------
! A test coupled application dummy model component
!
! Author:
!   Tim Campbell
!   Naval Research Laboratory
!   November 2014
!-------------------------------------------------------------------------------

module DUM
  use ESMF
  implicit none
  private
  public SetServices
  contains
  subroutine SetServices(comp, rc)
    type(ESMF_GridComp)    :: comp
    integer, intent(out)   :: rc
    character(ESMF_MAXSTR) :: cname, msg
    call ESMF_GridCompGet(comp, name=cname, rc=rc)
    if (ESMF_LogFoundError(rc, PASSTHRU)) return ! bail out
    msg=trim(cname)//': dummy component should never be invoked'
    call ESMF_LogSetError(ESMF_FAILURE, msg=trim(msg), rcToReturn=rc)
  end subroutine
end module
