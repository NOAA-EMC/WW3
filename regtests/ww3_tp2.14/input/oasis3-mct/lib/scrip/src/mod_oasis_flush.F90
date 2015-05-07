MODULE mod_oasis_flush

  USE kinds_mod     ! defines common data types
  !
  IMPLICIT NONE

   private

   public oasis_flush_scrip

!--------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------

!==========================================================================
   SUBROUTINE oasis_flush_scrip(nu)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=int_kind),INTENT(in) :: nu
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_flush_scrip'
!--------------------------------------------------------------------

   CALL FLUSH(nu)

 END SUBROUTINE oasis_flush_scrip

!==========================================================================

END MODULE mod_oasis_flush
