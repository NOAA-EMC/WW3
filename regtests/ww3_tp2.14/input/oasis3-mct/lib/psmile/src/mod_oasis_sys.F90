MODULE mod_oasis_sys

   USE mod_oasis_kinds
   USE mod_oasis_data

   IMPLICIT NONE

   private

   public oasis_abort_noarg
   public oasis_abort
   public oasis_flush
   public oasis_unitsetmin
   public oasis_unitget
   public oasis_unitfree
   public oasis_debug_enter
   public oasis_debug_exit
   public oasis_debug_note

   integer(ip_intwp_p),parameter :: muni = 20
   integer(ip_intwp_p),save :: unitno(muni) = -1
   integer(ip_intwp_p),save :: maxion
   integer(ip_intwp_p),parameter :: tree_delta = 2
   integer(ip_intwp_p),save :: tree_indent = 0

!--------------------------------------------------------------------
CONTAINS
!--------------------------------------------------------------------

   SUBROUTINE oasis_abort_noarg()

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER                      :: ierror
   character(len=*),parameter   :: subname = 'oasis_abort_noarg'
!--------------------------------------------------------------------

#if defined use_comm_MPI1 || defined use_comm_MPI2
   CALL MPI_ABORT (mpi_comm_global, 0, ierror)
#endif

   STOP

 END SUBROUTINE oasis_abort_noarg

!--------------------------------------------------------------------

   SUBROUTINE oasis_abort(id_compid, cd_routine, cd_message)

   IMPLICIT NONE
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: id_compid
   CHARACTER(len=*), INTENT(in) :: cd_routine
   CHARACTER(len=*), INTENT(in) :: cd_message
!--------------------------------------------------------------------
   INTEGER                      :: ierror
   character(len=*),parameter   :: subname = 'oasis_abort'
!--------------------------------------------------------------------

   WRITE (nulprt,'(a)') subname//' from '//TRIM(cd_routine)
   WRITE (nulprt,'(a)') subname//' error = '//TRIM(cd_message)

#if defined use_comm_MPI1 || defined use_comm_MPI2
   CALL MPI_ABORT (mpi_comm_global, 0, ierror)
#endif

   STOP

 END SUBROUTINE oasis_abort

!==========================================================================
   SUBROUTINE oasis_flush(nu)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: nu
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_flush'
!--------------------------------------------------------------------

   CALL FLUSH(nu)

 END SUBROUTINE oasis_flush

!==========================================================================
   SUBROUTINE oasis_unitget(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(out) :: uio
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p) :: n1
   logical :: found
   character(len=*),parameter :: subname = 'oasis_unitget'
!--------------------------------------------------------------------

   n1 = 0
   found = .false.
   do while (n1 < muni .and. .not.found)
      n1 = n1 + 1
      if (unitno(n1) < 0) then
         found = .true.
         uio = n1 + maxion
         unitno(n1) = uio
         if (OASIS_debug >= 2) write(nulprt,*) subname,n1,uio
      endif
   enddo

   if (.not.found) then
      write(nulprt,*) subname,' ERROR no unitno available '
      WRITE(nulprt,*) subname,' abort by model ',compid,' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif
     
 END SUBROUTINE oasis_unitget

!==========================================================================
   SUBROUTINE oasis_unitsetmin(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio
!--------------------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_unitsetmin'
!--------------------------------------------------------------------

   maxion = uio
   if (OASIS_debug >= 20) write(nulprt,*) subname,maxion
     
 END SUBROUTINE oasis_unitsetmin

!==========================================================================
   SUBROUTINE oasis_unitfree(uio)

   IMPLICIT NONE

!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p),INTENT(in) :: uio
!--------------------------------------------------------------------
   INTEGER(kind=ip_intwp_p) :: n1
   character(len=*),parameter :: subname = 'oasis_unitfree'
!--------------------------------------------------------------------

   do n1 = 1,muni
      if (unitno(n1) == uio) then
         unitno(n1) = -1
         if (OASIS_debug >= 20) write(nulprt,*) subname,n1,uio
      endif
   enddo

 END SUBROUTINE oasis_unitfree

!=========================================================================
!==========================================================================
SUBROUTINE oasis_debug_enter(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'oasis_debug_enter'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500) :: tree_enter

   if (OASIS_debug >= 10) then
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_enter='**** ENTER '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_enter)
       tree_indent = tree_indent + tree_delta
       DEALLOCATE (ch_blank)
       CALL oasis_flush(nulprt)
   endif

 END SUBROUTINE oasis_debug_enter

!==========================================================================
SUBROUTINE oasis_debug_exit(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'oasis_debug_exit'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500)        :: tree_exit

   IF (OASIS_debug >= 10) THEN
       tree_indent = MAX(0,tree_indent - tree_delta)
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_exit='**** EXIT  '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_exit)
       DEALLOCATE (ch_blank)
       CALL oasis_flush(nulprt)
   ENDIF

 END SUBROUTINE oasis_debug_exit

!==========================================================================
SUBROUTINE oasis_debug_note(string)

   IMPLICIT NONE

!--------------------------------------------------------------------
   CHARACTER(len=*), INTENT(in) :: string
   character(len=*),parameter :: subname = 'oasis_debug_note'
   CHARACTER(len=1), pointer :: ch_blank(:)
   CHARACTER(len=500) :: tree_note

   if (OASIS_debug >= 12) then
       ALLOCATE (ch_blank(tree_indent))
       ch_blank='-'
       tree_note='**** NOTE '//TRIM(string)
       WRITE(nulprt,*) ch_blank,TRIM(tree_note)
      DEALLOCATE(ch_blank)
      call oasis_flush(nulprt)
   endif

 END SUBROUTINE oasis_debug_note

!==========================================================================

END MODULE mod_oasis_sys
