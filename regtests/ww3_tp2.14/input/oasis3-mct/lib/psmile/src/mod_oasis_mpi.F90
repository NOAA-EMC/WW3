MODULE mod_oasis_mpi

!-------------------------------------------------------------------------------
! PURPOSE: general layer on MPI functions
!-------------------------------------------------------------------------------

   use mod_oasis_kinds
   USE mod_oasis_data, ONLY: compid, mpi_rank_local
   USE mod_oasis_sys, ONLY: oasis_debug_enter, oasis_debug_exit, oasis_flush

   implicit none
   private

! PUBLIC: Public interfaces

   public :: oasis_mpi_chkerr
   public :: oasis_mpi_send
   public :: oasis_mpi_recv
   public :: oasis_mpi_bcast
   public :: oasis_mpi_gathScatVInit
   public :: oasis_mpi_gatherV
   public :: oasis_mpi_scatterV
   public :: oasis_mpi_sum
   public :: oasis_mpi_min
   public :: oasis_mpi_max
   public :: oasis_mpi_commsize
   public :: oasis_mpi_commrank
   public :: oasis_mpi_initialized
   public :: oasis_mpi_wtime
   public :: oasis_mpi_abort
   public :: oasis_mpi_barrier
   public :: oasis_mpi_init
   public :: oasis_mpi_finalize

   interface oasis_mpi_send ; module procedure &
     oasis_mpi_sendi0, &
     oasis_mpi_sendi1, &
     oasis_mpi_sendr0, &
     oasis_mpi_sendr1, &
     oasis_mpi_sendr3
   end interface
   interface oasis_mpi_recv ; module procedure &
     oasis_mpi_recvi0, &
     oasis_mpi_recvi1, &
     oasis_mpi_recvr0, &
     oasis_mpi_recvr1, &
     oasis_mpi_recvr3
   end interface
   interface oasis_mpi_bcast ; module procedure &
     oasis_mpi_bcastc0, &
     oasis_mpi_bcastc1, &
     oasis_mpi_bcastl0, &
     oasis_mpi_bcastl1, &
     oasis_mpi_bcasti0, &
     oasis_mpi_bcasti1, &
     oasis_mpi_bcasti2, &
     oasis_mpi_bcastr0, &
     oasis_mpi_bcastr1, &
     oasis_mpi_bcastr2, &
     oasis_mpi_bcastr3
   end interface
   interface oasis_mpi_gathScatVInit ; module procedure &
     oasis_mpi_gathScatVInitr1
   end interface
   interface oasis_mpi_gatherv ; module procedure &
     oasis_mpi_gatherVr1
   end interface
   interface oasis_mpi_scatterv ; module procedure &
     oasis_mpi_scatterVr1
   end interface
   interface oasis_mpi_sum ; module procedure &
     oasis_mpi_sumi0, &
     oasis_mpi_sumi1, &
     oasis_mpi_sumb0, &
     oasis_mpi_sumb1, &
     oasis_mpi_sumr0, &
     oasis_mpi_sumr1, &
     oasis_mpi_sumr2, &
     oasis_mpi_sumr3
   end interface
   interface oasis_mpi_min ; module procedure &
     oasis_mpi_mini0, &
     oasis_mpi_mini1, &
     oasis_mpi_minr0, &
     oasis_mpi_minr1
   end interface
   interface oasis_mpi_max ; module procedure &
     oasis_mpi_maxi0, &
     oasis_mpi_maxi1, &
     oasis_mpi_maxr0, &
     oasis_mpi_maxr1
   end interface

! mpi library include file
#include <mpif.h>         

!===============================================================================
CONTAINS
!===============================================================================

SUBROUTINE oasis_mpi_chkerr(rcode,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: rcode  ! input MPI error code
   character(*),         intent(in) :: string ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_chkerr'
   character(MPI_MAX_ERROR_STRING)  :: lstring
   integer(ip_i4_p)             :: len
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: layer on MPI error checking
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lstring = ' '
   if (rcode /= MPI_SUCCESS) then
     call MPI_ERROR_STRING(rcode,lstring,len,ierr)
     write(nulprt,*) trim(subName),' model :',compid,' proc :',&
                     mpi_rank_local,":",lstring(1:len)
     CALL oasis_flush(nulprt)
     call oasis_mpi_abort(string,rcode)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_chkerr

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sendi0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec     ! send value
   integer(ip_i4_p), intent(in) :: pid      ! pid to send to
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sendi0'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a single integer
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_SEND(lvec,lsize,MPI_INTEGER,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendi0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sendi1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec(:)  ! in/out local values
   integer(ip_i4_p), intent(in) :: pid      ! pid to send to
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sendi1'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of integers
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_SEND(lvec,lsize,MPI_INTEGER,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendi1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sendr0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec     ! in/out local values
   integer(ip_i4_p), intent(in) :: pid      ! pid to send to
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sendr0'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a real scalar
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_SEND(lvec,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendr0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sendr1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:)  ! in/out local values
   integer(ip_i4_p), intent(in) :: pid      ! pid to send to
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sendr1'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_SEND(lvec,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sendr3(array,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real   (ip_double_p), intent(in) :: array(:,:,:)  ! in/out local values
   integer(ip_i4_p), intent(in) :: pid           ! pid to send to
   integer(ip_i4_p), intent(in) :: tag           ! tag
   integer(ip_i4_p), intent(in) :: comm          ! mpi communicator
   character(*),optional,intent(in) :: string        ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sendr3'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Send a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(array)

   call MPI_SEND(array,lsize,MPI_REAL8,pid,tag,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sendr3

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_recvi0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(out):: lvec     ! in/out local values
   integer(ip_i4_p), intent(in) :: pid      ! pid to recv from
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_recvi0'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_RECV(lvec,lsize,MPI_INTEGER,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvi0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_recvi1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(out):: lvec(:)  ! in/out local values
   integer(ip_i4_p), intent(in) :: pid      ! pid to recv from
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_recvi1'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_RECV(lvec,lsize,MPI_INTEGER,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvi1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_recvr0(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(out):: lvec     ! in/out local values
   integer(ip_i4_p), intent(in) :: pid      ! pid to recv from
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_recvr0'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1

   call MPI_RECV(lvec,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvr0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_recvr1(lvec,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(out):: lvec(:)  ! in/out local values
   integer(ip_i4_p), intent(in) :: pid      ! pid to recv from
   integer(ip_i4_p), intent(in) :: tag      ! tag
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_recvr1'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(lvec)

   call MPI_RECV(lvec,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_recvr3(array,pid,tag,comm,string)

   IMPLICIT none

   !----- arguments ---
   real   (ip_double_p), intent(out):: array(:,:,:)  ! in/out local values
   integer(ip_i4_p), intent(in) :: pid           ! pid to recv from
   integer(ip_i4_p), intent(in) :: tag           ! tag
   integer(ip_i4_p), intent(in) :: comm          ! mpi communicator
   character(*),optional,intent(in) :: string        ! message

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_recvr3'
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: status(MPI_STATUS_SIZE)  ! mpi status info
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Recv a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(array)

   call MPI_RECV(array,lsize,MPI_REAL8,pid,tag,comm,status,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_recvr3

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcasti0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(inout):: vec      ! vector of 1
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcasti0'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast an integer
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_INTEGER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcasti0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastl0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   logical, intent(inout):: vec      ! vector of 1
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcastl0'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a logical
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_LOGICAL,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastl0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastc0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   character(len=*), intent(inout)    :: vec      ! vector of 1
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcastc0'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a character string
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = len(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_CHARACTER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastc0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastc1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   character(len=*), intent(inout)    :: vec(:)   ! 1D vector
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcastc1'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a character string
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)*len(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_CHARACTER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastc1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastr0(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(inout):: vec      ! vector of 1
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcastr0'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a real
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = 1
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcasti1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(inout):: vec(:)   ! vector 
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcasti1'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a vector of integers
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_INTEGER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcasti1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastl1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   logical, intent(inout):: vec(:)      ! vector of 1
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcastl1'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a logical
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_LOGICAL,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastl1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastr1(vec,comm,string,pebcast)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(inout):: vec(:)   ! vector 
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_bcastr1'
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a vector of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(vec)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(vec,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastr2(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),    intent(inout):: arr(:,:) ! array, 2d 
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local -----
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

   !----- formats -----
   character(*),parameter             :: subName = 'oasis_mpi_bcastr2'

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 2d array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr2

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcasti2(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   integer,              intent(inout):: arr(:,:) ! array, 2d 
   integer(ip_i4_p), intent(in)   :: comm     ! mpi communicator
   character(*),optional,intent(in)   :: string   ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local -----
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

   !----- formats -----
   character(*),parameter             :: subName = 'oasis_mpi_bcasti2'

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 2d array of integers
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_INTEGER,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcasti2

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_bcastr3(arr,comm,string,pebcast)

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),    intent(inout):: arr(:,:,:) ! array, 3d 
   integer(ip_i4_p), intent(in)   :: comm       ! mpi communicator
   character(*),optional,intent(in)   :: string     ! message
   integer(ip_i4_p), optional, intent(in)   :: pebcast  ! bcast pe (otherwise zero)

   !----- local -----
   integer(ip_i4_p)               :: ierr
   integer(ip_i4_p)               :: lsize
   integer(ip_i4_p)               :: lpebcast

   !----- formats -----
   character(*),parameter             :: subName = 'oasis_mpi_bcastr3'

!-------------------------------------------------------------------------------
! PURPOSE: Broadcast a 3d array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lsize = size(arr)
   lpebcast = 0
   if (present(pebcast)) lpebcast = pebcast

   call MPI_BCAST(arr,lsize,MPI_REAL8,lpebcast,comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_bcastr3

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_gathScatvInitr1(comm, rootid, locArr, glob1DArr, globSize, &
                                   displs, string )

   IMPLICIT none

   !----- arguments -----
   integer(ip_i4_p), intent(in)   :: comm          ! mpi communicator
   integer(ip_i4_p), intent(in)   :: rootid        ! MPI task to gather/scatter on
   real(ip_double_p),    intent(in)   :: locArr(:)     ! Local array of distributed data
   real(ip_double_p),    pointer      :: glob1DArr(:)  ! Global 1D array of gathered data
   integer(ip_i4_p), pointer      :: globSize(:)   ! Size of each distributed piece
   integer(ip_i4_p), pointer      :: displs(:)     ! Displacements for receive
   character(*),optional,intent(in)   :: string        ! message

   !----- local -----
   integer(ip_i4_p)               :: npes          ! Number of MPI tasks
   integer(ip_i4_p)               :: locSize       ! Size of local distributed data
   integer(ip_i4_p), pointer      :: sendSize(:)   ! Size to send for initial gather
   integer(ip_i4_p)               :: i             ! Index
   integer(ip_i4_p)               :: rank          ! Rank of this MPI task
   integer(ip_i4_p)               :: nSize         ! Maximum size to send
   integer(ip_i4_p)               :: ierr          ! Error code
   integer(ip_i4_p)               :: nSiz1D        ! Size of 1D global array
   integer(ip_i4_p)               :: maxSize       ! Maximum size

   !----- formats -----
   character(*),parameter             :: subName = 'oasis_mpi_gathScatvInitr1'

!-------------------------------------------------------------------------------
! PURPOSE: Setup arrays for a gatherv/scatterv operation
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   locSize = size(locarr)
   call oasis_mpi_commsize( comm, npes )
   call oasis_mpi_commrank( comm, rank )
   allocate( globSize(npes) )
   !
   ! --- Gather the send global sizes from each MPI task -----------------------
   !
   allocate( sendSize(npes) )
   sendSize(:) = 1
   globSize(:) = 1
   call MPI_GATHER( locSize, 1, MPI_INTEGER, globSize, sendSize, &
                    MPI_INTEGER, rootid, comm, ierr )
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif
   deallocate( sendSize )
   !
   ! --- Prepare the displacement and allocate arrays -------------------------
   !
   allocate( displs(npes) )
   displs(1) = 0
   if ( rootid /= rank )then
      maxSize = 1
      globSize = 1
   else
      maxSize = maxval(globSize)
   end if
   nsiz1D  = min(maxSize,globSize(1))
   do i = 2, npes
      nSize = min(maxSize,globSize(i-1))
      displs(i) = displs(i-1) + nSize
      nsiz1D = nsiz1D + min(maxSize,globSize(i))
   end do
   allocate( glob1DArr(nsiz1D) )
   !----- Do some error checking for the root task arrays computed ----
   if ( rootid == rank )then
      if ( nsiz1D /= sum(globSize) ) &
         call oasis_mpi_abort( subName//" : Error, size of global array not right" )
      if ( any(displs < 0) .or. any(displs >= nsiz1D) ) &
         call oasis_mpi_abort( subName//" : Error, displacement array not right" )
      if ( (displs(npes)+globSize(npes)) /= nsiz1D ) &
         call oasis_mpi_abort( subName//" : Error, displacement array values too big" )
   end if

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_gathScatvInitr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_gathervr1(locarr, locSize, glob1DArr, globSize, displs, rootid, &
                             comm, string )

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),    intent(in)   :: locArr(:)     ! Local array
   real(ip_double_p),    intent(inout):: glob1DArr(:)  ! Global 1D array to receive in on
   integer(ip_i4_p), intent(in)   :: locSize       ! Number to send this PE
   integer(ip_i4_p), intent(in)   :: globSize(:)   ! Number to receive each PE
   integer(ip_i4_p), intent(in)   :: displs(:)     ! Displacements for receive
   integer(ip_i4_p), intent(in)   :: rootid        ! MPI task to gather on
   integer(ip_i4_p), intent(in)   :: comm          ! mpi communicator
   character(*),optional,intent(in)   :: string        ! message

   !----- local -----
   integer(ip_i4_p)               :: ierr          ! Error code

   !----- formats -----
   character(*),parameter             :: subName = 'oasis_mpi_gathervr1'

!-------------------------------------------------------------------------------
! PURPOSE: Gather a 1D array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_GATHERV( locarr, locSize, MPI_REAL8, glob1Darr, globSize, displs, &
                     MPI_REAL8, rootid, comm, ierr )
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_gathervr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_scattervr1(locarr, locSize, glob1Darr, globSize, displs, rootid, &
                              comm, string )

   IMPLICIT none

   !----- arguments -----
   real(ip_double_p),    intent(out)  :: locarr(:)     ! Local array
   real(ip_double_p),    intent(in)   :: glob1Darr(:)  ! Global 1D array to send from
   integer(ip_i4_p), intent(in)   :: locSize       ! Number to receive this PE
   integer(ip_i4_p), intent(in)   :: globSize(:)   ! Number to send to each PE
   integer(ip_i4_p), intent(in)   :: displs(:)     ! Displacements for send
   integer(ip_i4_p), intent(in)   :: rootid        ! MPI task to scatter on
   integer(ip_i4_p), intent(in)   :: comm          ! mpi communicator
   character(*),optional,intent(in)   :: string        ! message

   !----- local -----
   integer(ip_i4_p)               :: ierr          ! Error code

   !----- formats -----
   character(*),parameter             :: subName = 'oasis_mpi_scattervr1'

!-------------------------------------------------------------------------------
! PURPOSE: Scatter a 1D array of reals
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_SCATTERV( glob1Darr, globSize, displs, MPI_REAL8, locarr, locSize, &
                      MPI_REAL8, rootid, comm, ierr )
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_scattervr1


!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumi0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec     ! in/out local values
   integer(ip_i4_p), intent(out):: gvec     ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumi0'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumi0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumi1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec(:)  ! in/out local values
   integer(ip_i4_p), intent(out):: gvec(:)  ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumi1'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumi1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumb0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i8_p), intent(in) :: lvec     ! in/out local values
   integer(ip_i8_p), intent(out):: gvec     ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumb0'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumb0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumb1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i8_p), intent(in) :: lvec(:)  ! in/out local values
   integer(ip_i8_p), intent(out):: gvec(:)  ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumb1'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumb1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec     ! in/out local values
   real(ip_double_p),    intent(out):: gvec     ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumr0'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:)  ! in/out local values
   real(ip_double_p),    intent(out):: gvec(:)  ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumr1'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumr2(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:,:)! in/out local values
   real(ip_double_p),    intent(out):: gvec(:,:)! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumr2'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr2

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_sumr3(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:,:,:) ! in/out local values
   real(ip_double_p),    intent(out):: gvec(:,:,:) ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_sumr3'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds sum of a distributed vector of values, assume local sum
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_SUM
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_sumr3

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_mini0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec     ! in/out local values
   integer(ip_i4_p), intent(out):: gvec     ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_mini0'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_mini0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_mini1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec(:)  ! in/out local values
   integer(ip_i4_p), intent(out):: gvec(:)  ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_mini1'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_mini1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_minr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec     ! in/out local values
   real(ip_double_p),    intent(out):: gvec     ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_minr0'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_minr0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_minr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:)  ! in/out local values
   real(ip_double_p),    intent(out):: gvec(:)  ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_minr1'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds min of a distributed vector of values, assume local min
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MIN
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_minr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_maxi0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec     ! in/out local values
   integer(ip_i4_p), intent(out):: gvec     ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_maxi0'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxi0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_maxi1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   integer(ip_i4_p), intent(in) :: lvec(:)  ! in/out local values
   integer(ip_i4_p), intent(out):: gvec(:)  ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_maxi1'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_INTEGER,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxi1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_maxr0(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec     ! in/out local values
   real(ip_double_p),    intent(out):: gvec     ! in/out global values
   integer(ip_i4_p), intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_maxr0'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = 1
   gsize = 1

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxr0

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_maxr1(lvec,gvec,comm,string,all)

   IMPLICIT none

   !----- arguments ---
   real(ip_double_p),    intent(in) :: lvec(:)  ! in/out local values
   real(ip_double_p),    intent(out):: gvec(:)  ! in/out global values
   integer(ip_i4_p) ,    intent(in) :: comm     ! mpi communicator
   character(*),optional,intent(in) :: string   ! message
   logical,     optional,intent(in) :: all      ! allreduce if true

   !----- local ---
   character(*),parameter           :: subName = 'oasis_mpi_maxr1'
   logical                          :: lall
   character(len=256)           :: lstring
   integer(ip_i4_p)             :: reduce_type  ! mpi reduction type
   integer(ip_i4_p)             :: lsize
   integer(ip_i4_p)             :: gsize
   integer(ip_i4_p)             :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: Finds max of a distributed vector of values, assume local max
!          already computed
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   reduce_type = MPI_MAX
   if (present(all)) then
     lall = all
   else
     lall = .false.
   endif
   if (present(string)) then
     lstring = trim(subName)//":"//trim(string)
   else
     lstring = trim(subName)
   endif

   lsize = size(lvec)
   gsize = size(gvec)

   if (lsize /= gsize) then
     call oasis_mpi_abort(subName//" lsize,gsize incompatable "//trim(string))
   endif

   if (lall) then
     call MPI_ALLREDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_ALLREDUCE")
   else
     call MPI_REDUCE(lvec,gvec,gsize,MPI_REAL8,reduce_type,0,comm,ierr)
     call oasis_mpi_chkerr(ierr,trim(lstring)//" MPI_REDUCE")
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_maxr1

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_commsize(comm,size,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm
   integer,intent(out)                :: size
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_commsize'
   integer(ip_i4_p)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI commsize
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_COMM_SIZE(comm,size,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_commsize

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_commrank(comm,rank,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm
   integer,intent(out)                :: rank
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_commrank'
   integer(ip_i4_p)                   :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI commrank
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_COMM_RANK(comm,rank,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_commrank

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_initialized(flag,string)

   IMPLICIT none

   !----- arguments ---
   logical,intent(out)                :: flag
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_initialized'
   integer(ip_i4_p)                   :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI initialized
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_INITIALIZED(flag,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_initialized

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_wtime(wtime)

   IMPLICIT none

   !----- arguments ---
   real(ip_r8_p), intent(out) :: wtime  ! time

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_wtime'

!-------------------------------------------------------------------------------
! PURPOSE: MPI wtime
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   wtime = MPI_WTIME()

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_wtime

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_abort(string,rcode)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   ! message
   integer,optional,intent(in)        :: rcode    ! optional code

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_abort'
   integer(ip_i4_p)                   :: ierr
   integer                            :: rc       ! return code

!-------------------------------------------------------------------------------
! PURPOSE: MPI abort
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   if ( present(string) .and. present(rcode) ) then
      write(nulprt,*) trim(subName),' model :',compid,' proc :',&
                      mpi_rank_local,":",trim(string),rcode
      CALL oasis_flush(nulprt)
   endif
   if ( present(rcode) )then
      rc = rcode
   else
      rc = 1001
   end if
   call MPI_ABORT(MPI_COMM_WORLD,rcode,ierr)

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_abort

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_barrier(comm,string)

   IMPLICIT none

   !----- arguments ---
   integer,intent(in)                 :: comm
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_barrier'
   integer(ip_i4_p)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI barrier
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_BARRIER(comm,ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_barrier

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_init(string)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_init'
   integer(ip_i4_p)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI init
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_INIT(ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_init

!===============================================================================
!===============================================================================

SUBROUTINE oasis_mpi_finalize(string)

   IMPLICIT none

   !----- arguments ---
   character(*),optional,intent(in)   :: string   ! message

   !----- local ---
   character(*),parameter             :: subName = 'oasis_mpi_finalize'
   integer(ip_i4_p)               :: ierr

!-------------------------------------------------------------------------------
! PURPOSE: MPI finalize
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   call MPI_BARRIER(MPI_COMM_WORLD,ierr)
   call MPI_FINALIZE(ierr)
   if (present(string)) then
     call oasis_mpi_chkerr(ierr,subName//trim(string))
   else
     call oasis_mpi_chkerr(ierr,subName)
   endif

   call oasis_debug_exit(subname)

END SUBROUTINE oasis_mpi_finalize

!===============================================================================
!===============================================================================

END MODULE mod_oasis_mpi
