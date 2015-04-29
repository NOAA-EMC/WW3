MODULE mod_oasis_data
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!
  USE mod_oasis_kinds

  IMPLICIT NONE

  public

! public prism_data_zero

#include <mpif.h>

! GENERAL

  integer(kind=ip_i4_p) :: compid
  character(len=ic_lvar):: compnm
  !
  ! Variables 

  integer(ip_intwp_p)   :: mvar
  integer(kind=ip_i4_p),parameter :: mvarcpl = 10
  !
  CHARACTER(len=ic_field), POINTER :: total_namsrcfld(:), total_namdstfld(:)
  !
  type prism_var_type
     character(len=ic_lvar):: name
     integer(kind=ip_i4_p) :: part
     integer(kind=ip_i4_p) :: ndim
     integer(kind=ip_i4_p) :: num
     integer(kind=ip_i4_p) :: ops
     integer(kind=ip_i4_p) :: type
     integer(kind=ip_i4_p) :: size
     integer(kind=ip_i4_p) :: ncpl
     integer(kind=ip_i4_p) :: cpl(mvarcpl)
  end type prism_var_type

  TYPE(prism_var_type),POINTER :: prism_var(:)

! MPI

  INTEGER(kind=ip_i4_p) :: mpi_comm_global
  INTEGER(kind=ip_i4_p) :: mpi_rank_global
  INTEGER(kind=ip_i4_p) :: mpi_size_global
  INTEGER(kind=ip_i4_p) :: mpi_comm_local
  INTEGER(kind=ip_i4_p) :: mpi_rank_local
  INTEGER(kind=ip_i4_p) :: mpi_size_local
  INTEGER(kind=ip_i4_p) :: mpi_root_local
  INTEGER(kind=ip_i4_p) :: mpi_err
  INTEGER(kind=ip_i4_p),allocatable :: mpi_root_global(:)  ! for each model, the rank in comm_world 
                                                           ! of the root process

  character(len=*) ,parameter :: cspval = "spval_undef"
  real(ip_double_p),parameter :: rspval = 1.0e36
  integer(ip_i4_p) ,parameter :: ispval = -999999

  real(ip_double_p),parameter :: prism_pi = 3.14159265358979323846
  real(ip_double_p),parameter :: eradius = 6371229.    ! meters

!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

  SUBROUTINE oasis_data_zero()

  IMPLICIT NONE

  character(len=*),parameter :: subname = 'oasis_data_zero'

  nulprt = 6
  nulprt1 = 6
  OASIS_debug = 0
  TIMER_debug = 0
  compid = -1
  compnm = trim(cspval)
  mpi_comm_global = -1
  mpi_rank_global = -1
  mpi_size_global = -1
  mpi_comm_local = -1
  mpi_rank_local = -1
  mpi_size_local = -1
  
END SUBROUTINE oasis_data_zero

!------------------------------------------------------------
END MODULE mod_oasis_data


