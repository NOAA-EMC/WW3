!PDLIB Software License
!
!Software, as understood herein, shall be broadly interpreted as being inclusive of algorithms,
!source code, object code, data bases and related documentation, all of which shall be furnished
!free of charge to the Licensee. Corrections, upgrades or enhancements may be furnished and, if
!furnished, shall also be furnished to the Licensee without charge. NOAA, however, is not
!required to develop or furnish such corrections, upgrades or enhancements.
!Roland & Partner software, whether that initially furnished or corrections or upgrades,
!are furnished "as is". Roland & Partner furnishes its software without any warranty
!whatsoever and is not responsible for any direct, indirect or consequential damages
!that may be incurred by the Licensee. Warranties of merchantability, fitness for any
!particular purpose, title, and non-infringement, are specifically negated.
!The Licensee is not required to develop any software related to the licensed software.
!However, in the event that the Licensee does so, the Licensee is required to offer same
!to Roland & Partner for inclusion under the instant licensing terms with Roland & Partner
!licensed software along with documentation regarding its principles, use and its advantages.
!This includes changes to the wave model proper including numerical and physical approaches
!to wave modeling, and boundary layer parameterizations embedded in the wave model
!A Licensee may reproduce sufficient software to satisfy its needs.
!All copies shall bear the name of the software with any version number
!as well as replicas of any applied copyright notice, trademark notice,
!other notices and credit lines. Additionally, if the copies have been modified,
!e.g. with deletions or additions, this shall be so stated and identified.
!All of Licensee's employees who have a need to use the software may have access
!to the software but only after reading the instant license and stating, in writing,
!that they have read and understood the license and have agreed to its terms.
!Licensee is responsible for employing reasonable efforts to assure
!that only those of its employees that should have access to the software, in fact, have access.
!The Licensee may use the software for any purpose relating to sea state prediction.
!No disclosure of any portion of the software, whether by means of a media or verbally,
!may be made to any third party by the Licensee or the Licensee's employees
!The Licensee is responsible for compliance with any applicable export or
!import control laws of the United States, the European Union and Germany.
!
!© 2009 Roland&Partner, Georgenstr.32, 64297 Germany. All rights reserved.
!PDLIB is a trademark of Roland & Partner. No unauthorized use without permission.
!
!> Has only the ghost nodes assign to a neighbor domain
module yowExchangeModule
  use yowDatapool, only: rkind
  use MPI, only: MPI_DATATYPE_NULL
  implicit none
  private
  public :: initNbrDomains, createMPITypes, setDimSize
  public :: finalizeExchangeModule, PDLIB_exchange1Dreal
  public :: PDLIB_exchange2Dreal, PDLIB_exchange2Dreal_zero

  !> Holds some data belong to a neighbor Domain
  type, public :: t_neighborDomain

    !> the domain ID
    !> The domain ID of the neighbor domain. Starts by 1
    integer :: domainID = 0

    !> number of ghosts nodes.
    !> holds the number of ghosts nodes the two domains share together
    !> (the neighbor domain has a copy of the ghosts. when you change some
    !> value in a ghost node, it dosen't change in the node on the other domain)
    integer :: numNodesToReceive = 0

    !> this are the ghosts that we.
    !> has in this neighbor domain. global node IDs
    integer, allocatable :: nodesToReceive(:)

    !> number of nodes we have to send to this neighbor
    integer :: numNodesToSend = 0

    !> this are the ghosts from this neighbor.
    !> global node IDs to send
    integer, allocatable :: nodesToSend(:)

    ! MPI datatypes for size(U) == npa+1  U(0:npa)

    !> MPI datatypes for 1D exchange
    integer :: p1DRsendType_zero = MPI_DATATYPE_NULL
    integer :: p1DRrecvType_zero = MPI_DATATYPE_NULL

    !> MPI datatypes for 2D exchange
    integer :: p2DRsendType_zero = MPI_DATATYPE_NULL
    integer :: p2DRrecvType_zero = MPI_DATATYPE_NULL

    ! MPI datatypes for size(U) == npa  U(1:npa)
    !> MPI datatypes for 1D exchange
    integer :: p1DRsendType = MPI_DATATYPE_NULL
    integer :: p1DRrecvType = MPI_DATATYPE_NULL
    !> MPI datatypes for 2D exchange
    integer :: p2DRsendType1 = MPI_DATATYPE_NULL
    integer :: p2DRrecvType1 = MPI_DATATYPE_NULL
    integer :: p2DRsendType2 = MPI_DATATYPE_NULL
    integer :: p2DRrecvType2 = MPI_DATATYPE_NULL

  contains
    !     procedure :: exchangeGhostIds
    !     final :: finalizeNeighborDomain
    procedure :: finalize
    procedure :: createMPIType

  end type t_neighborDomain

  !> Knows for all domains neighbors, which node we must send or revc from neighbor domains
  !> from 1 to nConnDomains
  type(t_neighborDomain), public, allocatable :: neighborDomains(:)

  !> Number of neighbor domains
  integer, public :: nConnDomains = 0

  !> number of the second dimension for exchange
  integer, public :: n2ndDim = 1

  !> number of the second dimension for exchange (nth only for wave model) 
  integer, public :: nnthDim = 1


contains


  subroutine finalize(this)
    use yowerr
    use MPI
    implicit none
    class(t_neighborDomain), intent(inout) :: this
    integer :: ierr

    if(allocated(this%nodesToSend))    deallocate(this%nodesToSend)
    if(allocated(this%nodesToReceive)) deallocate(this%nodesToReceive)

    call mpi_type_free(this%p1DRsendType_zero, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p1DRrecvType_zero, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p2DRsendType_zero, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p2DRrecvType_zero, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p1DRsendType, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p1DRrecvType, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p2DRsendType1, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p2DRrecvType1, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p2DRsendType2, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
    call mpi_type_free(this%p2DRrecvType2, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("freeMPItype", ierr)
  end subroutine finalize

  ! create MPI indexed datatype for this neighborDomain
  subroutine createMPIType(this)
    use yowerr
    use MPI
    use yowNodepool, only: ghostgl, np, ipgl
    use yowDatapool, only: rtype, itype
    implicit none
    class(t_neighborDomain), intent(inout) :: this

    integer :: ierr
    integer :: dsplSend(this%numNodesToSend)
    integer :: dsplRecv(this%numNodesToReceive)

    ! MPI datatypes for size(U) == npa+1  U(0:npa)
    dsplSend = ipgl(this%nodesToSend)
    dsplRecv = ghostgl(this%nodesToReceive) + np

    ! p1D real
    call mpi_type_create_indexed_block(this%numNodesToSend, 1, dsplSend, rtype, this%p1DRsendType_zero, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p1DRsendType_zero,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)

    call mpi_type_create_indexed_block(this%numNodesToReceive, 1, dsplRecv, rtype, this%p1DRrecvType_zero, ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p1DRrecvType_zero,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)

    ! MPI datatypes for size(U) == npa  U(1:npa)
    dsplSend(:) = dsplSend(:) - 1 ! C count from 0; FORTRAN count from 1
    dsplRecv(:) = dsplRecv(:) - 1

    ! p1D real
    call mpi_type_create_indexed_block(this%numNodesToSend, 1, dsplSend, rtype, this%p1DRsendType,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p1DRsendType,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)

    call mpi_type_create_indexed_block(this%numNodesToReceive, 1, dsplRecv, rtype, this%p1DRrecvType,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p1DRrecvType,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)

    ! MPI datatypes for size(U) == npa+1  U(0:npa)
    ! p2D real second dim is n2ndDim long
    dsplSend = (ipgl(this%nodesToSend)) * n2ndDim
    dsplRecv = (ghostgl(this%nodesToReceive) + np) * n2ndDim
    call mpi_type_create_indexed_block(this%numNodesToSend, n2ndDim, dsplSend, rtype, this%p2DRsendType_zero,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p2DRsendType_zero,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)

    call mpi_type_create_indexed_block(this%numNodesToReceive, n2ndDim, dsplRecv, rtype, this%p2DRrecvType_zero,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p2DRrecvType_zero,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)

    ! MPI datatypes for size(U) == npa  U(1:npa)
    ! p2D real second dim is n2ndDim long
    dsplSend = (ipgl(this%nodesToSend)-1) * n2ndDim
    dsplRecv = (ghostgl(this%nodesToReceive) + np -1) * n2ndDim
    call mpi_type_create_indexed_block(this%numNodesToSend, n2ndDim, dsplSend, rtype, this%p2DRsendType1,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p2DRsendType1,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)

    call mpi_type_create_indexed_block(this%numNodesToReceive, n2ndDim, dsplRecv, rtype, this%p2DRrecvType1,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)
    call mpi_type_commit(this%p2DRrecvType1,ierr)
    if(ierr /= MPI_SUCCESS) CALL PARALLEL_ABORT("createMPIType", ierr)


  end subroutine createMPIType

  subroutine initNbrDomains(nConnD)
    use yowerr
    implicit none
    integer, intent(in) :: nConnD
    integer :: stat

    call finalizeExchangeModule()
    nConnDomains = nConnD
    allocate(neighborDomains(nConnDomains), stat=stat)
    if(stat/=0)  CALL ABORT('neighborDomains allocation failure')
  end subroutine initNbrDomains

  subroutine createMPITypes()
    implicit none
    integer :: i

    do i=1, nConnDomains
      call neighborDomains(i)%createMPIType()
    end do
  end subroutine createMPITypes

  !> exchange values in U.
  !> \param[inout] U array with values to exchange. np+ng long.
  !> Send values from U(1:np) to other threads.
  !> Receive values from other threads and updates U(np+1:np+ng
  !> \note MPI recv tag: 10000 + MPI rank
  !> \note MPI send tag: 10000 + neighbor MPI rank
  subroutine PDLIB_exchange1Dreal(U)
    use yowDatapool, only: comm, myrank, rkind
    use yowNodepool, only: t_Node, nodes_global, np, ng, ghosts, npa
    use yowerr
    use MPI
    implicit none
    real(kind=rkind), intent(inout) :: U(:)

    integer :: i, ierr, tag
    integer :: sendRqst(nConnDomains), recvRqst(nConnDomains)
    integer :: recvStat(MPI_STATUS_SIZE, nConnDomains), sendStat(MPI_STATUS_SIZE, nConnDomains)
    character(len=140) :: errmsg

    if(size(U) /= npa) then
      WRITE(errmsg, *) 'size(U)=', size(U), ' but npa=', npa
      CALL ABORT(errmsg)
    endif

    ! post receives
    do i=1, nConnDomains
      tag = 10000 + myrank
      call MPI_IRecv(U, 1, neighborDomains(i)%p1DRrecvType, &
           neighborDomains(i)%domainID-1, tag, comm, &
           recvRqst(i), ierr)
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_IRecv", ierr)
      endif
    enddo

    ! post sends
    do i=1, nConnDomains
      tag = 10000 + (neighborDomains(i)%domainID-1)
      call MPI_ISend(U, 1, neighborDomains(i)%p1DRsendType, &
           neighborDomains(i)%domainID-1, tag, comm, &
           sendRqst(i), ierr);
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_ISend", ierr)
      endif
    end do

    ! Wait for completion
    call mpi_waitall(nConnDomains, recvRqst, recvStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
    call mpi_waitall(nConnDomains, sendRqst, sendStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
  end subroutine PDLIB_exchange1Dreal


  !> \overload PDLIB_exchange1Dreal
  !>
  !> \note MPI recv tag: 30000 + MPI rank
  !> \note MPI send tag: 30000 + neighbor MPI rank
  subroutine PDLIB_exchange2Dreal(U)
    use yowDatapool, only: comm, myrank, rkind
    use yowNodepool, only: t_Node, nodes_global, np, ng, ghosts, npa
    use yowerr
    use MPI
    USE W3ODATMD, only : IAPROC
    implicit none
    real(kind=rkind), intent(inout) :: U(:,:)

    integer :: i, ierr, tag
    integer :: sendRqst(nConnDomains), recvRqst(nConnDomains)
    integer :: recvStat(MPI_STATUS_SIZE, nConnDomains), sendStat(MPI_STATUS_SIZE, nConnDomains)


#ifdef W3_DEBUGEXCH
    WRITE(740+IAPROC,*) 'PDLIB_exchange2Dreal, step 3'
    FLUSH(740+IAPROC)
#endif

    ! post receives
#ifdef W3_DEBUGEXCH
    WRITE(740+IAPROC,*) 'PDLIB_exchange2Dreal, step 4'
    FLUSH(740+IAPROC)
#endif
    do i=1, nConnDomains
      tag = 30000 + myrank
      call MPI_IRecv(U, 1, neighborDomains(i)%p2DRrecvType1, &
           neighborDomains(i)%domainID-1, tag, comm, &
           recvRqst(i), ierr)
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_IRecv", ierr)
      endif
    enddo
#ifdef W3_DEBUGEXCH
    WRITE(740+IAPROC,*) 'PDLIB_exchange2Dreal, step 5'
    FLUSH(740+IAPROC)
#endif

    ! post sends
    do i=1, nConnDomains
      tag = 30000 + (neighborDomains(i)%domainID-1)
      call MPI_ISend(U, 1, neighborDomains(i)%p2DRsendType1, &
           neighborDomains(i)%domainID-1, tag, comm, &
           sendRqst(i), ierr)
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_ISend", ierr)
      endif
    end do
#ifdef W3_DEBUGEXCH
    WRITE(740+IAPROC,*) 'PDLIB_exchange2Dreal, step 6'
    FLUSH(740+IAPROC)
#endif

    ! Wait for completion
    call mpi_waitall(nConnDomains, recvRqst, recvStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
#ifdef W3_DEBUGEXCH
    WRITE(740+IAPROC,*) 'PDLIB_exchange2Dreal, step 11'
    FLUSH(740+IAPROC)
#endif
    call mpi_waitall(nConnDomains, sendRqst, sendStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
#ifdef W3_DEBUGEXCH
    WRITE(740+IAPROC,*) 'PDLIB_exchange2Dreal, step 12'
    FLUSH(740+IAPROC)
#endif
  end subroutine PDLIB_exchange2Dreal


  !> set the size of the second and third dimension for exchange
  !> \note the size of the first dimension is npa
  !> \note call this before initPD()
  subroutine setDimSize(second)
    implicit none
    integer, intent(in) :: second
    n2ndDim = second
  end subroutine setDimSize

  subroutine finalizeExchangeModule()
    implicit none
    integer :: i

    if(allocated(neighborDomains)) then
      do i=1, size(neighborDomains)
        call neighborDomains(i)%finalize()
      end do
      deallocate(neighborDomains)
    endif
  end subroutine finalizeExchangeModule
  !> exchange values in U.
  !> \param[inout] U array with values to exchange. np+ng+1 long.
  !> U[0:npa] Send values from U(1:np) to other threads.
  !> Receive values from other threads and updates U(np+1:np+ng)
  !> \note MPI recv tag: 10001 + MPI rank
  !> \note MPI send tag: 10001 + neighbor MPI rank
  subroutine PDLIB_exchange1Dreal_zero(U)
    use yowDatapool, only: comm, myrank, rkind
    use yowNodepool, only: npa
    use yowErr
    use Mpi
    implicit none
    real(kind=rkind), intent(inout) :: U(0:npa)

    integer :: i, ierr, tag
    integer :: sendRqst(nConnDomains), recvRqst(nConnDomains)
    integer :: recvStat(MPI_STATUS_SIZE, nConnDomains), sendStat(MPI_STATUS_SIZE, nConnDomains)
    character(len=200) errstr

    ! It is impossible to add these range checks because assumed shape array start vom 1:npa+1 even if you allocate it from 0:npa

    !     if(size(U) /= npa+1) then
    !       write(errstr, *) "size(U) /= npa+1", size(U), "should be", npa+1
    !       ABORT(errstr)
    !     endif
    !
    !     if(ubound(U,1) /= npa) then
    !       write(errstr, *) "ubound(U) /= npa", ubound(U), "should be", npa
    !       ABORT(errstr)
    !     endif
    !
    !     if(lbound(U,1) /= 0) then
    !       write(errstr, *) "lbound(U) /= 0", lbound(U), "should be 0"
    !       ABORT(errstr)
    !     endif

    ! post receives
    do i=1, nConnDomains
      tag = 10001 + myrank
      call MPI_IRecv(U, &
           1, &
           neighborDomains(i)%p1DRrecvType_zero, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           recvRqst(i), &
           ierr)
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_IRecv", ierr)
      endif
    enddo

    ! post sends
    do i=1, nConnDomains
      tag = 10001 + (neighborDomains(i)%domainID-1)
      call MPI_ISend(U, &
           1, &
           neighborDomains(i)%p1DRsendType_zero, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           sendRqst(i), &
           ierr);
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_ISend", ierr)
      endif
    end do

    ! Wait for completion
    call mpi_waitall(nConnDomains, recvRqst, recvStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
    call mpi_waitall(nConnDomains, sendRqst, sendStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
  end subroutine PDLIB_exchange1Dreal_zero
  !> \note MPI recv tag: 30001 + MPI rank
  !> \note MPI send tag: 30001 + neighbor MPI rank
  subroutine PDLIB_exchange2Dreal_zero(U)
    use yowDatapool, only: comm, myrank, rkind
    use yowNodepool, only: npa
    use yowErr
    use Mpi
    implicit none
    real(kind=rkind), intent(inout) :: U(n2ndDim,0:npa)

    integer :: i, ierr, tag
    integer :: sendRqst(nConnDomains), recvRqst(nConnDomains)
    integer :: recvStat(MPI_STATUS_SIZE, nConnDomains), sendStat(MPI_STATUS_SIZE, nConnDomains)
    character(len=200) errstr

    ! It is impossible to add these range checks because assumed shape array start vom 1:npa+1 even if you allocate it from 0:npa
    !     if(size(U,2) /= npa+1) then
    !       write(errstr, *) "size(U,2) /= npa+1", size(U,2), "should be", npa+1
    !       ABORT(errstr)
    !     endif
    !
    !     if(ubound(U,2) /= npa) then
    !       write(errstr, *) "ubound(U,2) /= npa", ubound(U,2), "should be", npa
    !       ABORT(errstr)
    !     endif
    !
    !     if(lbound(U,2) /= 0) then
    !       write(errstr, *) "lbound(U,2) /= 0", lbound(U,2), "should be 0"
    !       ABORT(errstr)
    !     endif

    !    if((size(U,1) /= n2ndDim) ) then
    !        write(errstr, *) "size(U,1) /= n2ndDim size(U,1)=", size(U,1), " n2ndDim=", n2ndDim
    !        ABORT(errstr)
    !    endif

    ! post receives
    do i=1, nConnDomains
      tag = 30001 + myrank
      call MPI_IRecv(U, &
           1, &
           neighborDomains(i)%p2DRrecvType_zero, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           recvRqst(i), &
           ierr)
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_IRecv", ierr)
      endif
    enddo

    ! post sends
    do i=1, nConnDomains
      tag = 30001 + (neighborDomains(i)%domainID-1)
      call MPI_ISend(U, &
           1, &
           neighborDomains(i)%p2DRsendType_zero, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           sendRqst(i), &
           ierr);
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("MPI_ISend", ierr)
      endif
    end do


    ! Wait for completion
    call mpi_waitall(nConnDomains, recvRqst, recvStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
    call mpi_waitall(nConnDomains, sendRqst, sendStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
  end subroutine PDLIB_exchange2Dreal_zero


end module yowExchangeModule
