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
!> \file yowrankModule.F90
!> \brief provides RANK array
!> \author Thomas Huxhorn
!> \date 2013

!> Provides access to some information of all threads e.g. iplg
module yowRankModule
  use yowerr
  implicit none
  private
  public :: initRankModule, finalizeRankModule

  type, public :: t_rank
    !> number of local nodes of this rank
    integer :: np = 0

    !> nummer of ghost + resident nodes of this rank
    integer :: npa = 0

    !> Node local to gloabl mapping of this rank
    !> rank%npa long
    integer, allocatable :: iplg(:)

    !> global start node number for every thread
    integer:: IStart = 0
  end type

  !> Provides access to some information of all threads e.g. iplg
  !> \note range [1:nTasks]
  !> \note range[myrank] are filled with the values from this rank
  type(t_rank),public, allocatable :: rank(:)
  integer, public, allocatable :: IPGL_TO_PROC(:), ipgl_tot(:)
  integer, public, allocatable :: ipgl_npa(:)

  contains

  !> allocate and exchange
  subroutine initRankModule()
    use yowDatapool, only: nTasks, myrank
    implicit none
    integer :: stat

    if(allocated(rank)) deallocate(rank)
    allocate(rank(nTasks), stat=stat)
    if(stat/=0) CALL ABORT('rank allocation failure')

    call exchangeIPLG()
    call calcISTART()
  end subroutine

  !> send iplg from this thread to every neighbor thread
  !> \internal
  subroutine exchangeIPLG()
    use yowNodepool, only: np, npa, iplg, np_global
    use yowDatapool, only: nTasks, myrank, comm, itype
    use MPI
    implicit none
    integer :: i, ierr, stat
    integer :: sendRqst(nTasks), recvRqst(nTasks)
    integer :: recvStat(MPI_STATUS_SIZE, nTasks), sendStat(MPI_STATUS_SIZE, nTasks)
    integer IPglob, J, istat

    ! step1 exchange np
    ! step2 exchange npa
    ! step3 allocate rank%iplg
    ! step4 exchange iplg

    ! step1 exchange np
    ! post receives
    do i=1, nTasks
      if(i /= myrank+1) then
        call MPI_IRecv(rank(i)%np, 1, itype, i-1, &
            42, comm, recvRqst(i), ierr)
        if(ierr/=MPI_SUCCESS) then
          CALL PARALLEL_ABORT("MPI_IRecv", ierr)
        endif
      else
        recvRqst(i) = MPI_REQUEST_NULL
      endif
    end do

    ! post sends
    do i=1, nTasks
      if(i /= myrank+1) then
        call MPI_ISend(np, 1, itype, i-1, &
            42, comm, sendRqst(i), ierr)
        if(ierr/=MPI_SUCCESS) then
          CALL PARALLEL_ABORT("MPI_ISend", ierr)
        endif
      else
        sendRqst(i) = MPI_REQUEST_NULL
      endif
    end do

    rank(myrank+1)%np = np

    ! Wait for completion
    call mpi_waitall(nTasks, recvRqst, recvStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
    call mpi_waitall(nTasks, sendRqst, sendStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)

    ! step2 exchange npa
    ! post receives
    do i=1, nTasks
      if(i /= myrank+1) then
        call MPI_IRecv(rank(i)%npa, 1, itype, i-1, &
             42, comm, recvRqst(i), ierr)
        if(ierr/=MPI_SUCCESS) then
          CALL PARALLEL_ABORT("MPI_IRecv", ierr)
        endif
      else
        recvRqst(i) = MPI_REQUEST_NULL
      endif
    end do

    ! post sends
    do i=1, nTasks
      if(i /= myrank+1) then
        call MPI_ISend(npa, 1, itype, i-1, &
            42, comm, sendRqst(i), ierr)
        if(ierr/=MPI_SUCCESS) then
          CALL PARALLEL_ABORT("MPI_ISend", ierr)
        endif
      else
        sendRqst(i) = MPI_REQUEST_NULL
      endif
    end do

    rank(myrank+1)%npa = npa

    ! Wait for completion
    call mpi_waitall(nTasks, recvRqst, recvStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
    call mpi_waitall(nTasks, sendRqst, sendStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)

    ! step3 allocal rank%iplg
    do i=1, nTasks
      if(allocated(rank(i)%iplg)) deallocate(rank(i)%iplg)
      allocate(rank(i)%iplg(rank(i)%npa), stat=stat)
      if(stat/=0) CALL ABORT('rank%iplg allocation failure')
      rank(i)%iplg = 0
    end do

    ! step4 exchange iplg
    ! post receives
    do i=1, nTasks
      if(i /= myrank+1) then
        call MPI_IRecv(rank(i)%iplg, rank(i)%npa, itype, i-1, &
            42, comm, recvRqst(i), ierr)
        if(ierr/=MPI_SUCCESS) then
          CALL PARALLEL_ABORT("MPI_IRecv", ierr)
        endif
      else
        recvRqst(i) = MPI_REQUEST_NULL
      endif
    end do

    ! post sends
    do i=1, nTasks
      if(i /= myrank+1) then
        call MPI_ISend(iplg, npa, itype, i-1, &
            42, comm, sendRqst(i), ierr)
        if(ierr/=MPI_SUCCESS) then
          CALL PARALLEL_ABORT("MPI_ISend", ierr)
        endif
      else
        sendRqst(i) = MPI_REQUEST_NULL
      endif
    end do

    rank(myrank+1)%iplg = iplg

    ! Wait for completion
    call mpi_waitall(nTasks, recvRqst, recvStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)
    call mpi_waitall(nTasks, sendRqst, sendStat,ierr)
    if(ierr/=MPI_SUCCESS) CALL PARALLEL_ABORT("waitall", ierr)

    allocate(IPGL_TO_PROC(np_global), ipgl_tot(np_global), stat=istat)
    if(istat /= 0) CALL PARALLEL_ABORT("allocatation error", 1)
    do i=1,nTasks
      DO J=1,rank(i)%np
        IPglob=rank(i)%iplg(J)
        IPGL_TO_PROC(IPglob)=i
        ipgl_tot(IPglob)=J
      END DO
    END DO
    allocate(ipgl_npa(np_global), stat=istat)
    if(istat /= 0) CALL PARALLEL_ABORT("allocatation error b", 1)
    ipgl_npa=0
    DO J=1,rank(myrank+1)%npa
      IPglob=rank(myrank+1)%iplg(J)
      ipgl_npa(IPglob)=J
    END DO
  end subroutine

  !> \internal
  subroutine calcISTART()
    use yowDatapool, only: nTasks, myrank
    implicit none
    integer :: ir

    rank(1)%IStart = 1
    do ir=2, nTasks
      rank(ir)%IStart = rank(ir-1)%IStart + rank(ir-1)%np
    end do
  end subroutine

  subroutine finalizeRankModule()
    implicit none
    integer :: i
  
    if(allocated(rank)) then
      do i=1, size(rank)
        if(allocated(rank(i)%iplg)) deallocate(rank(i)%iplg)
      end do
      deallocate(rank)
    endif
  end subroutine
end module
