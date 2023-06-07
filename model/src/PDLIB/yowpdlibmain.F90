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
!> \file yowpdlibmain.F90
!> \brief initialization
!> \author Thomas Huxhorn
!> \date 2011-2012
module yowpdlibMain
  use yowerr
  use yowDatapool, only : rkind
  use w3servmd,    only : print_memcheck

  !module default
  implicit none

  private
  public :: initFromGridDim, finalizePD

contains

  !> @param[in] MNP number of nodes global
  !> @param[in] XP node X value
  !> @param[in] XY node Y value
  !> @param[in] DEP node Z value
  !> @param[in] MNE number of element global
  !> @param[in] INE element array
  !> @param[in] secDim size of the second dimensions to exchange
  !> @param[in] thirdDim size of the third dimensions to exchange
  !> @param[in] MPIComm MPI communicator to use with pdlib
  !> @overload initPD1
  subroutine initFromGridDim(MNP, MNE, INE_global, secDim, MPIcomm)
    use yowDatapool,       only: myrank, debugPrePartition, debugPostPartition
    use yowNodepool,       only: np_global, np, np_perProcSum, ng, ipgl, iplg, npa
    use yowElementpool,    only: ne_global,ne
    use yowSidepool,       only: ns, ns_global
    use yowExchangeModule, only: nConnDomains, setDimSize
    use yowRankModule,     only: initRankModule, ipgl_npa

    integer, intent(in) :: MNP, MNE
    integer, intent(in) :: INE_global(3,MNE)
    integer, intent(in) :: secDim
    integer, intent(in) :: MPIcomm
    integer :: istat, memunit

    ! note: myrank=0 until after initMPI is called, so only rank=0 file
    ! contains the 'section 1' information
    memunit = 70000+myrank

    call setDimSize(secDim)
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 1')
#ifdef W3_DEBUGINIT
    Print *, '1: MPIcomm=', MPIcomm
#endif
    call initMPI(MPIcomm)

    memunit = 70000+myrank
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 2')
#ifdef W3_DEBUGINIT
    Print *, '2: After initMPI'
#endif
    call assignMesh(MNP, MNE)
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 3')
#ifdef W3_DEBUGINIT
    Print *, '3: After assignMesh'
#endif
    call prePartition()
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 4')
#ifdef W3_DEBUGINIT
    Print *, '3: After prePartition'
#endif
    call findConnNodes(INE_global)
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 5')
#ifdef W3_DEBUGINIT
    Print *, '4: After findConnNodes'
#endif
    if(debugPrePartition) then
      if(myrank == 0) then
        write(*,*) "pre-partition"
        write(*,*) "# Nodes: ", np_global
        write(*,*) "# Elements: ", ne_global
        write(*,*) "# Sides ", ns_global
        write(*,*) "np_perProcSum :", np_perProcSum
      end if
      write(*,*) "Thread", myrank, "# local nodes ", np
      write(*,*) "Thread", myrank, "# local sides" , ns
    endif

    !   call writeMesh()
#ifdef W3_DEBUGINIT
    Print *, '4.1: After findConnNodes'
#endif
    !    CALL REAL_MPI_BARRIER_PDLIB(MPIcomm, "Before call to runParmetis")
    call runParmetis(MNP)
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 6')
    !    CALL REAL_MPI_BARRIER_PDLIB(MPIcomm, "After call to runParmetis")
#ifdef W3_DEBUGINIT
    Print *, '5: After runParmetis'
#endif
    call postPartition
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 7')
#ifdef W3_DEBUGINIT
    Print *, 'Before findGhostNodes'
#endif
    call findGhostNodes
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 8')

    call findConnDomains
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 9')

    call exchangeGhostIds
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 10')

    call postPartition2(INE_global)
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 11')

    call initRankModule
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 12')

    call ComputeTRIA_IEN_SI_CCON
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 13')

    call ComputeIA_JA_POSI_NNZ
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PDLIB SECTION 14')

    if(debugPostPartition) then
      if(myrank == 0) then
        write(*,*) "New data after partition"
        write(*,*) "# Nodes: ", np_global
        write(*,*) "# Elements: ", ne_global
        write(*,*) "# Sides ", ns_global
        write(*,*) "np_perProcSum :", np_perProcSum
      end if
      write(*,*) "Thread", myrank, "# local elements ", ne
      write(*,*) "Thread", myrank, "# local nodes ", np
      write(*,*) "Thread", myrank, "# local sides" , ns
      write(*,*) "Thread", myrank, "# of ghosts", ng
      write(*,*) "Thread", myrank, "# of neighbor domains", nConnDomains
    endif
  end subroutine initFromGridDim



  SUBROUTINE REAL_MPI_BARRIER_PDLIB(TheComm, string)

    INCLUDE "mpif.h"
    integer, intent(in) :: TheComm
    character(*), intent(in) :: string
    integer NbProc, eRank
    integer :: istatus(MPI_STATUS_SIZE)
    integer ierr, iField(1), iProc
    !      Print *, 'Start of REAL_MPI_BARRIER_PDLIB'
    CALL MPI_COMM_RANK(TheComm, eRank, ierr)
    CALL MPI_COMM_SIZE(TheComm, NbProc, ierr)
    !      Print *, '   eRank=', eRank, ' NbProc=', NbProc
    iField(1)=1
    IF (eRank .eq. 0) THEN
      DO iProc=2,NbProc
        !          Print *, '   Before MPI_RECV 1 iProc=', iProc
        CALL MPI_RECV(iField, 1, MPI_INTEGER, iProc-1, 711, TheComm, istatus, ierr)
        !          Print *, '   Before MPI_SEND 1'
        CALL MPI_SEND(iField, 1, MPI_INTEGER, iProc-1, 712, TheComm, ierr)
      END DO
    ELSE
      !        Print *, '   Before MPI_SEND 2 eRank=', eRank
      CALL MPI_SEND(iField, 1, MPI_INTEGER, 0, 711, TheComm, ierr)
      !        Print *, '   Before MPI_RECV 2 eRank=', eRank
      CALL MPI_RECV(iField, 1, MPI_INTEGER, 0, 712, TheComm, istatus, ierr)
    END IF
    !      Print *, 'Passing barrier string=', string
  END SUBROUTINE REAL_MPI_BARRIER_PDLIB
  !--------------------------------------------------------------------------
  ! Init MPI
  !--------------------------------------------------------------------------

  !> initialize MPI.
  subroutine initMPI(MPIcomm)
    use yowDatapool, only: comm, nTasks, myrank
    use yowerr
    use MPI

    integer, intent(in) :: MPIcomm
    logical :: flag
    integer :: ierr
#ifdef W3_DEBUGINIT
    Print *, '2: MPIcomm=', MPIcomm
#endif
    if(MPIcomm == MPI_COMM_NULL) then
      CALL ABORT("A null communicator is not allowed")
    endif

    comm = MPIcomm
    call MPI_Initialized(flag, ierr)
    if(ierr/=MPI_SUCCESS) call parallel_abort(error=ierr)

    if(flag .eqv. .false.) then
#ifdef W3_DEBUGINIT
      Print *, 'Before MPI_INIT yowpdlibmain'
#endif
      call mpi_init(ierr)
#ifdef W3_DEBUGINIT
      Print *, 'After MPI_INIT yowpdlibmain'
#endif
      if(ierr/=MPI_SUCCESS) call parallel_abort(error=ierr)
    endif

    ! Get number of processors
    call mpi_comm_size(comm, nTasks,ierr)
    if(ierr/=MPI_SUCCESS) call parallel_abort(error=ierr)

    ! Get rank
    call mpi_comm_rank(comm, myrank,ierr)
    if(ierr/=MPI_SUCCESS) call parallel_abort(error=ierr)
  end subroutine initMPI


  !> @param[in] MNP number of nodes global
  !> @param[in] XP node X value
  !> @param[in] XY node Y value
  !> @param[in] DEP node Z value
  !> @param[in] MNE number of element global
  !> @param[in] INE element array
  !> alter: np_global, nodes_global(), ne_global, elements(), INE_global
  subroutine assignMesh(MNP, MNE)
    use yowNodepool,    only: nodes_global, np_global
    use yowElementpool, only: ne_global
    use yowerr,       only: parallel_abort

    integer, intent(in) :: MNP, MNE
    !integer, intent(in) :: INE(3,MNE)
    integer :: stat, i

    np_global=MNP
    if(allocated(nodes_global)) deallocate(nodes_global)
    allocate(nodes_global(np_global), stat=stat);
    if(stat/=0) CALL ABORT('nodes_global() allocate failure')

    do i =1, np_global
      nodes_global(i)%id_global = i
    end do

    ne_global=MNE

    !if(allocated(INE_global)) deallocate(INE_global)
    !allocate(INE_global(3, ne_global), stat=stat);
    !if(stat/=0) CALL ABORT('INE_global allocate failure')
    !INE_global = INE
  end subroutine assignMesh


  !-------------------------------------------------------------------------
  ! pre-partition: divide the mesh into nTasks parts.
  !-------------------------------------------------------------------------

  !> pre-partition the mesh
  !> just divide the mesh into nTasks parts
  !> and create a premature iplg
  !> alter: np_perProc, np_perProcSum, np, iplg
  subroutine prePartition
    use yowDatapool, only: nTasks ,myrank
    use yowerr,    only: parallel_abort
    use yowNodepool, only: np_global, np, np_perProc, np_perProcSum, iplg

    integer :: i, stat

    ! determine equal number of nodes in each processor (except for the last one).
    ! and create a provisional node local to global mapping iplg

    ! start the arrays from 0, because the first thread id is 0
    if(allocated(np_perProc)) deallocate(np_perProc)
    allocate(np_perProc(0:nTasks-1), stat=stat)
    if(stat/=0) call parallel_abort('np_perProc allocation failure')

    if(allocated(np_perProcSum)) deallocate(np_perProcSum)
    allocate(np_perProcSum(0:nTasks), stat=stat)
    if(stat/=0) call parallel_abort('np_perProcSum allocation failure')

    np_perProcSum = 0
    np = np_global / nTasks

    do i = 0, nTasks-2
      np_perProc(i) = np
      np_perProcSum(i+1) = np_perProcSum(i) + np
    end do

    np_perProc(nTasks-1) = np_global - np_perProcSum(nTasks-1)
    np_perProcSum(nTasks) = np_perProcSum(nTasks-1) + np_perProc(nTasks-1)
    np = np_perProc(myrank)

    ! create a provisional node local to global mapping iplg
    ! this will override later with the data from parmetis
    if(allocated(iplg)) deallocate(iplg)
    allocate(iplg(np), stat=stat);
    if(stat/=0) call parallel_abort(' iplg allocate failure')
    do i = 1, np
      iplg(i) = i + np_perProcSum(myrank)
    end do
  end subroutine prePartition

  !-------------------------------------------------------------------------
  !  Create the connected Nodes array
  !-------------------------------------------------------------------------

  !> create the connected Nodes array
  !> loop over all elements and their nodes. get then the neighbor nodes
  !> finally calculate the number of sides
  !> alter: maxConnNodes, connNodes_data, ns, ns_global, node%nConnNodes
  subroutine findConnNodes(INE_global)
    use yowerr,       only: parallel_abort
    use yowNodepool,    only: np, np_global, nodes_global, nodes, maxConnNodes, t_Node, connNodes_data
    use yowElementpool, only: ne_global
    use yowSidepool,    only: ns, ns_global

    integer, intent(in) :: INE_global(3,ne_global)
    integer :: i, j, stat
    type(t_Node), pointer :: node
    integer JPREV, JNEXT

    ! Loop over all nlements
    ! look at their nodes
    ! get the node 1, insert node 2 and 3 into the connected nodes array
    ! do that for node 2 and 3 again

    ! implementation is some different
    ! loop over alle elements to get the # of connected nodes
    ! allocate space
    ! loop a second time to insert the connected nodes

    ! first loop
    do i = 1, ne_global
      do j = 1, 3
        node => nodes_global(INE_global(j,i))
        call node%insertConnNode()
        call node%insertConnNode()
      end do
    end do

    maxConnNodes = maxval(nodes_global(:)%nConnNodes)
    nodes_global(:)%nConnNodes = 0

    ! allocate space
    !> \todo we allocate more than we really need
    if(allocated(connNodes_data)) deallocate(connNodes_data)
    allocate(connNodes_data(np_global, maxConnNodes), stat = stat)
    if(stat/=0) call parallel_abort('connNodes allocation failure')

    ! second loop
    do i = 1, ne_global
      DO J=1,3
        IF (J .eq. 3) THEN
          JNEXT = 1
        ELSE
          JNEXT = J + 1
        END IF
        IF (J .eq. 1) THEN
          JPREV = 3
        ELSE
          JPREV = J - 1
        END IF
        node => nodes_global(INE_global(J,i))
        call node%insertConnNode(INE_global(JNEXT,i))
        call node%insertConnNode(INE_global(JPREV,i))
      END DO

    end do

    ns = 0
    ! calc # sides local
    do i = 1, np
      node => nodes(i)
      ns = ns + node%nConnNodes
    end do

    do i = 1, np_global
      node => nodes_global(i)
      ns_global = ns_global + node%nConnNodes
    end do
  end subroutine findConnNodes


  !------------------------------------------------------------------------
  ! Collect all data for parmetis und partition the mesh
  !------------------------------------------------------------------------

  !> Collect all data for parmetis und partition the mesh
  !> after that, we know for every node the domain ID
  !> alter: t_Node::domainID
  subroutine runParmetis(MNP)
    use yowerr,    only: parallel_abort
    use yowDatapool, only: debugParmetis,debugPartition, nTasks, myrank, itype, comm
    use yowNodepool, only: np, npa, np_global, nodes, nodes_global, t_Node, iplg, np_perProcSum, np_perProc
    use yowSidepool, only: ns
    use yowElementpool, only: ne, ne_global
    use w3gdatmd, only: xgrd, ygrd
    use MPI

    integer, intent(in) :: MNP

    ! Parmetis
    ! Node neighbor information
    integer :: wgtflag, numflag, ndims, nparts, edgecut, ncon
    integer, allocatable :: xadj(:), part(:), vwgt(:), adjwgt(:), vtxdist(:), options(:), adjncy(:), iweights(:)
    ! parmetis need single precision
    real(4), allocatable :: xyz(:), tpwgts(:), ubvec(:)
    integer :: IP_glob, itmp
    integer :: ref
    logical :: lexist = .false.

    ! Node to domain mapping.
    ! np_global long. give the domain number for die global node number
    integer, allocatable :: node2domain(:)

    ! Mics
    integer :: i, j, stat, ierr
    type(t_Node), pointer :: node, nodeNeighbor

    !    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 1")
    ! Create xadj and adjncy arrays. They holds the nodes neighbors in CSR Format
    ! Here, the adjacency structure of a graph is represented by two arrays,
    ! xadj[n+1] and adjncy[m]; n vertices and m edges. Every edge is listen twice
    allocate(adjncy(ns), stat=stat)
    if(stat/=0) call parallel_abort('adjncy allocation failure')
    allocate(xadj(np+1), stat=stat)
    if(stat/=0) call parallel_abort('xadj allocation failure')
    !    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 2")

    xadj = 0
    xadj(1) = 1
    adjncy = 0
    do i=1,np
      node => nodes(i)
      xadj(i+1) = xadj(i) + node%nConnNodes
      if(debugParmetis) write(710+myrank,*) i, xadj(i), xadj(i+1)
      if(node%nConnNodes == 0) then
        write(*,*) "Thread", myrank,"global node has no conn nodes", node%id_global
      endif
      do j=1, node%nConnNodes
        nodeNeighbor => node%connNodes(j)
        adjncy(j + xadj(i) - 1) = nodeNeighbor%id_global
        if(debugParmetis) write(710+myrank,*) i, j, j + xadj(i) - 1, adjncy(j + xadj(i) - 1), nodeNeighbor%id_global
      end do
    end do
    !    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 3")

    ! Option for Parmetis
    allocate(options(3))
    options(1)=1   ! 0: default options; 1: user options
    if(debugParmetis) then
      options(2)=15
    else
      options(2)=0  ! Level of information returned: see defs.h in ParMETIS-Lib dir
    endif
    options(3)=15  ! Random number seed
    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 4")
    ! Fortran-style numbering that starts from 1
    numflag = 1
    ! # dimensions of the space in which the graph is embedded
    ndims = 2
    ! # parts the mesh is divide. Usually nTasks
    nparts = nTasks
    ! # weight per node
    ncon = 1
    ! wgtflag: 0: none (vwgt and adjwgt are NULL); 1: edges (vwgt is NULL); 2: vertices (adjwgt is NULL); 3: both vertices & edges;

#ifdef WEIGHTS
    wgtflag = 2
    INQUIRE ( FILE='weights.ww3', EXIST = lexist )
    IF (lexist) THEN
      OPEN(100001,FILE='weights.ww3',FORM='FORMATTED',status='unknown')
      allocate(iweights(np_global)); iweights = 0
      do i = 1, np_global
        read(100001,*) iweights(i)
      enddo
      CLOSE(100001)
    ELSE
      wgtflag = 0
    ENDIF
#else
    wgtflag = 0
#endif

    ! Create weights
    allocate(vwgt(np*ncon), stat=stat)
    if(stat/=0) call parallel_abort('vwgt allocation failure')

#ifdef WEIGHTS
    if (lexist) then
      do i = 1, np
        itmp = max(1,int(real((iweights(iplg(i))+100))))
        !vwgt(i) = max(1,int(real(iweights(iplg(i)))/maxval(iweights)) * 10)
        !vwgt(i) = max(1,iweights(iplg(i)))
        !vwgt(i) = max(1,itmp)
        vwgt(i) = itmp
      enddo
      vwgt = 1
      deallocate(iweights)
    else
      vwgt = 1
    endif
#else
    vwgt = 1
#endif

    allocate(adjwgt(ns), stat=stat)
    if(stat/=0) call parallel_abort('adjwgt allocation failure')
    !> \todo
    adjwgt = 1

    ! Vertex weight fraction
    allocate(tpwgts(ncon*nTasks),stat=stat)
    if(stat/=0) call parallel_abort('partition: tpwgts allocation failure')
    tpwgts=1.0/real(nTasks)

    ! Imbalance tolerance
    allocate(ubvec(ncon),stat=stat)
    if(stat/=0) call parallel_abort('partition: ubvec allocation failure')
    ubvec=1.01
    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 5")

    ! Partition dual graph
    allocate(xyz(2*np),stat=stat)
    if(stat/=0) call parallel_abort('xyz: ubvec allocation failure')
    if(debugParmetis) write(710+myrank,*) 'np_global, ne_global, np, npa, ne'
    if(debugParmetis) write(710+myrank,*) np_global, ne_global, np, npa, ne
    do i = 1, np
      IP_glob = iplg(i)
      !AR: this is questionable ...
      xyz(2*(i-1)+1) = REAL(xgrd(1,IP_glob))
      xyz(2*(i-1)+2) = REAL(ygrd(1,IP_glob))
      if(debugParmetis) then
        write(710+myrank,*) i, np, xyz(2*(i-1)+1), xyz(2*(i-1)+2)
        call flush(710+myrank)
      endif
    end do
    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 6")

    ! Partition array returned from ParMeTiS
    allocate(part(np),stat=stat)
    if(stat/=0) call parallel_abort('part: ubvec allocation failure')

    ! ParMeTiS vertex distribution array (starts at 1)
    allocate(vtxdist(nTasks+1),stat=stat)
    if(stat/=0) call parallel_abort('partition: vtxdist allocation failure')

    call mpi_allgather(np_perProcSum(myrank)+1, 1, itype, vtxdist, 1, itype, comm, ierr)
    if(ierr/=MPI_SUCCESS) call parallel_abort('partition: mpi_allgather',ierr)
    vtxdist(nTasks+1)=np_global+1
    !    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 7")

    ! check vtxdist
    ! myrank stats from 0
    if((vtxdist(myrank+2) - vtxdist(myrank+1)) < 1) then
      write(*,*) "Thread", myrank, "has no nodes"
      write(*,*) "vtxdist", vtxdist
      CALL ABORT("Poor initial vertex distribution detected")
    endif



    !  My notes from manual:
    !  p: # of processors;
    !  n: total # of vertices (local) in graph sense;
    !  m: total # of neighboring vertices ("edges"); double counted between neighboring vertice u and v.
    !  ncon: # of weights for each vertex;
    !  int(in) vtxdist(p+1): Processor j stores vertices vtxdist(j):vtxdist(j+1)-1
    !  int (in) xadj(n+1), adjncy(m):
    !           locally, vertex j's neighboring vertices are adjncy(xadj(j):xadj(j+1)-1). adjncy points to global index;
    !  int(in) vwgt(ncon*n), adjwgt(m): weights at vertices and "edges". Format of adjwgt follows adjncy;
    !  int(in) wgtflag: 0: none (vwgt and adjwgt are NULL);
    !          1: edges (vwgt is NULL); 2: vertices (adjwgt is NULL); 3: both vertices & edges;
    !  int(in) numflag: 0: C-style numbering from 0; 1: FORTRAN style from 1;
    !  int(in) ndims: 2 or 3 (D);
    !  float(in) xyz(ndims*n): coordinate for vertex j is xyz(j*ndims:(j+1)*ndims-1);
    !  int(in)   nparts: # of desired sub-domains (usually nTasks);
    !  float(in) tpwgts(ncon*nparts): =1/nparts if sub-domains are to be of same size for each vertex weight;
    !  float(in) ubvec(ncon): imbalance tolerance for each weight;
    !  int(in)   options: additonal parameters for the routine (see above);
    !  int(out)  edgecut: # of edges that are cut by the partitioning;
    !  int(out)  part(): array size = # of local vertices. It stores indices of local vertices.

    ! write(1112+myrank,*) "Thread",myrank,"sum;vtxdist", sum(vtxdist), vtxdist
    ! write(1112+myrank,*) "Thread",myrank,"sum;xadj", sum(xadj), xadj
    ! write(1112+myrank,*) "Thread",myrank,"sum;adjncy", sum(adjncy), adjncy

    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 8")

    if(debugParmetis) then
      write(710+myrank,*) vtxdist, xadj, adjncy, &
           vwgt, & !vwgt - ignore weights
           adjwgt, & ! adjwgt - ignore weights
           wgtflag, &
           numflag,ndims,ncon,nparts,tpwgts,ubvec,options, &
           edgecut,part,comm
      call flush(710+myrank)
    endif

    !if(debugParmetis) write(710+myrank,*) "Run ParMETIS now..."
#ifdef W3_SCOTCH
    call SCOTCH_ParMETIS_V3_PartGeomKway(vtxdist, xadj, adjncy, &
         vwgt, & !vwgt - ignore weights
         adjwgt, & ! adjwgt - ignore weights
         wgtflag, &
         numflag,ndims,xyz,ncon,nparts,tpwgts,ubvec,options, &
         edgecut,part, comm,ref)
#endif

#ifdef W3_METIS
    call ParMETIS_V3_PartGeomKway(vtxdist, xadj, adjncy, &
         vwgt, & !vwgt - ignore weights
         adjwgt, & ! adjwgt - ignore weights
         wgtflag, &
         numflag,ndims,xyz,ncon,nparts,tpwgts,ubvec,options, &
         edgecut,part, comm)
#endif


    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 9")

    if(nTasks == 1) then
      !    write(*,*) myrank, "minval part", minval(part)
      if(minval(part) == 0) then
        part(:) = part(:) + 1
      endif
    endif

    ! write(*,*) myrank, "edge cuted", edgecut

    ! Collect the parmetis data from all threads
    ! and create a global node to domain number mapping
    allocate(node2domain(np_global),stat=stat)
    if(stat/=0) call parallel_abort(' node2domain allocation failure')
    !
    call mpi_allgatherv(part, np, itype, node2domain, np_perProc, np_perProcSum, itype, comm, ierr)
    if(ierr/=MPI_SUCCESS) call parallel_abort('mpi_allgatherv ',ierr)
    !
    do i = 1, np_global
      node => nodes_global(i)
      node%domainID = node2domain(node%id_global)
    end do
    !    CALL REAL_MPI_BARRIER_PDLIB(comm, "runParmetis, step 10")


    ! write out partition info for katerfempresenter
    if(debugPartition) write(600,*) node2domain
    !    Print *, 'runparmetis step 1'

    if(allocated(xadj))        deallocate(xadj)
    !    Print *, 'runparmetis step 2'
    if(allocated(adjncy))      deallocate(adjncy)
    !    Print *, 'runparmetis step 3'
    if(allocated(part))        deallocate(part)
    !    Print *, 'runparmetis step 4'
    if(allocated(vwgt))        deallocate(vwgt)
    !    Print *, 'runparmetis step 5'
    if(allocated(adjwgt))      deallocate(adjwgt)
    !    Print *, 'runparmetis step 6'
    if(allocated(xyz))         deallocate(xyz)
    !    Print *, 'runparmetis step 7'
    if(allocated(tpwgts))      deallocate(tpwgts)
    !    Print *, 'runparmetis step 8'
    if(allocated(ubvec))       deallocate(ubvec)
    !    Print *, 'runparmetis step 9'
    if(allocated(vtxdist))     deallocate(vtxdist)
    !    Print *, 'runparmetis step 10'
    if(allocated(node2domain)) deallocate(node2domain)
    !    Print *, 'runparmetis step 11'
  end subroutine runParmetis

  !------------------------------------------------------------------------
  ! with the new data from parmetis, recalculate some variables
  !------------------------------------------------------------------------

  ! Create for every boundary node a dummy node and for every dummy node two local elements
  ! extend x,y,z, INE
  ! rebuild  NCONE, CONE, NCONN, CONN
  ! alter nd, nde
  !subroutine dummyNodes
  ! use yowExchangeModule
  ! use yowNodepool, only: npa, nb, nd, PDLIB_NCONN, PDLIB_CONN, boundaryNodes, nodes, x, y, z, outwardNormal, XY, dummy2boundary, BNDprevnext
  ! use yowElementpool, only: ne, nde, INE, NCONE, CONE
  ! implicit none
  ! integer :: i, IP, IPprev, IPnext, newIP, newIE
  ! integer, allocatable :: INEnew(:,:)
  ! real(rkind) :: vec(2), newPoint(2), newLength
  ! real(rkind), allocatable :: xNew(:), yNew(:), zNew(:)
  !
  !    nd = nb
  !
  !    allocate(dummy2boundary(nd))
  !
  !    nde = 2*nd
  !    allocate(INEnew(3,ne+nde))
  !    INEnew(:,1:ne) = INE(:,1:ne)
  !
  !    allocate(xNew(npa+nd), yNew(npa+nd), zNew(npa+nd))
  !    xNew(1:npa) = x(1:npa)
  !    yNew(1:npa) = y(1:npa)
  !    zNew(1:npa) = z(1:npa)
  !
  !    nd = 0
  !    nde = 0
  !   do i=1, nb
  !     nd = nd + 1
  !     IP = boundaryNodes(i)
  !     dummy2boundary(nd) = IP
  !
  !     call BNDprevnext(IP, IPprev, IPnext)
  !     newLength = max(distanceTo(XY(IP), XY(IPprev)), distanceTo(XY(IP), XY(IPnext)))
  !     newPoint  = XY(IP) + outwardNormal(:,i) * newLength
  !    newIP = npa+i
  !    xNew(newIP) = newPoint(1)
  !    yNew(newIP) = newPoint(2)
  !    zNew(newIP) = z(IP)
  !    NCONN(IP) = NCONN(IP) + 1
  !    CONN(NCONN(IP),IP) = newIP
  !
  !    nde = nde +1
  !     newIE = ne+nde
  !     INEnew(1,newIE) = IP
  !     INEnew(2,newIE) = IPprev
  !     INEnew(3,newIE) = newIP
  !     NCONE(IP) = NCONE(IP) +1
  !     CONE(NCONE(IP),IP) = newIE
  !
  !     nde = nde +1
  !     newIE = ne+nde
  !     INEnew(1,newIE) = IP
  !     INEnew(2,newIE) = newIP
  !     INEnew(3,newIE) = IPnext
  !     NCONE(IP) = NCONE(IP) +1
  !     CONE(NCONE(IP),IP) = newIE
  !   end do
  !
  !   call move_alloc(INEnew, INE)
  !   call move_alloc(xNew, x)
  !   call move_alloc(yNew, y)
  !   call move_alloc(zNew, z)
  !
  !   ! Die Anzahl der angeschlossenen Knoten fuer Geisterknoten wird fuer findBoundaryNodes and Dummynodes gebraucht.
  !   call exchange(NCONN)
  !   call exchange(NCONE)
  !end subroutine

  !> recalculate some variables
  !> parmetis change the number of sides per domain. So recalculate some variables
  !> alter: np, ns, np_perProc, np_perProcSum, iplg, ipgl, nodes_global%id
  !> @note connNodes_data(:) has not changed after the call to parmetis.
  subroutine postPartition
    use yowerr,    only: parallel_abort
    use yowDatapool, only: myrank, nTasks
    use yowNodepool, only: np_global, np, nodes_global, nodes, np_perProc, np_perProcSum, iplg, ipgl, t_Node
    use yowSidepool, only: ns

    integer :: i, j, stat
    type(t_Node), pointer :: node

    ! determine how many nodes now belong to which thread
    ! and set the nodes local id
    np_perProc = 0
    do i = 1, np_global
      ! fortran counts from 1. np_perProc from 0
      np_perProc(nodes_global(i)%domainID-1) = np_perProc(nodes_global(i)%domainID-1)+1
      ! set the new local id
      nodes_global(i)%id = np_perProc(nodes_global(i)%domainID-1)
    end do

    np_perProcSum(0) = 0
    do i = 1, nTasks-1
      np_perProcSum(i) = np_perProcSum(i-1) + np_perProc(i-1)
    end do

    np = np_perProc(myrank)

    ! create the new node local to global mapping iplg. This is not the final one.
    ! create a iplg with ghost nodes in findGhostNodes
    if(allocated(iplg)) deallocate(iplg)
    allocate(iplg(np), stat=stat)
    if(stat/=0) call parallel_abort('iplg second allocation failure')
    iplg = 0

    if(allocated(ipgl)) deallocate(ipgl)
    allocate(ipgl(np_global), stat=stat)
    if(stat/=0) call parallel_abort('ipgl allocation failure')
    ipgl = 0

    j = 1
    do i = 1, np_global
      node => nodes_global(i)
      if(node%domainID == myrank+1) then
        iplg(j) = i
        ipgl(i) = j
        j = j + 1
      endif
    end do

    ! calc # sides local again, because the nodes now belongs to another domain
    ns = 0
    do i = 1, np
      node => nodes(i)
      ns = ns + node%nConnNodes
    end do
  end subroutine postPartition


  !----------------------------------------------------------------------
  ! find the ghost nodes of the local domain
  !----------------------------------------------------------------------

  !> find the ghost nodes of the local domain
  !> alter: ng, ghosts(), ghostlg, ghostgl, npa, iplg
  subroutine findGhostNodes
    use yowerr,    only: parallel_abort
    use yowDatapool, only: myrank
    use yowNodepool, only: t_Node, np, nodes, ghosts, nodes_global, ng, ghostlg, ghostgl, npa, np_global, iplg

    integer :: i, j, k, stat
    type(t_Node), pointer :: node, nodeNeighbor, nodeGhost
    !> temporary hold the ghost numbers
    integer, save, allocatable :: ghostTemp(:)
#ifdef W3_DEBUGINIT
    Print *, 'Passing in findGhostNodes'
#endif

    ! iterate over all local nodes and look at their neighbors
    ! has the neighbor another domain id, than it is a ghost

    ! implementation is some different
    ! loop over all nodes to get the # of ghost nodes
    ! allocate space
    ! loop a second time to insert the ghost nodes
    !> \todo make this faster. dont check all neighbors from all local nodes. mark if an node has already been checked.

    ! first loop. find out how many ghost nodes we have (with double entries)
    ng = 0
    do i = 1, np
      node => nodes(i)
      do j = 1, node%nConnNodes
        nodeNeighbor => node%connNodes(j)
        if(nodeNeighbor%domainID /= node%domainID) then
          ! yes, we found a ghost
          ng = ng + 1
        end if
      end do
    end do

    allocate(ghostTemp(ng), stat=stat)
    if(stat/=0) call parallel_abort('ghostTemp allocation failure')

#ifdef W3_DEBUGINIT
    Print *, 'np_global=', np_global
#endif
    IF (allocated(ghostgl)) THEN
      Print *, 'ghostgl is already allocated'
    END IF
    allocate(ghostgl(np_global), stat=stat)
    if(stat/=0) call parallel_abort('ghostgl allocation failure')
    ghostgl = 0

    ! second loop. fill ghostlg. ignore double entries
    ng = 0
    ! iterate over all local nodes
    do i = 1, np
      node => nodes(i)

      ! check their neighbors
      secondloop: do j = 1, node%nConnNodes
        nodeNeighbor => node%connNodes(j)

        if(nodeNeighbor%domainID /= node%domainID) then
          ! yes, we found a ghost
          ! check if this ghost is allready in the ghost list
          do k = 1, ng
            nodeGhost => nodes_global(ghostTemp(k))
            if(nodeNeighbor%id_global == nodeGhost%id_global) then
              ! yes, we allready know this ghost.
              ! check the next neighbor
              cycle secondloop
            end if
          end do

          ! no we don't know this ghost. insert it
          ng = ng + 1
          ghostTemp(ng) = nodeNeighbor%id_global
          ghostgl(nodeNeighbor%id_global) = ng
        end if
      end do secondloop
    end do

    ! reallocate the gosttemp array becouse it is longer then the new ng
    if(allocated(ghostlg)) deallocate(ghostlg)
    allocate(ghostlg(ng), stat=stat)
    if(stat/=0) call parallel_abort('ghostlg allocation failure')
    ghostlg = ghostTemp(1:ng)
    deallocate(ghostTemp)

    npa = np + ng

    ! check if ghostlg contains only uniqe values
    do i=1, ng
      do j=i+1, ng
        if(ghostlg(i) == ghostlg(j)) then
          write(*,*) "double global ghost id in ghostlg(i,j)", i, j
          stop "double global ghost id in ghostlg(i,j)"
        endif
      end do
    end do


    ! create the new node local to global mapping iplg with ghost. final one.
    if(allocated(iplg)) deallocate(iplg)
    allocate(iplg(npa), stat=stat)
    if(stat/=0) call parallel_abort('iplg second allocation failure')
    iplg = 0

    j = 1
    do i = 1, np_global
      node => nodes_global(i)
      if(node%domainID == myrank+1) then
        iplg(j) = i
        j = j + 1
      endif
    end do

    iplg(np+1: npa) = ghostlg(1:ng)
  end subroutine findGhostNodes
  !-------------------------------------------------------------------------------
  ! find the number of connected domains and their ghosts
  !-------------------------------------------------------------------------------

  !> find the number of connected domains and their ghosts
  !> 1) Iterate over all ghost nodes and look at their thread id to find # neighbor domains
  !> 2) assign the ghost nodes to their domains
  !> alter: neighborDomains(), nConnDomains
  subroutine findConnDomains
    use yowerr,          only: parallel_abort
    use yowNodepool,       only: ghosts, ng, t_Node
    use yowDatapool,       only: nTasks, myrank
    use yowExchangeModule, only: neighborDomains, initNbrDomains

    integer :: i, stat, itemp
    type(t_Node), pointer :: ghost

    ! # of ghost per neighbor domain
    integer, allocatable :: numberGhostPerNeighborDomainTemp(:)
    ! look up table. domainID to neighbor (ID)
    integer, allocatable :: domainID2NeighborTemp(:)

    ! Part 1) find # neighbor domains

    ! allocate this array with a fixed size of nTasks. even if we not have
    ! so many neighbor domains, nTasks will never be very large
    allocate(numberGhostPerNeighborDomainTemp(nTasks), stat=stat)
    if(stat/=0) call parallel_abort('numberGhostPerNeighborDomainTemp allocation failure')
    numberGhostPerNeighborDomainTemp = 0

    allocate(domainID2NeighborTemp(nTasks), stat=stat)
    if(stat/=0) call parallel_abort('domainID2NeighborTemp allocation failure')
    domainID2NeighborTemp = 0


    ! iterate over all ghost nodes an get their thread id
    itemp = 0
    do i = 1, ng
      ghost => ghosts(i)

      ! sum how many ghost belongs to the ghost domainID
      numberGhostPerNeighborDomainTemp(ghost%domainID) = numberGhostPerNeighborDomainTemp(ghost%domainID) + 1

      ! check if this ghost domainID is allready in the domains list
      if(domainID2NeighborTemp(ghost%domainID) /= 0) then
        ! yes we have allready insert this domain id
        cycle
      end if

      ! no we dont know this domain id. insert it
      itemp = itemp + 1
      domainID2NeighborTemp(ghost%domainID) = itemp
    end do

    ! Part 2)  assign the ghost nodes to their domains
    call initNbrDomains(itemp)

    do i = 1, nTasks
      if(numberGhostPerNeighborDomainTemp(i) /= 0) then
        neighborDomains(domainID2NeighborTemp(i) )%domainID = i

        allocate(neighborDomains(domainID2NeighborTemp(i))%nodesToReceive(numberGhostPerNeighborDomainTemp(i)), stat=stat)
        if(stat/=0) call parallel_abort('neighborDomains%ghosts allocation failure')
        neighborDomains(domainID2NeighborTemp(i))%nodesToReceive = 0
      end if
    end do

    do i = 1, ng
      ghost => ghosts(i)
      itemp = domainID2NeighborTemp(ghost%domainID)

      neighborDomains(itemp)%numNodesToReceive = neighborDomains(itemp)%numNodesToReceive + 1
      neighborDomains(itemp)%nodesToReceive(neighborDomains(itemp)%numNodesToReceive) = ghost%id_global
    end do

    if(allocated(numberGhostPerNeighborDomainTemp)) deallocate(numberGhostPerNeighborDomainTemp)
    if(allocated(domainID2NeighborTemp)) deallocate(domainID2NeighborTemp)
  end subroutine findConnDomains


  !-------------------------------------------------------------------------------
  ! exchange Ghost Ids so every thread knows which nodes he has to send to the
  ! other parition
  !-------------------------------------------------------------------------------

  !> exchange Ghost Ids so every thread knows which nodes he has to send to the
  !> other parition. every parition has a list of ghost nodes from the other
  !> partition. this data is stored in the neighborDomains variable
  !> \todo make a better  explanation
  !> 1) send to the neighbor domain which ghost nodes we want from him
  !> 2) receive from the neighbor domain which ghost we must send to him
  !> alter: neighborDomains()%{numNodesToSend, nodesToSend},
  subroutine exchangeGhostIds
    use yowerr
    use yowNodepool,       only: np, t_node, nodes
    use yowDatapool,       only: nTasks, myrank, comm
    use yowExchangeModule, only: neighborDomains, nConnDomains, createMPITypes
    use MPI

    integer :: i, j, k
    integer :: ierr
    ! uniq tag that identify the sender and which information he sends
    integer :: tag
    ! we use non-blocking send and recv subroutines
    ! store the send status
    integer :: sendRequest(nConnDomains)
    ! store the revc status
    integer :: recvRequest(nConnDomains)
    ! status to verify if one communication fails or not
    integer :: status(MPI_STATUS_SIZE, nConnDomains);


    type(t_node), pointer :: node

    ! send to all domain neighbors how many ghosts nodes we want from him and which ones
    do i=1, nConnDomains
      ! create a uniq tag for this domain
      tag = neighborDomains(i)%domainID*10 + 1
      ! send to the neighbor how many ghost nodes we want from him
      call MPI_Isend(neighborDomains(i)%numNodesToReceive, &
           1, &
           MPI_INT, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           sendRequest(i), &
           ierr);
      if(ierr/=MPI_SUCCESS) then
        write(*,*) "mpi send failure"
      endif

      tag = neighborDomains(i)%domainID*10 + 2
      ! send to the neighbor which ghost nodes we want from him
      call MPI_Isend(neighborDomains(i)%nodesToReceive, &
           neighborDomains(i)%numNodesToReceive, &
           MPI_INT, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           !> todo use a second sendRequest array here
           sendRequest(i), &
           ierr);
      if(ierr/=MPI_SUCCESS) then
        write(*,*) "mpi send failure"
      endif

      ! receive from neighbor how many ghost nodes we have to send him
      tag = (myrank+1)*10 + 1
      call MPI_Irecv(neighborDomains(i)%numNodesToSend, &
           1, &
           MPI_INT, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           recvRequest(i), &
           ierr)
      if(ierr/=MPI_SUCCESS) then
        write(*,*) "mpi recv failure"
      endif
    end do

    ! wait for communication end
    call MPI_Waitall(nConnDomains, recvRequest, status, ierr)

    ! test for all neighbor domains
    do i=1, nConnDomains
      ! test if the neighbor wants more ghost nodes than we have
      if(neighborDomains(i)%numNodesToSend > np) then
        write(*,'(i5, a, i5, a, i5,a, i5, a, /)', advance='no') myrank, " ERROR neighbordomain ", neighborDomains(i)%domainID, &
             " wants ", neighborDomains(i)%numNodesToSend, &
             " nodes, but we have only ", np, " nodes"
        CALL ABORT("")
      end if
    end do

    ! receive from all neighbor domains which nodes we must send him
    do i=1, nConnDomains
      allocate(neighborDomains(i)%nodesToSend(neighborDomains(i)%numNodesToSend))
      neighborDomains(i)%nodesToSend = 0

      ! receive from neighbor which nodes we must send
      tag = (myrank+1)*10 + 2
      call MPI_Irecv(neighborDomains(i)%nodesToSend, &
           neighborDomains(i)%numNodesToSend, &
           MPI_INT, &
           neighborDomains(i)%domainID-1, &
           tag, &
           comm, &
           recvRequest(i), &
           ierr)
      if(ierr/=MPI_SUCCESS) then
        CALL PARALLEL_ABORT("mpi recv failure", ierr)
      endif
    end do

    ! wait for communication end
    call MPI_Waitall(nConnDomains, recvRequest, status, ierr)

    ! test for all neighbor domains
    do i=1, nConnDomains
      ! test if the neighbor wants nodes that we don't own
      outerloop: do j=1, neighborDomains(i)%numNodesToSend
        ! compare with all local nodes
        do k=1, np
          node => nodes(k)
          if(node%id_global == neighborDomains(i)%nodesToSend(j)) then
            cycle outerloop
          end if
        end do
        write(*,*) myrank, "Neighbordomain", neighborDomains(i)%domainID, &
             " want Node", neighborDomains(i)%nodesToSend(j), &
             " but we don't own this node"
        stop
      end do outerloop
    end do

    call createMPITypes()
  end subroutine exchangeGhostIds

  !> this collects all data which depends on ghost information
  !> alter: ne, INE, x, y, z, ielg
  subroutine postPartition2(INE_global)
    use yowElementpool, only: ne, ne_global, INE, belongto, ielg
    use yowerr,         only: parallel_abort
    use yowDatapool,    only: myrank
    use yowNodepool,    only: np_global, np, nodes_global, iplg, t_Node, ghostlg, ng, npa
    use yowNodepool,    only: x, y, z
    use w3gdatmd,       only: xgrd, ygrd, zb

    integer, intent(in) :: INE_global(3,ne_global)

    integer :: i, j, k, stat, IP_glob
    type(t_Node), pointer :: node
    logical :: assigned

    ! to find the local elements, iterate over all global elements and check their node domain IDs

    ! step 1: calc the number of local elements
    ne = 0
    do i=1, ne_global
      if (belongto(INE_global(:,i))) then
        ne = ne +1
      endif
    end do

    ! step 2: fill the local element index array
    if(allocated(INE)) deallocate(INE)
    allocate(INE(3, ne), stat=stat)
    if(stat/=0) call parallel_abort('INE allocation failure')
    INE = 0

    ne = 0
    do i=1, ne_global
      ! yes, this element belongs to this domain
      if (belongto(INE_global(:,i))) then
        ne = ne + 1
        do j=1, 3
          assigned = .false.
          node => nodes_global(INE_global(j,i))
          if(node%domainID == myrank+1) then
            INE(j, ne) = node%id
            assigned = .true.
          else
            ! the element have a ghost node
            !> \todo create some localnode to localghost mapping
            !> What number is this ghost
            do k=1, ng
              if(node%id_global == ghostlg(k)) then
                ! conversion: the ghost nodes are stored behind the local nodes.
                if(INE(j,ne) /= 0) then
                  write(*,*) "will write to INE(j, ne) but there is allready a value", j, ne, INE(j, ne)
                endif
                INE(j, ne) = np + k
                node%id = np+k
                assigned = .true.
                !                 write(*,*) myrank, "node to ele", node%id_global-1, i-1, np+k
                exit
              endif
            end do
          endif
          if(assigned .eqv. .false.) then
            write(*,*) "Can't assign global node to INE", node%id_global
          endif
        end do
      endif
    end do

    ! check if INE contains 0.
    do i=1, ne
      if(MINVAL(ABS(INE(:,i))) == 0) then
        write(*,*) "0 in INE ne=", ne
        stop "0 in INE"
      endif
    end do

    if(MAXVAL(INE) /= npa) then
      write(*,*) "MAXVAL(INE) /= npa ERROR?"
    endif

    ! create element local to global mapping ielg
    if(allocated(ielg)) deallocate(ielg)
    allocate(ielg(ne), stat=stat)
    if(stat/=0) call parallel_abort('ielg allocation failure')
    ielg = 0

    j = 0
    do i=1, ne_global
      if (belongto(INE_global(:,i))) then
        j = j +1
        ielg(j) = i
      end if
    end do

    ! fill the local x,y arrays
    if(allocated(x)) deallocate(x)
    allocate(x(npa), stat=stat)
    if(stat/=0) call parallel_abort('x allocation failure')

    if(allocated(y)) deallocate(y)
    allocate(y(npa), stat=stat)
    if(stat/=0) call parallel_abort('y allocation failure')

    if(allocated(z)) deallocate(z)
    allocate(z(npa), stat=stat)
    if(stat/=0) call parallel_abort('z allocation failure')

    do i=1, np
      IP_glob = iplg(i)
      x(i) = xgrd(1,IP_glob)
      y(i) = ygrd(1,IP_glob)
      z(i) = zb(IP_glob)
    end do

    do i=1, ng
      IP_glob = ghostlg(i)
      x(np+i) = xgrd(1,IP_glob)
      y(np+i) = ygrd(1,IP_glob)
      z(np+i) = zb(IP_glob)
    end do

  end subroutine postPartition2
  !**********************************************************************
  !*                                                                    *
  !**********************************************************************
  subroutine ComputeTRIA_IEN_SI_CCON
    use yowElementpool, only: ne, ne_global, INE, ielg
    use yowExchangeModule, only : PDLIB_exchange1Dreal
    use yowerr,       only: parallel_abort
    use yowDatapool,    only: myrank
    use yowNodepool,    only: np_global, np, iplg, t_Node, ghostlg, ng, npa
    use yowNodepool,    only: x, y, z, PDLIB_SI, PDLIB_IEN, PDLIB_TRIA, PDLIB_CCON, PDLIB_TRIA03

    integer I1, I2, I3, stat, IE, NI(3)
    real  :: DXP1, DXP2, DXP3, DYP1, DYP2, DYP3, DBLTMP, TRIA03
    logical :: CROSSES_DATELINE

    allocate(PDLIB_SI(npa), PDLIB_CCON(npa), PDLIB_IEN(6,ne), PDLIB_TRIA(ne), PDLIB_TRIA03(ne), stat=stat)
    if(stat/=0) call parallel_abort('SI allocation failure')

    PDLIB_SI(:)   = 0.0d0 ! Median Dual Patch Area of each Node
    PDLIB_CCON(:) = 0     ! Number of connected Elements
    DO IE = 1 , ne
      I1 = INE(1,IE)
      I2 = INE(2,IE)
      I3 = INE(3,IE)
      NI = INE(:,IE)

      DXP1=x(I2) - x(I1)
      DYP1=y(I2) - y(I1)
      DXP2=x(I3) - x(I2)
      DYP2=y(I3) - y(I2)
      DXP3=x(I1) - x(I3)
      DYP3=y(I1) - y(I3)
      CALL ELEMENT_CROSSES_DATELINE(DXP1, DXP2, DXP3, CROSSES_DATELINE)
      IF (CROSSES_DATELINE) THEN
        CALL CORRECT_DX_GT180(DXP1)
        CALL CORRECT_DX_GT180(DXP2)
        CALL CORRECT_DX_GT180(DXP3)
      ENDIF

      PDLIB_IEN(1,IE) = - DYP2
      PDLIB_IEN(2,IE) =   DXP2
      PDLIB_IEN(3,IE) = - DYP3
      PDLIB_IEN(4,IE) =   DXP3
      PDLIB_IEN(5,IE) = - DYP1
      PDLIB_IEN(6,IE) =   DXP1
      DBLTMP = (DXP3*DYP1 - DYP3*DXP1)*0.5
      PDLIB_TRIA(IE) = DBLTMP
      IF (PDLIB_TRIA(IE) .lt. TINY(1.)) THEN
        WRITE(*,*) PDLIB_IEN(:,IE)
        WRITE(*,*)
        WRITE(*,*) 'AREA SMALLER ZERO IN PDLIB', IE, NE, PDLIB_TRIA(IE)
        STOP
      ENDIF

      PDLIB_CCON(I1) = PDLIB_CCON(I1) + 1
      PDLIB_CCON(I2) = PDLIB_CCON(I2) + 1
      PDLIB_CCON(I3) = PDLIB_CCON(I3) + 1
      TRIA03         = PDLIB_TRIA(IE)/3.d0
      PDLIB_SI(I1) = PDLIB_SI(I1) + TRIA03
      PDLIB_SI(I2) = PDLIB_SI(I2) + TRIA03
      PDLIB_SI(I3) = PDLIB_SI(I3) + TRIA03
      PDLIB_TRIA03(IE) = TRIA03
    ENDDO
    CALL PDLIB_exchange1Dreal(PDLIB_SI)
  end subroutine ComputeTRIA_IEN_SI_CCON
  !**********************************************************************
  !*                                                                    *
  !**********************************************************************
  subroutine ELEMENT_CROSSES_DATELINE(RX1, RX2, RX3, CROSSES_DATELINE)
    !   Purpose: understanding if an element crosses the dateline.
    !   An element crossing the dateline has, e.g. a node with lon < 180
    !   and another 2 with lon > -180

    REAL(rkind),  INTENT(IN)  :: RX1, RX2, RX3
    LOGICAL, INTENT(OUT) :: CROSSES_DATELINE
    INTEGER :: R1GT180, R2GT180, R3GT180
    R1GT180 = MERGE(1, 0, ABS(RX1).GT.180)
    R2GT180 = MERGE(1, 0, ABS(RX2).GT.180)
    R3GT180 = MERGE(1, 0, ABS(RX3).GT.180)
    ! if R1GT180+R2GT180+R3GT180 .eq. 0 the element does not cross the dateline
    ! if R1GT180+R2GT180+R3GT180 .eq. 1 the element contains the pole
    ! if R1GT180+R2GT180+R3GT180 .eq. 2 the element crosses the dateline
    CROSSES_DATELINE = R1GT180+R2GT180+R3GT180 .EQ. 2
  end subroutine ELEMENT_CROSSES_DATELINE
  !**********************************************************************
  !*                                                                    *
  !**********************************************************************
  subroutine CORRECT_DX_GT180(DXP)
    !   Purpose: the absolute zonal distance between 2 points is always <= 180
    !            This subroutine corrects the zonal distance to satifsy
    !            this requirement

    REAL(rkind), INTENT(INOUT) :: DXP
    IF (DXP .le. -180) THEN
      DXP=DXP + 360
    END IF
    IF (DXP .ge. 180) THEN
      DXP=DXP - 360
    END IF
  end subroutine CORRECT_DX_GT180
  !**********************************************************************
  !*                                                                    *
  !**********************************************************************
  subroutine ComputeIA_JA_POSI_NNZ
    use yowElementpool, only: ne, ne_global, INE, ielg
    use yowerr,       only: parallel_abort
    use yowDatapool,    only: myrank
    use yowNodepool,    only: np_global, np, nodes_global, iplg, t_Node, ghostlg, ng, npa
    use yowNodepool,    only: PDLIB_CCON, PDLIB_IA, PDLIB_JA, PDLIB_JA_IE, PDLIB_IA_P, PDLIB_JA_P
    use yowNodepool,    only: PDLIB_NNZ, PDLIB_POSI, PDLIB_IE_CELL, PDLIB_POS_CELL, PDLIB_IE_CELL2
    use yowNodepool,    only: PDLIB_POS_CELL2, PDLIB_I_DIAG

    integer CHILF(npa)
    integer istat
    integer MAXMNECON
    integer IE, J, I, IP, K, IP_J, IP_K, IP_I
    integer I1, I2, I3, POS, POS_J, POS_K
    integer COUNT_MAX
    integer, allocatable :: CELLVERTEX(:,:,:), PTABLE(:,:)
    integer :: ITMP(npa)
    MAXMNECON  = MAXVAL(PDLIB_CCON)
    ALLOCATE(CELLVERTEX(npa,MAXMNECON,2), stat=istat)
    IF (istat/=0) CALL PARALLEL_ABORT('ComputeIA_JA_POSI_NNZ, allocate error 4')
    !
    CELLVERTEX(:,:,:) = 0
    CHILF             = 0
    DO IE = 1, ne
      DO J=1,3
        I = INE(J,IE)
        CHILF(I) = CHILF(I)+1
        CELLVERTEX(I,CHILF(I),1) = IE
        CELLVERTEX(I,CHILF(I),2) = J
      END DO
    ENDDO
    !
    !        Emulates loop structure and counts max. entries in the different pointers that have to be designed
    !
    J = 0
    DO IP = 1, npa
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
      END DO
    END DO
    COUNT_MAX = J
    ALLOCATE (PDLIB_IE_CELL(COUNT_MAX), PDLIB_POS_CELL(COUNT_MAX), stat=istat)
    IF (istat/=0) CALL PARALLEL_ABORT('wwm_fluctsplit, allocate error 5a')
    ALLOCATE (PDLIB_IE_CELL2(MAXMNECON,NPA), PDLIB_POS_CELL2(MAXMNECON, NPA), stat=istat)
    IF (istat/=0) CALL PARALLEL_ABORT('wwm_fluctsplit, allocate error 5b')
    ! Just a remapping from CELLVERTEX ... Element number in the
    ! order of the occurence in the loop during runtime
    PDLIB_IE_CELL  = 0
    ! Just a remapping from CELLVERTEX ... Position of the node
    ! in the Element index -"-
    PDLIB_POS_CELL = 0
    J = 0
    DO IP = 1, npa
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        PDLIB_IE_CELL(J)      = CELLVERTEX(IP,I,1)
        PDLIB_POS_CELL(J)     = CELLVERTEX(IP,I,2)
        PDLIB_IE_CELL2(I,IP)  = CELLVERTEX(IP,I,1)
        PDLIB_POS_CELL2(I,IP) = CELLVERTEX(IP,I,2)
      END DO
    END DO
    deallocate(CELLVERTEX)

    ALLOCATE(PTABLE(COUNT_MAX,7), stat=istat)
    IF (istat/=0) CALL PARALLEL_ABORT('wwm_fluctsplit, allocate error 6')
    ALLOCATE(PDLIB_JA_IE(3,3,ne), stat=istat)
    IF (istat/=0) CALL PARALLEL_ABORT('wwm_fluctsplit, allocate error 6.1')

    J = 0
    PTABLE(:,:) = 0.
    DO IP = 1, npa
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        IE    = PDLIB_IE_CELL(J)
        POS   = PDLIB_POS_CELL(J)
        I1 = INE(1,IE)
        I2 = INE(2,IE)
        I3 = INE(3,IE)
        IF (POS == 1) THEN
          POS_J = 2
          POS_K = 3
        ELSE IF (POS == 2) THEN
          POS_J = 3
          POS_K = 1
        ELSE
          POS_J = 1
          POS_K = 2
        END IF
        IP_I = IP
        IP_J = INE(POS_J,IE)
        IP_K = INE(POS_K,IE)
        PTABLE(J,1) = IP_I ! Node numbers of the connected elements
        PTABLE(J,2) = IP_J
        PTABLE(J,3) = IP_K
        PTABLE(J,4) = POS  ! Position of the nodes in the element index
        PTABLE(J,5) = POS_J
        PTABLE(J,6) = POS_K
        PTABLE(J,7) = IE   ! Element numbers same as PDLIB_IE_CELL
      END DO
    END DO
    !
    ! Count number of nonzero entries in the matrix ...
    ! Basically, each connected element may have two off-diagonal
    ! contribution and one diagonal related to the connected vertex itself ...
    !
    J = 0
    PDLIB_NNZ = 0
    ITMP = 0
    DO IP = 1, npa
      ITMP = 0
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        IP_J  = PTABLE(J,2)
        IP_K  = PTABLE(J,3)
        ITMP(IP)   = 1
        ITMP(IP_J) = 1
        ITMP(IP_K) = 1
      END DO
      PDLIB_NNZ = PDLIB_NNZ + SUM(ITMP)
    END DO
    !
    ! Allocate sparse matrix pointers using the Compressed Sparse Row Format CSR ... this is now done only of npa nodes
    ! The next step is to do it for the whole Matrix npa * MSC * MDC
    ! see ...:x
    !
    ALLOCATE (PDLIB_JA(PDLIB_NNZ), PDLIB_IA(npa+1), PDLIB_JA_P(PDLIB_NNZ), stat=istat)
    IF (istat/=0) CALL PARALLEL_ABORT('wwm_fluctsplit, allocate error 6a')
    ALLOCATE (PDLIB_IA_P(npa+1), PDLIB_POSI(3,COUNT_MAX), PDLIB_I_DIAG(npa), stat=istat)
    IF (istat/=0) CALL PARALLEL_ABORT('wwm_fluctsplit, allocate error 6b')
    PDLIB_JA = 0
    PDLIB_IA = 0
    PDLIB_JA_P = 0
    PDLIB_IA_P = 0
    PDLIB_POSI = 0
    ! Points to the position of the matrix entry in the mass matrix
    ! according to the CSR matrix format see p. 124
    J = 0
    K = 0
    PDLIB_IA  (1) = 1
    PDLIB_IA_P(1) = 0
    DO IP = 1, npa ! Run through all rows
      ITMP=0
      DO I = 1, PDLIB_CCON(IP) ! Check how many entries there are ...
        J = J + 1
        IP_J = PTABLE(J,2)
        IP_K = PTABLE(J,3)
        ITMP(IP)   = 1
        ITMP(IP_J) = 1
        ITMP(IP_K) = 1
      END DO
      DO I = 1, npa ! Run through all columns
        IF (ITMP(I) .GT. 0) THEN
          K = K + 1
          PDLIB_JA(K) = I
          PDLIB_JA_P(K) = I-1
        END IF
      END DO
      PDLIB_IA  (IP + 1) = K + 1
      PDLIB_IA_P(IP + 1) = K
    END DO
    PDLIB_POSI = 0
    J = 0
    DO IP = 1, npa
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        IP_J  = PTABLE(J,2)
        IP_K  = PTABLE(J,3)
        DO K = PDLIB_IA(IP), PDLIB_IA(IP+1) - 1
          IF (IP   == PDLIB_JA(K)) PDLIB_POSI(1,J)  = K
          IF (IP   == PDLIB_JA(K)) PDLIB_I_DIAG(IP) = K
          IF (IP_J == PDLIB_JA(K)) PDLIB_POSI(2,J)  = K
          IF (IP_K == PDLIB_JA(K)) PDLIB_POSI(3,J)  = K
        END DO
      END DO
    END DO
    J=0
    DO IP=1,npa
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        IE    =  PDLIB_IE_CELL(J)
        POS   =  PDLIB_POS_CELL(J)
        I1    =  PDLIB_POSI(1,J)
        I2    =  PDLIB_POSI(2,J)
        I3    =  PDLIB_POSI(3,J)
        PDLIB_JA_IE(POS,1,IE) = I1
        PDLIB_JA_IE(POS,2,IE) = I2
        PDLIB_JA_IE(POS,3,IE) = I3
      END DO
    END DO
    deallocate(PTABLE)
  end subroutine ComputeIA_JA_POSI_NNZ
  !**********************************************************************
  !*                                                                    *
  !**********************************************************************
  subroutine finalizePD()
    use yowExchangeModule, only: finalizeExchangeModule
    use yowNodepool,    only: finalizeNodepool
    use yowElementpool, only: finalizeElementpool
    use yowRankModule,  only: finalizeRankModule

    call finalizeRankModule()
    call finalizeExchangeModule()
    call finalizeElementpool()
    call finalizeNodepool()
  end subroutine finalizePD

end module yowpdlibMain
!**********************************************************************
!*                                                                    *
!**********************************************************************
