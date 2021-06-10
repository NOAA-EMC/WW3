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
!> Has data that belong to nodes
module yowNodepool
  use yowDatapool, only: rkind
  implicit none
  private
  public :: finalizeNodepool, nodes, ghosts


  !> Holds the nodes data.
  !> Such as x, y, z data or the number of connected nodes
  type, public :: t_Node

    !> the local node number
    integer :: id = 0

    !> the global node number
    integer :: id_global = 0

    !> number of connected nodes.
    !> holds the number of neighbors conntected to this node.
    !> to get the connected nodes, iterate over the connNodes() Array
    integer :: nConnNodes = 0

    !> The domain ID to which this node belongs.
    !> The first domain starts by 1. Fortran Stye
    integer :: domainID = 0

    contains
      !> Insert a node to the connected Nodes array. See Node_insertConnNode()
      !> Just a helper subroutine to make nicer code
      procedure :: insertConnNode

      !> return a pointer to the i-th node number conntected to this node
      !> Just a helper function to make nicer code
      procedure :: connNodes

      !> returns true if this node is a ghost node
      procedure :: isGhost
  end type

  !> coordinates of the local +  ghost nodes. range [1:npa]
  real(rkind), public, target, allocatable :: x(:), y(:), z(:)
  real(rkind), public, target, allocatable :: PDLIB_SI(:), PDLIB_TRIA(:), PDLIB_IEN(:,:)
  integer, public, target, allocatable :: PDLIB_CCON(:), PDLIB_IA(:), PDLIB_JA(:)
  integer, public, target, allocatable :: PDLIB_IA_P(:), PDLIB_JA_P(:), PDLIB_JA_IE(:,:,:)
  integer, public, target, allocatable :: PDLIB_POS_CELL(:), PDLIB_IE_CELL(:)
  integer, public, target, allocatable :: PDLIB_IE_CELL2(:,:), PDLIB_POS_CELL2(:,:)
  integer, public, target, allocatable :: PDLIB_POSI(:,:), PDLIB_I_DIAG(:)
  integer, public, target, allocatable :: ListNP(:), ListNPA(:), ListIPLG(:)

  !> number of nodes, global
  integer, public :: np_global = 0
  integer, public :: PDLIB_NNZ = 0

  !> number of nodes, local
  integer, public :: np  = 0

  !> number of ghost nodes this partition holds
  integer, public :: ng = 0

  !> number of ghost + resident nodes this partition holds
  integer, public :: npa = 0

  !> all nodes with their data.
  !> to iterate over all local nodes, use funtion node(local id) to get a pointer to t_node
  type(t_Node), public, allocatable, target :: nodes_global(:)

  !> max number of conntected nodes to a node
  integer, public :: maxConnNodes = 0

  !> conntected Node Array.
  !> 2D Array. Holds the global node numbers conntected to each other.
  !> \param 1 global node number from wich you want the neighbors
  !> \param 2 from 1 to t_Node::nConnNodes
  integer, public, allocatable :: connNodes_data(:,:)

  !> Node local to global mapping.
  !> np long. give the gobal node id
  integer, public, allocatable :: iplg(:)

  !> Node global to local mapping
  !> np_global long. give the local node id but only for this rank. local node id for other ranks are set to 0!
  integer, public, allocatable :: ipgl(:)

  !> Ghost local to global mapping
  !> ng long. give the global node id of nodes, which
  !> belong to adjacent domains
  integer, public, allocatable :: ghostlg(:)

  !> Ghost global to local mapping
  !> np_global long. give the local ghost node id. local ghost node ids for other ranks are set to 0!
  integer, public, allocatable :: ghostgl(:)

  !> Numbers of Nodes pro Processor.
  !> Has the number of nodes each thread ows. Array is nTasks long
  integer, public, allocatable :: np_perProc(:)

  !> Number of Nodes pro Processor totalize.
  !> Has the sum of nodes each thread owen. Array in nTasks+1 long
  !> Processor i stores np_perProcSum(i)::np_perProcSum(i+1)-1 nodes
  integer, public, allocatable :: np_perProcSum(:)


  contains

  !> return a pointer to the i-th node number conntected to this node.
  !> \param i
  !> \return pointer to the i-th node number
  function connNodes(this, i)
    implicit none
    class(t_Node) :: this
    integer, intent(in) :: i
    type(t_Node), pointer :: connNodes
    connNodes => nodes_global(connNodes_data(this%id_global, i))
  end function

  !> return pointer to the (global) node from the local id.
  !> This is in effekt iplg(id_local)
  !> \param id_local the local node number
  !> \return poiner to the (global) node
  function nodes(id_local)
    implicit none
    integer, intent(in) :: id_local
    type(t_Node), pointer :: nodes
    nodes => nodes_global(iplg(id_local))
  end function

  !> return pointer to the (global) (ghost) node
  !> Ghost nodes are nodes in the global node array, with the particularity
  !> that their local id is the id from another domain
  !> This is in effekt ghostlg(1:ng)
  !> \param id Counts from 1 to ng
  !> \return pointer to the (global) node
  function ghosts(id)
    implicit none
    integer, intent(in) :: id
    type(t_Node), pointer :: ghosts
    ghosts => nodes_global(ghostlg(id))
  end function

  !> Insert a node number to the end of the conntected node array
  !> \param index optional - node number to insert. If it not present, just increas temporarily array lenght for later allocation
  subroutine insertConnNode(this, ind)
    implicit none
    class(t_Node) :: this
    integer    , intent(in), optional :: ind
    integer :: i
    type(t_Node), pointer :: node

    ! if index is present, Check if the node has allreay be insert. Then insert it
    ! if index is not present, just increas temporarily array lenght for later allocation
    if(present(ind)) then
      do i = 1, this%nConnNodes
        node => this%connNodes(i)
        if(node%id_global == ind) then
          return
        end if
      end do

      this%nConnNodes = this%nConnNodes +1
!       connNode => this%connNodes(this%nConnNodes)
      connNodes_data(this%id_global, this%nConnNodes) = ind
!       connNode = index
    else
      this%nConnNodes = this%nConnNodes +1
    end if
  end subroutine insertConnNode

  !> Returns true if this node is a ghost node
  function isGhost(this)
    implicit none
    class(t_node), intent(in) :: this
    logical :: isGhost

    if(this%id <= np) then
      isGhost = .false.
    else
      isGhost = .true.
    endif
  end function

  subroutine finalizeNodepool()
    implicit none

    if(allocated(x))              deallocate(x)
    if(allocated(y))              deallocate(y)
    if(allocated(z))              deallocate(z)
    if(allocated(nodes_global))   deallocate(nodes_global)
    if(allocated(connNodes_data)) deallocate(connNodes_data)
    if(allocated(iplg))           deallocate(iplg)
    if(allocated(ipgl))           deallocate(ipgl)
    if(allocated(ghostlg))        deallocate(ghostlg)
    if(allocated(ghostgl))        deallocate(ghostgl)
    if(allocated(np_perProc))     deallocate(np_perProc)
    if(allocated(np_perProcSum))  deallocate(np_perProcSum)
  end subroutine
end module yowNodepool
