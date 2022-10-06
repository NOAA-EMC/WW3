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
module yowElementpool
  implicit none
  private
  public :: finalizeElementpool, belongto


  !> number of elements, global
  integer, public :: ne_global = 0

  !> number of local elements
  integer, public :: ne = 0

  !> number of elements of the augmented domain

  !> local element array. it stores the local node IDs
  !> first index from 1 to 3.
  !> second index from 1 to ne.
  !> local node IDs in [1:np]. local ghost IDs in [np+1:np+ng]
  integer, public, target, allocatable :: INE(:,:)

  !> global element array. it stored the global node IDs
  !> first index from 1 to 3.
  !> second index from 1 to ne_global
  !integer, public, allocatable :: INE_global(:,:)

  !> Element local to global mapping
  !> ne long. give the global element id
  integer, public, target, allocatable :: ielg(:)

contains


  !> Returns true if the element belongs to rank.
  !> conversione: If a element is connected to domain 1,2 and 3. It belongs to 1,2 and 3.
  !> @param[in] rank optional. If not given, datapool:myrank is used
  function belongTo(ele_in, rank)
    use yowDatapool, only: myrank, nTasks
    use yowNodepool, only: t_Node, nodes_global
    implicit none
    integer, intent(in) :: ele_in(3)
    integer, intent(in), optional :: rank
    logical :: belongTo

    integer :: myDomainID
    integer :: nodes(3)
    integer J

    if(present(rank) .eqv. .true.) then
      myDomainID = rank +1
    else
      myDomainID = myrank + 1
    endif

    ! check if this element adjoint to three different domains.
    belongTo = .false.
    DO J=1,3
      IF (nodes_global(ele_in(J)) % domainID == myDomainID) THEN
        belongTo = .true.
      END IF
    END DO
  end function belongTo


  subroutine finalizeElementpool()
    implicit none

    if(allocated(INE))        deallocate(INE)
    !if(allocated(INE_global)) deallocate(INE_global)
    if(allocated(ielg))       deallocate(ielg)
  end subroutine finalizeElementpool
end module yowElementpool
