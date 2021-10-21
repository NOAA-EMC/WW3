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
!> Has fancy data
module yowDatapool
  use MPI, only: MPI_COMM_WORLD, MPI_INTEGER, MPI_REAL4, MPI_REAL8, MPI_STATUS_SIZE
  implicit none
!#ifdef USE_SINGLE
!  !> single precision. Enable with compiler flag -DUSE_SINGLE
!  integer,parameter :: rkind = 4
!#else
  !> double precision. Default real datatype
  integer,parameter :: rkind = 4 
!#endif
  logical, parameter :: debugPrePartition = .false.
  logical, parameter :: debugPostPartition = .false.
  logical, parameter :: debugParmetis = .false.
  !> write partition information into file fort.600.
  !> one can display partition with katerfempresenter. just open the system.dat and
  !> click on the partitions icon
  logical, parameter :: debugPartition = .false.

  !> Number of threads
  integer, save :: nTasks = 0

  !> The thread id.
  !> starts by 0. The first Thread has rank 0
  integer, save :: myrank = 0

  !> MPI Communicator.
  !> Should be MPI_COMM_WORLD. If pdlib is run into a existing MPI enviroment, comm is set to a new communicator
  integer,public,save :: comm

  !> MPI Integer Type.
  !> Should be MPI_INTEGER
  integer,save :: itype = MPI_INTEGER

  !> MPI Real Type
  !> Shpuld be MPI_REAL8
  integer :: istatus(MPI_STATUS_SIZE)
!#ifdef USE_SINGLE
  integer, save :: rtype = MPI_REAL4
!#else
!  integer, save :: rtype = MPI_REAL8  
!#endif

end module yowDatapool
