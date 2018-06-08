module MallocInfo_m
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/    01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Init pdlib part
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!
  use :: iso_c_binding
  implicit none
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3XXXX')
  
    !> This structure type is used to return information about the dynamic memory allocator.
    type, bind(c) :: MallInfo_t
      !> This is the total size of memory allocated with sbrk by malloc, in bytes.
      integer(c_int) :: arena                  
      !> This is the number of chunks not in use. (The memory allocator internally gets chunks of memory from the operating system, and then carves them up to satisfy individual malloc requests; see Efficiency and Malloc.)
      integer(c_int) :: ordblks        
      !> This field is unused.
      integer(c_int) :: smblks        
      !> This is the total number of chunks allocated with mmap.
      integer(c_int) :: hblks        
      !> This is the total size of memory allocated with mmap, in bytes.
      integer(c_int) :: hblkhd        
      !> This field is unused.
      integer(c_int) :: usmblks        
      !> This field is unused.
      integer(c_int) :: fsmblks        
      !> This is the total size of memory occupied by chunks handed out by malloc.
      integer(c_int) :: uordblks        
      !> This is the total size of memory occupied by free (not in use) chunks.
      integer(c_int) :: fordblks        
      !> This is the size of the top-most releasable chunk that normally borders the end of the heap (i.e., the high end of the virtual address space’s data segment).
      integer(c_int) :: keepcost      
    end type
    
    interface
      function mallinfo() bind(c, name="mallinfo") result(data)
        use :: iso_c_binding
        implicit none
        
        type, bind(c) :: MallInfo_t      
          integer(c_int) :: arena                        
          integer(c_int) :: ordblks              
          integer(c_int) :: smblks        
          integer(c_int) :: hblks        
          integer(c_int) :: hblkhd        
          integer(c_int) :: usmblks        
          integer(c_int) :: fsmblks        
          integer(c_int) :: uordblks        
          integer(c_int) :: fordblks        
          integer(c_int) :: keepcost      
        end type
        type(MallInfo_t) :: data
      end function
    end interface
  
  contains
  
  subroutine getMallocInfo(malinfo)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | THomas Huxhorn (BGS IT&E GmbH     |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/    01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Init pdlib part
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!
    implicit none
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3XXXX')
    type(MallInfo_t), intent(out) :: malinfo
    malinfo = mallinfo()
  end subroutine
  
  subroutine printMallInfo(ihdnl,malinfo)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/    01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Init pdlib part
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!
    implicit none
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3XXXX')
    real :: ib2m
    integer(8) :: vmsize, vmRSS
    integer, intent(in)          :: ihdnl 
    type(MallInfo_t), intent(in) :: malinfo
    ib2m=1./REAL(1024**2)
    vmsize = getVmSize()
    vmRSS  = getVMRSS()
    !write(*,'(A72,2F20.10)') "Total size of memory allocated with sbrk by malloc in mbyte.  ", malinfo%arena*ib2m
    !write(*,'(A72,2F20.10)') "Total size of memory allocated with mmap, in mbytes.          ", malinfo%hblkhd*ib2m
    !write(*,'(A72,2F20.10)') "Total size of memory occupied by chunks handed out by malloc.", malinfo%uordblks*ib2m
    !write(*,'(A72,I10)') "Total number of chunks allocated with mmap.                  ", malinfo%hblks
    !write(*,'(A72,I10)') "Number of chunks not in use.                                 ", malinfo%ordblks
    !write(*,'(A72,2F20.10)') "Total size of memory occupied by free (not in use) chunks.   ", malinfo%fordblks*ib2m
    !write(*,'(A72,2F20.10)') "Size of the top-most releasable chunk borders end of the heap", malinfo%keepcost*ib2m    
    write(740+ihdnl,'(A72,2F20.10)') "VM size in proc ", vmsize/1024. 
    write(740+ihdnl,'(A72,2F20.10)') "RSS size in prof ", vmRSS/1024.
    call flush(740+ihdnl)
  end subroutine

!VmPeak: Peak virtual memory usage
!VmSize: Current virtual memory usage
!VmLck:	Current mlocked memory
!VmHWM:	Peak resident set size
!VmRSS:	Resident set size
!VmData: Size of "data" segment
!VmStk:	Size of stack
!VmExe:	Size of "text" segment
!VmLib:	Shared library usage
!VmPTE:	Pagetable entries size
!VmSwap: Swap space used

  function getVmSize() result(vmsize)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/    01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Init pdlib part
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!
    implicit none
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3XXXX')
    integer(8) :: vmsize
    character(len=80) :: stat_key, stat_value
    !
    vmsize = 0
    open(unit=1000, file="/proc/self/status", status='old', err=99)
    do while (.true.)
      read(unit=1000, fmt=*, err=88) stat_key, stat_value
      if (stat_key == 'VmSize:') then
        read(stat_value, *) vmsize
        exit
      end if
    end do
    88 close(unit=1000)
      if (vmsize == 0) goto 99
      return
      !
    99 print *, 'ERROR: procfs not mounted or not compatible'
    vmsize = -1
  end function getVmSize

  function getVmRSS() result(vmRSS)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :        01-June-2018 |
!/                  +-----------------------------------+
!/
!/    01-June-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Init pdlib part
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/S      USE W3SERVMD, ONLY: STRACE
!
    implicit none
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'W3XXXX')
    integer(8) :: vmRSS
    character(len=80) :: stat_key, stat_value
    !
    vmRSS = 0
    open(unit=1000, file="/proc/self/status", status='old', err=99)
    do while (.true.)
      read(unit=1000, fmt=*, err=88) stat_key, stat_value
      if (stat_key == 'VmRSS:') then
        read(stat_value, *) vmRSS
        exit
      end if
    end do
    88 close(unit=1000)
      if (vmRSS == 0) goto 99
      return
      !
    99 print *, 'ERROR: procfs not mounted or not compatible'
    vmRSS = -1
  end function getVmRSS
  
end module

!program test
!  use MallocInfo_m
!  implicit none
!  type(MallInfo_t) :: mallinfos(10000)  
!  integer :: i, nInfos
!  integer, allocatable :: data(:)
!  
!  allocate(data(0))
!  nInfos = 0
!  do i=1, 10
!    write(*,*) "Iteration",i
!    deallocate(data)
!    allocate(data(i*100000))
!    nInfos = nInfos+1
!    call getMallocInfo(mallinfos(nInfos))    
!    call printMallInfo(IAPROC,mallInfos(nInfos))
!    call sleep(1)
!  end do
  
!   do i=10, 1, -1
!    write(*,*) "Iteration",i
!    deallocate(data)
!    allocate(data(i*100000))
!    nInfos = nInfos+1
!    call getMallocInfo(mallinfos(nInfos))    
!    call printMallInfo(IAPROC,mallInfos(nInfos))    
!    call sleep(1)
!  end do
  
!  write(*,*) "Total size of memory allocated with sbrk. min, mean, max", minval(mallinfos(1:nInfos)%arena), sum(mallinfos(1:nInfos)%arena)/nInfos, maxval(mallinfos(1:nInfos)%arena)
!end program
