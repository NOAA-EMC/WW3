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
!> Has some subroutine to make a nice error message
module yowerr
  implicit none
contains
  subroutine parallel_abort(string, error)
    use yowDatapool, only: comm
    use MPI
    implicit none

    character(*),optional,intent(in) :: string !string to print
    integer,optional,intent(in) :: error       !mpi errorcode
    integer :: ierr,i
    logical :: lopen
    integer :: sl
    ! MPI_MAX_ERROR_STRING = 1024
    character(1024) :: errorstring
    integer :: myrank

    ! Get rank
    call mpi_comm_rank(comm, myrank,ierr)
    if(ierr/=MPI_SUCCESS) write(*,*) "parallel_abort: ierr=", ierr

    inquire(11,opened=lopen)

    if(present(string)) then
      write(*,'(i4,2a)') myrank,': ABORT: ',string
      if(lopen) write(11,'(i4,2a)') myrank,': ABORT: ',string
    endif

    if(present(error)) then
      if(error /= MPI_SUCCESS) then
        ! get errorstring associatet fuer errorcode err
        call mpi_error_string(error, errorstring, sl, ierr)
        if(ierr/=MPI_SUCCESS) write(*,*) "parallel_abort: ierr=", ierr
        write(*,'(i4,2a)') myrank,': MPI ERROR: ', errorstring(1:sl)
        if(lopen) write(11,'(i4,2a)') myrank,': MPI ERROR: ', errorstring
      endif
      do i=1,200; inquire(i,opened=lopen); if(lopen) close(i); enddo;
        call mpi_abort(comm, error,ierr)
        if(ierr/=MPI_SUCCESS) write(*,*) "parallel_abort: ierr=", ierr
      else
        do i=1,200; inquire(i,opened=lopen); if(lopen) close(i); enddo;
          call mpi_abort(comm, 0,ierr)
          if(ierr/=MPI_SUCCESS) write(*,*) "parallel_abort: ierr=", ierr
        endif
      end subroutine parallel_abort


      !> print various error strings and exit.
      !> Call this to print an error string and optional line number, file and MPI error string
      !> \param[in] string Errorstring
      !> \param[in] line Line number
      !> \param[in] file Filename
      !> \param[in] errno The MPI error number which is translated into an error string
      subroutine abort(string, line, file, errno)
        use yowDatapool, only: comm
        use MPI
        implicit none
        ! Errorstring to print
        character(*), optional, intent(in) :: string
        ! Linenumber to print
        integer,      optional, intent(in) :: line
        ! Filename to print
        character(*), optional, intent(in) :: file
        ! MPI error number to translate
        integer,      optional, intent(in) :: errno
        ! Linenumber as string
        character(50) :: lineNumber
        ! MPI_MAX_ERROR_STRING = 1024
        ! MPI Errorstring
        character(MPI_MAX_ERROR_STRING) :: errorstring
        ! The rank of this thread
        integer :: myrank
        ! real MPI errorsting lengt
        integer :: stringLengh
        !
        integer :: ierr

        ! Get rank
        call mpi_comm_rank(comm, myrank,ierr)
        !     if(ierr/=MPI_SUCCESS) write(*,*) "parallel_abort: ierr=", ierr

        ! Always print rank
        write(*, '(i2,a)', advance='no') myrank, " "

        ! Print a simple "ERROR" when no MPI error number was given because the MPI error string contain an "ERROR" allready
        if(.not. present(errno)) then
          write(*,'(a)', advance='no' ) " ERROR "
        endif

        ! print file and linenumber
        if(present(file)) then
          write(*,'(a)',advance='no' ) file

          if(present(line)) then
            Write(lineNumber, '(i10)') line
            write(*, '(2a)', advance='no') ":", trim(adjustl(lineNumber))
          endif

          write(*, '(a)', advance='no') " "
        endif

        ! if only linenumber is present, add an "Line:" string
        if(.not. present(file) .and. present(line)) then
          Write(lineNumber, '(i10)') line
          write(*, '(2a)', advance='no') "Line:", trim(adjustl(lineNumber))
          write(*, '(a)', advance='no') " "
        endif

        ! print the errror string
        if(present(string)) then
          write(*,'(a)', advance='no') string
        endif

        ! translate and print the MPI error string
        if(present(errno) .and. errno /= MPI_SUCCESS) then
          call mpi_error_string(errno, errorstring, stringLengh, ierr)
          write(*,'(2a)', advance='no') 'MPI ERROR: ', errorstring(1:stringLengh)
        endif

        write(*,*)
        stop

      end subroutine abort

      !> print warning
      !> Call this to print an warning string and optional line number, and file
      !> \param[in] string warnstring
      !> \param[in] line Line number
      !> \param[in] file Filename
      subroutine warn(string, line, file)
        use yowDatapool, only: comm
        use MPI
        implicit none
        ! Errorstring to print
        character(*), optional, intent(in) :: string
        ! Linenumber to print
        integer,      optional, intent(in) :: line
        ! Filename to print
        character(*), optional, intent(in) :: file
        ! Linenumber as string
        character(50) :: lineNumber
        ! The rank of this thread
        integer :: myrank
        !
        integer :: ierr

        ! Get rank
        call mpi_comm_rank(comm, myrank,ierr)
        !     if(ierr/=MPI_SUCCESS) write(*,*) "parallel_abort: ierr=", ierr

        ! Always print rank
        write(*, '(i2,a)', advance='no') myrank, " "

        write(*,'(a)', advance='no' ) " WARN "

        ! print file and linenumber
        if(present(file)) then
          write(*,'(a)',advance='no' ) file

          if(present(line)) then
            Write(lineNumber, '(i10)') line
            write(*, '(2a)', advance='no') ":", trim(adjustl(lineNumber))
          endif

          write(*, '(a)', advance='no') " "
        endif

        ! if only linenumber is present, add an "Line:" string
        if(.not. present(file) .and. present(line)) then
          Write(lineNumber, '(i10)') line
          write(*, '(2a)', advance='no') "Line:", trim(adjustl(lineNumber))
          write(*, '(a)', advance='no') " "
        endif

        ! print the errror string
        if(present(string)) then
          write(*,'(a)', advance='no') string
        endif

        write(*,*)
      end subroutine warn
    end module yowerr
