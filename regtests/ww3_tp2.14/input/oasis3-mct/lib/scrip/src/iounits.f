!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This module is a dynamic I/O unit manager.  It keeps track of
!     which units are in use and reserves units for stdin, stdout, and
!     stderr.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: iounits.f 2826 2010-12-10 11:14:21Z valcke $
!
!     Copyright (c) 1997, 1998 the Regents of the University of 
!       California.
!
!     This software and ancillary information (herein called software) 
!     called SCRIP is made available under the terms described here.  
!     The software has been approved for release with associated 
!     LA-CC Number 98-45.
!
!     Unless otherwise indicated, this software has been authored
!     by an employee or employees of the University of California,
!     operator of the Los Alamos National Laboratory under Contract
!     No. W-7405-ENG-36 with the U.S. Department of Energy.  The U.S.
!     Government has rights to use, reproduce, and distribute this
!     software.  The public may copy and use this software without
!     charge, provided that this Notice and any statement of authorship
!     are reproduced on all copies.  Neither the Government nor the
!     University makes any warranty, express or implied, or assumes
!     any liability or responsibility for the use of this software.
!
!     If software is modified to produce derivative works, such modified
!     software should be clearly marked, so as not to confuse it with 
!     the version available from Los Alamos National Laboratory.
!
!***********************************************************************

      module iounits

!-----------------------------------------------------------------------

      use kinds_mod   ! defines data types
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------

      logical (kind=log_kind), dimension(99), save ::
     &    unit_free   ! flags to determine whether unit is free for use

      integer (kind=int_kind), parameter ::
     &    stdin  = 5, ! reserves unit for standard input
     &    stdout = 6, ! reserves unit for standard output
     &    stderr = 6  ! reserves unit for standard error

!***********************************************************************

      contains

!***********************************************************************

      subroutine get_unit(iunit)

!-----------------------------------------------------------------------
!
!     This routine returns the next available I/O unit number.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(out) ::
     &     iunit   ! next free I/O unit

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n

      logical (kind=log_kind), save :: first_call = .true.

!-----------------------------------------------------------------------
!
!     if this is the first call, reserve stdout, stdin and stderr
!
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine get_unit'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      if (first_call) then
        unit_free = .true.
        unit_free(stdin)  = .false.
        unit_free(stdout) = .false.
        unit_free(stderr) = .false.
        first_call = .false.
      endif

!-----------------------------------------------------------------------
!
!     search for next available unit
!
!-----------------------------------------------------------------------

      srch_unit: do n=1,99
        if (unit_free(n)) then
          iunit = n
          unit_free(n) = .false.
          exit srch_unit
        endif
      end do srch_unit
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine get_unit'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------

      end subroutine get_unit

!***********************************************************************

      subroutine release_unit(iunit)

!-----------------------------------------------------------------------
!
!     This routine releases the specified unit and closes the file.
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) ::
     &     iunit   ! I/O unit to release

!-----------------------------------------------------------------------
!
!     closes I/O unit and declares it free
!
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine release_unit'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      unit_free(iunit) = .true.
      close(iunit)

!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine release_unit'
         WRITE (UNIT = nulou,FMT = *)' '
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      end subroutine release_unit

!***********************************************************************

      end module iounits

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
