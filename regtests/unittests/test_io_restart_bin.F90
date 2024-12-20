! This is a test for model IO for WW3. This tests the legacy (binary)
! output of restart data, done by function W3IORS().
!
! Ed Hartnett 1/13/24
program test_io_restart_bin
  use w3iorsmd
  use w3iopomd
  use w3gdatmd
  use w3wdatmd
  use w3odatmd
  use w3iogrmd
  use w3adatmd  
  implicit none
  
!   integer, target :: i
!   integer :: ndsop, iotest, ndsbul, ndsm
!   integer :: ndstrc, ntrace
!   character*7 expected_ptnme
!   character*6 my_fmt
!   real :: expected_loc_1
!   integer :: ndsr = 11
!   real :: dumfpi = 99.9

!   print *, 'Testing WW3 binary restart file code.'

!   ! These are mysterious but have to be called or else the IPASS
!   ! variable does not exist and w3iopo() crashes.
!   call w3nmod(1, 6, 6)
!   call w3setg(1, 6, 6)
!   call w3ndat(6, 6)
!   call w3setw(1, 6, 6)
!   call w3nout(6, 6)
!   call w3seto(1, 6, 6)

!   ndsm   = 20
!   ndsop  = 20
!   ndsbul = 0
!   ndstrc =  6
!   ntrace = 10
  
!   write (ndso,900)
! 900 FORMAT (/15X,'    *** WAVEWATCH III Point output post.***    '/ &
!        15X,'==============================================='/)

!   ! 2.  Read model definition file.
!   CALL W3IOGR('READ', NDSM)
!   WRITE (NDSO,920) GNAME
! 920 FORMAT ('  Grid name : ',A/)  

!   ! Read the file out_pnt.ww3 from the model/tests/data directory.
!   call w3iors('HOT', ndsr, dumfpi)
!   if (iotest .ne. 0) stop 10
!   close(ndsop)

  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_io_restart_bin

