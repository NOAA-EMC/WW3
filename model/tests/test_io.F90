! This is a test for model IO for WW3.
!
! Ed Hartnett 10/14/23
program test_io
  use w3iopomd
  use w3gdatmd
  use w3wdatmd
  use w3odatmd
  use w3iogrmd
  use w3adatmd  
  implicit none
  
  integer, target :: i, j, k, l
  integer :: ndsop, iotest, imod, ndstst, ierr, ndsbul, ndsi, ndsm
  integer :: ndstrc, ntrace
  real :: m2km

  print *, 'Testing WW3 IO...'

  call w3nmod(1, 6, 6)
  call w3setg(1, 6, 6)
  call w3ndat(6, 6)
  call w3setw(1, 6, 6)
#ifdef w3_nl1
  call w3naux(6, 6)
  call w3seta(1, 6, 6)
#endif
  call w3nout(6, 6)
  call w3seto(1, 6, 6)

  ndsi   = 10
  ndsm   = 20
  ndsop  = 20
  ndsbul = 0
#ifdef w3_nco
  ndscbul = 0
#endif
  ndstrc =  6
  ntrace = 10

  write (ndso,900)
900 FORMAT (/15X,'    *** WAVEWATCH III Point output post.***    '/ &
       15X,'==============================================='/)

  open(ndsi, file = 'ww3_outp.inp', status='old', iostat = ierr)
  if (ierr .ne. 0) stop 10

  ! 2.  Read model definition file.
  CALL W3IOGR('READ', NDSM)
  WRITE (NDSO,920) GNAME
920 FORMAT ('  Grid name : ',A/)  

  IF (FLAGLL) THEN
    M2KM = 1.
  ELSE
    M2KM = 1.E-3
  END IF

  ! Read the file out_pnt.ww3 from the model/tests/data directory.n
  call w3iopo('READ', ndsop, iotest)
  if (iotest .ne. 0) stop 10
  close(ndsop)

  ! Make sure we got the values we expected.
  if (nopts .ne. 11) stop 11
  do i = 1, nopts
     print *, ptnme(i), ptloc(1, i), ptloc(2, i)
  end do
  
  print *, 'SUCCESS!'
end program test_io
  
