! This is a test for model IO for WW3.
!
! Ed Hartnett 10/14/23
program test_io
  use w3iopomd
  ! USE W3ODATMD, ONLY: NDST, NDSE, IPASS => IPASS2, NOPTS, IPTINT, &
  !      IL, IW, II, PTLOC, PTIFAC, DPO, WAO, WDO,   &
  !      ASO, CAO, CDO, SPCO, PTNME, O2INIT, FNMPRE, &
  !      GRDID, ICEO, ICEHO, ICEFO
!  USE W3GDATMD, ONLY: NGRIDS, NAUXGR
  USE W3GDATMD
  use w3wdatmd
  use w3odatmd
  implicit none
  
  integer, target :: i, j, k, l
  integer :: ndsop, iotst, imod, ndstst, ierr, ndsbul, ndsi, ndsm
  integer :: ndstrc, ntrace

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

  ! ndsop = 10
  ! iotst = 11
  ! ipass => i
  ! ! Open file for error output.
  ! ndse => j
  ! ndse = 20
  ! open(unit = ndse, file = "test_io_error")
  ! ndst => k
  ! ndst = 21
  ! ngrids = 9
  ! imod = 1
  ! ndstst = 22
  ! open(unit = ndstst, file = "test_io_test")
!  call w3iopo('WRITE', ndsop, iotst, imod)
  call w3iopo('READ', ndsop, iotst, imod)
  if (iotst .ne. 0) stop 10
  close(20)

  print *, 'SUCCESS!'
end program test_io
  
