! This is a test for model IO for WW3.
!
! Ed Hartnett 10/14/23
program test_io
  use w3iopomd
  USE W3ODATMD, ONLY: NDST, NDSE, IPASS => IPASS2, NOPTS, IPTINT, &
       IL, IW, II, PTLOC, PTIFAC, DPO, WAO, WDO,   &
       ASO, CAO, CDO, SPCO, PTNME, O2INIT, FNMPRE, &
       GRDID, ICEO, ICEHO, ICEFO
  USE W3GDATMD, ONLY: NGRIDS, NAUXGR
  implicit none
  integer, target :: i, j, k, l

  integer :: ndsop, iotst, imod
  print *, 'Testing WW3 IO...'

  ndsop = 10
  iotst = 11
  ipass => i
  ! Open file for error output.
  ndse => j
  ndse = 20
  open(unit = ndse, file = "test_io_error")
  ndst => k
  ndst = 21
  ngrids = 9
  imod = 1
  ndstst = 22
  open(unit = ndstst, file = "test_io_test")
  call w3iopo('WRITE', ndsop, iotst, imod)
  if (iotst .ne. 0) stop 10
  close(20)

  print *, 'SUCCESS!'
end program test_io
  
