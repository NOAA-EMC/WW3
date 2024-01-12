! This is a test for model IO for WW3. This tests the w3nmod() subroutine.
!
! Ed Hartnett 1/12/24
program test_w3nmod
  use w3iopomd
  use w3gdatmd
  use w3wdatmd
  use w3odatmd
  use w3iogrmd
  use w3adatmd  
  implicit none
  
  integer, target :: i
  integer :: ndsop, iotest, ndsbul, ndsm
  integer :: ndstrc, ntrace
  character*7 expected_ptnme
  character*6 my_fmt
  real :: expected_loc_1
  integer :: write_test_file

  print *, 'Testing w3nmod().'

  call w3nmod(1, 6, 6)
  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_w3nmod
