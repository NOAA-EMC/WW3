! This is a test for model IO for WW3. This tests the w3nmod() subroutine.
!
! Ed Hartnett 1/12/24
program test_w3nmod
  use w3gdatmd
  implicit none
  
  integer :: i
  
  print *, 'Testing w3nmod().'

  call w3nmod(3, 6, 6)
  if (ngrids .ne. 3) stop 10
  if (nauxgr .ne. -1) stop 11
  do i = 1, 3
     if (grids(i)%ginit .neqv. .false.) stop 12
     if (grids(i)%guginit .neqv. .false.) stop 12
     if (sgrds(i)%sinit .neqv. .false.) stop 12
     if (mpars(i)%pinit .neqv. .false.) stop 12
  end do
  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_w3nmod
