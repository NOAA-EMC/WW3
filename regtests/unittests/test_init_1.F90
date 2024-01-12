! This is a test for model IO for WW3. This tests the w3nmod() subroutine.
!
! Ed Hartnett 1/12/24
program test_w3nmod
  use w3gdatmd
  use w3wdatmd
  use w3odatmd ! w3nout(), w3seto()
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

  print *, 'Testing w3setg().'
  call w3setg(3, 6, 6)
  if (igrid .ne. 3 .or. isgrd .ne. 3 .or. ipars .ne. 3) stop 20
  print *, 'OK!'

  print *, 'Testing w3ndat().'
  call w3ndat(6, 6)
  do i = 1, 3
     if (wdatas(i)%dinit .neqv. .false.) stop 30
     if (wdatas(i)%fl_all .neqv. .false.) stop 31
  end do
  print *, 'OK!'

  print *, 'Testing w3setw().'
  call w3setw(3, 6, 6)
  if (iwdata .ne. 3) stop 40
  print *, 'OK!'
  
  print *, 'Testing w3nout().'
  call w3nout(6, 6)
  if (noutp .ne. 3) stop 50
  do i = 1, 3
     if (outpts(i)%ndso .ne. 6) stop 51
  end do
  if (noge(1) .ne. 9) stop 52
  if (idout( 1, 1) .ne. 'Water depth         ') stop 53
  print *, 'OK!'
  
  print *, 'Testing w3seto().'
  call w3seto(3, 6, 6)
  if (ioutp .ne. 3) stop 60
  if (ndso .ne. 6) stop 61
  print *, 'OK!'
  
  print *, 'SUCCESS!'
end program test_w3nmod
