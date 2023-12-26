! This is a test for model IO for WW3. This tests the legacy (binary)
! output of points data, done by function W3IOPO().
!
! Ed Hartnett 10/14/23
program test_io_points_bin
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

  print *, 'Testing WW3 binary point file code.'

  ! These are mysterious but have to be called or else the IPASS
  ! variable does not exist and w3iopo() crashes.
  call w3nmod(1, 6, 6)
  call w3setg(1, 6, 6)
  call w3ndat(6, 6)
  call w3setw(1, 6, 6)
  call w3nout(6, 6)
  call w3seto(1, 6, 6)

  ndsm   = 20
  ndsop  = 20
  ndsbul = 0
  ndstrc =  6
  ntrace = 10
  
  ! Create a point output file needed for this test.
  if (write_test_file() .ne. 0) stop 1

  write (ndso,900)
900 FORMAT (/15X,'    *** WAVEWATCH III Point output post.***    '/ &
       15X,'==============================================='/)

  ! 2.  Read model definition file.
  CALL W3IOGR('READ', NDSM)
  WRITE (NDSO,920) GNAME
920 FORMAT ('  Grid name : ',A/)  

  ! This will not work. But cannot be tested because it will change the value of IPASS,
!  call w3iopo('EAD', ndsop, iotest)
!  if (iotest .ne. 1) stop 7

  ! Read the file out_pnt.ww3 from the model/tests/data directory.
  call w3iopo('READ', ndsop, iotest)
  if (iotest .ne. 0) stop 10
  close(ndsop)

  ! Make sure we got the values we expected.
  if (nopts .ne. 11) stop 11
  expected_loc_1 = 0.0
  do i = 1, nopts
     ! Check ptnme and ptloc arrays.
     print *, ptnme(i), ptloc(1, i), ptloc(2, i)
     if (i .lt. 10) then
        my_fmt = '(a,i1)'
     else
        my_fmt = '(a,i2)'
     endif
     write(fmt = my_fmt, unit=expected_ptnme) 'Point', i
     if (ptnme(i) .ne. expected_ptnme) stop 20
     print *, expected_loc_1
     if (ptloc(1, i) .ne. expected_loc_1) stop 21
     expected_loc_1 = expected_loc_1 + 5000.0
     if (ptloc(2, i) .ne. 0) stop 22
  end do
  
  print *, 'OK!'
  print *, 'SUCCESS!'
end program test_io_points_bin

integer function write_test_file()
  implicit none

  integer :: ntlu, nk, nth, nopts
  character(len=10), parameter :: veropt = '2021-04-06'
  character(len=31), parameter :: idstr = 'WAVEWATCH III POINT OUTPUT FILE'
  real :: ptloc(2,11) = reshape((/ 0., 0., 5000., 0., 10000., 0., 15000., 0., &
       20000., 0., 25000., 0., 30000., 0., 35000., 0., 40000., 0., 45000., 0., 50000., 0. /), &
       (/ 2, 11 /))
  character*40 ptnme(11)
  integer :: time(2) = (/ 19680606, 0 /)
  integer :: nspec = 72
  integer :: iw(11) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  integer :: ii(11) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  integer :: il(11) = (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
  real :: iceo(11) = (/ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. /)
  real :: iceho(11) = (/ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. /)    
  real :: icefo(11) = (/ 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000., 1000. /)    
  real :: dpo(11) = (/ 50., 50., 45., 40., 35., 30., 25., 20., 15., 10., 5. /)
  real :: wao(11) = (/ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. /)      
  real :: wdo(11) = (/ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. /)
  real :: aso(11) = (/ -999.900024, -999.900024, -999.900024, -999.900024, -999.900024, &
       -999.900024, -999.900024, -999.900024, -999.900024, -999.900024, -999.900024 /)    
  real :: cao(11) = (/ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. /)
  real :: cdo(11) = (/ 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0. /)
  character*13 :: grdid(11)
  real :: spco(72, 11)
  integer :: i, j
  integer :: ierr

  ! Initialize some values.
  ntlu = 21
  nk = 3
  nth = 24
  nopts = 11
  do i = 1, nopts
     if (i .le. 9) then
        write(ptnme(i), '(a,i1)') 'Point', i
     else
        write(ptnme(i), '(a,i2)') 'Point', i
     endif
     grdid(i) = 'ww3          '
  end do

  ! Open the file.
  open(ntlu, file="out_pnt.ww3", form="unformatted", status="replace", &
       action="write", convert="big_endian", iostat=ierr)
  if (ierr .ne. 0) stop 111

  ! Write our values.
  write (ntlu, iostat=ierr) idstr, veropt, nk, nth, nopts
  if (ierr .ne. 0) stop 112
  write (ntlu, iostat=ierr) ((ptloc(j,i),j=1,2),i=1,nopts), (ptnme(i),i=1,nopts)
  if (ierr .ne. 0) stop 113
  write (ntlu, iostat=ierr) time
  if (ierr .ne. 0) stop 114
  do i=1, nopts
     write (ntlu, iostat=ierr) iw(i), ii(i), il(i), dpo(i), wao(i), wdo(i),      &
          aso(i), cao(i), cdo(i), iceo(i), iceho(i),        &
          icefo(i), grdid(i), (spco(j,i),j=1,nspec)
     if (ierr .ne. 0) stop 115
  enddo

  ! Close the file.
  close(ntlu)

  ! We're done!
  write_test_file = 0
end function write_test_file
  
