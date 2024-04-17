! This is test code for the WW3 I/O unit tests.
!
! This file holds a function used by multiple tests.
!
! Ed Hartnett, 1/11/24
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
  
