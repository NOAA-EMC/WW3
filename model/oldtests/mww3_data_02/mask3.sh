#!/bin/sh

  set -e

cat > data << EOF
    585    401
      2.     1.    16.
    162.    50.     1.
   'coast_4m.mask'
     1    78
     1    97
     1   133
    13   137
    14   139
    15   140
    18   159
    66   317
   135   338
   201   170
   205   237
   579   353
     0     0
     125.
   195   230
   203   236
   204   236
   585   319
   585   320
     0     0
   'coast_4m.mask2'
EOF


cat > remask.f90 << EOF
      program remask
!
      implicit none
!
      integer               :: nx, ny, ix, iy, ixc, iyc, imtst,       &
                               irange, icount, iyr, ixr
      integer, allocatable  :: mask1(:,:), mask2(:,:), test(:,:),     &
                              lmask(:,:)
      real                  :: sx, sy, x0, y0, fac, rmax,             &
                               y, dera, sxkm, sykm, dist
      character(len=40)     ::  fname
!
      dera = 4. * atan(1.) / 180.
!
      open (10,file='data')
      read (10,*) nx, ny
      read (10,*) sx, sy, fac
      sx = sx / fac
      sy = sy / fac
      read (10,*) x0, y0, fac
      x0 = x0 / fac
      y0 = y0 / fac
!
      write (*,*) 'Grid dimensions ', nx, ny
      write (*,*) 'Grid increments ', sx, sy
      write (*,*) 'Grid origin ', x0, y0
!
      allocate ( mask1(nx,ny) , mask2(nx,ny), test(nx,ny) )
      read (10,*) fname
      write (*,*)
      write (*,*) 'reading from ', fname
      open (11,file=fname, status='OLD')
      rewind (11) 
      do iy=1, ny
        read(11,*) mask1(:,iy)
        end do
      mask2 = mask1
      close (11)
      write (*,*) 'original mask read'
!
! Land to excluded
!
      do 
        read (10,*) ixc, iyc
        if ( ixc.eq.0 .and. iyc.eq.0 ) exit
        test = -1
        imtst = mask2(ixc,iyc)
        test(ixc,iyc) = 0
        mask2(ixc,iyc) = 3
        irange = 0
        do
          irange = irange + 1
          icount = 0
          do ix = max(1,ixc-irange), min(nx,ixc+irange)
            do iy = max(1,iyc-irange), min(ny,iyc+irange)
              if ( test(ix,iy).eq.-1 .and. mask2(ix,iy).eq.imtst ) then
                  if ( ix.gt.1 .and. test(ix-1,iy).eq.0 ) then
                      test (ix,iy) = 0
                      mask2(ix,iy) = 3
                      icount = icount + 1
                    end if
                  if ( ix.lt.nx .and. test(ix+1,iy).eq.imtst ) then
                      test (ix,iy) = 0
                      mask2(ix,iy) = 3
                      icount = icount + 1
                    end if
                  if ( iy.gt.1 .and. test(ix,iy-1).eq.imtst ) then
                      test (ix,iy) = 0
                      mask2(ix,iy) = 3
                      icount = icount + 1
                    end if
                  if ( iy.lt.ny .and. test(ix,iy+1).eq.imtst ) then
                      test (ix,iy) = 0
                      mask2(ix,iy) = 3
                      icount = icount + 1
                    end if
                end if
              end do
            end do
          if ( icount .eq. 0 ) exit
          end do
        end do
!
! do ix=139, nx
! do iy=ny,200,-1
! if ( mask2(ix,iy).eq.0) then
! write (*,*) ix,iy
! goto 100
! end if
! end do
! end do
! 100 continue
!
! do iy=ny, 1, -1
! write (*,'(2x,145i1)') mask2(441:585,iy)
! end do
! stop
!
! Set up distance to (remaining) land mask
!
      read (10,*) rmax
      write (*,*)
      write (*,*) 'Maximum distance to land (km) ', rmax
!
      sykm = sy * 40000. / 360.
      iyr = int(rmax/sykm)
      test = 0
      do iy=2, ny
        y = y0 + real(iy-1)*sy
        sxkm = sx * 40000 / 360. * cos(y*dera)
        ixr = int(rmax/sxkm)
        allocate ( lmask(-ixr:ixr,-iyr:iyr) )
        do ixc=-ixr, ixr
          do iyc=-iyr, iyr
            dist = sqrt ( (real(ixc)*sxkm)**2 + (real(iyc)*sykm)**2 )
            if ( dist.lt.rmax ) then
                lmask(ixc,iyc) = 1
              else
                lmask(ixc,iyc) = 0
              end if
            end do
          end do
        do ix=2, nx-1
          if ( mask2(ix,iy) .eq. 0 ) then
              if ( mask2(ix+1,iy  ).eq.1 .or.  &
                   mask2(ix-1,iy  ).eq.1 .or.  &
                   mask2(ix  ,iy+1).eq.1 .or.  &
                   mask2(ix  ,iy-1).eq.1 ) then
                do ixc=max(1,ix-ixr), min(nx,ix+ixr)
                  do iyc=max(1,iy-iyr), min(ny,iy+iyr)
                    if ( mask2(ixc,iyc).eq.1 .or. mask1(ixc,iyc).eq.0 ) then
                        if ( lmask(ix-ixc,iy-iyc) .eq. 1 ) test(ixc,iyc) = 1
                      end if
                    end do
                  end do
                end if
            end if
          end do
        deallocate ( lmask )
        end do
!
      do ix=1, nx
        do iy=1, ny
          if ( mask2(ix,iy).eq.1 .and. test(ix,iy).eq.0 ) mask2(ix,iy) = 3
          if ( mask1(ix,iy).eq.0 .and. test(ix,iy).eq.1 ) mask2(ix,iy) = 0
          end do
        end do
!
! Automated boundary points
!
      do ix=1, nx
        if ( mask2(ix, 1) .eq. 1 )  mask2(ix, 1) = 2
        if ( mask2(ix,ny) .eq. 1 )  mask2(ix,ny) = 2
        end do
      do iy=1, ny
        if ( mask2( 1,iy) .eq. 1 )  mask2( 1,iy) = 2
        if ( mask2(nx,iy) .eq. 1 )  mask2(nx,iy) = 2
        end do
      do ix=2, nx-1
        do iy=2, ny-1
          if ( mask2(ix,iy) .eq. 1 ) then
              if ( (mask2(ix-1,iy  ).eq.3 .and. mask1(ix-1,iy  ).eq.1 ) .or. &
                   (mask2(ix+1,iy  ).eq.3 .and. mask1(ix+1,iy  ).eq.1 ) .or. &
                   (mask2(ix  ,iy-1).eq.3 .and. mask1(ix  ,iy-1).eq.1 ) .or. &
                   (mask2(ix  ,iy+1).eq.3 .and. mask1(ix  ,iy+1).eq.1 ) ) then
                  mask2(ix,iy) = 2
                end if
            end if
          end do
        end do
!
! Remove individual boundary points
!
      do 
        read (10,*) ix, iy
        if ( ix.eq.0 .and. iy.eq.0 ) exit
        if ( mask2(ix,iy) .ne. 2 ) then
            write (*,*) 'point ', ix, iy, 'not a boundary point'
          end if
        mask2(ix,iy) = 3
        end do
!
! Write new mask
!
      read (10,*) fname
      write (*,*)
      write (*,*) 'writing to ', fname
      open (11,file=fname)
      rewind (11) 
      do iy=1, ny
        write(11,'(40i2)') mask2(:,iy)
        end do
      close (11)
      write (*,*) 'new mask written'
!
      end
EOF


  pgf90 -Mlist remask.f90

  a.out

  rm -f a.out remask.f90 data remask.lst
