      program wind
c
      integer     nx, ny
      real        radius, umax, dxy
c
      parameter ( nx     = 43 )
      parameter ( radius = 100. )
      parameter ( umax   = 35. )
      parameter ( dxy    = 25. )
c
      integer     ixyc, ix, iy
      real        distx, disty, dist, u10
      real        u(nx,nx), v(nx,nx)
c
      ixyc   = 1 + nx/2
c
      do 110, ix=1, nx
        do 100, iy=1, nx
          distx  = dxy * real(ix-ixyc)
          disty  = dxy * real(iy-ixyc)
          dist   = sqrt ( distx**2 + disty**2 )
          if ( dist .le. radius ) then
              u10    = umax * dist / radius
            else
              u10    = umax * radius / dist
            endif
          if ( dist .gt. 0.1*dxy ) then
              u(ix,iy) = - u10 * disty / dist
              v(ix,iy) =   u10 * distx / dist
            else
              u(ix,iy) = 0.
              v(ix,iy) = 0.
            end if
  100     continue
  110   continue
c
      open(10,file='ww3_prep_wind.inp',status='unknown')
c
      write(10,'(a)') "$ WAVEWATCH III Wind preprocessing"
      write(10,'(a)') "$ --------------------------------"
      write(10,'(a)') "  'WND' 'AI' F T"
      write(10,'(a)') "  19680606 000000"
      write(10,'(a)') "$"
      write(10,'(a)') "  'UNIT' 2 1 '(..T..)' '(..F..)'"
      write(10,'(a)') "  10 'dummy_name '"
      write(10,'(a)') "$"
c
      write (10,'(10f8.2)') ((u(ix,iy),ix=1,nx),iy=1,nx)
      write (10,'(10f8.2)') ((v(ix,iy),ix=1,nx),iy=1,nx)
c
      close(10)
c
      end
