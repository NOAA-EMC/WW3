      program wind
! Purpose : create wind field on a regular grid to test interpolation onto irregular grid.
      integer     nx, ny
      real        radius, umax, dxy
      real        xmin,xmax,ymin,ymax
c
      parameter ( nx     = 175 )
      parameter ( ny     = 40 )
      parameter ( radius = 2. )
      parameter ( umax   = 35. )
      parameter ( dxy    = 0.5 )
c
      integer     ixc,iyc, ix, iy
      real        distx, disty, dist, u10,XS,YS,theta
      real        u(nx,ny), v(nx,ny)
c
      ixc   = int(nx/2)
      iyc   = int(ny/2)
      xc= 0.0
      yc=72.0
c
      do ix=1, nx
        do  iy=1, ny

! distx disty is the location relative to the center
! they are used to get azimuth angle relative to center (approximate) (theta)
          distx  = dxy * real(ix-ixc)
          disty  = dxy * real(iy-iyc)
          theta=atan2(disty,distx)

! XS and YS is the actual location
          XS=distx + xc
          YS=disty + yc
          dist=W3DIST(.true., XC, YC, XS, YS)
          if ( dist .le. radius ) then
             u10    = umax * dist / radius
          else
             u10    = umax * radius / dist
          endif
          if ( dist .gt. 0.1*dxy ) then
             u(ix,iy) =  -u10 * sin(theta)
             v(ix,iy) =  +u10 * cos(theta)
          else
             u(ix,iy) = 0.
             v(ix,iy) = 0.
          end if

          if(dist.lt.1.0)then
          write(*,*)'XC,YC,XS,YS = ',XC,YC,XS,YS
          write(*,*)'dist = ',dist,distx,disty
          write(*,*)'theta = ',(theta*180.0/3.14)
          write(*,*)'u10 = ',u10,u(ix,iy),v(ix,iy)
          write(*,*)'----------------------------'
          endif

       end do
      end do
c
      open(10,file='ww3_prep_wind.inp',status='unknown')
c
      write(10,'(a)') "$ WAVEWATCH III Wind preprocessing"
      write(10,'(a)') "$ --------------------------------"
      write(10,'(a)') "  'WND' 'LL' F T"
      write(10,'(a)') "  20080522 000000"
      write(10,'(a)') "$"

      xmin=dxy*real( 1-ixc) + xc
      xmax=dxy*real(nx-ixc) + xc
      ymin=dxy*real( 1-iyc) + yc
      ymax=dxy*real(ny-iyc) + yc

      write(10,'(2(F6.1),I4,2(F6.1),I4)'),xmin,xmax,nx,ymin,ymax,ny !  "   183.4 263.0 399 25.1 62.5 188"
      write(10,'(a)') "$"
      write(10,'(a)') "  'UNIT' 2 1 '(..T..)' '(..F..)'"
      write(10,'(a)') "  10 'dummy_name '"
      write(10,'(a)') "$"

      write (10,'(10f8.2)') ((u(ix,iy),ix=1,nx),iy=1,ny)
      write (10,'(10f8.2)') ((v(ix,iy),ix=1,nx),iy=1,ny)

      close(10)

      write(*,*)'x0 = ',xmin,'; '
      write(*,*)'y0 = ',ymin,'; '

      open(11,file='u.dat')
      open(12,file='v.dat')
      do ix=1,nx
         write (11,'(1000f8.2)') (u(ix,iy),iy=1,ny)
         write (12,'(1000f8.2)') (v(ix,iy),iy=1,ny)
      end do
      close(11)
      close(12)

      end program wind

      FUNCTION W3DIST(LLG, XT, YT, XS, YS) RESULT(DIST)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |        T. J. Campbell, NRL        |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         12-Nov-2010 |
!/                  +-----------------------------------+
!/
!/    30-Oct-2009 : Origination.                        ( version 3.14 )
!/    14-Jun-2010 : Fix for ACOS argument > 1.          ( version 3.14 )
!/    12-Nov-2010 : Implement r4 & r8 interfaces.       ( version 3.14 )
!/
!  1. Purpose :
!
!     Compute distance between two points.  If spherical grid, then
!     distance is the angle (in degrees) between the two points.
!     Single precision interface.
!
!  2. Method :
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!  6. Error messages :
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T8   Enables NaN check.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!/ ------------------------------------------------------------------- /
!/ Return parameter
!/
      REAL             :: DIST
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      LOGICAL, INTENT(IN) :: LLG
      REAL, INTENT(IN) :: XT, YT
      REAL, INTENT(IN) :: XS, YS
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      REAL :: DX, DY, ARGD

      REAL, PARAMETER :: PI = 3.14159265358979323846
      REAL, PARAMETER :: PI2 = 2.0*PI
      REAL, PARAMETER :: PI3H = 3.0*PI/2.0
      REAL, PARAMETER :: D2R = PI/180.0
      REAL, PARAMETER :: R2D = 1.0/D2R
      REAL, PARAMETER :: D360 = 360.0
      REAL, PARAMETER :: D270 = 270.0
      REAL, PARAMETER :: D180 = 180.0
      REAL, PARAMETER :: D90  =  90.0
      REAL, PARAMETER :: ZERO = 0.00
      REAL, PARAMETER :: ONE  = 1.00
      REAL, PARAMETER :: HALF = 0.50
!!/
!/S      INTEGER, SAVE           :: IENT = 0
!/S      CALL STRACE (IENT, 'W3DIST_R4')
!
! -------------------------------------------------------------------- /
!
!-----compute displacements
      DX = XT - XS
      DY = YT - YS

      IF ( LLG ) THEN !spherical coordinates
!---------check for longitudinal branch cut crossing
          IF ( ABS(DX) .GT. D270 ) THEN
              DX = DX - SIGN(D360,DX)
            END IF
!---------compute angular distance (min required for rare
!         situation of acos(1+small) generating NaN)
          ARGD = MIN( ONE, COS(YT*D2R)*COS(YS*D2R)*COS(DX*D2R) 
     &                     + SIN(YT*D2R)*SIN(YS*D2R) )
          DIST = R2D*ACOS( ARGD )
        ELSE !cartesian coordinates
!---------compute cartesian distance
          DIST = SQRT( DX**2 + DY**2 )
        END IF !cartesian coordinates
!/T8      IF ( W3INAN(DIST) ) THEN
!/T8          WRITE(*,'(/1A/)') 'W3DIST_R4 ERROR -- result is NaN'
!/T8          CALL EXTCDE (1)
!/T8        END IF
!/
!/ End of W3DIST_R4 -------------------------------------------------- /
!/
      END FUNCTION W3DIST
!/ ------------------------------------------------------------------- /
