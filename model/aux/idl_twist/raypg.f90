!*********************************************************************
MODULE RKPARAM          !parameters for Runge Kutta solver
   REAL,DIMENSION(3) :: yscal
   REAL h1,hmin,eps
   REAL co,freq,dl
   INTEGER,PARAMETER :: ndim=3
END MODULE RKPARAM
!*********************************************************************
MODULE BATHYGRID          !bottom topography grid
   REAL    rlonmax,rlonmin,rlatmin,rlatmax,rot
   INTEGER                          nx,ny,ip,jp
   REAL                             sx,sy,deltaz
   REAL(kind=4)         ,DIMENSION(:,:),ALLOCATABLE :: grid
   REAL                 ,DIMENSION(1,6)             :: einter
   REAL                 ,DIMENSION(1,12)            :: dinter
END MODULE BATHYGRID
!*********************************************************************
MODULE Raytraj         !bottom topography grid
   real xch(500),ych(500),ach(500)
   INTEGER nchunk
   REAL timestep,depthmin,depthmax
END MODULE Raytraj
!*********************************************************************
MODULE Constants
   REAL,    PARAMETER :: pi = 3.14159265,tpi=6.2831853,d2r=.01745329
   REAL,    PARAMETER :: max15bit=32768.,max8bit=256.,max7bit=128.
   REAL,    PARAMETER :: max14bit=16384.
   INTEGER, PARAMETER :: i15bit=32768,i8bit=256,i7bit=128,i14bit=16384
   REAL,    PARAMETER :: g =  9.81          !gravity       : SI  (m/s^2)
   REAL,    PARAMETER :: rhow =  1026.      !water density : SI  (kg/m^3)
   REAL,    PARAMETER :: kappa = 0.4        !Von Karman's constant
   REAL,    PARAMETER :: nu  = 3.E-6        !Salt water kinematic viscosity
   REAL, DIMENSION(:),ALLOCATABLE  ::invxtanhx
   INTEGER ntanh
END MODULE Constants

!**********************************************************************
SUBROUTINE tfact(depth,f,fact,fact2)
!... calculates the transformation factor (dk/dko)*(cgo/cg)
!... to avoid floating point exception in sinh, max value of sig=75
!... first get wave no. and group velocity at transformation site
   IMPLICIT NONE
   REAL,                INTENT(in)     ::depth,f
   REAL,                INTENT(out)    ::fact,fact2
   REAL d,dk,c,cg,dk0,c0,cg0
   
   CALL disp(depth,f,dk,c,cg)
   d=800.
   CALL disp(d,f,dk0,c0,cg0)
   fact=(dk/dk0)*(cg0/cg)
   fact2=(dk/cg)
   RETURN
END


!*********************************************************************
SUBROUTINE ref(xs,ys,as,iflag)
!*********************************************************************
! This routine integrates the ray equations (using the Cash-Karp Runge-Kutta
! driver RKdriver, from the initial position (xs,ys) until it crosses into a
! different zone. The subroutine returns the position and direction of the
! ray at the zone boundary.
!
! If the Fsource flag is equal to 1 then the routine also computes linear interpolation
! coefficents from the grid points. These coefficients are weighted by the propagation time.
! In order to do that the routine keeps track of the current timestep,
! direction bin and triangle. If any of these change then a new set of
! interpolation coefficients is generated. If not, the coefficients are updated
! and are given more weight. This is checked at every mini-timestep (h1) of the
! RKdriver.
!
! When the zone boundary is reached, coefficents are genrated in order 
! to be able to interpolate energy density from the neighboring grid 
! points at the end of the ray.  
!*********************************************************************
   USE BATHYGRID
   USE Constants
   USE Raytraj
   USE RKPARAM
   IMPLICIT NONE
      
   REAL,INTENT(inout)         :: xs,ys,as !start position (grid units) and angle (rad)
   INTEGER,INTENT(out)     :: iflag    !IFlag=0 hit land or cross-shelf boundary, 

   REAL x1,x2,tchunk,dt1
   REAL yrad(3)            ! current x,y, and angle
   INTEGER kchunk,nstep

      !WRITE(6,*) 'xs ....',xs,ys,as,iflag
      yrad(1)=xs
      yrad(2)=ys
      yrad(3)=as
   
      tchunk=0.
      IFlag=2
      nchunk=0
   
! Starts first chunk
      CALL RKdriver(yrad) !take a step
      dt1=h1/(60.)                 !time = time step * (C / Cg)
      tchunk=dt1
      kchunk=1
      
      DO nstep=1,100000
         CALL RKdriver(yrad) !take a step
         tchunk=tchunk+dt1
           
         
         IF (tchunk.GT.timestep) THEN     !timestep change
            nchunk=nchunk+1
            xch(nchunk)=yrad(1)/d2r
            ych(nchunk)=yrad(2)/d2r
            ach(nchunk)=yrad(3)/d2r
            tchunk=tchunk-timestep
         ENDIF

         IF (ip.lt.20.or.ip.gt.nx-20) IFlag=0
         IF (jp.lt.20.or.jp.gt.ny-20) IFlag=0
         !WRITE(6,*) 'Iflag 1:',Iflag,ip,jp,nx,ny
         IF (dl.gt.depthmax) IFlag=1
         IF (dl.lt.depthmin) IFlag=0
         IF (iflag.eq.0.or.iflag.eq.1) THEN
            !WRITE(6,*) 'Iflag:',iflag,dl,depthmin,nchunk*timestep+tchunk
            nchunk=nchunk+1
            xch(nchunk)=yrad(1)/d2r
            ych(nchunk)=yrad(2)/d2r
            ach(nchunk)=yrad(3)/d2r
            RETURN
         ENDIF
      ENDDO    !end of do loop on nstep
      print *,' error - too many steps '
      !stop   
      RETURN
      end


!**********************************************************************
SUBROUTINE RKdriver(ystart)
!     Integrates the 3 starting variables from 0 to h1
!     with accuracy eps.  h1 is a guess of the step size, hmin is the
!     min step size (0 is ok) ystart is overwritten with the desired
!     answer
!     yscal is a vector containing the typical scales of each variable
!     these scales define the admissible errors: eps*yscal(i)
!**********************************************************************
!  The Cash-Karp Runge Kutta solver is adapted to this specific problem
!  by Fabrice Ardhuin, june 1999, from Numerical Recipes, 2nd ed.
!**********************************************************************
   USE RKPARAM
   USE Raytraj
   USE Constants
   IMPLICIT NONE
   REAL,DIMENSION(ndim),INTENT(inout) ::ystart
   INTEGER,PARAMETER ::maxstp=1000
   REAL,DIMENSION(ndim) :: y,dydt
   REAL x,h,hdid,hnext
   INTEGER nstp   
   
   x=0.
   h=h1
   y(:)=ystart(:)
   DO nstp=1,maxstp
      !WRITE(6,*) 'RK driver',nstp,y(1),y(2),y(3),h
      CALL derivs(y,dydt)
      IF(x+h.gt.h1) h=h1-x       !limits step if it goes over the remaining
                                 !time h1-x
      CALL rkqs(y,dydt,h,hdid,hnext)
      x=x+hdid
      IF(x.ge.h1) THEN
         ystart(:)=y(:)
         !WRITE(6,*) 'RKdriver',x,hdid,h1,nstp,dl
         !WRITE(6,*) 'ycurr',ystart
         RETURN
      ENDIF
      h=hnext
   ENDDO
   IF (dl.lt.depthmin) THEN 
   ELSE
   WRITE(6,*) 'Step size too smal in RK driver',nstp,x,h1,hdid
   STOP
   ENDIF
   RETURN
END

!**********************************************************************
SUBROUTINE rkqs(y,dydt,htry,hdid,hnext)
!   Fifth order R-K step with monitoring of local error.  Input
!   is the n-vector of the input variable y and its derivatives
!   dydt at the starting value of x. htry is the stepsize to be
!   attempted, eps is the required accuracy and yscal is a
!   vector against which error is scaled.  On output, y and x
!   are replaced by their new values,hid is the accomplished
!   stepsize and hnext is the estimate of the next stepsize.
!**********************************************************************
   USE RKPARAM
   IMPLICIT NONE
   REAL,DIMENSION(ndim),INTENT(inout) ::y,dydt
   REAL,                INTENT(in)    ::htry
   REAL,                INTENT(out)   ::hdid,hnext
   
   REAL,PARAMETER :: pgrow=-.2
   REAL,PARAMETER :: pshrink=-.25
   REAL,PARAMETER :: safety=.9
   REAL,PARAMETER :: errcon=1.89e-4          !errcon=(5/safety)**(1/pgrow)
   REAL,DIMENSION(3):: yerr,ytemp
   REAL h,htemp,errmax
   INTEGER i,kount
   kount=0
      
   h=htry
   DO
      kount=kount+1
      CALL rkck(y,dydt,h,ytemp,yerr)
         errmax=0.
         DO i=1,ndim
            errmax=MAX(errmax,abs(yerr(i)/yscal(i)))
         ENDDO
         errmax=errmax/eps
      !WRITE(6,*) 'RKQS',errmax,kount,h
      IF(errmax.GT.1) THEN
         htemp=safety*h*(errmax**pshrink)
         h=MAX(htemp,0.1*h)                  !h should always be positive !!
                                             !I got rid of the test on xnew=x
      ELSE
         IF(errmax.gt.errcon) THEN
            hnext=safety*h*(errmax**pgrow)
         ELSE
            hnext=5.0*h
         ENDIF
         hdid=h
         y(:)=ytemp(:)
         RETURN
      ENDIF
   ENDDO
   !WRITE(6,*) 'Step size too smal in rkqs'
   RETURN
END

!**********************************************************************
SUBROUTINE rkck(y,dydt,h,yout,yerr)
!   For ndim variables y and their derivs dydt known at y, use
!   to advance solution over an interval h and return the
!   incremented variables as yout ( which may be the same as y)
!   the SUBROUTINE derivs(y,dydt) returns dydt at y
   USE RKPARAM
   IMPLICIT NONE
   REAL,DIMENSION(ndim),INTENT(in)     ::y,dydt
   REAL,                INTENT(in)     ::h
   REAL,DIMENSION(ndim),INTENT(out)    ::yout,yerr
   
   REAL,DIMENSION(ndim) ::    ak2,ak3,ak4,ak5,ak6,ytemp
   REAL,PARAMETER       ::    A2=.2,A3=.3,A4=.6,A5=1.,A6=.875
   REAL,PARAMETER       ::    B21=.2,B31=.075,B32=9./40.
   REAL,PARAMETER       ::    B41=.3,B42=-.9,B43=1.2
   REAL,PARAMETER       ::    B51=-11./54.,B52=2.5,B53=-70/27.,B54=35./27.
   REAL,PARAMETER       ::    B61=1631./55296.,B62=175./512.,B63=575./13824.
   REAL,PARAMETER       ::    B64=44275./110592.,B65=253./4096.
   REAL,PARAMETER       ::    C1=37./378.,C3=250./621.,C4=125./594.,C6=512./1771.
   REAL,PARAMETER       ::    DC1=C1-2825./27648.,DC3=C3-18575./48384.
   REAL,PARAMETER       ::    DC4=C4-13525/55296.,DC5=-277/14336,DC6=C6-.25
   
   ytemp(:)=y(:)+B21*h*dydt(:)
   CALL derivs(ytemp,ak2)
   ytemp(:)=y(:)+h*(B31*dydt(:)+B32*ak2(:))
   CALL derivs(ytemp,ak3)
   ytemp(:)=y(:)+h*(B41*dydt(:)+B42*ak2(:)+B43*ak3(:))
   CALL derivs(ytemp,ak4)
   ytemp(:)=y(:)+h*(B51*dydt(:)+B52*ak2(:)+B53*ak3(:)+B54*ak4(:))
   CALL derivs(ytemp,ak5)
   ytemp(:)=y(:)+h*(B61*dydt(:)+B62*ak2(:)+B63*ak3(:)+B64*ak4(:)+B65*ak5(:))
   CALL derivs(ytemp,ak6)
   yout(:)=y(:)+h*(C1*dydt(:)+C3*ak3(:)+C4*ak4(:)+C6*ak6(:))
   yerr(:)=h*(DC1*dydt(:)+DC3*ak3(:)+DC4*ak4(:)+DC6*ak6(:))
   RETURN
END

!**********************************************************************
SUBROUTINE derivs(y,dydt)
!     c=phase speed and dydt are the derivatives of the n vectors
!     with respect to time.
!**********************************************************************
   USE RKPARAM
   USE BATHYGRID
   IMPLICIT NONE
   REAL,DIMENSION(ndim),INTENT(in) ::y
   REAL,DIMENSION(ndim),INTENT(out)::dydt
   REAL mu,nu,sp
   REAL xg,yg,cg,depth,dcdx,dcdy,ca,sa,sa2 
   
   xg=y(1)
   yg=y(2)
   CALL speed(xg,yg,cg,depth,dcdx,dcdy)
   ca=cos(y(3))
   sa=sin(y(3))
   !dydt(3)=cg*(sa*dcdx/sx-ca*dcdy/sy)
   
   !spherical earth
   mu=6378100. !earth radius in meters
   nu=6378100.
   
   !cos(90-y)=-sin(y)
   !sin(90-y)=cos(y)
   sp=cos(y(2))
   dydt(1)=cg*ca/(nu*sp)   ! x and y are the lon,lat in radians
   dydt(2)=cg*sa/mu
   sa2=ca
   dydt(3)=cg*(sa*dcdx/(sx*nu*sp)-ca*dcdy/(sy*mu)-sa2*sin(y(2))/(nu*sp))

   RETURN
END


!**********************************************************************
SUBROUTINE speed(lon,lat,cg,depth,dcdx,dcdy)
!**********************************************************************
!     Wave speed gradients, from Dobson (1967), for not-too-shallow water
!     Code by William C. O'Reilly (1990-1998)
!     Modified by Fabrice Ardhuin (1999) to accomodate any depth.
!     Modified .................. 11/8/2000 with a Newton-Rapson method for
!                                 the inversion of the dispersion relation
!**********************************************************************
!     uses biquadric interpolation (bilinear would not be smooth)
!... calculates the depth, wave phase speed and gradients
!  Positions of the grid points (+) used in the computations
!  The current point xg,yg (*) is located in the [1,2,3,4] rectangle
!                  + 8   + 7
!
!            + 9   + 4   + 3   + 6
!                    *
!            + 10  + 1   + 2   + 5
!
!                  + 11  + 12
!**********************************************************************
   USE BATHYGRID
   USE Constants
   USE RKparam
   IMPLICIT NONE
   REAL,                INTENT(in)     ::lon,lat
   REAL,                INTENT(out)    ::cg,depth,dcdx,dcdy
   
   REAL,DIMENSION(12,6),SAVE :: sxy=reshape((/ 0.30861241,0.23684207,0.21770331, &
      0.23684207,-0.08492823,-0.05143541,-0.05143541,-0.08492823,0.00598086, &
      0.13038277,0.13038277,0.00598086, &
      0.05322964,0.19677030,0.14413872,0.10586122,0.09031100,-0.06758374, &
      -0.03349283,0.03349282,-0.18241626,-0.34031099,-0.12440190,0.12440190, &
      0.05322964,0.10586122,0.14413872,0.19677030,0.03349282,-0.03349283, &
      -0.06758374,0.09031099,0.12440190,-0.12440191,-0.34031099,-0.18241625, &
      -0.125,-0.125,-0.125,-0.125,0.125,0.125, &
      0.,0.,0.125,0.125,-0.,-0., &
      0.05263157,-0.05263157,0.05263158,-0.05263157,-0.15789473,0.15789474, &
      0.15789474,-0.15789473,-0.15789473,0.15789473,0.15789473,-0.15789473, &
      -0.125,-0.125,-0.125,-0.125,0.,0., &
      0.125,0.125,-0.,-0.,0.125,0.125 /),(/12,6/))
      
      
   INTEGER i,j
   REAL X,Y,sigma,dy,k,c
   REAL xg,yg,xp,yp,sig,aka,yhat,t,a,dddx,dddy,dcdd
   
   xg=MOD(lon-rlonmin*d2r,tpi)/sx
   yg=(lat-rlatmin*d2r)/sy
   !WRITE(6,*) 'Xg...:',lon,lon-rlonmin*d2r,lat,xg,yg,sx,sy
   i=INT(xg)
   j=INT(yg)
   xp=mod(xg,1.0)
   yp=mod(yg,1.0)
   IF(ip.ne.i.OR.jp.NE.j) THEN
      ip=i
      jp=j
      dinter(1,11)=grid(i,j-1)
      dinter(1,12)=grid(i+1,j-1)
      dinter(1,10)=grid(i-1,j)
      dinter(1,1)=grid(i,j)
      dinter(1,2)=grid(i+1,j)
      dinter(1,5)=grid(i+2,j)
      dinter(1,9)=grid(i-1,j+1)
      dinter(1,4)=grid(i,j+1)
      dinter(1,3)=grid(i+1,j+1)
      dinter(1,6)=grid(i+2,j+1)
      dinter(1,8)=grid(i,j+2)
      dinter(1,7)=grid(i+1,j+2)
      einter=MATMUL(dinter,sxy)
   ENDIF
   depth=einter(1,1)+(einter(1,2)+einter(1,4)*xp+einter(1,5)*yp)*xp &
         +(einter(1,3)+einter(1,6)*yp)*yp

!... adjust water level here
   depth=depth+deltaz

   IF(depth.lt.1.) depth=1.
   dl=depth
!... gradients of bottom depths
   dddx=einter(1,4)*2.*xp+einter(1,5)*yp+einter(1,2)
   dddy=einter(1,6)*2.*yp+einter(1,5)*xp+einter(1,3)
!... calculate phase speed gradient with depth 
!... (accurate in Shallow Water where Dobson was not)
   sigma=freq*tpi
   Y=(sigma**2*DEPTH)/G
      IF (Y.GE.14.5) THEN 
         X=Y 
      ELSE
         I=INT(Y*REAL(ntanh)/15.)
      	dy=mod(Y*REAL(ntanh)/15.,1.0)
         X=(invxtanhx(I)*(1-dy)+invxtanhx(I+1)*dy)
      ENDIF
      k=X/DEPTH
      c=sigma/k
      cg=g*((y/x)+x*(1-(y/x)**2))/(2.*sigma)
                           !dk=aka/depth
                           !cg=tpi*freq/dk
                           !rc=c/co
	                        ! old formula: Dobson
                           !dcdd=sig*(1.-rc*rc)/(c*rc+sig*depth*(1-rc*rc))
      ! the following formula is exaclty equivalent to Dobson in deep water
      ! but it is accurate in shallow water
      dcdd=(1-y/(y+x**2-y**2))/depth  !=(dc/dh)/c 
      dcdx=dcdd*dddx
      dcdy=dcdd*dddy
      !WRITE(6,*) 'SPEED',X,Y,I1,xinvxtanh(I1+1),k,c
      !WRITE(6,*) 'SPEED',xg,yg,cg,depth,dcdx,dcdy
      RETURN
      END

!*********************************************************************
	SUBROUTINE DISP(depth,freq,k,c,cg)
!*********************************************************************
   USE Constants
   IMPLICIT NONE
   REAL,                INTENT(in)     ::depth,freq  !depth (m) and frequency (Hz)
   REAL,                INTENT(out)    ::k,c,cg      !wave number (m-1),phase speed (m/s)
   REAL dep,sigma,X,Y,dy
   INTEGER I
   
   dep=depth
   IF (depth.LT.1) dep=1.
   sigma=freq*tpi
	Y=(sigma**2*DEP)/G
   IF (Y.GE.14.5) THEN 
      X=Y 
   ELSE
      I=INT(Y*REAL(ntanh)/15.)
   	dy=mod(Y*REAL(ntanh)/15.,1.0)
      X=invxtanhx(I)*(1-dy)+invxtanhx(I+1)*dy
   ENDIF
   k=X/DEP
   c=sigma/k
   cg=g*((y/x)+x*(1-(y/x)**2))/(2.*sigma)
   
   RETURN
	END


!*********************************************************************
	SUBROUTINE TABULATE
!*********************************************************************
   USE Constants
   USE RKPARAM
   IMPLICIT NONE

   REAL X,Y,H,F,FD
   INTEGER I,J
   ntanh=10000
   ALLOCATE(invxtanhx(0:ntanh))
   DO J=0,ntanh
      Y=15.*REAL(J)/ntanh
      X=Y
   	F=1.
      DO 10 I=1,100
	   	H=TANH(X)
		   F=Y-X*H
         IF (ABS(F) .LT. 1.E-6) GO TO 20
	   	FD=-H-X/COSH(X)**2
   		X=X-F/FD
10	   CONTINUE
   	PRINT *,' SUBROUTINE DISP DOES NOT CONVERGE !!! '
20	   invxtanhx(J)=X
      ENDDO
      RETURN
	   END

!**********************************************************************
!**********************************************************************
!**********************************************************************
!**********************************************************************
PROGRAM rayplot
   USE BATHYGRID
   USE Constants
   USE Raytraj
   USE RKPARAM
   IMPLICIT NONE
      
   REAL dtheta,theta0,xs,ys,sig,dw,dk,c,cg,dto,h_len2
   REAL h_len,dxy,dcdx,dcdy,a_step,ang,angs
   INTEGER I,J,nrays,Iray,k,ircode
   CHARACTER(len=50):: fblog,fbathy

   CALL Tabulate

      
      OPEN(3,file='Ray3a.inpray',status='old')
      READ(3,'(a)') fblog
      READ(3,'(a)') fbathy
      READ(3,*) nrays
      READ(3,*) freq,dtheta,theta0,timestep,depthmax,depthmin
      READ(3,*) deltaz
      READ(3,*) xs,ys
      CLOSE(3)

      h_len=200
      eps=0.0001    ! error tolerance in ray integration

!-------------------------------------------------------------------
!                 Get bathymetry grid data
!-------------------------------------------------------------------
      open(2,file=fblog)
      read(2,*) rlatmin,rlatmax,rlonmin,rlonmax
      read(2,*) rot
      read(2,*) sx,sy
      read(2,*) nx,ny 
      close(2)
      yscal(1)=REAL(nx)
      yscal(2)=REAL(ny)
      yscal(3)=1.
      
      sx=(rlonmax-rlonmin)*d2r/(nx-1)
      sy=(rlatmax-rlatmin)*d2r/(ny-1)
      WRITE(6,*) 'sx:',sx,sy
!                 Get the grid itself

      xs=MOD(360+xs+rlonmin,360.)-rlonmin
      WRITE(6,*) 'lon,lat:',xs,ys
      
   ALLOCATE(grid(nx,ny))
      open(2,file=fbathy,form='unformatted',access='direct',recl=4*ny)
      DO i=1,nx !beware 1 should be replaced by 4 on HP terminals
         read(2,rec=i) (grid(i,j),j=1,ny)
      ENDDO
      CLOSE(2)

      sig=2.*pi*freq

      dw=8000.
      call disp(dw,freq,dk,c,cg)    !calculate deep water phase speed

      dto=timestep*60.  !converts the time step to seconds
      h_len2=cg*dto*0.1 !lengthen the steps for long output timestep
      IF (h_len2 > h_len) h_len=h_len2

      co=c
      hmin=0.
      h1=h_len/co                !calculate initial ray time steps, h1
                                          ! (major factor in computation time)
      xs=xs*d2r
      ys=ys*d2r
      CALL speed(xs,ys,c,dxy,dcdx,dcdy)
      WRITE(6,*) 'speed',xs,ys,dxy,c,dcdx,dcdy
      a_step=dtheta
      !OPEN(995,file='rays.bin',status='unknown',form='unformatted')
      OPEN(995,file='rays.asc',status='unknown')
      ang=theta0-a_step
      DO IRAY=1,nrays
         ang=ang+a_step
         angs=ang*d2r                  !transforms to radians
         CALL ref(xs,ys,angs,ircode)
      WRITE(6,*) 'ray:',IRAY,xs/d2r,ys/d2r,ang,ircode,nchunk,xch(nchunk),ych(nchunk)
     !    write(995) nchunk,ircode
     !   write(995) (xch(k),k=1,nchunk),  &
     !   (ych(k),k=1,nchunk),(ach(k),k=1,nchunk)
         write(995,*) nchunk,ircode
         write(995,*) (xch(k),k=1,nchunk),  &
        (ych(k),k=1,nchunk),(ach(k),k=1,nchunk)
      ENDDO
      CLOSE(995)
      END
