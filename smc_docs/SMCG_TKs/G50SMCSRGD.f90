!  This module is a common block similar in all AFT Model programs and is
!  written in FORTRAN 90.
!                     J G Li   26 Oct 2000
!!
!! Adapted for multiple cell 2D advection tests using UNO schemes.
!!                    J G Li    8 Aug 2007
!! Reformated for global multiple cell advection tests.
!!                    J G Li   16 Nov 2007
!! Modified for extended global SMC grid advection tests.
!!                    J G Li   28 Nov 2008
!! Modified for Arctic 2-D spectral transportation tests.
!!                    J G Li   16 Jul 2009
!! Adapted for UK3km multi-resolution grid spectral transport.
!!                    J G Li    8 Feb 2010
!! Changed for multi-step, multi-resolution G6kSMC grid transport.
!!                    J G Li   23 Feb 2010
!! Adapted for 2-part, 4-step, 8-resolution G6kSMC grid spectral transport.
!!                    J G Li    5 Mar 2010
!! Add great circle turning term in G6kSMC grid spectral transport.
!!                    J G Li   22 Mar 2010
!! Add refraction term and use shallow water wave speed. 
!!                    J G Li   26 Mar 2010
!! Modify refraction term with rotation sub.
!!                    J G Li   18 May 2010
!! Add diffusion term in advection subs.
!!                    J G Li    3 Jun 2010
!! Adapted for G25SMC grid wave propagation test.
!!                    J G Li   14 Oct 2010
!! New AngleD program for SMC grid only. 
!!                    J G Li   21 Apr 2010
!! Test Gulf of Mexico area for high coastal waves. 
!!                    J G Li    4 May 2010
!! Adapted for SMC50 grid global + Arctic test. 
!!                    J G Li   26 Oct 2011
!!
                      
      MODULE Constants
          IMPLICIT NONE

! Parameters fixed in the program
     INTEGER,PARAMETER::NCL=112000, NFC=112000, NBDY=510, NDIR=36,  &
                    &   NLat= 384, NLon=512, NLat2=NLat/2

     REAL,PARAMETER:: PoLAT= 0.0, PoLON=-180.0, DLON=0.7031250, DLAT=0.4687500,  &
        &             ZrLAT=-90.0, ZrLON=-DLON*0.5, Agu36=4.8481E-5, Frqcy=0.0625,   &
        &             Pie=3.141592654, RAD2D=180.0/Pie, D2RAD=Pie/180.0

     REAL,PARAMETER:: DT=900.0, DTR=1.0/DT, DTF=6.0, AKH=9000.0

!  Some physical and atmospheric constants
       REAL,PARAMETER:: GRVTY=9.806,CPVAP=1004.5,RDRY=287.05, &
        &    CT0=273.16,CALJO=4.1868,PATM=101325.0,ANGUL=7.2921E-5,  &
        &    EPSLN=0.6220,CLIGHT=2.99792458E8, GeoPie=3.141592654,   &
        &    REARTH=6.371E6

! Array variables to be used for data storage
       REAL::  AMG, CMX, CTT, UMX, DY, DYR, DX0, DThta, SWH0, Alpha
       REAL::  AKHDT2, CGCMX, CRFMX
       REAL, DIMENSION(-9:NCL):: A, C, D, F, AU, AV, DX, DXR, UC, VC, RCELA
       REAL, DIMENSION(-9:NCL):: HCel, DHDX, DHDY, REFR, CGrp
       REAL, DIMENSION( NBDY ):: MBGlo, MBArc
       REAL, DIMENSION( NDIR ):: Theta, Spectr, CSeta, SNeta, SpeGCT
       REAL, DIMENSION(NFC)::   U, V, FU, FV, FX, FY, AngU, AngV, AngCD, CoGCT
       REAL, DIMENSION(NDIR,NFC)::   UDBn, VDBn
       REAL, DIMENSION(NDIR,NCL)::   UCBn, VCBn, CDBn, CoRfr
       REAL, DIMENSION(-NLat2:NLat2)::  YLat, CSLat, CCLat, DXLat, TnLat 

       INTEGER:: NU, NV, NC, NS, NT, ND, NB, N1, N2, N4, N8, N9
       INTEGER:: NU1, NV1, NU2, NV2, NU4, NV4, NU8, NV8, NU9, NV9
       INTEGER:: NGLo, NGLA, NGLB, NArc, NArA, NArB, NUGL, NUAr, NVGL, NVAr 
       INTEGER:: ICE(4,-9:NCL), KG(NCL), NTS, NWP
       INTEGER, DIMENSION(7,NFC)::  ISD
       INTEGER, DIMENSION(8,NFC)::  JSD
       INTEGER:: I,II,IJ,IJK,J,JJ,JK,K,KK,L,LL,LM,LMN,M,MM,MN,N,NN
       CHARACTER(LEN=9):: FL9NM='Cn10000.d'

!      Date and time for timing of program by calling Date_And_Time
       CHARACTER(LEN=10):: CDate, CTime

! Initialised variables

       CHARACTER(LEN=1)::XEXT(6)=(/'S','B','W','E','C','M'/)

       CHARACTER(LEN=16):: RUNDATE=' SMC  3 Jun 2010'

      END MODULE Constants

!!
!! Adapted for multiple cell 2D advection tests using UNO schemes.
!!                    J G Li   26 Jul 2007
!!
!! Adapted for global multiple cell advection tests using UNO2 scheme.
!!                    J G Li   22 Nov 2007
!!
!! Modified for global SMC grid extended to cover the N Pole.
!!                    J G Li   26 Nov 2008
!!
!! Adapted for 2-part, 4-step, 8-resolution G6kSMC grid spectral transport.
!!                    J G Li    5 Mar 2010
!!
!! Adapted for G25SMC grid wave propagation test.
!!                    J G Li   13 Oct 2010
!!

      PROGRAM GMC2Dvct 
      USE constants
      IMPLICIT NONE

       REAL:: CNST, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6, CNST8


!  Read Global and Arctic part Multiple-Cell info

       OPEN(UNIT=8,FILE='DatGMC/G50SMCels.dat',STATUS='OLD',IOSTAT=nn,ACTION='READ')
       IF(nn /= 0) PRINT*,' File G50SMCels.dat was not opened! '
          READ (8,*) NGLo, N1, N2, N4
       DO J=1,NGLo
          READ (8,'(2i6,2i4,i6)') ICE(1,J), ICE(2,J), ICE(3,J), ICE(4,J), KG(J)
       END DO
       CLOSE(8)
       PRINT*, ' G50SMCels.dat read done ', NGLo, N1, N2, N4

       OPEN(UNIT=9,FILE='DatGMC/G50SMCBAr.dat',STATUS='OLD',IOSTAT=nn,ACTION='READ')
       IF(nn /= 0) PRINT*,' File G50SMCBAr.dat was not opened! '
          READ (9,*) NArc, NArB, NGLB
       DO J=NGLo+1, NGLo+NArc
          READ (9,'(2i6,2i4,i6)') ICE(1,J), ICE(2,J), ICE(3,J), ICE(4,J), KG(J)
       END DO
       CLOSE(9)
       PRINT*, ' G50SMCBAr.dat read done ', NArc, NGLB, NArB 

!  Total cell number will be sum of two parts
       NC = NGLo + NArc

!!   Set boundary cell counts.  Boundary cells for the global part are at the end
!!   of G25SMCels.dat and for the Arctic part at the start of G25SMCBAr.dat.
!!   Boundary cell will then from NGLo-NGLB+1 to NGLo for lower part and NGLo+1 to NGLo+NArB
!!   NGLA and NArA are the extra numbers to be added for boundary loop 1, NGLB and 1, NArB
       NGLA=NGLo-NGLB
       NArA=NGLo

!  Output a few to check input values
       DO J=1, NC, 10000
          WRITE(6,'(i8,2i6,2i4,i6)') J, ICE(1,J), ICE(2,J), ICE(3,J), ICE(4,J), KG(J)
       END DO

!!   Match global boundary cells with Arctic inner cells
       DO i=1, NGLB
          ii=i+NGLA
!!   Search arctic part cells to match global part boundary cells
          mm=0
          DO k=NGLo+NArB+1, NC-1
             IF(ICE(1,ii) .EQ. ICE(1,k) .AND. ICE(2,ii) .EQ. ICE(2,k)) THEN
                MBGLo(i)=k
                mm=1
             ENDIF
          ENDDO
          IF( mm .EQ. 0 ) PRINT*,' Miss global part boundary cell i=',i
       ENDDO

!!   Match Arctic boundary cells with global inner cells
      DO i=1, NArB
          ii=i+NArA
!!   Search global part to match arctic part boundary cells
          mm=0
          DO k=1, NGLA
             IF(ICE(1,ii) .EQ. ICE(1,k) .AND. ICE(2,ii) .EQ. ICE(2,k)) THEN
                MBArc(i)=k
                mm=1
             ENDIF
          ENDDO
          IF( mm .EQ. 0 ) PRINT*,' Miss Arctic part boundary cell i=',i
       ENDDO
       PRINT*, ' Boundary cells matched for', NGLB, NArB


!    Boundary -9 to 0 cells for cell size 2**n
!    Note the position indice for bounary cell are not used.
       ICE(1,-9:0)=0
       ICE(2,-9:0)=0
       ICE(3,   0)=1
       ICE(4,   0)=1
       DO i=1,9
          ICE(3,-i)=ICE(3,-i+1)*2
          ICE(4,-i)=1
!         ICE(4,-i)=ICE(3,-i)
       ENDDO

!!   Evaluate dx length along latitude in rad
       DX0=DLON*D2RAD
       DO n=-NLat2, NLat2
          YLat(n)=Real( n )*DLAT
          TnLat(n)=TAN(  YLat(n)*D2RAD )
          CSLat(n)=COS(  YLat(n)*D2RAD )
          CCLat(n)=COS( (YLat(n)+0.5*DLAT)*D2RAD )
          DXLat(n)=CCLat(n)*DX0
       ENDDO
     
!!   Evaluate cell dx length in rad and cell area in rad**2
!!   except for the polar (last) cell.
       DY=DLAT*D2RAD
       DYR=1.0/DY
       DO L=-9,NC-1
          J=ICE(2,L)
          DX(L)=DXLat(J)*Real( ICE(3,L) )
          DXR(L)=1.0/DX(L)
          RCELA(L)=1.0/Real( ICE(3,L)*ICE(4,L) )
       ENDDO

!!  North Polar cell NC is a round cell of radius DY*ICE(4,NC), 
!!  equivalent DX=Area/(DY*ICE(4,NC))
!!  The RCELA(NC) represent the net V-flux factor for the polar cell
!!  CSLat will be cancelled as all cell update are divided by the factor.
!!  As polar cell does not need this factor, the factor is included in RCELA
!!  so that it will be cancelled at update when it is divided by the factor.
          DX(NC)=Real(ICE(4,NC))*DY*Pie
          DXR(NC)=1.0/DX(NC)
          RCELA(NC)=DXR(NC)*DX0*CSLat(ICE(2,NC)+ICE(4,NC)/2)/Real( ICE(4,NC) )

!!  Read sorted ISD JSD variables for global part.
      OPEN(UNIT=10,FILE='DatGMC/G50GISide.dat',STATUS='OLD',IOSTAT=nn,ACTION='READ')
      IF(nn /= 0) PRINT*,' File DatGMC/G50GISide.d was not opened! '
!     READ(10,*) NUGL, NU9, NU8, NU4, NU2, NU1
      READ(10,*) NUGL 
      WRITE(6,*) " Read u face numbers NU =", NUGL
!     WRITE(6,*) " Read u face numbers NU, NU9, NU8, NU4, NU2, NU1 ="
!     WRITE(6,*)                       NUGL, NU9, NU8, NU4, NU2, NU1
      DO I=1,NUGL
         READ(10,FMT='(2i6,i4,4I8)')  (ISD(N,I), N=1,7)
      END DO
      CLOSE(10)

      OPEN(UNIT=11,FILE='DatGMC/G50GJSide.dat',STATUS='UNKNOWN',IOSTAT=nn,ACTION='READ')
      IF(nn /= 0) PRINT*,' File DatGMC/G50GJSide.d was not opened! '
!     READ(11,*) NVGL, NV9, NV8, NV4, NV2, NV1
      READ(11,*) NVGL
      WRITE(6,*) " Read v face numbers NV =", NVGL
!     WRITE(6,*) " Read v face numbers NV, NV9, NV8, NV4, NV2, NV1 ="
!     WRITE(6,*)                       NVGL, NV9, NV8, NV4, NV2, NV1
      DO J=1,NVGL
         READ(11,FMT='(2i6,i4,4I8,i4)')  (JSD(N,J), N=1,8)
      END DO
      CLOSE(11)

!!  Read sorted ISD JSD variables for Arctic part.
      OPEN(UNIT=10,FILE='DatGMC/G50AISide.dat',STATUS='OLD',IOSTAT=nn,ACTION='READ')
      IF(nn /= 0) PRINT*,' File DatGMC/G50AISide.d was not opened! '
      READ(10,*) NUAr
      WRITE(6,*) " Read u face numbers NUAr =", NUAr
      DO I=1,NUAr
         READ(10,FMT='(2i6,i4,4I8)')  (ISD(N,I+NUGL), N=1,7)
      END DO
      CLOSE(10)

      OPEN(UNIT=11,FILE='DatGMC/G50AJSide.dat',STATUS='UNKNOWN',IOSTAT=nn,ACTION='READ')
      IF(nn /= 0) PRINT*,' File DatGMC/G50AJSide.d was not opened! '
      READ(11,*) NVAr
      WRITE(6,*) " Read v face numbers NVAr =", NVAr
      DO J=1,NVAr
         READ(11,FMT='(2i6,i4,4I8,i4)')  (JSD(N,J+NVGL), N=1,8)
      END DO
      CLOSE(11)

!!  Set total face nubmers
      NU=NUGL+NUAr
      NV=NVGL+NVAr

!!  Reset arctic part cell numbers in I/JSD by adding NGLo for positive cells only.
!!  The 0 and negative cells for boundary useage will be shared by the two parts.
      DO I=NUGL+1, NU
         DO M=4,7
            IF(ISD(M,I) > 0) ISD(M,I)=ISD(M,I)+NGLo
         END DO
      END DO

      DO J=NVGL+1, NV
         DO M=4,7
            IF(JSD(M,J) > 0) JSD(M,J)=JSD(M,J)+NGLo
         END DO
      END DO

      WRITE(6,*) " Arctic u v face cell values have been adjusted."

!!  Directional bins in rad, shifted by half-width from axises
        DThta=2.0*Pie/FLOAT(NDIR)
      DO K=1, NDIR
         Theta(K)=(FLOAT(K) - 0.5)*DThta
         CSeta(K)=COS(Theta(K))
         SNeta(K)=SIN(Theta(K))
      ENDDO

!!  Convert diffusivity into radian^2 s-1 and multiply by 2*DT
!!  The 2.0 factor is added to cancel the 0.5 factor in the gradient.
        AKHDT2 = 2.0*AKH*DT/(REARTH*REARTH)

!!  Calculate bathymetry gradient DHDX/DY and group speed
        CALL CGDHDXDY

!   Whole array assignment for i=-8,NCL
        C=0.0

!!  Input selection for rotation or deformation test
!100    PRINT*,'Please enter 0 for rotation or 1 for deformation:'
!       READ(5,*) NRoDfm
!       NRoDfm=0

!!  Specify run time steps and writeup interval
!       NTS= 2160
        NTS= 1200
        NWP= 40
        WRITE(6,*) ' Run/writeup length NTS NWP=', NTS, NWP

!  Initialise C, U, V, UC, VC 
        NT=0

!  Generate flux face and cell rotation Angle for Arctic cells
        CALL SideWind

!  Initialise wave spectra and bin velocities,
!  including GCT and refraction Courant numbers.
        CALL SPECUUVV

!  Read restart time step or set default = 0
!  Restart from NS > 0 overwrite C
!       WRITE(6,*) " Enter restart time step NS, default 0 "
!       READ(5,*)  NS
!       IF (NS .LT. 0) NS=0
        NS = 0
        WRITE(6,*) " Restart time step NS set to be ", NS

!  Save a copy of initial values at writeup time step.
        NT=NS
        write(FL9NM(3:7), FMT='(i5)' )  10000+NT
        OPEN(UNIT=26, FILE=FL9NM, STATUS='NEW',IOSTAT=nn)
        IF(nn /= 0) PRINT*,' File FL9NM was not opened! '
        WRITE(UNIT=6,FMT='(2x,"NT= ",i6,3x,A9)') NT,FL9NM

!!   Filter very small C(n) value so it is greater than E-90
           ii=0
        DO i=1, NC
!! Skip the overlapping boundary cells
        IF( i .LE. NGLo-NGLB .OR. i .GT. NGLo+NArB ) THEN
               ii=ii+1
           IF( Abs(C(i)) .LT. 1.0E-90 ) THEN
               D(ii)=SIGN(1.0E-90, C(i))
           ELSE
               D(ii)=C(i)
           ENDIF
        ENDIF
        ENDDO

!    Central basic cells only, all at end of cell array
        WRITE(UNIT=26, FMT='(2x,2i8)' )  NT, ii
        WRITE(UNIT=26, FMT=7113)  (D(n), n=1,ii)

        CLOSE(26)


!     Open files to store writups
       OPEN(UNIT=16,FILE='CMesgs.txt',STATUS='UNKNOWN',IOSTAT=nn, &
        &           ACTION='WRITE')
       IF(nn /= 0) PRINT*,' File CMesgs.txt was not opened! '

!     Header messages and configuration information 
       WRITE(UNIT=16,FMT='(1x/   &
        &  "  Global Multiple-Cell 2-D Spherical Advection Model" /    &
        &  "         SMC Version 2.0   J G  Li  Mar 2010  " /)' )

       CALL DATE_AND_TIME(CDate, CTime)
       WRITE(UNIT=16,FMT="(1x,' Run time date ',A10,2x,A10)") CTime, CDate

       WRITE(UNIT=16,FMT='(1x," Size-1 Units DLON DLAT = ",2f14.10)')  DLON, DLAT
       WRITE(UNIT=16,FMT='(1x," Equatorial PoLat PoLon = ",2f8.2)')  PoLat, PoLon
       WRITE(UNIT=16,FMT='(1x," Standard grid ZrLatLon = ",2f9.5)')  ZrLat, ZrLon
       WRITE(UNIT=16,FMT='(1x," Angular speed Eq Agu36 = ",ES12.3)') Agu36
       WRITE(UNIT=16,FMT='(1x," Horizontal diffusivity = ",f8.1)' )  AKH
       WRITE(UNIT=16,FMT='(1x," Basic time step DT (s) = ",f8.1)' )  DT
       WRITE(UNIT=16,FMT='(1x," Maximum grid speed s-1 = ",ES12.3)') UMX
!      WRITE(UNIT=16,FMT='(1x," Maxm group speed m s-1 = ",f8.3)' )  MAX(CGrp(1:NGLo))
       WRITE(UNIT=16,FMT='(1x," Maximum Courant number = ",f8.3)' )  CMX
       WRITE(UNIT=16,FMT='(1x," Max GCT Courant number = ",f8.3)' )  CGCMX
       WRITE(UNIT=16,FMT='(1x," Max Rfr Courant number = ",f8.3)' )  CRFMX
!      WRITE(UNIT=16,FMT='(1x," Max bathy grad DH/DXDY = ",2f8.3)' ) MAX(ABS(DHDX(1:NGLo))), MAX(ABS(DHDY(1:NGLo)))
       WRITE(UNIT=16,FMT='(1x," Initial integrated SWH = ",f8.3)' )  SWH0
       WRITE(UNIT=16,FMT='(1x," Wave Frequency FrqcyHz = ",f8.4)' )  Frqcy
       WRITE(UNIT=16,FMT='(1x," Total time step no NTS = ",i8)' )  NTS
       WRITE(UNIT=16,FMT='(1x," Restart time step   NS = ",i8)' )  NS
       WRITE(UNIT=16,FMT='(1x," Writeup timestep every = ",i8)' )  NWP
       WRITE(UNIT=16,FMT='(1x," Horizontal cell number = ",2i8)')  NC, NCL
       WRITE(UNIT=16,FMT='(1x," Globl/Arctic cell No.s = ",2i8)')  NGLo, NArc
       WRITE(UNIT=16,FMT='(1x," Globl/Arctic bndy No.s = ",2i8)')  NGLB, NArB
       WRITE(UNIT=16,FMT='(1x," Total number of U-face = ",2i8)')  NU, NFC
       WRITE(UNIT=16,FMT='(1x," Globl/Arctic U-face No = ",2i8)')  NUGL, NUAr
       WRITE(UNIT=16,FMT='(1x," Total number of V-face = ",i8)' )  NV
       WRITE(UNIT=16,FMT='(1x," Globl/Arctic V-face No = ",2i8)')  NVGL, NVAr
       WRITE(UNIT=16,FMT='("Global N9,    N8,    N4,    N2,    N1")')
       WRITE(UNIT=16,FMT='(1x,5i8)')  N9, N8, N4, N2, N1 
!      WRITE(UNIT=16,FMT='("Global NU9,   NU8,   NU4,   NU2,   NU1")')
!      WRITE(UNIT=16,FMT='(1x,5i8)')  NU9, NU8, NU4, NU2, NU1 
!      WRITE(UNIT=16,FMT='("Global NV9,   NV8,   NV4,   NV2,   NV1")')
!      WRITE(UNIT=16,FMT='(1x,5i8)')  NV9, NV8, NV4, NV2, NV1 

 3912 FORMAT(1x,i4,3F9.1,ES12.3)

      WRITE(16,FMT='(/1x," YLat CSLat at step of  ",i6)' )  10
      WRITE(16,FMT='(8F9.3)')  ( YLat(n), n=-NLat2,NLat2,10)
      WRITE(16,FMT='(8F9.5)')  (CSLat(n), n=-NLat2,NLat2,10)
      WRITE(16,FMT='(/1x," First and last ICE values ")' ) 
      WRITE(16,FMT='(1x,6i8)') (ICE(i,1),i=1,3), (ICE(i,NC),i=1,3)
      WRITE(16,FMT='(/1x," First and last ISD values ")' ) 
      WRITE(16,FMT='(1x,8i8)')  1,(ISD(i, 1),i=1,7)
      WRITE(16,FMT='(1x,8i8)') NU,(ISD(i,NU),i=1,7)
      WRITE(16,FMT='(/1x," First and last JSD values ")' ) 
      WRITE(16,FMT='(1x,8i8)')  1,(JSD(i, 1),i=1,7)
      WRITE(16,FMT='(1x,8i8)') NV,(JSD(i,NV),i=1,7)
      WRITE(16,FMT='(1x,8i8)') 

       CALL DATE_AND_TIME(CDate, CTime)
       WRITE(6,"(' Loop start time ',A,'  ',A)")  CTime

!     Start of major time step loop
! TSLoop:  DO  NT=NS, NS+NTS-1, 4
 TSLoop:  DO  NT=NS+1, NS+NTS 

!!    36 directional bin loop
  DirLop:  DO  ND=1, NDIR

!!    Assign CDBn(ND,1:NC) to C(1:NC)
         C(1:NC)=CDBn(ND,1:NC)

!!    Assign U/VDBn(ND,1:NU/V) to U/V(1:NU/V)
         U(1:NU)=UDBn(ND,1:NU)
         V(1:NV)=VDBn(ND,1:NV)
         UC(1:NC)=UCBn(ND,1:NC)
         VC(1:NC)=VCBn(ND,1:NC)

!! Reset net flux arrays for main step
           F  = 0.0
           AU = 0.0
           AV = 0.0

!! Sub-stepping for different sizes of cells
!       DO  NB=1, 4
!  Call subroutines to calculate advection
!  Call GMCxUNO2 to calculate MFx value
!          CALL GMCxUNO2(1, NU1, 1)

!  Store conservative flux in F advective one in A
!          DO i=1, NU1
!             M=ISD(5,i)
!             N=ISD(6,i)
!  Add diffusion flux as well, note FX with an negative sign
!             F(M) = F(M) - FU(i)*U(i) + FX(i)  
!             F(N) = F(N) + FU(i)*U(i) - FX(i) 
!             AU(M) = AU(M) - FU(i)*UC(M) + FX(i)
!             AU(N) = AU(N) + FU(i)*UC(N) - FX(i)
!          ENDDO

!  Store conservative update in D and advective update in C
!  The side length in MF value has to be cancelled with cell length
!  Also divided by another cell length as U UC is in basic unit
!          DO n=1,N1
!             D(n)=C(n) + F(n)*RCELA(n)
!             C(n)=C(n) + AU(n)*RCELA(n)
!             F(n)=0.0
!             AU(n)=0.0
!          ENDDO
!! Note the N Polar cell does not have any U-flux input.

!  Call GMCyUNO2 to calculate MFy value
!          CALL GMCyUNO2(1, NV1, 1)

!  Store conservative flux in F
!          DO j=1, NV1
!             M=JSD(5,j)
!             N=JSD(6,j)
!  Add diffusion flux FY, note FY with an negative sign
!             AV(M) = AV(M) - FV(j)*V(j) + FY(j)
!             AV(N) = AV(N) + FV(j)*V(j) - FY(j)
!          ENDDO

!  Store conservative update of D in C 
!  The v side length in MF value has to be cancelled with cell length
!! One cosine factor is also needed to be divided for GMC grid
!          DO n=1,N1
!             CNST=RCELA(n)/CCLat( ICE(2,n) )
!             C(n)=D(n) + AV(n)*CNST
!             AV(n)=0.0
!          ENDDO

!! For size-2 cells integrate every two time steps
!          IF( MOD(NB, 2) .EQ. 0 ) THEN

!          CALL GMCxUNO2(NU1+1, NU1+NU2, 2)

!  Store conservative flux in F advective one in A
!          CNST2=2.0
!          DO i=NU1+1, NU1+NU2
!             M=ISD(5,i)
!             N=ISD(6,i)
!             F(M) = F(M) - FU(i)*U(i)*CNST2 + FX(i)
!             F(N) = F(N) + FU(i)*U(i)*CNST2 - FX(i)
!             AU(M) = AU(M) - FU(i)*UC(M)*CNST2 + FX(i)
!             AU(N) = AU(N) + FU(i)*UC(N)*CNST2 - FX(i)
!          ENDDO

!  Store conservative update in D and advective update in C
!  The side length in MF value has to be cancelled with cell length
!  Also divided by another cell length as U UC is in basic unit
!          DO n=N1+1, N1+N2
!             D(n)=C(n) + F(n)*RCELA(n)
!             C(n)=C(n) + AU(n)*RCELA(n)
!             F(n)=0.0
!             AU(n)=0.0
!          ENDDO
!! Note the N Polar cell does not have any U-flux input.

!  Call GMCyUNO2 to calculate MFy value
!          CALL GMCyUNO2(NV1+1,NV1+NV2, 2)

!  Store conservative flux in F
!          DO j=NV1+1, NV1+NV2
!             M=JSD(5,j)
!             N=JSD(6,j)
!             AV(M) = AV(M) - FV(j)*V(j)*CNST2 + FY(j)
!             AV(N) = AV(N) + FV(j)*V(j)*CNST2 - FY(j)
!          ENDDO

!  Store conservative update of D in C 
!  The v side length in MF value has to be cancelled with cell length
!! One cosine factor is also needed to be divided for GMC grid
!! Note y-size-2 cell centre is at CSLat rather than CCLat
!          DO n=N1+1, N1+N2
!             CNST=RCELA(n)/CSLat( ICE(2,n)+ICE(4,n)/2 )
!             C(n)=D(n) + AV(n)*CNST
!             AV(n)=0.0
!          ENDDO

!! End of size-2 cell integration
!          ENDIF

!! For size-4 cells integrate every 4 time steps
!          IF( MOD(NB, 4) .EQ. 0 ) THEN

!          CALL GMCxUNO2(NU1+NU2+1, NU, 4)
           CALL GMCxUNO2(1, NU, 1)

!  Store conservative flux in F advective one in A
           CNST4=4.0
!          DO i=NU1+NU2+1, NU
           DO i=1, NU
              M=ISD(5,i)
              N=ISD(6,i)
!             F(M) = F(M) - FU(i)*U(i)*CNST4 + FX(i) 
!             F(N) = F(N) + FU(i)*U(i)*CNST4 - FX(i)
!             AU(M) = AU(M) - FU(i)*UC(M)*CNST4 + FX(i)
!             AU(N) = AU(N) + FU(i)*UC(N)*CNST4 - FX(i)
              F(M) = F(M) - FU(i)*U(i) + FX(i) 
              F(N) = F(N) + FU(i)*U(i) - FX(i)
              AU(M) = AU(M) - FU(i)*UC(M) + FX(i)
              AU(N) = AU(N) + FU(i)*UC(N) - FX(i)
           ENDDO

!  Store conservative update in D and advective update in C
!  The side length in MF value has to be cancelled with cell length
!  Also divided by another cell length as U UC is in basic unit
!          DO n=N1+N2+1, NC
           DO n=1, NC
              D(n)=C(n) + F(n)*RCELA(n)
              C(n)=C(n) + AU(n)*RCELA(n)
              F(n)=0.0
              AU(n)=0.0
           ENDDO
!! Note the N Polar cell does not have any U-flux input.

!  Call GMCyUNO2 to calculate MFy value
!          CALL GMCyUNO2(NV1+NV2+1,NV, 4)
           CALL GMCyUNO2(1,NV, 1)

!  Store conservative flux in F
!          DO j=NV1+NV2+1, NV
           DO j=1, NV
              M=JSD(5,j)
              N=JSD(6,j)
              AV(M) = AV(M) - FV(j)*V(j) + FY(j)
              AV(N) = AV(N) + FV(j)*V(j) - FY(j)
!             AV(M) = AV(M) - FV(j)*V(j)*CNST4 + FY(j)
!             AV(N) = AV(N) + FV(j)*V(j)*CNST4 - FY(j)
           ENDDO

!  Store conservative update of D in C 
!  The v side length in MF value has to be cancelled with cell length
!! One cosine factor is also needed to be divided for GMC grid
!! Note y-size-4 cell centre is at CSLat rather than CCLat
!! including the polar (last) cell
!          DO n=N1+N2+1, NC
           DO n=1, NC
              CNST=RCELA(n)/CSLat( ICE(2,n)+ICE(4,n)/2 )
              C(n)=D(n) + AV(n)*CNST
              AV(n)=0.0
           ENDDO

!! End of size-4 cell integration
!          ENDIF

!! End of sub-step loops NB
!       ENDDO

!!    Store C(1:NC) back to CDBn(ND,1:NC)
         CDBn(ND,1:NC)=C(1:NC)

!!    End of directional bin loop
      ENDDO  DirLop

!!    Great circle turning (GCT) for lower part cells at 4 times of substeps,
!!    excluding lower part bounary cells. 
!      CALL GMCGtCrTn(1, NGLA, 4)
       CALL GMCGtCrTn(1, NGLA, 1)

!!    Arctic cells for global boundary cells
       DO i=1,NGLB
          ii=i+NGLA
          kk=MBGLo(i)

!!   Rotate the Arctic spectra by -AnglD before assigning to the lower part
!!   Note that it is equivalent to rotate the directional bins by AnglD.
!!   AngleD is measured positive anticlockwise from the rotated east to the 
!!   standard east. So Arctic spectral bins are rotated clockwise by AnglD 
!!   from the stand east, that is, Theta' = Theta + AnglD.

          Spectr=CDBn(1:NDIR,kk)
          Alpha=  AngCD(kk)

          CALL Specturn( NDir, 1, Alpha, Spectr )

          CDBn(1:NDIR,ii)=Spectr

       ENDDO

!!    Global cells for Arctic boundary cells
       DO i=1,NArB
          ii=i+NArA
          kk=MBArc(i)

!!   Rotate the lower part spectra by AnglD before assigning to the Arctic part
!!   Or turn the directional bins by -AnglD.   21 Jul 2009
          Spectr=CDBn(1:NDIR,kk)
!!   Note only the Arctic cells are assigned the AngCD value
          Alpha= -AngCD(ii)

          CALL Specturn( NDir, 1, Alpha, Spectr )

          CDBn(1:NDIR,ii)=Spectr

       ENDDO


!  Output tracer concentration if at selected writeup time steps
      IF( (NT .LT. 10*NWP .AND. MOD(NT,NWP/2) .eq. 0) .OR.    &
     &    (MOD(NT,NWP) .eq. 0) .OR. (NT .eq. NWP/4) )  THEN
        write(FL9NM(3:7), FMT='(i5)' )  10000+NT
        OPEN(UNIT=26, FILE=FL9NM, STATUS='NEW',IOSTAT=nn)
        IF(nn /= 0) PRINT*,' File FL9NM was not opened! '
        WRITE(UNIT=6,FMT='(2x,"NT= ",i6,3x,A9)') NT,FL9NM

           ii=0
        DO i=1, NC
!! Skip the overlapping boundary cells 
        IF( i .LE. NGLo-NGLB .OR. i .GT. NGLo+NArB ) THEN
               ii=ii+1
!  Integrate spectrum to get the SWH without factor 4
           CTT=0.0
           DO k=1, NDIR
              CTT = CTT + CDBn(k,i)
           ENDDO
           C(ii)=SQRT(CTT*DThta)

!!   Filter very small C(n) value so it is greater than E-90
           IF( Abs(C(ii)) .LT. 1.0E-90 ) THEN
               D(ii)=SIGN(1.0E-90, C(ii))
           ELSE
               D(ii)=C(ii)
           ENDIF
        ENDIF
        ENDDO

!    All cells are saved 
        WRITE(UNIT=26, FMT='(2x,2i8)' )  NT, ii
        WRITE(UNIT=26, FMT=7113)  (D(n),  n=1, ii)

        CLOSE(26)
      ENDIF
 7113 FORMAT( 1x, 7ES11.3 )

!!  End of time step loop
      ENDDO  TSLoop

       CALL DATE_AND_TIME(CDate, CTime)
       WRITE(6,"(' Loop ended time ',A,'  ',A)")  CTime

 9999  PRINT*, ' GMC2Dvct completed '

       CALL DATE_AND_TIME(CDate, CTime)
       WRITE(UNIT= 6,FMT="(1x,' End time date ',A10,2x,A10)") CTime, CDate
       WRITE(UNIT=16,FMT="(1x,' End time date ',A10,2x,A10)") CTime, CDate

       END PROGRAM GMC2Dvct 
!  End of main program


! Subroutine that calculate mid-flux values for x dimension 
       SUBROUTINE GMCxUNO2(NUA, NUB, MTP)
         USE Constants
         IMPLICIT NONE
         INTEGER, INTENT(IN):: NUA, NUB, MTP
         REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6

!    Two layer of boundary cells are added to each boundary cell face
!    with all boundary cells refer to C(-2:0)=0.0.

!    Notice an extra side length L is multiplied to mid-flux to give correct
!    proportion of flux into the cells.  This length will be removed by the
!    cell length when the tracer concentration is updated.

      DO i=NUA, NUB

!    Diffusion k*dt*2/dx multiplied by MTP for multiple steps
         CNST0=AKHDT2*MTP/DXLat( ISD(2,i) )

!    Courant number in local size-1 cell unit
         CNST6=U(i)*MTP

!    For positive velocity case
         IF(CNST6 >= 0.0)  THEN

!    Select Upstream, Central and Downstream cells
           L=ISD(4,i)
           M=ISD(5,i)
           N=ISD(6,i)

!    Cell length of UCD cells
           CNST1=REAL( ICE(3,L) )
           CNST2=Real( ICE(3,M) )
           CNST3=Real( ICE(3,N) )

!    Side gradients for central cells include 0.5 factor
           CNST4=(C(M)-C(L))/(CNST1+CNST2)
           CNST5=(C(N)-C(M))/(CNST3+CNST2)

!    Use minimum gradient all region
           CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

!    Mid-flux value inside central cell
           FU(i)=(C(M) + CNST*(CNST2-CNST6))*ISD(3,i)
           
!    For negative velocity case
         ELSE

!    Select Upstream, Central and Downstream cells
           L=ISD(5,i)
           M=ISD(6,i)
           N=ISD(7,i)

!    Cell length of UCD cells
           CNST1=REAL( ICE(3,L) )
           CNST2=Real( ICE(3,M) )
           CNST3=Real( ICE(3,N) )

!    Side gradients for central cells include 0.5 factor
!    Swap CNST4 and CNST5 so that CNST5 is alway the sign/face gradient
           CNST5=(C(M)-C(L))/(CNST1+CNST2)
           CNST4=(C(N)-C(M))/(CNST3+CNST2)

!    Use minimum gradient outside monotonic region
           CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

!    Mid-flux value inside central cell M
           FU(i)=(C(M) - CNST*(CNST2+CNST6))*ISD(3,i)

         ENDIF

!    Diffusion flux by face gradient x DT x face_width
         FX(i)=CNST0*CNST5*ISD(3,i)

      END DO

! 999  PRINT*, ' Sub GMCxUNO2 ended.'

      RETURN
      END SUBROUTINE GMCxUNO2


! Subroutine that calculate mid-flux values for x dimension 
       SUBROUTINE GMCyUNO2(NVA, NVB, MTP)
         USE Constants
         IMPLICIT NONE
         INTEGER, INTENT(IN):: NVA, NVB, MTP
         REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6, CNST8

!    Two layer of boundary cells are added to each boundary cell face
!    with all boundary cells refer to C(-2:0)=0.0.

!    Diffusion k*dt*2/DY multiplied by MTP for multiple steps
         CNST0=AKHDT2*MTP*DYR

!    Notice an extra side length L is multiplied to mid-flux to give correct
!    proportion of flux into the cells.  This length will be removed by the
!    cell length when the tracer concentration is updated.

      DO j=NVA, NVB

!    Courant number in basic cell unit
         CNST6=V(j)*MTP

!    Face size integer and cosine factor
         CNST8=CSLat( JSD(2,j) )*Real( JSD(3,j) )

!    For positive velocity case
         IF(CNST6 >= 0.0)  THEN

!    Select Upstream, Central and Downstream cells
           L=JSD(4,j)
           M=JSD(5,j)
           N=JSD(6,j)

!    Cell length of UCD cells
           CNST1=Real( ICE(4,L) )
           CNST2=Real( ICE(4,M) )
           CNST3=Real( ICE(4,N) )

!    Side gradients for central cells including 0.5 factor
           CNST4=(C(M)-C(L))/(CNST1+CNST2)
           CNST5=(C(N)-C(M))/(CNST3+CNST2)

!    Use minimum gradient outside monotonic region
           CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

!    Mid-flux value multiplied by face width and cosine factor
           FV(j)=( C(M) + CNST*(CNST2-CNST6) )*CNST8

!    For negative velocity case
         ELSE

!    Select Upstream, Central and Downstream cells
           L=JSD(5,j)
           M=JSD(6,j)
           N=JSD(7,j)

!    Cell length of UCD cells
           CNST1=REAL( ICE(4,L) )
           CNST2=Real( ICE(4,M) )
           CNST3=Real( ICE(4,N) )

!    Side gradients for central cells including 0.5 factor
!    Swap CNST4 and CNST5 so that CNST5 is the face/sign gradient
           CNST5=(C(M)-C(L))/(CNST1+CNST2)
           CNST4=(C(N)-C(M))/(CNST3+CNST2)

!    Use minimum gradient outside monotonic region
           CNST=Sign(1.0, CNST5)*min( Abs(CNST4), Abs(CNST5) )

!    Mid-flux value multiplied by face width and cosine factor
           FV(j)=( C(M) - CNST*(CNST2+CNST6) )*CNST8 

         ENDIF

!    Diffusion flux by face gradient x DT x face_width x cos(lat)
         FY(j)=CNST0*CNST5*CNST8

      END DO

! 999  PRINT*, ' Sub GMCyUNO2 ended.'

      RETURN
      END SUBROUTINE GMCyUNO2


! Subroutine that calculate mid-flux values for x dimension 
       SUBROUTINE CGDHDXDY
         USE Constants
         IMPLICIT NONE
         REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6

!!   Assign water depth to HCel from KG integer values.
!!   set all depth zero for negative cells.
       CGRP(-9:0)=0.0
       DHDX(-9:0)=0.0
       DHDY(-9:0)=0.0
       HCel(-9:0)=0.0
       HCel(1:NC)=FLOAT( KG(1:NC) )

!!   Calculate group speed and refaction factor for all cells
       CNST=2.0*GeoPie*Frqcy
       CNST0=1.0E-8

       DO n=1, NC
            CNST3=HCel(n)
            CNST4=CNST*CNST/GRVTY
!!  Iteration to calculate kH value for this cell
            CNST1=CNST4/TANH(CNST3)
            CNST2=CNST4/TANH(CNST1*CNST3)
            DO WHILE ( ABS(CNST2 - CNST1) .GT. CNST0 )
               CNST1=CNST2
               CNST2=CNST4/TANH(CNST1*CNST3)
            ENDDO
            
            CNST1=CNST2*CNST3 
            CNST2=1.0/COSH(CNST1)
!!  Group speed
            CGrp(n)=(GRVTY*0.5/CNST)*(TANH(CNST1)+CNST2*CNST2*CNST1)

!!  Refraction rate factor, without the dt/d(theta) factor
!!          REFR(n)=2.0*CNST5*CNST*( 1.0-CNST3 )/( 2.0*CNST1+SINH(2.0*CNST1) )
            REFR(n)=CNST/SINH(2.0*CNST1)

       ENDDO

!!   Calculate DH/DX using ISD flux array to find neighbouring cells.
!!   0.5 divided by the basic cell x-length at Equator in meter
       CNST6=0.5/(DX0*REARTH)

       AU=0.0
!!   Use the face arrays to calculate the bathymetry x-gradients.
       DO i=1, NU

!    Select Central and Downstream cells
           M=ISD(5,i)
           N=ISD(6,i)

!    Cell length of UCD cells
           CNST2=Real( ICE(3,M) )
           CNST3=Real( ICE(3,N) )

!    Side gradients over basic cell length for central cells include face length factor
           FU(i)=CNST6*ISD(3,i)*(HCel(N)-HCel(M))/(CNST3+CNST2)

!    Store side gradient in two neighbouring cells
           AU(M) = AU(M) + FU(i)
           AU(N) = AU(N) + FU(i)

       END DO

!  Assign averaged side-gradient to DHDX, plus latitude factor
!  Note averaging over 2 times of cell y-width factor. 
!  Excluding polar cell and restrict gradient < 0.1
       DO n=1,NC-1
            CNST3=CCLat( ICE(2,n) )*ICE(4,n)*2.0
            DHDX(n)=AU(n)/CNST3
            IF(DHDX(n) .GT. 0.1) DHDX(n)=0.1
       ENDDO

!! Set polar cell gradient to be zero
            DHDX(NC)=0.0


!!   Calculate DH/DY using JSD flux array to find neighbouring cells.
!!   0.5 divided by the basic cell y-length at Equator in meter
       CNST6=0.5/(DY*REARTH)

       AV=0.0
!!   Use the face arrays to calculate the bathymetry y-gradients.
       DO j=1, NV

!    Select Central and Downstream cells
           M=JSD(5,j)
           N=JSD(6,j)

!    Cell length of UCD cells
           CNST2=Real( ICE(4,M) )
           CNST3=Real( ICE(4,N) )

!    Side gradients over basic cell length for central cells include face length factor
           FV(j)=CNST6*JSD(3,j)*(HCel(N)-HCel(M))/(CNST3+CNST2)

!    Store side gradient in two neighbouring cells
           AV(M) = AV(M) + FV(j)
           AV(N) = AV(N) + FV(j)

       END DO

!  Assign averaged side-gradient to DHDY.
!  Note averaging over 2 times of cell x-width factor. 
!  Excluding the polar cell
       DO n=1,NC-1
            CNST3=ICE(3,n)*2.0
            DHDY(n)=AV(n)/CNST3
            IF(DHDY(n) .GT. 0.1) DHDY(n)=0.1
       ENDDO

!! Set polar cell gradient to be zero
            DHDY(NC)=0.0

!!  Output DHDX DHDY variables for examination
      WRITE(6,*) " Storing bathy gradient array 1, ", NGLo

      OPEN(UNIT=10,FILE='G6kmDHDX.d',STATUS='UNKNOWN',IOSTAT=nn)
      IF(nn /= 0) PRINT*,' File Pros was not opened! '
      WRITE(10,FMT='(1x,i8)') NGLo
      WRITE(10,FMT='((10F8.4))')  (DHDX(N), N=1,NGLo)
      CLOSE(10)

      OPEN(UNIT=11,FILE='G6kmDHDY.d',STATUS='UNKNOWN',IOSTAT=nn)
      IF(nn /= 0) PRINT*,' File Pros was not opened! '
      WRITE(11,FMT='(1x,i8)') NGLo
      WRITE(11,FMT='((10F8.4))')  (DHDY(N), N=1,NGLo)
      CLOSE(11)

      OPEN(UNIT=12,FILE='G6kmCgrp.d',STATUS='UNKNOWN',IOSTAT=nn)
      IF(nn /= 0) PRINT*,' File Pros was not opened! '
      WRITE(12,FMT='(1x,i8)') NGLo
      WRITE(12,FMT='((10F8.3))')  (CGrp(N), N=1,NGLo)
      CLOSE(12)

      OPEN(UNIT=13,FILE='G6kmRefr.d',STATUS='UNKNOWN',IOSTAT=nn)
      IF(nn /= 0) PRINT*,' File Pros was not opened! '
      WRITE(13,FMT='(1x,i8)') NGLo
      WRITE(13,FMT='((10F8.4))')  (Refr(N), N=1,NGLo)
      CLOSE(13)

! 999  PRINT*, ' Sub CGDHDXDY ended.'

       RETURN
       END SUBROUTINE CGDHDXDY


! Subroutine that calculate great circle turning (GCT)
       SUBROUTINE GMCGtCrTn(NCA, NCB, MTP)
         USE Constants
         IMPLICIT NONE
         INTEGER, INTENT(IN):: NCA, NCB, MTP
         REAL:: CNST, CNST0, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6

!    Notice an extra side length L is multiplied to mid-flux to give correct
!    proportion of flux into the cells.  This length will be removed by the
!    cell length when the tracer concentration is updated.

      DO n=NCA, NCB

!!   GCT Courant number in MTP times of basic time step without cos(theta)
!!   and it is assumed to be less than 1 in magnitude.
!        CNST0=CoGCT(n)*MTP
!!   Combined into CoRfr

!!   Restrict Courant number to be less than 1.0
!        IF(ABS(CNST0) > 1.0) THEN
!           WRITE(6,*) "GCT C > 1 at cell n=", n, CNST0
!           CNST0=Sign( 1.0, CNST0 )
!        ENDIF

!!   Asign cell spectrum to temporary variable Spcetr
         Spectr=CDBn(1:NDIR,n)
         SpeGCT=0.0 

!!   Loop through NDIR directional bins for each cell spectrum
         DO j=1, NDIR

!    Final GCT Courant number for this dirctional bin
!    Add refraction Courant nubmer as well
!        CNST6=CNST0*CSeta(j) + CoRfr(j,n)*MTP
         CNST6=CoRfr(j,n)*MTP
!!   CoRfr now contains GCT term and refraction (limited below 80.0)

!    For positive turning case
         IF(CNST6 > 0.0)  THEN

!    Work out integer number of bins to be skipped.
!    If K is great than NDIR, full circle turning is removed.
           K=MOD( INT(CNST6), NDIR )
 
!    Select the upstream and downstream bins to rotate in, wrap at end
           L=j+K
           M=j+K+1
           IF( L .GT. NDIR ) L = L - NDIR
           IF( M .GT. NDIR ) M = M - NDIR

!!   Divid the j bin energy by fraction of CNST6 and store in SpeGCT
           CNST1=CNST6 - FLOAT( INT(CNST6) )
           CNST2=1.0 - CNST1
           SpeGCT(L)=SpeGCT(L)+Spectr(j)*CNST2
           SpeGCT(M)=SpeGCT(M)+Spectr(j)*CNST1

!    For negative or no turning case
         ELSE 

!    Work out integer number of bins to be skipped.
!    If K is great than NDIR, full circle turning is removed.
           K=MOD( INT(-CNST6), NDIR )

!    Select the upstream and downstream bins to rotate in, wrap at end
           L=j-K
           M=j-K-1
           IF( L .LT. 1 ) L = L + NDIR
           IF( M .LT. 1 ) M = M + NDIR

!!   Divid the bin energy by fraction of CNST6 and store in SpeGCT
           CNST1=-CNST6 - FLOAT( INT(-CNST6) )
           CNST2=1.0 - CNST1
           SpeGCT(L)=SpeGCT(L)+Spectr(j)*CNST2
           SpeGCT(M)=SpeGCT(M)+Spectr(j)*CNST1

         ENDIF

!!   End of directional loop j
         END DO

!!   Store GCT spectrum
         CDBn(1:NDIR,n) = SpeGCT

!!   End of cell loop n
      END DO

! 999  PRINT*, ' Sub GMCGtCrTn ended.'

      RETURN
      END SUBROUTINE GMCGtCrTn


! Subroutine that initialise C U V UC VC for rotating or deform 
!   flow field, using the same grid and time step.

      SUBROUTINE SPECUUVV
        USE Constants
        IMPLICIT NONE
        REAL:: CNST, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6
        REAL:: Spec1D(NDIR), Spec2D(NDIR)

!!  All velocities are in unit of basic cell length and time step or 
!!  divided by the grid velocity BX/DT or BY/DT

!! Initialise C U V for spherical rotation test in Arctic

!! Whole array assignment for DDBn(NDir,NCL)
        CDBn=0.0

!! All spectra are identical to a cosine distribution with main direction
!! at 45 deg NE and total wave energy equal to 25.0 so SQRT(E)=5.0.
!! or at -45 deg SE
        CNST=50.0/Pie
        Spec1D = 0.0
        Spec2D = 0.0
        CNST6 = 0.0
        DO k=1, NDIR
           CNST1=COS(Theta(k) + Pie/4.0)
           CNST2=COS(Theta(k) - Pie/4.0)
           IF(CNST1 .GT. 0.0) THEN
              Spec1D(k)=CNST*CNST1*CNST1
           ENDIF
           IF(CNST2 .GT. 0.0) THEN
              Spec2D(k)=CNST*CNST2*CNST2
           ENDIF
           CNST6 = CNST6 + Spec1D(k)
        ENDDO
        SWH0=SQRT(CNST6*DThta)
        

!!   Rotate the first spectra by -90 so that its peak direction is at -45. 
!!   Note that it is equivalent to rotated the directional bins by 90.
!         Spectr= Spec1D
!         Alpha=  90.0

!         CALL Specturn( NDir, 1, Alpha, Spectr )

!         Spec2D = Spectr

!! Initialise a strip below 45S-60S or j = -768 to -1024 in Southern Ocean 
!! and 45N to 60N or j = 768 to 1024 in upper part of the G50SMC domain.
!! Also put a non-zero spectral zone in the Arctic above 85N or j=1450.
!! Note ICE(2,*) is on SW cell corner.
!! Equator is on cell face and ICE is equal to 0 at Equator.
      ij=NINT(45.0/DLat)
      jk=NINT(60.0/DLat)
      mn=NINT(85.0/DLat)

!! N Atlantic round patch (30W 15N) to match 5 deg polar disk patch
      ijk=NINT(330.0/DLon)
      lmn=NINT( 15.0/DLat)
      CNST3 = 1.0
      CNST2 = 1.0/(0.5 + 5.0/DLat)
      CNST1 = CNST2*DLon/DLat

! Gulf of Mexico patch to test coastal high waves.  JGLi 04May2011
      ii=NINT(270.0/DLon)
      jj=NINT( 25.0/DLat)

      DO i=1,NC

         kk=ICE(2,i)
!! A round patch in N Atlantic to match Arctic one
         CNST5 = Real( ICE(1,i) - ijk )*CNST1*CCLat(kk)
         CNST6 = Real( kk       - lmn )*CNST2
         CNST4 = CNST5*CNST5 + CNST6*CNST6 
         IF( CNST4 .LT. CNST3 ) THEN
             CDBn(1:NDIR,i)=Spec2D
             C(i)=SWH0
         ENDIF

!! A round patch in Gulf of Mexico same as Arctic one
         CNST5 = Real( ICE(1,i) - ii )*CNST1*CCLat(kk)
         CNST6 = Real( kk       - jj )*CNST2
         CNST4 = CNST5*CNST5 + CNST6*CNST6
         IF( CNST4 .LT. CNST3 ) THEN
             CDBn(1:NDIR,i)=Spec2D
             C(i)=SWH0
         ENDIF

!!  Southern ocean uses Spec2D
         IF( -jk .LT. kk  .AND.  kk .LT. -ij ) THEN
             CDBn(1:NDIR,i)=Spec2D
             C(i)=SWH0
         ENDIF

!!  Northern Atlantic and Pacific use Spec1D
         IF( ij .LT. kk  .AND.  kk .LT. jk ) THEN
             CDBn(1:NDIR,i)=Spec1D
             C(i)=SWH0
         ENDIF

!!  Arctic uses Spec2D as well
         IF( mn .LT. kk ) THEN
             CDBn(1:NDIR,i)=Spec2D
             C(i)=SWH0
         ENDIF

      ENDDO

!  Set angular speed to be 36 hr per cycle
       AMG= Agu36

!!  Factor to convert group speed Cg into Courant number with cell legth and time step
      CNST=DT/REarth

!!  The same angular speed of 36 hr per cycel is used for 24 bin directins
!!  Note dirctions for the Arctic part are deducted by AngU/V in radian.
!!  Face size is for the proportion of face length, not gradient distance. 
      DO L=1, NU
!!  Use average Cg on cell face
         CNST3=0.5*(CGrp( ISD(5,L) ) + CGrp( ISD(6,L) ))*CNST/DXLat( ISD(2,L) )
!!  24 bins are done in one vector assignment 
         UDBn(1:NDIR,L)=CNST3*COS( Theta(1:NDIR)-AngU(L) )
      ENDDO
 
      CNST2=CNST/DY
      DO L=1, NV
         CNST3=0.5*(CGrp( JSD(5,L) ) + CGrp( JSD(6,L) ))*CNST2
         VDBn(1:NDIR,L)=CNST3*SIN( Theta(1:NDIR)-AngV(L) )
      ENDDO
      WRITE(6,*) " Convertin U V to Courant number done"

!!  Cell centre U V for advective flux update.
!!  Set boundary cell velocity to be zero
      UC(-9:0)=0.0
      VC(-9:0)=0.0

!!  Find maximum Courant number with cell centre speed
!!  while converting cell centre UC VC to Courant number
!!  in size-1 unit, not really the cell size for other sized cells.
!!  Note that Polar cell DX(NC) is used to store its area
!!  rather than the desired DX(NC), which has no definition.
!!  So UC(NC) is not properly specified here as Courant number.
!!  But this value is never used because no U-flux is associated 
!!  with the Polar cell.  VC(NC) is fine here as DY is cancelled
!!  at the cell update line to give the proper polar cell area.
!!  Note size-1 dx and dy are included in UC and VC, respectively.
!!  Note longitudinal merging factor is divided here as
!!  UC is only divided by the single siz-1 dx.  y-size is
!!  added because subtime steps is proportional to it.

!     CNST=AMG*DT
      CNST=DT/REarth
      CNST1=0.0
      CNST2=0.0
      CNST3=0.0
      DO i=1, NC
         UC(i)=CGrp(i)*CNST/DXLAT( ICE(2,i) )
         VC(i)=CGrp(i)*CNST*DYR
         CNST1=Max( CNST1, Abs(UC(i)*ICE(4,i)/ICE(3,i)) )
         CNST2=Max( CNST2, Abs(VC(i)) )
!!  24 bins are done in one vector assignment 
         UCBn(1:NDIR,i)=UC(i)*COS( Theta(1:NDIR)-AngCD(i)*D2RAD )
         VCBn(1:NDIR,i)=VC(i)*SIN( Theta(1:NDIR)-AngCD(i)*D2RAD )
      ENDDO
      CMX=Max(CNST1, CNST2)
!!  Maximum grid speed, i.e. the ratio of speed to grid length
!!  Not necessrily the maximum speed as grid length varies.
      UMX=CMX/DT

!!  Great circle tuning Courant number without cos(theta)
      CNST=DT/(REarth*DThta)
      CNST3=0.0

!!  Only specify cells in the lower part (without the Arctic part )
      DO i=1, NGLo
!!  Add GCT term Courant number part 1 (excluding cos(theta))
         CoGCT(i)= -Cgrp(i)*CNST*TnLat( ICE(2,i)+ICE(4,i)/2 )
         CNST3=Max( CNST3, Abs(CoGCT(i)) )
      ENDDO

!!  Maximum GCT Courant number is at main time step (4*dt)
      CGCMX=4.0*CNST3

!!  Final refraction Courant number for lower part only
      CNST=DT/DThta
      CNST3=0.0

      DO i=1, NGLo
      DO k=1, NDIR
         CNST6=CNST*REFR(i)*( DHDX(i)*SNeta(k) - DHDY(i)*CSeta(k) )
!!  Restrict refraction angle to be less than 80 deg or CoRfr < 8.0/4.0
!!  As there are 36 directional bins 80 degree covers 8 dir bins. 
!!  The refraction will be calculated with rotation code so there is 
!!  no need to keep the refraction coefficient as a Courant number < 1.
!!             JGLi 18May2010
         IF(Abs(CNST6) .GT. 2.0)  THEN
            CoRfr(k,i)=Sign( 2.0, CNST6 )
         ELSE
            CoRfr(k,i)=CNST6
         ENDIF
         CNST3=Max( CNST3, Abs(CoRfr(k,i)) )
!!  Combining GCT term and restrict overall rotation Courant number to 0.25
!!  Restriction is lifted as rotation sub is used.  JGLi 18May2010
!!  Note it will be multiplied by 4 at every main steps (or 4 substeps).
!        CNST5=CoRfr(k,i)+CoGCT(i)*CSeta(k)
!        IF(Abs(CNST5) .GT. 0.25)  THEN
!           CoRfr(k,i)=Sign( 0.25, CNST6 )
!        ELSE
!           CoRfr(k,i)=CNST5
!        ENDIF
         CoRfr(k,i)=CoRfr(k,i)+CoGCT(i)*CSeta(k)
      ENDDO
      ENDDO

!!  Maximum refraction Courant number is at main time step (4*dt)
      CRFMX=4.0*CNST3


      WRITE(6,*) '  Wind file conversion done!'

! 999  PRINT*, ' Sub SPECUUVV ended.'

      RETURN

      END SUBROUTINE SPECUUVV


!  This subroutine turn the wave spectrum by an fixed angle anti-clockwise
!  so that it may be used in the rotated or stanadard system.
!  First created:   26 Aug 2005   Jian-Guo Li
!  Last modified:   20 Jul 2009   Jian-Guo Li
!
! Subroutine Interface:

  Subroutine Specturn( NDirc, NFreq, Alphad, Spectr )
 
! Description:
!   Rotates wave spectrum anticlockwise by angle alphad
!
! Subroutine arguments
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: NFreq, NDirc         ! No. frequ and direc bins
   REAL,    INTENT(IN) :: Alphad               ! Turning angle in degree
   REAL, INTENT(INOUT) :: Spectr(NDirc,NFreq)  ! Wave spectrum in and out

! Local variables
   INTEGER :: ii, jj, kk, nsft
   REAL    :: Ddirc, frac, CNST
   REAL, Dimension(NFreq)      ::  Wrkfrq, Tmpfrq
   REAL, Dimension(NDirc,NFreq)::  Wrkspc

! Check input bin numbers
   IF( (NFreq .LT. 0) .OR. (NDirc .LT. 0) )  THEN
      PRINT*, " Invalid bin number NF or ND", NFreq, NDirc
      RETURN
   ELSE
      Ddirc=360.0/FLOAT(NDirc)
   ENDIF

! Work out shift bin number and fraction

      CNST=Alphad/Ddirc
      nsft=INT( CNST )
      frac= CNST - FLOAT( nsft )
!     PRINT*, ' nsft and frac =', nsft, frac

! Shift nsft bins if >=1
    IF( ABS(nsft) .GE. 1 )  THEN
      DO ii=1, NDirc

! Wave spectral direction bin number is assumed to increase clockwise from North
! So shift nsft bins anticlockwise results in local bin number increases by nsft
         jj=ii + nsft
 
! As nsft may be either positive or negative depends on alphad, wrapping may
! happen in either ends of the bin number train
         IF( jj > NDirc )  jj=jj - NDirc
         IF( jj < 1     )  jj=jj + NDirc

! Copy the selected bin to the loop bin number
         Wrkspc(ii,:)=Spectr(jj,:)
 
      Enddo

! If nsft=0, no need to shift, simply copy
    ELSE
        Wrkspc = Spectr
    ENDIF

! Pass fraction of wave energy in frac direction
! Positive or anticlock case, larger bin upstream
    IF( frac > 0.0 ) THEN
      Tmpfrq=Wrkspc(1,:)*frac
      DO kk=NDirc, 1, -1
         Wrkfrq=Wrkspc(kk,:)*frac 
         Spectr(kk,:)=Wrkspc(kk,:) - Wrkfrq + Tmpfrq 
         Tmpfrq=Wrkfrq
      ENDDO
    ELSE
! Negative or clockwise case, smaller bin upstream
      Tmpfrq=Wrkspc(NDirc,:)*frac
      DO kk=1, NDirc
         Wrkfrq=Wrkspc(kk,:)*frac
         Spectr(kk,:)=Wrkspc(kk,:) + Wrkfrq - Tmpfrq
         Tmpfrq=Wrkfrq
      ENDDO
    ENDIF

! Specturn completed

   Return 
   End Subroutine Specturn
!

! Subroutine that generates the cell side wind velocity
 SUBROUTINE SideWind
   USE Constants
   IMPLICIT NONE
   REAL:: CNST, CNST1, CNST2, CNST3, CNST4, CNST5, CNST6
   REAL, ALLOCATABLE, DIMENSION(:)::  XLon, WLat, AnglD
   REAL ::  DfPolat, DfPolon, Omega, DfSpeed

!!    Note only the Arctic part needs the rotation angles.
!     Work out u-face central position XLon, WLat in standard grid
      CNST1=DLon*0.5
      CNST2=DLat*0.5
      DfPolat=Polat
      DfPolon=Polon

      ALLOCATE( XLon(NUAr), WLat(NUAr), AnglD(NUAr) )
      WRITE(6,*) " Calculating U component ..."

!!  Initialise to be zero for all values
         AngU=0.0
         AngV=0.0
         AngCD=0.0

      DO L=1, NUAr
         i=L+NUGL 
!!  U-face latitude with half dlat increase from SW corner
!!  Note j is from -220 to 239 and j=0 corresponds to v-face on the Equator
!!  Longitude is measured half-grid from ZrLon and first cell i=0 coicides with XLon=0.
         XLon(L)= Float( ISD(1,i) )*DLon 
         WLat(L)= Float( ISD(2,i) )*DLat + Float( ISD(3,i) )*CNST2 
      END DO

!!  Convert standard lat/lon into rotated lat/lon for deformation wind
      CALL PolarAnglD ( WLat, XLon, AnglD, NUAr )
!     CALL LLTOEQANGLE( WLat, XLon, ELat, ELon,     &
!    &                 AnglD, DfPolat, DfPolon, NUAr)

      DO L=1, NUAr
         i=L+NUGL 
!!  Convert the AnglD into rad and store in AngU(L). True U value for all 
!!  directional bins will be generated from the value later.
         AngU(i)=AnglD(L)*D2RAD 
      END DO

      DEALLOCATE( XLon, WLat, AnglD )

        ALLOCATE( XLon(NVAr), WLat(NVAr), AnglD(NVAr) )

!     Work out v-face central position XLon, WLat in standard grid
      DO L=1, NVAr
         j=L+NVGL
!!  V-face latitude with half_dlon*JSD(3) increase from SW corner
         XLon(L)= Float( JSD(1,j) )*DLon + CNST1*Float( JSD(3,j) ) 
         WLat(L)= Float( JSD(2,j) )*DLat
      END DO

!!  Convert standard lat/lon into rotated lat/lon for deformation wind
      CALL PolarAnglD ( WLat, XLon, AnglD, NVAr )
!     CALL LLTOEQANGLE( WLat, XLon, ELat, ELon,     &
!    &                 AnglD, DfPolat, DfPolon, NVAr)

      DO L=1, NVAr
         j=L+NVGL
!!  Convert the AnglD into rad and store in AngV(L). True V value for all 
!!  directional bins will be generated from the value later.
         AngV(j)= AnglD(L)*D2RAD
      END DO

      DEALLOCATE( XLon, WLat, AnglD )


!! Specific cell centre velocity compenents
      ALLOCATE( XLon(NArc), WLat(NArc),  AnglD(NArc) )
      WRITE(6,*) " Calculating UC, VC component ..."

      CNST1=DLon*0.5
      CNST2=DLat*0.5
!! All cells include the polar cell
!! Note the wlat is not at 90N for the polar cell as direction will be undefined.
!! Here Wlat is half dlat from the polar cell edge and half dlat from the NP.
      DO L=1, NArc-1
         i=L+NGLo

!!  Cell centre latitude equal to west side centre latitude.
!!  Cell centre longitude with half cell width increase from West side centre
!!  Although the polar cell is of angular radius dlat (not dlat/2) the 
!!  transformation location is still used dlat/2 from its SW corner. The error
!!  will be negeligible as only the AnglD is used.
         XLon(L)= Float( ICE(1,i) )*DLon + CNST1*Float( ICE(3,i) ) 
         WLat(L)= Float( ICE(2,i) )*DLat + CNST2*Float( ICE(4,i) )

      END DO

!! North Polar cell centre coincide with NP
         XLon(NArc)=0.0
         WLat(NArc)=90.0
!! AnglD will be undefined with NP location as no local east at NP.

!!  Convert standard lat/lon into rotated lat/lon for deformation wind
      CALL PolarAnglD ( WLat, XLon, AnglD, NArc )
!     CALL LLTOEQANGLE( WLat, XLon, ELat, ELon,     &
!    &                 AnglD, DfPolat, DfPolon, NArc )

      DO L=1, NArc
         i=L+NGLo
!!  Keep the AnglD in Deg and store in AngCD(L).  Spectral rotation for
!!  boundary cell update will use this angle later.
         AngCD(i)=  AnglD(L) 
      END DO

 999  PRINT*, ' Sub SideWind ended.'

      RETURN

 END SUBROUTINE SideWind


!Li
!Li  Merged UM source code for rotated grid, consiting the following
!Li  original subroutines in UM 6.1
!Li    LLTOEQ1A  WCOEFF1A  and  LBCROTWINDS1
!Li  The last subroutine is modified to process only one level winds
!Li  cpp directives are removed and required header C_Pi.h inserted.
!Li	    Jian-Guo Li     26 May 2005
!Li
!Li  The WCOEFF1A subroutine is merged into LLTOEQ to reduce repetition
!Li  of the same calculations. Subroutine interface changed to 
!Li  LLTOEQANGLE
!Li	    Jian-GUo Li     23 Aug 2005
!Li
!LL  Subroutine LLTOEQANGLE--------------------------------------------    
!LL                                                                        
!LL  Purpose:  Calculates latitude and longitude on equatorial             
!LL            latitude-longitude (eq) grid used in regional               
!LL            models from input arrays of latitude and                    
!LL            longitude on standard grid. Both input and output           
!LL            latitudes and longitudes are in degrees.                    
!Li	       Also calculate rotation angle in degree to tranform
!Li            standard wind velocity into equatorial wind.
!Li	       Valid for 0<PHI_POLE<90 or new pole in N. hemisphere.
!LL                                                                        
!* Arguments:--------------------------------------------------------    
      SUBROUTINE LLTOEQANGLE( PHI, LAMBDA, PHI_EQ, LAMBDA_EQ,     &              
     &                 ANGLED, PHI_POLE, LAMBDA_POLE, POINTS )             
                                                                           
      IMPLICIT NONE                                                        
                                                                           
      INTEGER:: POINTS    !IN  Number of points to be processed             

      REAL :: PHI_POLE,  & !IN  Latitude of equatorial lat-lon pole
     &        LAMBDA_POLE  !INOUT  Longitude of equatorial lat-lon pole
                                                                           
      REAL, DIMENSION(POINTS) ::         &
     &        PHI,       & !IN  Latitude
     &        LAMBDA,    & !IN  Longitude
     &        ANGLED,    & !OUT turning angle in deg for standard wind
     &        LAMBDA_EQ, & !OUT Longitude in equatorial lat-lon coords
     &        PHI_EQ       !OUT Latitude in equatorial lat-lon coords

! Define local varables:-----------------------------------------------
      REAL :: A_LAMBDA, A_PHI, E_LAMBDA, E_PHI, SIN_PHI_POLE, COS_PHI_POLE,  &
     &        TERM1, TERM2, ARG, LAMBDA_ZERO, LAMBDA_POLE_KEEP
      INTEGER   :: I                                                            
      REAL, PARAMETER :: SMALL=1.0E-6

! Constants from comdecks:---------------------------------------------

      Real, Parameter :: Pi = 3.14159265358979323846  , &
     &                   Pi_Over_180 = Pi/180.0       , &
     &                   Recip_Pi_Over_180 = 180.0/Pi                     

!*----------------------------------------------------------------------   

! 1. Initialise local constants
! Scale lambda pole to range -180 to 180 degs
      LAMBDA_POLE_KEEP=LAMBDA_POLE
      IF (LAMBDA_POLE.GT. 180.0) then
          LAMBDA_POLE=LAMBDA_POLE-360.0
      ENDIF

! Latitude of zeroth meridian
      LAMBDA_ZERO=LAMBDA_POLE+180.0
! Sine and cosine of latitude of eq pole
      IF (PHI_POLE >= 0.0) THEN
        SIN_PHI_POLE =  SIN(PI_OVER_180*PHI_POLE)
        COS_PHI_POLE =  COS(PI_OVER_180*PHI_POLE)
      ELSE
        SIN_PHI_POLE = -SIN(PI_OVER_180*PHI_POLE)
        COS_PHI_POLE = -COS(PI_OVER_180*PHI_POLE)
      ENDIF

! 2. Transform from standard to equatorial latitude-longitude

      DO 200 I= 1, POINTS

! Scale longitude to range -180 to +180 degs

      A_LAMBDA=LAMBDA(I)-LAMBDA_ZERO
      IF(A_LAMBDA.GT. 180.0) A_LAMBDA=A_LAMBDA-360.
      IF(A_LAMBDA.LE.-180.0) A_LAMBDA=A_LAMBDA+360.

! Convert latitude & longitude to radians

      A_LAMBDA=PI_OVER_180*A_LAMBDA
      A_PHI=PI_OVER_180*PHI(I)

! Compute eq latitude using equation (4.4)

      ARG=-COS_PHI_POLE*COS(A_PHI)*COS(A_LAMBDA)   &
     &    +SIN_PHI_POLE*SIN(A_PHI)
      ARG=MIN(ARG, 1.0)
      ARG=MAX(ARG,-1.0)
      E_PHI=ASIN(ARG)
      PHI_EQ(I)=RECIP_PI_OVER_180*E_PHI

! Compute eq longitude using equation (4.6)

      TERM1 = SIN_PHI_POLE*COS(A_PHI)*COS(A_LAMBDA)   &
     &       +COS_PHI_POLE*SIN(A_PHI)
      TERM2 = COS(E_PHI)
      IF(TERM2 .LT. SMALL) THEN
        E_LAMBDA=0.0
      ELSE
        ARG=TERM1/TERM2
        ARG=MIN(ARG, 1.0)
        ARG=MAX(ARG,-1.0)
        E_LAMBDA=RECIP_PI_OVER_180*ACOS(ARG)
        E_LAMBDA=SIGN(E_LAMBDA,A_LAMBDA)
      ENDIF

! Scale longitude to range 0 to 360 degs

      IF(E_LAMBDA.GE.360.0) E_LAMBDA=E_LAMBDA-360.0
      IF(E_LAMBDA.LT.  0.0) E_LAMBDA=E_LAMBDA+360.0
      LAMBDA_EQ(I)=E_LAMBDA

!Li  Calculate turning angle for standard wind velocity

      E_LAMBDA=PI_OVER_180*LAMBDA_EQ(I)

! Formulae used are from eqs (4.19) and (4.21)

      TERM2=SIN(E_LAMBDA)
      ARG= SIN(A_LAMBDA)*TERM2*SIN_PHI_POLE      &
     &    +COS(A_LAMBDA)*COS(E_LAMBDA)
      ARG=MIN(ARG, 1.0)
      ARG=MAX(ARG,-1.0)
      TERM1=RECIP_PI_OVER_180*ACOS(ARG)
      ANGLED(I)=SIGN(TERM1,TERM2)
!Li

 200  CONTINUE

! Reset Lambda pole to the setting on entry to subroutine
      LAMBDA_POLE=LAMBDA_POLE_KEEP

      RETURN
      END SUBROUTINE LLTOEQANGLE
!Li


!Li
!LL  Subroutine PolarAnglD --------------------------------------------    
!LL                                                                        
!LL  Purpose:  Calculates the rotation angle required for polar region     
!LL            wave spectral conversion from map-east to local east        
!LL            reference directions.  The map-east is approximated with 
!LL            the local east of a rotated grid with its rotated pole on   
!LL            the Equator at 180E.  The formulation is intended for the 
!Li	       Arctic region only.  Not applicable for the S hemisphere.
!Li	                     Jian-Guo Li  18 Apr 2011.
!LL                                                                        
!* Arguments:--------------------------------------------------------    
      SUBROUTINE PolarAnglD ( PHI, LAMBDA, ANGLED, POINTS )            

      IMPLICIT NONE 

      INTEGER:: POINTS    !IN  Number of points to be processed  

      REAL, DIMENSION(POINTS) ::         &
     &        PHI,       & !IN  Latitude
     &        LAMBDA,    & !IN  Longitude
     &        ANGLED       !OUT turning angle in deg for standard wind

! Define local varables:-----------------------------------------------
      REAL :: A_LAMBDA, A_PHI, SIN_PHI, COS_PHI,  &
     &        ARG,  SIN_Alf, COS_Alf, SIN_ELn
      INTEGER   :: I   
      REAL, PARAMETER :: SMALL=1.0E-6

! Constants from comdecks:---------------------------------------------

      Real, Parameter :: Pi = 3.14159265358979323846  , &
     &                   Pi_Over_180 = Pi/180.0       , &
     &                   Recip_Pi_Over_180 = 180.0/Pi  

!*----------------------------------------------------------------------   

! 1. Initialise local constants

! 2. Transform from standard to equatorial latitude-longitude

      DO 200 I= 1, POINTS

!!  Check latitude PHI is in the northern polar region
      IF( PHI(I) .LT. 10.0 ) THEN
        WRITE(6, *) " Latitude < 10N, i, phi=", i, phi(i)
        RETURN
      ENDIF

! Scale longitude to range -180 to +180 degs

      A_LAMBDA=LAMBDA(I)
      IF(A_LAMBDA.GT. 180.0) A_LAMBDA=A_LAMBDA-360.
      IF(A_LAMBDA.LE.-180.0) A_LAMBDA=A_LAMBDA+360.

! Convert latitude & longitude to radians

      A_LAMBDA=PI_OVER_180*A_LAMBDA
      A_PHI=PI_OVER_180*PHI(I)

! Compute cosine of rotated latitude 

      SIN_ELn =  COS(A_PHI)*SIN(A_LAMBDA)
      SIN_PHI = -COS(A_PHI)*COS(A_LAMBDA)
      COS_PHI = SQRT( 1.0 - SIN_PHI*SIN_PHI )

! Compute cosine and sine of rotation angle

      COS_Alf = COS(A_LAMBDA)*SIN(A_PHI)/COS_PHI
      COS_Alf=MIN(COS_Alf, 1.0)
      COS_Alf=MAX(COS_Alf,-1.0)

! Calculate turning angle for standard wind velocity

      ARG = ACOS( COS_Alf )*RECIP_PI_OVER_180 
      ANGLED(I)=SIGN(ARG, SIN_ELn)

! AngleD is measured positive anticlockwise from the 
! rotated east to the standard east. So standard wind 
! has to turn by this angle for use in rotated grid.

!Li

 200  CONTINUE

      RETURN
      END SUBROUTINE PolarAnglD
!Li

