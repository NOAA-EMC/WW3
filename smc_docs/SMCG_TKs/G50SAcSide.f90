!  This module is a common block similar in all AFT Model programs and is
!  written in FORTRAN 90.
!                     J G Li   26 Oct 2000
!! Adapted for multiple cell 2D advection tests using UNO schemes.
!!                    J G Li    8 Aug 2007
!! Adapted for global multiple cell grid   J G Li   12 Nov 2007
!! Modified for SMC extended over Arctic Ocean  J G Li   26 Nov 2008
!! Adapted for multi-resolution UK3 to Global 25km grid  J G Li   8 Feb 2010 
!! Modified to add minimum y-size in V-flux array.       J G Li  16 Feb 2010 
!! Adapted for 6-25km global ocean SMC grid.  J G Li   22 Feb 2010
!! Modified to use new rules on second cell selection.  J G Li   26 Feb 2010 
!! Restore G25 SMC grid for Arctic part.   J G Li   12 Oct 2010 
!! Rectify U-face boundary cell size.      J G Li    1 Apr 2011 
!! Adapted for SMC50 grid model.           J G Li   26 Oct 2011 
!!

MODULE Constants
   IMPLICIT NONE

! Parameters fixed in the program
   INTEGER,PARAMETER::NCL=110000, NFC=120000,   &
                    & NTS=576, NFTS=5, NWP=12,  &
                    & MSWP=9,  NLat=384, NLon=512, NPol=NLat/2
   REAL,PARAMETER:: BX=0.7031250, BY=0.4687500, BX0=-BX*0.5, BY0=-90.0, &
        &    DT=10.0, DTR=1.0/DT, DTF=6.0,  &
        &    AKH=1000.0, RCM=1.25

!  Some physical and atmospheric constants
   REAL,PARAMETER:: GRVTY=9.806,CPVAP=1004.5,RDRY=287.05, &
        &    CT0=273.16,CALJO=4.1868,PATM=101325.0,ANGUL=7.2921E-5,  &
        &    EPSLN=0.6220,CLIGHT=2.99792458E8, PIE=3.141592654

! Array variables to be used for data storage
   REAL::  CC0, FC0, AKV, RDF, HDF
   REAL, DIMENSION(0:NCL):: DX, DY, DXR, DYR, CHG
   INTEGER:: NU, NV, NC, NS, NT, N9, N8, N4, N2, N1
   INTEGER:: ICE(4,NCL), KG(NCL)
!  INTEGER, DIMENSION(7,NFC)::  ISD, JSD
   INTEGER, DIMENSION(7,NFC)::  ISD
   INTEGER, DIMENSION(8,NFC)::  JSD
   INTEGER:: I,II,IJ,IJK,J,JJ,JK,JKL,K,KK,KL,KLM,L,LL,LM,LMN,M,MM,MN,N,NN
   CHARACTER(LEN=8)::FLNAME(5), FLNM, FLZT, FL9NM*9 

! Initialised variables

   CHARACTER(LEN=1)::XEXT(6)=(/'S','B','W','E','C','M'/)

   CHARACTER(LEN=16):: RUNDATE=' SMC50 Oct 2011 '

END MODULE Constants

!!
!! This program genearte the one level 2D multiple cell grids.
!! Adapted for multiple cell 2D advection tests using UNO schemes.
!!                    J G Li   26 Jul 2007
!!

 PROGRAM AdapGrid 
      USE Constants
      IMPLICIT NONE

   REAL:: CNST, CNST1, CNST2, CNST3

!  Read Global Multiple-Cell info

       OPEN(UNIT=8,FILE='DatGMC/G50SMCBAr.dat',STATUS='OLD',IOSTAT=nn,ACTION='READ')
       IF(nn /= 0) PRINT*,' File G50SMCBAr.dat was not opened! '
          READ (8,*) LL, LM, MN
       DO J=1,LL
          READ (8,'(2i6,2i4,i6)') ICE(1,J), ICE(2,J), ICE(3,J), ICE(4,J), KG(J)
       END DO
       CLOSE(8)
       PRINT*, ' G50SMCBAr.dat read done  NC=', LL

!  Total cell number by combining two parts
       NC = LL 
       PRINT*, ' Fianl  NC, NG, NA '
       PRINT*,          NC, LM, MN

!  Output a few to check input values
       DO J=NC/10,NC,NC/10
          WRITE(6,'(i8,2i6,2i4,i6)') J, ICE(1,J), ICE(2,J), ICE(3,J), ICE(4,J), KG(J)
       END DO


!  Call subroutines to generate flux faces

   CALL CellSide

!     Open files to store writups
   OPEN(UNIT=16,FILE='G50ArCSide.txt',STATUS='UNKNOWN',IOSTAT=nn,ACTION='WRITE')
   IF(nn /= 0) PRINT*,' File messgs.txt was not opened! '

!     Header messages and configuration information 
      WRITE(UNIT=16,FMT='(1x/   &
        &  "  Multiple Cell 2D Grid Generation Output " /)' )

      WRITE(UNIT=16,FMT='(1x," Cell Units  BX  BY (m) = ",2f14.11)')  BX, BY
      WRITE(UNIT=16,FMT='(1x," Horizontal cell number = ",i8)' )  NC
      WRITE(UNIT=16,FMT='(1x," Size 1 2 4 cell number = ",3i8)')  N1, N2, N4
      WRITE(UNIT=16,FMT='(1x," Size 8 >8s cell number = ",3i8)')  N8, N9
      WRITE(UNIT=16,FMT='(1x," Lon/Lat/NPol grid No.s = ",3i8)')  NLon, NLat, NPol
      WRITE(UNIT=16,FMT='(1x," Total number of U-face = ",i8)' )  NU
      WRITE(UNIT=16,FMT='(1x," Total number of V-face = ",i8)' )  NV
 3912 FORMAT(1x,i4,3F9.1,ES12.3)
 638  FORMAT(5(i6,f8.2))

!    Close all files
      CLOSE(16)

 9999  PRINT*, ' AdapGrid completed '

 END PROGRAM AdapGrid 
!  End of main program


! Subroutine that generates the cell side information
 SUBROUTINE CellSide
   USE Constants
   IMPLICIT NONE
   REAL:: CNST, CNST1, CNST2, CNST3

!!    Test integer division for boundary cell numbers
!!    Size 2**n cell should bounded by -n cells
      DO ii=0, 8
         JJ=2**ii
         CNST=FLOAT(JJ)
         K=-INT( LOG(CNST)/LOG(2.) + 0.1)
      Write(6,*) " Cell size and boundary cell index =", JJ, K
      ENDDO

!     Generate i & j inter-sides for all cells
      WRITE(6,*) " Start creating inner face ..."

!     Generate i & j inter-sides for all cells at the highest level

      II=0
      JJ=0
!     DO L=1, NC
!!    Exclude last cell, the North Polar cell.
      DO L=1, NC-1

         IF(MOD(L, 5000) .eq. 0) WRITE(6,*) " Done L=", L, II, JJ

!!  Cyclic boundary for i-side at L-cell east side
         LM=ICE(1,L)+ICE(3,L)
         IF(LM .ge. NLon) LM=LM-NLon

!!  Cell height size is different from width size sometimes
         KL=ICE(2,L)+ICE(4,L)

!        DO M=1, NC
!!    Exclude last cell, the North Polar cell.
         DO M=1, NC-1

!!  Cyclic boundary for i-side at M-cell east side
            MN=ICE(1,M) + ICE(3,M)
            IF(MN .ge. NLon) MN=MN-NLon

!!  U-faces
            IF(( ICE(1,M) .eq. LM ) .AND.          &
     &         ( ICE(2,M)+ICE(4,M) .eq. KL .OR.    &
     &           ICE(2,M) .eq. ICE(2,L) ))  THEN  
               II=II+1
               ISD(1,II)=ICE(1,M)
               ISD(2,II)=MAX(ICE(2,M), ICE(2,L)) 
               ISD(3,II)=MIN(ICE(4,M), ICE(4,L)) 
               ISD(5,II)=L
               ISD(6,II)=M 
            ENDIF

!!   V-faces
            IF(( ICE(2,M) .eq. KL ) .AND.        &
               ( ICE(1,M) .eq. ICE(1,L) .OR. MN .eq. LM ))  THEN 
               JJ=JJ+1
               JSD(1,JJ)=MAX(ICE(1,M), ICE(1,L)) 
               JSD(2,JJ)=ICE(2,M)
               JSD(3,JJ)=MIN(ICE(3,M), ICE(3,L)) 
               JSD(5,JJ)=L
               JSD(6,JJ)=M 
!!  Minimum Y-size of the two bounding cells will be used to sort 
!!  cell sizes for multi-step implementation.
               JSD(8,JJ)=MIN(ICE(4,M), ICE(4,L)) 
            ENDIF
          END DO
      END DO
 
      ijk=II
      lmn=JJ

!     Set boundary u faces
      WRITE(6,*) " Start creating u boundary face II JJ=", II, JJ

!!    Exclude last cell, the North Polar cell.
      DO 111 L=1, NC-1
!!    Loop over all cells.
!     DO 111 L=1, NC
         i=0
         j=0
         ij=0
         k=0
         n=0
         kk=0
         
         IF(MOD(L, 10000) .eq. 0) WRITE(6,*) " Done L II=", L, II

!!    Cyclic boundary need to be taken into account
         LM=ICE(1,L)+ICE(3,L)
         IF(LM .ge. NLon)  LM=LM-NLon

!!    Loop through all inner faces 
         DO M=1, ijk

!!    See if the L cell west face is covered
            IF( ISD(1,M) .eq. ICE(1,L) ) THEN
                IF( ISD(2,M) .eq. ICE(2,L) ) THEN
                    i=1 
                    ij=ij+ISD(3,M)
                ELSEIF( ISD(2,M)+ISD(3,M) .eq. ICE(2,L)+ICE(4,L) ) THEN
                    j=1
                    ij=ij+ISD(3,M)
                ENDIF
            ENDIF

!!          and see if the L cell east face is covered
            IF( ISD(1,M) .eq. LM ) THEN
                IF( ISD(2,M) .eq. ICE(2,L) )  THEN 
                    k=1
                    kk=kk+ISD(3,M)
                ELSEIF( ISD(2,M)+ISD(3,M) .eq. ICE(2,L)+ICE(4,L) ) THEN
                    n=1
                    kk=kk+ISD(3,M)
                ENDIF
            ENDIF

!!  End of inner face M=1,ijk loop
         END DO

         IF(kk+ij .gt. 2*ICE(4,L) )  WRITE(6,*) "Over done i-side for cell L,ij,kk=", L, ij, kk
         IF(kk+ij .ge. 2*ICE(4,L) )  GOTO  111

          IF(ij .eq. 0)  THEN
!!  Full boundary cell for west side
               II=II+1
               ISD(1,II)=ICE(1,L)
               ISD(2,II)=ICE(2,L)
               ISD(3,II)=ICE(4,L)
!!  New boundary cells proportional to cell x-sizes 
!!  Updated for any 2**n sizes
!              ISD(5,II)=-INT( LOG(FLOAT(ISD(3,II)))/LOG(2.) + 0.01 )
               ISD(5,II)=-INT( LOG(FLOAT(ICE(3, L)))/LOG(2.) + 0.01 )
               ISD(6,II)=L
          ENDIF
          IF(kk .eq. 0)  THEN
!!  Full boundary cell for east side
               II=II+1
               ISD(1,II)=LM
               ISD(2,II)=ICE(2,L)
               ISD(3,II)=ICE(4,L)
               ISD(5,II)=L
!!  Updated for any 2**n sizes
!              ISD(6,II)=-INT( LOG(FLOAT(ISD(3,II)))/LOG(2.) + 0.01 )
               ISD(6,II)=-INT( LOG(FLOAT(ICE(3, L)))/LOG(2.) + 0.01 )
          ENDIF

!!  Half cell size west boundary faces
          IF(ij .gt. 0  .and. ij .lt. ICE(4,L) )  THEN
             IF( i .eq. 0 )  THEN
!!  lower half west cell face
               II=II+1
               ISD(1,II)=ICE(1,L)
               ISD(2,II)=ICE(2,L)
               ISD(3,II)=ICE(4,L)/2
!!  Updated for any 2**n sizes
               ISD(5,II)=-INT( LOG(FLOAT(ISD(3,II)))/LOG(2.) + 0.01 )
!!  Size 1 for cell 0, size 2 uses cell -1 and size 4 uses cell -2
               ISD(6,II)=L
             ENDIF
             IF( j .eq. 0 )  THEN
!!  Upper half west cell face
               II=II+1
               ISD(1,II)=ICE(1,L)
               ISD(2,II)=ICE(2,L)+ICE(4,L)/2
               ISD(3,II)=ICE(4,L)/2
!!  Updated for any 2**n sizes
               ISD(5,II)=-INT( LOG(FLOAT(ISD(3,II)))/LOG(2.) + 0.01 )
               ISD(6,II)=L
             ENDIF
          ENDIF

!!  Half cell size east boundary faces
          IF(kk .gt. 0  .and. kk .lt. ICE(4,L) )  THEN
             IF( k .eq. 0 )  THEN
!!  lower half east cell face
               II=II+1
               ISD(1,II)=LM
               ISD(2,II)=ICE(2,L)
               ISD(3,II)=ICE(4,L)/2
!!  Size 1 for cell 0, size 2 uses cell -1 and size 4 uses cell -2
               ISD(5,II)=L
!!  Updated for any 2**n sizes
               ISD(6,II)=-INT( LOG(FLOAT(ISD(3,II)))/LOG(2.) + 0.01 )
             ENDIF
             IF( n .eq. 0 )  THEN
!!  Upper half west cell face
               II=II+1
               ISD(1,II)=LM
               ISD(2,II)=ICE(2,L)+ICE(4,L)/2
               ISD(3,II)=ICE(4,L)/2
!!  Size 1 for cell 0, size 2 uses cell -1 and size 4 uses cell -2
               ISD(5,II)=L
!!  Updated for any 2**n sizes
               ISD(6,II)=-INT( LOG(FLOAT(ISD(3,II)))/LOG(2.) + 0.01 )
             ENDIF
          ENDIF

 111  CONTINUE


!     Set boundary v faces
      WRITE(6,*) " Start creating v boundary face II JJ=", II, JJ

!!    Exclude the last polar cell
      DO 222 L=1, NC-1
!!    Loop over all cells
!     DO 222 L=1, NC
         i=0
         j=0
         ij=0
         k=0
         n=0
         nn=0
         
         IF(MOD(L, 10000) .eq. 0) WRITE(6,*) " Done L JJ=", L, II

!!    Loop through all V faces already set 
         DO M=1, lmn

!!    See if the L cell south face is covered
            IF( JSD(2,M) .eq. ICE(2,L) ) THEN
               IF( JSD(1,M) .eq. ICE(1,L) ) THEN
                  i=1
                  ij=ij+JSD(3,M)
               ELSEIF( JSD(1,M)+JSD(3,M) .eq. ICE(1,L)+ICE(3,L) ) THEN
                  j=1
                  ij=ij+JSD(3,M)
               ENDIF
            ENDIF
!!          and see if the L cell north face is covered
            IF( JSD(2,M) .eq. ICE(2,L) + ICE(4,L) )  THEN 
               IF( JSD(1,M) .eq. ICE(1,L) ) THEN 
                  k=1
                  nn=nn+JSD(3,M)
               ELSEIF( JSD(1,M)+JSD(3,M) .eq. ICE(1,L)+ICE(3,L) )  THEN
                  n=1
                  nn=nn+JSD(3,M)
               ENDIF
            ENDIF

!!   End M=1, lmn V-side loop
         END DO

         IF(nn+ij .gt. 2*ICE(3,L) )  WRITE(6,*)  "Over done j-side for L, ij, nn=", L, ij, nn
         IF(nn+ij .ge. 2*ICE(3,L) )  GOTO  222

          IF(ij .eq. 0)  THEN
!!  Full boundary cell for south side
               JJ=JJ+1
               JSD(1,JJ)=ICE(1,L)
               JSD(2,JJ)=ICE(2,L)
               JSD(3,JJ)=ICE(3,L)
!!  New boundary cells proportional to cell sizes 
!!  Updated for any 2**n sizes
               JSD(5,JJ)=-INT( LOG(FLOAT(ICE(3,L)))/LOG(2.) + 0.01 )
               JSD(6,JJ)=L
               JSD(8,JJ)=ICE(4,L)
!!  No cells over Antarctic land so there is no S Polar cell.
          ENDIF
          IF(nn .eq. 0)  THEN
!!  Full boundary cell for north side
               JJ=JJ+1
               JSD(1,JJ)=ICE(1,L)
               JSD(2,JJ)=ICE(2,L)+ICE(4,L)
               JSD(3,JJ)=ICE(3,L)
               JSD(5,JJ)=L
!!  North polar cell takes the whole last 4 rows above JSD=ICE(2,NC).
!!  Note ICE(2,L) represents lower-side of the cell.  Polar cell is the last cell NC.
             IF( ICE(2,L)+ICE(4,L) .eq. ICE(2,NC) ) THEN
               JSD(6,JJ)=NC
               WRITE(6,*) "Set north pole v face for cell L", L
             ELSE
!!  Updated for any 2**n sizes
               JSD(6,JJ)=-INT( LOG(FLOAT(ICE(3,L)))/LOG(2.) + 0.01 )
             ENDIF
               JSD(8,JJ)=ICE(4,L)
          ENDIF

!!  Half cell size south boundary faces
          IF(ij .gt. 0  .and. ij .lt. ICE(3,L) )  THEN
             IF( i .eq. 0 )  THEN
!!  left half cell face
               JJ=JJ+1
               JSD(1,JJ)=ICE(1,L)
               JSD(2,JJ)=ICE(2,L)
               JSD(3,JJ)=ICE(3,L)/2
!!  New boundary cells proportional to cell sizes 
!!  Updated for any 2**n sizes
               JSD(5,JJ)=-INT( LOG(FLOAT(JSD(3,JJ)))/LOG(2.) + 0.01 )
               JSD(6,JJ)=L
               JSD(8,JJ)=ICE(4,L)
             ENDIF
             IF( j .eq. 0 )  THEN
!!  right half cell face
               JJ=JJ+1
               JSD(1,JJ)=ICE(1,L)+ICE(3,L)/2
               JSD(2,JJ)=ICE(2,L)
               JSD(3,JJ)=ICE(3,L)/2
!!  New boundary cells proportional to cell sizes 
!!  Updated for any 2**n sizes
               JSD(5,JJ)=-INT( LOG(FLOAT(JSD(3,JJ)))/LOG(2.) + 0.01 )
               JSD(6,JJ)=L
               JSD(8,JJ)=ICE(4,L)
             ENDIF
          ENDIF

!!  Half cell size north boundary faces
          IF(nn .gt. 0  .and. nn .lt. ICE(3,L) )  THEN
             IF( k .eq. 0 )  THEN
!!  left half north cell face
               JJ=JJ+1
               JSD(1,JJ)=ICE(1,L)
               JSD(2,JJ)=ICE(2,L)+ICE(4,L)
               JSD(3,JJ)=ICE(3,L)/2
               JSD(5,JJ)=L
!!  New boundary cells proportional to cell sizes 
!!  Updated for any 2**n sizes
               JSD(6,JJ)=-INT( LOG(FLOAT(JSD(3,JJ)))/LOG(2.) + 0.01 )
               JSD(8,JJ)=ICE(4,L)
             ENDIF
             IF( n .eq. 0 )  THEN
!!  right half north cell face
               JJ=JJ+1
               JSD(1,JJ)=ICE(1,L)+ICE(3,L)/2
               JSD(2,JJ)=ICE(2,L)+ICE(4,L)
               JSD(3,JJ)=ICE(3,L)/2
               JSD(5,JJ)=L
!!  New boundary cells proportional to cell sizes 
!!  Updated for any 2**n sizes
               JSD(6,JJ)=-INT( LOG(FLOAT(JSD(3,JJ)))/LOG(2.) + 0.01 )
               JSD(8,JJ)=ICE(4,L)
             ENDIF
          ENDIF

 222  CONTINUE

!   Store top level U V side numbers in NU NV 
      NU=II
      NV=JJ

!!  Loop over all u faces to find the second cells next to the L and M cells
!!  Boundary cells will be duplicated for second cells
      WRITE(6,*) " Find extra second u cell II JJ=", II, JJ
      DO i=1, NU
         L=ISD(5,i)
         M=ISD(6,i)
         kk=0
         nn=0

!!  Boundary L cell just duplicate it as LL cell
         IF(L .LE. 0) THEN
            ISD(4,i)=L
            kk=1
         ELSE
!!  Find the second LL cell by loop over all faces again
!!  The two U faces have to share at least one y-end.
!!  The second U face should be no less than this face. 
            DO k=1, NU
               IF( (L .EQ. ISD(6,k)) .AND. (ISD(3,i) .LE. ISD(3,k)) .AND.  &
     &             ( (ISD(2,i)+ISD(3,i) .eq. ISD(2,k)+ISD(3,k)) .or.       &
     &               (ISD(2,i) .eq. ISD(2,k)) ) )  THEN
                   ISD(4,i)=ISD(5,k)
                   kk=1
               ENDIF
            ENDDO
         ENDIF

!!  Boundary M cell just duplicate it as MM cell
         IF(M .LE. 0) THEN
            ISD(7,i)=M
            nn=1
         ELSE
!!  Find the second LL cell by loop over all faces again
!!  The two U faces have to share at least one y-end.
            DO n=1, NU
               IF( (M .EQ. ISD(5,n)) .AND. (ISD(3,i) .LE. ISD(3,n)) .AND.   &
     &             ( (ISD(2,i)+ISD(3,i) .eq. ISD(2,n)+ISD(3,n)) .or.        &
     &               (ISD(2,i) .eq. ISD(2,n)) ) )  THEN
                   ISD(7,i)=ISD(6,n)
                   nn=1
               ENDIF
            ENDDO
         ENDIF

!!  Duplicate central cell if upstream cells are not selected.
         IF( kk .eq. 0) THEN
!           WRITE(6,*) " L cell duplicated for U face i=", i
            ISD(4,i)=L
         ENDIF
         IF( nn .eq. 0) THEN
!           WRITE(6,*) " M cell duplicated for U face i=", i
            ISD(7,i)=M
         ENDIF

         IF(MOD(i, 10000) .eq. 0) WRITE(6,*) " Done U face i=", i

!!  End of u face loop
      ENDDO

!!  Loop over all v faces to find the second cells next to the L and M cells
!!  Boundary cells will be duplicated for second cells
      WRITE(6,*) " Find extra second v cell II JJ=", II, JJ

      DO j=1, NV
         L=JSD(5,j)
         M=JSD(6,j)
         kk=0
         nn=0

!!  Boundary L cell just duplicate it as LL cell
         IF(L .LE. 0) THEN
            JSD(4,j)=L
            kk=1
         ELSE
!!  Find the second LL cell by loop over all faces again
!!  The two V faces have to share at least one x-end.
            DO k=1, NV
               IF( (L .EQ. JSD(6,k)) .AND. (JSD(3,j) .LE. JSD(3,k)) .AND.  &
     &             ( (JSD(1,j)+JSD(3,j) .eq. JSD(1,k)+JSD(3,k)) .or.       &
     &               (JSD(1,j) .eq. JSD(1,k)) ) )  THEN 
                   JSD(4,j)=JSD(5,k)
                   kk=1
               ENDIF
            ENDDO
         ENDIF

!!  Boundary M cell just duplicate it as MM cell
!!  Duplicate north polar cell as well
         IF(M .LE. 0 .OR. M .EQ. NC) THEN
            JSD(7,j)=M
            nn=1
         ELSE
!!  Find the second LL cell by loop over all faces again
!!  The two V faces have to share at least one x-end.
            DO n=1, JJ
               IF( (M .EQ. JSD(5,n)) .AND. (JSD(3,j) .LE. JSD(3,n)) .AND.  &
     &             ( (JSD(1,j)+JSD(3,j) .eq. JSD(1,n)+JSD(3,n)) .or.       &
     &               (JSD(1,j) .eq. JSD(1,n)) ) )  THEN 
                   JSD(7,j)=JSD(6,n)
                   nn=1
               ENDIF
            ENDDO
         ENDIF

         IF( kk .eq. 0) THEN
!            WRITE(6,*) " L cell duplicated for V face j=", j
             JSD(4,j)=L
         ENDIF
         IF( nn .eq. 0) THEN
!            WRITE(6,*) " M cell duplicated for V face j=", j
             JSD(7,j)=M
         ENDIF

         IF(MOD(j, 10000) .eq. 0) WRITE(6,*) " Done V face j=", j

!!  End of v face loop
      ENDDO

!!  Check whether any overlaping exists
      WRITE(6,*) " Check any overlaping NU NV=", NU, NV

      ij=0
      DO i=1, NU-1
         L=ISD(1,i)
         M=ISD(2,i)
            DO k=i+1, NU
               IF( L .EQ. ISD(1,k) .AND. M .EQ. ISD(2,k) )  THEN
                 ij=ij+1
                 WRITE(6,*) ij, ' Overlaping u face k, i, j, l, mm, m, n, nn' 
                 WRITE(6,333) i, (ISD(n,i), n=1,7)
                 WRITE(6,333) k, (ISD(n,k), n=1,7)
               ENDIF
            ENDDO
         IF(MOD(i, 10000) .eq. 0) WRITE(6,*) " Checked U face i=", i
      ENDDO

 333  FORMAT(7I8)

      ij=0
      DO j=1, NV-1
         L=JSD(1,j)
         M=JSD(2,j)
            DO k=j+1, NV
               IF( L .EQ. JSD(1,k) .AND. M .EQ. JSD(2,k) )  THEN
                 ij=ij+1
                 WRITE(6,*) ij, ' Overlaping v face k, i, j, l, mm, m, n, nn'
                 WRITE(6,333) j, (JSD(n,j), n=1,7)
                 WRITE(6,333) k, (JSD(n,k), n=1,7)
               ENDIF
            ENDDO
         IF(MOD(j, 10000) .eq. 0) WRITE(6,*) " Checked V face j=", j
      ENDDO


!!  Output ISD JSD variables for later use
      WRITE(6,*) " Storing face array info NU,NV=", NU, NV

   OPEN(UNIT=10,FILE='G50AISide.d',STATUS='UNKNOWN',IOSTAT=nn)
   IF(nn /= 0) PRINT*,' File Pros was not opened! '
!     WRITE(10,FMT='(1x,i8)') NU
      DO I=1,NU
         WRITE(10,FMT='(2i6,i4,4i8)')  (ISD(N,I), N=1,7)
      END DO
   CLOSE(10)

   OPEN(UNIT=11,FILE='G50AJSide.d',STATUS='UNKNOWN',IOSTAT=nn)
   IF(nn /= 0) PRINT*,' File Pros was not opened! '
!     WRITE(11,FMT='(1x,i8)') NV
      DO J=1,NV
!        WRITE(11,FMT='(2i6,i4,4i8)')  (JSD(N,J), N=1,7)
         WRITE(11,FMT='(2i6,i4,4i8,i4)')  (JSD(N,J), N=1,8)
      END DO
   CLOSE(11)

   PRINT*, ' I J-Sides output done '

 999  PRINT*, ' Sub CellSide ended.'

      RETURN

 END SUBROUTINE CellSide


