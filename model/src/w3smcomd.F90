      MODULE W3SMCOMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |        Chris Bunney, UKMO         |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         21-Jul-2021 |
!/                  +-----------------------------------+
!/
!/    18-Jan-2016 : Initial version.                    ( version 4.18 )
!/                        (Chris Bunney, UK Met Office)
!/    28-Sep-2016 : Bug fix to EXO and EYO calcs for    ( version 4.18 )
!/                  whole domain output. (C. Bunney, UK Met Office)
!/    05-Oct-2016 : Bug fix to calculation of indicies  ( version 4.18 )
!/                  into regular grid for type 2 output.
!/                  (C. Bunney, UK Met Office)
!/    29-Sep-2017 : Revise calculation of indicies to   ( version 4.18 )
!/                  ensure selected cells fall inside
!/                  user selected areas.
!/                  (A. Saulter, UK Met Office)
!/    18-Apr-2018 : Added Type 3 and 4 SMC output:      ( version 4.18 )
!/                  re-gridding to an arbitrary regular 
!/                  grid using pre-calculated indices and
!/                  weights (see WW3_SMCINT). Chris Bunney, UKMO
!/    20-Jun-2018   Updated routines to ensure that     ( version 4.18 )
!/                  directional fields are converted to 
!/                  degrees (nautical convention) on output.
!/                  (Chris Bunney, UK Met Office)
!/    27-Jun-2018   Ported to V6 (Chris Bunney)         ( version 6.05 )
!/    06-Jan-2021   Use ARCTC option for SMC grid. JGLi ( version 7.12 )
!/    20-Jul-2021   Fix bug where edge cells in design  ( version 7.12 )
!/                  grid may not be matched due where
!/                  SMC cell > base grid size.
!/    21-Jul-2021   Output grid definition variables    ( version 7.12 )
!/                  elevated to DOUBLE PRECISION. Fixed
!/                  bug in EXO/EYO calculation for full
!/                  grid extents. (Chris Bunney, UKMO)
!/
!/    Copyright 2009-2012 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Service module for support of SMC => regular grid output
!
!  2. Method :
!
!     For SMC grids, four types of output are possible:
!       1 - Flat grid (seapoint) output of SMC cells with associated
!           cell size variables (cx and cy). Requires extra effort to
!           plot as grid is not regular.
!
!       2 - Regular, uniformly gridded output to a specified output grid.
!           This is achieved by area averaging of the SMC cells. The output
!           grid will be aligned with the SMC grid cell edges which may 
!           result in the actual output grid being slightly different to
!           the original user request. A land/sea mask is created by
!           keeping track of the cell coverage and setting cells with
!           total coverage of <50% UNDEF.
!
!       3 - Arbitrary regualar grid re-gridding using indices and weights
!           generated from WW3_SMCINT. Uses the local gradient within the
!           grid cell and distance between cell centres to interpolate.
!
!       4 - As type 3, but with no interpolation - value from surrounding
!           SMC cell only.
!
!       Note - directional fields are expected to be passed to into
!              routines with units of RADIANS (CARTESIAN CONVENTION).
!              They will be OUTPUT in units of DEGREES (NAUTICAL CONVENTION).
!
      USE W3GDATMD
      USE CONSTANTS
      USE W3ODATMD, ONLY: UNDEF
        
      PUBLIC

      ! Output grid definition
      DOUBLE PRECISION        :: SXO, SYO, EXO, EYO      ! First lat/lons ...
      DOUBLE PRECISION        :: DXO, DYO                ! ... grid size ...
      INTEGER                 :: NXO, NYO                ! ... and number grid cells.

      ! Variables for SMC regridding (type 2 output):
      INTEGER                 :: SMCOTYPE                ! Type of SMC output
      INTEGER                 :: CELFAC                  ! Requested output cell scaling factor
      INTEGER, ALLOCATABLE    :: XIDX(:), YIDX(:)        ! Indices of SMC cells into regular grid
      INTEGER, ALLOCATABLE    :: XSPAN(:), YSPAN(:)      ! How many regualr grid cells SMC grid spans
      REAL, ALLOCATABLE       :: WTS(:), COV(:,:)        ! Regirdding weights and cell coverage (wet fraction)
      INTEGER, ALLOCATABLE    :: MAPSMC(:,:)             ! Regridded MAPSTA
      LOGICAL, ALLOCATABLE    :: SMCMASK(:)              ! Mask for type 1 output (flat array)
      INTEGER, ALLOCATABLE    :: SMCIDX(:)               ! Indices of SMC cells witin output grid domain
      
      INTEGER, ALLOCATABLE    :: smccx(:), smccy(:)      ! Cell size factors
      REAL                    :: dlat, dlon              ! Base lat/lon sizes
      INTEGER                 :: cfac                    ! SMC scaling factor

      REAL                    :: NOVAL                   ! Fill value for seapoints with no value

      ! Variables for SMC nearest neighbour interpolation (type 3/4 output)
      INTEGER, ALLOCATABLE    :: NNIDX(:,:)              ! nearest neighbour smc point to regular grid
      REAL, ALLOCATABLE       :: XDIST(:,:), YDIST(:,:)  ! distance to nearest neighbour
      INTEGER                 :: NDSMC                   ! ww3_smcint file unit number

      ! Counters:
      INTEGER                 :: SMCNOUT, NSMC

      CONTAINS

!--------------------------------------------------------------------------
      SUBROUTINE SMC_INTERP()
      ! This subroutine generates index or mask values for extraction
      ! of SMC data to either a flat grid or regular lat/lon grid,
      ! depending on the type of SMC output grid selected:
      !
      !   Type 1: Generates a mask for extracting only points from
      !           the user requested region.
      !
      !   Type 2: Calculate interpolation indices and weights for
      !           regridding the irregular SMC grid onto a regular,
      !           uniformly spaced lat/lon grid.
      !
      ! Chris Bunney, UK Met Office. 22-Oct-2015
      
      IMPLICIT NONE
      
      ! Locals
      REAL    :: CX0, CY0   ! SW corner of origin of grid
      REAL    :: S0CHK, XSNAP, YSNAP

      INTEGER :: ISEA, mx, my, ixx, iyy, J
      REAL    :: lat, lon

      J = 1
      NSMC = 0

      ! Determine smallest cell size factor:
      cfac = 2**(NRLv - 1)

      ! Get smallest SMC grid cells step size:
      dlat = SY / cfac
      dlon = SX / cfac
      ! SW Corner of grid origin cell:
      CX0 = X0 - SX / 2.
      CY0 = Y0 - SY / 2.

      ! Grid cell size to snap design grid to. Will be regular grid
      ! resolution for cellsize <= cfac, or cellsize for cellsize > cfac
      XSNAP = SX
      YSNAP = SY
      IF(CELFAC .gt. CFAC) XSNAP = CELFAC * dlon
      IF(CELFAC .gt. CFAC) YSNAP = CELFAC * dlat

      ! Get start lat,lon (must be aligned with SMC grid edges). Use
      ! regular grid origins if SXO or SYO is -999.9 (use full grid):
      IF(ABS(SXO + 999.9) .LT. 1E-4) THEN
          SXO = CX0
      ELSE
          S0CHK = CX0 + FLOOR((SXO - CX0) / XSNAP) * XSNAP
          ! Ensure first grid value falls within specified range
          IF (S0CHK .LT. SXO) THEN
              SXO = S0CHK + XSNAP
          ELSE
              SXO = S0CHK
          ENDIF
      ENDIF
      IF(ABS(SYO + 999.9) .LT. 1E-4) THEN
          SYO = CY0
      ELSE 
          S0CHK = CY0 + FLOOR((SYO - CY0) / YSNAP) * YSNAP
          ! Ensure first grid value falls within specified range
          IF (S0CHK .LT. SYO) THEN
              SYO = S0CHK + YSNAP
          ELSE
              SYO = S0CHK
          ENDIF
      ENDIF

      ! Use regular grid extents for last lat/lon if user
      ! specifies -999.9 for EXO/EYO (use full grid):
      IF(ABS(EXO + 999.9) .LT. 1E-4) THEN
          EXO = CX0 + SX * NX ! TRHC of last cell
      ENDIF
      IF(ABS(EYO + 999.9) .LT. 1E-4) THEN
          EYO = CY0 + SY * NY ! TRHC of last cell
      ENDIF

      ! Ouput grid cell dx/dy will be integer factor of smallest
      ! SMC grid cell size:
      DXO = dlon * celfac
      DYO = dlat * celfac

      ! Determine number of cells in grid:
      NXO = NINT((EXO - SXO) / DXO)
      NYO = NINT((EYO - SYO) / DYO)

      IF(SMCOTYPE .EQ. 2) THEN
         ! Initialise all indices to "missing":
         XIDX(:) = -1
         YIDX(:) = -1
      ENDIF

      ! Loop over cell array and calculate regidding factors:
      DO ISEA=1, NSEA
!          ! For grids with Arctic region: make sure we don't double count
!          ! the overlapping boundary cells. Also, don't process the arctic
!          ! cell (which is always the last cell).
!          ! Note: NARC contains ALL the boundary cells (global + arctic).
!          ! whereas NGLO contains only the global boundary cells.
!          IF(ISEA .GT. NGLO-NBAC .AND. ISEA .LT. NSEA-NARC+1) CYCLE
          IF( ARCTC .AND. & 
              ISEA .GT. NGLO-NBAC .AND. ISEA .LT. NSEA-NARC+1) CYCLE

          ! Get grid cell size:
          mx = IJKCel(3,ISEA)
          my = IJKCel(4,ISEA)

          ! Determine cell lat/lon (bottom left corner of cell)
          lon = CX0 + IJKCel(1,ISEA) * dlon
          lat = CY0 + IJKCel(2,ISEA) * dlat

          ! For output type 1 (seapoint array), just check whether
          ! cell centre is within specified domain range, and update
          ! output mask accordingly:
          IF( SMCOTYPE .EQ. 1 ) THEN
             ! Ensure longitude ranges are aligned
             lon = lon + 0.5 * mx * dlon
             lat = lat + 0.5 * my * dlat
             IF(lon .LT. SXO) lon = lon + 360.0
             IF(lon .GT. EXO) lon = lon - 360.0

             ! Now check if it is within range of requested domain:
             IF(lon .GE. SXO .AND. lon .LE. EXO .AND.           &
                lat .GE. SYO .AND. lat .LE. EYO ) THEN
                SMCMASK(ISEA) = .TRUE.
                SMCIDX(J) = ISEA
                J = J + 1
             ENDIF
             CYCLE
          ENDIF ! SMCOTYPE == 1

          ! For output type 2 (area averaged regular grid), determine
          ! SMC grid cell location and coverage in output grid:

          ! Align lons
          IF(lon .LT. SXO) THEN
              lon = lon + 360.
          ENDIF
          IF(lon .GT. EXO) THEN
              lon = lon - 360.
          ENDIF

          ! Find first SW cell in design grid:
          ! We add on 1/2 of the smallest SMC cell dlon/dlat values to ensure
          ! source grid cell ends up in the correct target grid cell (after 
          ! integer trunction):
          ixx = FLOOR((lon + 0.5*dlon - SXO) / DXO) + 1
          iyy = FLOOR((lat + 0.5*dlat - SYO) / DYO) + 1

          ! If we fall outside the left/bottom edge of the design grid,
          ! check for cases where the SMC cell has a lon or lat
          ! scaling factor > cfac (design grid is assumed to align
          ! its origin with cells of size cfac). For such cells,
          ! keep moving the left/bottom edge up by cfac until
          ! the SW corner (possibly) matches a design grid cell.
          IF(ixx .LE. 0 .AND. ixx + mx / celfac .GT. 0) THEN
              DO WHILE(mx .GT. cfac)
                mx = mx - cfac
                lon = lon + dlon * cfac
                ixx = FLOOR((lon + 0.5*dlon - SXO) / DXO) + 1
                IF(ixx .GT. 0) EXIT ! Found cell lon-edge in design grid
              ENDDO
          ENDIF
          IF(iyy .LE. 0 .AND. iyy + my / celfac .GT. 0) THEN
              DO WHILE(my .GT. cfac)
                my = my - cfac
                lat = lat + dlat * cfac
                iyy = FLOOR((lat + 0.5*dlat - SYO) / DYO) + 1
                IF(iyy .GT. 0) EXIT ! Found cell lat-edge in design grid
              ENDDO
          ENDIF

          ! If SMC cell definitely out of design grid domain, then cycle.
          IF(ixx .LE. 0 .OR. ixx .GT. NXO .OR.                          &
                iyy .LE. 0 .OR. iyy .GT. NYO) THEN
              xidx(ISEA) = -1
              yidx(ISEA) = -1
              CYCLE
          ENDIF

          XIDX(ISEA) = ixx
          YIDX(ISEA) = iyy
          NSMC = NSMC + 1
          SMCIDX(NSMC) = ISEA

          ! find out how many cells it covers in the x/y directions:
          XSPAN(ISEA) = MAX(1, INT(mx / CELFAC))
          YSPAN(ISEA) = MAX(1, INT(my / CELFAC))

          ! Do a bit of error checking (non fatal - just produced warning):
          IF(XSPAN(ISEA) .GT. 1) THEN
              IF(ABS((sxo+(ixx-1)*dxo) - lon) .GT. dxo/100.0) THEN
                  PRINT*, 'Potential problem with SMC grid cell span:'
                  PRINT*, xspan(ISEA), FLOAT(mx) / celfac
                  PRINT*, lon,lat
                  PRINT*, sxo+(ixx-1)*dxo,syo+iyy*dyo,dxo,dyo
                  PRINT*, "diff:", (sxo+(ixx-1)*dxo) - lon
              ENDIF
          ENDIF

          ! calc cell weight in relation to output grid:
          WTS(ISEA) = MIN(1., DBLE(MIN(CELFAC, mx) * MIN(CELFAC, my)) / &
                      (CELFAC**2))

      ENDDO

      ! Reset SXO and SYO to be the cell-centre (currently cell SW edge):
      SXO = SXO + 0.5 * DXO
      SYO = SYO + 0.5 * DYO

      END SUBROUTINE SMC_INTERP
      
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
      SUBROUTINE W3S2XY_SMCRG(S, XY)
      ! Regrid SMC data onto a regular grid using pre-calculated grid
      ! indices and weights.
      ! Chris Bunney, 02-Jul-2013
 
      IMPLICIT NONE

      ! Input parameters:
      REAL, INTENT(IN)  :: S(:)
      REAL, INTENT(OUT) :: XY(NXO,NYO)

      ! Local parameters
      INTEGER           :: I, J, IX, IY, ISEA, ISMC

      ! Initialise coverage and output arrays:
      COV(:,:) = 0.0
      XY(:,:) = 0.0

      DO ISMC=1,NSMC
         ISEA = SMCIDX(ISMC)

         IF(S(ISEA) .EQ. UNDEF) CYCLE   ! MDI

         ! Loop over number of spanned cells:
         DO I=0, XSPAN(ISEA) - 1
            DO J=0, YSPAN(ISEA) - 1
               IX = XIDX(ISEA) + I
               IY = YIDX(ISEA) + J

               ! Spans outside of grid?
               IF(IX .GT. NXO .OR. IY .GT. NYO) CYCLE

               ! Interpolate:
               XY(IX, IY) = XY(IX, IY) + S(ISEA) * WTS(ISEA) 
 
               ! Keep track of how much of cell is (wet) covered:
               COV(IX, IY) = COV(IX, IY) + WTS(ISEA)
            ENDDO
         ENDDO

      ENDDO 

      ! Create coastline by masking out areas with < 50% coverage:
      DO IX=1,NXO
         DO IY=1,NYO
            IF(MAPSMC(IX,IY) .EQ. 0) THEN
               ! Make land point
               XY(IX,IY) = UNDEF
            ELSE IF(COV(IX,IY) .LT. 0.5) THEN
               ! More than half of cell has UNDEF values - set to NOVAL:
               XY(IX,IY) = NOVAL 
            ELSE IF(COV(IX,IY) .LT. 1.0) THEN
               ! If coverage < 1.0, scale values back to full cell coverage.
               ! Without this step, points around coast could end up with lower
               ! waveheights due to weights not summing to 1.0:
               XY(IX,IY) = XY(IX,IY) * ( 1.0 / COV(IX,IY) )
            ENDIF
         ENDDO
      ENDDO

      RETURN

      END SUBROUTINE W3S2XY_SMCRG
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
      SUBROUTINE W3S2XY_SMCRG_DIR(S, XY)
      ! Regrid SMC data from a directional field onto a regular grid using
      ! pre-calculated grid indices and weights. As W3S2XY_SMC, but
      ! decomposes the field into u/v components first to ensure proper
      ! area averaging of directional data.

      ! Chris Bunney, 02-Jul-2013
 
      IMPLICIT NONE

      ! Input parameters:
      REAL, INTENT(IN)  :: S(:)
      REAL, INTENT(OUT) :: XY(NXO,NYO)

      ! Local parameters
      INTEGER           :: I, J, IX, IY, ISEA, ISMC
      REAL, ALLOCATABLE :: AUX1(:,:), AUX2(:,:)
      REAL              :: COSS, SINS

      ! Initialise coverage and output arrays:
      ALLOCATE(AUX1(NXO,NYO),AUX2(NXO,NYO))
      COV(:,:) = 0.0
      XY(:,:) = 0.0
      AUX1(:,:) = 0.0
      AUX2(:,:) = 0.0

      DO ISMC=1,NSMC
         ISEA = SMCIDX(ISMC)

         IF(S(ISEA) .EQ. UNDEF) CYCLE   ! MDI
         COSS = COS(S(ISEA))
         SINS = SIN(S(ISEA))

         ! Loop over number of spanned cells:
         DO I=0, XSPAN(ISEA) - 1
            DO J=0, YSPAN(ISEA) - 1
               IX = XIDX(ISEA) + I
               IY = YIDX(ISEA) + J

               ! Spans outside of grid?
               IF(IX .GT. NXO .OR. IY .GT. NYO) CYCLE

               ! Interpolate:
               !XY(IX, IY) = XY(IX, IY) + S(ISEA) * WTS(ISEA) 
               AUX1(IX, IY) = AUX1(IX, IY) + COSS * WTS(ISEA) 
               AUX2(IX, IY) = AUX2(IX, IY) + SINS * WTS(ISEA) 
 
               ! Keep track of how much of cell is (wet) covered:
               COV(IX, IY) = COV(IX, IY) + WTS(ISEA)
            ENDDO
         ENDDO

      ENDDO 

      ! Create coastline by masking out areas with < 50% coverage:
      DO IX=1,NXO
         DO IY=1,NYO
            IF(MAPSMC(IX,IY) .EQ. 0) THEN
               ! Make land point
               XY(IX,IY) = UNDEF
            ELSE IF(COV(IX,IY) .LT. 0.5) THEN
               ! More than half of cell has UNDEF values - set to NOVAL
               XY(IX,IY) = NOVAL
            ELSE IF(COV(IX,IY) .LT. 1.0) THEN
               ! If coverage < 1.0, scale values back to full cell coverage.
               ! Without this step, points around coast could end up with lower
               ! waveheights due to weights not summing to 1.0:
               XY(IX,IY) = ATAN2(AUX2(IX,IY), AUX1(IX,IY))
               XY(IX,IY) = MOD(630. - RADE * XY(IX,IY), 360. )
            ELSE
               XY(IX,IY) = ATAN2(AUX2(IX,IY), AUX1(IX,IY))
               XY(IX,IY) = MOD(630. - RADE * XY(IX,IY), 360. )
            ENDIF
         ENDDO
      ENDDO

      RETURN

      END SUBROUTINE W3S2XY_SMCRG_DIR
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
      SUBROUTINE MAPSTA_SMC()
      ! Calculates a new MATSTA using SMC grid cell averaging.
      ! Chris Bunney, 02-Jul-2013

      IMPLICIT NONE

      ! Local parameters
      INTEGER           :: I, J, IX, IY, IMX, IMY, ISEA

      ! Initialise coverage and output arrays:
      COV(:,:) = 0.0
      MAPSMC(:,:) = 0

      DO ISEA=1,NSEA
         IMX = MAPSF(ISEA,1)
         IMY = MAPSF(ISEA,2)

         IF(XIDX(ISEA) .EQ. -1) CYCLE   ! Out of grid

         ! Loop over number of spanned cells:
         DO I=0, XSPAN(ISEA) - 1
            DO J=0, YSPAN(ISEA) - 1
               IX = XIDX(ISEA) + I
               IY = YIDX(ISEA) + J

               ! Spans outside of grid?
               IF(IX .GT. NXO .OR. IY .GT. NYO) CYCLE

               ! MAPSTA values: 0=Excluded, (+-)1=Sea, (+-2)=Input boundary
               ! We will just keep track of sea and non-sea points:
               IF(MAPSTA(IMY, IMX) .NE. 0) THEN
                  ! Keep track of how much of cell is (wet) covered:
                  COV(IX, IY) = COV(IX, IY) + WTS(ISEA)
               ENDIF
            ENDDO
         ENDDO

      ENDDO 

      ! Create coastline by masking out areas with < 50% coverage:
      DO IX=1,NXO
         DO IY=1,NYO
            IF(COV(IX,IY) .LT. 0.5) THEN
               MAPSMC(IX, IY) = 0
            ELSE
               MAPSMC(IX, IY) = 1
            ENDIF
         ENDDO
      ENDDO

      RETURN

      END SUBROUTINE MAPSTA_SMC
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
      SUBROUTINE READ_SMCINT()
!     Reads the interpolation indices and distance weights from the
!     smcint.ww3 file generated by ww3_smcint.
!--------------------------------------------------------------------------
      USE W3SERVMD, ONLY: EXTCDE
      IMPLICIT NONE

      ! Locals
      INTEGER :: IERR, I, J
      REAL :: PLATO, PLONO ! Not used yet....future version might allow 
                           ! output to a rotated pole grid...

      NDSMC = 50
      OPEN(NDSMC, file='smcint.ww3', status='old', form='unformatted', iostat=ierr)
      IF(ierr .NE. 0) THEN
         WRITE(*,*) "ERROR! Failed to open smcint.ww3 for reading"
         CALL EXTCDE(1)
      ENDIF

      ! Header
      READ(NDSMC) NXO, NYO, SXO, SYO, DXO, DYO, PLONO, PLATO
      ALLOCATE(NNIDX(NXO,NYO), XDIST(NXO,NYO), YDIST(NXO,NYO))

      ! Indices and weights:
      READ(NDSMC)((NNIDX(I,J), XDIST(I,J), YDIST(I,J),I=1,NXO),J=1,NYO)

      CLOSE(NDSMC)

      END SUBROUTINE READ_SMCINT
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
      SUBROUTINE CALC_INTERP()
!     Calculates the interpolation indices and weights for regirdding
!     and SMC grid to an arbitrary regular grid. Index is that of 
!     SMC cell that contains output cell centre. Weights are the distance
!     in metres between the output and SMC cell centres.
!     A future version ~may~ allow for output grids to be on a rotated pole.
!--------------------------------------------------------------------------
      USE W3GDATMD, ONLY: CLATS
      USE CONSTANTS, ONLY : DERA, RADIUS
#ifdef W3_RTD
      USE W3SERVMD, ONLY: W3LLTOEQ
      USE W3GDATMD, ONLY: POLON, POLAT
#endif

      IMPLICIT NONE
      INTEGER :: IERR, I, J, ISEA, N, CFAC
      REAL :: mlon(NSEA), mlat(NSEA), olon(nxo,nyo), olat(nxo,nyo),     &
             ang(nxo,nyo), lon, lat
#ifdef W3_RTD
     REAL :: tmplon(nxo,nyo), tmplat(nxo,nyo)
#endif

      ! Determine smallest cell size factor:
      cfac = 2**(NRLv - 1)

      ! Get smallest SMC grid cells step size:
      dlat = SY / cfac
      dlon = SX / cfac

      ALLOCATE(xdist(nx,ny), ydist(ny,nx))

      ! Model lat/lons:
      DO ISEA = 1,NSEA
         mlon(isea) = (X0-0.5*SX) + (IJKCel(1,ISEA) + 0.5 * IJKCel(3,ISEA)) * dlon
         mlat(isea) = (Y0-0.5*SY) + (IJKCel(2,ISEA) + 0.5 * IJKCel(4,ISEA)) * dlat
      ENDDO

      ! Generate output grid cell centres:
      DO I=1,NXO
        DO J=1,NYO
           olon(i,J) = SXO + (I-1) * DXO
           olat(i,J) = SYO + (J-1) * DYO
        ENDDO
      ENDDO

#ifdef W3_RTD
          tmplat = olat
          tmplon = olon
          PRINT*,'Rotating coordinates'
          CALL W3LLTOEQ ( tmplat, tmplon, olat, olon,     &              
                            ang, POLAT, POLON, NXO*NYO )             
          PRINT*,'Rotating coordinates complete'
#endif

      ! Cycle over output grid points and find containing SMC cell:
      ! NOTE : BRUTE FORCE!
      NNIDX(:,:) = -1
      DO I=1,NXO
        PRINT*,I,' of ',NXO
        DO J=1,NYO
           lon = olon(i,j)
           lat = olat(i,j)
           IF(lon .LT. X0 - SX / 2) lon = lon + 360.0
           IF(lon .GT. (X0 + (NX-1) * SX) + 0.5 * SX) lon = lon - 360.0
           DO ISEA=1,NSEA
              IF(mlon(ISEA) - 0.5 * IJKCel(3,ISEA) * dlon .LE. lon .AND.    &
                    mlon(ISEA) + 0.5 * IJKCel(3,ISEA) * dlon .GE. lon .AND.    &
                    mlat(ISEA) - 0.5 * IJKCel(4,ISEA) * dlat .LE. lat .AND.    &
                    mlat(ISEA) + 0.5 * IJKCel(4,ISEA) * dlat .GE. lat ) THEN
                 ! Match!
                 NNIDX(I,J) = ISEA
                 xdist(I,J) = (lon - mlon(ISEA)) * DERA * RADIUS * CLats(ISEA)
                 ydist(I,J) = (lat - mlat(ISEA)) * DERA * RADIUS
                 EXIT
              ENDIF
           ENDDO
        ENDDO
      ENDDO

      END SUBROUTINE CALC_INTERP
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
      SUBROUTINE W3S2XY_SMCNN(S, XY, DIRN)
!     Fill regular grid using nearest SMC point data
!     Directional fields (DIRN=True) will be assumed to be in radians
!     and will be converted to degrees in nautical convention.
!--------------------------------------------------------------------------
      IMPLICIT NONE 

      ! Input parameters:
      REAL, INTENT(IN)    :: S(:)        ! Inupt array
      REAL, INTENT(OUT)   :: XY(NXO,NYO) ! Output data
      LOGICAL, INTENT(IN) :: DIRN        ! Directional field?

      ! Local parameters
      INTEGER           :: I, J, IX, IY, ISEA, ISMC
      DO IX = 1,NXO
        DO IY = 1,NYO
           ISEA = NNIDX(IX,IY) ! Nearest neighbour SMC point
           IF(ISEA .EQ. -1) THEN
              ! Land 
              XY(IX,IY) = UNDEF
           ELSE
              IF(S(ISEA) .EQ. UNDEF) THEN
                 ! Set undefined sea points to NOVAL
                 XY(IX,IY) = NOVAL
              ELSE
                 XY(IX,IY) = S(ISEA)
                 IF(DIRN) THEN
                   ! Convert direction fields to degrees nautical
                   XY(IX,IY) = MOD(630. - RADE * XY(IX,IY), 360.0)
                 ENDIF
              ENDIF
           ENDIF
        ENDDO
      ENDDO

      END SUBROUTINE W3S2XY_SMCNN

!--------------------------------------------------------------------------
      SUBROUTINE W3S2XY_SMCNN_INT(S, XY, DIRN)
!     Fill regular grid using nearest SMC point data and interpolate
!     output value based on local gradient and distance between grid
!     cell centres.
!     Directional fields (DIRN=True) will be assumed to be in radians
!     and will be converted to degrees in nautical convention.
!--------------------------------------------------------------------------
      USE W3PSMCMD, ONLY: SMCGradn
      IMPLICIT NONE 

      ! Input parameters:
      REAL, INTENT(IN)    :: S(:)        ! Input array
      REAL, INTENT(OUT)   :: XY(NXO,NYO) ! Output array
      LOGICAL, INTENT(IN) :: DIRN        ! Directional field?

      ! Locals
      INTEGER           :: I, J, IX, IY, ISEA, ISMC
      REAL              :: CVQ(-9:NSEA)
      REAL              :: GrdX(NSEA), GrdY(NSEA)

      ! Calculate local gradients:
      CVQ(1:NSEA) = S ! Need to copy S into array with bounds starting at -9
      CALL SMCGradn(CVQ, GrdX, GrdY, 0)

      ! Interpolate:
      DO IX = 1,NXO
        DO IY = 1,NYO
           ISEA = NNIDX(IX,IY) ! Nearest neighbour SMC point
           IF(ISEA .EQ. -1) THEN
              XY(IX,IY) = UNDEF
           ELSE
              ! Interpolate using local gradient and distance from cell centre:
              XY(IX,IY) = S(ISEA) + grdx(isea) * xdist(ix,iy) + grdy(isea) * ydist(ix,iy)
              IF(DIRN) THEN
                ! Convert direction fields to degrees nautical
                XY(IX,IY) = MOD(630. - RADE * XY(IX,IY), 360.0)
              ENDIF
           ENDIF
        ENDDO
      ENDDO

      END SUBROUTINE W3S2XY_SMCNN_INT
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
      SUBROUTINE W3S2XY_SMC(S, XY, DIR)
!     Entry point for SMC version of W3S2XY.
!     Dispatches to regridding subroutine based on SMCOTYPE.
!     Optional DIR logical specifies whether field is a directional
!     value; in which case it will be decomposed into u/v components
!     prior to any interpolation.
!--------------------------------------------------------------------------
      IMPLICIT NONE

      REAL, INTENT(IN)  :: S(:)
      REAL, INTENT(OUT) :: XY(NXO,NYO)
      LOGICAL, OPTIONAL :: DIR

      LOGICAL           :: DIRN
      INTEGER           :: ISEA

      IF(PRESENT(DIR)) THEN
         DIRN = DIR
      ELSE
         DIRN = .false.
      ENDIF

      IF(SMCOTYPE .EQ. 1) THEN
         ! Flat sea point array
         XY(:,1) = PACK(S, SMCMASK)
         IF(DIRN) THEN
            ! Convert to nautical convention in degrees
            DO ISEA=1,NXO
               IF(XY(ISEA,1) .NE. UNDEF) THEN
                XY(ISEA,1) = MOD(630. - RADE * XY(ISEA,1), 360.)
               ENDIF
            ENDDO
         ENDIF
      ELSEIF(SMCOTYPE .EQ. 2) THEN
         ! Regular gridded SMC cells
         IF(DIRN) THEN
            CALL W3S2XY_SMCRG_DIR(S, XY)
         ELSE
            CALL W3S2XY_SMCRG(S, XY)
         ENDIF
      ELSEIF(SMCOTYPE .EQ. 3) THEN
         ! Regridded to arbitrary regular grid with interpolation
         CALL W3S2XY_SMCNN_INT(S, XY, DIRN)
      ELSEIF(SMCOTYPE .EQ. 4) THEN
         ! Regridded to arbitrary regular grid - no interpolation
         CALL W3S2XY_SMCNN(S, XY, DIRN)
      ELSE
         WRITE(*,*) "Uknonwn SMC type!", SMCOTYPE
         ! Unknown SMC type!
         STOP
      ENDIF

      END SUBROUTINE W3S2XY_SMC
!--------------------------------------------------------------------------

      END MODULE W3SMCOMD
