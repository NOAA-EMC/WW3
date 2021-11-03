#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      PROGRAM W3GRID_INTERP
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |             A. Chawla             |SX
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Jun-2021 |
!/                  +-----------------------------------+
!/
!/    15-Mar-2007 : Origination.                        ( version 3.13 )
!/    24-Sep-2007 : Original code                       ( version 3.14 )
!/    01-Aug-2011 : Modified to match grid output       ( version 4.01 )
!/    20-Feb-2013 : Modified for new output fields      ( version 4.11 )
!/    11-Nov-2013 : Update for curvilinear grids        ( version 4.13 )
!/    22-Jan-2014 : Update for UNST grids (F. Ardhuin)  ( version 4.18 )
!/    30-Apr-2014 : Add group 3 (M. Accensi)            ( version 5.00 )
!/    26-Jul-2018 : Write weights file WHTGRIDINT.bin
!/                  (F.Ardhuin, M.Accensi, J.H.Alves)   ( version 6.05 )
!/    31-Aug-2018 : Update groups 2,4,6,8 (S. Zieger)   ( version 6.05 )
!/    26-Jan-2021 : Added TP field (derived from FP)    ( version 7.12 )
!/    22-Mar-2021 : New coupling fields output          ( version 7.13 )
!/    02-Jun-2021 : Bug fix (*SUMGRD; Q. Liu)           ( version 7.13 )
!/
!   1. Purpose : 
!
!   Re-gridding binary output (out_grd.* files) to another grid 
!
!   2. Method :
!
!   Data is interpolated from a combination of base grids to the target
!   grid. For each grid, if resolution is coarser or similar to target
!   grid then a linear interpolation approach is used. On the other hand
!   if resolution is much higher then an averaging technique based on 
!   cell areas. 
!   Total number of base grids to be used for interpolation together with
!   their (and target grid) file extns are read from 'ww3_gint.inp'. 
!   Base grids can be arranged in any order but the target grid should 
!       always be the last grid. 
!
!   3. Parameters : 
!
!   4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3NMOD    Subr. W3GDATMD Set number of model.
!      W3SETG    Subr.   Id.    Point to selected model.
!      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
!      NEXTLN    Subr. W3SERVMD Get next line from input file
!      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
!      ITRACE    Subr.   Id.    Subroutine tracing initialization.
!      STRACE    Subr.   Id.    Subroutine tracing.
!      W3NOUT    Subr. W3ODATMD Set number of model for output.
!      W3SETO    Subr.   Id.    Point to selected model for output.
!      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
!      W3SETW    Subr. W3WDATMD Point to selected model for wave data.
!      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
!      W3SETA    Subr.   Id.    Point to selected model for aux data.
!      W3DIMA      Subr.   Id.    Assign memory for aux data.
!      W3GRMP    Func. W3GSRUMD Compute interpolation coeff. from grid.
!      W3CKCL    Func.   Id.    Check if point lies within grid cell. 
!      W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
!     ----------------------------------------------------------------
!  
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!     Checks on input, checks on determining the interpolation weights
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!       !/S     Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
        USE CONSTANTS
!/
        USE W3IOGRMD
        USE W3TIMEMD
        USE W3IOGOMD, ONLY : W3IOGO
        USE W3ADATMD, ONLY : W3DIMA, W3NAUX, W3SETA
        USE W3GDATMD
        USE W3ODATMD, ONLY : FNMPRE, NOGRP, NGRPP, OUTPTS, UNDEF, FLOGRD,      &
                             NAPROC, NOSWLL, IDOUT
        USE W3ODATMD, ONLY : W3NOUT, W3SETO
        USE W3IDATMD
        USE W3WDATMD, ONLY : W3NDAT, W3DIMW, W3SETW
        USE W3WDATMD, ONLY : WDATAS, TIME, WLV, ICE, ICEH, ICEF,               &
                             UST, USTDIR, ASF, RHOAIR
        USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY : STRACE
#endif
        USE W3ARRYMD, ONLY : PRTBLK
        USE W3GSRUMD
        USE W3TRIAMD
!/
        IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local data structure
!/
        TYPE GR_WT
           INTEGER              :: NP
           INTEGER, ALLOCATABLE :: IP(:), JP(:)
           REAL,    ALLOCATABLE :: WT(:)
           REAL                 :: AR
        END TYPE GR_WT
!
        TYPE GR_INT
           INTEGER              :: NGRDS
           INTEGER, ALLOCATABLE :: GDID(:)
           TYPE(GR_WT), ALLOCATABLE :: IND_WTS(:)
        END TYPE GR_INT
!/
!/ Local variables
!/

        TYPE(GR_INT), TARGET, ALLOCATABLE :: GR_INTS(:)
        INTEGER                 :: I, J, IERR, NG, IG, JG, ISEA, IX, IY, IXT
        INTEGER                 :: IYT, NS, COUNTF, COUNTG, NOSWLL_MIN, ITOUT
        INTEGER                 :: NDSM, NDSI, NDSE, NDSO, NDSTRC, NTRACE, IOTST
        INTEGER                 :: INTMETHOD, NSEA_FILE
        INTEGER, ALLOCATABLE    :: FIDOUT(:), MAP(:,:), TMP_INDX(:)
        REAL                    :: SXT, SYT, XT, YT, XTT
        DOUBLE PRECISION        :: DAREA, SAREA
        REAL                    :: XCRNR(5),YCRNR(5),DT(4),DX,DY,XSUB,YSUB
        INTEGER                 :: TOUT(2), NOUT, IOUT
        REAL                    :: DTREQ, DTEST
        INTEGER                 :: IS(4), JS(4)
        INTEGER                 :: MAPINT
        REAL                    :: RW(4), SUMWT
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
        REAL, ALLOCATABLE       :: INT_MAP(:,:)
        LOGICAL                 :: L360=.FALSE., LPLC, INGRD, BRNCHCL, BRNCHCR, INGRID
        CHARACTER               :: COMSTR*1, IDTIME*23, FNAMEWHT*32
!
!---------------------------------------------------------------------------
! 1. Initialization
!
        NDSM   = 20
        NDSI   = 10
        NDSE   = 6
        NDSO   = 6
!
        NDSTRC = 6
        NTRACE = 10

!
!---------------------------------------------------------------------------
! 2.  I/O Setup
!
!
        J      = LEN_TRIM(FNMPRE)
        OPEN(NDSI,FILE=FNMPRE(:J)//'ww3_gint.inp',STATUS='OLD', ERR=2000, &
             IOSTAT=IERR)
        WRITE (NDSO,900)
!
        CALL ITRACE ( NDSTRC, NTRACE )
#ifdef W3_S
        CALL STRACE (IENT, 'W3GRIDINT')
#endif
!---------------------------------------------------------------------------
! 3.  Read and process input file upto number of grids
! 3.a Get comment character
!
        REWIND (NDSI)
        READ (NDSI,'(A)',END=2001,ERR=2002) COMSTR
        IF ( COMSTR .EQ. ' ' ) COMSTR = '$'
        WRITE (NDSO,901) COMSTR
!
! 3.b Read starting time, time step and number of outputs
!
        CALL NEXTLN ( COMSTR, NDSI, NDSE )
        READ (NDSI,*,END=2001,ERR=2002) TOUT, DTREQ, NOUT
        DTREQ  = MAX ( 0. , DTREQ )
        IF ( DTREQ.EQ.0 ) NOUT = 1
        NOUT   = MAX ( 1 , NOUT )
!
        CALL STME21 ( TOUT , IDTIME )
        WRITE (NDSO,902) IDTIME, DTREQ, NOUT
!
! 3.c Read number of grids and allocate memory
!   
        CALL NEXTLN ( COMSTR, NDSI, NDSE )
        READ (NDSI,*,END=2001,ERR=2002) NG
        WRITE (NDSO,903) NG
!
        CALL W3NMOD (NG, 6, 6)
        CALL W3NDAT (    6, 6)
        CALL W3NAUX (    6, 6)
        CALL W3NINP (    6, 6)
        CALL W3NOUT(     6, 6)
!
! 3.d Read file extensions for each of the grids and 
!     the grid information from the corresponding mod_def files
!
        NOSWLL_MIN = 9999999
        CALL NEXTLN ( COMSTR, NDSI, NDSE )
!
        DO IG = 1,NG
          READ (NDSI,*,END=2001,ERR=2002) GRIDS(IG)%FILEXT
          WRITE (NDSO,904) IG,GRIDS(IG)%FILEXT
!
          CALL W3SETO( IG, 6, 6)
          CALL W3SETA( IG, 6, 6)
          CALL W3SETW( IG, 6, 6)
          CALL W3SETG( IG, 6, 6)
          CALL W3IOGR ('READ', NDSM, IG, GRIDS(IG)%FILEXT)
          WRITE (NDSO,905) NX, NY, GTYPE, ICLOSE 

          IF ( ICLOSE .EQ. ICLOSE_TRPL ) THEN
             WRITE(NDSE,*)'PROGRAM W3GRID_INTERP HAS NOT BEEN '//  &
                  'TESTED WITH TRIPOLE GRIDS. STOPPING NOW.'
             CALL EXTCDE ( 1 )
          END IF

          IF ( IG .NE. NG .AND. NOSWLL_MIN .GE. OUTPTS(IG)%NOSWLL ) THEN
             NOSWLL_MIN = OUTPTS(IG)%NOSWLL
          END IF
!
        END DO
!
        IF ( NOSWLL_MIN .NE. OUTPTS(NG)%NOSWLL ) THEN
           WRITE (NDSO,907) NOSWLL_MIN, OUTPTS(NG)%NOSWLL
           NOSWLL_MIN = MIN (NOSWLL_MIN,OUTPTS(NG)%NOSWLL)
        END IF

        CALL NEXTLN ( COMSTR, NDSI, NDSE )
        READ (NDSI,'(I1)',END=2001,ERR=2002) INTMETHOD
        WRITE (NDSO,917) INTMETHOD

!
! 3.e Allocate memory for integration map and initialize with grid status map
!
        ALLOCATE(INT_MAP(NX,NY),MAP(NX,NY))
        INT_MAP = 0.0
!        MAP = TRANSPOSE(MAPSTA)
        DO IX = 1,NX
          DO IY = 1,NY
            IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
              MAP(IX,IY) = -1
            END IF
          END DO
        END DO
!
!---------------------------------------------------------------------------
! 4.  Determine interpolation weights for output grids
!
!
! 4.a Point to output grid and allocate space for interpolation weights
!
        CALL W3SETG( NG, 6, 6)
        WRITE (NDSO,908) NSEA
!
        ALLOCATE ( GR_INTS(NSEA) )
!
        IF ( FLAGLL ) THEN
          IF ( MINVAL ( XGRD ) .LT. 0 .OR.                                     &
               MAXVAL ( XGRD ) .GT. 180.0 ) L360 = .TRUE. 
        END IF
!
! 4.b Check if weight files exist or create it
!
        FNAMEWHT='WHTGRIDINT.bin'
        OPEN (994,FILE=FNMPRE(:J)//TRIM(FNAMEWHT),FORM='UNFORMATTED',IOSTAT=IERR,STATUS='OLD')
        NSEA_FILE = 0
        IF (IERR.EQ.0) READ(994) NSEA_FILE ! basic consistency check ... 
        IF (NSEA_FILE.EQ.NSEA) THEN 
          DO ISEA = 1, NSEA
            READ(994) COUNTG
            ALLOCATE ( GR_INTS(ISEA)%IND_WTS(COUNTG),GR_INTS(ISEA)%GDID(COUNTG) ) 
            DO IG = 1,COUNTG
              READ(994) GR_INTS(ISEA)%IND_WTS(IG)%AR
              READ(994) GR_INTS(ISEA)%GDID(IG)
              READ(994) COUNTF
              ALLOCATE ( GR_INTS(ISEA)%IND_WTS(IG)%IP(COUNTF),         &
                         GR_INTS(ISEA)%IND_WTS(IG)%JP(COUNTF),         &
                         GR_INTS(ISEA)%IND_WTS(IG)%WT(COUNTF) )
              DO I = 1,COUNTF
                READ(994) GR_INTS(ISEA)%IND_WTS(IG)%IP(I)
                READ(994) GR_INTS(ISEA)%IND_WTS(IG)%JP(I)
                READ(994) GR_INTS(ISEA)%IND_WTS(IG)%WT(I)
              END DO
              READ(994) GR_INTS(ISEA)%IND_WTS(IG)%NP
            END DO ! IG
            READ(994) GR_INTS(ISEA)%NGRDS
          END DO ! ISEA

        ELSE
          OPEN (994,FILE=FNMPRE(:J)//TRIM(FNAMEWHT),FORM='UNFORMATTED',IOSTAT=IERR)

!
! 4.b Loop through the wet points
!
        DO ISEA = 1, NSEA
!
!          IF (MOD(ISEA,NINT(REAL(NSEA)/100)).EQ.1)  &
!            WRITE(6,*) 'Treating point ',ISEA,' out of ', NSEA
          IX = MAPSF(ISEA,1)
          IY = MAPSF(ISEA,2)
          DAREA = ABS(GSQRT(IY,IX))
!
          ALLOCATE ( GR_INTS(ISEA)%IND_WTS(NG-1),GR_INTS(ISEA)%GDID(NG-1) )
!
! 4.b.i Loop through the input grids for each wet point
!
          COUNTG = 0
          DO IG = 1,NG-1
!       
! 4.b.ii Check if point is enclosed in grid domain
!
            INGRID=.FALSE.
            IF (GRIDS(IG)%GTYPE .EQ. UNGTYPE) THEN
              ! Look for a triangle at the coarse cell center
              CALL IS_IN_UNGRID(IG, XGRD(IY,IX), YGRD(IY,IX),  &
                                                 ITOUT, IS, JS, RW)
              IF (ITOUT.GT.0) INGRID=.TRUE.
!              ! If extrapolation activated, force to find if a triangles is inside 
!              ! the coarse grid cell even if there is no triangle in the cell center
              IF (INTMETHOD.EQ.1) THEN
                WRITE(991,'(2I6,2F9.4,I8,3I8,3F5.3)') IX,IY,XGRD(IY,IX), YGRD(IY,IX), ITOUT, IS(1:3), RW(1:3)
                IF (ITOUT.EQ.0)  WRITE(992,*) IX,IY,ISEA,XGRD(IY,IX), YGRD(IY,IX)
                IF (ITOUT.EQ.0) THEN 
                   CALL IS_IN_UNGRID2(IG, XGRD(IY,IX), YGRD(IY,IX), INTMETHOD,  &
                                                 ITOUT, IS, JS, RW)
                   WRITE(993,'(2I6,2F9.4,I8,3I8,3F6.3)') IX,IY,XGRD(IY,IX), YGRD(IY,IX), ITOUT, IS(1:3), RW(1:3)
                ENDIF  
                !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX)+DX, YGRD(IY,IX), ITOUT, IS, JS, RW)
                !IF (ITOUT.GT.0) INGRID=.TRUE.
                !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX)-DX, YGRD(IY,IX), ITOUT, IS, JS, RW)
                !IF (ITOUT.GT.0) INGRID=.TRUE.
                !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX), YGRD(IY,IX)+DY, ITOUT, IS, JS, RW)
                !IF (ITOUT.GT.0) INGRID=.TRUE.
                !IF (ITOUT.EQ.0) CALL IS_IN_UNGRID(IG, XGRD(IY,IX), YGRD(IY,IX)-DY, ITOUT, IS, JS, RW)
                !IF (ITOUT.GT.0) INGRID=.TRUE.
              END IF           
            ELSE
              IF ( W3GRMP ( GRIDS(IG)%GSU, XGRD(IY,IX), YGRD(IY,IX), IS,        &
                   JS, RW ) ) INGRID=.TRUE.
              END IF
           IF (INGRID) THEN 
!
! 4.b.iii Check source grid resolution vs target grid resolution 
!         (averaging used for finer resolution source grids)
!
             IF (GRIDS(IG)%GTYPE .EQ. UNGTYPE) THEN 
                SAREA = GRIDS(IG)%TRIA(ITOUT)
             ELSE 
               DO I = 1,4
                 XCRNR(I) = GRIDS(IG)%XGRD(JS(I),IS(I))
                 YCRNR(I) = GRIDS(IG)%YGRD(JS(I),IS(I))
               END DO
               XCRNR(5) = XCRNR(1)
               YCRNR(5) = YCRNR(1)
               DO I = 1,4
                 IF ( ABS (XCRNR(I+1)-XCRNR(I)) .GT. 180. .AND.                 &
                   GRIDS(IG)%ICLOSE .EQ.  ICLOSE_SMPL ) THEN
                   DT(I) = SQRT ( (ABS(XCRNR(I+1)-XCRNR(I))-360.)**2 +          &
                                      (YCRNR(I+1)-YCRNR(I))**2 )
                 ELSE
                   DT(I) = SQRT ( (XCRNR(I+1)-XCRNR(I))**2 +                    &
                                  (YCRNR(I+1)-YCRNR(I))**2 )
                 END IF
               END DO
               SXT = 0.5*(DT(1)+DT(3))
               SYT = 0.5*(DT(2)+DT(4))
               SAREA = (SXT*SYT)
             END IF
             NS = NINT(DAREA/SAREA)
!
             IF ( NS .LE. 2 .OR. GRIDS(IG)%GTYPE .EQ. UNGTYPE ) THEN
! FA: Quick fix for UNST type grids: always perform interpolation 
!     To be updated later ... 
!
! 4.b.iv Counting the contributing nodes to re-normalize the weights RW
! 
                ALLOCATE ( TMP_INDX(4) )
                COUNTF = 0
                SUMWT = 0.0
                DO I = 1,4 
! The following two IF tests are separated because for triangles, JS(4)=IS(4)=0
                  IF ( RW(I) .GT. 0.0 ) THEN 
                    ! MAPSTA == 0 indicated excluded point (either land
                    ! or truly excluded) 
                    IF ( GRIDS(IG)%MAPSTA(JS(I),IS(I)) .NE. 0) THEN
                      COUNTF = COUNTF+1
                      TMP_INDX(COUNTF) = I
                      SUMWT = SUMWT + RW(I)
                    END IF 
                  END IF
                END DO
!
! 4.b.v  Interpolating to target grid
! 
                IF ( COUNTF .GT. 0 ) THEN                
! Should use SAREA info to prevent the increment of COUNTG ... 
! what about islands / land in triangle meshes? they are not part of the triangles... 
                  COUNTG = COUNTG + 1
                  IF (COUNTG.GT.1) THEN 
                    IF (SAREA.LT.0.5*GR_INTS(ISEA)%IND_WTS(COUNTG-1)%AR) THEN 
                       DO JG=1,COUNTG-1
                         DEALLOCATE (GR_INTS(ISEA)%IND_WTS(JG)%IP)
                         DEALLOCATE (GR_INTS(ISEA)%IND_WTS(JG)%JP)
                         DEALLOCATE (GR_INTS(ISEA)%IND_WTS(JG)%WT)
                         END DO
                       COUNTG=1
                       END IF
                    END IF

                  GR_INTS(ISEA)%IND_WTS(COUNTG)%AR = SAREA
                  GR_INTS(ISEA)%GDID(COUNTG) = IG
                  INT_MAP(IX,IY) = REAL( IG )

                  ALLOCATE ( GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(COUNTF),         &
                             GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(COUNTF),         &
                             GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(COUNTF) )
                  DO I = 1,COUNTF
                    GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(I) = IS(TMP_INDX(I))
                    GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(I) = JS(TMP_INDX(I))
                    GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(I) = RW(TMP_INDX(I))/SUMWT
                  END DO
                  GR_INTS(ISEA)%IND_WTS(COUNTG)%NP = COUNTF
                END IF
                DEALLOCATE ( TMP_INDX )
!
              ELSE
! 
! 4.b.vi Find the averaging points for higher resolution grid
!        Step 1 : Compute the corners of the cell
!
                X0 = XGRD(IY,IX)
                Y0 = YGRD(IY,IX)
                IF ( IX .GT. 1 .AND. IX .LT. NX .AND. IY .GT. 1                &
                     .AND. IY .LT. NY ) THEN
                  XT = XGRD(IY-1,IX+1)
                  YT = YGRD(IY-1,IX+1)
                  IF ( ABS(XT-X0) .GT. 270 ) THEN
                    XT = XT - SIGN(360.,XT-X0)
                    END IF
                  XCRNR(1) = 0.5*(XT+X0)
                  YCRNR(1) = 0.5*(YT+Y0)
                  XT = XGRD(IY+1,IX+1)
                  YT = YGRD(IY+1,IX+1)
                  IF ( ABS(XT-X0) .GT. 270 ) THEN
                    XT = XT - SIGN(360.,XT-X0)
                    END IF
                  XCRNR(2) = 0.5*(XT+X0)
                  YCRNR(2) = 0.5*(YT+Y0)
                  XT = XGRD(IY+1,IX-1)
                  YT = YGRD(IY+1,IX-1)
                  IF ( ABS(XT-X0) .GT. 270 ) THEN
                    XT = XT - SIGN(360.,XT-X0)
                    END IF
                  XCRNR(3) = 0.5*(XT+X0)
                  YCRNR(3) = 0.5*(YT+Y0)
                  XT = XGRD(IY-1,IX-1)
                  YT = YGRD(IY-1,IX-1)
                  IF ( ABS(XT-X0) .GT. 270 ) THEN
                    XT = XT - SIGN(360.,XT-X0)
                    END IF
                  XCRNR(4) = 0.5*(XT+X0)
                  YCRNR(4) = 0.5*(YT+Y0)
                ELSEIF ( IX .EQ. 1 ) THEN
                  IF ( IY .EQ. 1 ) THEN
                    XT = XGRD(IY+1,IX+1)     
                    YT = YGRD(IY+1,IX+1)     
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                      END IF
                    XCRNR(2) = 0.5*(XT+X0)
                    YCRNR(2) = 0.5*(YT+Y0)
                    XCRNR(4) = 2*X0 - XCRNR(2)
                    YCRNR(4) = 2*Y0 - YCRNR(2)
                    XCRNR(3) = X0 - (YCRNR(2)-Y0) 
                    YCRNR(3) = Y0 + (XCRNR(2)-X0) 
                    XCRNR(1) = 2*X0 - XCRNR(3)
                    YCRNR(1) = 2*Y0 - YCRNR(3)
                  ELSEIF ( IY .EQ. NY ) THEN
                    XT = XGRD(IY-1,IX+1)     
                    YT = YGRD(IY-1,IX+1)     
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(1) = 0.5*(XT+X0)
                    YCRNR(1) = 0.5*(YT+Y0)
                    XCRNR(3) = 2*X0 - XCRNR(1)
                    YCRNR(3) = 2*Y0 - YCRNR(1)
                    XCRNR(2) = X0 - (Y0-YCRNR(1)) 
                    YCRNR(2) = Y0 + (X0-XCRNR(1)) 
                    XCRNR(4) = 2*X0 - XCRNR(2)
                    YCRNR(4) = 2*Y0 - YCRNR(2)
                  ELSE
                    XT = XGRD(IY-1,IX+1)
                    YT = YGRD(IY-1,IX+1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(1) = 0.5*(XT+X0)
                    YCRNR(1) = 0.5*(YT+Y0)
                    XT = XGRD(IY+1,IX+1)
                    YT = YGRD(IY+1,IX+1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(2) = 0.5*(XT+X0)
                    YCRNR(2) = 0.5*(YT+Y0)
                    XCRNR(3) = 2*X0 - XCRNR(1)
                    YCRNR(3) = 2*Y0 - YCRNR(1)
                    XCRNR(4) = 2*X0 - XCRNR(2)
                    YCRNR(4) = 2*Y0 - YCRNR(2)
                  ENDIF
                ELSEIF ( IX .EQ. NX ) THEN
                  IF ( IY .EQ. 1 ) THEN
                    XT = XGRD(IY+1,IX-1)     
                    YT = YGRD(IY+1,IX-1)     
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(3) = 0.5*(XT+X0)
                    YCRNR(3) = 0.5*(YT+Y0)
                    XCRNR(2) = X0 - (YCRNR(3)-Y0) 
                    YCRNR(2) = Y0 + (XCRNR(3)-X0) 
                    XCRNR(1) = 2*X0 - XCRNR(3)
                    YCRNR(1) = 2*Y0 - YCRNR(3)
                    XCRNR(4) = 2*X0 - XCRNR(2)
                    YCRNR(4) = 2*Y0 - YCRNR(2)
                  ELSEIF ( IY .EQ. NY ) THEN
                    XT = XGRD(IY-1,IX-1)     
                    YT = YGRD(IY-1,IX-1)     
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(4) = 0.5*(XT+X0)
                    YCRNR(4) = 0.5*(YT+Y0)
                    XCRNR(3) = X0 - (YCRNR(4)-Y0) 
                    YCRNR(3) = Y0 + (XCRNR(4)-X0) 
                    XCRNR(1) = 2*X0 - XCRNR(3)
                    YCRNR(1) = 2*Y0 - YCRNR(3)
                    XCRNR(2) = 2*X0 - XCRNR(4)
                    YCRNR(2) = 2*Y0 - YCRNR(4)
                  ELSE
                    XT = XGRD(IY+1,IX-1)
                    YT = YGRD(IY+1,IX-1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(3) = 0.5*(XT+X0)
                    YCRNR(3) = 0.5*(YT+Y0)
                    XT = XGRD(IY-1,IX-1)
                    YT = YGRD(IY-1,IX-1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(4) = 0.5*(XT+X0)
                    YCRNR(4) = 0.5*(YT+Y0)
                    XCRNR(1) = 2*X0 - XCRNR(3)
                    YCRNR(1) = 2*Y0 - YCRNR(3)
                    XCRNR(2) = 2*X0 - XCRNR(4)
                    YCRNR(2) = 2*Y0 - YCRNR(4)
                  ENDIF
                ELSE
                  IF ( IY .EQ. 1 ) THEN
                    XT = XGRD(IY+1,IX+1)
                    YT = YGRD(IY+1,IX+1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(2) = 0.5*(XT+X0)
                    YCRNR(2) = 0.5*(YT+Y0)
                    XT = XGRD(IY+1,IX-1)
                    YT = YGRD(IY+1,IX-1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(3) = 0.5*(XT+X0)
                    YCRNR(3) = 0.5*(YT+Y0)
                    XCRNR(4) = 2*X0 - XCRNR(2)
                    YCRNR(4) = 2*Y0 - YCRNR(2)
                    XCRNR(1) = 2*X0 - XCRNR(3)
                    YCRNR(1) = 2*Y0 - YCRNR(3)
                  ELSE
                    XT = XGRD(IY-1,IX-1)
                    YT = YGRD(IY-1,IX-1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(4) = 0.5*(XT+X0)
                    YCRNR(4) = 0.5*(YT+Y0)
                    XT = XGRD(IY-1,IX+1)
                    YT = YGRD(IY-1,IX+1)
                    IF ( ABS(XT-X0) .GT. 270 ) THEN
                      XT = XT - SIGN(360.,XT-X0)
                    END IF
                    XCRNR(1) = 0.5*(XT+X0)
                    YCRNR(1) = 0.5*(YT+Y0)
                    XCRNR(2) = 2*X0 - XCRNR(4)
                    YCRNR(2) = 2*Y0 - YCRNR(4)
                    XCRNR(3) = 2*X0 - XCRNR(1)
                    YCRNR(3) = 2*Y0 - YCRNR(1)
                  END IF
                END IF
                BRNCHCL = .FALSE. 
                BRNCHCR = .FALSE. 
                IF ( FLAGLL .AND. ICLOSE .EQ. ICLOSE_SMPL ) THEN
                  IF ( L360 ) THEN
                    IF ( MINVAL ( XCRNR(1:4) ) .LT.   0.0 ) BRNCHCL = .TRUE. 
                    IF ( MAXVAL ( XCRNR(1:4) ) .GT. 360.0 ) BRNCHCR = .TRUE. 
                  ELSE
                    IF ( MINVAL ( XCRNR(1:4) ) .LT. -180.0 ) BRNCHCL = .TRUE. 
                    IF ( MAXVAL ( XCRNR(1:4) ) .GT.  180.0 ) BRNCHCR = .TRUE. 
                  END IF
                END IF
!
!        Step 2 : Loop through source grid to find all active points in cell
!
!FA : why only *5 ???... 
! 
                ALLOCATE ( TMP_INDX(NS*5) )
                COUNTF = 0
                DO I = 1, GRIDS(IG)%NSEA
                  IXT = GRIDS(IG)%MAPSF(I,1)
                  IYT = GRIDS(IG)%MAPSF(I,2)
                  XT = GRIDS(IG)%XGRD(IYT,IXT)
                  YT = GRIDS(IG)%YGRD(IYT,IXT)
!
                  IF ( FLAGLL ) THEN
                    IF (  L360 ) THEN
                      IF ( XT .LT. 0 ) XT = XT + 360.
                    ELSE
                      IF ( XT .GT. 180. ) XT = XT - 360.
                    END IF
                  END IF
                  INGRD = W3CKCL (FLAGLL,XT,YT,4,XCRNR,YCRNR,LPLC)
                  IF ( INGRD ) THEN
                    COUNTF = COUNTF+1
                    TMP_INDX(COUNTF) = I
                  ELSEIF ( BRNCHCL .AND. GRIDS(IG)%ICLOSE                      &
                           .EQ. ICLOSE_SMPL ) THEN
                    XTT = XT - 360.0
                    INGRD = W3CKCL (FLAGLL,XTT,YT,4,XCRNR,YCRNR,LPLC)
                    IF ( INGRD ) THEN
                      COUNTF = COUNTF+1
                      TMP_INDX(COUNTF) = I
                    END IF
                  ELSEIF ( BRNCHCR .AND. GRIDS(IG)%ICLOSE                  &
                          .EQ. ICLOSE_SMPL ) THEN
                    XTT = XT + 360.0
                    INGRD = W3CKCL (FLAGLL,XTT,YT,4,XCRNR,YCRNR,LPLC)
                    IF ( INGRD ) THEN
                      COUNTF = COUNTF+1
                      TMP_INDX(COUNTF) = I
                    END IF
                  END IF
                END DO
!
!        Step 3 : Save interior points for equal wt. interpolation (averaging)
!
                IF ( COUNTF .NE. 0 ) THEN
                  COUNTG = COUNTG + 1
                  GR_INTS(ISEA)%GDID(COUNTG) = IG
                  INT_MAP(IX,IY) = REAL( IG )
                  ALLOCATE ( GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(COUNTF),         &
                             GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(COUNTF),         &
                             GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(COUNTF) )
                  DO I = 1,COUNTF
                    IXT = GRIDS(IG)%MAPSF(TMP_INDX(I),1)
                    IYT = GRIDS(IG)%MAPSF(TMP_INDX(I),2)
                    GR_INTS(ISEA)%IND_WTS(COUNTG)%IP(I) = IXT
                    GR_INTS(ISEA)%IND_WTS(COUNTG)%JP(I) = IYT
                    GR_INTS(ISEA)%IND_WTS(COUNTG)%WT(I) = 1./( REAL(COUNTF) )
                  END DO
                  GR_INTS(ISEA)%IND_WTS(COUNTG)%NP = COUNTF
                END IF   
                DEALLOCATE ( TMP_INDX )
!
              END IF  ! End of check for grid resolution
!
            END IF    ! End of check for point inside grid
!
          END DO      ! End of loop through all input grids
!
          GR_INTS(ISEA)%NGRDS = COUNTG
! 
! 4.b.vii Check to see if interpolation weights found.  
!        Status of output points with / without weights set in MAPST2
!        using the next available bit
!
          IF ( GR_INTS(ISEA)%NGRDS .EQ. 0 ) THEN
#ifdef W3_T
            WRITE (NDSO,909)IX, IY
#endif
            MAPINT = 1
            MAPST2(IY,IX) = MAPST2(IY,IX) + MAPINT*16 
            MAPSTA(IY,IX) = -ABS ( MAPSTA(IY,IX) )
          END IF
!
        END DO       ! End of loop through all wet points
!
! Now dumps the coefficients to file ...
        WRITE(994) NSEA
        DO ISEA = 1, NSEA
          COUNTG = GR_INTS(ISEA)%NGRDS
          WRITE(994) COUNTG              
          DO IG = 1,COUNTG                   
            WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%AR
            WRITE(994) GR_INTS(ISEA)%GDID(IG)
            COUNTF = GR_INTS(ISEA)%IND_WTS(IG)%NP
            WRITE(994) COUNTF
            DO I = 1,COUNTF
              WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%IP(I)
              WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%JP(I)
              WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%WT(I)
            END DO
            WRITE(994) GR_INTS(ISEA)%IND_WTS(IG)%NP
          END DO ! IG
          WRITE(994) GR_INTS(ISEA)%NGRDS
        END DO ! ISEA
        END IF ! NSEA.EQ.NSEA_FILE	
        CLOSE(994)
!
! 4.c Print Interpolation grids map
!
        IX = 1+NX/24
        IY = 1+NY/24
        CALL PRTBLK ( NDSO, NX, NY, NX, INT_MAP, MAP, -1, 1., 1, NX, IX, 1,    &
                      NY, IY, 'Grid Interpolation Map', ' ' )
!
!---------------------------------------------------------------------------
! 5   Output interpolations
!
! 5.a Set-up dimensions for target grid outputs and allocate file pointers
!
        CALL W3SETA(NG, 6, 6)
        CALL W3DIMA(NG, 6, 6, .TRUE. )
        CALL W3DIMW(NG, 6, 6, .TRUE. )
        ALLOCATE(FIDOUT(NG))
        DO IG = 1,NG
          FIDOUT(IG) = 30 + (IG-1)*10
        END DO
!
! 5.b Initialize and read the first set of fields for base grids
!
        DO IG = 1,NG-1
          CALL W3SETO( IG, 6, 6)
          CALL W3IOGO('READ',FIDOUT(IG),IOTST,IG)
          IF ( IOTST .NE. 0 ) THEN
            GO TO 2111
          ENDIF
        END DO
!
! 5.c Setup the output flag options for the target grid
!
        WRITE (NDSO,910)
        DO I = 1, NOGRP
          OUTPTS(NG)%OUT1%FLOGRD(I,:) = OUTPTS(1)%OUT1%FLOGRD(I,:)
          WRITE (NDSO,911) I
          IF (I.LT.9) THEN
            WRITE (NDSO, 912) (OUTPTS(NG)%OUT1%FLOGRD(I,J),J=1,NGRPP)
           ELSE
            WRITE (NDSO, 913)
          END IF
        END DO
        WRITE (NDSO, 915)
!
!     Print output flags in human readable from. Mark
!     groups that do not make sense to interpolate to
!     target grid (e.g. Groups 9, 10).
!
        DO I=1, NOGRP
         DO J=1, NGRPP
           IF ( OUTPTS(NG)%OUT1%FLOGRD(I,J) ) THEN
             IF ( I .EQ. 4 .AND. J .EQ. 8 ) THEN
               WRITE (NDSO, 916) I,IDOUT(I,J), '*** NOT IMPLEMENTED ***'
               OUTPTS(NG)%OUT1%FLOGRD(I,J) = .FALSE.
             ELSE IF ( I .LE. 8 ) THEN
               WRITE (NDSO, 916) I,IDOUT(I,J), ' '
             ELSE
               WRITE (NDSO, 916) I,IDOUT(I,J), '*** NOT IMPLEMENTED ***'
               OUTPTS(NG)%OUT1%FLOGRD(I,J) = .FALSE.
             END IF
           END IF
         END DO
        END DO
        WRITE (NDSO, 915)
!
! 5.d Carry out interpolation in an infinite loop till appropriate 
!     time steps are interpolated 
!
        IOUT = 0
!
        DO
          DTEST = DSEC21 ( WDATAS(1)%TIME, TOUT )
          IF ( DTEST .GT. 0. ) THEN
            DO IG = 1,NG-1
              CALL W3IOGO('READ',FIDOUT(IG),IOTST,IG)
              IF ( IOTST .NE. 0 ) THEN
                GO TO 2111
              ENDIF
            END DO
            CYCLE
          ENDIF
          IF ( DTEST .LT. 0. ) THEN
            CALL TICK21 ( TOUT , DTREQ )
            CYCLE
          END IF
!
          IOUT = IOUT + 1
          CALL STME21 ( TOUT, IDTIME)
          WRITE (NDSO,914) IDTIME
!
          WDATAS(NG)%TIME = WDATAS(1)%TIME
          CALL W3SETO(NG, 6, 6)
          CALL W3SETG(NG, 6, 6)
          CALL W3SETA(NG, 6, 6)
          CALL W3SETW(NG, 6, 6)
!
          CALL W3EXGI ( NG-1, NSEA, NOSWLL_MIN, INTMETHOD )
!
          CALL TICK21 ( TOUT , DTREQ )
          IF ( IOUT .GE. NOUT ) EXIT
        END DO
        GOTO 2222
!
!---------------------------------------------------------------------------
! Escape locations read errors :
!
 2000   CONTINUE
        WRITE (NDSE,1000) IERR
        CALL EXTCDE ( 1 )
!
 2001   CONTINUE
        WRITE(NDSE,1001)
        CALL EXTCDE ( 2 )
!
 2002   CONTINUE
        WRITE(NDSE,1002) IERR
        CALL EXTCDE ( 3 )
!
 2111   CONTINUE
        WRITE(NDSO,950)
 2222   CONTINUE
        WRITE(NDSO,999)
!
!---------------------------------------------------------------------------
! Formats
!
 900    FORMAT (/15X,'    *** WAVEWATCH III Grid interpolation ***    '/       &
               15X,'==============================================='/)
 901    FORMAT ( '  Comment character is ''',A,''''/)
 902    FORMAT ( '  Time Information : '/                                      &
                 '---------------------------------------------'/              &
                 '    Starting Time      : ',A/                                &
                 '    Interval (in sec)  : ',F10.2/                            &
                 '    Number of requests : ',I4/                               &
                 '---------------------------------------------')
 903    FORMAT ( '  Number of grids (including output grid) =',I3/)
 904    FORMAT ( /'  Extension for grid ',I3,' is --> ',A10/)
 905    FORMAT ( '    Grid Particulars are : '/                                &
                 '      Dimensions =  ',2(I9,2X)/                              &
                 '       Grid Type = ',I3,'  ==> 1 Rect, 2 Curv, 3 Unstr'/     &
                 '    Grid Closure = ',I3,'  ==> -1 None, 2 Simple, 8 Tripolar')
 907    FORMAT ( /' NOTE :  The no. of swell partitions from input and',       &
                    ' target grids do not match',/                             &
                  '  The Min. no. of partitions from input grids =',I5/        &
                  '        The no. of partitions for target grid =',I5/        &
                  '  Interpolation will be limited to the smaller',            &
                  ' number of the partitions,',/                               &
                  '  rest will be marked undefined.'                           )
 908    FORMAT (/'  Preparing interpolation weights for output grid ' /        &
                 '  Total number of wet points for interpolation ',I7/)
 909    FORMAT (/'  *** WARNING !! No interpolation points at ',2(I5)/)
 910    FORMAT (/'  Interpolating fields .... '/)
 911    FORMAT ('     Output group   ', I5)
 912    FORMAT ('     Output variable flags are -> ',7(5L2,1X))
 913    FORMAT ('     Output variables skipped')
 914    FORMAT ( '        OUTPUT TIME : ',A)
 915    FORMAT ( '   ------------------------------------------------')
 916    FORMAT ( I5,A,2X,A)
 917    FORMAT (/'   Interpolation scheme = ',I1,'  ==> 0 linear, ',           &
                '1 extrapolate unstructured, 2 nearest'/)
 950    FORMAT (/'  End of file reached'/)
 999    FORMAT (/15X,'    *** End of Grid interpolation Routine ***    '/      &
                 15X,'==============================================='/)
!
 1000   FORMAT (/' *** ERROR IN WAVEGRID_INTERP : '/                           &
                 '     ERROR IN OPENING INPUT FILE'/                           &
                 '     IOSTAT =',I5/)
 1001   FORMAT (/' *** ERROR IN WAVEGRID_INTERP : '/                           &
                 '     PREMATURE END IN INPUT FILE'/)
 1002   FORMAT (/' *** ERROR IN WAVEGRID_INTERP : '/                           &
                 '     ERROR IN READING FROM INPUT FILE'/                      &
                 '     IOSTAT =',I5/)
!
!/
!/ Internal Subroutine 
!/
!/ Internal Subroutine W3EXGI ----------------------------------------------/
!/
        CONTAINS
!/ -----------------------------------------------------------------------/
        SUBROUTINE W3EXGI ( NGRD, NSEA, NOSWLL_MIN, INTMETHOD )
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |             A. Chawla             |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    09-Jul-2009 : Original code                       ( version 3.14 )
!/    21-Feb-2013 : Modified to new output structure    ( version 4.11 )
!/    30-Apr-2014 : Add group 3                         ( version 5.00 )
!/    27-Aug-2015 : ice thick. and floe added as output ( version 5.10 )
!/    22-Mar-2021 : New coupling fields output          ( version 7.13 )
!/
!   1. Purpose :
!
!      Perform actual output of interpolated data. 
!
!   3. Parameters : 
!
!   4. Subroutines used : 
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!        W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
!     ----------------------------------------------------------------
!
!   5. Called by : 
!
!      Subroutine it resides in
!
!   6. Error messages : 
!
!      None.
!
!   7. Remarks : 
!
!   8. Structure : 
!
!      See source code.
!
!   9. Switches : 
!
!      10. Source code : 
!
!/ -------------------------------------------------------------------------/
        USE W3ADATMD
        USE W3WDATMD
        USE W3ODATMD, ONLY: NOGE
        USE W3IOGOMD, ONLY: W3IOGO
        USE W3GDATMD, ONLY: E3DF, NK
!/ -------------------------------------------------------------------------/
!/ Parameter List
!/
        INTEGER, INTENT(IN) :: NGRD, NSEA, NOSWLL_MIN, INTMETHOD
!/
!/ Local Parameters
!/
        INTEGER       :: ISEA, GSEA, IG, IGRID, IPTS, IGX, IGY, IX,    &
                         IY, ISWLL, ICAP, IBED, IFREQ, IK, INRST
        INTEGER       :: MAPINT, MAPICE, MAPDRY, MAPMSK, MAPLND,       &
                         NMAPICE, NMAPDRY, NMAPMSK, NMAPLND,           &
                         LMAPICE, LMAPDRY, LMAPMSK, LMAPLND,           &
                         MAPICET, MAPDRYT, MAPMSKT, MAPLNDT
        INTEGER       :: SUMGRD
        REAL          :: VAR1, VAR2, WT
! Local group 1 variables
        REAL          :: DWAUX, CXAUX, CYAUX, UAAUX, UDAUX, ASAUX,     &
                         WLVAUX, ICEAUX, ICEHAUX, ICEFAUX, BERGAUX,    &
                         SED_D50AUX, RHOAIRAUX, TAUAAUX, TAUADIRAUX,   &
                         SUMWT1(NOGE(1))
! Local group 2 variables
        REAL          :: HSAUX, WLMAUX, T02AUX, T0M1AUX, T01AUX,       &
                         FP0AUX, THMAUX1, THMAUX2, THSAUX, THP0AUX1,   &
                         THP0AUX2, HSIGAUX, STMAXEAUX,STMAXDAUX,       &
                         HMAXEAUX, HCMAXEAUX, HMAXDAUX, HCMAXDAUX,     &
                         WBTAUX, WNMEANAUX, SUMWT2(NOGE(2))
! Local group 3 variables
        REAL          :: EFAUX(E3DF(2,1):E3DF(3,1)),                   &
                         TH1MAUX(E3DF(2,2):E3DF(3,2)),                 &
                         STH1MAUX(E3DF(2,3):E3DF(3,3)),                &
                         TH2MAUX(E3DF(2,4):E3DF(3,4)),                 &
                         STH2MAUX(E3DF(2,5):E3DF(3,5)), WNAUX(1:NK),   &
                         SUMWT3A(E3DF(2,1):E3DF(3,1)),                 &
                         SUMWT3B(E3DF(2,2):E3DF(3,2)),                 &
                         SUMWT3C(E3DF(2,3):E3DF(3,3)),                 &
                         SUMWT3D(E3DF(2,4):E3DF(3,4)),                 &
                         SUMWT3E(E3DF(2,5):E3DF(3,5)),                 &
                         SUMWT3F(1:NK)
! Local group 4 variables
        REAL          :: PHSAUX(0:NOSWLL_MIN), PTPAUX(0:NOSWLL_MIN),   &
                         PLPAUX(0:NOSWLL_MIN), PSIAUX(0:NOSWLL_MIN),   &
                         PWSAUX(0:NOSWLL_MIN), PDIRAUX1(0:NOSWLL_MIN), &
                         PWSTAUX, PDIRAUX2(0:NOSWLL_MIN),              &
                         PTHP0AUX1(0:NOSWLL_MIN),                      &
                         PTHP0AUX2(0:NOSWLL_MIN),                      &
                         PQPAUX(0:NOSWLL_MIN), PPEAUX(0:NOSWLL_MIN),   &
                         PGWAUX(0:NOSWLL_MIN), PSWAUX(0:NOSWLL_MIN),   &
                         PTM1AUX(0:NOSWLL_MIN), PT1AUX(0:NOSWLL_MIN), &
                         PT2AUX(0:NOSWLL_MIN), PEPAUX(0:NOSWLL_MIN),   &
                         SUMWT4(NOGE(4),0:NOSWLL_MIN)
! Local group 5 variables
        REAL          :: USTAUX1, USTAUX2, CHARNAUX, CGEAUX,           &
                         PHIAWAUX, TAUWIXAUX, TAUWIYAUX, TAUWNXAUX,    &
                         TAUWNYAUX, WHITECAPAUX(4), SUMWT5(NOGE(5)),   &
                         SUMWTC(4)
! Local group 6 variables
        REAL          :: SXXAUX, SYYAUX, SXYAUX, TAUOXAUX, TAUOYAUX,   &
                         BHDAUX, PHIOCAUX, TUSXAUX, TUSYAUX, USSXAUX,  &
                         USSYAUX, PRMSAUX, TPMSAUX, SUMWT6(NOGE(6)),   &
                         TAUICEAUX(2), PHICEAUX,                       &
                         TAUOCXAUX, TAUOCYAUX,                         &
                         US3DAUX(2*NK), SUMWT68(2*NK),                 &
                         P2SMSAUX(P2MSF(2):P2MSF(3)),                  &
                         SUMWT69(P2MSF(2):P2MSF(3)),                   &
                         USSPAUX(2*NK), SUMWT612(2*NK)
! Local Group 7 variables
        REAL          :: ABAAUX, ABDAUX, UBAAUX, UBDAUX, PHIBBLAUX,    &
                         BEDFORMSAUX(3), TAUBBLAUX(2),                 &
                         SUMWT7(NOGE(7)), SUMWTB(3)
! Local group 8 variables
        REAL          :: MSSXAUX, MSSYAUX, MSCXAUX, MSCYAUX, MSSDAUX1, &
                         MSSDAUX2, MSCDAUX1, MSCDAUX2, QPAUX,          &
                         SUMWT8(NOGE(8))
!/
        LOGICAL       :: ACTIVE
        LOGICAL       :: USEGRID(NGRD)
!/
!
!-------------------------------------------------------------------
! 1.  Preparations
!
! Group 1 Variables
!
        DW       = UNDEF
        CX       = UNDEF
        CY       = UNDEF
        UA       = UNDEF
        UD       = UNDEF
        AS       = UNDEF
        WLV      = UNDEF
        ICE      = UNDEF
        BERG     = UNDEF
        RHOAIR   = UNDEF
        TAUA     = UNDEF
        TAUADIR  = UNDEF
#ifdef W3_BT4
        SED_D50  = UNDEF
#endif
#ifdef W3_IS2
        ICEH     = UNDEF
        ICEF     = UNDEF
#endif
!
! Group 2 variables
!
        HS       = UNDEF
        WLM      = UNDEF
        T02      = UNDEF
        T0M1     = UNDEF
        T01      = UNDEF
        FP0      = UNDEF
        THM      = UNDEF
        THS      = UNDEF
        THP0     = UNDEF
        HSIG     = UNDEF
        STMAXE   = UNDEF
        STMAXD   = UNDEF
        HMAXE    = UNDEF
        HCMAXE   = UNDEF
        HMAXD    = UNDEF
        HCMAXD   = UNDEF
        WBT      = UNDEF
        WNMEAN   = UNDEF
!
! Group 3 variables
!
      IF (  E3DF(1,1).GT.0 ) EF      = UNDEF
      IF (  E3DF(1,2).GT.0 ) TH1M    = UNDEF
      IF (  E3DF(1,3).GT.0 ) STH1M   = UNDEF
      IF (  E3DF(1,4).GT.0 ) TH2M    = UNDEF
      IF (  E3DF(1,5).GT.0 ) STH2M   = UNDEF
        WN      = UNDEF
!
! Group 4 variables
!
        PHS      = UNDEF
        PTP      = UNDEF
        PLP      = UNDEF
        PDIR     = UNDEF
        PSI      = UNDEF
        PWS      = UNDEF
        PWST     = UNDEF
        PNR      = UNDEF
        PTHP0    = UNDEF
        PQP      = UNDEF
        PPE      = UNDEF
        PGW      = UNDEF
        PSW      = UNDEF
        PTM1     = UNDEF
        PT1      = UNDEF
        PT2      = UNDEF
        PEP      = UNDEF
!
! Group 5 variables
!
        UST      = UNDEF
        USTDIR   = UNDEF
        CHARN    = UNDEF
        CGE      = UNDEF
        PHIAW    = UNDEF
        TAUWIX   = UNDEF
        TAUWIY   = UNDEF
        TAUWNX   = UNDEF
        TAUWNY   = UNDEF
        WHITECAP = UNDEF
!
! Group 6 variables
!
        SXX      = UNDEF
        SXY      = UNDEF
        SYY      = UNDEF
        TAUOX    = UNDEF
        TAUOY    = UNDEF
        BHD      = UNDEF
        PHIOC    = UNDEF
        TUSX     = UNDEF
        TUSY     = UNDEF
        USSX     = UNDEF
        USSY     = UNDEF
        TAUOCX   = UNDEF
        TAUOCY   = UNDEF
        PRMS     = UNDEF
        TPMS     = UNDEF
        IF ( US3DF(1).GT.0 ) THEN
             US3D     = UNDEF
        ENDIF
        IF ( P2MSF(1).GT.0) THEN
             P2SMS    = UNDEF
        ENDIF
        TAUICE   = UNDEF
        PHICE    = UNDEF
        IF ( USSPF(1).GT.0 ) THEN
             USSP     = UNDEF
        ENDIF
!
! Group 7 variables
!
        ABA      = UNDEF
        ABD      = UNDEF
        UBA      = UNDEF
        UBD      = UNDEF
        BEDFORMS = UNDEF
        PHIBBL   = UNDEF
        TAUBBL   = UNDEF
!
! Group 8 variables
!
        MSSX     = UNDEF
        MSSY     = UNDEF
        MSCX     = UNDEF
        MSCY     = UNDEF
        MSSD     = UNDEF
        MSCD     = UNDEF
        QP       = UNDEF
!
!-------------------------------------------------------------------
! 2.  Loop through output points
!
        DO ISEA = 1, NSEA
!
          IX = MAPSF(ISEA,1)
          IY = MAPSF(ISEA,2)
          MAPICE = MOD(MAPST2(IY,IX),2) 
          MAPDRY = MOD(MAPST2(IY,IX)/2,2) 
          MAPLND = MOD(MAPST2(IY,IX)/4,2) 
          MAPMSK = MOD(MAPST2(IY,IX)/8,2) 
          MAPINT = MOD(MAPST2(IY,IX)/16,2) 
          MAPST2(IY,IX) = MAPST2(IY,IX) - MAPICE - 2*MAPDRY - 4*MAPLND         &
                          - 8*MAPMSK
          ACTIVE =  (MAPICE .NE. 1 .AND. MAPDRY .NE. 1)
!
          IF ( MAPINT .EQ. 0 ) THEN
!
! Initial loop to determine status map
! Initialize by setting it to be ice free and wet
!
            MAPICE = 0
            MAPDRY = 0
            MAPMSK = 0
            MAPLND = 0
            ACTIVE = .TRUE.
            MAPSTA(IY,IX) = ABS ( MAPSTA(IY,IX) )
            SUMGRD = 0
            DO IG = 1,GR_INTS(ISEA)%NGRDS
              IGRID = GR_INTS(ISEA)%GDID(IG)
              NMAPICE = 0
              NMAPDRY = 0
              NMAPLND = 0
              NMAPMSK = 0
              MAPICET = 0
              MAPDRYT = 0
              MAPLNDT = 0
              MAPMSKT = 0
              IF ( INTMETHOD == 2 ) THEN
                ! Nearest neighbour is the one with the most weight
                INRST = MAXLOC(GR_INTS(ISEA)%IND_WTS(IG)%WT, DIM=1)
                GR_INTS(ISEA)%IND_WTS(IG)%WT(:) = -1.
                GR_INTS(ISEA)%IND_WTS(IG)%WT(INRST) = 1.
              END IF
              DO IPTS = 1,GR_INTS(ISEA)%IND_WTS(IG)%NP
                IGX = GR_INTS(ISEA)%IND_WTS(IG)%IP(IPTS)
                IGY = GR_INTS(ISEA)%IND_WTS(IG)%JP(IPTS)
                LMAPICE = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX),2 )
                LMAPDRY = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX)/2,2 )
                LMAPLND = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX)/4,2 )
                LMAPMSK = MOD ( GRIDS(IGRID)%MAPST2(IGY,IGX)/8,2 )
                IF ( LMAPICE .EQ. 1 ) NMAPICE = NMAPICE + 1
                IF ( LMAPDRY .EQ. 1 ) NMAPDRY = NMAPDRY + 1
                IF ( LMAPLND .EQ. 1 ) NMAPLND = NMAPLND + 1
                IF ( LMAPMSK .EQ. 1 ) NMAPMSK = NMAPMSK + 1
              END DO 
              NMAPICE = NMAPICE*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
              NMAPDRY = NMAPDRY*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
              NMAPLND = NMAPLND*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
              NMAPMSK = NMAPMSK*100/GR_INTS(ISEA)%IND_WTS(IG)%NP
              IF ( NMAPICE .GT. 50 ) MAPICET = 1
              IF ( NMAPDRY .GT. 50 ) MAPDRYT = 1
              IF ( NMAPLND .GT. 50 ) MAPLNDT = 1
              IF ( NMAPMSK .GT. 50 ) MAPMSKT = 1
              ACTIVE =  (MAPICET .NE. 1 .AND. MAPDRYT .NE. 1 .AND.             &
                         MAPLNDT .NE. 1 .AND. MAPMSKT .NE. 1)
              IF ( ACTIVE ) THEN
                USEGRID(IG) = .TRUE.
                SUMGRD = SUMGRD+1
                MAPICE = MAPICET
                MAPDRY = MAPDRYT
                MAPLND = MAPLNDT
                MAPMSK = MAPMSKT
              ELSE
                USEGRID(IG) = .FALSE.
              END IF
            END DO
            IF ( SUMGRD .EQ. 0 ) THEN
              MAPICE = MAPICET
              MAPDRY = MAPDRYT
              MAPLND = MAPLNDT
              MAPMSK = MAPMSKT
            END IF
!
! Reset the status map
!
            MAPST2(IY,IX) = MAPST2(IY,IX) + MAPICE + 2*MAPDRY + 4*MAPLND +     &
                            8*MAPMSK
            ACTIVE =  (MAPICE .NE. 1 .AND. MAPDRY .NE. 1 .AND. MAPLND .NE. 1   &
                       .AND. MAPMSK .NE. 1)
            IF ( .NOT. ACTIVE ) MAPSTA(IY,IX) = -ABS ( MAPSTA(IY,IX) )
!
! Second loop to do the actual interpolation
!
            DO IG = 1,GR_INTS(ISEA)%NGRDS
!
              IF ( USEGRID(IG) ) THEN
!
                IGRID = GR_INTS(ISEA)%GDID(IG)
!
! Initialize temporary variables used
!
                SUMWT = 0.0
!
! Group 1 variables
!
                DWAUX       = UNDEF
                CXAUX       = UNDEF
                CYAUX       = UNDEF
                UAAUX       = UNDEF
                UDAUX       = UNDEF
                ASAUX       = UNDEF
                WLVAUX      = UNDEF
                ICEAUX      = UNDEF
                BERGAUX     = UNDEF
                SED_D50AUX  = UNDEF
                ICEHAUX     = UNDEF
                ICEFAUX     = UNDEF
                RHOAIRAUX   = UNDEF
                TAUAAUX     = UNDEF
                TAUADIRAUX  = UNDEF
                SUMWT1      = 0
!
! Group 2 variables
!
                HSAUX       = UNDEF
                WLMAUX      = UNDEF
                T02AUX      = UNDEF
                T0M1AUX     = UNDEF
                T01AUX      = UNDEF
                FP0AUX      = UNDEF
                THMAUX1     = UNDEF
                THMAUX2     = UNDEF
                THSAUX      = UNDEF
                THP0AUX1    = UNDEF
                THP0AUX2    = UNDEF
                HSIGAUX     = UNDEF
                STMAXEAUX   = UNDEF
                STMAXDAUX   = UNDEF
                HMAXEAUX    = UNDEF
                HCMAXEAUX   = UNDEF
                HMAXDAUX    = UNDEF
                HCMAXDAUX   = UNDEF
                WBTAUX      = UNDEF
                WNMEANAUX   = UNDEF
                SUMWT2      = 0
!
! Group 3 variables
!
                EFAUX      = UNDEF
                TH1MAUX    = UNDEF
                STH1MAUX   = UNDEF
                TH2MAUX    = UNDEF
                STH2MAUX   = UNDEF
                WNAUX      = UNDEF
                SUMWT3A    = 0
                SUMWT3B    = 0
                SUMWT3C    = 0
                SUMWT3D    = 0
                SUMWT3E    = 0
                SUMWT3F    = 0
!
! Group 4 variables
!
                PHSAUX      = UNDEF
                PTPAUX      = UNDEF
                PLPAUX      = UNDEF
                PDIRAUX1    = UNDEF
                PDIRAUX2    = UNDEF
                PSIAUX      = UNDEF
                PWSAUX      = UNDEF
                PWSTAUX     = UNDEF
                PTHP0AUX1   = UNDEF
                PTHP0AUX2   = UNDEF
                PQPAUX      = UNDEF
                PPEAUX      = UNDEF
                PGWAUX      = UNDEF
                PSWAUX      = UNDEF
                PTM1AUX     = UNDEF
                PT1AUX      = UNDEF
                PT2AUX      = UNDEF
                PEPAUX      = UNDEF
                SUMWT4      = 0
!
! Group 5 variables
!
                USTAUX1     = UNDEF
                USTAUX2     = UNDEF
                CHARNAUX    = UNDEF
                CGEAUX      = UNDEF
                PHIAWAUX    = UNDEF
                TAUWIXAUX   = UNDEF
                TAUWIYAUX   = UNDEF
                TAUWNXAUX   = UNDEF
                TAUWNYAUX   = UNDEF
                WHITECAPAUX = UNDEF
                SUMWT5      = 0
                SUMWTC      = 0
!
! Group 6 variables
!
                SXXAUX      = UNDEF
                SXYAUX      = UNDEF
                SYYAUX      = UNDEF
                TAUOXAUX    = UNDEF
                TAUOYAUX    = UNDEF
                BHDAUX      = UNDEF
                PHIOCAUX    = UNDEF
                TUSXAUX     = UNDEF
                TUSYAUX     = UNDEF
                USSXAUX     = UNDEF
                USSYAUX     = UNDEF
                TAUOCXAUX   = UNDEF
                TAUOCYAUX   = UNDEF
                PRMSAUX     = UNDEF
                TPMSAUX     = UNDEF
                P2SMSAUX    = UNDEF
                US3DAUX     = UNDEF
                PHICEAUX    = UNDEF
                TAUICEAUX   = UNDEF
                USSPAUX     = UNDEF
                SUMWT69     = 0
                SUMWT68     = 0
                SUMWT612    = 0
                SUMWT6      = 0
!
! Group 7 variables
!
                ABAAUX      = UNDEF
                ABDAUX      = UNDEF
                UBAAUX      = UNDEF
                UBDAUX      = UNDEF
                BEDFORMSAUX = UNDEF
                PHIBBLAUX   = UNDEF
                TAUBBLAUX   = UNDEF
                SUMWT7      = 0
                SUMWTB      = 0
!
! Group 8 variables
!
                MSSXAUX     = UNDEF
                MSSYAUX     = UNDEF
                MSCXAUX     = UNDEF
                MSCYAUX     = UNDEF
                MSSDAUX1    = UNDEF
                MSSDAUX2    = UNDEF
                MSCDAUX1    = UNDEF
                MSCDAUX2    = UNDEF
                QPAUX       = UNDEF
                SUMWT8      = 0
!
! Loop through the points per grid to obtain interpolated values
!
                DO IPTS = 1,GR_INTS(ISEA)%IND_WTS(IG)%NP
                  IGX = GR_INTS(ISEA)%IND_WTS(IG)%IP(IPTS)
                  IGY = GR_INTS(ISEA)%IND_WTS(IG)%JP(IPTS)
                  WT = GR_INTS(ISEA)%IND_WTS(IG)%WT(IPTS)
                  IF (  WT < 0. ) THEN
                     ! Point is not nearest
                     CYCLE
                  END IF
                  GSEA = GRIDS(IGRID)%MAPFS(IGY,IGX)
!
! Group 1 variables
!
                  IF ( FLOGRD(1,1) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%DW(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(1) = SUMWT1(1) + WT
                      IF ( DWAUX .EQ. UNDEF ) THEN
                        DWAUX = WADATS(IGRID)%DW(GSEA)*WT
                      ELSE
                        DWAUX = DWAUX + WADATS(IGRID)%DW(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,2) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%CX(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(2) = SUMWT1(2) + WT
                      IF ( CXAUX .EQ. UNDEF ) THEN
                        CXAUX = WADATS(IGRID)%CX(GSEA)*WT
                        CYAUX = WADATS(IGRID)%CY(GSEA)*WT
                      ELSE
                        CXAUX = CXAUX + WADATS(IGRID)%CX(GSEA)*WT
                        CYAUX = CYAUX + WADATS(IGRID)%CY(GSEA)*WT
                       END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,3) ) THEN
                    IF ( WADATS(IGRID)%UA(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(3) = SUMWT1(3) + WT
                      IF ( UAAUX .EQ. UNDEF ) THEN
                        UAAUX = WADATS(IGRID)%UA(GSEA)*WT
                        UDAUX = WADATS(IGRID)%UD(GSEA)*WT
                      ELSE
                        UAAUX = UAAUX + WADATS(IGRID)%UA(GSEA)*WT
                        UDAUX = UDAUX + WADATS(IGRID)%UD(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,4) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%AS(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(4) = SUMWT1(4) + WT
                      IF ( ASAUX .EQ. UNDEF ) THEN
                        ASAUX = WADATS(IGRID)%AS(GSEA)*WT
                      ELSE
                        ASAUX = ASAUX + WADATS(IGRID)%AS(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,5) .AND. ACTIVE ) THEN
                    IF ( WDATAS(IGRID)%WLV(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(5) = SUMWT1(5) + WT
                      IF ( WLVAUX .EQ. UNDEF ) THEN
                        WLVAUX = WDATAS(IGRID)%WLV(GSEA)*WT
                      ELSE
                        WLVAUX = WLVAUX + WDATAS(IGRID)%WLV(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,6) ) THEN
                    IF ( WDATAS(IGRID)%ICE(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(6) = SUMWT1(6) + WT
                      IF ( ICEAUX .EQ. UNDEF ) THEN
                        ICEAUX = WDATAS(IGRID)%ICE(GSEA)*WT
                      ELSE
                        ICEAUX = ICEAUX + WDATAS(IGRID)%ICE(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,7) .AND. ACTIVE ) THEN
                    IF ( WDATAS(IGRID)%BERG(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(7) = SUMWT1(7) + WT
                      IF ( BERGAUX .EQ. UNDEF ) THEN
                        BERGAUX = WDATAS(IGRID)%BERG(GSEA)*WT
                      ELSE
                        BERGAUX = BERGAUX + WDATAS(IGRID)%BERG(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,8) ) THEN
                    IF ( WADATS(IGRID)%TAUA(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(8) = SUMWT1(8) + WT
                      IF ( TAUAAUX .EQ. UNDEF ) THEN
                        TAUAAUX = WADATS(IGRID)%TAUA(GSEA)*WT
                        TAUADIRAUX = WADATS(IGRID)%TAUADIR(GSEA)*WT
                      ELSE
                        TAUAAUX = TAUAAUX + WADATS(IGRID)%TAUA(GSEA)*WT
                        TAUADIRAUX = TAUADIRAUX + WADATS(IGRID)%TAUADIR(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(1,9) .AND. ACTIVE ) THEN
                    IF ( WDATAS(IGRID)%RHOAIR(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(9) = SUMWT1(9) + WT
                      IF ( RHOAIRAUX .EQ. UNDEF ) THEN
                        RHOAIRAUX = WDATAS(IGRID)%RHOAIR(GSEA)*WT
                      ELSE
                        RHOAIRAUX = RHOAIRAUX + WDATAS(IGRID)%RHOAIR(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
#ifdef W3_BT4
                  IF ( FLOGRD(1,10) ) THEN
                    IF ( GRIDS(IGRID)%SED_D50(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(10) = SUMWT1(10) + WT
                      IF ( SED_D50AUX .EQ. UNDEF ) THEN
                        SED_D50AUX = GRIDS(IGRID)%SED_D50(GSEA)*WT
                      ELSE
                        SED_D50AUX = SED_D50AUX + GRIDS(IGRID)%SED_D50(GSEA)*WT
                      END IF
                    END IF
                  END IF
#endif
!
#ifdef W3_IS2
                  IF ( FLOGRD(1,11) ) THEN
                    IF ( WDATAS(IGRID)%ICEH(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(11) = SUMWT1(11) + WT
                      IF (ICEHAUX .EQ. UNDEF) THEN
                        ICEHAUX = WDATAS(IGRID)%ICEH(GSEA)*WT
                      ELSE 
                        ICEHAUX = ICEHAUX + WDATAS(IGRID)%ICEH(GSEA)*WT
                      END IF
                    END IF
                  END IF
#endif
!
#ifdef W3_IS2
                  IF ( FLOGRD(1,12) ) THEN
                    IF ( WDATAS(IGRID)%ICEF(GSEA) .NE. UNDEF ) THEN
                      SUMWT1(12) = SUMWT1(12) + WT
                      IF (ICEFAUX .EQ. UNDEF) THEN
                        ICEFAUX = WDATAS(IGRID)%ICEF(GSEA)*WT
                      ELSE 
                        ICEFAUX = ICEFAUX + WDATAS(IGRID)%ICEF(GSEA)*WT
                      END IF
                    END IF
                  END IF
#endif
!
! Group 2 variables
!
                  IF ( FLOGRD(2,1) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%HS(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(1) = SUMWT2(1) + WT
                      IF ( HSAUX .EQ. UNDEF ) THEN
                        HSAUX = WADATS(IGRID)%HS(GSEA)*WT
                      ELSE
                        HSAUX = HSAUX + WADATS(IGRID)%HS(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,2) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%WLM(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(2) = SUMWT2(2) + WT
                      IF ( WLMAUX .EQ. UNDEF ) THEN
                        WLMAUX = WADATS(IGRID)%WLM(GSEA)*WT
                      ELSE
                        WLMAUX = WLMAUX + WADATS(IGRID)%WLM(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,3) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%T02(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(3) = SUMWT2(3) + WT
                      IF ( T02AUX .EQ. UNDEF ) THEN
                        T02AUX = WADATS(IGRID)%T02(GSEA)*WT
                      ELSE
                        T02AUX = T02AUX + WADATS(IGRID)%T02(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,4) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%T0M1(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(4) = SUMWT2(4) + WT
                      IF ( T0M1AUX .EQ. UNDEF ) THEN
                        T0M1AUX = WADATS(IGRID)%T0M1(GSEA)*WT
                      ELSE
                        T0M1AUX = T0M1AUX + WADATS(IGRID)%T0M1(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,5) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%T01(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(5) = SUMWT2(5) + WT
                      IF ( T01AUX .EQ. UNDEF ) THEN
                        T01AUX = WADATS(IGRID)%T01(GSEA)*WT
                      ELSE
                        T01AUX = T01AUX + WADATS(IGRID)%T01(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( (FLOGRD(2,6) .OR. FLOGRD(2,18)) .AND. ACTIVE ) THEN
                    ! Note: Output TP [FLOGRD(2,18)] is derived from FP0
                    IF ( WADATS(IGRID)%FP0(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(6) = SUMWT2(6) + WT
                      IF ( FP0AUX .EQ. UNDEF ) THEN
                        FP0AUX = WADATS(IGRID)%FP0(GSEA)*WT
                      ELSE
                        FP0AUX = FP0AUX + WADATS(IGRID)%FP0(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,7) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%THM(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(7) = SUMWT2(7) + WT
                      IF ( THMAUX1 .EQ. UNDEF ) THEN
                        THMAUX1 = COS ( WADATS(IGRID)%THM(GSEA) )*WT
                        THMAUX2 = SIN ( WADATS(IGRID)%THM(GSEA) )*WT
                      ELSE
                        THMAUX1 = THMAUX1 + COS ( WADATS(IGRID)%THM(GSEA) )*WT
                        THMAUX2 = THMAUX2 + SIN ( WADATS(IGRID)%THM(GSEA) )*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,8) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%THS(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(8) = SUMWT2(8) + WT
                      IF ( THSAUX .EQ. UNDEF ) THEN
                        THSAUX = WADATS(IGRID)%THS(GSEA)*WT
                      ELSE
                        THSAUX = THSAUX + WADATS(IGRID)%THS(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,9) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%THP0(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(9) = SUMWT2(9) + WT
                      IF ( THP0AUX1 .EQ. UNDEF ) THEN
                        THP0AUX1 = COS ( WADATS(IGRID)%THP0(GSEA) )*WT
                        THP0AUX2 = SIN ( WADATS(IGRID)%THP0(GSEA) )*WT
                      ELSE
                        THP0AUX1 = THP0AUX1 +                                  &
                                   COS ( WADATS(IGRID)%THP0(GSEA) )*WT
                        THP0AUX2 = THP0AUX2 +                                  &
                                   SIN ( WADATS(IGRID)%THP0(GSEA) )*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,10) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%HSIG(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(10) = SUMWT2(10) + WT
                      IF ( HSIGAUX .EQ. UNDEF )   HSIGAUX = 0.
                      HSIGAUX = HSIGAUX + WADATS(IGRID)%HSIG(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,11) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%STMAXE(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(11) = SUMWT2(11) + WT
                      IF ( STMAXEAUX .EQ. UNDEF )   STMAXEAUX = 0.
                      STMAXEAUX = STMAXEAUX + WADATS(IGRID)%STMAXE(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,12) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%STMAXD(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(12) = SUMWT2(12) + WT
                      IF ( STMAXDAUX .EQ. UNDEF )   STMAXDAUX = 0.
                      STMAXDAUX = STMAXDAUX + WADATS(IGRID)%STMAXD(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,13) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%HMAXE(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(13) = SUMWT2(13) + WT
                      IF ( HMAXEAUX .EQ. UNDEF )   HMAXEAUX = 0.
                      HMAXEAUX = HMAXEAUX + WADATS(IGRID)%HMAXE(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,14) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%HCMAXE(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(14) = SUMWT2(14) + WT
                      IF ( HCMAXEAUX .EQ. UNDEF ) HCMAXEAUX = 0.
                      HCMAXEAUX = HCMAXEAUX + WADATS(IGRID)%HCMAXE(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,15) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%HMAXD(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(15) = SUMWT2(15) + WT
                      IF ( HMAXDAUX .EQ. UNDEF )   HMAXDAUX = 0.
                      HMAXDAUX = HMAXDAUX + WADATS(IGRID)%HMAXD(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,16) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%HCMAXD(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(16) = SUMWT2(16) + WT
                      IF ( HCMAXDAUX .EQ. UNDEF )   HCMAXDAUX = 0.
                      HCMAXDAUX = HCMAXDAUX + WADATS(IGRID)%HCMAXD(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,17) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%WBT(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(17) = SUMWT2(17) + WT
                      IF ( WBTAUX .EQ. UNDEF )   WBTAUX = 0.
                      WBTAUX = WBTAUX + WADATS(IGRID)%WBT(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(2,19) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%WNMEAN(GSEA) .NE. UNDEF ) THEN
                      SUMWT2(19) = SUMWT2(19) + WT
                      IF ( WNMEANAUX .EQ. UNDEF )   WNMEANAUX = 0.
                      WNMEANAUX = WNMEANAUX + WADATS(IGRID)%WNMEAN(GSEA)*WT
                    END IF
                  END IF
!
! Group 3 variables
!
                  IF ( FLOGRD(3,1) .AND. ACTIVE ) THEN
                    DO IFREQ = E3DF(2,1),E3DF(3,1)
                      IF ( WADATS(IGRID)%EF(GSEA,IFREQ) .NE. UNDEF ) THEN
                        SUMWT3A(IFREQ) = SUMWT3A(IFREQ) + WT
                        IF ( EFAUX(IFREQ) .EQ. UNDEF ) THEN
                          EFAUX(IFREQ) = WADATS(IGRID)%EF(GSEA,IFREQ)*WT
                        ELSE
                          EFAUX(IFREQ) = EFAUX(IFREQ) + WADATS(IGRID)%EF(GSEA,IFREQ)*WT
                        END IF
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(3,2) .AND. ACTIVE ) THEN
                    DO IFREQ = E3DF(2,2),E3DF(3,2)
                      IF ( WADATS(IGRID)%TH1M(GSEA,IFREQ) .NE. UNDEF ) THEN
                        SUMWT3B(IFREQ) = SUMWT3B(IFREQ) + WT
                        IF ( TH1MAUX(IFREQ) .EQ. UNDEF ) THEN
                          TH1MAUX(IFREQ) = WADATS(IGRID)%TH1M(GSEA,IFREQ)*WT
                        ELSE
                          TH1MAUX(IFREQ) = TH1MAUX(IFREQ) + WADATS(IGRID)%TH1M(GSEA,IFREQ)*WT
                        END IF
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(3,3) .AND. ACTIVE ) THEN
                    DO IFREQ = E3DF(2,3),E3DF(3,3)
                      IF ( WADATS(IGRID)%STH1M(GSEA,IFREQ) .NE. UNDEF ) THEN
                        SUMWT3C(IFREQ) = SUMWT3C(IFREQ) + WT
                        IF ( STH1MAUX(IFREQ) .EQ. UNDEF ) THEN
                          STH1MAUX(IFREQ) = WADATS(IGRID)%STH1M(GSEA,IFREQ)*WT
                        ELSE
                          STH1MAUX(IFREQ) = STH1MAUX(IFREQ) + WADATS(IGRID)%STH1M(GSEA,IFREQ)*WT
                        END IF
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(3,4) .AND. ACTIVE ) THEN
                    DO IFREQ = E3DF(2,4),E3DF(3,4)
                      IF ( WADATS(IGRID)%TH2M(GSEA,IFREQ) .NE. UNDEF ) THEN
                        SUMWT3D(IFREQ) = SUMWT3D(IFREQ) + WT
                        IF ( TH2MAUX(IFREQ) .EQ. UNDEF ) THEN
                          TH2MAUX(IFREQ) = WADATS(IGRID)%TH2M(GSEA,IFREQ)*WT
                        ELSE
                          TH2MAUX(IFREQ) = TH2MAUX(IFREQ) + WADATS(IGRID)%TH2M(GSEA,IFREQ)*WT
                        END IF
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(3,5) .AND. ACTIVE ) THEN
                    DO IFREQ = E3DF(2,5),E3DF(3,5)
                      IF ( WADATS(IGRID)%STH2M(GSEA,IFREQ) .NE. UNDEF ) THEN
                        SUMWT3E(IFREQ) = SUMWT3E(IFREQ) + WT
                        IF ( STH2MAUX(IFREQ) .EQ. UNDEF ) THEN
                          STH2MAUX(IFREQ) = WADATS(IGRID)%STH2M(GSEA,IFREQ)*WT
                        ELSE
                          STH2MAUX(IFREQ) = STH2MAUX(IFREQ) + WADATS(IGRID)%STH2M(GSEA,IFREQ)*WT
                        END IF
                      END IF
                    END DO
                  END IF

!
                  IF ( FLOGRD(3,6) .AND. ACTIVE ) THEN
                    DO IK = 1,NK
                      IF ( WADATS(IGRID)%WN(IK,GSEA) .NE. UNDEF ) THEN
                        SUMWT3F(IK) = SUMWT3F(IK) + WT
                        IF ( WNAUX(IK) .EQ. UNDEF ) THEN
                          WNAUX(IK) = WADATS(IGRID)%WN(IK,GSEA)*WT
                        ELSE
                          WNAUX(IK) = WNAUX(IK) + WADATS(IGRID)%WN(IK,GSEA)*WT
                        END IF
                      END IF
                    END DO
                  END IF
!
! Group 4 variables
!
                  DO ISWLL = 0, NOSWLL_MIN
!
                    IF ( FLOGRD(4,1) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PHS(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(1,ISWLL) = SUMWT4(1,ISWLL) + WT
                        IF ( PHSAUX(ISWLL) .EQ. UNDEF ) THEN
                          PHSAUX(ISWLL) = WADATS(IGRID)%PHS(GSEA,ISWLL)*WT
                        ELSE
                          PHSAUX(ISWLL) = PHSAUX(ISWLL) +                      &
                                          WADATS(IGRID)%PHS(GSEA,ISWLL)*WT
                        END IF
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,2) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PTP(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(2,ISWLL) = SUMWT4(2,ISWLL) + WT
                        IF ( PTPAUX(ISWLL) .EQ. UNDEF ) THEN
                          PTPAUX(ISWLL) = WADATS(IGRID)%PTP(GSEA,ISWLL)*WT
                        ELSE
                          PTPAUX(ISWLL) = PTPAUX(ISWLL) +                      &
                                          WADATS(IGRID)%PTP(GSEA,ISWLL)*WT
                        END IF
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,3) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PLP(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(3,ISWLL) = SUMWT4(3,ISWLL) + WT
                        IF ( PLPAUX(ISWLL) .EQ. UNDEF ) THEN
                          PLPAUX(ISWLL) = WADATS(IGRID)%PLP(GSEA,ISWLL)*WT
                        ELSE
                          PLPAUX(ISWLL) = PLPAUX(ISWLL) +                      &
                                          WADATS(IGRID)%PLP(GSEA,ISWLL)*WT
                        END IF
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,4) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PDIR(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(4,ISWLL) = SUMWT4(4,ISWLL) + WT
                        IF ( PDIRAUX1(ISWLL) .EQ. UNDEF ) THEN
                          PDIRAUX1(ISWLL) =                                     &
                               COS ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
                          PDIRAUX2(ISWLL) =                                     &
                               SIN ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
                        ELSE
                          PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) +                    &
                               COS ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
                          PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) +                    &
                               SIN ( WADATS(IGRID)%PDIR(GSEA,ISWLL) )*WT
                        END IF
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,5) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PSI(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(5,ISWLL) = SUMWT4(5,ISWLL) + WT
                        IF ( PSIAUX(ISWLL) .EQ. UNDEF ) THEN
                          PSIAUX(ISWLL) = WADATS(IGRID)%PSI(GSEA,ISWLL)*WT
                        ELSE
                          PSIAUX(ISWLL) = PSIAUX(ISWLL) +                      &
                                          WADATS(IGRID)%PSI(GSEA,ISWLL)*WT
                        END IF
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,6) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PWS(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(6,ISWLL) = SUMWT4(6,ISWLL) + WT
                        IF ( PWSAUX(ISWLL) .EQ. UNDEF ) THEN
                          PWSAUX(ISWLL) = WADATS(IGRID)%PWS(GSEA,ISWLL)*WT
                        ELSE
                          PWSAUX(ISWLL) = PWSAUX(ISWLL) +                      &
                                          WADATS(IGRID)%PWS(GSEA,ISWLL)*WT
                        END IF
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,7) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PTHP0(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(7,ISWLL) = SUMWT4(7,ISWLL) + WT
                        IF (PTHP0AUX1(ISWLL).EQ.UNDEF)                 &
                                                   PTHP0AUX1(ISWLL) = 0.
                        IF (PTHP0AUX2(ISWLL).EQ.UNDEF)                 &
                                                   PTHP0AUX2(ISWLL) = 0.
                        PTHP0AUX1(ISWLL) = PTHP0AUX1(ISWLL) +          &
                              COS ( WADATS(IGRID)%PTHP0(GSEA,ISWLL) )*WT
                        PTHP0AUX2(ISWLL) = PTHP0AUX2(ISWLL) +          &
                              SIN ( WADATS(IGRID)%PTHP0(GSEA,ISWLL) )*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,8) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PQP(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(8,ISWLL) = SUMWT4(8,ISWLL) + WT
                        IF ( PQPAUX(ISWLL).EQ.UNDEF ) PQPAUX(ISWLL) = 0.
                        PQPAUX(ISWLL) = PQPAUX(ISWLL) +                &
                                        WADATS(IGRID)%PQP(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,9) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PPE(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(9,ISWLL) = SUMWT4(9,ISWLL) + WT
                        IF ( PPEAUX(ISWLL).EQ.UNDEF ) PPEAUX(ISWLL) = 0.
                        PPEAUX(ISWLL) = PPEAUX(ISWLL) +                &
                                        WADATS(IGRID)%PPE(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,10) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PGW(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(10,ISWLL) = SUMWT4(10,ISWLL) + WT
                        IF ( PGWAUX(ISWLL).EQ.UNDEF ) PGWAUX(ISWLL) = 0.
                        PGWAUX(ISWLL) = PGWAUX(ISWLL) +                &
                                        WADATS(IGRID)%PGW(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,11) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PSW(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(11,ISWLL) = SUMWT4(11,ISWLL) + WT
                        IF ( PSWAUX(ISWLL).EQ.UNDEF ) PSWAUX(ISWLL) = 0.
                        PSWAUX(ISWLL) = PSWAUX(ISWLL) +                &
                                        WADATS(IGRID)%PSW(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,12) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PTM1(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(12,ISWLL) = SUMWT4(12,ISWLL) + WT
                        IF ( PTM1AUX(ISWLL).EQ.UNDEF )                &
                                                    PTM1AUX(ISWLL) = 0.
                        PTM1AUX(ISWLL) = PTM1AUX(ISWLL) +            &
                                      WADATS(IGRID)%PTM1(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,13) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PT1(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(13,ISWLL) = SUMWT4(13,ISWLL) + WT
                        IF ( PT1AUX(ISWLL).EQ.UNDEF ) PT1AUX(ISWLL) = 0.
                        PT1AUX(ISWLL) = PT1AUX(ISWLL) +                &
                                        WADATS(IGRID)%PT1(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,14) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PT2(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(14,ISWLL) = SUMWT4(14,ISWLL) + WT
                        IF ( PT2AUX(ISWLL).EQ.UNDEF ) PT2AUX(ISWLL) = 0.
                        PT2AUX(ISWLL) = PT2AUX(ISWLL) +                &
                                        WADATS(IGRID)%PT2(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                    IF ( FLOGRD(4,15) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%PEP(GSEA,ISWLL) .NE. UNDEF ) THEN
                        SUMWT4(15,ISWLL) = SUMWT4(15,ISWLL) + WT
                        IF ( PEPAUX(ISWLL).EQ.UNDEF ) PEPAUX(ISWLL) = 0.
                        PEPAUX(ISWLL) = PEPAUX(ISWLL) +                &
                                        WADATS(IGRID)%PEP(GSEA,ISWLL)*WT
                      END IF
                    END IF
!
                  END DO !/ ISWLL = 0, NOSWLL_MIN
!
                  IF ( FLOGRD(4,16) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%PWST(GSEA) .NE. UNDEF ) THEN
                        SUMWT4(16,0) = SUMWT4(16,0) + WT
                      IF ( PWSTAUX .EQ. UNDEF ) THEN
                        PWSTAUX = WADATS(IGRID)%PWST(GSEA)*WT
                      ELSE
                        PWSTAUX = PWSTAUX + WADATS(IGRID)%PWST(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
! Group 5 variables
!
                  IF ( FLOGRD(5,1) ) THEN
                    IF ( WDATAS(IGRID)%UST(GSEA) .NE. UNDEF ) THEN
                      SUMWT5(1) = SUMWT5(1) + WT
                      IF ( USTAUX1 .EQ. UNDEF ) THEN
                        USTAUX1 = WDATAS(IGRID)%UST(GSEA)*WT
                        USTAUX2 = WDATAS(IGRID)%USTDIR(GSEA)*WT
                      ELSE
                        USTAUX1 = USTAUX1 + WDATAS(IGRID)%UST(GSEA)*WT
                        USTAUX2 = USTAUX2 + WDATAS(IGRID)%USTDIR(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(5,2) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%CHARN(GSEA) .NE. UNDEF ) THEN
                      SUMWT5(2) = SUMWT5(2) + WT
                      IF ( CHARNAUX .EQ. UNDEF ) THEN
                        CHARNAUX = WADATS(IGRID)%CHARN(GSEA)*WT
                      ELSE
                        CHARNAUX = CHARNAUX + WADATS(IGRID)%CHARN(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(5,3) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%CGE(GSEA) .NE. UNDEF ) THEN
                      SUMWT5(3) = SUMWT5(3) + WT
                      IF ( CGEAUX .EQ. UNDEF ) THEN
                        CGEAUX = WADATS(IGRID)%CGE(GSEA)*WT
                      ELSE
                        CGEAUX = CGEAUX + WADATS(IGRID)%CGE(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(5,4) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%PHIAW(GSEA) .NE. UNDEF ) THEN
                      SUMWT5(4) = SUMWT5(4) + WT
                      IF ( PHIAWAUX .EQ. UNDEF ) THEN
                        PHIAWAUX = WADATS(IGRID)%PHIAW(GSEA)*WT
                      ELSE
                        PHIAWAUX = PHIAWAUX + WADATS(IGRID)%PHIAW(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(5,5) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%TAUWIX(GSEA) .NE. UNDEF ) THEN
                      SUMWT5(5) = SUMWT5(5) + WT
                      IF ( TAUWIXAUX .EQ. UNDEF ) THEN
                        TAUWIXAUX = WADATS(IGRID)%TAUWIX(GSEA)*WT
                        TAUWIYAUX = WADATS(IGRID)%TAUWIY(GSEA)*WT
                      ELSE
                        TAUWIXAUX = TAUWIXAUX + WADATS(IGRID)%TAUWIX(GSEA)*WT
                        TAUWIYAUX = TAUWIYAUX + WADATS(IGRID)%TAUWIY(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(5,6) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%TAUWNX(GSEA) .NE. UNDEF ) THEN
                      SUMWT5(6) = SUMWT5(6) + WT
                      IF ( TAUWNXAUX .EQ. UNDEF ) THEN
                        TAUWNXAUX = WADATS(IGRID)%TAUWNX(GSEA)*WT
                        TAUWNYAUX = WADATS(IGRID)%TAUWNY(GSEA)*WT
                      ELSE
                        TAUWNXAUX = TAUWNXAUX + WADATS(IGRID)%TAUWNX(GSEA)*WT
                        TAUWNYAUX = TAUWNYAUX + WADATS(IGRID)%TAUWNY(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  DO ICAP = 1,4
!
                    IF ( FLOGRD(5,ICAP+6) .AND. ACTIVE ) THEN
                      IF ( WADATS(IGRID)%WHITECAP(GSEA,ICAP) .NE. UNDEF ) THEN
                      SUMWTC(ICAP) = SUMWTC(ICAP) + WT
                        IF ( WHITECAPAUX(ICAP) .EQ. UNDEF ) THEN
                          WHITECAPAUX(ICAP) = WADATS(IGRID)%WHITECAP(GSEA,ICAP)&
                                              *WT
                        ELSE
                          WHITECAPAUX(ICAP) = WHITECAPAUX(ICAP) +              &
                                     WADATS(IGRID)%WHITECAP(GSEA,ICAP)*WT
                        END IF
                      END IF
                    END IF
!
                  END DO
!
! Group 6 variables
!
                  IF ( FLOGRD(6,1) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%SXX(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(1) = SUMWT6(1) + WT
                      IF ( SXXAUX .EQ. UNDEF ) THEN
                        SXXAUX = WADATS(IGRID)%SXX(GSEA)*WT
                        SXYAUX = WADATS(IGRID)%SXY(GSEA)*WT
                        SYYAUX = WADATS(IGRID)%SYY(GSEA)*WT
                      ELSE
                        SXXAUX = SXXAUX + WADATS(IGRID)%SXX(GSEA)*WT
                        SXYAUX = SXYAUX + WADATS(IGRID)%SXY(GSEA)*WT
                        SYYAUX = SYYAUX + WADATS(IGRID)%SYY(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,2) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%TAUOX(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(2) = SUMWT6(2) + WT
                      IF ( TAUOXAUX .EQ. UNDEF ) THEN
                        TAUOXAUX = WADATS(IGRID)%TAUOX(GSEA)*WT
                        TAUOYAUX = WADATS(IGRID)%TAUOY(GSEA)*WT
                      ELSE
                        TAUOXAUX = TAUOXAUX + WADATS(IGRID)%TAUOX(GSEA)*WT
                        TAUOYAUX = TAUOYAUX + WADATS(IGRID)%TAUOY(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,3) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%BHD(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(3) = SUMWT6(3) + WT
                      IF ( BHDAUX .EQ. UNDEF ) THEN
                        BHDAUX = WADATS(IGRID)%BHD(GSEA)*WT
                      ELSE
                        BHDAUX = BHDAUX + WADATS(IGRID)%BHD(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,4) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%PHIOC(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(4) = SUMWT6(4) + WT
                      IF ( PHIOCAUX .EQ. UNDEF ) THEN
                        PHIOCAUX = WADATS(IGRID)%PHIOC(GSEA)*WT
                      ELSE
                        PHIOCAUX = PHIOCAUX + WADATS(IGRID)%PHIOC(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,5) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%TUSX(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(5) = SUMWT6(5) + WT
                      IF ( TUSXAUX .EQ. UNDEF ) THEN
                        TUSXAUX = WADATS(IGRID)%TUSX(GSEA)*WT
                        TUSYAUX = WADATS(IGRID)%TUSY(GSEA)*WT
                      ELSE
                        TUSXAUX = TUSXAUX + WADATS(IGRID)%TUSX(GSEA)*WT
                        TUSYAUX = TUSYAUX + WADATS(IGRID)%TUSY(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,6) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%USSX(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(6) = SUMWT6(6) + WT
                      IF ( USSXAUX .EQ. UNDEF ) THEN
                        USSXAUX = WADATS(IGRID)%USSX(GSEA)*WT
                        USSYAUX = WADATS(IGRID)%USSY(GSEA)*WT
                      ELSE
                        USSXAUX = USSXAUX + WADATS(IGRID)%USSX(GSEA)*WT
                        USSYAUX = USSYAUX + WADATS(IGRID)%USSY(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,7) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%PRMS(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(7) = SUMWT6(7) + WT
                      IF ( PRMSAUX .EQ. UNDEF ) THEN
                        PRMSAUX = WADATS(IGRID)%PRMS(GSEA)*WT
                        TPMSAUX = WADATS(IGRID)%TPMS(GSEA)*WT
                      ELSE
                        PRMSAUX = PRMSAUX + WADATS(IGRID)%PRMS(GSEA)*WT
                        TPMSAUX = TPMSAUX + WADATS(IGRID)%TPMS(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,8) .AND. ACTIVE .AND. US3DF(1).GT.0 ) THEN
                    DO IK = US3DF(2),US3DF(3)
                      IF ( WADATS(IGRID)%US3D(GSEA,IK) .NE. UNDEF ) THEN
                        SUMWT68(IK) = SUMWT68(IK) + WT
                        IF ( US3DAUX(IK) .EQ. UNDEF )   US3DAUX(IK) = 0.
                        US3DAUX(IK) = US3DAUX(IK) +                    &
                                          WADATS(IGRID)%US3D(GSEA,IK)*WT
                      END IF
                      IF ( WADATS(IGRID)%US3D(GSEA,NK+IK) .NE. UNDEF ) THEN
                        SUMWT68(NK+IK) = SUMWT68(NK+IK) + WT
                        IF ( US3DAUX(NK+IK) .EQ. UNDEF )               &
                                                     US3DAUX(NK+IK) = 0.
                        US3DAUX(NK+IK) = US3DAUX(NK+IK) +            &
                                     WADATS(IGRID)%US3D(GSEA,NK+IK)*WT
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(6,9) .AND. ACTIVE .AND. P2MSF(1).GT.0) THEN
                    DO IK = P2MSF(2),P2MSF(3)
                      IF ( WADATS(IGRID)%P2SMS(GSEA,IK) .NE. UNDEF ) THEN
                        SUMWT69(IK) = SUMWT69(IK) + WT
                        IF ( P2SMSAUX(IK) .EQ. UNDEF ) P2SMSAUX(IK) = 0.
                        P2SMSAUX(IK) = P2SMSAUX(IK) +                  &
                                         WADATS(IGRID)%P2SMS(GSEA,IK)*WT
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(6,10) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%TAUICE(GSEA,1) .NE. UNDEF ) THEN
                      SUMWT6(10) = SUMWT6(10) + WT
                      IF ( TAUICEAUX(1) .EQ. UNDEF ) TAUICEAUX(1) = 0.
                      IF ( TAUICEAUX(2) .EQ. UNDEF ) TAUICEAUX(2) = 0.
                      TAUICEAUX(1) = TAUICEAUX(1) +                    &
                                         WADATS(IGRID)%TAUICE(GSEA,1)*WT
                      TAUICEAUX(2) = TAUICEAUX(2) +                    &
                                         WADATS(IGRID)%TAUICE(GSEA,2)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,11) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%PHICE(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(11) = SUMWT6(11) + WT
                      IF ( PHICEAUX.EQ.UNDEF ) PHICEAUX = 0.
                      PHICEAUX = PHICEAUX + WADATS(IGRID)%PHICE(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(6,12) .AND. ACTIVE .AND. USSPF(1).GT.0 ) THEN
                    DO IK = 1,USSPF(2)
                      IF ( WADATS(IGRID)%USSP(GSEA,IK) .NE. UNDEF ) THEN
                        SUMWT612(IK) = SUMWT612(IK) + WT
                        IF ( USSPAUX(IK) .EQ. UNDEF ) USSPAUX(IK) = 0.
                        USSPAUX(IK) = USSPAUX(IK) +                  &
                                        WADATS(IGRID)%USSP(GSEA,IK)*WT
                      END IF
                      IF ( WADATS(IGRID)%USSP(GSEA,NK+IK) .NE. UNDEF ) THEN
                        SUMWT612(NK+IK) = SUMWT612(NK+IK) + WT
                        IF ( USSPAUX(NK+IK) .EQ. UNDEF )             &
                                                   USSPAUX(NK+IK) = 0.
                        USSPAUX(NK+IK) = USSPAUX(NK+IK) +            &
                                     WADATS(IGRID)%USSP(GSEA,NK+IK)*WT
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(6,13) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%TAUOCX(GSEA) .NE. UNDEF ) THEN
                      SUMWT6(13) = SUMWT6(13) + WT
                      IF ( TAUOCXAUX .EQ. UNDEF ) THEN
                        TAUOCXAUX = WADATS(IGRID)%TAUOCX(GSEA)*WT
                        TAUOCYAUX = WADATS(IGRID)%TAUOCY(GSEA)*WT
                      ELSE
                        TAUOCXAUX = TAUOCXAUX + WADATS(IGRID)%TAUOCX(GSEA)*WT
                        TAUOCYAUX = TAUOCYAUX + WADATS(IGRID)%TAUOCY(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
! Group 7 variables
!
                  IF ( FLOGRD(7,1) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%ABA(GSEA) .NE. UNDEF ) THEN
                      SUMWT7(1) = SUMWT7(1) + WT
                      IF ( ABAAUX .EQ. UNDEF ) THEN
                        ABAAUX = WADATS(IGRID)%ABA(GSEA)*WT
                        ABDAUX = WADATS(IGRID)%ABD(GSEA)*WT
                      ELSE
                        ABAAUX = ABAAUX + WADATS(IGRID)%ABA(GSEA)*WT
                        ABDAUX = ABDAUX + WADATS(IGRID)%ABD(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(7,2) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%ABA(GSEA) .NE. UNDEF ) THEN
                      SUMWT7(2) = SUMWT7(2) + WT
                      IF ( UBAAUX .EQ. UNDEF ) THEN
                        UBAAUX = WADATS(IGRID)%UBA(GSEA)*WT
                        UBDAUX = WADATS(IGRID)%UBD(GSEA)*WT
                      ELSE
                        UBAAUX = UBAAUX + WADATS(IGRID)%UBA(GSEA)*WT
                        UBDAUX = UBDAUX + WADATS(IGRID)%UBD(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(7,3) .AND. ACTIVE ) THEN
                    DO IBED = 1, 3
                      IF ( WADATS(IGRID)%BEDFORMS(GSEA,IBED) .NE. UNDEF ) THEN
                      SUMWTB(IBED) = SUMWTB(IBED) + WT
                        IF ( BEDFORMSAUX(IBED) .EQ. UNDEF ) THEN
                          BEDFORMSAUX(IBED) = WADATS(IGRID)%BEDFORMS(GSEA,IBED)&
                                              *WT
                        ELSE
                          BEDFORMSAUX(IBED) = BEDFORMSAUX(IBED) +              &
                                   WADATS(IGRID)%BEDFORMS(GSEA,IBED)*WT
                        END IF
                      END IF
                    END DO
                  END IF
!
                  IF ( FLOGRD(7,4) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%PHIBBL(GSEA) .NE. UNDEF ) THEN
                      SUMWT7(4) = SUMWT7(4) + WT
                      IF ( PHIBBLAUX .EQ. UNDEF ) THEN
                         PHIBBLAUX = WADATS(IGRID)%PHIBBL(GSEA)*WT
                      ELSE
                        PHIBBLAUX = PHIBBLAUX + WADATS(IGRID)%PHIBBL(GSEA)*WT
                      END IF
                    END IF
                  END IF
!
                  IF ( FLOGRD(7,5) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%TAUBBL(GSEA,1) .NE. UNDEF ) THEN
                      SUMWT7(5) = SUMWT7(5) + WT
                      IF ( TAUBBLAUX(1) .EQ. UNDEF ) THEN
                        TAUBBLAUX(1) = WADATS(IGRID)%TAUBBL(GSEA,1)*WT
                        TAUBBLAUX(2) = WADATS(IGRID)%TAUBBL(GSEA,2)*WT
                      ELSE
                        TAUBBLAUX(1) = TAUBBLAUX(1) +                          &
                                WADATS(IGRID)%TAUBBL(GSEA,1)*WT
                        TAUBBLAUX(2) = TAUBBLAUX(2) +                          &
                                WADATS(IGRID)%TAUBBL(GSEA,2)*WT
                      END IF
                    END IF
                  END IF
!
! Group 8 variables
!
                  IF ( FLOGRD(8,1) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%MSSX(GSEA) .NE. UNDEF ) THEN
                      SUMWT8(1) = SUMWT8(1) + WT
                      IF ( MSSXAUX .EQ. UNDEF )   MSSXAUX = 0.
                      IF ( MSSYAUX .EQ. UNDEF )   MSSYAUX = 0.
                      MSSXAUX = MSSXAUX + WADATS(IGRID)%MSSX(GSEA)*WT
                      MSSYAUX = MSSYAUX + WADATS(IGRID)%MSSY(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(8,2) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%MSCX(GSEA) .NE. UNDEF ) THEN
                      SUMWT8(2) = SUMWT8(2) + WT
                      IF ( MSCXAUX .EQ. UNDEF )   MSCXAUX = 0.
                      IF ( MSCYAUX .EQ. UNDEF )   MSCYAUX = 0.
                      MSCXAUX = MSCXAUX + WADATS(IGRID)%MSCX(GSEA)*WT
                      MSCYAUX = MSCYAUX + WADATS(IGRID)%MSCY(GSEA)*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(8,3) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%MSSD(GSEA) .NE. UNDEF ) THEN
                      SUMWT8(3) = SUMWT8(3) + WT
                      IF ( MSSDAUX1 .EQ. UNDEF )   MSSDAUX1 = 0.
                      IF ( MSSDAUX2 .EQ. UNDEF )   MSSDAUX2 = 0.
                      MSSDAUX1 = MSSDAUX1 +                           &
                                 COS ( WADATS(IGRID)%MSSD(GSEA) )*WT
                      MSSDAUX2 = MSSDAUX2 +                           &
                                 SIN ( WADATS(IGRID)%MSSD(GSEA) )*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(8,4) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%MSCD(GSEA) .NE. UNDEF ) THEN
                      SUMWT8(4) = SUMWT8(4) + WT
                      IF ( MSCDAUX1 .EQ. UNDEF )   MSCDAUX1 = 0.
                      IF ( MSCDAUX2 .EQ. UNDEF )   MSCDAUX2 = 0.
                      MSCDAUX1 = MSCDAUX1 +                           &
                                 COS ( WADATS(IGRID)%MSCD(GSEA) )*WT
                      MSCDAUX2 = MSCDAUX2 +                           &
                                 SIN ( WADATS(IGRID)%MSCD(GSEA) )*WT
                    END IF
                  END IF
!
                  IF ( FLOGRD(8,5) .AND. ACTIVE ) THEN
                    IF ( WADATS(IGRID)%QP(GSEA) .NE. UNDEF ) THEN
                      SUMWT8(5) = SUMWT8(5) + WT
                      IF ( QPAUX .EQ. UNDEF )   QPAUX = 0.
                      QPAUX = QPAUX + WADATS(IGRID)%QP(GSEA)*WT
                    END IF
                  END IF
!
! End of loop through the points per grid to obtain interpolated values
                END DO   !/ IPTS = 1, ...
!
! Save temp. interpolated variables in proper variables
! (weighted by the number of grids)
!
!
! Group 1 variables
!
                IF ( DWAUX .NE. UNDEF ) THEN
                  DWAUX = DWAUX / SUMWT1(1)
                  IF ( DW(ISEA) .EQ. UNDEF )  THEN
                    DW(ISEA) = DWAUX / REAL( SUMGRD )
                  ELSE
                    DW(ISEA) = DW(ISEA) + DWAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( CXAUX .NE. UNDEF ) THEN
                  CXAUX = CXAUX / SUMWT1(2)
                  CYAUX = CYAUX / SUMWT1(2)
                  IF ( CX(ISEA) .EQ. UNDEF )  THEN
                    CX(ISEA) = CXAUX / REAL( SUMGRD )
                    CY(ISEA) = CYAUX / REAL( SUMGRD )
                  ELSE
                    CX(ISEA) = CX(ISEA) + CXAUX / REAL( SUMGRD ) 
                    CY(ISEA) = CY(ISEA) + CYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( UAAUX .NE. UNDEF ) THEN
                  UAAUX = UAAUX / SUMWT1(3)
                  UDAUX = UDAUX / SUMWT1(3)
                  IF ( UA(ISEA) .EQ. UNDEF )  THEN
                    UA(ISEA) = UAAUX / REAL( SUMGRD ) 
                    UD(ISEA) = UDAUX / REAL( SUMGRD )
                  ELSE
                    UA(ISEA) = UA(ISEA) + UAAUX / REAL( SUMGRD ) 
                    UD(ISEA) = UD(ISEA) + UDAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( ASAUX .NE. UNDEF ) THEN
                  ASAUX = ASAUX / SUMWT1(4)
                  IF ( AS(ISEA) .EQ. UNDEF )  THEN
                    AS(ISEA) = ASAUX / REAL( SUMGRD )
                  ELSE
                    AS(ISEA) = AS(ISEA) + ASAUX / REAL( SUMGRD ) 
                  END IF
                END IF
!
                IF ( WLVAUX .NE. UNDEF ) THEN
                  WLVAUX = WLVAUX / SUMWT1(5)
                  IF ( WLV(ISEA) .EQ. UNDEF )  THEN
                    WLV(ISEA) = WLVAUX / REAL( SUMGRD )
                  ELSE
                    WLV(ISEA) = WLV(ISEA) + WLVAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( ICEAUX .NE. UNDEF ) THEN
                  ICEAUX = ICEAUX / SUMWT1(6)
                  IF ( ICE(ISEA) .EQ. UNDEF )  THEN
                    ICE(ISEA) = ICEAUX / REAL( SUMGRD )
                  ELSE
                    ICE(ISEA) = ICE(ISEA) + ICEAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( BERGAUX .NE. UNDEF ) THEN
                  BERGAUX = BERGAUX / SUMWT1(7)
                  IF ( BERG(ISEA) .EQ. UNDEF )  THEN
                    BERG(ISEA) = BERGAUX / REAL( SUMGRD )
                  ELSE
                    BERG(ISEA) = BERG(ISEA) + BERGAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( TAUAAUX .NE. UNDEF ) THEN
                  TAUAAUX = TAUAAUX / SUMWT1(8)
                  TAUADIRAUX = TAUADIRAUX / SUMWT1(8)
                  IF ( TAUA(ISEA) .EQ. UNDEF )  THEN
                    TAUA(ISEA) = TAUAAUX / REAL( SUMGRD ) 
                    TAUADIR(ISEA) = TAUADIRAUX / REAL( SUMGRD )
                  ELSE
                    TAUA(ISEA) = TAUA(ISEA) + TAUAAUX / REAL( SUMGRD ) 
                    TAUADIR(ISEA) = TAUADIR(ISEA) + TAUADIRAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( RHOAIRAUX .NE. UNDEF ) THEN
                  RHOAIRAUX = RHOAIRAUX / SUMWT1(9)
                  IF ( RHOAIR(ISEA) .EQ. UNDEF )  THEN
                    RHOAIR(ISEA) = RHOAIRAUX / REAL( SUMGRD )
                  ELSE
                    RHOAIR(ISEA) = RHOAIR(ISEA) + RHOAIRAUX / REAL( SUMGRD )
                  END IF
                END IF
!
#ifdef W3_BT4
                IF ( SED_D50AUX .NE. UNDEF ) THEN
                  SED_D50AUX = SED_D50AUX / SUMWT1(10)
                  IF ( SED_D50(ISEA) .EQ. UNDEF )  THEN
                    SED_D50(ISEA) = SED_D50AUX / REAL( SUMGRD )
                  ELSE
                    SED_D50(ISEA) = SED_D50(ISEA) + SED_D50AUX / REAL( SUMGRD )
                  END IF
                END IF
#endif
!
#ifdef W3_IS2
                IF ( ICEHAUX .NE. UNDEF ) THEN
                  ICEHAUX = ICEHAUX / SUMWT1(11)
                  IF ( ICEH(ISEA) .EQ. UNDEF )  THEN
                    ICEH(ISEA) = ICEHAUX / REAL( SUMGRD )
                  ELSE
                    ICEH(ISEA) = ICEH(ISEA) + ICEHAUX / REAL( SUMGRD )
                  END IF
                END IF
#endif
!
#ifdef W3_IS2
                IF ( ICEFAUX .NE. UNDEF ) THEN
                  ICEFAUX = ICEFAUX / SUMWT1(12)
                  IF ( ICEF(ISEA) .EQ. UNDEF )  THEN
                    ICEF(ISEA) = ICEFAUX / REAL( SUMGRD )
                  ELSE
                    ICEF(ISEA) = ICEF(ISEA) + ICEFAUX / REAL( SUMGRD )
                  END IF
                END IF
#endif
!
! Group 2 variables
!
                IF ( HSAUX .NE. UNDEF ) THEN
                  HSAUX = HSAUX / SUMWT2(1)
                  IF ( HS(ISEA) .EQ. UNDEF )  THEN
                    HS(ISEA) = HSAUX / REAL( SUMGRD )
                  ELSE
                    HS(ISEA) = HS(ISEA) + HSAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( WLMAUX .NE. UNDEF ) THEN
                  WLMAUX = WLMAUX / SUMWT2(2)
                  IF ( WLM(ISEA) .EQ. UNDEF )  THEN
                    WLM(ISEA) = WLMAUX / REAL( SUMGRD )
                  ELSE
                    WLM(ISEA) = WLM(ISEA) + WLMAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( T02AUX .NE. UNDEF ) THEN
                  T02AUX = T02AUX / SUMWT2(3)
                  IF ( T02(ISEA) .EQ. UNDEF )  THEN
                    T02(ISEA) = T02AUX / REAL( SUMGRD )
                  ELSE
                    T02(ISEA) = T02(ISEA) + T02AUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( T0M1AUX .NE. UNDEF ) THEN
                  T0M1AUX = T0M1AUX / SUMWT2(4)
                  IF ( T0M1(ISEA) .EQ. UNDEF )  THEN
                    T0M1(ISEA) = T0M1AUX / REAL( SUMGRD )
                  ELSE
                    T0M1(ISEA) = T0M1(ISEA) + T0M1AUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( T01AUX .NE. UNDEF ) THEN
                  T01AUX = T01AUX / SUMWT2(5)
                  IF ( T01(ISEA) .EQ. UNDEF )  THEN
                    T01(ISEA) = T01AUX / REAL( SUMGRD )
                  ELSE
                    T01(ISEA) = T01(ISEA) + T01AUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( FP0AUX .NE. UNDEF ) THEN
                  FP0AUX = FP0AUX / SUMWT2(6)
                  IF ( FP0(ISEA) .EQ. UNDEF )  THEN
                    FP0(ISEA) = FP0AUX / REAL( SUMGRD )
                  ELSE
                    FP0(ISEA) = FP0(ISEA) + FP0AUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( THMAUX1 .NE. UNDEF ) THEN
                  THMAUX1 = THMAUX1 / SUMWT2(7)
                  THMAUX2 = THMAUX2 / SUMWT2(7)
                  IF ( THM(ISEA) .EQ. UNDEF )  THEN
                    THMAUX1 = THMAUX1 / REAL( SUMGRD )
                    THMAUX2 = THMAUX2 / REAL( SUMGRD )
                    THM(ISEA) = ATAN2 ( THMAUX2, THMAUX1 )
                  ELSE
                    THMAUX1 = THMAUX1 / REAL( SUMGRD ) +  COS ( THM(ISEA) )
                    THMAUX2 = THMAUX2 / REAL( SUMGRD ) +  SIN ( THM(ISEA) )
                    THM(ISEA) = ATAN2 ( THMAUX2, THMAUX1 )
                  END IF
                END IF
!
                IF ( THSAUX .NE. UNDEF ) THEN
                  THSAUX = THSAUX / SUMWT2(8)
                  IF ( THS(ISEA) .EQ. UNDEF )  THEN
                    THS(ISEA) = THSAUX / REAL( SUMGRD )
                  ELSE
                    THS(ISEA) = THS(ISEA) + THSAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( THP0AUX1 .NE. UNDEF ) THEN
                  THP0AUX1 = THP0AUX1 / SUMWT2(9)
                  THP0AUX2 = THP0AUX2 / SUMWT2(9)
                  IF ( THP0(ISEA) .EQ. UNDEF )  THEN
                    THP0AUX1 = THP0AUX1 / REAL( SUMGRD )
                    THP0AUX2 = THP0AUX2 / REAL( SUMGRD )
                    THP0(ISEA) = ATAN2 ( THP0AUX2, THP0AUX1 )
                  ELSE
                    THP0AUX1 = THP0AUX1 / REAL( SUMGRD ) + COS ( THP0(ISEA) )
                    THP0AUX2 = THP0AUX2 / REAL( SUMGRD ) + SIN ( THP0(ISEA) )
                    THP0(ISEA) = ATAN2 ( THP0AUX2, THP0AUX1 )
                  END IF
                END IF
!
                IF ( HSIGAUX .NE. UNDEF ) THEN
                  IF ( HSIG(ISEA) .EQ. UNDEF )   HSIG(ISEA) = 0.
                  HSIG(ISEA) = HSIG(ISEA) +                            &
                               HSIGAUX / REAL( SUMWT2(10)*SUMGRD )
                END IF
!
                IF ( STMAXEAUX .NE. UNDEF ) THEN
                  IF ( STMAXE(ISEA) .EQ. UNDEF )   STMAXE(ISEA) = 0.
                  STMAXE(ISEA) = STMAXE(ISEA) +                        &
                               STMAXEAUX / REAL( SUMWT2(11) * SUMGRD )
                END IF
!
                IF ( STMAXDAUX .NE. UNDEF ) THEN
                  IF ( STMAXD(ISEA) .EQ. UNDEF )   STMAXD(ISEA) = 0.
                  STMAXD(ISEA) = STMAXD(ISEA) +                        &
                                 STMAXDAUX / REAL( SUMWT2(12) * SUMGRD )
                END IF
!
                IF ( HMAXEAUX .NE. UNDEF ) THEN
                  IF ( HMAXE(ISEA) .EQ. UNDEF )   HMAXE(ISEA) = 0.
                  HMAXE(ISEA) = HMAXE(ISEA) +                          &
                                HMAXEAUX / REAL( SUMWT2(13) * SUMGRD )
                END IF
!
                IF ( HCMAXEAUX .NE. UNDEF ) THEN
                  IF ( HCMAXE(ISEA) .EQ. UNDEF )   HCMAXE(ISEA) = 0.
                  HCMAXE(ISEA) = HCMAXE(ISEA) +                        &
                                 HCMAXEAUX / REAL( SUMWT2(14) * SUMGRD )
                END IF
!
                IF ( HMAXDAUX .NE. UNDEF ) THEN
                  IF ( HMAXD(ISEA) .EQ. UNDEF )   HMAXD(ISEA) = 0.
                  HMAXD(ISEA) = HMAXD(ISEA)  +                         &
                                HMAXDAUX / REAL( SUMWT2(15) * SUMGRD )
                END IF
!
                IF ( HCMAXDAUX .NE. UNDEF ) THEN
                  IF ( HCMAXD(ISEA) .EQ. UNDEF )   HCMAXD(ISEA) = 0.
                  HCMAXD(ISEA) = HCMAXD(ISEA) +                        &
                                 HCMAXDAUX / REAL( SUMWT2(16) * SUMGRD )
                END IF
!
                IF ( WBTAUX .NE. UNDEF ) THEN
                  IF ( WBT(ISEA) .EQ. UNDEF )   WBT(ISEA) = 0.
                  WBT(ISEA) = WBT(ISEA) +                            &
                               WBTAUX / REAL( SUMWT2(17)*SUMGRD )
                END IF
!
                IF ( WNMEANAUX .NE. UNDEF ) THEN
                  IF ( WNMEAN(ISEA) .EQ. UNDEF )   WNMEAN(ISEA) = 0.
                  WNMEAN(ISEA) = WNMEAN(ISEA) +                      &
                               WNMEANAUX / REAL( SUMWT2(19)*SUMGRD )
                END IF
!
! Group 3 variables
!
                IF (  E3DF(1,1).GT.0 ) THEN
                  DO IFREQ = E3DF(2,1),E3DF(3,1)
                    IF ( EFAUX(IFREQ) .NE. UNDEF ) THEN
                      EFAUX(IFREQ) = EFAUX(IFREQ) / SUMWT3A(IFREQ)
                      IF ( EF(ISEA,IFREQ) .EQ. UNDEF )  THEN
                        EF(ISEA,IFREQ) = EFAUX(IFREQ) / REAL( SUMGRD )
                      ELSE
                        EF(ISEA,IFREQ) = EF(ISEA,IFREQ) +             &
                            EFAUX(IFREQ) / REAL( SUMGRD )
                      END IF
                    END IF
                  END DO
                END IF
!
                IF (  E3DF(1,2).GT.0 ) THEN
                  DO IFREQ = E3DF(2,2),E3DF(3,2)
                    IF ( TH1MAUX(IFREQ) .NE. UNDEF ) THEN
                      TH1MAUX(IFREQ) = TH1MAUX(IFREQ) / SUMWT3B(IFREQ)
                      IF ( TH1M(ISEA,IFREQ) .EQ. UNDEF )  THEN
                        TH1M(ISEA,IFREQ) = TH1MAUX(IFREQ) / REAL( SUMGRD )
                      ELSE
                        TH1M(ISEA,IFREQ) = TH1M(ISEA,IFREQ) +         &
                            TH1MAUX(IFREQ) / REAL( SUMGRD )
                      END IF
                    END IF
                  END DO
                END IF
!
                IF (  E3DF(1,3).GT.0 ) THEN
                  DO IFREQ = E3DF(2,3),E3DF(3,3)
                    IF ( STH1MAUX(IFREQ) .NE. UNDEF ) THEN
                      STH1MAUX(IFREQ) = STH1MAUX(IFREQ) / SUMWT3C(IFREQ)
                      IF ( STH1M(ISEA,IFREQ) .EQ. UNDEF )  THEN
                        STH1M(ISEA,IFREQ) = STH1MAUX(IFREQ) / REAL( SUMGRD )
                      ELSE
                        STH1M(ISEA,IFREQ) = STH1M(ISEA,IFREQ) +       &
                            STH1MAUX(IFREQ) / REAL( SUMGRD )
                      END IF
                    END IF
                  END DO
                END IF
!
                IF (  E3DF(1,4).GT.0 ) THEN
                  DO IFREQ = E3DF(2,4),E3DF(3,4)
                    IF ( TH2MAUX(IFREQ) .NE. UNDEF ) THEN
                      TH2MAUX(IFREQ) = TH2MAUX(IFREQ) / SUMWT3D(IFREQ)
                      IF ( TH2M(ISEA,IFREQ) .EQ. UNDEF )  THEN
                        TH2M(ISEA,IFREQ) = TH2MAUX(IFREQ) / REAL( SUMGRD )
                      ELSE
                        TH2M(ISEA,IFREQ) = TH2M(ISEA,IFREQ) +         &
                            TH2MAUX(IFREQ) / REAL( SUMGRD )
                      END IF
                    END IF
                  END DO
                END IF
!
                IF (  E3DF(1,5).GT.0 ) THEN
                  DO IFREQ = E3DF(2,5),E3DF(3,5)
                    IF ( STH2MAUX(IFREQ) .NE. UNDEF ) THEN
                      STH2MAUX(IFREQ) = STH2MAUX(IFREQ) / SUMWT3E(IFREQ)
                      IF ( STH2M(ISEA,IFREQ) .EQ. UNDEF )  THEN
                        STH2M(ISEA,IFREQ) = STH2MAUX(IFREQ) / REAL( SUMGRD )
                      ELSE
                        STH2M(ISEA,IFREQ) = STH2M(ISEA,IFREQ) +       &
                            STH2MAUX(IFREQ) / REAL( SUMGRD )
                      END IF
                    END IF
                  END DO
                END IF
!
                DO IK = 1,NK
                  IF ( WNAUX(IK) .NE. UNDEF ) THEN
                    WNAUX(IK) = WNAUX(IK) / SUMWT3F(IK)
                    IF ( WN(IK,ISEA) .EQ. UNDEF )  THEN
                      WN(IK,ISEA) = WNAUX(IK) / REAL( SUMGRD )
                    ELSE
                      WN(IK,ISEA) = WN(IK,ISEA) +                     &
                          WNAUX(IK) / REAL( SUMGRD )
                    END IF
                  END IF
                END DO
!
! Group 4 variables
!
                DO ISWLL = 0, NOSWLL_MIN
!
                  IF ( PHSAUX(ISWLL) .NE. UNDEF ) THEN
                    PHSAUX(ISWLL) = PHSAUX(ISWLL) / SUMWT4(1,ISWLL)
                    IF ( PHS(ISEA,ISWLL) .EQ. UNDEF )  THEN
                      PHS(ISEA,ISWLL) = PHSAUX(ISWLL) / REAL( SUMGRD )
                    ELSE
                      PHS(ISEA,ISWLL) = PHS(ISEA,ISWLL) +                      &
                          PHSAUX(ISWLL) / REAL( SUMGRD )
                    END IF
                  END IF
!
                  IF ( PTPAUX(ISWLL) .NE. UNDEF ) THEN
                    PTPAUX(ISWLL) = PTPAUX(ISWLL) / SUMWT4(2,ISWLL)
                    IF ( PTP(ISEA,ISWLL) .EQ. UNDEF )  THEN
                      PTP(ISEA,ISWLL) = PTPAUX(ISWLL) / REAL( SUMGRD )
                    ELSE
                      PTP(ISEA,ISWLL) = PTP(ISEA,ISWLL) +                      &
                          PTPAUX(ISWLL) / REAL( SUMGRD )
                    END IF
                  END IF
!
                  IF ( PLPAUX(ISWLL) .NE. UNDEF ) THEN
                    PLPAUX(ISWLL) = PLPAUX(ISWLL) / SUMWT4(3,ISWLL)
                    IF ( PLP(ISEA,ISWLL) .EQ. UNDEF )  THEN
                      PLP(ISEA,ISWLL) = PLPAUX(ISWLL) / REAL( SUMGRD )
                    ELSE
                      PLP(ISEA,ISWLL) = PLP(ISEA,ISWLL) +                      &
                          PLPAUX(ISWLL) / REAL( SUMGRD )
                    END IF
                  END IF
!
                  IF ( PDIRAUX1(ISWLL) .NE. UNDEF ) THEN
                    PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) / SUMWT4(4,ISWLL)
                    PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) / SUMWT4(4,ISWLL)
                    IF ( PDIR(ISEA,ISWLL) .EQ. UNDEF )  THEN
                      PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) / REAL( SUMGRD )
                      PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) / REAL( SUMGRD )
                      PDIR(ISEA,ISWLL) = ATAN2 ( PDIRAUX2(ISWLL), PDIRAUX1(ISWLL) )
                    ELSE
                      PDIRAUX1(ISWLL) = PDIRAUX1(ISWLL) / REAL( SUMGRD ) +       &
                               COS ( PDIR(ISEA,ISWLL) )
                      PDIRAUX2(ISWLL) = PDIRAUX2(ISWLL) / REAL( SUMGRD ) +       &
                               SIN ( PDIR(ISEA,ISWLL) )
                      PDIR(ISEA,ISWLL) = ATAN2 ( PDIRAUX2(ISWLL), PDIRAUX1(ISWLL) )
                    END IF
                  END IF
!
                  IF ( PSIAUX(ISWLL) .NE. UNDEF ) THEN
                    PSIAUX(ISWLL) = PSIAUX(ISWLL) / SUMWT4(5,ISWLL)
                    IF ( PSI(ISEA,ISWLL) .EQ. UNDEF )  THEN
                      PSI(ISEA,ISWLL) = PSIAUX(ISWLL) / REAL( SUMGRD )
                    ELSE
                      PSI(ISEA,ISWLL) = PSI(ISEA,ISWLL) +                      &
                          PSIAUX(ISWLL) / REAL( SUMGRD )
                    END IF
                  END IF
!
                  IF ( PWSAUX(ISWLL) .NE. UNDEF ) THEN
                    PWSAUX(ISWLL) = PWSAUX(ISWLL) / SUMWT4(6,ISWLL)
                    IF ( PWS(ISEA,ISWLL) .EQ. UNDEF )  THEN
                      PWS(ISEA,ISWLL) = PWSAUX(ISWLL) / REAL( SUMGRD )
                    ELSE
                      PWS(ISEA,ISWLL) = PWS(ISEA,ISWLL) +                      &
                          PWSAUX(ISWLL) / REAL( SUMGRD )
                    END IF
                  END IF
!
                  IF ( PTHP0AUX1(ISWLL) .NE. UNDEF ) THEN
                    PTHP0AUX1(ISWLL) = PTHP0AUX1(ISWLL)                &
                                        / REAL( SUMWT4(7,ISWLL)*SUMGRD )
                    PTHP0AUX2(ISWLL) = PTHP0AUX2(ISWLL)                &
                                        / REAL( SUMWT4(7,ISWLL)*SUMGRD )
                    IF ( PTHP0(ISEA,ISWLL) .NE. UNDEF ) THEN
                      PTHP0AUX1(ISWLL) = PTHP0AUX1(ISWLL) +            &
                                               COS ( PTHP0(ISEA,ISWLL) )
                      PTHP0AUX2(ISWLL) = PTHP0AUX2(ISWLL) +            &
                                               SIN ( PTHP0(ISEA,ISWLL) )
                    END IF
                    PTHP0(ISEA,ISWLL) =                                &
                            ATAN2 ( PTHP0AUX2(ISWLL), PTHP0AUX1(ISWLL) )
                  END IF
!
                  IF ( PQPAUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PQP(ISEA,ISWLL) .EQ. UNDEF )                  &
                                                    PQP(ISEA,ISWLL) = 0.
                    PQP(ISEA,ISWLL) = PQP(ISEA,ISWLL) +                &
                         PQPAUX(ISWLL) / REAL( SUMWT4(8,ISWLL)*SUMGRD )
                  END IF
!
                  IF ( PPEAUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PPE(ISEA,ISWLL) .EQ. UNDEF )                  &
                                                    PPE(ISEA,ISWLL) = 0.
                    PPE(ISEA,ISWLL) = PPE(ISEA,ISWLL) +                &
                         PPEAUX(ISWLL) / REAL( SUMWT4(9,ISWLL)*SUMGRD )

                  END IF
!
                  IF ( PGWAUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PGW(ISEA,ISWLL) .EQ. UNDEF )                  &
                                                    PGW(ISEA,ISWLL) = 0.
                    PGW(ISEA,ISWLL) = PGW(ISEA,ISWLL) +                &
                         PGWAUX(ISWLL) / REAL( SUMWT4(10,ISWLL)*SUMGRD )
                  END IF
!
                  IF ( PSWAUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PSW(ISEA,ISWLL) .EQ. UNDEF )                  &
                                                    PSW(ISEA,ISWLL) = 0.
                    PSW(ISEA,ISWLL) = PSW(ISEA,ISWLL) +                &
                         PSWAUX(ISWLL) / REAL( SUMWT4(11,ISWLL)*SUMGRD )
                  END IF
!
                  IF ( PTM1AUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PTM1(ISEA,ISWLL) .EQ. UNDEF )                &
                                                  PTM1(ISEA,ISWLL) = 0.
                    PTM1(ISEA,ISWLL) = PTM1(ISEA,ISWLL) +            &
                       PTM1AUX(ISWLL) / REAL( SUMWT4(12,ISWLL)*SUMGRD )
                  END IF
!
                  IF ( PT1AUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PT1(ISEA,ISWLL) .EQ. UNDEF )                  &
                                                    PT1(ISEA,ISWLL) = 0.
                    PT1(ISEA,ISWLL) = PT1(ISEA,ISWLL) +                &
                         PT1AUX(ISWLL) / REAL( SUMWT4(13,ISWLL)*SUMGRD )
                  END IF
!
                  IF ( PT2AUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PT2(ISEA,ISWLL) .EQ. UNDEF )                  &
                                                    PT2(ISEA,ISWLL) = 0.
                    PT2(ISEA,ISWLL) = PT2(ISEA,ISWLL) +                &
                         PT2AUX(ISWLL) / REAL( SUMWT4(14,ISWLL)*SUMGRD )
                  END IF
!
                  IF ( PEPAUX(ISWLL) .NE. UNDEF ) THEN
                    IF ( PEP(ISEA,ISWLL) .EQ. UNDEF )                  &
                                                    PEP(ISEA,ISWLL) = 0.
                    PEP(ISEA,ISWLL) = PEP(ISEA,ISWLL) +                &
                         PEPAUX(ISWLL) / REAL( SUMWT4(15,ISWLL)*SUMGRD )
                  END IF
!
                END DO  !/ ISWLL = 0, NOSWLL_MIN
!
                IF ( PWSTAUX .NE. UNDEF ) THEN
                  PWSTAUX = PWSTAUX / SUMWT4(16,0)
                  IF ( PWST(ISEA) .EQ. UNDEF )  THEN
                    PWST(ISEA) = PWSTAUX / REAL( SUMGRD )
                  ELSE
                    PWST(ISEA) = PWST(ISEA) + PWSTAUX / REAL( SUMGRD )
                  END IF
                END IF
!
! Group 5 variables
!
                IF ( USTAUX1 .NE. UNDEF ) THEN
                  USTAUX1 = USTAUX1 / SUMWT5(1)
                  USTAUX2 = USTAUX2 / SUMWT5(1)
                  IF ( UST(ISEA) .EQ. UNDEF )  THEN
                    UST(ISEA) = USTAUX1 / REAL( SUMGRD )
                    USTDIR(ISEA) = USTAUX2 / REAL( SUMGRD )
                  ELSE
                    UST(ISEA) = UST(ISEA) + USTAUX1 / REAL( SUMGRD )
                    USTDIR(ISEA) = USTDIR(ISEA) + USTAUX2 / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( CHARNAUX .NE. UNDEF ) THEN
                  CHARNAUX = CHARNAUX / SUMWT5(2)
                  IF ( CHARN(ISEA) .EQ. UNDEF )  THEN
                    CHARN(ISEA) = CHARNAUX / REAL( SUMGRD )
                  ELSE
                    CHARN(ISEA) = CHARN(ISEA) + CHARNAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( CGEAUX .NE. UNDEF ) THEN
                  CGEAUX = CGEAUX / SUMWT5(3)
                  IF ( CGE(ISEA) .EQ. UNDEF )  THEN
                    CGE(ISEA) = CGEAUX / REAL( SUMGRD )
                  ELSE
                    CGE(ISEA) = CGE(ISEA) + CGEAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( PHIAWAUX .NE. UNDEF ) THEN
                  PHIAWAUX = PHIAWAUX / SUMWT5(4)
                  IF ( PHIAW(ISEA) .EQ. UNDEF )  THEN
                    PHIAW(ISEA) = PHIAWAUX / REAL( SUMGRD )
                  ELSE
                    PHIAW(ISEA) = PHIAW(ISEA) + PHIAWAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( TAUWIXAUX .NE. UNDEF ) THEN
                  TAUWIXAUX = TAUWIXAUX / SUMWT5(5)
                  TAUWIYAUX = TAUWIYAUX / SUMWT5(5)
                  IF ( TAUWIX(ISEA) .EQ. UNDEF )  THEN
                    TAUWIX(ISEA) = TAUWIXAUX / REAL( SUMGRD )
                    TAUWIY(ISEA) = TAUWIYAUX / REAL( SUMGRD )
                  ELSE
                    TAUWIX(ISEA) = TAUWIX(ISEA) + TAUWIXAUX / REAL( SUMGRD )
                    TAUWIY(ISEA) = TAUWIY(ISEA) + TAUWIYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( TAUWNXAUX .NE. UNDEF ) THEN
                  TAUWNXAUX = TAUWNXAUX / SUMWT5(6)
                  TAUWNYAUX = TAUWNYAUX / SUMWT5(6)
                  IF ( TAUWNX(ISEA) .EQ. UNDEF )  THEN
                    TAUWNX(ISEA) = TAUWNXAUX / REAL( SUMGRD )
                    TAUWNY(ISEA) = TAUWNYAUX / REAL( SUMGRD )
                  ELSE
                    TAUWNX(ISEA) = TAUWNX(ISEA) + TAUWNXAUX / REAL( SUMGRD )
                    TAUWNY(ISEA) = TAUWNY(ISEA) + TAUWNYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                DO ICAP = 1,4
                  IF ( WHITECAPAUX(ICAP) .NE. UNDEF ) THEN
                    WHITECAPAUX(ICAP) = WHITECAPAUX(ICAP) / SUMWTC(ICAP)
                    IF ( WHITECAP(ISEA,ICAP) .EQ. UNDEF )  THEN
                      WHITECAP(ISEA,ICAP) = WHITECAPAUX(ICAP) / REAL( SUMGRD ) 
                    ELSE
                      WHITECAP(ISEA,ICAP) = WHITECAP(ISEA,ICAP) +              &
                               WHITECAPAUX(ICAP) / REAL( SUMGRD )
                    END IF
                  END IF
                END DO
!
! Group 6 variables
!
                IF ( SXXAUX .NE. UNDEF ) THEN
                  SXXAUX = SXXAUX / SUMWT6(1)
                  SXYAUX = SXYAUX / SUMWT6(1)
                  SYYAUX = SYYAUX / SUMWT6(1)
                  IF ( SXX(ISEA) .EQ. UNDEF )  THEN
                    SXX(ISEA) = SXXAUX / REAL( SUMGRD )
                    SXY(ISEA) = SXYAUX / REAL( SUMGRD )
                    SYY(ISEA) = SYYAUX / REAL( SUMGRD )
                  ELSE
                    SXX(ISEA) = SXX(ISEA) + SXXAUX / REAL( SUMGRD )
                    SXY(ISEA) = SXY(ISEA) + SXYAUX / REAL( SUMGRD )
                    SYY(ISEA) = SYY(ISEA) + SYYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( TAUOXAUX .NE. UNDEF ) THEN
                  TAUOXAUX = TAUOXAUX / SUMWT6(2)
                  TAUOYAUX = TAUOYAUX / SUMWT6(2)
                  IF ( TAUOX(ISEA) .EQ. UNDEF )  THEN
                    TAUOX(ISEA) = TAUOXAUX / REAL( SUMGRD )
                    TAUOY(ISEA) = TAUOYAUX / REAL( SUMGRD )
                  ELSE
                    TAUOX(ISEA) = TAUOX(ISEA) + TAUOXAUX / REAL( SUMGRD )
                    TAUOY(ISEA) = TAUOY(ISEA) + TAUOYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( BHDAUX .NE. UNDEF ) THEN
                  BHDAUX = BHDAUX / SUMWT6(3)
                  IF ( BHD(ISEA) .EQ. UNDEF )  THEN
                    BHD(ISEA) = BHDAUX / REAL( SUMGRD )
                  ELSE
                    BHD(ISEA) = BHD(ISEA) + BHDAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( PHIOCAUX .NE. UNDEF ) THEN
                  PHIOCAUX = PHIOCAUX / SUMWT6(4)
                  IF ( PHIOC(ISEA) .EQ. UNDEF )  THEN
                    PHIOC(ISEA) = PHIOCAUX / REAL( SUMGRD )
                  ELSE
                    PHIOC(ISEA) = PHIOC(ISEA) + PHIOCAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( TUSXAUX .NE. UNDEF ) THEN
                  TUSXAUX = TUSXAUX / SUMWT6(5)
                  TUSYAUX = TUSYAUX / SUMWT6(5)
                  IF ( TUSX(ISEA) .EQ. UNDEF )  THEN
                    TUSX(ISEA) = TUSXAUX / REAL( SUMGRD )
                    TUSY(ISEA) = TUSYAUX / REAL( SUMGRD )
                  ELSE
                    TUSX(ISEA) = TUSX(ISEA) + TUSXAUX / REAL( SUMGRD )
                    TUSY(ISEA) = TUSY(ISEA) + TUSYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( USSXAUX .NE. UNDEF ) THEN
                  USSXAUX = USSXAUX / SUMWT6(6)
                  USSYAUX = USSYAUX / SUMWT6(6)
                  IF ( USSX(ISEA) .EQ. UNDEF )  THEN
                    USSX(ISEA) = USSXAUX / REAL( SUMGRD )
                    USSY(ISEA) = USSYAUX / REAL( SUMGRD )
                  ELSE
                    USSX(ISEA) = USSX(ISEA) + USSXAUX / REAL( SUMGRD )
                    USSY(ISEA) = USSY(ISEA) + USSYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( PRMSAUX .NE. UNDEF ) THEN
                  PRMSAUX = PRMSAUX / SUMWT6(7)
                  TPMSAUX = TPMSAUX / SUMWT6(7)
                  IF ( PRMS(ISEA) .EQ. UNDEF )  THEN
                    PRMS(ISEA) = PRMSAUX / REAL( SUMGRD )
                    TPMS(ISEA) = TPMSAUX / REAL( SUMGRD )
                  ELSE
                    PRMS(ISEA) = PRMS(ISEA) + PRMSAUX / REAL( SUMGRD )
                    TPMS(ISEA) = TPMS(ISEA) + TPMSAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( US3DF(1).GT.0 ) THEN
                  DO IK = US3DF(2),US3DF(3)
                    IF ( US3DAUX(IK) .NE. UNDEF ) THEN
                      IF ( US3D(ISEA,IK) .EQ. UNDEF ) US3D(ISEA,IK) = 0.
                      US3D(ISEA,IK) = US3D(ISEA,IK) +                  &
                              US3DAUX(IK) / REAL( SUMWT68(IK) * SUMGRD )
                    END IF
                    IF ( US3DAUX(NK+IK) .NE. UNDEF ) THEN
                      IF ( US3D(ISEA,NK+IK) .EQ. UNDEF )               &
                                                   US3D(ISEA,NK+IK) = 0.
                      US3D(ISEA,NK+IK) = US3D(ISEA,NK+IK) +            &
                        US3DAUX(NK+IK) / REAL( SUMWT68(NK+IK) * SUMGRD )
                    END IF
                  END DO
                END IF
!
                IF ( P2MSF(1).GT.0 ) THEN
                  DO IK = P2MSF(2),P2MSF(3)
                    IF ( P2SMSAUX(IK) .NE. UNDEF ) THEN
                      IF ( P2SMS(ISEA,IK).EQ.UNDEF ) P2SMS(ISEA,IK) = 0.
                      P2SMS(ISEA,IK) = P2SMS(ISEA,IK) +                &
                             P2SMSAUX(IK) / REAL( SUMWT69(IK) * SUMGRD )
                    END IF
                  END DO
                END IF
!
                IF ( TAUICEAUX(1) .NE. UNDEF ) THEN
                  IF ( TAUICE(ISEA,1) .EQ. UNDEF )   TAUICE(ISEA,1) = 0.
                  IF ( TAUICE(ISEA,2) .EQ. UNDEF )   TAUICE(ISEA,2) = 0.
                  TAUICE(ISEA,1) = TAUICE(ISEA,1) +                    &
                              TAUICEAUX(1) / REAL( SUMWT6(10) * SUMGRD )
                  TAUICE(ISEA,2) = TAUICE(ISEA,2) +                    &
                              TAUICEAUX(2) / REAL( SUMWT6(10) * SUMGRD )
                END IF
!
                IF ( PHICEAUX .NE. UNDEF ) THEN
                  IF ( PHICE(ISEA) .EQ. UNDEF )   PHICE(ISEA) = 0.
                  PHICE(ISEA) = PHICE(ISEA) +                          &
                                  PHICEAUX / REAL( SUMWT6(11) * SUMGRD )
                END IF
!
                IF ( USSPF(1).GT.0 ) THEN
                  DO IK = 1,USSPF(2)
                    IF ( USSPAUX(IK) .NE. UNDEF ) THEN
                      IF ( USSP(ISEA,IK) .EQ. UNDEF ) USSP(ISEA,IK) = 0.
                      USSP(ISEA,IK) = USSP(ISEA,IK) +                  &
                             USSPAUX(IK) / REAL( SUMWT612(IK) * SUMGRD )
                    END IF
                    IF ( USSPAUX(NK+IK) .NE. UNDEF ) THEN
                      IF ( USSP(ISEA,NK+IK) .EQ. UNDEF )               &
                                                   USSP(ISEA,NK+IK) = 0.
                      USSP(ISEA,NK+IK) = USSP(ISEA,NK+IK) +            &
                       USSPAUX(NK+IK) / REAL( SUMWT612(NK+IK) * SUMGRD )
                    END IF
                  END DO
                END IF
!
                IF ( TAUOCXAUX .NE. UNDEF ) THEN
                  TAUOCXAUX = TAUOCXAUX / SUMWT6(13)
                  TAUOCYAUX = TAUOCYAUX / SUMWT6(13)
                  IF ( TAUOCX(ISEA) .EQ. UNDEF )  THEN
                    TAUOCX(ISEA) = TAUOCXAUX / REAL( SUMGRD )
                    TAUOCY(ISEA) = TAUOCYAUX / REAL( SUMGRD )
                  ELSE
                    TAUOCX(ISEA) = TAUOCX(ISEA) + TAUOCXAUX / REAL( SUMGRD )
                    TAUOCY(ISEA) = TAUOCY(ISEA) + TAUOCYAUX / REAL( SUMGRD )
                  END IF
                END IF
!
! Group 7 variables
!
                IF ( ABAAUX .NE. UNDEF ) THEN
                  ABAAUX = ABAAUX / SUMWT7(1)
                  ABDAUX = ABDAUX / SUMWT7(1)
                  IF ( ABA(ISEA) .EQ. UNDEF )  THEN
                    ABA(ISEA) = ABAAUX / REAL( SUMGRD )
                    ABD(ISEA) = ABDAUX / REAL( SUMGRD )
                  ELSE
                    ABA(ISEA) = ABA(ISEA) + ABAAUX / REAL( SUMGRD )
                    ABD(ISEA) = ABD(ISEA) + ABDAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( UBAAUX .NE. UNDEF ) THEN
                  UBAAUX = UBAAUX / SUMWT7(2)
                  UBDAUX = UBDAUX / SUMWT7(2)
                  IF ( UBA(ISEA) .EQ. UNDEF )  THEN
                    UBA(ISEA) = UBAAUX / REAL( SUMGRD )
                    UBD(ISEA) = UBDAUX / REAL( SUMGRD )
                  ELSE
                    UBA(ISEA) = UBA(ISEA) + UBAAUX / REAL( SUMGRD )
                    UBD(ISEA) = UBD(ISEA) + UBDAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                DO IBED = 1,3
                  IF ( BEDFORMSAUX(IBED) .NE. UNDEF ) THEN
                    BEDFORMSAUX(IBED) = BEDFORMSAUX(IBED) / SUMWTB(IBED)
                    IF ( BEDFORMS(ISEA,IBED) .EQ. UNDEF )  THEN
                      BEDFORMS(ISEA,IBED) = BEDFORMSAUX(IBED) / REAL( SUMGRD )
                    ELSE
                      BEDFORMS(ISEA,IBED) = BEDFORMS(ISEA,IBED) +              &
                               BEDFORMSAUX(IBED) / REAL( SUMGRD )
                    END IF
                  END IF
                END DO
!
                IF ( PHIBBLAUX .NE. UNDEF ) THEN
                  PHIBBLAUX = PHIBBLAUX / SUMWT7(4)
                  IF ( PHIBBL(ISEA) .EQ. UNDEF )  THEN
                    PHIBBL(ISEA) = PHIBBLAUX / REAL( SUMGRD )
                  ELSE
                    PHIBBL(ISEA) = PHIBBL(ISEA) + PHIBBLAUX / REAL( SUMGRD )
                  END IF
                END IF
!
                IF ( TAUBBLAUX(1) .NE. UNDEF ) THEN
                  TAUBBLAUX(1) = TAUBBLAUX(1) / SUMWT7(5)
                  TAUBBLAUX(2) = TAUBBLAUX(2) / SUMWT7(5)
                  IF ( TAUBBL(ISEA,1) .EQ. UNDEF )  THEN
                    TAUBBL(ISEA,1) = TAUBBLAUX(1) / REAL( SUMGRD )
                    TAUBBL(ISEA,2) = TAUBBLAUX(2) / REAL( SUMGRD )
                  ELSE
                    TAUBBL(ISEA,1) = TAUBBL(ISEA,1) +                  &
                           TAUBBLAUX(1) / REAL( SUMGRD )
                    TAUBBL(ISEA,2) = TAUBBL(ISEA,2) +                  &
                           TAUBBLAUX(2) / REAL( SUMGRD )
                  END IF
                END IF
!
! Group 8 variables
!
                IF ( MSSXAUX .NE. UNDEF ) THEN
                  IF ( MSSX(ISEA) .EQ. UNDEF )   MSSX(ISEA) = 0.
                  MSSX(ISEA) = MSSX(ISEA) +                            &
                               MSSXAUX / REAL( SUMWT8(1)*SUMGRD )
                END IF
!
                IF ( MSSYAUX .NE. UNDEF ) THEN
                  IF ( MSSY(ISEA) .EQ. UNDEF )   MSSY(ISEA) = 0.
                  MSSY(ISEA) = MSSY(ISEA) +                            &
                               MSSYAUX / REAL( SUMWT8(1)*SUMGRD )
                END IF
!
                IF ( MSCXAUX .NE. UNDEF ) THEN
                  IF ( MSCX(ISEA) .EQ. UNDEF )   MSCX(ISEA) = 0.
                  MSCX(ISEA) = MSCX(ISEA) +                            &
                               MSCXAUX / REAL( SUMWT8(2)*SUMGRD )
                END IF
!
                IF ( MSCYAUX .NE. UNDEF ) THEN
                  IF ( MSCY(ISEA) .EQ. UNDEF )   MSCY(ISEA) = 0.
                  MSCY(ISEA) = MSCY(ISEA) +                            &
                               MSCYAUX / REAL( SUMWT8(2)*SUMGRD )
                END IF
!
                IF ( MSSDAUX1 .NE. UNDEF .AND. MSSDAUX2 .NE. UNDEF ) THEN
                  MSSDAUX1 = MSSDAUX1 / REAL( SUMWT8(3)*SUMGRD )
                  MSSDAUX2 = MSSDAUX2 / REAL( SUMWT8(3)*SUMGRD )
                  IF ( MSSD(ISEA) .NE. UNDEF ) THEN
                    MSSDAUX1 = MSSDAUX1 + COS ( MSSD(ISEA) )
                    MSSDAUX2 = MSSDAUX2 + SIN ( MSSD(ISEA) )
                  END IF
                  MSSD(ISEA) = ATAN2 ( MSSDAUX2, MSSDAUX1 )
                END IF
!
                IF ( MSCDAUX1 .NE. UNDEF .AND. MSCDAUX2 .NE. UNDEF ) THEN
                  MSCDAUX1 = MSCDAUX1 / REAL( SUMWT8(4)*SUMGRD )
                  MSCDAUX2 = MSCDAUX2 / REAL( SUMWT8(4)*SUMGRD )
                  IF ( MSCD(ISEA) .NE. UNDEF ) THEN
                    MSCDAUX1 = MSCDAUX1 + COS ( MSCD(ISEA) )
                    MSCDAUX2 = MSCDAUX2 + SIN ( MSCD(ISEA) )
                  END IF
                  MSCD(ISEA) = ATAN2 ( MSCDAUX2, MSCDAUX1 )
                END IF
!
                IF ( QPAUX .NE. UNDEF ) THEN
                  IF ( QP(ISEA) .EQ. UNDEF )   QP(ISEA) = 0.
                  QP(ISEA) = QP(ISEA) + QPAUX / REAL( SUMWT8(5)*SUMGRD )
                END IF
!
              END IF !/ ( USEGRID(IG) )
!
! End of Second loop
            END DO !/ IG = 1, GR_INTS
!
! Convert select variables back to polar notation. This is done because just
! prior to writing to file the w3iogo routine converts these variables 
! from polar to cartesian coordinates
!
            IF ( UA(ISEA) .NE. UNDEF ) THEN
              VAR1 = UA(ISEA)
              VAR2 = UD(ISEA)
              UA(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
              UD(ISEA) = ATAN2 ( VAR2, VAR1 )
            END IF 
!
            IF ( UST(ISEA) .NE. UNDEF ) THEN
              VAR1 = UST(ISEA)
              VAR2 = USTDIR(ISEA)
              UST(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
              USTDIR(ISEA) = ATAN2 ( VAR2, VAR1 )
            END IF
!
            IF ( ABA(ISEA) .NE. UNDEF ) THEN
              VAR1 = ABA(ISEA)
              VAR2 = ABD(ISEA)
              ABA(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
              ABD(ISEA) = ATAN2 ( VAR2, VAR1 )
            END IF
!
            IF ( UBA(ISEA) .NE. UNDEF ) THEN
              VAR1 = UBA(ISEA)
              VAR2 = UBD(ISEA)
              UBA(ISEA) = SQRT ( VAR1**2 + VAR2**2 )
              UBD(ISEA) = ATAN2 ( VAR2, VAR1 )
            END IF
!
          END IF
!
!/ End of main loop through output points
        END DO   !/ ISEA = 1, NSEA
!
!------------------------------------------------------------------------------
! 3. Write out interpolated data to target output file
!
        CALL W3IOGO('WRITE',FIDOUT(NG),IOTST,NG)
!
        RETURN
!
! Error escape locations
!
!/
!/ End of W3EXGI ------------------------------------------------------------/
!/
        END SUBROUTINE W3EXGI
!/
!/ End of W3GRID_INTERP -----------------------------------------------------/
!/
        END PROGRAM W3GRID_INTERP
