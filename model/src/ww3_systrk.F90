!/ ------------------------------------------------------------------- /
      PROGRAM WW3_SYSTRK
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |     A. J. van der Westhuysen      |
!/                  |            Jeff Hanson            |
!/                  |        Eve-Marie Devaliere        |
!/                  |                        FORTRAN 95 |
!/                  | Last update :         16-Jan-2017 |
!/                  +-----------------------------------+
!/
!/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
!/                  by Jeff Hanson & Eve-Marie Devaliere
!/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
!/    29-Nov-2013 : Remove DOC control characters,
!/                  update MPI! to MPI/! (H.L. Tolman). ( version 4.15 )
!/    11-Feb-2014 : Add NetCDF output option. Both NetCDF-3 and
!/                  NetCDF-4 are available. (B. Li).    ( version 4.18 )
!/    26-Sep-2016 : Optimization updates (A. van der Westhuysen)
!/                                                      ( version 5.15 )
!/    20-Sep-2016 : Add support for unformatted partition file.
!/                  (S.Zieger BoM, Australia)           ( version 5.16 )
!/    20-Dec-2016 : Optimized search algorithms and
!/                  set functions. (S.Zieger)           ( version 5.16 )
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
      USE W3STRKMD
      USE W3TIMEMD, ONLY: TDIFF
      IMPLICIT NONE
#ifdef W3_MPI

      INCLUDE "mpif.h"
#endif
!
!  1. Purpose :
!
!     Perform spatial and temporal tracking of wave systems, based 
!     on spectral partition (bulletin) output.
!
!  2. Method :
!
!     This is a controller program. It reads the input parameter file 
!     ww3_systrk.inp and calls subroutine waveTracking_NWS_V2 to 
!     perform the actual tracking procedure. Write output (fields and 
!     point output).
!
!  3. Parameters :
!
      LOGICAL      :: testout
      PARAMETER (testout = .FALSE.)
      CHARACTER    :: filename*80, paramFile*32
      REAL         :: dirKnob, perKnob, hsKnob, wetPts, seedLat, &
                      seedLon, dirTimeKnob, tpTimeKnob, tint
      REAL         :: lonout(100), latout(100)                            !Increase dimension?
      INTEGER      :: maxGroup, ntint, noutp
      INTEGER      :: CLKDT0(8),CLKDT1(8)
      REAL         :: CLKFEL
      TYPE(dat2d), POINTER :: wsdat(:)
      TYPE(timsys), POINTER :: sysA(:)
      INTEGER, POINTER :: maxSys(:)
!
!     Local parameters.
!     ----------------------------------------------------------------
!     intype         Int       input  Type of input (0 = from memory; 1 = from file)
!     tmax           Int       input  Value of maxTs to apply (1 or 2, used for model coupling)
!     tcur           Int       input  Index of current time step (1 or 2, used for model coupling)
!     ulimGroup      Int       input  Upper limit of number of wave systems to output
!
      LOGICAL           :: file_exists
      CHARACTER         :: inpstr*72
      INTEGER           :: intype, tmax, tcur, maxI, maxJ
      INTEGER           :: it, igrp, sysmatch, ind, ip
      INTEGER           :: i, j, leng, ulimGroup
      REAL, ALLOCATABLE :: dum(:,:)
#ifdef W3_TRKNC
      REAL, ALLOCATABLE :: dum2nc(:,:,:,:)
      REAL, ALLOCATABLE ::  hsprt_nc(:,:,:)
      REAL, ALLOCATABLE ::  tpprt_nc(:,:,:)
      REAL, ALLOCATABLE ::  dirprt_nc(:,:,:)
      REAL, ALLOCATABLE ::  longitude_nc(:),latitude_nc(:)
      REAL, ALLOCATABLE ::  lonprt_nc(:),latprt_nc(:)
#endif
      INTEGER NTIME_NC
      INTEGER           :: outputType
      LOGICAL           :: outputCheck1
      DOUBLE PRECISION  :: date1, date2, tstart, tend
      REAL              :: dlon, dlat, lonprt, latprt
      REAL              :: dt
      REAL              :: minlon, maxlon, minlat, maxlat
      INTEGER           :: mxcwt, mycwt
#ifdef W3_MPI
      INTEGER           :: rank, nproc, ierr
      CHARACTER         :: rankstr*4
#endif

!     For point output (bilinear interpolation)
      REAL :: hsprt(10),tpprt(10),dirprt(10)
      REAL :: BL_hsprt(10),BR_hsprt(10),TR_hsprt(10),TL_hsprt(10), &
              BL_tpprt(10),BR_tpprt(10),TR_tpprt(10),TL_tpprt(10), &
              BL_dirprt(10),BR_dirprt(10),TR_dirprt(10),TL_dirprt(10)
      REAL :: BL_dirx,BR_dirx,TR_dirx,TL_dirx, &
              BL_diry,BR_diry,TR_diry,TL_diry
      REAL :: BL_lonprt,BR_lonprt,TR_lonprt,TL_lonprt, &
              BL_latprt,BR_latprt,TR_latprt,TL_latprt
      REAL :: t, u, BL_W, BR_W, TR_W, TL_W
      REAL      :: PI
      PARAMETER  (PI = 3.1416) 
!
!  4. Subroutines used :
!
!     waveTracking_NWS_V2
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!  7. Remarks :
!
!  8. Structure :
!
!     Calls subroutine waveTracking_NWS_V2 in trackmd.95 - see that
!     file for structure.
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/MPI   Id.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
#ifdef W3_MPI
!     Start of parallel region
      CALL MPI_INIT(ierr)
    
      CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, nproc, ierr)

#endif
!     Open log file
#ifdef W3_MPI
      WRITE(rankstr,'(i4.4)')  rank
      OPEN(unit=20,file='sys_log'//rankstr//'.ww3',status='unknown')
#endif
#ifdef W3_SHRD
      OPEN(unit=20,file='sys_log.ww3',status='unknown')
#endif

!     Print code version
#ifdef W3_MPI
      IF (rank.EQ.0) THEN
#endif
         WRITE(6,900)
#ifdef W3_MPI
      END IF
#endif
      WRITE(20,900)
  900 FORMAT (/15X,'    *** WAVEWATCH III Wave system tracking ***  '/ &
               15X,'==============================================='/)

!     Since this program reads the raw partitioning input from file,
!     we set intype=1 or 2, and tmax and tcur to dummy values (not used).
      intype = 2
!      intype = 1
      IF (intype.EQ.1) WRITE(6,*) &
         '*** WARNING: partRes format input used!'
      tmax = 0
      tcur = 0

!     Read input parameter file
#ifdef W3_MPI
      IF (rank.EQ.0) THEN
#endif
      INQUIRE(FILE='ww3_systrk.inp', EXIST=file_exists)
      IF (.NOT.file_exists) THEN
         WRITE(20,2000)
         WRITE(6,2000)
         CALL ABORT
      END IF
      OPEN(unit=10,file='ww3_systrk.inp',status='old')

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) filename

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) date1, date2, dt, ntint
      tstart = date1 + date2/1000000

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) outputType

      !Check for correct outputType option:
      IF (outputType.EQ.1) THEN
         !ASCII output
      ELSEIF (outputType.EQ.3) THEN
         !NetCDF 3 - requrires !/TRKNC switch
         outputCheck1 = .TRUE.
#ifdef W3_TRKNC
     outputCheck1 = .FALSE.
#endif
         IF(outputCheck1)  THEN
            WRITE(6,993)
            STOP 
         END IF
      ELSEIF (outputType.EQ.4) THEN
         !NetCDF 4 - requrires !/TRKNC switch 
         outputCheck1 = .TRUE.
#ifdef W3_TRKNC
     outputCheck1 = .FALSE.
#endif
         IF(outputCheck1) THEN
            WRITE(6,994)
            STOP
         END IF
      ELSE
         !Not a valid outputType
         WRITE(6,995) outputType
         STOP
      ENDIF

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) minlon, maxlon, mxcwt

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) minlat, maxlat, mycwt

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) dirKnob, perKnob, hsKnob, wetPts, & 
                dirTimeKnob, tpTimeKnob

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) seedLat, seedLon

      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      noutp = 1
      lonout(:) = 9999.
      latout(:) = 9999.
      DO WHILE (.TRUE.)
         READ(10,*) lonout(noutp),latout(noutp)
         IF ((lonout(noutp).EQ.0.).AND.(latout(noutp).EQ.0.)) EXIT
         noutp = noutp + 1
      END DO
      noutp = noutp - 1 

      CLOSE(10)

      WRITE(20,*) 'Raw partition file = ',filename
      WRITE(20,'(A,F15.6)') 'Start time = ',tstart
      WRITE(20,*) 'dt = ',dt
      WRITE(20,*) 'No. time levels = ',ntint
      WRITE(20,'(A,2F7.2)') 'Domain limits: Longitude =',minlon, maxlon
      WRITE(20,'(A,2F7.2)') '               Latitude  =',minlat, maxlat
      WRITE(20,*) 'No. increments: Long, Lat  =',mxcwt, mycwt
      WRITE(20,*) 'dirKnob, perKnob, hsKnob, wetPts, & 
                dirTimeKnob, tpTimeKnob, seedLat, seedLon ='
      WRITE(20,'(8F6.2)') dirKnob, perKnob, hsKnob, wetPts, & 
                dirTimeKnob, tpTimeKnob, seedLat, seedLon
      WRITE(20,*) 'No. output points =',noutp
      DO i = 1,noutp
         WRITE(20,*) lonout(i), latout(i)
      END DO

      INQUIRE(FILE=filename, EXIST=file_exists)
      IF (.NOT.file_exists) THEN
         WRITE(20,2200) filename
         WRITE(6,2200) filename
         CALL EXIT(1)
      END IF


      CALL DATE_AND_TIME ( VALUES=CLKDT0 )

#ifdef W3_MPI
      END IF
#endif

#ifdef W3_MPI
!     MPI communication block 
      CALL MPI_BCAST(filename,80,MPI_CHARACTER,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(tstart,1,MPI_DOUBLE_PRECISION,0, &
                     MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(tend,1,MPI_DOUBLE_PRECISION,0, &
                     MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(dt,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(ntint,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(minlon,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(maxlon,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(minlat,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(maxlat,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(mxcwt,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(mycwt,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(dirKnob,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(perKnob,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(hsKnob,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(wetPts,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(dirTimeKnob,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(tpTimeKnob,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(seedLon,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(seedLat,1,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(noutp,1,MPI_INTEGER,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(lonout,100,MPI_REAL,0,MPI_COMM_WORLD,IERR)
      CALL MPI_BCAST(latout,100,MPI_REAL,0,MPI_COMM_WORLD,IERR)
#endif

#ifdef W3_MPI
      CALL MPI_Barrier(MPI_COMM_WORLD,IERR)
#endif

      CALL waveTracking_NWS_V2 (intype     ,tmax       , &
                                tcur       ,filename   , &
                                tstart     ,tend       , &
                                dt         ,ntint      , &
                                minlon     ,maxlon     , & 
                                minlat     ,maxlat     , &
                                mxcwt      ,mycwt      , &
                                dirKnob    ,             &
                                perKnob    ,hsKnob     , &
                                wetPts     ,seedLat    , &
                                seedLon    ,dirTimeKnob, &
                                tpTimeKnob ,paramFile  , &
                                sysA       ,wsdat      , &
                                maxSys     ,maxGroup   )

#ifdef W3_MPI
      IF (rank.EQ.0) THEN
#endif

         CALL DATE_AND_TIME ( VALUES=CLKDT1 )
         CLKFEL = TDIFF ( CLKDT0,CLKDT1 )
         WRITE (6,998) CLKFEL
         WRITE (6,*) 'Final system output...'

!     Set upper limit for wave systems to output (limited by AWIPS display)
      ulimGroup = 9

!-----Output systems as plain text----------------------------------------

      maxI = SIZE(wsdat(1)%lon,1)
      maxJ = SIZE(wsdat(1)%lon,2)
      dlon = wsdat(1)%lon(2,2)-wsdat(1)%lon(1,1)
      dlat = wsdat(1)%lat(2,2)-wsdat(1)%lat(1,1)
      WRITE(20,*) 'dlon, dlat =',dlon,dlat

!-----Final SYSTEM output: Coordinates
      OPEN(unit=21,file='sys_coord.ww3', status='unknown')

      WRITE(21,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(21,'(I6,69X,A)') maxI,'Number of cols'
#ifdef W3_TRKNC
      ALLOCATE( longitude_nc(maxI) )
      ALLOCATE( latitude_nc(maxJ) )
#endif

      WRITE(21,*) 'Longitude ='
      DO j = maxJ,1,-1
         DO i = 1,maxI
            WRITE(21,'(F7.2)',ADVANCE='NO') wsdat(1)%lon(i,j)
#ifdef W3_TRKNC
        longitude_nc(i)=wsdat(1)%lon(i,1)
#endif
         END DO
         WRITE(21,'(A)',ADVANCE='YES') ''
      END DO

      WRITE(21,*) 'Latitude = '
      DO j = maxJ,1,-1
         DO i = 1,maxI
            WRITE(21,'(F7.2)',ADVANCE='NO') wsdat(1)%lat(i,j)
#ifdef W3_TRKNC
        latitude_nc(j)=wsdat(1)%lat(1,j)
#endif
         END DO
         WRITE(21,'(A)',ADVANCE='YES') ''
      END DO

      CLOSE(21)

!-----Final SYSTEM output: hs
      IF(outputType == 1) THEN
      OPEN(unit=22,file='sys_hs.ww3', status='unknown')

      WRITE(22,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(22,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF

      NTIME_NC=SIZE(sysA)
      ALLOCATE( dum(maxI,maxJ) )
#ifdef W3_TRKNC
      IF(outputType == 3 .OR. outputType == 4) THEN
      ALLOCATE( dum2nc(maxI,maxJ,maxGroup,NTIME_NC) )
      ENDIF
#endif

      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to ulimGroup    
         IF(outputType == 1) THEN
            WRITE(22,'(F15.6,60x,A)') wsdat(it)%date,'Time'
            WRITE(22,'(I6,69x,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF 
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = &
                      sysA(it)%sys(sysmatch)%hs(ind)
               END DO
            ELSE
               leng = 0
            END IF
            
      IF(outputType == 1) THEN
            WRITE(22,'(I6,69x,A)') igrp,'System number'  
            WRITE(22,'(I6,69x,A)') leng,'Number of points in system'

            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(22,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(22,'(A)',ADVANCE='YES') ''
            END DO
      ELSE
#ifdef W3_TRKNC
            DO J = maxJ,1,-1
               DO i = 1,maxI
               dum2nc(i,j,igrp,it)=dum(i,j)
               END DO
            END DO
#endif
      ENDIF

         END DO
      END DO
        
#ifdef W3_TRKNC
      IF(outputType == 3 .OR. outputType == 4 ) THEN
      call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
         maxGroup,date1,date2,dt,NTIME_NC,1,outputType)
      ENDIF
#endif

      IF(outputType.EQ.1) CLOSE(22)

!-----Final SYSTEM output: tp
      IF(outputType == 1) THEN
      OPEN(unit=23,file='sys_tp.ww3',status='unknown')

      WRITE(23,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(23,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF

      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to ulimGroup
         IF(outputType == 1) THEN
            WRITE(23,'(F15.6,60x,A)') wsdat(it)%date,'Time'
            WRITE(23,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF 
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = &
                      sysA(it)%sys(sysmatch)%tp(ind)
               END DO
            ELSE
               leng = 0
            END IF
            
      IF(outputType == 1) THEN
            WRITE(23,'(I6,69X,A)') igrp,'System number'  
            WRITE(23,'(I6,69X,A)') leng,'Number of points in system'
            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(23,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(23,'(A)',ADVANCE='YES') ''
            END DO
      ELSE

#ifdef W3_TRKNC
            DO J = maxJ,1,-1
               DO i = 1,maxI
               dum2nc(i,j,igrp,it)=dum(i,j)
            END DO
            END DO
#endif
      ENDIF

         END DO
      END DO

#ifdef W3_TRKNC
      IF(outputType.EQ.3 .OR. outputType.EQ. 4 ) THEN
      call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
         maxGroup,date1,date2,dt,NTIME_NC,2,outputType)
      ENDIF
#endif

      IF(outputType.EQ.1) CLOSE(23)

!-----Final SYSTEM output: dir
      IF(outputType == 1) THEN
      OPEN(unit=24,file='sys_dir.ww3',status='unknown')

      WRITE(24,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(24,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF

      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to
!        ulimGroup
         IF(outputType == 1) THEN
             WRITE(24,'(F15.6,60x,A)') wsdat(it)%date,'Time'
             WRITE(24,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF 
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = & 
                      sysA(it)%sys(sysmatch)%dir(ind)
               END DO
            ELSE
               leng = 0
            END IF
            
      IF(outputType == 1) THEN
            WRITE(24,'(I6,69X,A)') igrp,'System number'  
            WRITE(24,'(I6,69X,A)') leng,'Number of points in system'
            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(24,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(24,'(A)',ADVANCE='YES') ''
            END DO
      ELSE
#ifdef W3_TRKNC
            DO J = maxJ,1,-1
               DO i = 1,maxI
               dum2nc(i,j,igrp,it)=dum(i,j)
               END DO
            END DO
#endif
      END IF

         END DO
      END DO

#ifdef W3_TRKNC
      IF(outputType.EQ.3 .OR. outputType.EQ.4 ) THEN
      call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
         maxGroup,date1,date2,dt,NTIME_NC,3,outputType)
      ENDIF
#endif
      IF(outputType.EQ.1) CLOSE(24)

!-----Final SYSTEM output: dspr
      IF(outputType == 1) THEN
      OPEN(unit=25,file='sys_dspr.ww3',status='unknown')

      WRITE(25,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(25,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF

      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to ulimGroup
         IF(outputType == 1) THEN
            WRITE(25,'(F15.6,60x,A)') wsdat(it)%date,'Time'
            WRITE(25,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = & 
                      sysA(it)%sys(sysmatch)%dspr(ind)
               END DO
            ELSE
               leng = 0
            END IF
            
      IF(outputType == 1) THEN
            WRITE(25,'(I6,69X,A)') igrp,'System number'  
            WRITE(25,'(I6,69X,A)') leng,'Number of points in system'
            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(25,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(25,'(A)',ADVANCE='YES') ''
            END DO
       ELSE
#ifdef W3_TRKNC
            DO J = maxJ,1,-1
               DO i = 1,maxI
               dum2nc(i,j,igrp,it)=dum(i,j)
               END DO
            END DO
#endif
       ENDIF

         END DO
      END DO

#ifdef W3_TRKNC
      IF(outputType.EQ.3 .OR. outputType.EQ.4 ) THEN
      call t2netcdf(longitude_nc,latitude_nc,dum2nc,maxI,maxJ,&
         maxGroup,date1,date2,dt,NTIME_NC,4,outputType)
      ENDIF
#endif
      IF(outputType.EQ.1) CLOSE(25)

      IF (ALLOCATED(DUM)) DEALLOCATE(dum)
#ifdef W3_TRKNC
      IF (ALLOCATED(dum2nc)) DEALLOCATE(dum2nc)
#endif

#ifdef W3_TRKNC
      IF(outputType.EQ.3.OR.outputType.EQ.4) THEN
      ALLOCATE( hsprt_nc(10,noutp,NTIME_NC) )
      ALLOCATE( tpprt_nc(10,noutp,NTIME_NC) )
      ALLOCATE( dirprt_nc(10,noutp,NTIME_NC) )
      ALLOCATE( lonprt_nc(noutp) )
      ALLOCATE( latprt_nc(noutp) )
      ENDIF
#endif

!-----Final SYSTEM output: point output
      IF(outputType == 1) THEN
      OPEN(unit=26,file='sys_pnt.ww3',status='unknown')
      WRITE(26,'(A)') '%'
      WRITE(26,'(A)') '%'
      WRITE(26,'(A)') '% WW3 Wave tracking point output'
      WRITE(26,'(A)') '%'
      WRITE(26,'(10A)') '%       Xp            Yp            ', &
           'HsSY01        HsSY02        HsSY03        HsSY04        ', &
           'HsSY05        HsSY06        HsSY07        HsSY08        ', &
           'HsSY09        HsSY10        ', &
           'TpSY01        TpSY02        TpSY03        TpSY04        ', &
           'TpSY05        TpSY06        TpSY07        TpSY08        ', &
           'TpSY09        TpSY10        ', &
           'DrSY01        DrSY02        DrSY03        DrSY04        ', &
           'DrSY05        DrSY06        DrSY07        DrSY08        ', &
           'DrSY09        DrSY10'   
      WRITE(26,'(10A)') '%       [degr]        [degr]        ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]'
      WRITE(26,'(A)') '%'
       ENDIF

      DO it = 1,SIZE(sysA)
      IF(outputType == 1) THEN
         WRITE(26,'(A,F15.6)') 'Time : ',wsdat(it)%date
       ENDIF

         DO ip = 1,noutp
            hsprt(1:10) = 999.9999
            tpprt(1:10) = 999.9999
            dirprt(1:10) = 999.9999
            lonprt = 999.9999
            latprt = 999.9999
            BL_hsprt(1:10) = 999.9999
            BL_tpprt(1:10) = 999.9999
            BL_dirprt(1:10) = 999.9999
            BR_hsprt(1:10) = 999.9999
            BR_tpprt(1:10) = 999.9999
            BR_dirprt(1:10) = 999.9999
            TL_hsprt(1:10) = 999.9999
            TL_tpprt(1:10) = 999.9999
            TL_dirprt(1:10) = 999.9999
            TR_hsprt(1:10) = 999.9999
            TR_tpprt(1:10) = 999.9999
            TR_dirprt(1:10) = 999.9999
            BL_lonprt = 999.9999
            BL_latprt = 999.9999
            BR_lonprt = 999.9999
            BR_latprt = 999.9999
            TL_lonprt = 999.9999
            TL_latprt = 999.9999
            TR_lonprt = 999.9999
            TR_latprt = 999.9999
            BL_W = 999
            BR_W = 999
            TR_W = 999
            TL_W = 999

            DO j = 1, (maxJ-1)
               DO i = 1, (maxI-1)
                  IF ( ( ((lonout(ip).GE. &
                           wsdat(1)%lon(i,j)).AND. &
                          (lonout(ip).LT. &
                           wsdat(1)%lon(i+1,j))).OR. &
                         ((lonout(ip).GT. &
                          wsdat(1)%lon(i,j)).AND. &
                         (lonout(ip).LE. &
                          wsdat(1)%lon(i+1,j))) ).AND. &
                       ( ((latout(ip).GE. &
                          wsdat(1)%lat(i,j)).AND. &
                          (latout(ip).LT. &
                          wsdat(1)%lat(i,j+1))).OR. &
                         ((latout(ip).GT. &
                          wsdat(1)%lat(i,j)).AND. &
                          (latout(ip).LE. &
                          wsdat(1)%lat(i,j+1))) ) ) &
                  THEN
                     BL_lonprt = wsdat(1)%lon(i,j)
                     BL_latprt = wsdat(1)%lat(i,j)
                     BR_lonprt = wsdat(1)%lon(i+1,j)
                     BR_latprt = wsdat(1)%lat(i+1,j)
                     TL_lonprt = wsdat(1)%lon(i,j+1)
                     TL_latprt = wsdat(1)%lat(i,j+1)
                     TR_lonprt = wsdat(1)%lon(i+1,j+1)
                     TR_latprt = wsdat(1)%lat(i+1,j+1)
!                    Compute weights for this point
                     t = (lonout(ip)-BL_lonprt)/(BR_lonprt-BL_lonprt)
                     u = (latout(ip)-BL_latprt)/(TL_latprt-BL_latprt)
                     BL_W = (1-t)*(1-u)
                     BR_W = t*(1-u)
                     TR_W = t*u
                     TL_W = (1-t)*u
!                    Compute output values using weights
                     lonprt = BL_W*BL_lonprt + BR_W*BR_lonprt + &
                              TL_W*TL_lonprt + TR_W*TR_lonprt
                     latprt = BL_W*BL_latprt + BR_W*BR_latprt + &
                              TL_W*TL_latprt + TR_W*TR_latprt
                  END IF
               END DO
            END DO
!           Loop through identified groups, limiting the output in file to 10
            DO igrp = 1,MIN(10,maxGroup)
!              Find system with this group tag
               sysmatch = 1
               DO WHILE (sysmatch.LE.maxSys(it))
                  IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
                  sysmatch = sysmatch+1
               END DO
               IF (sysmatch.LE.maxSys(it)) THEN
!                Match found: fill the output matrix with this data
                 leng = sysA(it)%sys(sysmatch)%nPoints
                 DO ind = 1, leng
!                  Write output point data with bilinear interpolation
                   IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                         BL_lonprt).AND.&
                        (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                         BL_latprt) ) THEN
                      BL_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      BL_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      BL_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                              BR_lonprt).AND.&
                             (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                              BR_latprt)) THEN
                      BR_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      BR_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      BR_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                              TL_lonprt).AND.&
                             (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                              TL_latprt)) THEN
                      TL_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      TL_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      TL_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                              TR_lonprt).AND.&
                             (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                              TR_latprt)) THEN
                      TR_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      TR_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      TR_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   END IF
                 END DO
!                  Compute output value using weights
!                  (only if output point is surrounded by valid points)
                   IF ( (BL_hsprt(igrp).NE.999.9999).AND. &
                        (BR_hsprt(igrp).NE.999.9999).AND. &
                        (TL_hsprt(igrp).NE.999.9999).AND. &
                        (TR_hsprt(igrp).NE.999.9999) ) THEN
                      hsprt(igrp) = BL_W * BL_hsprt(igrp) + &
                                    BR_W * BR_hsprt(igrp) + &
                                    TL_W * TL_hsprt(igrp) + &
                                    TR_W * TR_hsprt(igrp)
                      tpprt(igrp) = BL_W * BL_tpprt(igrp) + &
                                    BR_W * BR_tpprt(igrp) + &
                                    TL_W * TL_tpprt(igrp) + &
                                    TR_W * TR_tpprt(igrp)
                      BL_dirx = COS((270-BL_dirprt(igrp))*PI/180.)
                      BR_dirx = COS((270-BR_dirprt(igrp))*PI/180.)
                      TR_dirx = COS((270-TR_dirprt(igrp))*PI/180.)
                      TL_dirx = COS((270-TL_dirprt(igrp))*PI/180.)
                      BL_diry = SIN((270-BL_dirprt(igrp))*PI/180.)
                      BR_diry = SIN((270-BR_dirprt(igrp))*PI/180.)
                      TR_diry = SIN((270-TR_dirprt(igrp))*PI/180.)
                      TL_diry = SIN((270-TL_dirprt(igrp))*PI/180.)
                      dirprt(igrp)=270 - 180./PI* &
                         ATAN2(BL_W*BL_diry+BR_W*BR_diry+ &
                               TL_W*TL_diry+TR_W*TR_diry, &
                               BL_W*BL_dirx+BR_W*BR_dirx+ &
                               TL_W*TL_dirx+TR_W*TR_dirx)
                      IF (dirprt(igrp).GT.360.) THEN
                         dirprt(igrp) = dirprt(igrp) - 360.
                      END IF
                   ELSE
                      hsprt(igrp) = 999.9999
                      tpprt(igrp) = 999.9999
                      dirprt(igrp) = 999.9999
                   END IF
               END IF  
            END DO
      IF(outputType == 1) THEN
            WRITE(26,'(32F14.4)') lonprt,latprt, &
                       hsprt(1:10),tpprt(1:10),dirprt(1:10)  
        ENDIF
#ifdef W3_TRKNC
      IF(outputType.EQ.3.OR.outputType.EQ.4) THEN
        lonprt_nc(ip)=lonprt
        latprt_nc(ip)=latprt
        do igrp=1,10
        hsprt_nc(igrp,ip,it)=hsprt(igrp)
        tpprt_nc(igrp,ip,it)=tpprt(igrp)
        dirprt_nc(igrp,ip,it)=dirprt(igrp)
        enddo
      ENDIF
#endif

         END DO
      END DO
#ifdef W3_TRKNC
      IF(outputType.EQ.3.OR.outputType.EQ.4) THEN
      call pt2netcdf(lonprt_nc,latprt_nc,hsprt_nc,tpprt_nc, &
       dirprt_nc,noutp,date1,date2,dt,NTIME_NC,outputType)
      ENDIF
#endif

      IF(outputType.EQ.1) CLOSE(26)

!-----Final SYSTEM output: point output (Nearest neighbor, as a double check)
      IF (testout) THEN
      OPEN(unit=28,file='sys_pnt_nn.ww3',status='unknown')
      WRITE(28,'(A)') '%'
      WRITE(28,'(A)') '%'
      WRITE(28,'(A)') '% WW3 Wave tracking point output'
      WRITE(28,'(A)') '%'
      WRITE(28,'(10A)') '%       Xp            Yp            ', &
           'HsSY01        HsSY02        HsSY03        HsSY04        ', &
           'HsSY05        HsSY06        HsSY07        HsSY08        ', &
           'HsSY09        HsSY10        ', &
           'TpSY01        TpSY02        TpSY03        TpSY04        ', &
           'TpSY05        TpSY06        TpSY07        TpSY08        ', &
           'TpSY09        TpSY10        ', &
           'DrSY01        DrSY02        DrSY03        DrSY04        ', &
           'DrSY05        DrSY06        DrSY07        DrSY08        ', &
           'DrSY09        DrSY10'   
      WRITE(28,'(10A)') '%       [degr]        [degr]        ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]'
      WRITE(28,'(A)') '%'

      DO it = 1,SIZE(sysA)
         WRITE(28,'(A,F15.6)') 'Time : ',wsdat(it)%date

         DO ip = 1,noutp
            hsprt(1:10) = 999.9999
            tpprt(1:10) = 999.9999
            dirprt(1:10) = 999.9999
            lonprt = 999.9999
            latprt = 999.9999

            DO j = 1, maxJ
               DO i = 1, maxI
!                 Write nearest nearbor output (no bilinear interpolation)
                  IF ( (lonout(ip).GE. &
                        (wsdat(1)%lon(i,j)-dlon/2)).AND. &
                        (lonout(ip).LT. &
                        (wsdat(1)%lon(i,j)+dlon/2)).AND. &
                        (latout(ip).GE. &
                        (wsdat(1)%lat(i,j)-dlat/2)).AND. &
                        (latout(ip).LT. &
                        (wsdat(1)%lat(i,j)+dlat/2)) ) &
                  THEN
                         lonprt = wsdat(1)%lon(i,j)
                         latprt = wsdat(1)%lat(i,j)
                  END IF
               END DO
            END DO
!           Loop through identified groups, limiting the output in file to 10
            DO igrp = 1,MIN(10,maxGroup)
!              Find system with this group tag
               sysmatch = 1
               DO WHILE (sysmatch.LE.maxSys(it))
                  IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
                  sysmatch = sysmatch+1
               END DO
               IF (sysmatch.LE.maxSys(it)) THEN
!                Match found: fill the output matrix with this data
                 leng = sysA(it)%sys(sysmatch)%nPoints
                 DO ind = 1, leng
!                  Write nearest nearbor output (no bilinear interpolation)
                   IF ( (lonout(ip).GE. &
                        (sysA(it)%sys(sysmatch)%lon(ind)-dlon/2)).AND. &
                        (lonout(ip).LT. &
                        (sysA(it)%sys(sysmatch)%lon(ind)+dlon/2)).AND. &
                        (latout(ip).GE. &
                        (sysA(it)%sys(sysmatch)%lat(ind)-dlat/2)).AND. &
                        (latout(ip).LT. &
                        (sysA(it)%sys(sysmatch)%lat(ind)+dlat/2)) ) &
                      THEN
                         hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                         tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                         dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   END IF
                 END DO
               END IF  
            END DO
            WRITE(28,'(32F14.4)') lonprt,latprt, &
                       hsprt(1:10),tpprt(1:10),dirprt(1:10)  
         END DO
      END DO

      CLOSE(28)
      END IF

!-------------------------------------------------------------------------

      WRITE(20,*) 'In ww3_systrk: Deallocating wsdat ...'
      DO it=1,size(wsdat)
        IF (ASSOCIATED(wsdat(it)%lat)) DEALLOCATE(wsdat(it)%lat)
        IF (ASSOCIATED(wsdat(it)%lon)) DEALLOCATE(wsdat(it)%lon)
        IF (ASSOCIATED(wsdat(it)%par)) DEALLOCATE(wsdat(it)%par)
        IF (ASSOCIATED(wsdat(it)%wnd)) DEALLOCATE(wsdat(it)%wnd)
      END DO
      IF (ASSOCIATED(wsdat)) DEALLOCATE(wsdat)
      WRITE(20,*) '               Deallocating sysA ...'
      DO i=1,size(sysA)
       DO j=1,size(sysA(i)%sys)
         IF (ASSOCIATED(sysA(i)%sys(j)%i)) DEALLOCATE(sysA(i)%sys(j)%i)
         IF (ASSOCIATED(sysA(i)%sys(j)%j)) DEALLOCATE(sysA(i)%sys(j)%j)
         IF (ASSOCIATED(sysA(i)%sys(j)%lon)) &
                                         DEALLOCATE(sysA(i)%sys(j)%lon)
         IF (ASSOCIATED(sysA(i)%sys(j)%lat)) &
                                         DEALLOCATE(sysA(i)%sys(j)%lat)
         IF (ASSOCIATED(sysA(i)%sys(j)%hs))  &
                                          DEALLOCATE(sysA(i)%sys(j)%hs)
         IF (ASSOCIATED(sysA(i)%sys(j)%tp))  &
                                          DEALLOCATE(sysA(i)%sys(j)%tp)
         IF (ASSOCIATED(sysA(i)%sys(j)%dir)) &
                                         DEALLOCATE(sysA(i)%sys(j)%dir)
         IF (ASSOCIATED(sysA(i)%sys(j)%dspr)) &
                                        DEALLOCATE(sysA(i)%sys(j)%dspr)
       END DO
      END DO
      IF (ASSOCIATED(sysA)) DEALLOCATE(sysA)
      WRITE(20,*) '               Deallocating maxSys ...'
      IF (ASSOCIATED(maxSys)) DEALLOCATE(maxSys)
      CLOSE(20)

      WRITE(6,*) '... ww3_systrk completed successfully.'

      WRITE(6,999)

#ifdef W3_MPI
      END IF   !/IF (rank.EQ.0)
#endif

#ifdef W3_MPI
      CALL MPI_FINALIZE(IERR)
!     End of parallel region
#endif

  998 FORMAT ( ' ... finished. Elapsed time : ',F10.2,' s')
  993 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK : '/           &
               '     OutputType=3 needs TRKNC switch ')
  994 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK : '/           &
               '     OutputType=4 needs TRKNC switch ')
  995 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK : '/           &
               '     OutputType,',I3,'not valid. Options: 1,3,4')

  999 FORMAT (/15X,'End of program '/ &
               15X,'==============================================='/ &
               15X,'     *** WAVEWATCH III Wave system tracking ***  ')

 2000 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/    &
               '     ERROR IN OPENING INPUT FILE')
 2200 FORMAT (/' *** WAVEWATCH III ERROR IN W3SYSTRK : '/    &
               '     ERROR IN OPENING PARTITION FILE : ',A)

      END PROGRAM WW3_SYSTRK
!
#ifdef W3_TRKNC
      subroutine t2netcdf(lons,lats,data_in,nlons,nlats,nsys,date1,date2,&
         dt,ntime,ivar, outputType)
      USE W3TIMEMD
      use netcdf
      implicit none
      character (len = 15) :: file_name
      integer, parameter :: ndims = 4
      integer, parameter :: deflate = 1
      integer :: outputType, ncid, oldMode
      integer :: nlons,nlats,nsys,rec,ntime,ivar
      double precision :: date1,date2,timenc
      real :: data_in(nlons, nlats, nsys,ntime)
      real :: lats(nlats), lons(nlons),dt
      double precision :: times(ntime)
      integer :: iyc,imc,idc,ihc,iminc,isc,Jday,Jday0
      integer :: iret
#endif
!
#ifdef W3_TRKNC
      integer :: lon_varid, lat_varid, rec_varid
      character (len = *), parameter :: lsys_name = "system_index"
      character (len = *), parameter :: lat_name = "latitude"
      character (len = *), parameter :: lon_name = "longitude"
      character (len = *), parameter :: time_name = "time"
      integer :: sys_dimid, lon_dimid, lat_dimid, rec_dimid
      integer :: start(ndims), count(ndims)
#endif
!
#ifdef W3_TRKNC
      character (len = *), parameter :: var1_name="hs"
      character (len = *), parameter :: var2_name="tp"
      character (len = *), parameter :: var3_name="dir"
      character (len = *), parameter :: var4_name="dspr"
      integer :: var1_varid, var2_varid, var3_varid,var4_varid
      integer :: dimids(ndims)
#endif
!
#ifdef W3_TRKNC
      character (len = *), parameter :: units = "units"
      character (len = *), parameter :: var1_units = "m"
      character (len = *), parameter :: var2_units = "s"
      character (len = *), parameter :: var3_units = "degrees"
      character (len = *), parameter :: var4_units = "degrees"
      character (len = *), parameter :: lat_units = "degrees_north"
      character (len = *), parameter :: lon_units = "degrees_east"
      iyc=date1/10000
      imc=(date1-iyc*10000)/100
      idc=int(date1-DBLE(iyc*10000)-DBLE(imc*100))
      ihc=date2/10000
      iminc=(date2-ihc*10000)/100
      isc=date2-ihc*10000-100*iminc
      timenc=DBLE(julday(idc,imc,iyc))+(DBLE(ihc)+(DBLE(iminc)+ &
         (DBLE(isc)/60.0D0))/60.0D0)/24.0D0
      Jday0=julday(1,1,1990)
      timenc=timenc-Jday0
      do rec=1,ntime
       times(rec)=timenc+DBLE( (rec-1)*dt)/3600.0D0/24.0D0 
      enddo
      if( ivar == 1) then
       file_name = "sys_hs.ww3.nc"
      else if( ivar == 2) then
       file_name = "sys_tp.ww3.nc"
      else if( ivar == 3) then
       file_name = "sys_dir.ww3.nc"
      else
      file_name = "sys_dspr.ww3.nc"
      endif
#endif
!
#ifdef W3_TRKNC
! create the netcdf file. 
      if (outputType.EQ.3) then
      call check( nf90_create(file_name, NF90_CLOBBER, ncid) )
      endif
      if(outputType.EQ.4) call check( nf90_create(file_name,NF90_NETCDF4,ncid))
      call check ( nf90_set_fill(ncid,nf90_nofill,oldMode) )
      call check( nf90_def_dim(ncid, lsys_name, nsys, sys_dimid) )
      call check( nf90_def_dim(ncid, lat_name, nlats, lat_dimid) )
      call check( nf90_def_dim(ncid, lon_name, nlons, lon_dimid) )
      call check( nf90_def_dim(ncid, time_name, ntime, rec_dimid) )
      call check( nf90_def_var(ncid, lat_name, NF90_REAL, lat_dimid,lat_varid))
      if(outputType.EQ.4) call check( nf90_def_var_deflate(ncid,lat_varid,1,1,deflate) )
      call check( nf90_def_var(ncid, lon_name, NF90_REAL, lon_dimid,lon_varid))
      if(outputType.EQ.4) call check( nf90_def_var_deflate(ncid,lon_varid,1,1,deflate) )
      call check( nf90_def_var(ncid,time_name,NF90_DOUBLE,rec_dimid,rec_varid))
      if(outputType.EQ.4) call check( nf90_def_var_deflate(ncid,rec_varid,1,1,deflate) )
#endif
!
#ifdef W3_TRKNC
      call check( nf90_put_att(ncid, lat_varid, units, lat_units) )
      call check( nf90_put_att(ncid, lat_varid, 'long_name', 'latitude') )
      call check( nf90_put_att(ncid, lat_varid, 'standard_name', 'latitude') )
      call check( nf90_put_att(ncid, lat_varid, 'axis','Y'))
      call check( nf90_put_att(ncid, lon_varid, units, lon_units) )
      call check( nf90_put_att(ncid, lon_varid, 'long_name', 'longitude') )
      call check( nf90_put_att(ncid, lon_varid, 'standard_name', 'longitude') )
      call check( nf90_put_att(ncid, lon_varid, 'axis','X'))
      call check(nf90_put_att(ncid,rec_varid,units,& 
                  'days since 1990-01-01 00:00:00'))
      call check(nf90_put_att(ncid,rec_varid,'long_name','julian day (UT)'))
      call check( nf90_put_att(ncid, rec_varid,'standard_name', 'time') )
      call check( nf90_put_att(ncid, rec_varid, 'conventions',&
      'relative julian day with decimal part (as part of the day)' ) )
      call check( nf90_put_att(ncid, rec_varid, 'axis','T'))
#endif
!
#ifdef W3_TRKNC
      dimids = (/ lon_dimid, lat_dimid, sys_dimid, rec_dimid /)
      if( ivar == 1) then
      call check( nf90_def_var(ncid, var1_name, NF90_REAL, dimids,var1_varid) )
      if(outputType.EQ.4) call check( nf90_def_var_deflate(ncid,var1_varid,1,1,deflate) )
      call check( nf90_put_att(ncid, var1_varid, units, var1_units) )
      call check( nf90_put_att(ncid, var1_varid,'long_name','significant_wave_height') )
      call check( nf90_put_att(ncid, var1_varid,'missing_value','9999.00'))
       else if( ivar == 2) then
      call check( nf90_def_var(ncid, var2_name, NF90_REAL, dimids, var2_varid) )
      if(outputType.EQ.4) call check( nf90_def_var_deflate(ncid,var2_varid,1,1,deflate) )
      call check( nf90_put_att(ncid, var2_varid, units, var2_units) )
      call check( nf90_put_att(ncid, var2_varid,'long_name','peak_period') )
      call check( nf90_put_att(ncid, var2_varid,'missing_value','9999.00') )
       else if ( ivar ==3 ) then
      call check( nf90_def_var(ncid, var3_name, NF90_REAL, dimids, var3_varid) )
      if(outputType.EQ.4) call check( nf90_def_var_deflate(ncid,var3_varid,1,1,deflate) )
      call check( nf90_put_att(ncid, var3_varid, units, var3_units) )
      call check( nf90_put_att(ncid, var3_varid,'long_name','peak_direction') )
      call check( nf90_put_att(ncid, var3_varid,'missing_value','9999.00') )
       else
      call check( nf90_def_var(ncid, var4_name, NF90_REAL, dimids, var4_varid) )
      if(outputType.EQ.4) call check( nf90_def_var_deflate(ncid,var4_varid,1,1,deflate) )
      call check( nf90_put_att(ncid, var4_varid, units, var4_units) )
      call check( nf90_put_att(ncid,var4_varid,'long_name','directional_spread') )
      call check( nf90_put_att(ncid, var4_varid,'missing_value','9999.00') )
      endif
      call check( nf90_enddef(ncid) )
#endif
!  
#ifdef W3_TRKNC
      call check( nf90_put_var(ncid, lat_varid, lats) )
      call check( nf90_put_var(ncid, lon_varid, lons) )
      call check( nf90_put_var(ncid, rec_varid, times) )
#endif
!  
#ifdef W3_TRKNC
      count = (/ nlons, nlats, nsys, ntime /)
      start = (/ 1, 1, 1, 1 /)
      if( ivar == 1) then
      call check( nf90_put_var(ncid, var1_varid, data_in, start = start, &
                              count = count) )
       else if( ivar == 2) then
      call check( nf90_put_var(ncid, var2_varid, data_in, start = start, &
                              count = count) )
       else if( ivar == 3) then
      call check( nf90_put_var(ncid, var3_varid, data_in, start = start, &
                              count = count) )
       else
      call check( nf90_put_var(ncid, var4_varid, data_in, start = start, &
                              count = count) )
      endif
      call check( nf90_close(ncid) )
      end subroutine t2netcdf
#endif
!
#ifdef W3_TRKNC
      subroutine check(status)
      use netcdf
      integer, intent ( in) :: status
      if(status /= nf90_noerr) then 
        write(6,996)
          996 FORMAT (/' *** WAVEWATCH III ERROR IN WW3_SYSTRK:'/           &
                       'netCDF error:')
        print *, trim(nf90_strerror(status))
        stop "Stopped in netcdf output part"
      endif
      end subroutine check  
#endif
!
#ifdef W3_TRKNC
      subroutine pt2netcdf(longitude,latitude,hs,tp,&
       dir,npoints,date1,date2,dt,ntime,outputType)
      USE W3TIMEMD
      use netcdf
      implicit none
      integer :: ntime,npoints,outputType
      integer, parameter :: deflate = 1
      integer :: iret, oldMode
      integer :: ncid
      integer :: system_index_dim
      integer :: point_dim,rec_dim
      integer :: nsys
      integer :: start(3), count(3)
      parameter (nsys = 10)
      integer :: latitude_id
      integer :: longitude_id
      integer :: time_id
      integer :: hs_id
      integer :: tp_id
      integer :: dir_id
      integer :: time_rank
      integer :: hs_rank
      integer :: tp_rank
      integer :: dir_rank
      parameter (time_rank = 1)
      parameter (hs_rank = 3)
      parameter (tp_rank = 3)
      parameter (dir_rank = 3)
#endif
!
#ifdef W3_TRKNC
      integer :: hs_dims(hs_rank)
      integer :: tp_dims(tp_rank)
      integer :: dir_dims(dir_rank)
      real :: latitude(npoints),dt
      real :: longitude(npoints)
      real :: hs(nsys, npoints, ntime)
      real :: tp(nsys, npoints, ntime)
      real :: dir(nsys, npoints, ntime)
      integer :: iyc,imc,idc,ihc,iminc,isc,Jday,Jday0,rec
      double precision date1,date2,timenc
      double precision times(ntime)
#endif
!
#ifdef W3_TRKNC
      iyc=date1/10000
      imc=(date1-iyc*10000)/100
      idc=int(date1-DBLE(iyc*10000)-DBLE(imc*100))
      ihc=date2/10000
      iminc=(date2-ihc*10000)/100
      isc=date2-ihc*10000-100*iminc
      timenc=DBLE(julday(idc,imc,iyc))+(DBLE(ihc)+(DBLE(iminc)+ &
       (DBLE(isc)/60.0D0))/60.0D0)/24.0D0
      Jday0=julday(1,1,1990)
      timenc=timenc-Jday0
      do rec=1,ntime
       times(rec)=timenc+DBLE( (rec-1)*dt)/3600.0D0/24.0D0 
      enddo
#endif
!
#ifdef W3_TRKNC
      if(outputType.EQ.3) then
      iret = nf90_create('sys_pnt.ww3.nc', NF90_CLOBBER, ncid)
      endif
      if (outputType.EQ.4) iret = nf90_create('sys_pnt.ww3.nc',NF90_NETCDF4, ncid)
      call check(iret)
      iret = nf90_set_fill(ncid,nf90_nofill,oldMode) 
      call check(iret)
! define dimensions
      iret = nf90_def_dim(ncid, 'system_index', nsys, system_index_dim)
      call check(iret)
      iret = nf90_def_dim(ncid, 'point', npoints, point_dim)
      call check(iret)
      iret = nf90_def_dim(ncid, 'time', ntime, rec_dim)
      call check(iret)
! define variables
      iret = nf90_def_var(ncid, 'latitude', NF90_REAL, point_dim, &
          latitude_id)
      call check(iret)
      if (outputType.EQ.4) call check( nf90_def_var_deflate(ncid,latitude_id,1,1,deflate))
      iret = nf90_def_var(ncid, 'longitude', NF90_REAL, point_dim, &
        longitude_id)
      call check(iret)
      if (outputType.EQ.4) call check( nf90_def_var_deflate(ncid,longitude_id,1,1,deflate))
      iret = nf90_def_var(ncid, 'time', NF90_DOUBLE, rec_dim, &
         time_id)
      call check(iret)
      if (outputType.EQ.4) call check( nf90_def_var_deflate(ncid,time_id,1,1,deflate) )
      hs_dims(3) = rec_dim
      hs_dims(2) = point_dim
      hs_dims(1) = system_index_dim
      iret = nf90_def_var(ncid, 'hs', NF90_REAL,  &
        hs_dims, hs_id)
      call check(iret)
      if (outputType.EQ.4) call check( nf90_def_var_deflate(ncid,hs_id,1,1,deflate))
      tp_dims(3) = rec_dim
      tp_dims(2) = point_dim
      tp_dims(1) = system_index_dim
      iret = nf90_def_var(ncid, 'tp', NF90_REAL, &
        tp_dims, tp_id)
      call check(iret)
      if (outputType.EQ.4) call check( nf90_def_var_deflate(ncid,tp_id,1,1,deflate))
      dir_dims(3) = rec_dim
      dir_dims(2) = point_dim
      dir_dims(1) = system_index_dim
      iret = nf90_def_var(ncid, 'dir', NF90_REAL, & 
        dir_dims, dir_id)
      call check(iret)
      if (outputType.EQ.4) call check( nf90_def_var_deflate(ncid,dir_id,1,1,deflate))
! assign attributes
      iret = nf90_put_att(ncid, latitude_id, 'units', 'degrees_north')
      call check(iret)
      iret = nf90_put_att(ncid, latitude_id, 'long_name', 'latitude')
      call check(iret)
      iret = nf90_put_att(ncid, latitude_id, 'standard_name', 'latitude')
      call check(iret)
      iret = nf90_put_att(ncid, latitude_id, 'axis', 'Y')
      call check(iret)
      iret = nf90_put_att(ncid, longitude_id, 'units', 'degrees_east')
      call check(iret)
      iret = nf90_put_att(ncid, longitude_id,'long_name','longitude')
      call check(iret)
      iret = nf90_put_att(ncid, longitude_id,'standard_name','longitude')
      call check(iret)
      iret = nf90_put_att(ncid, longitude_id, 'axis', 'X')
      call check(iret)
      iret = nf90_put_att(ncid, time_id, 'units', &
        'days since 1990-01-01 00:00:00')
      call check(iret)
      iret = nf90_put_att(ncid, time_id, 'long_name','julian day(UT)')
      call check(iret)
      iret = nf90_put_att(ncid, time_id, 'standard_name','time')
      call check(iret)
      iret = nf90_put_att(ncid, time_id, 'conventions', &
       'relative julian day with decimal part (as part of the day)')
      call check(iret)
      iret = nf90_put_att(ncid, time_id, 'axis', 'T')
      call check(iret)
      iret = nf90_put_att(ncid, hs_id, 'units', 'm')
      call check(iret)
      iret = nf90_put_att(ncid, hs_id,'long_name','significant_wave_height')
      call check(iret)
      iret = nf90_put_att(ncid, hs_id, 'missing_value', & 
        '999.9999')
      call check(iret)
      iret = nf90_put_att(ncid, tp_id, 'units', 's')
      call check(iret)
      iret = nf90_put_att(ncid, tp_id,'long_name','peak_period')
      call check(iret)
      iret = nf90_put_att(ncid, tp_id, 'missing_value', &
       '999.9999')
      call check(iret)
      iret = nf90_put_att(ncid, dir_id, 'units', 'degrees')
      call check(iret)
      iret = nf90_put_att(ncid, dir_id,'long_name','peak_direction')
      call check(iret)
      iret = nf90_put_att(ncid, dir_id, 'missing_value',& 
         '999.9999')
      call check(iret)
! leave define mode
      iret = nf90_enddef(ncid)
      call check(iret)
      iret = nf90_put_var(ncid, latitude_id, latitude)
      call check(iret)
#endif
!
#ifdef W3_TRKNC
      iret = nf90_put_var(ncid, longitude_id, longitude)
      call check(iret)
#endif
! 
#ifdef W3_TRKNC
      iret = nf90_put_var(ncid, time_id, times)
      call check(iret)
#endif
! 
#ifdef W3_TRKNC
      start = (/ 1, 1, 1 /)
      count = (/ nsys,npoints,ntime /)
#endif
!
#ifdef W3_TRKNC
      iret = nf90_put_var(ncid, hs_id, hs,&
       start = start, count = count )
      call check(iret)
      iret = nf90_put_var(ncid, tp_id, tp, &
       start = start, count = count )
      call check(iret)
      iret = nf90_put_var(ncid, dir_id, dir,&
       start = start, count = count )
      call check(iret)
      iret = nf90_close(ncid)
      call check(iret)
      return
      end subroutine pt2netcdf
#endif

