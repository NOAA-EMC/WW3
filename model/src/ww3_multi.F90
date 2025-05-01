!> @file
!> @brief Contains driver program for multi-grid, W3MLTI.
!>
!> @author H. L. Tolman @date 29-May-2009

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!
!
!> @brief Program shell or driver to run the multi-grid wave model
!>  (uncoupled).
!>
!> @author H. L. Tolman @date 29-May-2009
PROGRAM W3MLTI
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         29-May-2009 |
  !/                  +-----------------------------------+
  !/
  !/    04-May-2005 : Origination.                        ( version 3.07 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    17-Feb-2016 : New version from namelist use       ( version 5.11 )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Program shell or driver to run the multi-grid wave model
  !     (uncoupled).
  !
  !  2. Method :
  !
  !  3. Parameters :
  !
  !     Local parameters.
  !     ----------------------------------------------------------------
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      WMINIT    Subr. WMINITMD Multi-grid model initialization.
  !      WMFINL    Subr. WMFINLMD Multi-grid model finalization.
  !
  !      MPI_INIT, MPI_COMM_SIZE, MPI_COMM_RANK, MPI_BARRIER,
  !         MPI_FINALIZE
  !                Subr.          Standard MPI routines.
  !     ----------------------------------------------------------------
  !
  !  5. Called by :
  !
  !     None, stand-alone program.
  !
  !  6. Error messages :
  !
  !  7. Remarks :
  !
  !     - This is he third version, version 1 and 2 were use for proof
  !       of concept only, and were not retained.
  !
  !  8. Structure :
  !
  !     ----------------------------------------------------------------
  !      0.  Initialization necessary for driver
  !        a General I/O: (implicit in wmmdatmd)
  !        b MPI environment
  !        c Identifying output to "screen" unit
  !      1.  Initialization of all wave models / grids       ( WMINIT )
  !      2.  Run the multi-grid models                       ( WMWAVE )
  !      3.  Finalization of wave model                      ( WMFINL )
  !      4.  Finalization of driver
  !     ----------------------------------------------------------------
  !
  !  9. Switches :
  !
  !       !/MPI   Including MPI routines / environment.
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  USE WMINITMD, ONLY: WMINIT, WMINITNML
  USE WMWAVEMD, ONLY: WMWAVE
  USE WMFINLMD, ONLY: WMFINL
  !/
  USE WMMDATMD, ONLY: MDSI, MDSO, MDSS, MDST, MDSE, &
       NMPROC, IMPROC, NMPSCR, NRGRD, ETIME
#ifdef W3_OMPG
  USE OMP_LIB
#endif
  !/
  IMPLICIT NONE
  !
#ifdef W3_MPI
  INCLUDE "mpif.h"
#endif
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
  INTEGER              :: I, MPI_COMM = -99
  INTEGER, ALLOCATABLE :: TEND(:,:)
  LOGICAL              :: FLGNML
#ifdef W3_MPI
  INTEGER              :: IERR_MPI
  LOGICAL              :: FLHYBR = .FALSE.
#endif
#ifdef W3_OMPH
  INTEGER              :: THRLEV
#endif
  !/
  !/ ------------------------------------------------------------------- /
  ! 0.  Initialization necessary for driver
  ! 0.a General I/O: all can start with initialization in wmmdatmd
  !
  ! 0.b MPI environment: Here, we use MPI_COMM_WORLD
  !
#ifdef W3_OMPH
  FLHYBR = .TRUE.
  ! For hybrid MPI-OpenMP specify required thread level:
  IF( FLHYBR ) THEN
    CALL MPI_INIT_THREAD(MPI_THREAD_FUNNELED, THRLEV, IERR_MPI)
  ELSE
#endif
#ifdef W3_MPI
    CALL MPI_INIT      ( IERR_MPI )
#endif
#ifdef W3_OMPH
  ENDIF
#endif
#ifdef W3_MPI
  MPI_COMM = MPI_COMM_WORLD
  CALL MPI_COMM_SIZE ( MPI_COMM, NMPROC, IERR_MPI )
  CALL MPI_COMM_RANK ( MPI_COMM, IMPROC, IERR_MPI )
  IMPROC = IMPROC + 1
#endif
  !
  ! 0.c Identifying output to "screen" unit
  !
  IF ( IMPROC .EQ. NMPSCR ) WRITE (*,900)
#ifdef W3_OMPH
  IF ( IMPROC .EQ. NMPSCR ) WRITE (*,905) &
       MPI_THREAD_FUNNELED, THRLEV
#endif
  !
#ifdef W3_OMPG
  IF( IMPROC .EQ. NMPSCR ) THEN
      WRITE(*,906) omp_get_max_threads()
    ENDIF
#endif
  !
  !/ ------------------------------------------------------------------- /
  ! 1.  Initialization of all wave models / grids
  !     Use only one of the calls ....
  !
  ! ... Log and screen output, no separate test output file
  !
  !     CALL WMINIT ( MDSI, MDSO, MDSS, MDST, MDSE, 'ww3_multi.inp', MPI_COMM )
  !
  ! ... Screen output disabled
  !
  !     CALL WMINIT ( MDSI, MDSO, MDSO, MDST, MDSE, 'ww3_multi.inp', MPI_COMM )
  !
  ! ... Separate test output file and file preamble defined
  !
  !     CALL WMINIT ( MDSI, MDSO, MDSS, 10, MDSE, 'ww3_multi.inp', MPI_COMM,        &
  !                   './data/' )
  !
  ! ... Separate test output file
  !
  INQUIRE(FILE="ww3_multi.nml", EXIST=FLGNML)
  IF (FLGNML) THEN
    CALL WMINITNML ( MDSI, MDSO, MDSS, 10, MDSE, 'ww3_multi.nml', MPI_COMM )
  ELSE
    CALL WMINIT ( MDSI, MDSO, MDSS, 10, MDSE, 'ww3_multi.inp', MPI_COMM )
  END IF
  !

  !
  !/ ------------------------------------------------------------------- /
  ! 2.  Run the wave model
  !
  ALLOCATE ( TEND(2,NRGRD) )
  !
  DO I=1, NRGRD
    TEND(:,I) = ETIME(:)
  END DO
  !
  CALL WMWAVE ( TEND )
  !
  DEALLOCATE ( TEND )
  !
  !/ ------------------------------------------------------------------- /
  ! 3.  Finalize the wave model
  !
  CALL WMFINL
  !
  !/ ------------------------------------------------------------------- /
  ! 4   Finalize the driver
  !
  IF ( IMPROC .EQ. NMPSCR ) WRITE (*,999)
  !
#ifdef W3_MPI
  CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
  CALL MPI_FINALIZE  ( IERR_MPI )
#endif
  !
  ! Formats
  !
900 FORMAT (/15X,'     *** WAVEWATCH III Multi-grid shell ***    '/ &
       15X,'================================================='/)
#ifdef W3_OMPH
905 FORMAT ( '  Hybrid MPI/OMP thread support level:'/        &
       '     Requested: ', I2/                          &
       '      Provided: ', I2/ )
#endif
  !
#ifdef W3_OMPG
906 FORMAT ( '  OMP threading enabled. Number of threads: ', I3 / )
#endif
  !
999 FORMAT(//'  End of program '/                                   &
       ' ========================================'/           &
       '          WAVEWATCH III Multi-grid shell '/)
  !/
  !/ End of W3MLTI ----------------------------------------------------- /
  !/
END PROGRAM W3MLTI
