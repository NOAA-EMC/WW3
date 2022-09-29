#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3NMLTRNCMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           M. Accensi              |
  !/                  |                                   |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         15-May-2018 |
  !/                  +-----------------------------------+
  !/
  !/    For updates see subroutines.
  !/
  !  1. Purpose :
  !
  !     Manages namelists from configuration file ww3_trnc.nml for ww3_trnc program
  !
  !/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! track structure
  TYPE NML_TRACK_T
    CHARACTER(15)               :: TIMESTART
    CHARACTER(15)               :: TIMESTRIDE
    CHARACTER(15)               :: TIMECOUNT
    INTEGER                     :: TIMESPLIT
  END TYPE NML_TRACK_T

  ! file structure
  TYPE NML_FILE_T
    CHARACTER(30)               :: PREFIX
    INTEGER                     :: NETCDF
  END TYPE NML_FILE_T

  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN




CONTAINS
  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLTRNC (NDSI, INFILE, NML_TRACK, NML_FILE, IERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-May-2018 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. Purpose :
    !
    !     Reads all the namelist to define the output track
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !      NDSI        Int.
    !      INFILE      Char.
    !      NML_TRACK   type.
    !      NML_FILE    type.
    !      IERR        Int.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !      READ_TRACK_NML
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_TRNC  Prog.   N/A    Postprocess output tracks.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE W3ODATMD, ONLY: NDSE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                         :: NDSI
    CHARACTER*(*), INTENT(IN)                   :: INFILE
    TYPE(NML_TRACK_T), INTENT(INOUT)            :: NML_TRACK
    TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
    INTEGER, INTENT(OUT)                        :: IERR
#ifdef W3_S
    INTEGER, SAVE                             :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'W3NMLTRNC')
#endif

    ! open namelist log file
    NDSN = 3
    OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.log failed'
      RETURN
    END IF

    ! open input file
    OPEN (NDSI, file=TRIM(INFILE), form='formatted', status='old', iostat=IERR)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' failed'
      RETURN
    END IF

    ! read track namelist
    CALL READ_TRACK_NML (NDSI, NML_TRACK)
    CALL REPORT_TRACK_NML (NML_TRACK)

    ! read file namelist
    CALL READ_FILE_NML (NDSI, NML_FILE)
    CALL REPORT_FILE_NML (NML_FILE)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)

  END SUBROUTINE W3NMLTRNC


  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_TRACK_NML (NDSI, NML_TRACK)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-May-2018 |
    !/                  +-----------------------------------+
    !/
    !  1. Purpose :
    !
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !      NDSI         Int.
    !      NML_TRACK    Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                 :: NDSI
    TYPE(NML_TRACK_T), INTENT(INOUT)    :: NML_TRACK
    INTEGER                             :: IERR

    ! locals
    TYPE(NML_TRACK_T) :: TRACK
    NAMELIST /TRACK_NML/ TRACK
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_TRACK_NML')
#endif

    ! set default values for track structure
    TRACK%TIMESTART  = '19000101 000000'
    TRACK%TIMESTRIDE = '0'
    TRACK%TIMECOUNT  = '1000000000'
    TRACK%TIMESPLIT  = 6

    ! read track namelist
    REWIND (NDSI)
    READ (NDSI, nml=TRACK_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_TRACK_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! save namelist
    NML_TRACK = TRACK

  END SUBROUTINE READ_TRACK_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_FILE_NML (NDSI, NML_FILE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-May-2018 |
    !/                  +-----------------------------------+
    !/
    !  1. Purpose :
    !
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !      NDSI         Int.
    !      NML_FILE     Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                 :: NDSI
    TYPE(NML_FILE_T), INTENT(INOUT)     :: NML_FILE
    INTEGER                             :: IERR

    ! locals
    TYPE(NML_FILE_T) :: FILE
    NAMELIST /FILE_NML/ FILE
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_FILE_NML')
#endif

    ! set default values for file structure
    FILE%PREFIX    = 'ww3.'
    FILE%NETCDF    = 3

    ! read file namelist
    REWIND (NDSI)
    READ (NDSI, nml=FILE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_FILE_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (2)
    END IF

    ! save namelist
    NML_FILE = FILE

  END SUBROUTINE READ_FILE_NML

  !/ ------------------------------------------------------------------- /







  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_TRACK_NML (NML_TRACK)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-May-2018 |
    !/                  +-----------------------------------+
    !/
    !/
    !  1. Purpose :
    !
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !      NML_TRACK  Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_TRACK_T), INTENT(IN) :: NML_TRACK
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_TRACK_NML')
#endif

    WRITE (MSG,'(A)') 'TRACK % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_TRACK%TIMESTART)
    WRITE (NDSN,10) TRIM(MSG),'TIMESTRIDE = ', TRIM(NML_TRACK%TIMESTRIDE)
    WRITE (NDSN,10) TRIM(MSG),'TIMECOUNT  = ', TRIM(NML_TRACK%TIMECOUNT)

    WRITE (NDSN,11) TRIM(MSG),'TIMESPLIT  = ', NML_TRACK%TIMESPLIT


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)

  END SUBROUTINE REPORT_TRACK_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_FILE_NML (NML_FILE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-May-2018 |
    !/                  +-----------------------------------+
    !/
    !/
    !  1. Purpose :
    !
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !      NML_FILE  Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLTRNC Subr.   N/A    Namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_FILE_T), INTENT(IN) :: NML_FILE
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_FILE_NML')
#endif

    WRITE (MSG,'(A)') 'FILE % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'PREFIX    = ', TRIM(NML_FILE%PREFIX)
    WRITE (NDSN,11) TRIM(MSG),'NETCDF    = ', NML_FILE%NETCDF


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)

  END SUBROUTINE REPORT_FILE_NML

  !/ ------------------------------------------------------------------- /





END MODULE W3NMLTRNCMD

!/ ------------------------------------------------------------------- /
