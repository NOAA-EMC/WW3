#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3NMLOUNPMD
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
  !     Manages namelists from configuration file ww3_ounp.nml for ww3_ounp program
  !
  !/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! point structure
  TYPE NML_POINT_T
    CHARACTER(15)               :: TIMESTART
    CHARACTER(15)               :: TIMESTRIDE
    CHARACTER(15)               :: TIMECOUNT
    INTEGER                     :: TIMESPLIT
    CHARACTER(1024)             :: LIST
    LOGICAL                     :: SAMEFILE
    INTEGER                     :: BUFFER
    INTEGER                     :: TYPE
    LOGICAL                     :: DIMORDER
  END TYPE NML_POINT_T

  ! file structure
  TYPE NML_FILE_T
    CHARACTER(30)               :: PREFIX
    INTEGER                     :: NETCDF
  END TYPE NML_FILE_T

  ! spectra structure
  TYPE NML_SPECTRA_T
    INTEGER                     :: OUTPUT
    REAL                        :: SCALE_FAC
    REAL                        :: OUTPUT_FAC
    INTEGER                     :: TYPE
  END TYPE NML_SPECTRA_T

  ! param structure
  TYPE NML_PARAM_T
    INTEGER                     :: OUTPUT
  END TYPE NML_PARAM_T

  ! source structure
  TYPE NML_SOURCE_T
    INTEGER                     :: OUTPUT
    REAL                        :: SCALE_FAC
    REAL                        :: OUTPUT_FAC
    INTEGER                     :: TABLE_FAC
    LOGICAL                     :: SPECTRUM
    LOGICAL                     :: INPUT
    LOGICAL                     :: INTERACTIONS
    LOGICAL                     :: DISSIPATION
    LOGICAL                     :: BOTTOM
    LOGICAL                     :: ICE
    LOGICAL                     :: TOTAL
  END TYPE NML_SOURCE_T


  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN




CONTAINS
  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLOUNP (NDSI, INFILE, NML_POINT, NML_FILE,             &
       NML_SPECTRA, NML_PARAM, NML_SOURCE, IERR)
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
    !     Reads all the namelist to define the output point
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !      NDSI          Int.
    !      INFILE        Char.
    !      NML_POINT     type.
    !      NML_FILE      type.
    !      NML_SPECTRA   type.
    !      NML_PARAM     type.
    !      NML_SOURCE    type.
    !      IERR          Int.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !      READ_POINT_NML
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_OUNP  Prog.   N/A    Postprocess output points.
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
    TYPE(NML_POINT_T), INTENT(INOUT)            :: NML_POINT
    TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
    TYPE(NML_SPECTRA_T), INTENT(INOUT)          :: NML_SPECTRA
    TYPE(NML_PARAM_T), INTENT(INOUT)            :: NML_PARAM
    TYPE(NML_SOURCE_T), INTENT(INOUT)           :: NML_SOURCE
    INTEGER, INTENT(OUT)                        :: IERR
#ifdef W3_S
    INTEGER, SAVE                             :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'W3NMLOUNP')
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

    ! read point namelist
    CALL READ_POINT_NML (NDSI, NML_POINT)
    CALL REPORT_POINT_NML (NML_POINT)

    ! read file namelist
    CALL READ_FILE_NML (NDSI, NML_FILE)
    CALL REPORT_FILE_NML (NML_FILE)

    ! read spectra namelist
    CALL READ_SPECTRA_NML (NDSI, NML_SPECTRA)
    CALL REPORT_SPECTRA_NML (NML_SPECTRA)

    ! read param namelist
    CALL READ_PARAM_NML (NDSI, NML_PARAM)
    CALL REPORT_PARAM_NML (NML_PARAM)

    ! read source namelist
    CALL READ_SOURCE_NML (NDSI, NML_SOURCE)
    CALL REPORT_SOURCE_NML (NML_SOURCE)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)

  END SUBROUTINE W3NMLOUNP


  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_POINT_NML (NDSI, NML_POINT)
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
    !      NML_POINT    Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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
    TYPE(NML_POINT_T), INTENT(INOUT)    :: NML_POINT

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_POINT_T) :: POINT
    NAMELIST /POINT_NML/ POINT
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_POINT_NML')
#endif

    ! set default values for point structure
    POINT%TIMESTART  = '19000101 000000'
    POINT%TIMESTRIDE = '0'
    POINT%TIMECOUNT  = '1000000000'
    POINT%TIMESPLIT  = 6
    POINT%LIST       = 'all'
    POINT%SAMEFILE   = .TRUE.
    POINT%BUFFER     = 150
    POINT%TYPE       = 1
    POINT%DIMORDER   = .TRUE.

    ! read point namelist
    REWIND (NDSI)
    READ (NDSI, nml=POINT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_POINT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! save namelist
    NML_POINT = POINT

  END SUBROUTINE READ_POINT_NML

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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_FILE_T) :: FILE
    NAMELIST /FILE_NML/ FILE
#ifdef W3_S
    INTEGER, SAVE                       :: IENT = 0
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

  SUBROUTINE READ_SPECTRA_NML (NDSI, NML_SPECTRA)
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
    !      NML_SPECTRA  Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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
    TYPE(NML_SPECTRA_T), INTENT(INOUT)  :: NML_SPECTRA

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_SPECTRA_T) :: SPECTRA
    NAMELIST /SPECTRA_NML/ SPECTRA
#ifdef W3_S
    INTEGER, SAVE                       :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_SPECTRA_NML')
#endif

    ! set default values for spectra structure
    SPECTRA%OUTPUT      = 3
    SPECTRA%SCALE_FAC   = 1
    SPECTRA%OUTPUT_FAC  = 0
    SPECTRA%TYPE        = 4


    ! read spectra namelist
    REWIND (NDSI)
    READ (NDSI, nml=SPECTRA_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_SPECTRA_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (3)
    END IF

    ! save namelist
    NML_SPECTRA = SPECTRA

  END SUBROUTINE READ_SPECTRA_NML

  !/ ------------------------------------------------------------------- /


  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_PARAM_NML (NDSI, NML_PARAM)
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
    !      NML_PARAM    Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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
    TYPE(NML_PARAM_T), INTENT(INOUT)    :: NML_PARAM

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_PARAM_T) :: PARAM
    NAMELIST /PARAM_NML/ PARAM
#ifdef W3_S
    INTEGER, SAVE                       :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_PARAM_NML')
#endif

    ! set default values for param structure
    PARAM%OUTPUT      = 3

    ! read param namelist
    REWIND (NDSI)
    READ (NDSI, nml=PARAM_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_PARAM_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (4)
    END IF

    ! save namelist
    NML_PARAM = PARAM

  END SUBROUTINE READ_PARAM_NML

  !/ ------------------------------------------------------------------- /



  SUBROUTINE READ_SOURCE_NML (NDSI, NML_SOURCE)
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
    !      NML_SOURCE   Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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
    TYPE(NML_SOURCE_T), INTENT(INOUT)   :: NML_SOURCE

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_SOURCE_T) :: SOURCE
    NAMELIST /SOURCE_NML/ SOURCE
#ifdef W3_S
    INTEGER, SAVE                       :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_SOURCE_NML')
#endif

    ! set default values for source structure
    SOURCE%OUTPUT      = 4
    SOURCE%SCALE_FAC   = 0
    SOURCE%OUTPUT_FAC  = 0
    SOURCE%TABLE_FAC   = 0
    SOURCE%SPECTRUM    = .TRUE.
    SOURCE%INPUT       = .TRUE.
    SOURCE%INTERACTIONS= .TRUE.
    SOURCE%DISSIPATION = .TRUE.
    SOURCE%BOTTOM      = .TRUE.
    SOURCE%ICE         = .TRUE.
    SOURCE%TOTAL       = .TRUE.

    ! read source namelist
    REWIND (NDSI)
    READ (NDSI, nml=SOURCE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_SOURCE_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (5)
    END IF

    ! save namelist
    NML_SOURCE = SOURCE

  END SUBROUTINE READ_SOURCE_NML

  !/ ------------------------------------------------------------------- /







  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_POINT_NML (NML_POINT)
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
    !      NML_POINT  Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_POINT_T), INTENT(IN) :: NML_POINT
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_POINT_NML')
#endif

    WRITE (MSG,'(A)') 'POINT % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_POINT%TIMESTART)
    WRITE (NDSN,10) TRIM(MSG),'TIMESTRIDE = ', TRIM(NML_POINT%TIMESTRIDE)
    WRITE (NDSN,10) TRIM(MSG),'TIMECOUNT  = ', TRIM(NML_POINT%TIMECOUNT)

    WRITE (NDSN,11) TRIM(MSG),'TIMESPLIT  = ', NML_POINT%TIMESPLIT
    WRITE (NDSN,10) TRIM(MSG),'LIST       = ', TRIM(NML_POINT%LIST)
    WRITE (NDSN,13) TRIM(MSG),'SAMEFILE   = ', NML_POINT%SAMEFILE
    WRITE (NDSN,11) TRIM(MSG),'BUFFER     = ', NML_POINT%BUFFER
    WRITE (NDSN,11) TRIM(MSG),'TYPE       = ', NML_POINT%TYPE
    WRITE (NDSN,13) TRIM(MSG),'DIMORDER   = ', NML_POINT%DIMORDER

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)

  END SUBROUTINE REPORT_POINT_NML

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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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


  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SPECTRA_NML (NML_SPECTRA)
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
    !      NML_SPECTRA  Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_SPECTRA_T), INTENT(IN) :: NML_SPECTRA
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_SPECTRA_NML')
#endif

    WRITE (MSG,'(A)') 'SPECTRA % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'OUTPUT     = ', NML_SPECTRA%OUTPUT
    WRITE (NDSN,14) TRIM(MSG),'SCALE_FAC  = ', NML_SPECTRA%SCALE_FAC
    WRITE (NDSN,14) TRIM(MSG),'OUTPUT_FAC = ', NML_SPECTRA%OUTPUT_FAC
    WRITE (NDSN,11) TRIM(MSG),'TYPE       = ', NML_SPECTRA%TYPE


11  FORMAT (A,2X,A,I8)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SPECTRA_NML

  !/ ------------------------------------------------------------------- /


  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_PARAM_NML (NML_PARAM)
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
    !      NML_PARAM  Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_PARAM_T), INTENT(IN) :: NML_PARAM
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_PARAM_NML')
#endif

    WRITE (MSG,'(A)') 'PARAM % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'OUTPUT     = ', NML_PARAM%OUTPUT

11  FORMAT (A,2X,A,I8)

  END SUBROUTINE REPORT_PARAM_NML

  !/ ------------------------------------------------------------------- /

  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SOURCE_NML (NML_SOURCE)
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
    !      NML_SOURCE  Type.
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
    !      W3NMLOUNP Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_SOURCE_T), INTENT(IN) :: NML_SOURCE
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_SOURCE_NML')
#endif

    WRITE (MSG,'(A)') 'SOURCE % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'OUTPUT       = ', NML_SOURCE%OUTPUT
    WRITE (NDSN,14) TRIM(MSG),'SCALE_FAC    = ', NML_SOURCE%SCALE_FAC
    WRITE (NDSN,14) TRIM(MSG),'OUTPUT_FAC   = ', NML_SOURCE%OUTPUT_FAC
    WRITE (NDSN,11) TRIM(MSG),'TABLE_FAC    = ', NML_SOURCE%TABLE_FAC
    WRITE (NDSN,13) TRIM(MSG),'SPECTRUM     = ', NML_SOURCE%SPECTRUM
    WRITE (NDSN,13) TRIM(MSG),'INPUT        = ', NML_SOURCE%INPUT
    WRITE (NDSN,13) TRIM(MSG),'INTERACTIONS = ', NML_SOURCE%INTERACTIONS
    WRITE (NDSN,13) TRIM(MSG),'DISSIPATION  = ', NML_SOURCE%DISSIPATION
    WRITE (NDSN,13) TRIM(MSG),'ICE          = ', NML_SOURCE%ICE
    WRITE (NDSN,13) TRIM(MSG),'TOTAL        = ', NML_SOURCE%TOTAL



11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SOURCE_NML

  !/ ------------------------------------------------------------------- /



END MODULE W3NMLOUNPMD

!/ ------------------------------------------------------------------- /
