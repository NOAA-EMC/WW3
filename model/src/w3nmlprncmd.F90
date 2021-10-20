#include "w3macros.h" 
!/ ------------------------------------------------------------------- /
      MODULE W3NMLPRNCMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    For updates see subroutines.
!/
!  1. Purpose :
!
!     Manages namelists from configuration file ww3_prnc.nml for ww3_prnc program
!
!/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! field structure
  TYPE NML_FIELD_T
    LOGICAL                     :: ICE_PARAM1
    LOGICAL                     :: ICE_PARAM2
    LOGICAL                     :: ICE_PARAM3
    LOGICAL                     :: ICE_PARAM4
    LOGICAL                     :: ICE_PARAM5
    LOGICAL                     :: MUD_DENSITY
    LOGICAL                     :: MUD_THICKNESS
    LOGICAL                     :: MUD_VISCOSITY
    LOGICAL                     :: WATER_LEVELS
    LOGICAL                     :: CURRENTS
    LOGICAL                     :: WINDS
    LOGICAL                     :: WINDS_AST
    LOGICAL                     :: ATM_MOMENTUM
    LOGICAL                     :: AIR_DENSITY
    LOGICAL                     :: ICE_CONC
    LOGICAL                     :: ICE_BERG
    LOGICAL                     :: DATA_ASSIM
  END TYPE NML_FIELD_T

  ! grid structure
  TYPE NML_GRID_T
    LOGICAL                     :: ASIS
    LOGICAL                     :: LATLON
  END TYPE NML_GRID_T

  ! forcing structure
  TYPE NML_FORCING_T
    CHARACTER(15)               :: TIMESTART
    CHARACTER(15)               :: TIMESTOP
    TYPE(NML_FIELD_T)           :: FIELD
    TYPE(NML_GRID_T)            :: GRID
    CHARACTER(256)              :: TIDAL
  END TYPE NML_FORCING_T

  ! file structure
  TYPE NML_FILE_T
    CHARACTER(256)              :: FILENAME
    CHARACTER(100)              :: LONGITUDE
    CHARACTER(100)              :: LATITUDE
    CHARACTER(100)              :: VAR(3)
    CHARACTER(15)               :: TIMESHIFT
  END TYPE NML_FILE_T


  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN




  CONTAINS
!/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLPRNC (NDSI, INFILE, NML_FORCING, NML_FILE, IERR)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         04-Jan-2018 |
!/                  +-----------------------------------+
!/
!
!  1. Purpose :
!
!     Reads all the namelist to define the forcing field
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
!      NML_FORCING type.
!      NML_FILE    type.
!      IERR        Int.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      TYPE  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
!      READ_FORCING_NML
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      TYPE  Module   Description
!     ----------------------------------------------------------------
!      WW3_PRNC  Prog.   N/A    Preprocess forcing fields.
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
    TYPE(NML_FORCING_T), INTENT(INOUT)          :: NML_FORCING
    TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
    INTEGER, INTENT(OUT)                        :: IERR
#ifdef W3_S
      INTEGER, SAVE                             :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
      CALL STRACE (IENT, 'W3NMLPRNC')
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

    ! read forcing namelist
    CALL READ_FORCING_NML (NDSI, NML_FORCING)
    CALL REPORT_FORCING_NML (NML_FORCING)

    ! read file namelist
    CALL READ_FILE_NML (NDSI, NML_FILE)
    CALL REPORT_FILE_NML (NML_FILE)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)

  END SUBROUTINE W3NMLPRNC


!/ ------------------------------------------------------------------- /






!/ ------------------------------------------------------------------- /

  SUBROUTINE READ_FORCING_NML (NDSI, NML_FORCING)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
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
!      NML_FORCING  Type.
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
!      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
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
    TYPE(NML_FORCING_T), INTENT(INOUT)  :: NML_FORCING

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_FORCING_T) :: FORCING
    NAMELIST /FORCING_NML/ FORCING
#ifdef W3_S
      INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
      CALL STRACE (IENT, 'READ_FORCING_NML')
#endif

    ! set default values for forcing structure
    FORCING%TIMESTART  = '19000101 000000'
    FORCING%TIMESTOP   = '29001231 000000'
!
    FORCING%FIELD%ICE_PARAM1     = .FALSE.
    FORCING%FIELD%ICE_PARAM2     = .FALSE.
    FORCING%FIELD%ICE_PARAM3     = .FALSE.
    FORCING%FIELD%ICE_PARAM4     = .FALSE.
    FORCING%FIELD%ICE_PARAM5     = .FALSE.
    FORCING%FIELD%MUD_DENSITY    = .FALSE.
    FORCING%FIELD%MUD_THICKNESS  = .FALSE.
    FORCING%FIELD%MUD_VISCOSITY  = .FALSE.
    FORCING%FIELD%WATER_LEVELS   = .FALSE.
    FORCING%FIELD%CURRENTS       = .FALSE.
    FORCING%FIELD%WINDS          = .FALSE.
    FORCING%FIELD%WINDS_AST      = .FALSE.
    FORCING%FIELD%ATM_MOMENTUM   = .FALSE.
    FORCING%FIELD%AIR_DENSITY    = .FALSE.
    FORCING%FIELD%ICE_CONC       = .FALSE.
    FORCING%FIELD%ICE_BERG       = .FALSE.
    FORCING%FIELD%DATA_ASSIM     = .FALSE.
!
    FORCING%GRID%LATLON  = .FALSE.
    FORCING%GRID%ASIS    = .FALSE.
!
    FORCING%TIDAL  = 'unset'


    ! read forcing namelist
    REWIND (NDSI)
    READ (NDSI, nml=FORCING_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
        'ERROR: READ_FORCING_NML: namelist read error', &
        'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! set/check RETURN values
    IF (FORCING%TIDAL.NE.'unset') THEN
      IF (.NOT. FORCING%FIELD%WATER_LEVELS .AND. .NOT. FORCING%FIELD%CURRENTS) THEN
        WRITE (NDSE,'(A,I3)') 'ERROR: TIDAL must only be used on FIELD%WATER_LEVELS or FIELD%CURRENTS'
        CALL EXTCDE (2)
      ELSE IF (.NOT. FORCING%GRID%ASIS) THEN
        WRITE (NDSE,'(A,I3)') 'ERROR: TIDAL must only be used on GRID%ASIS'
        CALL EXTCDE (3)
      END IF
    END IF

    ! save namelist
    NML_FORCING = FORCING

  END SUBROUTINE READ_FORCING_NML

!/ ------------------------------------------------------------------- /



!/ ------------------------------------------------------------------- /

  SUBROUTINE READ_FILE_NML (NDSI, NML_FILE)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         04-Jan-2018 |
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
!      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
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
      INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
      CALL STRACE (IENT, 'READ_FILE_NML')
#endif

    ! set default values for file structure
    FILE%FILENAME  = 'unset'
    FILE%LONGITUDE = 'unset'
    FILE%LATITUDE  = 'unset'
    FILE%VAR(1)    = 'unset'
    FILE%VAR(2)    = 'unset'
    FILE%VAR(3)    = 'unset'
    FILE%TIMESHIFT = '00000000 000000'

    ! read file namelist
    REWIND (NDSI)
    READ (NDSI, nml=FILE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
        'ERROR: READ_FILE_NML: namelist read error', &
        'ERROR: '//TRIM(MSG)
      CALL EXTCDE (4)
    END IF

    ! save namelist
    NML_FILE = FILE

  END SUBROUTINE READ_FILE_NML

!/ ------------------------------------------------------------------- /







!/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_FORCING_NML (NML_FORCING)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
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
!      NML_FORCING  Type.
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
!      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_FORCING_T), INTENT(IN) :: NML_FORCING
#ifdef W3_S
      INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
      CALL STRACE (IENT, 'REPORT_FORCING_NML')
#endif

      WRITE (MSG,'(A)') 'FORCING % '
      WRITE (NDSN,'(A)')
      WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_FORCING%TIMESTART)
      WRITE (NDSN,10) TRIM(MSG),'TIMESTOP   = ', TRIM(NML_FORCING%TIMESTOP)

      WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM1     = ', NML_FORCING%FIELD%ICE_PARAM1
      WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM2     = ', NML_FORCING%FIELD%ICE_PARAM2
      WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM3     = ', NML_FORCING%FIELD%ICE_PARAM3
      WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM4     = ', NML_FORCING%FIELD%ICE_PARAM4
      WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_PARAM5     = ', NML_FORCING%FIELD%ICE_PARAM5
      WRITE (NDSN,13) TRIM(MSG),'FIELD % MUD_DENSITY    = ', NML_FORCING%FIELD%MUD_DENSITY
      WRITE (NDSN,13) TRIM(MSG),'FIELD % MUD_THICKNESS  = ', NML_FORCING%FIELD%MUD_THICKNESS
      WRITE (NDSN,13) TRIM(MSG),'FIELD % MUD_VISCOSITY  = ', NML_FORCING%FIELD%MUD_VISCOSITY
      WRITE (NDSN,13) TRIM(MSG),'FIELD % WATER_LEVELS   = ', NML_FORCING%FIELD%WATER_LEVELS
      WRITE (NDSN,13) TRIM(MSG),'FIELD % CURRENTS       = ', NML_FORCING%FIELD%CURRENTS
      WRITE (NDSN,13) TRIM(MSG),'FIELD % WINDS          = ', NML_FORCING%FIELD%WINDS
      WRITE (NDSN,13) TRIM(MSG),'FIELD % WINDS_AST      = ', NML_FORCING%FIELD%WINDS_AST
      WRITE (NDSN,13) TRIM(MSG),'FIELD % ATM_MOMENTUM   = ', NML_FORCING%FIELD%ATM_MOMENTUM
      WRITE (NDSN,13) TRIM(MSG),'FIELD % AIR_DENSITY    = ', NML_FORCING%FIELD%AIR_DENSITY
      WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_CONC       = ', NML_FORCING%FIELD%ICE_CONC
      WRITE (NDSN,13) TRIM(MSG),'FIELD % ICE_BERG       = ', NML_FORCING%FIELD%ICE_BERG
      WRITE (NDSN,13) TRIM(MSG),'FIELD % DATA_ASSIM     = ', NML_FORCING%FIELD%DATA_ASSIM

      WRITE (NDSN,13) TRIM(MSG),'GRID % ASIS   = ', NML_FORCING%GRID%ASIS
      WRITE (NDSN,13) TRIM(MSG),'GRID % LATLON = ', NML_FORCING%GRID%LATLON

      WRITE (NDSN,10) TRIM(MSG),'TIDAL = ', TRIM(NML_FORCING%TIDAL)


10  FORMAT (A,2X,A,A)
13  FORMAT (A,2X,A,L1)

  END SUBROUTINE REPORT_FORCING_NML

!/ ------------------------------------------------------------------- /






!/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_FILE_NML (NML_FILE)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         04-Jan-2018 |
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
!      W3NMLPRNC Subr.   N/A    Namelist configuration routine.
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
      WRITE (NDSN,10) TRIM(MSG),'FILENAME    = ', TRIM(NML_FILE%FILENAME)
      WRITE (NDSN,10) TRIM(MSG),'LONGITUDE   = ', TRIM(NML_FILE%LONGITUDE)
      WRITE (NDSN,10) TRIM(MSG),'LATITUDE    = ', TRIM(NML_FILE%LATITUDE)
      WRITE (NDSN,10) TRIM(MSG),'VAR(1)      = ', TRIM(NML_FILE%VAR(1))
      WRITE (NDSN,10) TRIM(MSG),'VAR(2)      = ', TRIM(NML_FILE%VAR(2))
      WRITE (NDSN,10) TRIM(MSG),'VAR(3)      = ', TRIM(NML_FILE%VAR(3))
      WRITE (NDSN,10) TRIM(MSG),'TIMESHIFT   = ', TRIM(NML_FILE%TIMESHIFT)

10  FORMAT (A,2X,A,A)


  END SUBROUTINE REPORT_FILE_NML

!/ ------------------------------------------------------------------- /





END MODULE W3NMLPRNCMD

!/ ------------------------------------------------------------------- /


















