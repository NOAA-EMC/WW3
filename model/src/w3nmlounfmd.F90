#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3NMLOUNFMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           M. Accensi              |
  !/                  |                                   |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         12-Jan-2021 |
  !/                  +-----------------------------------+
  !/
  !/    For updates see subroutines.
  !/
  !  1. Purpose :
  !
  !     Manages namelists from configuration file ww3_ounf.nml for ww3_ounf program
  !
  !/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! field structure
  TYPE NML_FIELD_T
    CHARACTER(15)               :: TIMESTART
    CHARACTER(15)               :: TIMESTRIDE
    CHARACTER(15)               :: TIMECOUNT
    INTEGER                     :: TIMESPLIT
    CHARACTER(1024)             :: LIST
    CHARACTER(15)               :: PARTITION
    LOGICAL                     :: SAMEFILE
    LOGICAL                     :: VECTOR
    INTEGER                     :: TYPE
    CHARACTER(15)               :: TIMEREF
    LOGICAL                     :: FCVARS
    CHARACTER                   :: TIMEVAR
    CHARACTER                   :: TIMEUNIT
    CHARACTER(15)               :: TIMEEPOCH
    REAL                        :: NOVAL
    LOGICAL                     :: MAPSTA
  END TYPE NML_FIELD_T

  ! file structure
  TYPE NML_FILE_T
    CHARACTER(30)               :: PREFIX
    INTEGER                     :: NETCDF
    INTEGER                     :: IX0
    INTEGER                     :: IXN
    INTEGER                     :: IY0
    INTEGER                     :: IYN
  END TYPE NML_FILE_T

  ! smc grid structure
  TYPE NML_SMC_T
    INTEGER                     :: TYPE
    REAL                        :: SXO
    REAL                        :: SYO
    REAL                        :: EXO
    REAL                        :: EYO
    INTEGER                     :: CELFAC
  END TYPE NML_SMC_T

  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN




CONTAINS
  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLOUNF (NDSI, INFILE, NML_FIELD, NML_FILE, NML_SMC, IERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         12-Jan-2021 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. Purpose :
    !
    !     Reads all the namelist to define the output field
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
    !      NML_FIELD   type.
    !      NML_FILE    type.
    !      NML_SMC     type.
    !      IERR        Int.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !      READ_FIELD_NML
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_OUNF  Prog.   N/A    Postprocess output fields.
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
    TYPE(NML_FIELD_T), INTENT(INOUT)            :: NML_FIELD
    TYPE(NML_FILE_T), INTENT(INOUT)             :: NML_FILE
    TYPE(NML_SMC_T), INTENT(INOUT)              :: NML_SMC
    INTEGER, INTENT(OUT)                        :: IERR
#ifdef W3_S
    INTEGER, SAVE                             :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'W3NMLOUNF')
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

    ! read field namelist
    CALL READ_FIELD_NML (NDSI, NML_FIELD)
    CALL REPORT_FIELD_NML (NML_FIELD)

    ! read file namelist
    CALL READ_FILE_NML (NDSI, NML_FILE)
    CALL REPORT_FILE_NML (NML_FILE)

    ! read smc namelist
    CALL READ_SMC_NML (NDSI, NML_SMC)
    CALL REPORT_SMC_NML (NML_SMC)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)

  END SUBROUTINE W3NMLOUNF


  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_FIELD_NML (NDSI, NML_FIELD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         12-Jan-2021 |
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
    !      NML_FIELD    Type.
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
    !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
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
    USE CONSTANTS, ONLY: UNDEF

    IMPLICIT NONE

    INTEGER, INTENT(IN)                 :: NDSI
    TYPE(NML_FIELD_T), INTENT(INOUT)    :: NML_FIELD

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_FIELD_T) :: FIELD
    NAMELIST /FIELD_NML/ FIELD
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_FIELD_NML')
#endif

    ! set default values for field structure
    FIELD%TIMESTART  = '19000101 000000'
    FIELD%TIMESTRIDE = '0'
    FIELD%TIMECOUNT  = '1000000000'
    FIELD%TIMESPLIT  = 6
    FIELD%LIST       = 'unset'
    FIELD%PARTITION  = '0 1 2 3'
    FIELD%SAMEFILE   = .TRUE.
    FIELD%VECTOR     = .TRUE.
    FIELD%TYPE       = 3
    FIELD%TIMEREF    = 'unset'
    FIELD%FCVARS     = .FALSE.
    FIELD%TIMEVAR    = 'D'
    FIELD%TIMEUNIT   = 'D'
    FIELD%TIMEEPOCH  = '19900101 000000'
    FIELD%NOVAL      = UNDEF
    FIELD%MAPSTA     = .TRUE.

    ! read field namelist
    REWIND (NDSI)
    READ (NDSI, nml=FIELD_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_FIELD_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! Default forecast reference time to start time
    IF(FIELD%TIMEREF == 'unset') FIELD%TIMEREF = FIELD%TIMESTART

    ! save namelist
    NML_FIELD = FIELD

  END SUBROUTINE READ_FIELD_NML

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
    !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
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
    FILE%PREFIX    = 'ww3.'
    FILE%NETCDF    = 3
    FILE%IX0       = 1
    FILE%IXN       = 1000000000
    FILE%IY0       = 1
    FILE%IYN       = 1000000000

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

  SUBROUTINE READ_SMC_NML (NDSI, NML_SMC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         19-Sep-2018 |
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
    !      NML_SMC      Type.
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
    !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
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
    TYPE(NML_SMC_T), INTENT(INOUT)      :: NML_SMC

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_SMC_T) :: SMC
    NAMELIST /SMC_NML/ SMC
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_SMC_NML')
#endif

    ! set default values for smc structure
    SMC%SXO       = -999.9
    SMC%SYO       = -999.9
    SMC%EXO       = -999.9
    SMC%EYO       = -999.9
    SMC%CELFAC    = 1
    SMC%TYPE      = 1

    ! read smc namelist
    REWIND (NDSI)
    READ (NDSI, nml=SMC_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_SMC_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (3)
    END IF

    ! save namelist
    NML_SMC = SMC

  END SUBROUTINE READ_SMC_NML

  !/ ------------------------------------------------------------------- /





  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_FIELD_NML (NML_FIELD)
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
    !      NML_FIELD  Type.
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
    !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_FIELD_T), INTENT(IN) :: NML_FIELD
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_FIELD_NML')
#endif

    WRITE (MSG,'(A)') 'FIELD % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_FIELD%TIMESTART)
    WRITE (NDSN,10) TRIM(MSG),'TIMESTRIDE = ', TRIM(NML_FIELD%TIMESTRIDE)
    WRITE (NDSN,10) TRIM(MSG),'TIMECOUNT  = ', TRIM(NML_FIELD%TIMECOUNT)

    WRITE (NDSN,11) TRIM(MSG),'TIMESPLIT  = ', NML_FIELD%TIMESPLIT
    WRITE (NDSN,10) TRIM(MSG),'LIST       = ', TRIM(NML_FIELD%LIST)
    WRITE (NDSN,10) TRIM(MSG),'PARTITION  = ', TRIM(NML_FIELD%PARTITION)
    WRITE (NDSN,13) TRIM(MSG),'SAMEFILE   = ', NML_FIELD%SAMEFILE
    WRITE (NDSN,11) TRIM(MSG),'TYPE       = ', NML_FIELD%TYPE
    WRITE (NDSN,10) TRIM(MSG),'FCVARS     = ', NML_FIELD%FCVARS
    WRITE (NDSN,10) TRIM(MSG),'TIMEREF    = ', NML_FIELD%TIMEREF
    WRITE (NDSN,14) TRIM(MSG),'NOVAL      = ', NML_FIELD%NOVAL
    WRITE (NDSN,13) TRIM(MSG),'MAPSTA     = ', NML_FIELD%MAPSTA


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_FIELD_NML

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
    !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
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
    WRITE (NDSN,11) TRIM(MSG),'IX0       = ', NML_FILE%IX0
    WRITE (NDSN,11) TRIM(MSG),'IXN       = ', NML_FILE%IXN
    WRITE (NDSN,11) TRIM(MSG),'IY0       = ', NML_FILE%IY0
    WRITE (NDSN,11) TRIM(MSG),'IYN       = ', NML_FILE%IYN

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I12)

  END SUBROUTINE REPORT_FILE_NML

  !/ ------------------------------------------------------------------- /


  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SMC_NML (NML_SMC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         19-Sep-2018 |
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
    !      NML_SMC   Type.
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
    !      W3NMLOUNF Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_SMC_T), INTENT(IN) :: NML_SMC
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_SMC_NML')
#endif

    WRITE (MSG,'(A)') 'SMC % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'TYPE      = ', NML_SMC%TYPE
    WRITE (NDSN,14) TRIM(MSG),'SXO       = ', NML_SMC%SXO
    WRITE (NDSN,14) TRIM(MSG),'SYO       = ', NML_SMC%SYO
    WRITE (NDSN,14) TRIM(MSG),'EXO       = ', NML_SMC%EXO
    WRITE (NDSN,14) TRIM(MSG),'EYO       = ', NML_SMC%EYO
    WRITE (NDSN,11) TRIM(MSG),'CELFAC    = ', NML_SMC%CELFAC

11  FORMAT (A,2X,A,I12)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SMC_NML

  !/ ------------------------------------------------------------------- /




END MODULE W3NMLOUNFMD

!/ ------------------------------------------------------------------- /
