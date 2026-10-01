#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3NMLOUTPMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           E. Rainville            |
  !/                  |                                   |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         14-July-2026|
  !/                  +-----------------------------------+
  !/
  !/    For updates see subroutines.
  !/
  !  1. Purpose :
  !
  !     Contains all data structure defintions and subroutines to manage the outp namelist.
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
    CHARACTER(30)               :: PREFIX
    CHARACTER(1024)             :: LIST
    INTEGER                     :: ITYPE 
  END TYPE NML_POINT_T

  ! spectra structure
  TYPE NML_SPECTRA_T
    INTEGER                     :: OUTPUT
    REAL                        :: SCALE_FAC
    REAL                        :: OUTPUT_FAC
    INTEGER                     :: ITYPE
    INTEGER                     :: UNIT_NUM_TRANS
    LOGICAL                     :: FLAG_UNFORMAT_TRANS
  END TYPE NML_SPECTRA_T

  ! param structure
  TYPE NML_PARAM_T
    INTEGER                     :: OUTPUT
    INTEGER                     :: UNIT_NUM_TABLE
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

  ! Partition structure
  TYPE NML_PART_T
    INTEGER                     :: OTYPE
    INTEGER                     :: UNIT_NUM_TRANS
    CHARACTER(15)               :: REF_DATE
    CHARACTER(4)                :: TIME_ZONE
  END TYPE NML_PART_T

  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN

CONTAINS 
  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLOUTP(NDSI, INFILE, NML_POINT,              &
        NML_SPECTRA, NML_PARAM, NML_SOURCE, NML_PART, IERR) 
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         08-July-2026|
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
    !      NML_SPECTRA   type.
    !      NML_PARAM     type.
    !      NML_SOURCE    type.
    !      NML_PART      type.
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

    ! Define the input arguments
    INTEGER, INTENT(IN)                         :: NDSI
    CHARACTER*(*), INTENT(IN)                   :: INFILE
    TYPE(NML_POINT_T), INTENT(INOUT)            :: NML_POINT
    TYPE(NML_SPECTRA_T), INTENT(INOUT)          :: NML_SPECTRA
    TYPE(NML_PARAM_T), INTENT(INOUT)            :: NML_PARAM
    TYPE(NML_SOURCE_T), INTENT(INOUT)           :: NML_SOURCE
    TYPE(NML_PART_T), INTENT(INOUT)             :: NML_PART
    INTEGER, INTENT(OUT)                        :: IERR

#ifdef W3_S
    INTEGER, SAVE                             :: IENT = 0
#endif

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

    ! Point Structure - Read and Report
    CALL READ_POINT_NML (NDSI, NML_POINT)
    CALL REPORT_POINT_NML (NML_POINT)

    ! Spectra Structure - Read and Report
    CALL READ_SPECTRA_NML (NDSI, NML_SPECTRA)
    CALL REPORT_SPECTRA_NML (NML_SPECTRA)

    ! Param Structure - Read and Report
    CALL READ_PARAM_NML (NDSI, NML_PARAM)
    CALL REPORT_PARAM_NML (NML_PARAM)

    ! Source Structure - Read and Report
    CALL READ_SOURCE_NML (NDSI, NML_SOURCE)
    CALL REPORT_SOURCE_NML (NML_SOURCE)

    ! Partition Structure - Read and Report
    CALL READ_PART_NML (NDSI, NML_PART)
    CALL REPORT_PART_NML (NML_PART)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)
    
END SUBROUTINE W3NMLOUTP

!/ ------------------------------------------------------------------- /

  SUBROUTINE READ_POINT_NML (NDSI, NML_POINT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         10-July-2026|
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

#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'READ_POINT_NML')
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                 :: NDSI
    TYPE(NML_POINT_T), INTENT(INOUT)    :: NML_POINT

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_POINT_T) :: POINT
    NAMELIST /POINT_NML/ POINT

    ! Define additional variables
    IERR = 0

    ! set default values for point structure
    POINT%TIMESTART = '19000101 000000'
    POINT%TIMESTRIDE = '0'
    POINT%TIMECOUNT = '1000000'
    POINT%TIMESPLIT = 0
    POINT%PREFIX = 'wave'
    POINT%LIST = 'all'
    POINT%ITYPE = 1

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
  SUBROUTINE READ_SPECTRA_NML (NDSI, NML_SPECTRA)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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
    SPECTRA%OUTPUT              = 3
    SPECTRA%SCALE_FAC           = 1
    SPECTRA%OUTPUT_FAC          = 0
    SPECTRA%ITYPE                = 0
    SPECTRA%UNIT_NUM_TRANS      = 33 
    SPECTRA%FLAG_UNFORMAT_TRANS = .FALSE.

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
    !/                  |           E. Rainville            |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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
    !/                  |           E. Rainville            |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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

  SUBROUTINE READ_PART_NML (NDSI, NML_PART)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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
    !      NML_PART     Type.
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
    TYPE(NML_PART_T), INTENT(INOUT)     :: NML_PART

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_PART_T) :: PART
    NAMELIST /PART_NML/ PART

#ifdef W3_S
    INTEGER, SAVE                       :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_PART_NML')
#endif

    ! set default values for source structure
    PART%OTYPE            = 1
    PART%UNIT_NUM_TRANS   = 2
    PART%REF_DATE         = '19680606 060000'
    PART%TIME_ZONE        = 'UTC'

    ! read source namelist
    REWIND (NDSI)
    READ (NDSI, nml=PART_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_PART_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (5)
    END IF

    ! save namelist
    NML_PART = PART

  END SUBROUTINE READ_PART_NML

  !/ ------------------------------------------------------------------- /
   !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_POINT_NML (NML_POINT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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

#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_POINT_NML')
#endif

    IMPLICIT NONE

    TYPE(NML_POINT_T), INTENT(IN) :: NML_POINT

    WRITE (MSG,'(A)') 'POINT % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'TIMESTART  = ', TRIM(NML_POINT%TIMESTART)
    WRITE (NDSN,10) TRIM(MSG),'TIMESTRIDE = ', TRIM(NML_POINT%TIMESTRIDE)
    WRITE (NDSN,10) TRIM(MSG),'TIMECOUNT  = ', TRIM(NML_POINT%TIMECOUNT)
    WRITE (NDSN,11) TRIM(MSG),'TIMESPLIT  = ', NML_POINT%TIMESPLIT
    WRITE (NDSN,10) TRIM(MSG),'PREFIX     = ', TRIM(NML_POINT%PREFIX)
    WRITE (NDSN,10) TRIM(MSG),'LIST       = ', TRIM(NML_POINT%LIST)
    WRITE (NDSN,11) TRIM(MSG),'TYPE       = ', NML_POINT%ITYPE

    10 FORMAT (A,2X,A,A)
    11 FORMAT (A,2X,A,I8)
    13 FORMAT (A,2X,A,L1)

  END SUBROUTINE REPORT_POINT_NML

  !/ ------------------------------------------------------------------- /

  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SPECTRA_NML (NML_SPECTRA)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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
    WRITE (NDSN,11) TRIM(MSG),'UNIT_NUM_TRANS = ', NML_SPECTRA%UNIT_NUM_TRANS
    WRITE (NDSN,13) TRIM(MSG),'FLAG_UNFORMAT_TRANS = ', NML_SPECTRA%FLAG_UNFORMAT_TRANS


11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SPECTRA_NML

  !/ ------------------------------------------------------------------- /


  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_PARAM_NML (NML_PARAM)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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
    WRITE (NDSN,11) TRIM(MSG),'UNIT_NUM_TABLE = ', NML_PARAM%UNIT_NUM_TABLE

11  FORMAT (A,2X,A,I8)

  END SUBROUTINE REPORT_PARAM_NML

  !/ ------------------------------------------------------------------- /

  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SOURCE_NML (NML_SOURCE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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
    WRITE (NDSN,13) TRIM(MSG),'BOTTOM       = ', NML_SOURCE%BOTTOM
    WRITE (NDSN,13) TRIM(MSG),'ICE          = ', NML_SOURCE%ICE
    WRITE (NDSN,13) TRIM(MSG),'TOTAL        = ', NML_SOURCE%TOTAL



11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SOURCE_NML

  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_PART_NML (NML_PART)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           E. Rainville            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-July-2026|
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
    !      NML_PART  Type.
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
    !      W3NMLOUTP Subr.   N/A    Namelist configuration routine.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
  !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_PART_T), INTENT(IN) :: NML_PART
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_PART_NML')
#endif

    WRITE (MSG,'(A)') 'PART % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'OTYPE          = ', NML_PART%OTYPE
    WRITE (NDSN,11) TRIM(MSG),'UNIT_NUM_TRANS = ', NML_PART%UNIT_NUM_TRANS
    WRITE (NDSN,10) TRIM(MSG),'REF_DATE       = ', TRIM(NML_PART%REF_DATE)
    WRITE (NDSN,10) TRIM(MSG),'TIME_ZONE      = ', TRIM(NML_PART%TIME_ZONE)

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_PART_NML

END MODULE W3NMLOUTPMD
