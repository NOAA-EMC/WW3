#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3NMLBOUNDMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           M. Accensi              |
  !/                  |                                   |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         27-May-2021 |
  !/                  +-----------------------------------+
  !/
  !/    For updates see subroutines.
  !/
  !  1. Purpose :
  !
  !     Manages namelists from configuration file ww3_bound.nml for ww3_bound program
  !
  !/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! bound structure
  TYPE NML_BOUND_T
    CHARACTER(5)                :: MODE      !< read/write mode
    INTEGER                     :: INTERP    !< interpolation mode
    INTEGER                     :: VERBOSE   !< verbose flag
    CHARACTER(128)              :: FILE      !< listing spec file unit
  END TYPE NML_BOUND_T


  ! miscellaneous
  CHARACTER(256)                :: MSG      !< report message
  INTEGER                       :: NDSN     !< namelist file unit




CONTAINS
  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLBOUND (NDSI, INFILE, NML_BOUND, IERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         27-May-2021 |
    !/                  +-----------------------------------+
    !/
    !
    !  1. Purpose :
    !
    !     Reads all the namelist to define the input boundary
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
    !      NML_BOUND   type.
    !      IERR        Int.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !      READ_BOUND_NML
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_BOUND  Prog.   N/A    Preprocess input boundaries.
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
    !/S      USE W3SERVMD, ONLY: STRACE

    IMPLICIT NONE

    INTEGER, INTENT(IN)                         :: NDSI       !< input file unit
    CHARACTER*(*), INTENT(IN)                   :: INFILE     !< input file name
    TYPE(NML_BOUND_T), INTENT(INOUT)            :: NML_BOUND  !< bound structure
    INTEGER, INTENT(OUT)                        :: IERR       !< error code
    !/S      INTEGER, SAVE                             :: IENT = 0   !< strace error code

    IERR = 0
    !/S      CALL STRACE (IENT, 'W3NMLBOUND')

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

    ! read bound namelist
    CALL READ_BOUND_NML (NDSI, NML_BOUND)
    CALL REPORT_BOUND_NML (NML_BOUND)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)

  END SUBROUTINE W3NMLBOUND


  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_BOUND_NML (NDSI, NML_BOUND)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         27-May-2021 |
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
    !      NML_BOUND    Type.
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
    !      W3NMLBOUND Subr.   N/A    Namelist configuration routine.
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
    !/S      USE W3SERVMD, ONLY: STRACE

    IMPLICIT NONE

    INTEGER, INTENT(IN)                 :: NDSI        !< namelist file unit
    TYPE(NML_BOUND_T), INTENT(INOUT)    :: NML_BOUND   !< bound structure

    ! locals
    INTEGER                   :: IERR                  !< error code
    TYPE(NML_BOUND_T) :: BOUND                         !< bound structure
    NAMELIST /BOUND_NML/ BOUND                         !< boudn namelist
    !/S      INTEGER, SAVE                           :: IENT = 0       !< strace error code

    IERR = 0
    !/S      CALL STRACE (IENT, 'READ_BOUND_NML')

    ! set default values for track structure
    BOUND%MODE       = 'WRITE'
    BOUND%INTERP     = 2
    BOUND%VERBOSE    = 1
    BOUND%FILE       = 'spec.list'

    ! read bound namelist
    REWIND (NDSI)
    READ (NDSI, nml=BOUND_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_BOUND_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! save namelist
    NML_BOUND = BOUND

  END SUBROUTINE READ_BOUND_NML

  !/ ------------------------------------------------------------------- /








  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_BOUND_NML (NML_BOUND)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         27-May-2021 |
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
    !      NML_BOUND  Type.
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
    !      W3NMLBOUND Subr.   N/A    Namelist configuration routine.
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

    !/S      USE W3SERVMD, ONLY: STRACE

    IMPLICIT NONE

    TYPE(NML_BOUND_T), INTENT(IN) :: NML_BOUND                  !< bound structure
    !/S      INTEGER, SAVE                           :: IENT = 0            ! strace error code

    !/S      CALL STRACE (IENT, 'REPORT_BOUND_NML')

    WRITE (MSG,'(A)') 'BOUND % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'MODE       = ', TRIM(NML_BOUND%MODE)
    WRITE (NDSN,11) TRIM(MSG),'INTERP     = ', NML_BOUND%INTERP
    WRITE (NDSN,11) TRIM(MSG),'VERBOSE    = ', NML_BOUND%VERBOSE
    WRITE (NDSN,10) TRIM(MSG),'FILE       = ', TRIM(NML_BOUND%FILE)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)

  END SUBROUTINE REPORT_BOUND_NML

  !/ ------------------------------------------------------------------- /

END MODULE W3NMLBOUNDMD

!/ ------------------------------------------------------------------- /
