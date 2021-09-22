#include "w3macros.h" 
!/ ------------------------------------------------------------------- /
      MODULE W3NMLUPRSTRMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Oct-2020 |
!/                  +-----------------------------------+
!/
!/    For updates see subroutines.
!/
!  1. Purpose :
!
!     Manages namelists from configuration file ww3_uprstr.nml for ww3_uprstr program
!
!/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! restart time
  TYPE NML_RESTART_T
    CHARACTER(15)               :: RESTARTTIME
  END TYPE NML_RESTART_T

  ! update approach
  TYPE NML_UPDATE_T
    CHARACTER(5)                :: UPDPROC
    REAL                        :: PRCNTG
    REAL                        :: PRCNTGCAP
    REAL                        :: THRWSEA
    CHARACTER(30)               :: FILE
  END TYPE NML_UPDATE_T

  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN




  CONTAINS
!/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLUPRSTR (NDSI, INFILE, NML_RESTART, NML_UPDATE, IERR)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Oct-2020 |
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
!      NML_RESTART type.
!      NML_UPDATE  type.
!      IERR        Int.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      TYPE  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
!      READ_RESTART_NML
!      READ_UPDATE_NML
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name       TYPE   Module   Description
!     ----------------------------------------------------------------
!      WW3_UPRSTR Prog.  N/A      Update restart file
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

    INTEGER, INTENT(IN)                         :: NDSI
    CHARACTER*(*), INTENT(IN)                   :: INFILE
    TYPE(NML_RESTART_T), INTENT(INOUT)          :: NML_RESTART
    TYPE(NML_UPDATE_T), INTENT(INOUT)           :: NML_UPDATE
    INTEGER, INTENT(OUT)                        :: IERR
!/S      INTEGER, SAVE                             :: IENT = 0

    IERR = 0
!/S      CALL STRACE (IENT, 'W3NMLUPRSTR')

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

    ! read restart time namelist
    CALL READ_RESTART_NML (NDSI, NML_RESTART)
    CALL REPORT_RESTART_NML (NML_RESTART)

    ! read update approach namelist
    CALL READ_UPDATE_NML (NDSI, NML_UPDATE)
    CALL REPORT_UPDATE_NML (NML_UPDATE)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)

  END SUBROUTINE W3NMLUPRSTR


!/ ------------------------------------------------------------------- /



!/ ------------------------------------------------------------------- /

  SUBROUTINE READ_RESTART_NML (NDSI, NML_RESTART)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Oct-2020 |
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
!      NML_RESTART  Type.
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
!      Name        TYPE   Module  Description
!     ----------------------------------------------------------------
!      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                 :: NDSI
    TYPE(NML_RESTART_T), INTENT(INOUT)    :: NML_RESTART

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_RESTART_T) :: RESTART
    NAMELIST /RESTART_NML/ RESTART
!/S      INTEGER, SAVE                           :: IENT = 0

    IERR = 0
!/S      CALL STRACE (IENT, 'READ_RESTART_NML')

    ! set default values
    RESTART%RESTARTTIME  = '19680607 120000'

    ! read restart namelist
    REWIND (NDSI)
    READ (NDSI, nml=RESTART_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
        'ERROR: READ_RESTART_NML: namelist read error', &
        'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! save namelist
    NML_RESTART = RESTART

  END SUBROUTINE READ_RESTART_NML

!/ ------------------------------------------------------------------- /



!/ ------------------------------------------------------------------- /

  SUBROUTINE READ_UPDATE_NML (NDSI, NML_UPDATE)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Oct-2020 |
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
!      NML_UPDATE   Type.
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
!      Name        TYPE   Module  Description
!     ----------------------------------------------------------------
!      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                 :: NDSI
    TYPE(NML_UPDATE_T), INTENT(INOUT)   :: NML_UPDATE

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_UPDATE_T) :: UPDATE
    NAMELIST /UPDATE_NML/ UPDATE
!/S      INTEGER, SAVE                           :: IENT = 0

    IERR = 0
!/S      CALL STRACE (IENT, 'READ_UPDATE_NML')

    ! set default values for update approach
    ! as set, these would run the update but not correct
    ! any spectra (scalar correction by factor of 1.0)
    UPDATE%UPDPROC    = 'UPD0F'      ! Update type
    UPDATE%PRCNTG     = 1.0          ! Scalar correction factor (1.0=no correction)
    UPDATE%PRCNTGCAP  = 10.0         ! Cap on correction factor
    UPDATE%THRWSEA    = 0.7          ! Energy threshold for wind-sea dominance
    UPDATE%FILE       = 'anl.grbtxt' ! Corrected analysed SWH field file

    ! read file namelist
    REWIND (NDSI)
    READ (NDSI, nml=UPDATE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
        'ERROR: READ_UPDATE_NML: namelist read error', &
        'ERROR: '//TRIM(MSG)
      CALL EXTCDE (2)
    END IF

    ! save namelist
    NML_UPDATE = UPDATE

  END SUBROUTINE READ_UPDATE_NML

!/ ------------------------------------------------------------------- /



!/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_RESTART_NML (NML_RESTART)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Oct-2020 |
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
!      NML_RESTART  Type.
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
!      Name        TYPE   Module  Description
!     ----------------------------------------------------------------
!      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_RESTART_T), INTENT(IN) :: NML_RESTART
!/S      INTEGER, SAVE                           :: IENT = 0

!/S      CALL STRACE (IENT, 'REPORT_RESTART_NML')

      WRITE (MSG,'(A)') 'RESTART % '
      WRITE (NDSN,'(A)')
      WRITE (NDSN,10) TRIM(MSG),'RESTARTTIME = ', TRIM(NML_RESTART%RESTARTTIME)

10  FORMAT (A,2X,A,A)

  END SUBROUTINE REPORT_RESTART_NML

!/ ------------------------------------------------------------------- /



!/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_UPDATE_NML (NML_UPDATE)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           M. Accensi              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-Oct-2020 |
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
!      NML_UPDATE  Type.
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
!      Name        TYPE   Module  Description
!     ----------------------------------------------------------------
!      W3NMLUPRSTR Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_UPDATE_T), INTENT(IN) :: NML_UPDATE
!/S      INTEGER, SAVE                           :: IENT = 0

!/S      CALL STRACE (IENT, 'REPORT_UPDATE_NML')

      WRITE (MSG,'(A)') 'UPDATE % '
      WRITE (NDSN,'(A)')
      WRITE (NDSN,10) TRIM(MSG),'UPDPROC   = ', TRIM(NML_UPDATE%UPDPROC)
      ! PRCNTG only used by UPD0F
      IF (TRIM(NML_UPDATE%UPDPROC) .EQ. 'UPD0F') THEN
         WRITE (NDSN,11) TRIM(MSG),'PRCNTG    = ', NML_UPDATE%PRCNTG
      ELSE
         WRITE (NDSN,11) TRIM(MSG),'PRCNTGCAP = ', NML_UPDATE%PRCNTGCAP
         ! THRWSEA only used by UPD5/6
         IF ((TRIM(NML_UPDATE%UPDPROC) .EQ. 'UPD5') .OR. &
             (TRIM(NML_UPDATE%UPDPROC) .EQ. 'UPD6')) THEN
            WRITE (NDSN,11) TRIM(MSG),'THRWSEA   = ', NML_UPDATE%THRWSEA
         ENDIF
         WRITE (NDSN,10) TRIM(MSG),'FILE      = ', TRIM(NML_UPDATE%FILE)
      ENDIF

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,F5.3)

  END SUBROUTINE REPORT_UPDATE_NML

!/ ------------------------------------------------------------------- /



END MODULE W3NMLUPRSTRMD

!/ ------------------------------------------------------------------- /
