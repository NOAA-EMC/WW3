#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3NMLSHELMD
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
  !     Manages namelists from configuration file ww3_shel.nml for ww3_shel program
  !
  !/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! domain structure
  TYPE NML_DOMAIN_T
    INTEGER                     :: IOSTYP
    CHARACTER(15)               :: START
    CHARACTER(15)               :: STOP
    CHARACTER(1)                :: OUTFFILE
  END TYPE NML_DOMAIN_T



  ! input structure
  TYPE NML_FORCING_T
    CHARACTER(13)               :: WATER_LEVELS
    CHARACTER(13)               :: CURRENTS
    CHARACTER(13)               :: WINDS
    CHARACTER(13)               :: ATM_MOMENTUM
    CHARACTER(13)               :: AIR_DENSITY
    CHARACTER(13)               :: ICE_CONC
    CHARACTER(13)               :: ICE_PARAM1
    CHARACTER(13)               :: ICE_PARAM2
    CHARACTER(13)               :: ICE_PARAM3
    CHARACTER(13)               :: ICE_PARAM4
    CHARACTER(13)               :: ICE_PARAM5
    CHARACTER(13)               :: MUD_DENSITY
    CHARACTER(13)               :: MUD_THICKNESS
    CHARACTER(13)               :: MUD_VISCOSITY
  END TYPE NML_FORCING_T
  !
  TYPE NML_ASSIM_T
    CHARACTER(13)               :: MEAN
    CHARACTER(13)               :: SPEC1D
    CHARACTER(13)               :: SPEC2D
  END TYPE NML_ASSIM_T
  !
  TYPE NML_INPUT_T
    TYPE(NML_FORCING_T)         :: FORCING
    TYPE(NML_ASSIM_T)           :: ASSIM
  END TYPE NML_INPUT_T

  ! output type structure
  TYPE NML_FIELD_T
    CHARACTER(1024)             :: LIST
  END TYPE NML_FIELD_T
  !
  TYPE NML_POINT_T
    CHARACTER(64)               :: FILE
  END TYPE NML_POINT_T
  !
  TYPE NML_TRACK_T
    LOGICAL                     :: FORMAT
  END TYPE NML_TRACK_T
  !
  TYPE NML_PARTITION_T
    INTEGER                     :: X0
    INTEGER                     :: XN
    INTEGER                     :: NX
    INTEGER                     :: Y0
    INTEGER                     :: YN
    INTEGER                     :: NY
    LOGICAL                     :: FORMAT
  END TYPE NML_PARTITION_T
  !
#ifdef W3_COU
  TYPE NML_COUPLING_T
    CHARACTER(1024)             :: SENT
    CHARACTER(1024)             :: RECEIVED
    LOGICAL                     :: COUPLET0
  END TYPE NML_COUPLING_T
#endif
  !
  TYPE NML_RESTART_T
    CHARACTER(1024)             :: EXTRA
  END TYPE NML_RESTART_T
  !
  TYPE NML_OUTPUT_TYPE_T
    TYPE(NML_POINT_T)               :: POINT
    TYPE(NML_FIELD_T)               :: FIELD
    TYPE(NML_TRACK_T)               :: TRACK
    TYPE(NML_PARTITION_T)           :: PARTITION
#ifdef W3_COU
    TYPE(NML_COUPLING_T)           :: COUPLING
#endif
    TYPE(NML_RESTART_T)             :: RESTART
  END TYPE NML_OUTPUT_TYPE_T



  ! output date structure
  TYPE NML_OUTPUT_TIME_T
    CHARACTER(15)               :: START
    CHARACTER(15)               :: STRIDE
    CHARACTER(15)               :: STOP
    CHARACTER(15)               :: OUTFFILE
  END TYPE NML_OUTPUT_TIME_T
  !
  TYPE NML_OUTPUT_DATE_T
    TYPE(NML_OUTPUT_TIME_T)         :: FIELD
    TYPE(NML_OUTPUT_TIME_T)         :: POINT
    TYPE(NML_OUTPUT_TIME_T)         :: TRACK
    TYPE(NML_OUTPUT_TIME_T)         :: RESTART
    TYPE(NML_OUTPUT_TIME_T)         :: RESTART2
    TYPE(NML_OUTPUT_TIME_T)         :: BOUNDARY
    TYPE(NML_OUTPUT_TIME_T)         :: PARTITION
    TYPE(NML_OUTPUT_TIME_T)         :: COUPLING
  END TYPE NML_OUTPUT_DATE_T


  ! homogeneous input structure
  TYPE NML_HOMOG_COUNT_T
    INTEGER                     :: N_IC1
    INTEGER                     :: N_IC2
    INTEGER                     :: N_IC3
    INTEGER                     :: N_IC4
    INTEGER                     :: N_IC5
    INTEGER                     :: N_MDN
    INTEGER                     :: N_MTH
    INTEGER                     :: N_MVS
    INTEGER                     :: N_LEV
    INTEGER                     :: N_CUR
    INTEGER                     :: N_WND
    INTEGER                     :: N_ICE
    INTEGER                     :: N_TAU
    INTEGER                     :: N_RHO
    INTEGER                     :: N_MOV
    INTEGER                     :: N_TOT
  END TYPE NML_HOMOG_COUNT_T
  !
  TYPE NML_HOMOG_INPUT_T
    CHARACTER(15)               :: NAME
    CHARACTER(15)               :: DATE
    REAL                        :: VALUE1
    REAL                        :: VALUE2
    REAL                        :: VALUE3
  END TYPE NML_HOMOG_INPUT_T


  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN



CONTAINS

  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLSHEL (MPI_COMM, NDSI, INFILE, NML_DOMAIN,            &
       NML_INPUT, NML_OUTPUT_TYPE, NML_OUTPUT_DATE,   &
       NML_HOMOG_COUNT, NML_HOMOG_INPUT, IERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         18-Apr-2018 |
    !/                  +-----------------------------------+
    !/

    !  1. Purpose :
    !
    !     Reads all the namelist to define the single grid
    !
    !  2. Method :
    !
    !     See source term routines.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !      MPI_COMM  Int.  Public   Communicator used in the wave MODEL.
    !      NDSI
    !      INFILE
    !      NML_DOMAIN
    !      NML_INPUT
    !      NML_OUTPUT_TYPE
    !      NML_OUTPUT_DATE
    !      NML_HOMOG_COUNT
    !      NML_HOMOG_INPUT
    !      IERR
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !      READ_DOMAIN_NML
    !      REPORT_DOMAIN_NML
    !      READ_INPUT_NML
    !      REPORT_INPUT_NML
    !      READ_OUTPUT_TYPE_NML
    !      REPORT_OUTPUT_TYPE_NML
    !      READ_OUTPUT_DATE_NML
    !      REPORT_OUTPUT_DATE_NML
    !      READ_HOMOGENEOUS_NML
    !      REPORT_HOMOGENEOUS_NML
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_SHEL  Prog.   N/A    Single grid main program.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE WMMDATMD, ONLY: MDSE, IMPROC, NMPLOG
#ifdef W3_MPI
    USE WMMDATMD, ONLY: MPI_COMM_MWAVE
#endif
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                       :: MPI_COMM, NDSI
    CHARACTER*(*), INTENT(IN)                 :: INFILE
    TYPE(NML_DOMAIN_T), INTENT(INOUT)         :: NML_DOMAIN
    TYPE(NML_INPUT_T), INTENT(INOUT)          :: NML_INPUT
    TYPE(NML_OUTPUT_TYPE_T), INTENT(INOUT)    :: NML_OUTPUT_TYPE
    TYPE(NML_OUTPUT_DATE_T), INTENT(INOUT)    :: NML_OUTPUT_DATE
    TYPE(NML_HOMOG_COUNT_T), INTENT(INOUT)   :: NML_HOMOG_COUNT
    TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE, INTENT(INOUT)    :: NML_HOMOG_INPUT(:)
    INTEGER, INTENT(OUT)                      :: IERR

    ! locals
#ifdef W3_MPI
    INTEGER                                 :: IERR_MPI
#endif
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'W3NMLSHEL')
#endif

#ifdef W3_MPI
    MPI_COMM_MWAVE = MPI_COMM
    CALL MPI_COMM_RANK ( MPI_COMM_MWAVE, IMPROC, IERR_MPI )
    IMPROC = IMPROC + 1
#endif

    ! open namelist log file
    IF ( NMPLOG .EQ. IMPROC ) THEN
      NDSN = 3
      OPEN (NDSN, file=TRIM(INFILE)//'.log', form='formatted', iostat=IERR)
      IF (IERR.NE.0) THEN
        WRITE (MDSE,'(A)') 'ERROR: open full nml file '//TRIM(INFILE)//'.log failed'
        RETURN
      END IF
    END IF

    ! open input file
    open (NDSI, FILE=TRIM(INFILE), form='formatted', status='old', iostat=IERR)
    IF (IERR.NE.0) THEN
      WRITE (MDSE,'(A)') 'ERROR: open input file '//TRIM(INFILE)//' failed'
      RETURN
    END IF

    ! read domain namelist
    CALL READ_DOMAIN_NML (NDSI, NML_DOMAIN)
    IF ( IMPROC .EQ. NMPLOG ) CALL REPORT_DOMAIN_NML (NML_DOMAIN)

    ! read input namelist
    CALL READ_input_NML (NDSI, NML_INPUT)
    IF ( IMPROC .EQ. NMPLOG ) CALL REPORT_INPUT_NML (NML_INPUT)

    ! read output type namelist
    CALL READ_OUTPUT_TYPE_NML (NDSI, NML_OUTPUT_TYPE)
    IF ( IMPROC .EQ. NMPLOG ) CALL REPORT_OUTPUT_TYPE_NML (NML_OUTPUT_TYPE)

    ! read output date namelist
    CALL READ_OUTPUT_DATE_NML (NDSI, NML_OUTPUT_DATE)
    IF ( IMPROC .EQ. NMPLOG ) CALL REPORT_OUTPUT_DATE_NML (NML_OUTPUT_DATE)

    ! read homogeneous namelist
    CALL READ_HOMOGENEOUS_NML (NDSI, NML_HOMOG_COUNT, NML_HOMOG_INPUT)
    IF ( IMPROC .EQ. NMPLOG ) CALL REPORT_HOMOGENEOUS_NML (NML_HOMOG_COUNT, NML_HOMOG_INPUT)

    ! close namelist files
    CLOSE (NDSI)
    IF ( NMPLOG .EQ. IMPROC ) CLOSE (NDSN)

  END SUBROUTINE W3NMLSHEL

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_DOMAIN_NML (NDSI, NML_DOMAIN)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         18-Apr-2018 |
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
    !      NDSI              Int.
    !      NML_DOMAIN        Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE WMMDATMD, ONLY: MDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)               :: NDSI
    TYPE(NML_DOMAIN_T), INTENT(OUT)   :: NML_DOMAIN

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_DOMAIN_T) :: DOMAIN
    NAMELIST /DOMAIN_NML/ DOMAIN
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_DOMAIN_NML')
#endif

    ! set default values for domain structure
    DOMAIN%IOSTYP = 1
    DOMAIN%START  = '19680606 000000'
    DOMAIN%STOP   = '19680607 000000'

    ! read domain namelist
    REWIND (NDSI)
    READ (NDSI, nml=DOMAIN_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (MDSE,'(A,/A)') &
           'ERROR: READ_DOMAIN_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! set/check return values
    IF (DOMAIN%IOSTYP.LT.0.OR.DOMAIN%IOSTYP.GT.3) THEN
      WRITE (MDSE,'(A,I3)') 'ERROR: BAD IOSTYP INPUT: ',DOMAIN%IOSTYP
      CALL EXTCDE (2)
    END IF

    ! save namelist
    NML_DOMAIN = DOMAIN

  END SUBROUTINE READ_DOMAIN_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /


  SUBROUTINE READ_INPUT_NML (NDSI, NML_INPUT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
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
    !      NDSI              Int.
    !      NML_INPUT         Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE WMMDATMD, ONLY: MDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                :: NDSI
    TYPE(NML_INPUT_T), INTENT(INOUT)   :: NML_INPUT

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_INPUT_T) :: INPUT
    NAMELIST /INPUT_NML/ INPUT
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_INPUT_NML')
#endif


    ! set default values for input structure
    INPUT%FORCING%WATER_LEVELS  = 'F'
    INPUT%FORCING%CURRENTS      = 'F'
    INPUT%FORCING%WINDS         = 'F'
    INPUT%FORCING%ATM_MOMENTUM  = 'F'
    INPUT%FORCING%AIR_DENSITY   = 'F'
    INPUT%FORCING%ICE_CONC      = 'F'
    INPUT%FORCING%ICE_PARAM1    = 'F'
    INPUT%FORCING%ICE_PARAM2    = 'F'
    INPUT%FORCING%ICE_PARAM3    = 'F'
    INPUT%FORCING%ICE_PARAM4    = 'F'
    INPUT%FORCING%ICE_PARAM5    = 'F'
    INPUT%FORCING%MUD_DENSITY   = 'F'
    INPUT%FORCING%MUD_THICKNESS = 'F'
    INPUT%FORCING%MUD_VISCOSITY = 'F'
    INPUT%ASSIM%MEAN            = 'F'
    INPUT%ASSIM%SPEC1D          = 'F'
    INPUT%ASSIM%SPEC2D          = 'F'

    ! read input namelist
    REWIND (NDSI)
    READ (NDSI, nml=INPUT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (MDSE,'(A,/A)') &
           'ERROR: READ_INPUT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (3)
    END IF

    ! save namelist
    NML_INPUT = INPUT

  END SUBROUTINE READ_INPUT_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_OUTPUT_TYPE_NML (NDSI, NML_OUTPUT_TYPE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         25-Sep-2020 |
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
    !      NDSI              Int.
    !      NML_OUTPUT_TYPE   Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE WMMDATMD, ONLY: MDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                    :: NDSI
    TYPE(NML_OUTPUT_TYPE_T), INTENT(INOUT) :: NML_OUTPUT_TYPE

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_OUTPUT_TYPE_T) :: TYPE
    NAMELIST /OUTPUT_TYPE_NML/ TYPE
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_OUTPUT_TYPE_NML')
#endif

    ! set default values for output type structure
    TYPE%FIELD%LIST   = 'unset'
    TYPE%POINT%FILE   = 'points.list'
    TYPE%TRACK%FORMAT = .TRUE.
    TYPE%PARTITION%X0 = 0
    TYPE%PARTITION%XN = 0
    TYPE%PARTITION%NX = 0
    TYPE%PARTITION%Y0 = 0
    TYPE%PARTITION%YN = 0
    TYPE%PARTITION%NY = 0
    TYPE%PARTITION%FORMAT = .TRUE.
#ifdef W3_COU
    TYPE%COUPLING%SENT      = 'unset'
    TYPE%COUPLING%RECEIVED  = 'unset'
    TYPE%COUPLING%COUPLET0  = .FALSE.
#endif
    TYPE%RESTART%EXTRA = 'unset'


    ! read output type namelist
    REWIND (NDSI)
    READ (NDSI, nml=OUTPUT_TYPE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (MDSE,'(A,/A)') &
           'ERROR: READ_OUTPUT_TYPE_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (4)
    END IF

    ! save namelist
    NML_OUTPUT_TYPE = TYPE

  END SUBROUTINE READ_OUTPUT_TYPE_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_OUTPUT_DATE_NML (NDSI, NML_OUTPUT_DATE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         18-Apr-2018 |
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
    !      NDSI              Int.
    !      NML_OUTPUT_DATE   Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE WMMDATMD, ONLY: MDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                    :: NDSI
    TYPE(NML_OUTPUT_DATE_T), INTENT(INOUT) :: NML_OUTPUT_DATE

    ! locals
    INTEGER                                :: IERR
    TYPE(NML_OUTPUT_DATE_T) :: DATE
    NAMELIST /OUTPUT_DATE_NML/ DATE
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_OUTPUT_DATE_NML')
#endif

    ! set default values for output_date structure
    DATE%FIELD%START = '19680606 000000'
    DATE%FIELD%STRIDE = '0'
    DATE%FIELD%STOP = '19680607 000000'
    DATE%FIELD%OUTFFILE = '0'
    DATE%POINT%OUTFFILE = '0'
    DATE%POINT%START = '19680606 000000'
    DATE%POINT%STRIDE = '0'
    DATE%POINT%STOP = '19680607 000000'
    DATE%TRACK%START = '19680606 000000'
    DATE%TRACK%STRIDE = '0'
    DATE%TRACK%STOP = '19680607 000000'
    DATE%RESTART%START = '19680606 000000'
    DATE%RESTART%STRIDE = '0'
    DATE%RESTART%STOP = '19680607 000000'
    DATE%RESTART2%START = '19680606 000000'
    DATE%RESTART2%STRIDE = '0'
    DATE%RESTART2%STOP = '19680607 000000'
    DATE%BOUNDARY%START = '19680606 000000'
    DATE%BOUNDARY%STRIDE = '0'
    DATE%BOUNDARY%STOP = '19680607 000000'
    DATE%PARTITION%START = '19680606 000000'
    DATE%PARTITION%STRIDE = '0'
    DATE%PARTITION%STOP = '19680607 000000'
    DATE%COUPLING%START = '19680606 000000'
    DATE%COUPLING%STRIDE = '0'
    DATE%COUPLING%STOP = '19680607 000000'


    ! read output date namelist
    REWIND (NDSI)
    READ (NDSI, nml=OUTPUT_DATE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (MDSE,'(A,/A)') &
           'ERROR: READ_OUTPUT_DATE_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (5)
    END IF

    ! save namelist
    NML_OUTPUT_DATE = DATE

  END SUBROUTINE READ_OUTPUT_DATE_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_HOMOGENEOUS_NML (NDSI, NML_HOMOG_COUNT, NML_HOMOG_INPUT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
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
    !      NDSI              Int.
    !      NML_HOMOG_COUNT   Type.
    !      NML_HOMOG_INPUT   Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE WMMDATMD, ONLY: MDSE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    INTEGER, INTENT(IN)                    :: NDSI
    TYPE(NML_HOMOG_COUNT_T), INTENT(OUT)   :: NML_HOMOG_COUNT
    TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE, INTENT(OUT)   :: NML_HOMOG_INPUT(:)

    ! locals
    INTEGER                   :: IERR, I
    TYPE(NML_HOMOG_COUNT_T)  :: HOMOG_COUNT
    NAMELIST /HOMOG_COUNT_NML/  HOMOG_COUNT
    TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE   :: HOMOG_INPUT(:)
    NAMELIST /HOMOG_INPUT_NML/   HOMOG_INPUT
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_HOMOGENEOUS_NML')
#endif

    ! set default values for homogeneous number structure
    HOMOG_COUNT%N_IC1   = 0
    HOMOG_COUNT%N_IC2   = 0
    HOMOG_COUNT%N_IC3   = 0
    HOMOG_COUNT%N_IC4   = 0
    HOMOG_COUNT%N_IC5   = 0
    HOMOG_COUNT%N_MDN   = 0
    HOMOG_COUNT%N_MTH   = 0
    HOMOG_COUNT%N_MVS   = 0
    HOMOG_COUNT%N_LEV   = 0
    HOMOG_COUNT%N_CUR   = 0
    HOMOG_COUNT%N_WND   = 0
    HOMOG_COUNT%N_ICE   = 0
    HOMOG_COUNT%N_TAU   = 0
    HOMOG_COUNT%N_RHO   = 0
    HOMOG_COUNT%N_MOV   = 0
    HOMOG_COUNT%N_TOT   = 0


    ! read homogeneous count namelist
    REWIND (NDSI)
    READ (NDSI, nml=HOMOG_COUNT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (MDSE,'(A,/A)') &
           'ERROR: READ_HOMOGENEOUS_NML: namelist HOMOG_COUNT read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (6)
    END IF

    ! allocate the total count of homogeneous input
    HOMOG_COUNT%N_TOT = HOMOG_COUNT%N_IC1 + HOMOG_COUNT%N_IC2 + HOMOG_COUNT%N_IC3 + HOMOG_COUNT%N_IC4 + HOMOG_COUNT%N_IC5 + &
         HOMOG_COUNT%N_MDN + HOMOG_COUNT%N_MTH + HOMOG_COUNT%N_MVS + HOMOG_COUNT%N_LEV + HOMOG_COUNT%N_CUR + &
         HOMOG_COUNT%N_WND + HOMOG_COUNT%N_ICE + HOMOG_COUNT%N_TAU + HOMOG_COUNT%N_RHO + HOMOG_COUNT%N_MOV
    ALLOCATE(HOMOG_INPUT(HOMOG_COUNT%N_TOT))
    ALLOCATE(NML_HOMOG_INPUT(HOMOG_COUNT%N_TOT))

    ! set default values for homogeneous structure
    IF (HOMOG_COUNT%N_TOT .NE. 0 ) THEN
      DO I=1,HOMOG_COUNT%N_TOT
        HOMOG_INPUT(I)%NAME      = 'unset'
        HOMOG_INPUT(I)%DATE      = '19680606 000000'
        HOMOG_INPUT(I)%VALUE1    = 0.
        HOMOG_INPUT(I)%VALUE2    = 0.
        HOMOG_INPUT(I)%VALUE3    = 0.
      END DO
    END IF

    ! read homogeneous input namelist
    REWIND (NDSI)
    READ (NDSI, nml=HOMOG_INPUT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (MDSE,'(A,/A)') &
           'ERROR: READ_HOMOGENEOUS_NML: namelist HOMOG_INPUT_NML read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (7)
    END IF

    ! save namelist
    NML_HOMOG_COUNT = HOMOG_COUNT
    NML_HOMOG_INPUT = HOMOG_INPUT


  END SUBROUTINE READ_HOMOGENEOUS_NML

  !/ ------------------------------------------------------------------- /








  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_DOMAIN_NML (NML_DOMAIN)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         18-Apr-2018 |
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
    !      NML_DOMAIN      Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_DOMAIN_T), INTENT(IN) :: NML_DOMAIN
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_DOMAIN_NML')
#endif

    WRITE (MSG,'(A)') 'DOMAIN % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'IOSTYP = ', NML_DOMAIN%IOSTYP
    WRITE (NDSN,10) TRIM(MSG),'START  = ', TRIM(NML_DOMAIN%START)
    WRITE (NDSN,10) TRIM(MSG),'STOP   = ', TRIM(NML_DOMAIN%STOP)

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)

  END SUBROUTINE REPORT_DOMAIN_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_INPUT_NML (NML_INPUT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
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
    !      NML_INPUT         Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_INPUT_T), INTENT(IN) :: NML_INPUT

    ! locals
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_INPUT_NML')
#endif

    WRITE (MSG,'(A)') 'INPUT GRID % :'
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'FORCING % WATER_LEVELS   = ', NML_INPUT%FORCING%WATER_LEVELS
    WRITE (NDSN,10) TRIM(MSG),'FORCING % CURRENTS       = ', NML_INPUT%FORCING%CURRENTS
    WRITE (NDSN,10) TRIM(MSG),'FORCING % WINDS          = ', NML_INPUT%FORCING%WINDS
    WRITE (NDSN,10) TRIM(MSG),'FORCING % ATM_MOMENTUM   = ', NML_INPUT%FORCING%ATM_MOMENTUM
    WRITE (NDSN,10) TRIM(MSG),'FORCING % AIR_DENSITY    = ', NML_INPUT%FORCING%AIR_DENSITY
    WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_CONC       = ', NML_INPUT%FORCING%ICE_CONC
    WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM1     = ', NML_INPUT%FORCING%ICE_PARAM1
    WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM2     = ', NML_INPUT%FORCING%ICE_PARAM2
    WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM3     = ', NML_INPUT%FORCING%ICE_PARAM3
    WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM4     = ', NML_INPUT%FORCING%ICE_PARAM4
    WRITE (NDSN,10) TRIM(MSG),'FORCING % ICE_PARAM5     = ', NML_INPUT%FORCING%ICE_PARAM5
    WRITE (NDSN,10) TRIM(MSG),'FORCING % MUD_DENSITY    = ', NML_INPUT%FORCING%MUD_DENSITY
    WRITE (NDSN,10) TRIM(MSG),'FORCING % MUD_THICKNESS  = ', NML_INPUT%FORCING%MUD_THICKNESS
    WRITE (NDSN,10) TRIM(MSG),'FORCING % MUD_VISCOSITY  = ', NML_INPUT%FORCING%MUD_VISCOSITY
    WRITE (NDSN,10) TRIM(MSG),'ASSIM % MEAN             = ', NML_INPUT%ASSIM%MEAN
    WRITE (NDSN,10) TRIM(MSG),'ASSIM % SPEC1D           = ', NML_INPUT%ASSIM%SPEC1D
    WRITE (NDSN,10) TRIM(MSG),'ASSIM % SPEC2D           = ', NML_INPUT%ASSIM%SPEC2D


10  FORMAT (A,2X,A,A)
13  FORMAT (A,2X,A,L1)

  END SUBROUTINE REPORT_INPUT_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_OUTPUT_TYPE_NML (NML_OUTPUT_TYPE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         25-Sep-2020 |
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
    !      NML_OUTPUT_TYPE   Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_OUTPUT_TYPE_T), INTENT(IN) :: NML_OUTPUT_TYPE

    ! locals
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_OUTPUT_TYPE_NML')
#endif

    WRITE (MSG,'(A)') 'OUTPUT TYPE % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'FIELD % LIST         = ', TRIM(NML_OUTPUT_TYPE%FIELD%LIST)
    WRITE (NDSN,10) TRIM(MSG),'POINT % FILE         = ', TRIM(NML_OUTPUT_TYPE%POINT%FILE)
    WRITE (NDSN,13) TRIM(MSG),'TRACK % FORMAT       = ', NML_OUTPUT_TYPE%TRACK%FORMAT
    WRITE (NDSN,11) TRIM(MSG),'PARTITION % X0       = ', NML_OUTPUT_TYPE%PARTITION%X0
    WRITE (NDSN,11) TRIM(MSG),'PARTITION % XN       = ', NML_OUTPUT_TYPE%PARTITION%XN
    WRITE (NDSN,11) TRIM(MSG),'PARTITION % NX       = ', NML_OUTPUT_TYPE%PARTITION%NX
    WRITE (NDSN,11) TRIM(MSG),'PARTITION % Y0       = ', NML_OUTPUT_TYPE%PARTITION%Y0
    WRITE (NDSN,11) TRIM(MSG),'PARTITION % YN       = ', NML_OUTPUT_TYPE%PARTITION%YN
    WRITE (NDSN,11) TRIM(MSG),'PARTITION % NY       = ', NML_OUTPUT_TYPE%PARTITION%NY
    WRITE (NDSN,13) TRIM(MSG),'PARTITION % FORMAT   = ', NML_OUTPUT_TYPE%PARTITION%FORMAT
#ifdef W3_COU
    WRITE (NDSN,10) TRIM(MSG),'COUPLING % SENT         = ', TRIM(NML_OUTPUT_TYPE%COUPLING%SENT)
    WRITE (NDSN,10) TRIM(MSG),'COUPLING % RECEIVED     = ', TRIM(NML_OUTPUT_TYPE%COUPLING%RECEIVED)
    WRITE (NDSN,13) TRIM(MSG),'COUPLING % COUPLET0     = ', NML_OUTPUT_TYPE%COUPLING%COUPLET0
#endif
    WRITE (NDSN,10) TRIM(MSG),'RESTART % EXTRA      = ', TRIM(NML_OUTPUT_TYPE%RESTART%EXTRA)

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)

  END SUBROUTINE REPORT_OUTPUT_TYPE_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_OUTPUT_DATE_NML (NML_OUTPUT_DATE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         18-Apr-2018 |
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
    !      NML_OUTPUT_DATE   Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_OUTPUT_DATE_T), INTENT(IN) :: NML_OUTPUT_DATE

    ! locals
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_OUTPUT_DATE_NML')
#endif

    WRITE (MSG,'(A)') 'OUTPUT DATE MODEL GRID % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'FIELD % START        = ', TRIM(NML_OUTPUT_DATE%FIELD%START)
    WRITE (NDSN,10) TRIM(MSG),'FIELD % STRIDE       = ', TRIM(NML_OUTPUT_DATE%FIELD%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'FIELD % STOP         = ', TRIM(NML_OUTPUT_DATE%FIELD%STOP)
    WRITE (NDSN,10) TRIM(MSG),'POINT % START        = ', TRIM(NML_OUTPUT_DATE%POINT%START)
    WRITE (NDSN,10) TRIM(MSG),'POINT % STRIDE       = ', TRIM(NML_OUTPUT_DATE%POINT%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'POINT % STOP         = ', TRIM(NML_OUTPUT_DATE%POINT%STOP)
    WRITE (NDSN,10) TRIM(MSG),'TRACK % START        = ', TRIM(NML_OUTPUT_DATE%TRACK%START)
    WRITE (NDSN,10) TRIM(MSG),'TRACK % STRIDE       = ', TRIM(NML_OUTPUT_DATE%TRACK%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'TRACK % STOP         = ', TRIM(NML_OUTPUT_DATE%TRACK%STOP)
    WRITE (NDSN,10) TRIM(MSG),'RESTART % START      = ', TRIM(NML_OUTPUT_DATE%RESTART%START)
    WRITE (NDSN,10) TRIM(MSG),'RESTART % STRIDE     = ', TRIM(NML_OUTPUT_DATE%RESTART%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'RESTART % STOP       = ', TRIM(NML_OUTPUT_DATE%RESTART%STOP)
    WRITE (NDSN,10) TRIM(MSG),'RESTART2 % START      = ', TRIM(NML_OUTPUT_DATE%RESTART2%START)
    WRITE (NDSN,10) TRIM(MSG),'RESTART2 % STRIDE     = ', TRIM(NML_OUTPUT_DATE%RESTART2%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'RESTART2 % STOP       = ', TRIM(NML_OUTPUT_DATE%RESTART2%STOP)
    WRITE (NDSN,10) TRIM(MSG),'BOUNDARY % START     = ', TRIM(NML_OUTPUT_DATE%BOUNDARY%START)
    WRITE (NDSN,10) TRIM(MSG),'BOUNDARY % STRIDE    = ', TRIM(NML_OUTPUT_DATE%BOUNDARY%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'BOUNDARY % STOP      = ', TRIM(NML_OUTPUT_DATE%BOUNDARY%STOP)
    WRITE (NDSN,10) TRIM(MSG),'PARTITION % START    = ', TRIM(NML_OUTPUT_DATE%PARTITION%START)
    WRITE (NDSN,10) TRIM(MSG),'PARTITION % STRIDE   = ', TRIM(NML_OUTPUT_DATE%PARTITION%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'PARTITION % STOP     = ', TRIM(NML_OUTPUT_DATE%PARTITION%STOP)
#ifdef W3_COU
    WRITE (NDSN,10) TRIM(MSG),'COUPLING % START    = ', TRIM(NML_OUTPUT_DATE%COUPLING%START)
    WRITE (NDSN,10) TRIM(MSG),'COUPLING % STRIDE   = ', TRIM(NML_OUTPUT_DATE%COUPLING%STRIDE)
    WRITE (NDSN,10) TRIM(MSG),'COUPLING % STOP     = ', TRIM(NML_OUTPUT_DATE%COUPLING%STOP)
#endif


10  FORMAT (A,2X,A,A)

  END SUBROUTINE REPORT_OUTPUT_DATE_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_HOMOGENEOUS_NML (NML_HOMOG_COUNT, NML_HOMOG_INPUT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           M. Accensi              |
    !/                  |                                   |
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
    !      NML_HOMOG_COUNT   Type.
    !      NML_HOMOG_INPUT   Type.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3NMLSHEL Subr.   N/A    Namelist configuration routine.
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
    !     !/MPI  Uses MPI communications
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE

    TYPE(NML_HOMOG_COUNT_T), INTENT(IN)  :: NML_HOMOG_COUNT
    TYPE(NML_HOMOG_INPUT_T), INTENT(IN) :: NML_HOMOG_INPUT(NML_HOMOG_COUNT%N_TOT)

    ! locals
    INTEGER              :: I
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_HOMOGENEOUS_NML')
#endif

    WRITE (MSG,'(A)') 'HOMOG_COUNT % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'N_IC1       = ', NML_HOMOG_COUNT%N_IC1
    WRITE (NDSN,11) TRIM(MSG),'N_IC2       = ', NML_HOMOG_COUNT%N_IC2
    WRITE (NDSN,11) TRIM(MSG),'N_IC3       = ', NML_HOMOG_COUNT%N_IC3
    WRITE (NDSN,11) TRIM(MSG),'N_IC4       = ', NML_HOMOG_COUNT%N_IC4
    WRITE (NDSN,11) TRIM(MSG),'N_IC5       = ', NML_HOMOG_COUNT%N_IC5
    WRITE (NDSN,11) TRIM(MSG),'N_MDN       = ', NML_HOMOG_COUNT%N_MDN
    WRITE (NDSN,11) TRIM(MSG),'N_MTH       = ', NML_HOMOG_COUNT%N_MTH
    WRITE (NDSN,11) TRIM(MSG),'N_MVS       = ', NML_HOMOG_COUNT%N_MVS
    WRITE (NDSN,11) TRIM(MSG),'N_LEV       = ', NML_HOMOG_COUNT%N_LEV
    WRITE (NDSN,11) TRIM(MSG),'N_CUR       = ', NML_HOMOG_COUNT%N_CUR
    WRITE (NDSN,11) TRIM(MSG),'N_WND       = ', NML_HOMOG_COUNT%N_WND
    WRITE (NDSN,11) TRIM(MSG),'N_ICE       = ', NML_HOMOG_COUNT%N_ICE
    WRITE (NDSN,11) TRIM(MSG),'N_TAU       = ', NML_HOMOG_COUNT%N_TAU
    WRITE (NDSN,11) TRIM(MSG),'N_RHO       = ', NML_HOMOG_COUNT%N_RHO
    WRITE (NDSN,11) TRIM(MSG),'N_MOV       = ', NML_HOMOG_COUNT%N_MOV

    IF (NML_HOMOG_COUNT%N_TOT .NE. 0) THEN
      DO I=1,NML_HOMOG_COUNT%N_TOT
        WRITE (MSG,'(A,I5,A)') 'HOMOG_INPUT(',I,') % '
        WRITE (NDSN,'(A)')
        WRITE (NDSN,10) TRIM(MSG),'NAME      = ', TRIM(NML_HOMOG_INPUT(I)%NAME)
        WRITE (NDSN,10) TRIM(MSG),'DATE      = ', TRIM(NML_HOMOG_INPUT(I)%DATE)
        WRITE (NDSN,14) TRIM(MSG),'VALUE1    = ', NML_HOMOG_INPUT(I)%VALUE1
        WRITE (NDSN,14) TRIM(MSG),'VALUE2    = ', NML_HOMOG_INPUT(I)%VALUE2
        WRITE (NDSN,14) TRIM(MSG),'VALUE3    = ', NML_HOMOG_INPUT(I)%VALUE3
        WRITE (NDSN,'(A)')
      END DO
    END IF


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_HOMOGENEOUS_NML

  !/ ------------------------------------------------------------------- /





END MODULE W3NMLSHELMD

!/ ------------------------------------------------------------------- /
