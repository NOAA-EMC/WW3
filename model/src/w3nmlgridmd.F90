#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3NMLGRIDMD
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
  !     Manages namelists from configuration file ww3_grid.nml for ww3_grid program
  !
  !/ ------------------------------------------------------------------- /

  ! module defaults
  IMPLICIT NONE

  PUBLIC

  ! coord file structure
  TYPE NML_COORD_FILE_T
    REAL                        :: SF
    REAL                        :: OFF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: FROM
  END TYPE NML_COORD_FILE_T

  ! file structure
  TYPE NML_FILE_T
    REAL                        :: SF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: FROM
  END TYPE NML_FILE_T

  ! smc file structure
  TYPE NML_SMC_FILE_T
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
  END TYPE NML_SMC_FILE_T


  ! spectrum structure
  TYPE NML_SPECTRUM_T
    REAL                        :: XFR
    REAL                        :: FREQ1
    INTEGER                     :: NK
    INTEGER                     :: NTH
    REAL                        :: THOFF
  END TYPE NML_SPECTRUM_T

  ! run structure
  TYPE NML_RUN_T
    LOGICAL                     :: FLDRY
    LOGICAL                     :: FLCX
    LOGICAL                     :: FLCY
    LOGICAL                     :: FLCTH
    LOGICAL                     :: FLCK
    LOGICAL                     :: FLSOU
  END TYPE NML_RUN_T

  ! timesteps structure
  TYPE NML_TIMESTEPS_T
    REAL                        :: DTMAX
    REAL                        :: DTXY
    REAL                        :: DTKTH
    REAL                        :: DTMIN
  END TYPE NML_TIMESTEPS_T

  ! grid structure
  TYPE NML_GRID_T
    CHARACTER(256)              :: NAME
    CHARACTER(256)              :: NML
    CHARACTER(256)              :: TYPE
    CHARACTER(256)              :: COORD
    CHARACTER(256)              :: CLOS
    REAL                        :: ZLIM
    REAL                        :: DMIN
  END TYPE NML_GRID_T

  ! rect structure
  TYPE NML_RECT_T
    INTEGER                     :: NX
    INTEGER                     :: NY
    REAL                        :: SX
    REAL                        :: SY
    REAL                        :: SF
    REAL                        :: X0
    REAL                        :: Y0
    REAL                        :: SF0
  END TYPE NML_RECT_T

  ! curv structure
  TYPE NML_CURV_T
    INTEGER                     :: NX
    INTEGER                     :: NY
    TYPE(NML_COORD_FILE_T)      :: XCOORD
    TYPE(NML_COORD_FILE_T)      :: YCOORD
  END TYPE NML_CURV_T

  ! unst structure
  TYPE NML_UNST_T
    REAL                        :: SF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: UGOBCFILE
  END TYPE NML_UNST_T

  ! smc structure
  TYPE NML_SMC_T
    TYPE(NML_SMC_FILE_T)        :: MCELS
    TYPE(NML_SMC_FILE_T)        :: ISIDE
    TYPE(NML_SMC_FILE_T)        :: JSIDE
    TYPE(NML_SMC_FILE_T)        :: SUBTR
    TYPE(NML_SMC_FILE_T)        :: BUNDY
    TYPE(NML_SMC_FILE_T)        :: MBARC
    TYPE(NML_SMC_FILE_T)        :: AISID
    TYPE(NML_SMC_FILE_T)        :: AJSID
  END TYPE NML_SMC_T

  ! depth structure
  TYPE NML_DEPTH_T
    REAL                        :: SF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: FROM
  END TYPE NML_DEPTH_T

  ! mask structure
  TYPE NML_MASK_T
    REAL                        :: SF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: FROM
  END TYPE NML_MASK_T

  ! obst structure
  TYPE NML_OBST_T
    REAL                        :: SF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: FROM
  END TYPE NML_OBST_T

  ! slope structure
  TYPE NML_SLOPE_T
    REAL                        :: SF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: FROM
  END TYPE NML_SLOPE_T

  ! sed structure
  TYPE NML_SED_T
    REAL                        :: SF
    CHARACTER(256)              :: FILENAME
    INTEGER                     :: IDF
    INTEGER                     :: IDLA
    INTEGER                     :: IDFM
    CHARACTER(256)              :: FORMAT
    CHARACTER(256)              :: FROM
  END TYPE NML_SED_T

  ! inbound structure
  TYPE NML_INBND_COUNT_T
    INTEGER                     :: N_POINT
  END TYPE NML_INBND_COUNT_T

  TYPE NML_INBND_POINT_T
    INTEGER                     :: X_INDEX
    INTEGER                     :: Y_INDEX
    LOGICAL                     :: CONNECT
  END TYPE NML_INBND_POINT_T


  ! excluded structure
  TYPE NML_EXCL_COUNT_T
    INTEGER                     :: N_POINT
    INTEGER                     :: N_BODY
  END TYPE NML_EXCL_COUNT_T

  TYPE NML_EXCL_POINT_T
    INTEGER                     :: X_INDEX
    INTEGER                     :: Y_INDEX
    LOGICAL                     :: CONNECT
  END TYPE NML_EXCL_POINT_T

  TYPE NML_EXCL_BODY_T
    INTEGER                     :: X_INDEX
    INTEGER                     :: Y_INDEX
  END TYPE NML_EXCL_BODY_T


  ! outbound structure
  TYPE NML_OUTBND_COUNT_T
    INTEGER                     :: N_LINE
  END TYPE NML_OUTBND_COUNT_T

  TYPE NML_OUTBND_LINE_T
    REAL                        :: X0
    REAL                        :: Y0
    REAL                        :: DX
    REAL                        :: DY
    INTEGER                     :: NP
  END TYPE NML_OUTBND_LINE_T



  ! miscellaneous
  CHARACTER(256)                :: MSG
  INTEGER                       :: NDSN



CONTAINS
  !/ ------------------------------------------------------------------- /
  SUBROUTINE W3NMLGRID (NDSI, INFILE, NML_SPECTRUM, NML_RUN,           &
       NML_TIMESTEPS, NML_GRID, NML_RECT, NML_CURV,   &
       NML_UNST, NML_SMC, NML_DEPTH, NML_MASK,        &
       NML_OBST, NML_SLOPE, NML_SED, NML_INBND_COUNT, &
       NML_INBND_POINT, NML_EXCL_COUNT,               &
       NML_EXCL_POINT, NML_EXCL_BODY,                 &
       NML_OUTBND_COUNT, NML_OUTBND_LINE, IERR)
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
    !     Reads all the namelist to define the model grid
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
    !      INFILE            Char.
    !      NML_SPECTRUM      Type
    !      NML_RUN           Type
    !      NML_TIMESTEPS     Type
    !      NML_GRID          Type
    !      NML_RECT          Type
    !      NML_CURV          Type
    !      NML_UNST          Type
    !      NML_SMC           Type
    !      NML_DEPTH         Type
    !      NML_MASK          Type
    !      NML_OBST          Type
    !      NML_SLOPE         Type
    !      NML_SED           Type
    !      NML_INBND_COUNT   Type
    !      NML_INBND_POINT   Type
    !      NML_EXCL_COUNT    Type
    !      NML_EXCL_POINT    Type
    !      NML_EXCL_BODY     Type
    !      NML_OUTBND_COUNT  Type
    !      NML_OUTBND_LINE   Type
    !      IERR              Int.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD SUBROUTINE tracing.
    !      READ_SPECTRUM_NML
    !      REPORT_SPECTRUM_NML
    !      READ_RUN_NML
    !      REPORT_RUN_NML
    !      READ_TIMESTEPS_NML
    !      REPORT_TIMESTEPS_NML
    !      READ_GRID_NML
    !      REPORT_GRID_NML
    !      READ_RECT_NML
    !      REPORT_RECT_NML
    !      READ_CURV_NML
    !      REPORT_CURV_NML
    !      READ_UNST_NML
    !      REPORT_UNST_NML
    !      READ_SMC_NML
    !      REPORT_SMC_NML
    !      READ_DEPTH_NML
    !      REPORT_DEPTH_NML
    !      READ_MASK_NML
    !      REPORT_MASK_NML
    !      READ_OBST_NML
    !      REPORT_OBST_NML
    !      READ_SLOPE_NML
    !      REPORT_SLOPE_NML
    !      READ_SED_NML
    !      REPORT_SED_NML
    !      READ_INBOUND_NML
    !      REPORT_INBOUND_NML
    !      READ_EXCLUDED_NML
    !      REPORT_EXCLUDED_NML
    !      READ_OUTBOUND_NML
    !      REPORT_OUTBOUND_NML
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      TYPE  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_GRID  Prog.   N/A    Preprocess model grid.
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
    TYPE(NML_SPECTRUM_T), INTENT(INOUT)         :: NML_SPECTRUM
    TYPE(NML_RUN_T), INTENT(INOUT)              :: NML_RUN
    TYPE(NML_TIMESTEPS_T), INTENT(INOUT)        :: NML_TIMESTEPS
    TYPE(NML_GRID_T), INTENT(INOUT)             :: NML_GRID
    TYPE(NML_RECT_T), INTENT(INOUT)             :: NML_RECT
    TYPE(NML_CURV_T), INTENT(INOUT)             :: NML_CURV
    TYPE(NML_UNST_T), INTENT(INOUT)             :: NML_UNST
    TYPE(NML_SMC_T), INTENT(INOUT)              :: NML_SMC
    TYPE(NML_DEPTH_T), INTENT(INOUT)            :: NML_DEPTH
    TYPE(NML_MASK_T), INTENT(INOUT)             :: NML_MASK
    TYPE(NML_OBST_T), INTENT(INOUT)             :: NML_OBST
    TYPE(NML_SLOPE_T), INTENT(INOUT)            :: NML_SLOPE
    TYPE(NML_SED_T), INTENT(INOUT)              :: NML_SED
    TYPE(NML_INBND_COUNT_T), INTENT(INOUT)      :: NML_INBND_COUNT
    TYPE(NML_INBND_POINT_T), ALLOCATABLE, INTENT(INOUT)      :: NML_INBND_POINT(:)
    TYPE(NML_EXCL_COUNT_T), INTENT(INOUT)       :: NML_EXCL_COUNT
    TYPE(NML_EXCL_POINT_T), ALLOCATABLE, INTENT(INOUT)       :: NML_EXCL_POINT(:)
    TYPE(NML_EXCL_BODY_T), ALLOCATABLE, INTENT(INOUT)        :: NML_EXCL_BODY(:)
    TYPE(NML_OUTBND_COUNT_T), INTENT(INOUT)     :: NML_OUTBND_COUNT
    TYPE(NML_OUTBND_LINE_T), ALLOCATABLE, INTENT(INOUT)      :: NML_OUTBND_LINE(:)
    INTEGER, INTENT(OUT)                        :: IERR
#ifdef W3_S
    INTEGER, SAVE                             :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'W3NMLGRID')
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

    ! read spectrum namelist
    CALL READ_SPECTRUM_NML (NDSI, NML_SPECTRUM)
    CALL REPORT_SPECTRUM_NML (NML_SPECTRUM)

    ! read run namelist
    CALL READ_RUN_NML (NDSI, NML_RUN)
    CALL REPORT_RUN_NML (NML_RUN)

    ! read timesteps namelist
    CALL READ_TIMESTEPS_NML (NDSI, NML_TIMESTEPS)
    CALL REPORT_TIMESTEPS_NML (NML_TIMESTEPS)

    ! read grid namelist
    CALL READ_GRID_NML (NDSI, NML_GRID)
    CALL REPORT_GRID_NML (NML_GRID)

    ! read rect namelist
    CALL READ_RECT_NML (NDSI, NML_RECT)
    CALL REPORT_RECT_NML (NML_RECT)

    ! read curv namelist
    CALL READ_CURV_NML (NDSI, NML_CURV)
    CALL REPORT_CURV_NML (NML_CURV)

    ! read unst namelist
    CALL READ_UNST_NML (NDSI, NML_UNST)
    CALL REPORT_UNST_NML (NML_UNST)

    ! read smc namelist
    CALL READ_SMC_NML (NDSI, NML_SMC)
    CALL REPORT_SMC_NML (NML_SMC)

    ! read depth namelist
    CALL READ_DEPTH_NML (NDSI, NML_DEPTH)
    CALL REPORT_DEPTH_NML (NML_DEPTH)

    ! read mask namelist
    CALL READ_MASK_NML (NDSI, NML_MASK)
    CALL REPORT_MASK_NML (NML_MASK)

    ! read obst namelist
    CALL READ_OBST_NML (NDSI, NML_OBST)
    CALL REPORT_OBST_NML (NML_OBST)

    ! read slope namelist
    CALL READ_SLOPE_NML (NDSI, NML_SLOPE)
    CALL REPORT_SLOPE_NML (NML_SLOPE)

    ! read sed namelist
    CALL READ_SED_NML (NDSI, NML_SED)
    CALL REPORT_SED_NML (NML_SED)

    ! read inbound namelist
    CALL READ_INBOUND_NML (NDSI, NML_INBND_COUNT, NML_INBND_POINT)
    CALL REPORT_INBOUND_NML (NML_INBND_COUNT, NML_INBND_POINT)

    ! read excluded namelist
    CALL READ_EXCLUDED_NML (NDSI, NML_EXCL_COUNT, NML_EXCL_POINT, NML_EXCL_BODY)
    CALL REPORT_EXCLUDED_NML (NML_EXCL_COUNT, NML_EXCL_POINT, NML_EXCL_BODY)

    ! read outbound namelist
    CALL READ_OUTBOUND_NML (NDSI, NML_OUTBND_COUNT, NML_OUTBND_LINE)
    CALL REPORT_OUTBOUND_NML (NML_OUTBND_COUNT, NML_OUTBND_LINE)

    ! close namelist files
    CLOSE (NDSI)
    CLOSE (NDSN)

  END SUBROUTINE W3NMLGRID


  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_SPECTRUM_NML (NDSI, NML_SPECTRUM)
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
    !      NDSI            Int.
    !      NML_SPECTRUM    Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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
    TYPE(NML_SPECTRUM_T), INTENT(INOUT) :: NML_SPECTRUM

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_SPECTRUM_T) :: SPECTRUM
    NAMELIST /SPECTRUM_NML/ SPECTRUM
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_SPECTRUM_NML')
#endif

    ! set default values for spectrum structure
    SPECTRUM%XFR        = 0.
    SPECTRUM%FREQ1      = 0.
    SPECTRUM%NK         = 0
    SPECTRUM%NTH        = 0
    SPECTRUM%THOFF      = 0.

    ! read spectrum namelist
    REWIND (NDSI)
    READ (NDSI, nml=SPECTRUM_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_SPECTRUM_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (1)
    END IF

    ! save namelist
    NML_SPECTRUM = SPECTRUM

  END SUBROUTINE READ_SPECTRUM_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_RUN_NML (NDSI, NML_RUN)
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
    !      NML_RUN      Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)            :: NDSI
    TYPE(NML_RUN_T), INTENT(INOUT) :: NML_RUN

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_RUN_T) :: RUN
    NAMELIST /RUN_NML/ RUN
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_RUN_NML')
#endif

    ! set default values for run structure
    RUN%FLDRY      = .FALSE.
    RUN%FLCX       = .FALSE.
    RUN%FLCY       = .FALSE.
    RUN%FLCTH      = .FALSE.
    RUN%FLCK       = .FALSE.
    RUN%FLSOU      = .FALSE.

    ! read run namelist
    REWIND (NDSI)
    READ (NDSI, nml=RUN_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_RUN_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (2)
    END IF

    ! save namelist
    NML_RUN = RUN

  END SUBROUTINE READ_RUN_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_TIMESTEPS_NML (NDSI, NML_TIMESTEPS)
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
    !      NDSI             Int.
    !      NML_TIMESTEPS    Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_TIMESTEPS_T), INTENT(INOUT) :: NML_TIMESTEPS

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_TIMESTEPS_T) :: TIMESTEPS
    NAMELIST /TIMESTEPS_NML/ TIMESTEPS
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_TIMESTEPS_NML')
#endif

    ! set default values for timesteps structure
    TIMESTEPS%DTMAX      = 0.
    TIMESTEPS%DTXY       = 0.
    TIMESTEPS%DTKTH      = 0.
    TIMESTEPS%DTMIN      = 0.

    ! read timesteps namelist
    REWIND (NDSI)
    READ (NDSI, nml=TIMESTEPS_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_TIMESTEPS_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (3)
    END IF

    ! save namelist
    NML_TIMESTEPS = TIMESTEPS

  END SUBROUTINE READ_TIMESTEPS_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_GRID_NML (NDSI, NML_GRID)
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
    !      NDSI             Int.
    !      NML_GRID         Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_GRID_T), INTENT(INOUT)      :: NML_GRID

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_GRID_T) :: GRID
    NAMELIST /GRID_NML/ GRID
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_GRID_NML')
#endif

    ! set default values for grid structure
    GRID%NAME       = 'unset'
    GRID%NML        = 'namelists.nml'
    GRID%TYPE       = 'unset'
    GRID%COORD      = 'unset'
    GRID%CLOS       = 'unset'
    GRID%ZLIM       = 0.
    GRID%DMIN       = 0.

    ! read grid namelist
    REWIND (NDSI)
    READ (NDSI, nml=GRID_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.NE.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_GRID_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (4)
    END IF

    ! save namelist
    NML_GRID = GRID

  END SUBROUTINE READ_GRID_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_RECT_NML (NDSI, NML_RECT)
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
    !      NDSI             Int.
    !      NML_RECT         Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_RECT_T), INTENT(INOUT)      :: NML_RECT

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_RECT_T) :: RECT
    NAMELIST /RECT_NML/ RECT
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_RECT_NML')
#endif

    ! set default values for rect structure
    RECT%NX         = 0
    RECT%NY         = 0
    RECT%SX         = 0.
    RECT%SY         = 0.
    RECT%SF         = 1.
    RECT%X0         = 0.
    RECT%Y0         = 0.
    RECT%SF0        = 1.

    ! read rect namelist
    REWIND (NDSI)
    READ (NDSI, nml=RECT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_RECT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (5)
    END IF

    ! save namelist
    NML_RECT = RECT

  END SUBROUTINE READ_RECT_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_CURV_NML (NDSI, NML_CURV)
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
    !      NDSI             Int.
    !      NML_CURV         Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_CURV_T), INTENT(INOUT)      :: NML_CURV

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_CURV_T) :: CURV
    NAMELIST /CURV_NML/ CURV
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_CURV_NML')
#endif

    ! set default values for curv structure
    CURV%NX              = 0
    CURV%NY              = 0
    !
    CURV%XCOORD%SF        = 1.
    CURV%XCOORD%OFF       = 0.
    CURV%XCOORD%FILENAME  = 'unset'
    CURV%XCOORD%IDF       = 21
    CURV%XCOORD%IDLA      = 1
    CURV%XCOORD%IDFM      = 1
    CURV%XCOORD%FORMAT    = '(....)'
    CURV%XCOORD%FROM      = 'NAME'
    !
    CURV%YCOORD%SF        = 1.
    CURV%YCOORD%OFF       = 0.
    CURV%YCOORD%FILENAME  = 'unset'
    CURV%YCOORD%IDF       = 22
    CURV%YCOORD%IDLA      = 1
    CURV%YCOORD%IDFM      = 1
    CURV%YCOORD%FORMAT    = '(....)'
    CURV%YCOORD%FROM      = 'NAME'

    ! read curv namelist
    REWIND (NDSI)
    READ (NDSI, nml=CURV_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_CURV_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (6)
    END IF

    ! save namelist
    NML_CURV = CURV

  END SUBROUTINE READ_CURV_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_UNST_NML (NDSI, NML_UNST)
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
    !      NDSI             Int.
    !      NML_UNST         Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_UNST_T), INTENT(INOUT)      :: NML_UNST

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_UNST_T) :: UNST
    NAMELIST /UNST_NML/ UNST
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_UNST_NML')
#endif

    ! set default values for unst structure
    UNST%SF              = 0
    UNST%FILENAME        = 'unset'
    UNST%IDF             = 20
    UNST%IDLA            = 1
    UNST%IDFM            = 1
    UNST%FORMAT          = '(....)'
    UNST%UGOBCFILE       = 'unset'

    ! read unst namelist
    REWIND (NDSI)
    READ (NDSI, nml=UNST_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_UNST_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (7)
    END IF

    ! save namelist
    NML_UNST = UNST

  END SUBROUTINE READ_UNST_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_SMC_NML (NDSI, NML_SMC)
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
    !      NDSI             Int.
    !      NML_SMC          Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_SMC_T), INTENT(INOUT)       :: NML_SMC

    ! locals
    INTEGER                   :: IERR
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
    SMC%MCELS%FILENAME   = 'unset'
    SMC%MCELS%IDF        = 31
    SMC%MCELS%IDLA       = 1
    SMC%MCELS%IDFM       = 1
    SMC%MCELS%FORMAT     = '(....)'
    !
    SMC%ISIDE%FILENAME   = 'unset'
    SMC%ISIDE%IDF        = 32
    SMC%ISIDE%IDLA       = 1
    SMC%ISIDE%IDFM       = 1
    SMC%ISIDE%FORMAT     = '(....)'
    !
    SMC%JSIDE%FILENAME   = 'unset'
    SMC%JSIDE%IDF        = 33
    SMC%JSIDE%IDLA       = 1
    SMC%JSIDE%IDFM       = 1
    SMC%JSIDE%FORMAT     = '(....)'
    !
    SMC%SUBTR%FILENAME   = 'unset'
    SMC%SUBTR%IDF        = 34
    SMC%SUBTR%IDLA       = 1
    SMC%SUBTR%IDFM       = 1
    SMC%SUBTR%FORMAT     = '(....)'
    !
    SMC%BUNDY%FILENAME   = 'unset'
    SMC%BUNDY%IDF        = 35
    SMC%BUNDY%IDLA       = 1
    SMC%BUNDY%IDFM       = 1
    SMC%BUNDY%FORMAT     = '(....)'
    !
    SMC%MBARC%FILENAME   = 'unset'
    SMC%MBARC%IDF        = 36
    SMC%MBARC%IDLA       = 1
    SMC%MBARC%IDFM       = 1
    SMC%MBARC%FORMAT     = '(....)'
    !
    SMC%AISID%FILENAME   = 'unset'
    SMC%AISID%IDF        = 37
    SMC%AISID%IDLA       = 1
    SMC%AISID%IDFM       = 1
    SMC%AISID%FORMAT     = '(....)'
    !
    SMC%AJSID%FILENAME   = 'unset'
    SMC%AJSID%IDF        = 38
    SMC%AJSID%IDLA       = 1
    SMC%AJSID%IDFM       = 1
    SMC%AJSID%FORMAT     = '(....)'


    ! read smc namelist
    REWIND (NDSI)
    READ (NDSI, nml=SMC_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_SMC_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (8)
    END IF

    ! save namelist
    NML_SMC = SMC

  END SUBROUTINE READ_SMC_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_DEPTH_NML (NDSI, NML_DEPTH)
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
    !      NDSI             Int.
    !      NML_DEPTH        Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_DEPTH_T), INTENT(INOUT)     :: NML_DEPTH

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_DEPTH_T) :: DEPTH
    NAMELIST /DEPTH_NML/ DEPTH
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_DEPTH_NML')
#endif

    ! set default values for depth structure
    DEPTH%SF         = 1.
    DEPTH%FILENAME   = 'unset'
    DEPTH%IDF        = 50
    DEPTH%IDLA       = 1
    DEPTH%IDFM       = 1
    DEPTH%FORMAT     = '(....)'
    DEPTH%FROM       = 'NAME'

    ! read depth namelist
    REWIND (NDSI)
    READ (NDSI, nml=DEPTH_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_DEPTH_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (9)
    END IF

    ! save namelist
    NML_DEPTH = DEPTH

  END SUBROUTINE READ_DEPTH_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_MASK_NML (NDSI, NML_MASK)
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
    !      NDSI             Int.
    !      NML_MASK         Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_MASK_T), INTENT(INOUT)      :: NML_MASK

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_MASK_T) :: MASK
    NAMELIST /MASK_NML/ MASK
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_MASK_NML')
#endif

    ! set default values for mask structure
    MASK%SF         = 1.
    MASK%FILENAME   = 'unset'
    MASK%IDF        = 60
    MASK%IDLA       = 1
    MASK%IDFM       = 1
    MASK%FORMAT     = '(....)'
    MASK%FROM       = 'NAME'

    ! read mask namelist
    REWIND (NDSI)
    READ (NDSI, nml=MASK_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_MASK_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (10)
    END IF

    ! save namelist
    NML_MASK = MASK

  END SUBROUTINE READ_MASK_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_OBST_NML (NDSI, NML_OBST)
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
    !      NDSI             Int.
    !      NML_OBST         Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_OBST_T), INTENT(INOUT)      :: NML_OBST

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_OBST_T) :: OBST
    NAMELIST /OBST_NML/ OBST
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_OBST_NML')
#endif

    ! set default values for obst structure
    OBST%SF         = 1.
    OBST%FILENAME   = 'unset'
    OBST%IDF        = 70
    OBST%IDLA       = 1
    OBST%IDFM       = 1
    OBST%FORMAT     = '(....)'
    OBST%FROM       = 'NAME'

    ! read obst namelist
    REWIND (NDSI)
    READ (NDSI, nml=OBST_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_OBST_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (11)
    END IF

    ! save namelist
    NML_OBST = OBST

  END SUBROUTINE READ_OBST_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_SLOPE_NML (NDSI, NML_SLOPE)
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
    !      NDSI             Int.
    !      NML_SLOPE        Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_SLOPE_T), INTENT(INOUT)     :: NML_SLOPE

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_SLOPE_T) :: SLOPE
    NAMELIST /SLOPE_NML/ SLOPE
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_SLOPE_NML')
#endif

    ! set default values for slope structure
    SLOPE%SF         = 1.
    SLOPE%FILENAME   = 'unset'
    SLOPE%IDF        = 80
    SLOPE%IDLA       = 1
    SLOPE%IDFM       = 1
    SLOPE%FORMAT     = '(....)'
    SLOPE%FROM       = 'NAME'

    ! read slope namelist
    REWIND (NDSI)
    READ (NDSI, nml=SLOPE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_SLOPE_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (12)
    END IF

    ! save namelist
    NML_SLOPE = SLOPE

  END SUBROUTINE READ_SLOPE_NML

  !/ ------------------------------------------------------------------- /





  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_SED_NML (NDSI, NML_SED)
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
    !      NDSI             Int.
    !      NML_SED          Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                  :: NDSI
    TYPE(NML_SED_T), INTENT(INOUT)       :: NML_SED

    ! locals
    INTEGER                   :: IERR
    TYPE(NML_SED_T) :: SED
    NAMELIST /SED_NML/ SED
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_SED_NML')
#endif

    ! set default values for sed structure
    SED%SF         = 1.
    SED%FILENAME   = 'unset'
    SED%IDF        = 90
    SED%IDLA       = 1
    SED%IDFM       = 1
    SED%FORMAT     = '(....)'
    SED%FROM       = 'NAME'

    ! read sed namelist
    REWIND (NDSI)
    READ (NDSI, nml=SED_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_SED_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (13)
    END IF

    ! save namelist
    NML_SED = SED

  END SUBROUTINE READ_SED_NML

  !/ ------------------------------------------------------------------- /





  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_INBOUND_NML (NDSI, NML_INBND_COUNT, NML_INBND_POINT)
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
    !      NDSI             Int.
    !      NML_INBND_COUNT  Type.
    !      NML_INBND_POINT  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                      :: NDSI
    TYPE(NML_INBND_COUNT_T), INTENT(INOUT)   :: NML_INBND_COUNT
    TYPE(NML_INBND_POINT_T), ALLOCATABLE, INTENT(INOUT)   :: NML_INBND_POINT(:)

    ! locals
    INTEGER                   :: IERR, I
    TYPE(NML_INBND_COUNT_T) :: INBND_COUNT
    NAMELIST /INBND_COUNT_NML/ INBND_COUNT
    TYPE(NML_INBND_POINT_T), ALLOCATABLE :: INBND_POINT(:)
    NAMELIST /INBND_POINT_NML/ INBND_POINT
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_INBOUND_NML')
#endif

    ! set default values for inbnd count structure
    INBND_COUNT%N_POINT    = 0

    ! read inbnd count namelist
    REWIND (NDSI)
    READ (NDSI, nml=INBND_COUNT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_INBND_COUNT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (14)
    END IF

    ! allocate the total count of inbound points
    ALLOCATE(INBND_POINT(INBND_COUNT%N_POINT))
    ALLOCATE(NML_INBND_POINT(INBND_COUNT%N_POINT))

    ! set default values for inbnd point structure
    IF (INBND_COUNT%N_POINT .NE. 0 ) THEN
      DO I=1,INBND_COUNT%N_POINT
        INBND_POINT(I)%X_INDEX  = 0
        INBND_POINT(I)%Y_INDEX  = 0
        INBND_POINT(I)%CONNECT  = .FALSE.
      END DO
    END IF

    ! read inbnd point namelist
    REWIND (NDSI)
    READ (NDSI, nml=INBND_POINT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_INBND_POINT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (15)
    END IF

    ! save namelist
    NML_INBND_COUNT = INBND_COUNT
    NML_INBND_POINT = INBND_POINT

  END SUBROUTINE READ_INBOUND_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_EXCLUDED_NML (NDSI, NML_EXCL_COUNT, NML_EXCL_POINT, &
       NML_EXCL_BODY)
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
    !      NDSI             Int.
    !      NML_EXCL_COUNT   Type.
    !      NML_EXCL_POINT   Type.
    !      NML_EXCL_BODY    Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                      :: NDSI
    TYPE(NML_EXCL_COUNT_T), INTENT(INOUT)    :: NML_EXCL_COUNT
    TYPE(NML_EXCL_POINT_T), ALLOCATABLE, INTENT(INOUT)   :: NML_EXCL_POINT(:)
    TYPE(NML_EXCL_BODY_T), ALLOCATABLE, INTENT(INOUT)    :: NML_EXCL_BODY(:)

    ! locals
    INTEGER                   :: IERR, J, K
    TYPE(NML_EXCL_COUNT_T) :: EXCL_COUNT
    NAMELIST /EXCL_COUNT_NML/ EXCL_COUNT
    TYPE(NML_EXCL_POINT_T), ALLOCATABLE :: EXCL_POINT(:)
    NAMELIST /EXCL_POINT_NML/ EXCL_POINT
    TYPE(NML_EXCL_BODY_T), ALLOCATABLE :: EXCL_BODY(:)
    NAMELIST /EXCL_BODY_NML/ EXCL_BODY
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_EXCLUDED_NML')
#endif

    ! set default values for excl count structure
    EXCL_COUNT%N_POINT    = 0
    EXCL_COUNT%N_BODY     = 0

    ! read excl count namelist
    REWIND (NDSI)
    READ (NDSI, nml=EXCL_COUNT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_EXCL_COUNT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (16)
    END IF

    ! allocate the total count of excluded points
    ALLOCATE(EXCL_POINT(EXCL_COUNT%N_POINT))
    ALLOCATE(NML_EXCL_POINT(EXCL_COUNT%N_POINT))

    ! set default values for excl point structure
    IF (EXCL_COUNT%N_POINT .NE. 0 ) THEN
      DO J=1,EXCL_COUNT%N_POINT
        EXCL_POINT(J)%X_INDEX  = 0
        EXCL_POINT(J)%Y_INDEX  = 0
        EXCL_POINT(J)%CONNECT  = .FALSE.
      END DO
    END IF

    ! read excl point namelist
    REWIND (NDSI)
    READ (NDSI, nml=EXCL_POINT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_EXCL_POINT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (17)
    END IF

    ! allocate the total count of excluded bodies
    ALLOCATE(EXCL_BODY(EXCL_COUNT%N_BODY))
    ALLOCATE(NML_EXCL_BODY(EXCL_COUNT%N_BODY))

    ! set default values for excl body structure
    IF (EXCL_COUNT%N_BODY .NE. 0 ) THEN
      DO K=1,EXCL_COUNT%N_BODY
        EXCL_BODY(K)%X_INDEX  = 0
        EXCL_BODY(K)%Y_INDEX  = 0
      END DO
    END IF

    ! read excl body namelist
    REWIND (NDSI)
    READ (NDSI, nml=EXCL_POINT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_EXCL_POINT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (18)
    END IF

    ! save namelist
    NML_EXCL_COUNT = EXCL_COUNT
    NML_EXCL_POINT = EXCL_POINT
    NML_EXCL_BODY  = EXCL_BODY

  END SUBROUTINE READ_EXCLUDED_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE READ_OUTBOUND_NML (NDSI, NML_OUTBND_COUNT, NML_OUTBND_LINE)
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
    !      NDSI             Int.
    !      NML_OUTBND_COUNT Type.
    !      NML_OUTBND_LINE  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    INTEGER, INTENT(IN)                      :: NDSI
    TYPE(NML_OUTBND_COUNT_T), INTENT(INOUT)    :: NML_OUTBND_COUNT
    TYPE(NML_OUTBND_LINE_T), ALLOCATABLE, INTENT(INOUT)   :: NML_OUTBND_LINE(:)

    ! locals
    INTEGER                   :: IERR, I
    TYPE(NML_OUTBND_COUNT_T) :: OUTBND_COUNT
    NAMELIST /OUTBND_COUNT_NML/ OUTBND_COUNT
    TYPE(NML_OUTBND_LINE_T), ALLOCATABLE :: OUTBND_LINE(:)
    NAMELIST /OUTBND_LINE_NML/ OUTBND_LINE

#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

    IERR = 0
#ifdef W3_S
    CALL STRACE (IENT, 'READ_OUTBOUND_NML')
#endif

    ! set default values for outbnd count structure
    OUTBND_COUNT%N_LINE    = 0

    ! read outbnd count namelist
    REWIND (NDSI)
    READ (NDSI, nml=OUTBND_COUNT_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_OUTBND_COUNT_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (19)
    END IF

    ! allocate the total count of outbound lines
    ALLOCATE(OUTBND_LINE(OUTBND_COUNT%N_LINE))
    ALLOCATE(NML_OUTBND_LINE(OUTBND_COUNT%N_LINE))

    ! set default values for outbnd line structure
    IF (OUTBND_COUNT%N_LINE .NE. 0 ) THEN
      DO I=1,OUTBND_COUNT%N_LINE
        OUTBND_LINE(I)%X0   = 0.
        OUTBND_LINE(I)%Y0   = 0.
        OUTBND_LINE(I)%DX   = 0.
        OUTBND_LINE(I)%DY   = 0.
        OUTBND_LINE(I)%NP   = 0
      END DO
    END IF

    ! read outbnd line namelist
    REWIND (NDSI)
    READ (NDSI, nml=OUTBND_LINE_NML, iostat=IERR, iomsg=MSG)
    IF (IERR.GT.0) THEN
      WRITE (NDSE,'(A,/A)') &
           'ERROR: READ_OUTBND_LINE_NML: namelist read error', &
           'ERROR: '//TRIM(MSG)
      CALL EXTCDE (20)
    END IF

    ! save namelist
    NML_OUTBND_COUNT = OUTBND_COUNT
    NML_OUTBND_LINE  = OUTBND_LINE

  END SUBROUTINE READ_OUTBOUND_NML

  !/ ------------------------------------------------------------------- /





  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SPECTRUM_NML (NML_SPECTRUM)
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
    !      NML_SPECTRUM  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_SPECTRUM_T), INTENT(IN) :: NML_SPECTRUM
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_SPECTRUM_NML')
#endif

    WRITE (MSG,'(A)') 'SPECTRUM % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,14) TRIM(MSG),'XFR        = ', NML_SPECTRUM%XFR
    WRITE (NDSN,14) TRIM(MSG),'FREQ1      = ', NML_SPECTRUM%FREQ1
    WRITE (NDSN,11) TRIM(MSG),'NK         = ', NML_SPECTRUM%NK
    WRITE (NDSN,11) TRIM(MSG),'NTH        = ', NML_SPECTRUM%NTH
    WRITE (NDSN,14) TRIM(MSG),'THOFF      = ', NML_SPECTRUM%THOFF

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SPECTRUM_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_RUN_NML (NML_RUN)
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
    !      NML_RUN  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_RUN_T), INTENT(IN) :: NML_RUN
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_RUN_NML')
#endif

    WRITE (MSG,'(A)') 'RUN % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,13) TRIM(MSG),'FLDRY      = ', NML_RUN%FLDRY
    WRITE (NDSN,13) TRIM(MSG),'FLCX       = ', NML_RUN%FLCX
    WRITE (NDSN,13) TRIM(MSG),'FLCY       = ', NML_RUN%FLCY
    WRITE (NDSN,13) TRIM(MSG),'FLCTH      = ', NML_RUN%FLCTH
    WRITE (NDSN,13) TRIM(MSG),'FLCK       = ', NML_RUN%FLCK
    WRITE (NDSN,13) TRIM(MSG),'FLSOU      = ', NML_RUN%FLSOU


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_RUN_NML

  !/ ------------------------------------------------------------------- /


  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_TIMESTEPS_NML (NML_TIMESTEPS)
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
    !      NML_TIMESTEPS  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_TIMESTEPS_T), INTENT(IN) :: NML_TIMESTEPS
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_TIMESTEPS_NML')
#endif

    WRITE (MSG,'(A)') 'TIMESTEPS % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,14) TRIM(MSG),'DTMAX      = ', NML_TIMESTEPS%DTMAX
    WRITE (NDSN,14) TRIM(MSG),'DTXY       = ', NML_TIMESTEPS%DTXY
    WRITE (NDSN,14) TRIM(MSG),'DTKTH      = ', NML_TIMESTEPS%DTKTH
    WRITE (NDSN,14) TRIM(MSG),'DTMIN      = ', NML_TIMESTEPS%DTMIN

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_TIMESTEPS_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_GRID_NML (NML_GRID)
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
    !      NML_GRID  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_GRID_T), INTENT(IN) :: NML_GRID
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_GRID_NML')
#endif

    WRITE (MSG,'(A)') 'GRID % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'NAME       = ', TRIM(NML_GRID%NAME)
    WRITE (NDSN,10) TRIM(MSG),'NML        = ', TRIM(NML_GRID%NML)
    WRITE (NDSN,10) TRIM(MSG),'TYPE       = ', TRIM(NML_GRID%TYPE)
    WRITE (NDSN,10) TRIM(MSG),'COORD      = ', TRIM(NML_GRID%COORD)
    WRITE (NDSN,10) TRIM(MSG),'CLOS       = ', TRIM(NML_GRID%CLOS)
    WRITE (NDSN,14) TRIM(MSG),'ZLIM       = ', NML_GRID%ZLIM
    WRITE (NDSN,14) TRIM(MSG),'DMIN       = ', NML_GRID%DMIN

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_GRID_NML

  !/ ------------------------------------------------------------------- /


  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_RECT_NML (NML_RECT)
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
    !      NML_RECT  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_RECT_T), INTENT(IN) :: NML_RECT
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_RECT_NML')
#endif

    WRITE (MSG,'(A)') 'RECT % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'NX         = ', NML_RECT%NX
    WRITE (NDSN,11) TRIM(MSG),'NY         = ', NML_RECT%NY
    WRITE (NDSN,14) TRIM(MSG),'SX         = ', NML_RECT%SX
    WRITE (NDSN,14) TRIM(MSG),'SY         = ', NML_RECT%SY
    WRITE (NDSN,14) TRIM(MSG),'SF         = ', NML_RECT%SF
    WRITE (NDSN,14) TRIM(MSG),'X0         = ', NML_RECT%X0
    WRITE (NDSN,14) TRIM(MSG),'Y0         = ', NML_RECT%Y0
    WRITE (NDSN,14) TRIM(MSG),'SF0        = ', NML_RECT%SF0

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F12.2)

  END SUBROUTINE REPORT_RECT_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_CURV_NML (NML_CURV)
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
    !      NML_CURV  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_CURV_T), INTENT(IN) :: NML_CURV
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_CURV_NML')
#endif

    WRITE (MSG,'(A)') 'CURV % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'NX                 = ', NML_CURV%NX
    WRITE (NDSN,11) TRIM(MSG),'NY                 = ', NML_CURV%NY
    !
    WRITE (NDSN,14) TRIM(MSG),'XCOORD % SF        = ', NML_CURV%XCOORD%SF
    WRITE (NDSN,14) TRIM(MSG),'XCOORD % OFF       = ', NML_CURV%XCOORD%OFF
    WRITE (NDSN,10) TRIM(MSG),'XCOORD % FILENAME  = ', TRIM(NML_CURV%XCOORD%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'XCOORD % IDF       = ', NML_CURV%XCOORD%IDF
    WRITE (NDSN,11) TRIM(MSG),'XCOORD % IDLA      = ', NML_CURV%XCOORD%IDLA
    WRITE (NDSN,11) TRIM(MSG),'XCOORD % IDFM      = ', NML_CURV%XCOORD%IDFM
    WRITE (NDSN,10) TRIM(MSG),'XCOORD % FORMAT    = ', TRIM(NML_CURV%XCOORD%FORMAT)
    WRITE (NDSN,10) TRIM(MSG),'XCOORD % FROM      = ', TRIM(NML_CURV%XCOORD%FROM)
    !
    WRITE (NDSN,14) TRIM(MSG),'YCOORD % SF        = ', NML_CURV%YCOORD%SF
    WRITE (NDSN,14) TRIM(MSG),'YCOORD % OFF       = ', NML_CURV%YCOORD%OFF
    WRITE (NDSN,10) TRIM(MSG),'YCOORD % FILENAME  = ', TRIM(NML_CURV%YCOORD%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'YCOORD % IDF       = ', NML_CURV%YCOORD%IDF
    WRITE (NDSN,11) TRIM(MSG),'YCOORD % IDLA      = ', NML_CURV%YCOORD%IDLA
    WRITE (NDSN,11) TRIM(MSG),'YCOORD % IDFM      = ', NML_CURV%YCOORD%IDFM
    WRITE (NDSN,10) TRIM(MSG),'YCOORD % FORMAT    = ', TRIM(NML_CURV%YCOORD%FORMAT)
    WRITE (NDSN,10) TRIM(MSG),'YCOORD % FROM      = ', TRIM(NML_CURV%YCOORD%FROM)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_CURV_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_UNST_NML (NML_UNST)
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
    !      NML_UNST  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_UNST_T), INTENT(IN) :: NML_UNST
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_UNST_NML')
#endif

    WRITE (MSG,'(A)') 'UNST % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_UNST%SF
    WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_UNST%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_UNST%IDF
    WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_UNST%IDLA
    WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_UNST%IDFM
    WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_UNST%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'UGOBCFILE = ', TRIM(NML_UNST%UGOBCFILE)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_UNST_NML

  !/ ------------------------------------------------------------------- /



  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SMC_NML (NML_SMC)
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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
    !
    WRITE (NDSN,10) TRIM(MSG),'MCELS % FILENAME  = ', TRIM(NML_SMC%MCELS%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'MCELS % IDF       = ', NML_SMC%MCELS%IDF
    WRITE (NDSN,11) TRIM(MSG),'MCELS % IDLA      = ', NML_SMC%MCELS%IDLA
    WRITE (NDSN,11) TRIM(MSG),'MCELS % IDFM      = ', NML_SMC%MCELS%IDFM
    WRITE (NDSN,10) TRIM(MSG),'MCELS % FORMAT    = ', TRIM(NML_SMC%MCELS%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'ISIDE % FILENAME  = ', TRIM(NML_SMC%ISIDE%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'ISIDE % IDF       = ', NML_SMC%ISIDE%IDF
    WRITE (NDSN,11) TRIM(MSG),'ISIDE % IDLA      = ', NML_SMC%ISIDE%IDLA
    WRITE (NDSN,11) TRIM(MSG),'ISIDE % IDFM      = ', NML_SMC%ISIDE%IDFM
    WRITE (NDSN,10) TRIM(MSG),'ISIDE % FORMAT    = ', TRIM(NML_SMC%ISIDE%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'JSIDE % FILENAME  = ', TRIM(NML_SMC%JSIDE%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'JSIDE % IDF       = ', NML_SMC%JSIDE%IDF
    WRITE (NDSN,11) TRIM(MSG),'JSIDE % IDLA      = ', NML_SMC%JSIDE%IDLA
    WRITE (NDSN,11) TRIM(MSG),'JSIDE % IDFM      = ', NML_SMC%JSIDE%IDFM
    WRITE (NDSN,10) TRIM(MSG),'JSIDE % FORMAT    = ', TRIM(NML_SMC%JSIDE%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'SUBTR % FILENAME  = ', TRIM(NML_SMC%SUBTR%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'SUBTR % IDF       = ', NML_SMC%SUBTR%IDF
    WRITE (NDSN,11) TRIM(MSG),'SUBTR % IDLA      = ', NML_SMC%SUBTR%IDLA
    WRITE (NDSN,11) TRIM(MSG),'SUBTR % IDFM      = ', NML_SMC%SUBTR%IDFM
    WRITE (NDSN,10) TRIM(MSG),'SUBTR % FORMAT    = ', TRIM(NML_SMC%SUBTR%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'BUNDY % FILENAME  = ', TRIM(NML_SMC%BUNDY%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'BUNDY % IDF       = ', NML_SMC%BUNDY%IDF
    WRITE (NDSN,11) TRIM(MSG),'BUNDY % IDLA      = ', NML_SMC%BUNDY%IDLA
    WRITE (NDSN,11) TRIM(MSG),'BUNDY % IDFM      = ', NML_SMC%BUNDY%IDFM
    WRITE (NDSN,10) TRIM(MSG),'BUNDY % FORMAT    = ', TRIM(NML_SMC%BUNDY%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'MBARC % FILENAME  = ', TRIM(NML_SMC%MBARC%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'MBARC % IDF       = ', NML_SMC%MBARC%IDF
    WRITE (NDSN,11) TRIM(MSG),'MBARC % IDLA      = ', NML_SMC%MBARC%IDLA
    WRITE (NDSN,11) TRIM(MSG),'MBARC % IDFM      = ', NML_SMC%MBARC%IDFM
    WRITE (NDSN,10) TRIM(MSG),'MBARC % FORMAT    = ', TRIM(NML_SMC%MBARC%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'AISID % FILENAME  = ', TRIM(NML_SMC%AISID%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'AISID % IDF       = ', NML_SMC%AISID%IDF
    WRITE (NDSN,11) TRIM(MSG),'AISID % IDLA      = ', NML_SMC%AISID%IDLA
    WRITE (NDSN,11) TRIM(MSG),'AISID % IDFM      = ', NML_SMC%AISID%IDFM
    WRITE (NDSN,10) TRIM(MSG),'AISID % FORMAT    = ', TRIM(NML_SMC%AISID%FORMAT)
    !
    WRITE (NDSN,10) TRIM(MSG),'AJSID % FILENAME  = ', TRIM(NML_SMC%AJSID%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'AJSID % IDF       = ', NML_SMC%AJSID%IDF
    WRITE (NDSN,11) TRIM(MSG),'AJSID % IDLA      = ', NML_SMC%AJSID%IDLA
    WRITE (NDSN,11) TRIM(MSG),'AJSID % IDFM      = ', NML_SMC%AJSID%IDFM
    WRITE (NDSN,10) TRIM(MSG),'AJSID % FORMAT    = ', TRIM(NML_SMC%AJSID%FORMAT)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)


  END SUBROUTINE REPORT_SMC_NML

  !/ ------------------------------------------------------------------- /





  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_DEPTH_NML (NML_DEPTH)
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
    !      NML_DEPTH  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_DEPTH_T), INTENT(IN) :: NML_DEPTH
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_DEPTH_NML')
#endif

    WRITE (MSG,'(A)') 'DEPTH % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_DEPTH%SF
    WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_DEPTH%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_DEPTH%IDF
    WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_DEPTH%IDLA
    WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_DEPTH%IDFM
    WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_DEPTH%FORMAT)
    WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_DEPTH%FROM)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_DEPTH_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_MASK_NML (NML_MASK)
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
    !      NML_MASK  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_MASK_T), INTENT(IN) :: NML_MASK
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_MASK_NML')
#endif

    WRITE (MSG,'(A)') 'MASK % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_MASK%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_MASK%IDF
    WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_MASK%IDLA
    WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_MASK%IDFM
    WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_MASK%FORMAT)
    WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_MASK%FROM)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_MASK_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_OBST_NML (NML_OBST)
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
    !      NML_OBST  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_OBST_T), INTENT(IN) :: NML_OBST
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_OBST_NML')
#endif

    WRITE (MSG,'(A)') 'OBST % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_OBST%SF
    WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_OBST%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_OBST%IDF
    WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_OBST%IDLA
    WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_OBST%IDFM
    WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_OBST%FORMAT)
    WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_OBST%FROM)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_OBST_NML

  !/ ------------------------------------------------------------------- /







  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SLOPE_NML (NML_SLOPE)
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
    !      NML_SLOPE  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_SLOPE_T), INTENT(IN) :: NML_SLOPE
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_SLOPE_NML')
#endif

    WRITE (MSG,'(A)') 'SLOPE % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_SLOPE%SF
    WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_SLOPE%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_SLOPE%IDF
    WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_SLOPE%IDLA
    WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_SLOPE%IDFM
    WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_SLOPE%FORMAT)
    WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_SLOPE%FROM)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SLOPE_NML

  !/ ------------------------------------------------------------------- /








  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_SED_NML (NML_SED)
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
    !      NML_SED  Type.
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_SED_T), INTENT(IN) :: NML_SED
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_SED_NML')
#endif

    WRITE (MSG,'(A)') 'SED % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,14) TRIM(MSG),'SF        = ', NML_SED%SF
    WRITE (NDSN,10) TRIM(MSG),'FILENAME  = ', TRIM(NML_SED%FILENAME)
    WRITE (NDSN,11) TRIM(MSG),'IDF       = ', NML_SED%IDF
    WRITE (NDSN,11) TRIM(MSG),'IDLA      = ', NML_SED%IDLA
    WRITE (NDSN,11) TRIM(MSG),'IDFM      = ', NML_SED%IDFM
    WRITE (NDSN,10) TRIM(MSG),'FORMAT    = ', TRIM(NML_SED%FORMAT)
    WRITE (NDSN,10) TRIM(MSG),'FROM      = ', TRIM(NML_SED%FROM)


10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_SED_NML

  !/ ------------------------------------------------------------------- /






  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_INBOUND_NML (NML_INBND_COUNT, NML_INBND_POINT)
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
    !      NML_INBND_COUNT  Type
    !      NML_INBND_POINT  Type
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_INBND_COUNT_T), INTENT(IN) :: NML_INBND_COUNT
    TYPE(NML_INBND_POINT_T), INTENT(IN) :: NML_INBND_POINT(NML_INBND_COUNT%N_POINT)

    ! locals
    INTEGER              :: I
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_INBOUND_NML')
#endif

    WRITE (MSG,'(A)') 'INBND_COUNT % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'N_POINT       = ', NML_INBND_COUNT%N_POINT

    IF (NML_INBND_COUNT%N_POINT .NE. 0) THEN
      DO I=1,NML_INBND_COUNT%N_POINT
        WRITE (MSG,'(A,I8,A)') 'INBND_POINT(',I,') % '
        WRITE (NDSN,'(A)')
        WRITE (NDSN,11) TRIM(MSG),'X_INDEX   = ', NML_INBND_POINT(I)%X_INDEX
        WRITE (NDSN,11) TRIM(MSG),'Y_INDEX   = ', NML_INBND_POINT(I)%Y_INDEX
        WRITE (NDSN,13) TRIM(MSG),'CONNECT   = ', NML_INBND_POINT(I)%CONNECT
        WRITE (NDSN,'(A)')
      END DO
    END IF

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F8.2)

  END SUBROUTINE REPORT_INBOUND_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_EXCLUDED_NML (NML_EXCL_COUNT, NML_EXCL_POINT, NML_EXCL_BODY)
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
    !      NML_EXCL_COUNT  Type
    !      NML_EXCL_POINT  Type
    !      NML_EXCL_BODY   Type
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_EXCL_COUNT_T), INTENT(IN) :: NML_EXCL_COUNT
    TYPE(NML_EXCL_POINT_T), INTENT(IN) :: NML_EXCL_POINT(NML_EXCL_COUNT%N_POINT)
    TYPE(NML_EXCL_BODY_T), INTENT(IN)  :: NML_EXCL_BODY(NML_EXCL_COUNT%N_BODY)

    ! locals
    INTEGER              :: I
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_EXCLUDED_NML')
#endif

    WRITE (MSG,'(A)') 'EXCL_COUNT % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'N_POINT       = ', NML_EXCL_COUNT%N_POINT
    WRITE (NDSN,11) TRIM(MSG),'N_BODY        = ', NML_EXCL_COUNT%N_BODY

    IF (NML_EXCL_COUNT%N_POINT .NE. 0) THEN
      DO I=1,NML_EXCL_COUNT%N_POINT
        WRITE (MSG,'(A,I8,A)') 'EXCL_POINT(',I,') % '
        WRITE (NDSN,'(A)')
        WRITE (NDSN,11) TRIM(MSG),'X_INDEX   = ', NML_EXCL_POINT(I)%X_INDEX
        WRITE (NDSN,11) TRIM(MSG),'Y_INDEX   = ', NML_EXCL_POINT(I)%Y_INDEX
        WRITE (NDSN,13) TRIM(MSG),'CONNECT   = ', NML_EXCL_POINT(I)%CONNECT
        WRITE (NDSN,'(A)')
      END DO
    END IF

    IF (NML_EXCL_COUNT%N_BODY .NE. 0) THEN
      DO I=1,NML_EXCL_COUNT%N_BODY
        WRITE (MSG,'(A,I8,A)') 'EXCL_BODY(',I,') % '
        WRITE (NDSN,'(A)')
        WRITE (NDSN,11) TRIM(MSG),'X_INDEX   = ', NML_EXCL_BODY(I)%X_INDEX
        WRITE (NDSN,11) TRIM(MSG),'Y_INDEX   = ', NML_EXCL_BODY(I)%Y_INDEX
        WRITE (NDSN,'(A)')
      END DO
    END IF

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F12.2)

  END SUBROUTINE REPORT_EXCLUDED_NML

  !/ ------------------------------------------------------------------- /




  !/ ------------------------------------------------------------------- /

  SUBROUTINE REPORT_OUTBOUND_NML (NML_OUTBND_COUNT, NML_OUTBND_LINE)
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
    !      NML_OUTBND_COUNT  Type
    !      NML_OUTBND_LINE   Type
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
    !      W3NMLGRID Subr.   N/A    Namelist configuration routine.
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

    TYPE(NML_OUTBND_COUNT_T), INTENT(IN) :: NML_OUTBND_COUNT
    TYPE(NML_OUTBND_LINE_T), INTENT(IN)  :: NML_OUTBND_LINE(NML_OUTBND_COUNT%N_LINE)

    ! locals
    INTEGER              :: I
#ifdef W3_S
    INTEGER, SAVE                           :: IENT = 0
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'REPORT_OUTBOUND_NML')
#endif

    WRITE (MSG,'(A)') 'OUTBND_COUNT % '
    WRITE (NDSN,'(A)')
    WRITE (NDSN,11) TRIM(MSG),'N_LINE        = ', NML_OUTBND_COUNT%N_LINE

    IF (NML_OUTBND_COUNT%N_LINE .NE. 0) THEN
      DO I=1,NML_OUTBND_COUNT%N_LINE
        WRITE (MSG,'(A,I8,A)') 'OUTBND_LINE(',I,') % '
        WRITE (NDSN,'(A)')
        WRITE (NDSN,14) TRIM(MSG),'X0        = ', NML_OUTBND_LINE(I)%X0
        WRITE (NDSN,14) TRIM(MSG),'Y0        = ', NML_OUTBND_LINE(I)%Y0
        WRITE (NDSN,14) TRIM(MSG),'DX        = ', NML_OUTBND_LINE(I)%DX
        WRITE (NDSN,14) TRIM(MSG),'DY        = ', NML_OUTBND_LINE(I)%DY
        WRITE (NDSN,11) TRIM(MSG),'NP        = ', NML_OUTBND_LINE(I)%NP
        WRITE (NDSN,'(A)')
      END DO
    END IF

10  FORMAT (A,2X,A,A)
11  FORMAT (A,2X,A,I8)
13  FORMAT (A,2X,A,L1)
14  FORMAT (A,2X,A,F12.2)

  END SUBROUTINE REPORT_OUTBOUND_NML

  !/ ------------------------------------------------------------------- /








END MODULE W3NMLGRIDMD

!/ ------------------------------------------------------------------- /
