!> @file w3ounfmetamd.F90
!> @brief User configurable netCDF meta-data for ww3_ounf.
!> @author Chris Bunney @date 02-Nov-2020
!/ ------------------------------------------------------------------- /
!> @brief Manages user configurable netCDF meta-data for ww3_ounf program.
!>
!> @details Default netCDF meta data is provided for each WW3 output variable
!>    and is stored intentally via the META_T type. The meta values are
!>    grouped by component (max 3), field (IFI) and group (IFJ).
!>
!>    The user can override this meta data via an input text file
!>    with the filename `ounfmeta.inp`.
!>
!>    Entries in the file are formatted as follows:
!>
!>    @verbatim
!>    META [ IFI [ IFJ ]  |  FLDID ]   [ IFC ]
!>      attr_name = attr_value
!>      attr_name = attr_value
!>      extra_attr = extra_value [type]
!>    ... repeated as many times as required.
!>    @endverbatim
!>
!>    An output field is selected using the META keyword followed by
!>    either an [IFI, IFJ] integer pair or a FieldID string. Optionally,
!>    either form may be followed by an integer value to select the
!>    component in multi-component fields (such as wind).
!>
!>    Blank lines and comments lines (starting with $) are ignored.
!>
!>    attr_name is the netCDF attribute name that you wish to override.
!>    This can be one of the following:
!>        -  "varnm"
!>        -  "ename"
!>        -  "standard_name", or "varns"
!>        -  "long_name" or "varnl"
!>        -  "globwave_name" or "varng"
!>        -  "direction_reference", "dir_ref" or "varnd"
!>        -  "comment" or "varnc"
!>        -  "units"
!>        -  "valid_min" or "vmin"
!>        -  "valid_max" or "vmax"
!>        -  "scale_factor" or "fsc"
!>
!>    Any other attribute name is assumed to be an optional "extra"
!>    attribute. This extra attribute can take an optional "type"
!>    keyworkd to specify the variable tpye of the metadata. If
!>    no type is supplied, it defaults to a characer type. Valid
!>    types are one of ["c", "r", "i"] for character/string,
!>    real/float or integer values respectively.
!>
!>    Global meta data can be specified with a special "META global" line:
!>
!>    @verbatim
!>    META global
!>      extra_attr = extra_value [type]
!>      extra_attr = extra_value [type]
!>    @endverbatim
!>
!>    A "coordinate reference system" (CRS) can be specified for all output
!>    fields using the "CRS" keyword. As a minimum, the "grid_mapping_name"
!>    attribute must be specified. If the CRS section is defined, all output
!>    fields will have a "grid_mapping" attribute added referencing the
!>    CRS variable. "crs_vaname" will be created as a scalar NF90_CHAR
!>    variable in the output file.
!>
!>    @verbatim
!>    CRS <crs_varname>
!>      grid_mapping_name = <mapping name>
!>      attr = value
!>      attr = value
!>    @endverbatim
!>
!>    Note: ALL keywords and "Field Name ID" strings (e.g. HS) are
!>    case insensitive. All netCDF attribute names are case sensitive.
!>
!>    Partitioned outputs are handles slightly differently; one meta data
!>    entry is used for all partitions of a field. The metadata is made
!>    specific to a particular partition via template strings. There are
!>    two built-in template strings: SPART and IPART. These provide a
!>    "string" description (e.g. "wind sea", "primary swell", etc) or an
!>    integer partition number. These can be references in the meta data
!>    using the template name surrounded by < .. >, e.g. \<SPART\>
!>
!>    It is also possible to supply user defined partitioned parameter
!>    template strings in the ounfmeta.inp file using the TEMPLATE
!>    keyword, as below:
!>
!>    @verbatim
!>    TEMPLATE <template-name>
!>      String for partition 0
!>      String for partition 1
!>      String for partition 2
!>      String for partition 3
!>      ... etc
!>    @endverbatim
!>
!>    Specifying the <template-name> with a trailing underscore will
!>    provide an underscore seperated (_) string, rather than space
!>    seperated.
!>
!>    Example ounfmeta.inp file:
!>
!>    @verbatim
!>    $ Lines starting with dollars are comments.
!>    $ The line starts a meta-data section for the depth field
!>    META DPT
!>      standard_name = depth
!>      long_name = "can be quoted string"
!>      comment = or an unquoted string
!>      vmax = 999.9
!>
!>    $ Next one is HSig (group 2, field 1)
!>    META 2 1
!>      varns = "sig. wave height"
!>      varnl = "this is long name"
!>
!>    $ Next one is second component of wind. It also sets an
!>    $ "extra" meta data value (height - a float)
!>    META WND 2
!>      standard_name = "v-wind"
!>      height = 10.0 "r"
!>
!>    $ User defined partitioned parameters template strings:
!>    TEMPLATE PARTSTR
!>      wind wave
!>      primary swell
!>      secondary swell
!>
!>    $ Use partition templates in partitioned Hs field:
!>    $ (SPART and IPART are built-in)
!>    META PHS
!>      standard_name = "<SPART_>_sigificant_wave_height"
!>      long_name = "<PARTSTR>"
!>      partition_number = "<IPART>"
!>
!>    $ Coordinate reference system:
!>    CRS crs
!>      grid_mapping_name = "latitude_longitude"
!>      semi_major_axis = 6371000.0 f
!>      inverse_flattening = 0 f
!>
!>    $ Global metadata:
!>    META global
!>      institution = UKMO
!>      comment "space seperated strings should be quoted" c
!>      version = 1.0 r
!>    @endverbatim
!>
!> @author Chris Bunney @date 02-Nov-2020
!>
!> ### Change log
!>   Date      | Ver  | Comments
!> ------------|------|---------
!> 02-Nov-2020 | 7.12 | Creation
!> 26-Jan-2021 | 7.12 | Added Tp and alternative dir/mag metadata for directional fields.
!> 16-Dec-2020 | 7.12 | Added user partition templates and coordinate reference system.
!> 02-Feb-2021 | 7.12 | Improved partitioned parameter template string implementation.
!> 22-Mar-2021 | 7.12 | Add extra coupling fields
!> 02-Sep-2021 | 7.12 | Add coordinates attribute
!>
MODULE W3OUNFMETAMD
  !/
  !/    02-Nov-2020 : Creation                            ( version 7.12 )
  !/    26-Jan-2021 : Added TP and alternative dir/mag    ( version 7.12 )
  !/                  metadata for directional fields.
  !/    16-Dec-2020 : Added user partition templates      ( version 7.12 )
  !/                  and coordinate reference system.
  !/                  Freeform meta data uses linked list.
  !/    02-Feb-2021 : Improved partitioned parameter      ( version 7.12 )
  !/                  template string implementation.
  !/    22-Mar-2021 : Adds extra coupling fields          ( version 7.13 )
  !/    02-Sep-2021 : Add coordinates attribute           ( version 7.12 )
  !/
  !/ ------------------------------------------------------------------- /
  !/
  USE NETCDF
  USE CONSTANTS, ONLY: TPIINV
  USE W3GDATMD, ONLY: SIG, NK, GTYPE, UNGTYPE
#ifdef W3_RTD
  USE W3GDATMD, ONLY : FLAGUNR, POLAT, POLON
#endif
#ifdef W3_SMC
  USE W3SMCOMD, ONLY : SMCOTYPE
#endif
  USE W3ODATMD, ONLY: PTMETH, PTFCUT, NOGRP, NOGE, NGRPP,           &
       NDSE, FNMPRE, NOSWLL
  USE W3SERVMD, ONLY: EXTCDE, STR_TO_UPPER

  USE W3METAMD

  IMPLICIT NONE

  PUBLIC

  LOGICAL, PRIVATE :: DEBUG = .FALSE. !< Control debug output to screen

  !> Meta-data input filename
  CHARACTER(LEN=*), PARAMETER :: FN_META = "ounfmeta.inp"

  !> String token for integer partition number
  CHARACTER(LEN=*), PARAMETER :: IPART_TOKEN  = "<IPART>"

  !> String token for partition descriptive string (space separated)
  CHARACTER(LEN=*), PARAMETER :: SPART_TOKEN  = "<SPART>"

  !> String token for partition descriptive string (underscore separated)
  CHARACTER(LEN=*), PARAMETER :: SPART_TOKEN_ = "<SPART_>"

  !> Type for storing WW3 netCDF metadata for a variable
  TYPE META_T
    REAL :: FSC                             !< Scaling factor for data
    REAL :: VMIN                            !< "valid_min" attribute
    REAL :: VMAX = UNSETR                   !< "valid_max" attribute
    CHARACTER(LEN=24)  :: UNITS = UNSETC    !< SI units for field
    CHARACTER(LEN=50)  :: ENAME = UNSETC    !< Field name used in output filename
    CHARACTER(LEN=80)  :: VARNM = UNSETC, & !< netCDF variable name
         VARNL = UNSETC    !< "long_name" attibute
    CHARACTER(LEN=120) :: VARNS = UNSETC, & !< "standard_name" attribute
         VARNG = UNSETC, & !< "globwave_name" attribute
         VARND = UNSETC    !< "direction_convention" attribute
    CHARACTER(LEN=512) :: VARNC = UNSETC    !< "comment attribute
    TYPE(META_LIST_T) :: EXTRA              !< List of user defined meta data

    ! For updating meta only:
    INTEGER :: IFI = 0, &           !< Group index to update
         IFJ = 0, &           !< Field index to update
         IFC = 1              !< Component index to update
    CHARACTER(LEN=6) :: FLDID = ''  !< Field ID to update
  ENDTYPE META_T

  !> Type for storage of meta data aggregated by component (NFIELD)
  TYPE FIELD_T
    TYPE(META_T), POINTER :: META(:) !< Pointer to meta data for field
  END TYPE FIELD_T

  !> Type for storage of meta data aggregated by field (IFI)
  TYPE GROUP_T
    TYPE(FIELD_T), ALLOCATABLE :: FIELD(:) !< Pointer to fields in group
  END TYPE GROUP_T

  !> Storage for meta data aggregated by group (IFJ)
  TYPE(GROUP_T), ALLOCATABLE :: GROUP(:)

  !> Storage for the Global meta data (free form)
  TYPE(META_LIST_T) :: GLOBAL_META

  !> Flag for using default (true) or user-defined (false) global meta data
  LOGICAL           :: FL_DEFAULT_GBL_META = .TRUE.

  ! Storage for coordinate reference system (CRS) metadata:
  CHARACTER(LEN=128) :: CRS_NAME = '' !< Coordinate reference system (CRS) name
  TYPE(META_LIST_T)  :: CRS_META !< Meta data list for CRS
  LOGICAL            :: CRS_IS_DEFAULT = .FALSE. !< True if CRS set by this module

  !> "coordinates" attribute - for defining auxiliary coordinates (for all variables)
  CHARACTER(LEN=256) :: COORDS_ATTR = ''

  !> Type for storing partitioned parameter template strings.
  !> Defined as a linked-list
  TYPE PART_TMPL_T
    CHARACTER(LEN=128)              :: TMPL         !< Placeholder
    CHARACTER(LEN=128), ALLOCATABLE :: PART_TEXT(:) !< Partition description
    INTEGER(KIND=2)                 :: NP           !< Num parts (max NOSWLL)
    TYPE(PART_TMPL_T), POINTER      :: NEXT         !< LinkedList pointer
  END TYPE PART_TMPL_T

  !> User-defined partitionted paratmeters template strings
  TYPE(PART_TMPL_T), POINTER :: PART_TMPL

  INTEGER            :: NCVARTYPE   !< NetCDF variable type (2=int, 3=real, 4=depends)
  CHARACTER(LEN=30)  :: DIRCOM      !< Directional convention comment
  CHARACTER(LEN=128) :: PARTCOM     !< Partitioning method comment
  CHARACTER(LEN=15)  :: SNAMEP(5)   !< Part. standard name templates

  !> Flag for vector (true) or direction/magnitude (false) convention
  !> for directional fields
  LOGICAL, PRIVATE   :: VECTOR
  LOGICAL            :: FLRTD = .FALSE. !< Flag for rototed pole grid

CONTAINS

  !/ ------------------------------------------------------------------- /
  !> @brief Allocates space for the META_T arrays and sets some defaults.
  !>
  !> @details By default, directional fields will be set up to output
  !>    a magnitude and direction field. Alternatively, if VEC is set to
  !>    True, then u/v vectors will be generated instead.
  !>
  !> @note - vector output is currently only implemented for the
  !>    "current" and "wind" fields.
  !>
  !> @param VEC Output vectors for directional fields rather than
  !>    direction/magnitude.
  !>
  !> @author Chris Bunney @date 09-Mar-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE INIT_META(VEC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/    22-Mar-2021 : Added vector flag                   ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Allocates space for the META_T arrays and sets some constants.
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    LOGICAL, INTENT(IN), OPTIONAL  :: VEC
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    LOGICAL :: FLGNML
    INTEGER :: I, J

    VECTOR = .TRUE.
    IF(PRESENT(VEC)) VECTOR = VEC
#ifdef W3_RTD
    ! Is the grid really rotated?
    IF ( POLAT < 90. ) FLRTD = .True.
#endif
#if defined W3_SMC && defined W3_RTD
    ! SMC type 3/4 outputs are currently on standard pole grid only
    IF(SMCOTYPE .EQ. 3 .OR. SMCOTYPE .EQ. 4) FLRTD = .FALSE.
#endif

    ! 1. Allocate nested GROUP, FIELD structure:
    ALLOCATE(GROUP(NOGRP))
    DO I = 1,NOGRP
      ALLOCATE(GROUP(I)%FIELD(NOGE(I)))
      DO J = 1,NOGE(I)
        ALLOCATE(GROUP(I)%FIELD(J)%META(3)) ! Hardcode to 3 components for the moment
      ENDDO
    ENDDO

    ! 1.1 Make sure partitioned template pointer is null (i.e. empty list)
    NULLIFY(PART_TMPL)

    ! 2. Set direction convention:
    DIRCOM = ""
#ifdef W3_RTD
    IF( FLRTD ) THEN
      IF ( FLAGUNR ) THEN
        DIRCOM = 'True North'
      ELSE IF ( .NOT. FLAGUNR ) THEN
        DIRCOM = 'Rotated Pole Grid North'
      ENDIF
    ENDIF
#endif

    !  Set partitioning method comment and standard name templates:
    IF( PTMETH .LE. 3 ) THEN
      SNAMEP(1) = 'wind'
      SNAMEP(2) = 'primary swell'
      SNAMEP(3) = 'secondary swell'
      SNAMEP(4) = 'tertiary swell'
      SNAMEP(5) = 'swell'
    ELSE
      SNAMEP(1) = 'wind'
      SNAMEP(2) = 'swell'
      SNAMEP(3:5) = ''
    ENDIF

    IF ( PTMETH .EQ. 1 ) THEN
      PARTCOM = "Wind sea and swells defined using topographic " //   &
           "partitions and partition wave-age cut-off "     //   &
           "(WWIII default scheme)"
    ELSE IF ( PTMETH .EQ. 2 ) THEN
      PARTCOM = "Wind sea and swells defined using topographic " //   &
           "partitions and spectral wave-age cut-off"
    ELSE IF ( PTMETH .EQ. 3 ) THEN
      PARTCOM = "Wave components defined using topographic "     //   &
           "partitions only"
    ELSE IF ( PTMETH .EQ. 4 ) THEN
      PARTCOM = "Wind sea and swell defined using spectral "     //   &
           "wave-age cut-off"
    ELSE IF ( PTMETH .EQ. 5 ) THEN
      WRITE(PARTCOM, '("Wave components defined using ",F5.3,'   //   &
           '"Hz spectral frequency cutoff")') PTFCUT
    ELSE
      WRITE(PARTCOM, '("PTM_",I1,"_Unknown")') PTMETH
    ENDIF

    ! 3. Set the default values for the OUNF netCDF meta data.
    CALL DEFAULT_META()

    ! Set the default coordiante reference system (if applicable)
    CALL DEFAULT_CRS_META()

    ! If the ounfmeta.inp exists, read this in to override defaults:
    INQUIRE(FILE=TRIM(FNMPRE)//"ounfmeta.inp", EXIST=FLGNML)
    IF(FLGNML) THEN
      CALL READ_META()
    ENDIF

  END SUBROUTINE INIT_META
  !
  !/ ------------------------------------------------------------------- /
  !> @brief De-allocates memory used for the META_T arrays
  !>
  !> @author Chris Bunney @date 09-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE TEARDOWN_META()
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     De-allocates memory used for the META_T arrays
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: I, J

    DO I = 1,NOGRP
      DO J = 1,NOGE(I)
        DEALLOCATE(GROUP(I)%FIELD(J)%META)
      ENDDO
      DEALLOCATE(GROUP(I)%FIELD)
    ENDDO
    DEALLOCATE(GROUP)

    CALL DEL_META_LIST(GLOBAL_META)
    CALL DEL_META_LIST(CRS_META)

  END SUBROUTINE TEARDOWN_META

  !/ ------------------------------------------------------------------- /
  !> @brief Reads the next valid line from the user meta input file.
  !>
  !> @details Lines are repeatedly read in from the input file until
  !>    a valid input line is reached. Blank lines and comment lines
  !>    (starting with $) are skipped.
  !>
  !>    If the end of file is reached before any valid line is read
  !>    then EOF is set to true.
  !>
  !>    If the next valid line is a new section marker (META or TEMPLATE)
  !>    then the NEW_SECTION flag is set to true.
  !>
  !> @param[in]     NDMI   Unit number of input file
  !> @param[out]    BUF    Next input line read from file
  !> @param[in,out] ILINE  Line number of file
  !> @param[out]    EOF    True if end-of-file is reached.
  !> @param[out]    NEW_SECTION  True if new section marker found
  !>
  !> @author Chris Bunney @date 09-Nov-2020
  !/ ------------------------------------------------------------------- /

  SUBROUTINE NEXT_LINE(NDMI, BUF, ILINE, EOF, NEW_SECTION)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Reads the next valid line from the user meta input file.
    !
    !  2. Method :
    !     Lines are repeatedly read in from the input file until
    !     a valid input line is reached. Blank lines and comment lines
    !     (starting with $) are skipped.
    !
    !     If the end of file is reached before any valid line is read
    !     then EOF is set to true.
    !
    !     If the next valid line is a new section marker (META or TEMPLATE)
    !     then the NEW_SECTION flag is set to true.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDMI    Int.  I  Unit number of input file
    !       BUF    Char.  O  Next input line read from file
    !       ILINE   Int. I/O Line number of file
    !       EOF    Bool.  O  True if end-of-file is reached.
    !       NEW_SECTION
    !              Bool.  O  True if new section marker found
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    INTEGER, INTENT(IN)       :: NDMI
    CHARACTER(*), INTENT(OUT) :: BUF
    INTEGER, INTENT(INOUT)    :: ILINE
    LOGICAL, INTENT(OUT)      :: EOF
    LOGICAL, INTENT(OUT), OPTIONAL :: NEW_SECTION
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IERR
    CHARACTER(LEN=10) :: TEST

    EOF = .FALSE.

    ! Keep reading from file until we read a line that is not:
    !   - a blank line
    !   - a comment line (starting with $)
    !   - the end of the file
    DO
      READ(NDMI, '(A)', iostat=IERR, err=101, end=100) BUF

      ILINE = ILINE + 1

      ! Remove any tab characters from buffer (replace with a space)
      CALL NOTABS(BUF)

      ! Empty line?
      IF(TRIM(BUF) == '') THEN
        IF(DEBUG) WRITE(*,'(I5,1X,A20)') ILINE, '[blank line]'
        CYCLE
      ENDIF

      IF(TRIM(BUF) == "$ DEBUG ON") THEN
        WRITE(*,'(I5,1X,A20)') ILINE, '[DEBUG ON]'
        DEBUG = .TRUE.
        CYCLE
      ENDIF

      IF(TRIM(BUF) == "$ DEBUG OFF") THEN
        WRITE(*,'(I5,1X,A20)') ILINE, '[DEBUG OFF]'
        DEBUG = .FALSE.
        CYCLE
      ENDIF

      ! Read first token on line:
      READ(BUF, *) TEST

      ! Check for comment:
      IF(TEST(1:1) == "$" .OR. TRIM(BUF) == '') THEN
        IF(DEBUG) WRITE(*,'(I5,1X,A20)') ILINE, '[comment line]'
        CYCLE
      ENDIF

      ! Check if is section header
      IF(PRESENT(NEW_SECTION)) THEN
        CALL STR_TO_UPPER(TEST)
        SELECT CASE(TEST)
        CASE ("META", "TEMPLATE", "CRS")
          NEW_SECTION = .TRUE.
        CASE DEFAULT
          NEW_SECTION = .FALSE.
        END SELECT
      ENDIF

      ! Anything else can be considered the "next line"
      RETURN
    ENDDO

    !/    Escape locations
    !
    !     End of file
100 CONTINUE
    BUF = ''
    EOF = .TRUE.
    RETURN
    !
    !     I/O Error
101 CONTINUE
    WRITE(NDSE, 1000) FN_META, ILINE, IERR
    CALL EXTCDE(10)
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     ERROR READING METADATA FILE'/                    &
         '     FILENAME = ', A /                                &
         '     LINE NO = ', I5 /                                &
         '     IOSTAT =',I5 /)
    !
  END SUBROUTINE NEXT_LINE

  !/ ------------------------------------------------------------------- /
  !> @brief Replaces tab characters in a string with a space.
  !>
  !> @remark Assumes ASCII encoding (Tab character is ASCII value 9)
  !>
  !> @param[in,out] STR Character string to process
  !>
  !> @author Chris Bunney @date 02-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE NOTABS(STR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    02-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Replaces tab characters in a string with a space.
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       STR    Char.  I/O  Character string to process
    !     ----------------------------------------------------------------
    !
    !  3. Remarks :
    !
    !     Assumes ASCII encoding! Tab character is ASCII value 9.
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    CHARACTER(*), INTENT(INOUT) :: STR
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, PARAMETER :: ASCII_TAB = 9
    INTEGER :: SLEN
    INTEGER :: I
    !
    SLEN = LEN_TRIM(STR)

    DO I=1,SLEN
      IF(ICHAR(STR(I:I)) == ASCII_TAB) THEN
        STR(I:I) = ' '
      ENDIF
    ENDDO

  END SUBROUTINE NOTABS

  !/ ------------------------------------------------------------------- /
  !> @brief Replaces single characters in a string.
  !>
  !> @returns A new string
  !>
  !> @param[in]  STR  Character string to process
  !> @param[in]  C    Character to search for
  !> @param[in]  REP  Character to substitute
  !>
  !> @author Chris Bunney @date 02-Feb-2021
  !/ ------------------------------------------------------------------- /
  FUNCTION REPLACE_CHAR(STR, C, REP) RESULT(OSTR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Fev-2021 |
    !/                  +-----------------------------------+
    !/
    !/    02-Feb-2021 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Replaces single characters in a string. Returns a new string,
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       STR    CharArr  I  Character string to process
    !       C      Char     I  Character to search for
    !       REP    Char     I  Character to substitute
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    CHARACTER(*)        :: STR
    CHARACTER           :: C, REP
    CHARACTER(LEN(STR)) :: OSTR
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: I

    OSTR = TRIM(STR)
    DO
      I = INDEX(TRIM(OSTR), C)
      IF(I .LE. 0) EXIT
      OSTR(I:I) = REP
    ENDDO

  END FUNCTION REPLACE_CHAR

  !/ ------------------------------------------------------------------- /
  !> @brief Reads meta data entries from the ountmeta.inp file
  !>
  !> @details This is the main entry routine for parsing the ounfmeta.inp
  !>    file. Values read from the file will be used to update or add to
  !>    the default meta data values set in the default_meta()
  !>    subroutine.
  !>
  !> @author Chris Bunney @date 26-Jan-2021
  !/ ------------------------------------------------------------------- /
  SUBROUTINE READ_META()
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         26-Jan-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/    26-Jan-2021 : Added TP and alternative dir/mag    ( version 7.12 )
    !/                  metadata for directional fields.
    !/
    !
    !  1. Purpose :
    !
    !     Reads meta data entries from the ountmeta.inp file and update
    !     default values set via the DEFAULT_META subroutine.
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER ::  IERR, I, NDMI
    CHARACTER(LEN=256) :: BUF
    TYPE(META_T), POINTER :: PMETA
    CHARACTER(LEN=32) :: TEST, TESTU

    INTEGER :: IFI, IFJ, IFC
    INTEGER :: ILINE, MCNT
    LOGICAL :: EOF

    NDMI = 60
    ILINE = 0
    MCNT = 0

    OPEN(UNIT=NDMI, FILE=TRIM(FNMPRE)//TRIM(FN_META),                &
         STATUS="OLD", IOSTAT=IERR)

    IF(IERR .NE. 0) THEN
      WRITE(NDSE, 5010) TRIM(FNMPRE)//TRIM(FN_META), IERR
      CALL EXTCDE(10)
    ENDIF

    ! Loop over file, skipping comments or blank lines, until we find
    ! a META line.
    DO
      CALL NEXT_LINE(NDMI, BUF, ILINE, EOF)
      IF(EOF) EXIT

      ! Read first token on line:
      READ(BUF, *) TEST

      ! A new meta-data section will start with the keyword "META"
      TESTU = TEST
      CALL STR_TO_UPPER(TESTU)
      IF(TESTU == "META") THEN
        MCNT = MCNT + 1

        IF(DEBUG) WRITE(*,'(I5,1X,A20,1X,A)') ILINE, '[META header]', TRIM(BUF)

        ! Get the IFI, IFJ, IFC values from the header:
        I = INDEX(BUF, TRIM(TEST)) + 4 ! Handles lower/mixed-case META keyword
        CALL DECODE_HEADER(BUF(I:), ILINE, IFI, IFJ, IFC)
        IF(IFI .EQ. -1) THEN
          WRITE(NDSE, 5011) TRIM(BUF(I:)), TRIM(FN_META), ILINE
          CALL EXTCDE(10)
        ENDIF

        ! IFI = 999 is a section for the "global" meta data
        IF(IFI .EQ. 999) THEN
          CALL READ_FREEFORM_META_LIST(NDMI, ILINE, GLOBAL_META)
          CYCLE
        ENDIF

        ! Error checking on size of IFJ, ICOMP, IPART.
        IF(IFI .LT. 1 .OR. IFI .GT. NOGRP) THEN
          WRITE(NDSE,5013) NOGRP, TRIM(FN_META), ILINE
          CALL EXTCDE(1)
        ENDIF
        IF(IFJ .LT. 1 .OR. IFJ .GT. NOGE(IFI)) THEN
          WRITE(NDSE,5014) NOGE(IFI), TRIM(FN_META), ILINE
          CALL EXTCDE(1)
        ENDIF
        IF(IFC .LT. 1 .OR. IFC .GT. 3) THEN
          WRITE(NDSE,5015) TRIM(FN_META), ILINE
          CALL EXTCDE(1)
        ENDIF

        ! Select correct variable metadata entry:
        PMETA => GROUP(IFI)%FIELD(IFJ)%META(IFC)

        ! Update the metadata with values from file:
        CALL READ_META_PAIRS(NDMI, PMETA, ILINE)

      ELSE IF(TESTU == "TEMPLATE") THEN
        BACKSPACE(NDMI) ! We will reprocess this line
        CALL READ_PART_TMPL(NDMI, ILINE)
        CYCLE

      ELSE IF(TESTU == "CRS") THEN
        BACKSPACE(NDMI) ! We will reprocess this line
        CALL READ_CRS_META(NDMI, ILINE)
        CYCLE

      ELSE
        ! Anything else is a syntax error
        WRITE(NDSE, 5012) TRIM(FN_META), ILINE, TRIM(BUF)
        CALL EXTCDE(10)
      ENDIF
    ENDDO

    CLOSE(NDMI)
    !WRITE(*, 5000) MCNT, N_GBLMETA, N_CRSMETA
    RETURN
    !
5000 FORMAT(/' Read in: ',I3,' variable metadata entries' /          &
         '     and: ',I3,' global meta data entries' /           &
         '     and: ',I3,' CRS meta data entries' /)
    !
5010 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     ERROR OPENING METADATA FILE'/                    &
         '     FILENAME = ', A /                                &
         '     IOSTAT =', I5 /)
    !
5011 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     UNKNOWN FIELD ID: ',A /                          &
         '     FILENAME = ', A /                                &
         '     LINE =', I5 /)
    !
5012 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     SYNTAX ERROR ' /                                 &
         '     FILENAME = ', A /                                &
         '     LINE =', I5 /                                    &
         '  => ', A /)
    !
5013 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : ' /          &
         '     IFI value should be in range 1,',I2 /            &
         '     FILENAME = ', A /                                &
         '     LINE =', I5 /                                    &
         '  => ', A /)
    !
5014 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : ' /          &
         '     IFJ value should be in range 1,',I2 /            &
         '     FILENAME = ', A /                                &
         '     LINE =', I5 /                                    &
         '  => ', A /)
    !
5015 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : ' /          &
         '     IFC value should be in range 1,3' /              &
         '     FILENAME = ', A /                                &
         '     LINE =', I5 /                                    &
         '  => ', A /)
    !
  END SUBROUTINE READ_META

  !/ ------------------------------------------------------------------- /
  !> @brief Decode the META header line.
  !>
  !> @details The internal WW3 field can be specified either as an
  !>    [IFI, IFJ] integer combination, or a field ID tag (such as "HS").
  !>
  !>    Both forms can also specify an optional component (IFC) integer
  !>    value for multi-component fields (defaults to 1).
  !>
  !>    Field name ID tags are case-insensitive, HS == hs == Hs.
  !>
  !> @param[in]  BUF    Input header string (without leading META tag)
  !> @param[in]  ILINE  Line number (for error reporting)
  !> @param[out] IFI    Output group number
  !> @param[out] IFJ    Output field number
  !> @param[out] IFC    Component number (defaults to 1)
  !>
  !> @author Chris Bunney @date 02-Feb-2021
  !/ ------------------------------------------------------------------- /
  SUBROUTINE DECODE_HEADER(BUF, ILINE, IFI, IFJ, IFC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Feb-2021 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/    02-Feb-2021 : NODEFAULT option for Global meta    ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Decode the META header line.
    !
    !  2. Method:
    !
    !     The internal WW3 field can be specified either as an [IFI, IFJ]
    !     integer combination, or a field ID tag (such as "HS").
    !
    !     Both forms can also specify an optional component (IFC) integer
    !     value for multi-component fields (defaults to 1).
    !
    !     Field name ID tags are case-insensitive, HS == hs == Hs.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       BUF    Char.  I  Input header string (without leading META tag)
    !       ILINE   Int.  I  Line number (for error reporting)
    !       IFI     Int.  O  Output group number
    !       IFJ     Int.  O  Output field number
    !       IFC     Int.  O  Component number (defaults to 1)
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    USE W3IOGOMD, ONLY: W3FLDTOIJ

    IMPLICIT NONE

    CHARACTER(*), INTENT(IN) :: BUF
    INTEGER, INTENT(IN) :: ILINE
    INTEGER, INTENT(OUT) :: IFI, IFJ, IFC
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IERR, I
    CHARACTER(LEN=10) :: FLD, OFLD, OPT

    IFI = 0
    IFJ = 1
    IFC = 1
    FLD = ''

    ! Is first value an int?
    READ(BUF, *, IOSTAT=IERR) IFI
    IF(IERR .EQ. 0) THEN
      ! Try reading 3 values:
      READ(BUF, *, iostat=IERR) IFI, IFJ, IFC
      IF(IERR .NE. 0) THEN
        ! Try just two values:
        READ(BUF, *, IOSTAt=IERR) IFI, IFJ
      ENDIF
    ELSE
      ! Try reading field ID plus component
      READ(BUF, *, IOSTAT=IERR) FLD, IFC

      IF(ierr .NE. 0) THEN
        ! Try just fldid
        READ(BUF, *, IOSTAT=IERR) FLD
      ENDIF
    ENDIF

    IF(IERR .NE. 0) THEN
      WRITE(NDSE, 6000) TRIM(FN_META), ILINE, TRIM(BUF)
      CALL EXTCDE(10)
    ENDIf

    OFLD = FLD
    CALL STR_TO_UPPER(FLD) ! field names are case insensitive

    ! If string value (FLDID), then decode into IFI,IFJ value:
    IF(FLD /= '') THEN
      ! Special case for "global" attributes
      IF(TRIM(FLD) == "GLOBAL") THEN
        IF(DEBUG) WRITE(*,'(6X,A20,1X,A)') '[GLOBAL meta sec.]', TRIM(BUF)
        IFI = 999  ! Marker for global section

        ! check for any options:
        I = INDEX(BUF, TRIM(OFLD)) + LEN_TRIM(OFLD)
        OPT = ADJUSTL(BUF(I:))
        CALL STR_TO_UPPER(OPT)
        SELECT CASE(TRIM(OPT))
        CASE("")
          CONTINUE ! no option
        CASE("NODEFAULT")
          FL_DEFAULT_GBL_META = .FALSE.
          IF(DEBUG) WRITE(*,'(6X,A20,1X,A)') '[GLOBAL meta]', 'Defaults disabled'
        CASE DEFAULT
          WRITE(NDSE, *) "Unknown GLOBAL extra option: [", TRIM(OPT), "]"
        END SELECT
      ELSE
        IF(DEBUG) WRITE(*,'(6X,A20,1X,A)') '[Decoding field ID]', TRIM(BUF)
        CALL W3FLDTOIJ(FLD, IFI, IFJ, 1, 1, 1)
      ENDIF
    ENDIF

    IF(DEBUG) WRITE(*,'(6X,A20,1X,I4,2I2)') '[IFI, IFJ, IFC]', IFI,IFJ,IFC
    !
6000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     SYNTAX ERROR IN SECTION HEADER. ' /              &
         '     FILENAME = ', A /                                &
         '     LINE NO =', I5 /                                 &
         '  => ', A /)
    !
  END SUBROUTINE DECODE_HEADER

  !/ ------------------------------------------------------------------- /
  !> @brief Reads in attribute name/value pairs and updates the relevant
  !>    values in the META type.
  !>
  !> @details Keeps looping over input lines in file until next META section
  !>    or EOF is found. Splits meta pairs on the = character.
  !>
  !>    Note - the "extra" metadata pair can also provide a variable
  !>    type ("c", "i", or "r"; for character, int or real respectively)
  !>
  !> @param[in]      NDMI  Unit number of metadata input file
  !> @param[out]     META  Pointer to META type
  !> @param[in,out]  ILINE Current line number in file
  !>
  !> @author Chris Bunney @date 09-11-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE READ_META_PAIRS(NDMI, META, ILINE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Reads in attribute name/value pairs and updates the relevant
    !     values in the META type.
    !
    !  2. Method:
    !
    !     Keeps looping over input lines in file until next META section
    !     or EOF is found. Splits meta pairs on the = character.
    !
    !     Note - the "extra" metadata pair can also provide a variable
    !     type ("c", "i", or "r"; for character, int or real respectively)
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDMI      Int.   I  Unit number of metadata input file
    !       META  Int.Ptr.   O  Pointer to META type
    !       ILINE     Int. I/O  Current line number in file
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    INTEGER, INTENT(IN)                  :: NDMI
    TYPE(META_T), INTENT(INOUT), POINTER :: META
    INTEGER, INTENT(INOUT)               :: ILINE
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !
    CHARACTER(LEN=256) :: BUF
    CHARACTER(LEN=128) :: ATTN, ATTV, TMP
    CHARACTER(LEN=16)  :: ATT_TYPE!, TEST
    INTEGER            :: I, IERR
    REAL               :: R
    LOGICAL            :: EOF, NEW
    TYPE(META_PAIR_T)  :: EXTRA

    ! Keep reading lines until we hit EOF or anoter META keyword
    DO
      CALL NEXT_LINE(NDMI, BUF, ILINE, EOF, NEW_SECTION=NEW)
      IF(EOF) THEN
        BACKSPACE(NDMI)
        RETURN
      ENDIF

      IF(NEW) THEN
        IF(DEBUG) WRITE(*,'(I5,1X,A20)') ILINE, '[--end of section--]'
        ILINE = ILINE - 1
        BACKSPACE(NDMI)
        EXIT
      ENDIF

      IF(DEBUG) WRITE(*,'(I5,1X,A20,1X,A)') ILINE, '[META pair]', TRIM(BUF)

      ! Meta data should be formatted as "attr_name = attr_value"
      I = INDEX(BUF, "=")
      IF( I .LT. 1 ) THEN
        WRITE(NDSE, 7000) FN_META, ILINE, TRIM(BUF)
        CALL EXTCDE(10)
      ENDIF

      ATTN = ADJUSTL(BUF(1:I-1))
      ATTV = ADJUSTL(BUF(I+1:))

      ! Some compilers won't read an "empty" string unless quoted:
      IF(TRIM(ATTV) == '') THEN
        ATTV='""'
      ENDIF

      IERR = 0
      SELECT CASE(TRIM(attn))
        ! Character variables
        ! Note: Using internal reads will allow the use of quote marks in strings
      CASE("varnm")
        READ(attv, *, IOSTAT=IERR) META%VARNM

      CASE("ename")
        READ(attv, *, IOSTAT=IERR) META%ENAME

      CASE("standard_name", "varns")
        READ(attv, *, IOSTAT=IERR) META%VARNS

      CASE("long_name", "varnl")
        READ(attv, *, IOSTAT=IERR) META%VARNL

      CASE("globwave_name", "varng")
        READ(attv, *, IOSTAT=IERR) META%VARNG

      CASE("direction_reference", "dir_ref", "varnd")
        READ(attv, *, IOSTAT=IERR) META%VARND

      CASE("comment", "varnc")
        READ(attv, *, IOSTAT=IERR) META%VARNC

      CASE("units")
        READ(attv, *, IOSTAT=IERR) META%UNITS

        ! Real variables
      CASE("valid_min", "vmin")
        READ(attv, *, IOSTAT=IERR) META%VMIN

      CASE("valid_max", "vmax")
        READ(attv, *, IOSTAT=IERR) META%VMAX

      CASE("scale_factor", "fsc")
        READ(attv, *, IOSTAT=IERR) META%FSC

        ! Default case will be the "extra" meta data variable
      CASE DEFAULT
        TMP = ATTV
        CALL GET_ATTVAL_TYPE(TMP, ILINE, ATTV, ATT_TYPE)

        IF(DEBUG) THEN
          WRITE(*,'(I5,1X,A20,1X,A)') ILINE, '[META extra]', &
               TRIM(attn)//' = '//TRIM(attv)//' (type: '//TRIM(att_type)//")"
        ENDIF

        EXTRA%ATTNAME = TRIM(attn)
        EXTRA%ATTVAL = TRIM(attv)
        EXTRA%TYPE = TRIM(att_type)
        CALL META_LIST_APPEND(META%EXTRA, EXTRA)

      END SELECT

      IF(IERR /= 0) THEN
        WRITE(NDSE, 7002) FN_META, ILINE, TRIM(BUF)
        CALL EXTCDE(10)
      ENDIF

    ENDDO

    RETURN
    !
7000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     SYNTAX ERROR IN METADATA FILE ' /                &
         '     SHOULD BE "attr_name = attr_value" ' /           &
         '     FILENAME = ', A /                                &
         '     LINE NO =', I5 /                                 &
         '  => ', A /)
    !
7002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     IO ERROR READING ATTRIBUTE' /                    &
         '     FILENAME = ', A /                                &
         '     LINE NO =', I5 /                                 &
         '  => ', A /)
    !
  END SUBROUTINE READ_META_PAIRS

  !/ ------------------------------------------------------------------- /
  !> @brief Gets the attribute value and optional variable type from
  !>    the passed in string.
  !>
  !> @details If two freeform values can be read from the input string,
  !>    it is assumed to be a value and type, otherwise if only one value
  !>    can be read the type is assumed to be "character".
  !>
  !>    It is important to quote strings if they contain spaces.
  !>
  !>    Valid types are "c" "r/f", and "i" for character, real/float and
  !>     integer values.
  !>
  !> @param[in]   BUF       Input string to process
  !> @param[in]   ILINE     Line number (for error reporting)
  !> @param[out]  ATTV      Attribute value
  !> @param[out]  ATT_TYPE  Attribute type
  !>
  !> @author Chris Bunney @date 09-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE GET_ATTVAL_TYPE(BUF, ILINE, ATTV, ATT_TYPE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Gets the attribute value and optional variable type from
    !     the passed in string.
    !
    !  2. Method:
    !
    !     If two freeform values can be read from the input string, it is
    !     assumed to be a value and type, otherwise if only one value can
    !     be read the type is assumed to be "character".
    !
    !     It is important to quote strings if they contain spaces.
    !
    !     Valid types are "c" "r/f", and "i" for character, real/float and
    !     integer values.

    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       BUF      Char.   I  Input string to process
    !       ILINE    Int.    I  Line number (for error reporting)
    !       ATTV     Char.   O  Attribute value
    !       ATT_TYPE Char.   O  Attribute type
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    CHARACTER(*), INTENT(IN)  :: BUF
    INTEGER, INTENT(IN)       :: ILINE
    CHARACTER(*), INTENT(OUT) :: ATTV, ATT_TYPE
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !
    REAL :: R
    INTEGER :: I, IERR

    ! Get attribute and type (default to "c" if no type set)
    ATT_TYPE = 'c'
    ATTV = ''
    READ(BUF, *, IOSTAT=IERR) ATTV, ATT_TYPE
    IF(IERR /= 0) READ(BUF, *, IOSTAT=IERR) ATTV

    ! Check attr values are valid w.r.t. attr type
    SELECT CASE(TRIM(att_type))

    CASE("i")
      READ(attv, *, iostat=ierr) i
      IF(ierr .ne. 0) then
        WRITE(NDSE, 8001) "INTEGER", TRIM(FN_META), ILINE, TRIM(ATTV)
        CALL EXTCDE(10)
      ENDIF

    CASE("r", "f")
      READ(attv, *, iostat=ierr) r
      IF(ierr .ne. 0) THEN
        WRITE(NDSE, 8001) "REAL/FLOAT", TRIM(FN_META), ILINE, TRIM(ATTV)
        CALL EXTCDE(10)
      ENDIF

    CASE("c")
      ! Always ok.

    CASE DEFAULT
      WRITE(NDSE, 8002) TRIM(FN_META), ILINE, TRIM(BUF)
      CALL EXTCDE(10)

    END SELECT
    !
8001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     VALUE IS NOT A VALID ', A /                      &
         '     FILENAME = ', A /                                &
         '     LINE NO =', I5 /                                 &
         '  => ', A /)
    !
8002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     ATTRIBUTE TYPE SHOULD BE ONE OF [c,i,r] '/       &
         '     FILENAME = ', A /                                &
         '     LINE NO =', I5 /                                 &
         '  => ', A /)
    !
  END SUBROUTINE GET_ATTVAL_TYPE

  !/ ------------------------------------------------------------------- /
  !> @brief Reads in freeform attribute name/value pairs.
  !>
  !> @details Keeps looping over input lines in file until next section
  !>    or EOF is found. Splits meta pairs on the `=` character.
  !>
  !>    Freeform metadata pairs can also provide a variable type
  !>    ("c", "i", or "r"; for character, int or real respectively).
  !>    String values with spaces should be quoted.
  !>
  !> @param[in]      NDMI      Unit number of metadata input file
  !> @param[in,out]  ILINE     Current line number in file
  !> @param[in,out]  METALIST  A META_LIST_T object to append to
  !>
  !> @author Chris Bunney @date 16-Dec-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE READ_FREEFORM_META_LIST(NDMI, ILINE, METALIST)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         16-Dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    16-Dec-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Reads in freeform attribute name/value pairs.
    !
    !  2. Method:
    !
    !     Keeps looping over input lines in file until next section
    !     or EOF is found. Splits meta pairs on the = character.
    !
    !     Freeform metadata pairs can also provide a variable type
    !     ("c", "i", or "r"; for character, int or real respectively).
    !     String values with spaces should be quoted.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDMI     Char.   I  Unit number of metadata input file
    !       ILINE     Int. I/O  Current line number in file
    !       METALIST Type. I/O  A META_LIST_T object to append to
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    INTEGER, INTENT(IN)               :: NDMI
    INTEGER, INTENT(INOUT)            :: ILINE
    TYPE(META_LIST_T), INTENT(INOUT)  :: METALIST

    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    CHARACTER(LEN=256) :: BUF
    CHARACTER(LEN=128) :: ATTN, ATTV, TMP
    CHARACTER(LEN=16)  :: ATT_TYPE
    INTEGER            :: I, IERR
    REAL               :: R
    LOGICAL            :: EOF, NEW
    TYPE(META_PAIR_T)  :: META
    !
    ! Keep reading lines until we hit EOF or anoter META keyword
    DO
      CALL NEXT_LINE(NDMI, BUF, ILINE, EOF, NEW_SECTION=NEW)
      IF(EOF) THEN
        BACKSPACE(NDMI)
        RETURN
      ENDIF

      IF(NEW) THEN
        IF(DEBUG) WRITE(*,'(I5,1X,A20)') ILINE, '[--end of section--]'
        ILINE = ILINE - 1
        BACKSPACE(NDMI)
        EXIT
      ENDIF

      ! Split attr name/value pair
      I = INDEX(BUF, "=")
      IF( I .LT. 1 ) THEN
        WRITE(NDSE, 9000) TRIM(FN_META), ILINE, TRIM(BUF)
        CALL EXTCDE(10)
      ENDIF

      ATTN = ADJUSTL(BUF(1:I-1))
      TMP = ADJUSTL(BUF(I+1:))

      ! Get type, if set:
      CALL GET_ATTVAL_TYPE(TMP, ILINE, ATTV, ATT_TYPE)

      IF(DEBUG) THEN
        WRITE(*,'(I5,1X,A20,1X,A)') ILINE, '[FREEFORM meta]', &
             TRIM(attn)//' = '//TRIM(attv)//' (type: '//TRIM(att_type)//")"
      ENDIF


      META%ATTNAME = TRIM(ATTN)
      META%ATTVAL = TRIM(ATTV)
      META%TYPE = TRIM(ATT_TYPE)

      CALL META_LIST_APPEND(METALIST, META)

    ENDDO
    !
9000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     SYNTAX ERROR IN METADATA FILE ' /                &
         '     SHOULD BE "attr_name = attr_value" ' /           &
         '     FILENAME = ', A /                                &
         '     LINE NO =', I5 /                                 &
         '  => ', A /)
    !
  END SUBROUTINE READ_FREEFORM_META_LIST

  !/ ------------------------------------------------------------------- /
  !> @brief Reads in metadata for the coordinate reference system (CRS).
  !>
  !> @details The "grid_mapping_name" must be supplied as an attribute.
  !>
  !> @param[in]     NDMI   Unit number of metadata input file
  !> @param[in,out] ILINE  Current line number in file
  !>
  !> @author Chris Bunney @date 07-Dec-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE READ_CRS_META(NDMI, ILINE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         07-Dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    07-Dec-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Reads in metadata for the coordinate reference system (CRS)
    !     scalar variable. The "grid_mapping_name" must be supplied as
    !     an attribute.
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDMI     Char.   I  Unit number of metadata input file
    !       ILINE     Int. I/O  Current line number in file
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    INTEGER, INTENT(IN)     :: NDMI
    INTEGER, INTENT(INOUT)  :: ILINE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    CHARACTER(LEN=128) :: BUF, PREV_NAME
    INTEGER            :: I, IERR

    PREV_NAME = CRS_NAME

    ! Re-read header line (we only want the second field)
    READ(NDMI, '(A)') BUF
    READ(BUF, *, IOSTAT=IERR) CRS_NAME, CRS_NAME
    IF(IERR /= 0 ) THEN
      WRITE(NDSE,1000)
      WRITE(NDSE,2000) TRIM(FN_META), ILINE, TRIM(BUF)
      CALL EXTCDE(10)
    ENDIF
    IF(DEBUG) WRITE(*,'(I5,1X,A20,1X,A)') ILINE, '[CRS id]', TRIM(CRS_NAME)

    IF(CRS_META%N .NE. 0) THEN
      IF(CRS_IS_DEFAULT) THEN
        WRITE(NDSE,1001) TRIM(PREV_NAME)
        CRS_IS_DEFAULT = .FALSE.
      ELSE
        WRITE(NDSE,1002) TRIM(PREV_NAME)
      ENDIF
      WRITE(NDSE,2000) TRIM(FN_META), ILINE, TRIM(BUF)
      CALL DEL_META_LIST(CRS_META)
    ENDIF

    CALL READ_FREEFORM_META_LIST(NDMI, ILINE, CRS_META)

    ! Check that "grid_mapping_name" is defined
    IF(.NOT.  META_LIST_HAS_ATTR(CRS_META, "grid_mapping_name")) THEN
      WRITE(NDSE, 1003)
      WRITE(NDSE, 2000) TRIM(FN_META), ILINE, ""
      CALL EXTCDE(10)
    ENDIF

    RETURN

1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     ERROR READING CRS HEADER - MISSING CRS NAME?' )
    !
1001 FORMAT (/' *** WARNING : USER DEFINED CRS SECTION WILL ' /      &
         '     OVERIDE DEFAULT CRS DEFINITION FOR GRID' /       &
         '     PREV CRS = ', A )
    !
1002 FORMAT (/' *** WARNING : DUPLICATE CRS SECTION WILL ' /         &
         '     OVERRIDE PREVIOUS CRS DEFINITION' /              &
         '     PREV CRS = ', A )
    !
1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     CRS SECTION DOES NOT CONTAIN MANDATORY '/        &
         '     ATTRIBUTE "grid_mapping_name"' )

2000 FORMAT ( '     FILENAME = ', A /                                &
         '     LINE NO  = ', I5 /                               &
         '  => ', A /)
    !

  END SUBROUTINE READ_CRS_META

  !/ ------------------------------------------------------------------- /
  !> @brief Set up a default coordinate reference system for the grid
  !>
  !> @details The default coordinate reference system (CRS) will be defined
  !>    based on the type of grid the model is formulated on, e.g.
  !>    regular lat-lon, rotated pole, etc.
  !>
  !> @remark See "Grid Mappings" section of CF conventions:
  !>  - https://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/ch05s06.html
  !>  - https://cfconventions.org/Data/cf-conventions/cf-conventions-1.7/build/apf.html
  !>
  !> @author Chris Bunney @date 25-May-2021
  !/ ------------------------------------------------------------------- /
  SUBROUTINE DEFAULT_CRS_META()
    IMPLICIT NONE

    TYPE(META_PAIR_T) :: META

    IF(FLRTD) THEN
#ifdef W3_RTD
      ! Rotated pole location
      CRS_NAME = 'rotated_pole'
      CALL META_LIST_APPEND(CRS_META,                                 &
           'grid_mapping_name', 'rotated_latitude_longitude')
      CALL META_LIST_APPEND(CRS_META,                                 &
           'grid_north_pole_latitude', POLAT)
      CALL META_LIST_APPEND(CRS_META,                                 &
           'grid_north_pole_longitude', POLON)
      CRS_IS_DEFAULT = .TRUE.
#endif
    ELSE IF(GTYPE .EQ. UNGTYPE) THEN
      !       ! What do we want for unstructure grids?
    ELSE
      ! Lat/lon grid
      CRS_NAME = 'crs'
      CALL META_LIST_APPEND(CRS_META,                                &
           'grid_mapping_name', 'latitude_longitude')
      ! TODO: Default to a spherical Earth?
      CALL META_LIST_APPEND(CRS_META,                                &
           'semi_major_axis', 6371000.0)
      CALL META_LIST_APPEND(CRS_META,                                &
           'inverse_flattening', 0.0)
    ENDIF

  END SUBROUTINE DEFAULT_CRS_META

  !/ ------------------------------------------------------------------- /
  !> @brief Get the meta data for a particular field.
  !>
  !> @details The required field is specified using the group (IFI) and
  !>    field (IFJ) index. Optionally, the component (ICOMP) and partition
  !>    (IPART) numbers can be specified for vector/tensor or partitioned
  !>    parameter fields. If not specified, these default to 1.

  !>    A copy of the meta-data is returned, rather than a pointer.
  !>    This is because in the case of paritioned parameters, the metadata
  !>    will be updated with the partition number.
  !>
  !> @param[in]  IFI    Output group number
  !> @param[in]  IFJ    Output field number
  !> @param[in]  ICOMP  Component number (defaults to 1)
  !> @param[in]  IPART  Partition number (defaults to 1)
  !>
  !> @author Chris Bunney @date 02-Nov-2020
  !/ ------------------------------------------------------------------- /
  FUNCTION GETMETA(IFI, IFJ, ICOMP, IPART) RESULT(META)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Returns a META_T type containig the netCDF matadata for the
    !     requested field
    !
    !  2. Method :
    !
    !     A copy of the meta-data is returned, rather than a pointer. This
    !     is because in the case of paritioned parameters, the metadata
    !     will be updated with the partition number.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IFI     Int.  I  Output group number
    !       IFJ     Int.  I  Output field number
    !       ICOMP   Int.  I  Component number (defaults to 1)
    !       IPART   Int.  I  Partition number (defaults to 1)
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: IFI, IFJ
    INTEGER, INTENT(IN), OPTIONAL :: ICOMP, IPART
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IFP, IFC
    TYPE(META_T) :: META ! Not pointer as we might need to modify it

    IFC = 1
    IFP = 1
    IF(PRESENT(ICOMP)) IFC = ICOMP
    IF(PRESENT(IPART)) IFP = IPART

    ! Error checking on size of IFJ, ICOMP, IPART.
    IF(IFI .LT. 1 .OR. IFI .GT. NOGRP) THEN
      WRITE(NDSE,1000) NOGRP
      CALL EXTCDE(1)
    ENDIF
    IF(IFJ .LT. 1 .OR. IFJ .GT. NOGE(IFI)) THEN
      WRITE(NDSE,1001) NOGE(IFI)
      CALL EXTCDE(1)
    ENDIF
    IF(IFC .LT. 1 .OR. IFC .GT. 3) THEN
      WRITE(NDSE,1002)
      CALL EXTCDE(1)
    ENDIF

    META = META_DEEP_COPY(GROUP(IFI)%FIELD(IFJ)%META(IFC))

    ! For partitioned data, expand in the partition number:
    IF(IFI .EQ. 4) THEN
      CALL ADD_PARTNO(META, IFP)
    ENDIF

    RETURN

1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '            / &
         '     GETMETA: IFI value should be in range 1,',I2     / )
    !
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '           / &
         '     GETMETA: IFJ value should be in range 1,',I2    / )
    !
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '           / &
         '     GETMETA: IFC value should be in range 1,3'      / )
    !
  END FUNCTION GETMETA

  !/ ------------------------------------------------------------------- /
  !> @brief Reads in a TEMPLATE section from file.
  !>
  !> @details This section defines a list of text strings that will be
  !>    used to replace a "placeholder string" when generating metadata
  !>    for partitioned parameters.
  !>
  !>    Format of section is:
  !>
  !>    \code
  !>      TEMPLATE <placeholder_string>
  !>        Value for partition IPART=0
  !>        Value for partition IPART=1
  !>        Value for partition IPART=2
  !>        ...
  !>        Value for partition IPART=N
  !>    \endcode
  !>
  !> @param[in,out]  NDMI  Unit number
  !> @param[in,out]  ILINE Line number
  !>
  !> @author Chris Bunney @date 04-Dec-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE READ_PART_TMPL(NDMI, ILINE)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         04-Dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    04-Dec-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Reads in a TEMPLATE section from file.
    !     This section defines a list of text strings that will be used
    !     to replace a "placeholder string" when generating metadata for
    !     partitioned parameters.
    !
    !     Format of section is:
    !
    !       TEMPLATE <placeholder_string>
    !         Value for partition IPART=0
    !         Value for partition IPART=1
    !         Value for partition IPART=2
    !         ...
    !         Value for partition IPART=N
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDMI    Int.  I/O  Unit number
    !       ILINE   Int.  I/O  Line number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    INTEGER, INTENT(IN)               :: NDMI
    INTEGER, INTENT(INOUT)            :: ILINE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    CHARACTER(LEN=256) :: BUF, ID
    INTEGER            :: IERR
    LOGICAL            :: EOF, NEW
    TYPE(PART_TMPL_T), POINTER  :: P

    ! Re-read META line to get template string ID (the 2nd field)
    READ(NDMI, '(A)') BUF
    READ(BUF, *, IOSTAT=IERR) ID, ID
    IF(IERR /= 0) THEN
      WRITE(NDSE, 1000) FN_META, ILINE, BUF
      CALL EXTCDE(10)
    ENDIF
    ID = "<" // TRIM(ID) // ">"

    IF(DEBUG) WRITE(*,'(I5,1X,A20,1X,A)') ILINE, '[template id]', TRIM(ID)

    ! Extend list of partition template types:
    IF(ASSOCIATED(PART_TMPL)) THEN
      ! Got to end of list
      P => PART_TMPL
      DO WHILE(ASSOCIATED(P%NEXT))
        P => P%NEXT
      ENDDO
      ALLOCATE(P%NEXT)
      P => P%NEXT
    ELSE
      ALLOCATE(PART_TMPL)
      P => PART_TMPL
    ENDIF

    ! Set template id and read template strings from file:
    P%TMPL = TRIM(ID)
    ALLOCATE(P%PART_TEXT(0:NOSWLL))
    NULLIFY(P%NEXT)
    P%NP = 0

    DO
      CALL NEXT_LINE(NDMI, BUF, ILINE, EOF, NEW_SECTION=NEW)
      IF(EOF) THEN
        BACKSPACE(NDMI)
        RETURN
      ENDIF

      IF(NEW) THEN
        ! Start of new meta data entry
        IF(DEBUG) WRITE(*,'(I5,1X,A20)') ILINE, '[--end of section--]'
        ILINE = ILINE - 1
        BACKSPACE(NDMI)
        EXIT
      ENDIF

      ! Check we have not exceeded NOSWLL
      IF(P%NP .GT. NOSWLL) THEN
        WRITE(*,*) "Too many partition entries (NOSWLL=",NOSWLL,"). Ignoring"
        CYCLE
      ENDIF

      ! Add string to array of partition text
      IF(DEBUG) THEN
        WRITE(*,'(I5,1X,A20,1X,I1,1X,A)') ILINE, '[part template]', &
             P%NP, TRIM(BUF)
      ENDIF

      P%PART_TEXT(P%NP) = TRIM(ADJUSTL(BUF)) ! Zero indexed
      P%NP = P%NP + 1
    ENDDO

    RETURN
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '/           &
         '     ERROR READING PART HEADER - MISSING TEMPLATE ID?'/ &
         '     FILENAME = ', A /                                &
         '     LINE NO =', I5 /                                 &
         '  => ', A /)
    !
  END SUBROUTINE READ_PART_TMPL


  !/ ------------------------------------------------------------------- /
  !> @brief Prints the patition templates to screen (for debug use).
  !> @author Chris Bunney @date 04-Dec-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PRINT_PART_TMPL()
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         04-Dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    04-Dec-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Prints the patition templates to screen (for debug use).
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(PART_TMPL_T), POINTER :: P
    INTEGER :: I

    PRINT*,'=============='
    IF(.NOT. ASSOCIATED(PART_TMPL)) THEN
      PRINT*,'Empty partition list'
      RETURN
    ENDIF

    P => PART_TMPL
    DO
      PRINT*,P%TMPL
      DO I=0,P%NP - 1
        PRINT*,'   - ',I,TRIM(P%PART_TEXT(I))
      ENDDO
      IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
      P => P%NEXT
    ENDDO
    PRINT*,'=============='
  END SUBROUTINE PRINT_PART_TMPL

  !/ ------------------------------------------------------------------- /
  !> @brief Adds partition number to meta-data.
  !>
  !> @details Replaces all instances of "<IPART>" in the provided meta data
  !>     with the partition number IPART.
  !>
  !> @param[in]  META   Meta data type
  !> @param[in]  IPART  Partition number
  !>
  !> @author Chris Bunney @date 02-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE ADD_PARTNO(META, IPART)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Adds partition number to meta-data.
    !
    !  2. Method :
    !
    !     Replaces all instances of "<IPART>" in the provided meta data with
    !     the partition number IPART.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       META  META_T  I  Meta data type
    !       IPART   Int.  I  Partition number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    TYPE(META_T), INTENT(INOUT) :: META
    INTEGER, INTENT(IN) :: IPART
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    CHARACTER(LEN=80) :: TMP
    INTEGER :: I, J
    TYPE(META_PAIR_T), POINTER :: P

    CALL PARTNO_STRING_SUB(META%ENAME, IPART)
    CALL PARTNO_STRING_SUB(META%VARNM, IPART)
    CALL PARTNO_STRING_SUB(META%VARNL, IPART)
    CALL PARTNO_STRING_SUB(META%VARNS, IPART)
    CALL PARTNO_STRING_SUB(META%VARNG, IPART)
    CALL PARTNO_STRING_SUB(META%VARNC, IPART)
    CALL PARTNO_STRING_SUB(META%VARND, IPART)
    IF(META%EXTRA%N .GT. 0) THEN
      P => META%EXTRA%HEAD
      DO
        CALL PARTNO_STRING_SUB(P%ATTNAME, IPART)
        IF(P%TYPE .EQ. "c") THEN
          CALL PARTNO_STRING_SUB(P%ATTVAL, IPART)
        ENDIF
        IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
        P => P%NEXT
      ENDDO
    ENDIF

  END SUBROUTINE ADD_PARTNO

  !/ ------------------------------------------------------------------- /
  !> @brief Performs string substition of placeholder strings with
  !>    partition number specfic values.
  !>
  !> @details The placeholder \<IPART\> is automatically replaced with the
  !>    partition number (0, 1, 2, etc).
  !>
  !>    Other template placeholders can be defined in the ounfmeta.inp
  !>    file by the user.
  !>
  !> @param[in,out]  INSTR  Input string
  !> @param[in]      IPART  Partition number
  !>
  !> @author Chris Bunney @date 02-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PARTNO_STRING_SUB(INSTR, IPART)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Performs string substition of placeholder strings with partition
    !     number specfic values.
    !
    !     The placeholder <IPART> is automatically replaced with the
    !     partition number (0, 1, 2, etc).
    !
    !     Other template placeholders can be defined in the ounfmeta.inp
    !     file by the user.
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       INSTR  Char.  I/O  Input string
    !       IPART   Int.  I    Partition number
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(INOUT) :: INSTR
    INTEGER, INTENT(IN) :: IPART
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: I, J, ISN
    TYPE(PART_TMPL_T), POINTER :: P
    CHARACTER(LEN=512) :: TMPL

    ISN = IPART + 1
    IF(PTMETH .LE. 3) THEN
      IF (ISN .GT. 5) ISN = 5
    ELSE
      IF (ISN .GT. 2) ISN = 2
    ENDIF

    ! Set partition number (built-in IPART template)
    I = INDEX(INSTR, IPART_TOKEN)
    J = I + LEN_TRIM(IPART_TOKEN)
    IF(I .GT. 0) THEN
      WRITE(TMPL, '(A,I1,A)') INSTR(1:I-1), IPART, INSTR(J:LEN(INSTR))
      INSTR = TMPL
    ENDIF

    ! Set standard name string (built-in SPART template)
    I = INDEX(INSTR, SPART_TOKEN)
    J = I + LEN_TRIM(SPART_TOKEN)

    IF(I .GT. 0) THEN
      INSTR = INSTR(1:I-1) // TRIM(SNAMEP(ISN)) // INSTR(J:LEN(INSTR))
    ENDIF

    ! Also try underscore separated version: <SPART_>
    I = INDEX(INSTR, SPART_TOKEN_)
    J = I + LEN_TRIM(SPART_TOKEN_)

    IF(I .GT. 0) THEN
      INSTR = INSTR(1:I-1) // TRIM(REPLACE_CHAR(SNAMEP(ISN), " ", "_")) &
           // INSTR(J:LEN(INSTR))
    ENDIF

    ! Merge in user defined partition templates (if any):
    IF(.NOT. ASSOCIATED(PART_TMPL)) RETURN

    P => PART_TMPL
    DO
      I = INDEX(INSTR, TRIM(P%TMPL))
      J = I + LEN_TRIM(P%TMPL)

      IF(I .GT. 0) THEN
        IF(IPART .GE. P%NP) THEN
          WRITE(NDSE, 1000) TRIM(P%TMPL), P%NP, IPART
          CALL EXTCDE(10)
        ENDIF
        INSTR = INSTR(1:I-1) // TRIM(P%PART_TEXT(IPART)) // INSTR(J:LEN(INSTR))
      ENDIF

      ! Try "underscore" version <TMPL_>:
      I = LEN_TRIM(P%TMPL)
      TMPL = P%TMPL(1:I-1) // "_>"
      I = INDEX(INSTR, TRIM(TMPL))
      J = I + LEN_TRIM(TMPL)
      IF(I .GT. 0) THEN
        INSTR = INSTR(1:I-1) // TRIM(REPLACE_CHAR(P%PART_TEXT(IPART), " ", "_")) &
             // INSTR(J:LEN(INSTR))
      ENDIF

      IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
      P => P%NEXT
    ENDDO

    RETURN

1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '            / &
         '     NOT ENOUGH USER DEFINED ENTRIES FOR TEMPLATE'    / &
         '     TEMPLATE ID     : ',A                            / &
         '     NUM ENTRIES     : ',I2                           / &
         '     REQESTED IPART* : ',I2                           / &
         '     (*Note: IPART is zero-refernced)'                / &
         '     Please update your ounfmeta.inp file.'           /)

  END SUBROUTINE PARTNO_STRING_SUB

  !/ ------------------------------------------------------------------- /
  !> @brief Writes the meta-data entries for a variable.
  !>
  !> @details Attribute pairs defined in META are written to the netCDF
  !>    variable specificed in the VARID handle.
  !>
  !>    There are two stages to the write - first all "mandatory" or
  !>    "pre-defined" attributes are written out (those defined in the
  !>    META_T type). Secondly, if there is any user-defined "extra"
  !>    freeform meta data defined, this is written out via a separate
  !>    call to write_freeform_meta_list().
  !>
  !> @param[in,out]  NCID   NetCDF file ID
  !> @param[in,out]  VARID  NetCDF variable ID
  !> @param[in]      META   Meta data type
  !> @param[out]     ERR    Error value
  !>
  !> @author Chris Bunney @date 02-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE WRITE_META(NCID, VARID, META, ERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Writes the meta-data entries for a variable.
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NCID    Int.  I/O  NetCDF file ID
    !       VARID   Int.  I/O  NetCDF variable ID
    !       META    Int.  I    Meta data type
    !       ERR     Int.  O    Error value
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: NCID, VARID
    TYPE(META_T), INTENT(IN) :: META
    INTEGER, INTENT(OUT) :: ERR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IVAL, VAL
    REAL :: RVAL
    !/
    ERR = NF90_PUT_ATT(NCID, VARID, 'long_name', META%VARNL)
    IF(ERR /= NF90_NOERR) RETURN

    IF(META%VARNS .NE. '' .AND. META%VARNS .NE. UNSETC) THEN
      ERR = NF90_PUT_ATT(NCID, VARID, 'standard_name', META%VARNS)
      IF(ERR /= NF90_NOERR) RETURN
    ENDIF

    IF(META%VARNG .NE. '' .AND. META%VARNG .NE. UNSETC) THEN
      ERR = NF90_PUT_ATT(NCID, VARID, 'globwave_name', META%VARNG)
      IF(ERR /= NF90_NOERR) RETURN
    ENDIF

    ERR = NF90_PUT_ATT(NCID, VARID, 'units', META%UNITS)
    IF(ERR /= NF90_NOERR) RETURN

    ! Fill value dependent on variable type
    IF(NCVARTYPE .EQ. 2) THEN
      ERR = NF90_PUT_ATT(NCID, VARID, '_FillValue', NF90_FILL_SHORT)
    ELSE
      ERR = NF90_PUT_ATT(NCID, VARID, '_FillValue', NF90_FILL_FLOAT)
    END IF
    IF(ERR /= NF90_NOERR) RETURN

    ERR = NF90_PUT_ATT(NCID, VARID, 'scale_factor', META%FSC)
    IF(ERR /= NF90_NOERR) RETURN

    ERR = NF90_PUT_ATT(NCID, VARID, 'add_offset', 0.)
    IF(ERR /= NF90_NOERR) RETURN

    ! For variables with vartype SHORT, the valid min/max
    ! are scaled by scale_factor and converted to integers.
    ! If vartype is FLOAT, then no scaling is performed and
    ! valid min/max are written out directly as floats.
    IF(NCVARTYPE .EQ. 2) THEN
      VAL = NINT(META%VMIN / META%FSC)
      ERR = NF90_PUT_ATT(NCID, VARID,'valid_min', VAL)
      IF(ERR /= NF90_NOERR) RETURN

      VAL = NINT(META%VMAX / META%FSC)
      ERR = NF90_PUT_ATT(NCID, VARID,'valid_max', VAL)
      IF(ERR /= NF90_NOERR) RETURN
    ELSE
      ERR = NF90_PUT_ATT(NCID, VARID,'valid_min', META%VMIN)
      IF(ERR /= NF90_NOERR) RETURN

      ERR = NF90_PUT_ATT(NCID, VARID,'valid_max', META%VMAX)
      IF(ERR /= NF90_NOERR) RETURN
    ENDIF

    IF(META%VARNC .NE. '' .AND. META%VARNC .NE. UNSETC) THEN
      ERR = NF90_PUT_ATT(NCID, VARID, 'comment', META%VARNC)
      IF(ERR /= NF90_NOERR) RETURN
    ENDIF

    IF(META%VARND .NE. '' .AND. META%VARND .NE. UNSETC) THEN
      ERR = NF90_PUT_ATT(NCID, VARID, 'direction_reference', META%VARND)
      IF(ERR /= NF90_NOERR) RETURN
    END IF

    IF(CRS_NAME .NE. '' .AND. CRS_NAME .NE. UNSETC) THEN
      ERR = NF90_PUT_ATT(NCID, VARID, 'grid_mapping', CRS_NAME)
      IF(ERR /= NF90_NOERR) RETURN
    ENDIF

    IF(COORDS_ATTR .NE. '' .AND. COORDS_ATTR .NE. UNSETC) THEN
      ERR = NF90_PUT_ATT(NCID, VARID, 'coordinates', ADJUSTL(COORDS_ATTR))
      IF(ERR /= NF90_NOERR) RETURN
    ENDIF

    IF (META%EXTRA%N .GT. 0) THEN
      CALL WRITE_FREEFORM_META_LIST(NCID, VARID, META%EXTRA, ERR)
      IF(ERR /= NF90_NOERR) RETURN
    ENDIF

    RETURN
    !
  END SUBROUTINE WRITE_META

  !/ ------------------------------------------------------------------- /
  !> @brief Writes the user meta-data entries for the global attributes.
  !>
  !> @details Global meta-data is stored as a meta-data list, so this
  !>    is essentially a convenience/legacy function that calls the
  !>    write_freeform_meta_list() subroutine.
  !>
  !> @param[in]   NCID  NetCDF file ID
  !> @param[out]  ERR   Error value
  !>
  !> @author Chris Bunney @date 09-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE WRITE_GLOBAL_META(NCID, ERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         09-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Writes the user meta-data entries for the global attributes
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NCID    Int.  I/O  NetCDF file ID
    !       ERR     Int.  O    Error value
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: NCID
    INTEGER, INTENT(OUT) :: ERR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    CALL WRITE_FREEFORM_META_LIST(NCID, NF90_GLOBAL, GLOBAL_META, ERR)
  END SUBROUTINE WRITE_GLOBAL_META

  !/ ------------------------------------------------------------------- /
  !> @brief Writes the freeform user meta-data entries for a NetCDF variable
  !>
  !> @param[in,out]  NCID      NetCDF file ID
  !> @param[in,out]  VARID     NetCDF variable ID
  !> @param[in]      METALIST  META_LIST_T object to write
  !> @param[out]     ERR       Error value
  !>
  !> @author Chris Bunney @date 16-Dec-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE WRITE_FREEFORM_META_LIST(NCID, VARID, METALIST, ERR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         16-Dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    16-Dec-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Writes the freeform user meta-data entries for a NetCDF variable
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NCID      Int.  I/O  NetCDF file ID
    !       VARID     Int.  I/O  NetCDF file ID
    !       METALIST  Type. I    META_LIST_T object to write
    !       ERR       Int.  O    Error value
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: NCID, VARID
    TYPE(META_LIST_T), INTENT(IN) :: METALIST
    INTEGER, INTENT(OUT) :: ERR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: I, IVAL
    REAL    :: RVAL
    TYPE(META_PAIR_T), POINTER :: P

    IF(METALIST%N .EQ. 0) RETURN

    P => METALIST%HEAD

    ! Loop over global metadata pairs:
    DO

      IF (P%ATTNAME .EQ. '' .OR.                      &
           P%ATTNAME .EQ. UNSETC) CYCLE

      SELECT CASE(P%TYPE)

      CASE('i')
        READ(P%ATTVAL, *) IVAL
        ERR = NF90_PUT_ATT(NCID, VARID, P%ATTNAME, IVAL)
        IF(ERR /= NF90_NOERR) RETURN

      CASE('r', 'f')
        READ(P%ATTVAL, *) RVAL
        ERR = NF90_PUT_ATT(NCID, VARID, P%ATTNAME, RVAL)
        IF(ERR /= NF90_NOERR) RETURN

      CASE('c')
        ERR = NF90_PUT_ATT(NCID, VARID, P%ATTNAME,    &
             P%ATTVAL)
        IF(ERR /= NF90_NOERR) RETURN

      CASE DEFAULT
        WRITE(NDSE,1000) P%TYPE
        CALL EXTCDE(10)
      END SELECT

      IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
      P => P%NEXT
    ENDDO
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNFMETA : '            / &
         '     WRITE_FREEFORM_META: Unknown attribute'          / &
         '     data type: ', A1         / )
    !
  END SUBROUTINE WRITE_FREEFORM_META_LIST

  !/ ------------------------------------------------------------------- /
  !> @brief Writes meta-data to the screen - for debugging purposes.
  !>
  !> @param[in]  META   Meta data type
  !>
  !> @author Chris Bunney @date 09-Nov-2020
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PRINT_META(META)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         02-Nov-2020 |
    !/                  +-----------------------------------+
    !/
    !/    09-Nov-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Writes meta-data to the screen - for debugging purposes.
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       VARID   Int.  I/O  NetCDF variable ID
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    TYPE(META_T), INTENT(IN) :: META
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T), POINTER :: P

    WRITE(*,*) META%VARNM
    WRITE(*,"(A20,':',1X,A)") "Standard name", TRIM(META%VARNS)
    WRITE(*,"(A20,':',1X,A)") "Long name", TRIM(META%VARNL)
    WRITE(*,"(A20,':',1X,A)") "Units", TRIM(META%UNITS)
    WRITE(*,"(A20,':',1X,A)") "GLOBWAVE name", TRIM(META%VARNG)
    WRITE(*,"(A20,':',1X,A)") "Direction conv", TRIM(META%VARND)
    WRITE(*,"(A20,':',1X,A)") "Comment", TRIM(META%VARNC)
    WRITE(*,"(A20,':',1X,2F12.3)") "Min/Max", META%VMIN, META%VMAX
    IF(META%EXTRA%N .GT. 0) THEN
      P => META%EXTRA%HEAD
      DO
        WRITE(*,"(A20,':',1X,A)") TRIM(P%ATTNAME), TRIM(P%ATTVAL)
        IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
        P => P%NEXT
      ENDDO
    ENDIF

  END SUBROUTINE PRINT_META

  !/ ------------------------------------------------------------------- /
  !> @brief Performs "deep" copy of a META_T type.
  !>
  !> @details A "deep" copy ensures that the linked list data in the EXTRA
  !> field is copied, rather than just copying the pointer.
  !>
  !> Calls copy_meta_list() internally to copy the EXTRA linked list.
  !>
  !> @returns A new META_T variable
  !>
  !> @param[in]  META   META data structure to copy
  !>
  !> @author Chris Bunney @date 16-Dec-2020
  !/ ------------------------------------------------------------------- /
  FUNCTION META_DEEP_COPY(META) RESULT(COPY)
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         16-Dec-2020 |
    !/                  +-----------------------------------+
    !/
    !/    16-Dec-2020 : Creation                            ( version 7.12 )
    !/
    !
    !  1. Purpose :
    !
    !     Performs "Deep" copy of a META_T type. This ensures that the
    !     linked list data in the EXTRA field is copied, rather than just
    !     copying the pointer.
    !
    !  2. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       META   META_T.  I   META data structure to copy
    !     ----------------------------------------------------------------
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    TYPE(META_T), INTENT(IN) :: META
    TYPE(META_T) :: COPY

    ! Shallow copy first:
    COPY = META

    ! Now deep copy the EXTRA field (is pointer)
    COPY%EXTRA = COPY_META_LIST(META%EXTRA)

  END FUNCTION META_DEEP_COPY

  !/ ------------------------------------------------------------------- /
  !> @brief Populates the default meta data for ww3_ounf output fields.
  !>
  !> @remark VMIN and VMAX are now set in the units of the output field.
  !>    Previously, they were set with scaled values based on the scaling
  !>    factor FSC. The scaling is now performed (if necessary) in the
  !>    WRITE_META subroutine.
  !>
  !> @remark FSC (scale factor) is only applied to data and valid_min/max
  !>    if the netCDF variable type is NF90_SHORT.
  !>
  !> @author Chris Bunney @date 22-Mar-2021
  !/ ------------------------------------------------------------------- /
  SUBROUTINE DEFAULT_META()
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           C. Bunney               |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    02-Nov-2020 : Creation                            ( version 7.12 )
    !/    22-Mar-2021 : Adds extra coupling fields          ( version 7.13 )
    !/
    !
    !  1. Purpose :
    !
    !     Populates the default meta data for ww3_ounf.
    !
    !  2. Remarks :
    !
    !     VMIN and VMAX are now set in the units of the output field.
    !     Previously, they were set with scaled values based on the scaling
    !     factor FSC. The scaling is now performed (if necessary) in the
    !     WRITE_META subroutine.
    !
    !     FSC (scale factor) is only applied to data and valid_min/max if
    !     the netCDF variable type is NF90_SHORT.
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    TYPE(META_T), POINTER :: META(:)
    !
    !----------GROUP 1 ----------------
    !
    ! IFI=1, IFJ=1, DPT
    META => GROUP(1)%FIELD(1)%META
    META(1)%FSC    = 0.5
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.dpt'
    META(1)%VARNM = 'dpt'
    META(1)%VARNL ='depth'
    META(1)%VARNS ='depth'
    META(1)%VARNG ='depth'
    META(1)%VARNC =''
    META(1)%VARND =''
    META(1)%VMIN = -45000
    META(1)%VMAX = 70000
    ! IFI=1, IFJ=2, CUR
    META => GROUP(1)%FIELD(2)%META
    META(1)%ENAME  = '.cur'
    META(1)%VARND = DIRCOM
    IF(VECTOR) THEN
      META(1)%FSC    = 0.01
      META(1)%UNITS  = 'm s-1'
      META(1)%VMIN = -9.9
      META(1)%VMAX =  9.9
      META(1)%VARNM='ucur'
      META(1)%VARNL='eastward current'
      META(1)%VARNS='eastward_sea_water_velocity'
      META(1)%VARNG='eastward_sea_water_velocity'
      META(1)%VARNC='cur=sqrt(U**2+V**2)'

      ! Second component
      META(2) = META(1)
      META(2)%VARNM='vcur'
      META(2)%VARNL='northward current'
      META(2)%VARNS='northward_sea_water_velocity'
      META(2)%VARNG='northward_sea_water_velocity'
      META(2)%VARNC='cur=sqrt(U**2+V**2)'
    ELSE
      META(1)%FSC    = 0.01
      META(1)%UNITS  = 'm s-1'
      META(1)%VMIN = 0
      META(1)%VMAX = 10.0
      META(1)%VARNM='cspd'
      META(1)%VARNL='current speed'
      META(1)%VARNS='sea_water_speed'
      META(1)%VARNG='sea_water_speed'

      ! Second component
      META(2) = META(1)
      META(2)%FSC = 0.1
      META(2)%VARNM='cdir'
      META(2)%UNITS= 'degrees'
      META(2)%VARNL='current direction (toward)'
      META(2)%VARNS='direction_of_sea_water_velocity'
      META(2)%VARNG='direction_of_sea_water_velocity'
      META(2)%VMIN = 0
      META(2)%VMAX = 360.0
    ENDIF ! VECTOR
    ! IFI=1, IFJ=3
    META => GROUP(1)%FIELD(3)%META
    META(1)%ENAME  = '.wnd'
    META(1)%VARND = DIRCOM
    IF(VECTOR) THEN
      META(1)%FSC    = 0.1
      META(1)%UNITS  = 'm s-1'
      META(1)%VARNM='uwnd'
      META(1)%VARNL='eastward_wind'
      META(1)%VARNS='eastward_wind'
      META(1)%VARNG='eastward_wind'
      META(1)%VARNC='wind=sqrt(U10**2+V10**2)'
      META(1)%VMIN = -99.0
      META(1)%VMAX =  99.0

      ! Second component
      META(2) = META(1)
      META(2)%VARNM='vwnd'
      META(2)%VARNL='northward_wind'
      META(2)%VARNS='northward_wind'
      META(2)%VARNG='northward_wind'
      META(2)%VARNC='wind=sqrt(U10**2+V10**2)'
    ELSE
      META(1)%FSC = 0.01
      META(1)%UNITS= 'm s-1'
      META(1)%VARNM='wspd'
      META(1)%VARNL='wind speed'
      META(1)%VARNS='wind_speed'
      META(1)%VARNG='wind_speed'
      META(1)%VMIN = 0.0
      META(1)%VMAX = 100.0

      ! Second component
      META(2) = META(1)
      META(2)%FSC = 0.1
      META(2)%VARNM='wdir'
      META(2)%UNITS='degrees'
      META(2)%VARNL='wind direction (from)'
      META(2)%VARNS='wind_from_direction'
      META(2)%VARNG='wind_from_direction'
      META(2)%VMIN = 0.0
      META(2)%VMAX = 360.0
    ENDIF ! VECTOR
    ! IFI=1, IFJ=4, AST
    META => GROUP(1)%FIELD(4)%META
    META(1)%FSC    = 0.1
    META(1)%ENAME = '.ast'
    META(1)%UNITS = 'K'
    META(1)%VARNM='ast'
    META(1)%VARNL='air sea temperature difference'
    !META(1)%VARNS='air_sea_temperature_difference'
    META(1)%VARNS=''
    META(1)%VARNG='air_sea_temperature_difference'
    META(1)%VMIN = -200.0
    META(1)%VMAX = 200.0
    ! IFI=1, IFJ=5, WLV
    META => GROUP(1)%FIELD(5)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.wlv'
    META(1)%VARNM='wlv'
    META(1)%VARNL='sea surface height above sea level'
    META(1)%VARNS='sea_surface_height_above_mean_sea_level'
    META(1)%VARNG='sea_surface_height_above_sea_level'
    META(1)%VMIN = 0
    META(1)%VMAX = 100
    ! IFI=1, IFJ=6, ICE
    META => GROUP(1)%FIELD(6)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.ice'
    META(1)%VARNM='ice'
    META(1)%VARNL='sea ice area fraction'
    META(1)%VARNS='sea_ice_area_fraction'
    META(1)%VARNG='sea_ice_area_fraction'
    META(1)%VMIN = 0
    META(1)%VMAX = 1
    ! IFI=1, IFJ=7, IBG
    META => GROUP(1)%FIELD(7)%META
    META(1)%FSC    = 0.0001
    META(1)%UNITS  = 'km-1'
    META(1)%ENAME  = '.ibg'
    META(1)%VARNM='ibg'
    META(1)%VARNL='icebergs_damping'
    !META(1)%VARNS='icebergs_induced_attenuation_scale_for_waves'
    META(1)%VARNS=''
    META(1)%VARNG='icebergs_damping'
    META(1)%VMIN = 0
    META(1)%VMAX = 3.2
    ! IFI=1, IFJ=8, TAUA
    META => GROUP(1)%FIELD(8)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = 'Pa'
    META(1)%ENAME  = '.taua'
    META(1)%VARNM='utaua'
    META(1)%VARNL='surface_downward_eastward_stress'
    META(1)%VARNS='surface_downward_eastward_stress'
    META(1)%VARNG='surface_downward_eastward_stress'
    META(2)%VARNC='taua=sqrt(utaua**2+vtaua**2)'
    META(1)%VMIN = -320
    META(1)%VMAX =  320
    META(1)%VARND = DIRCOM

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtaua'
    META(2)%VARNL='surface_downward_northward_stress'
    META(2)%VARNS='surface_downward_northward_stress'
    META(2)%VARNG='surface_downward_northward_stress'
    META(2)%VARNC='taua=sqrt(utaua**2+vtaua**2)'
    ! IFI=1, IFJ=9, RHO
    META => GROUP(1)%FIELD(9)%META
    META(1)%FSC    = 0.0001
    META(1)%UNITS  = 'kg m-3'
    META(1)%ENAME  = '.rhoa'
    META(1)%VARNM='rhoa'
    META(1)%VARNL='air_density'
    META(1)%VARNS='air_density'
    META(1)%VARNG='air_density'
    META(1)%VMIN = 0
    META(1)%VMAX = 2
    ! IFI=1, IFJ=10, D50
#ifdef W3_BT4
    META => GROUP(1)%FIELD(10)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 'Krumbein phi scale'
    META(1)%ENAME  = '.d50'
    META(1)%VARNM='d50'
    META(1)%VARNL='grain_size'
    !META(1)%VARNS='sediment_grain_size'
    META(1)%VARNS=''
    META(1)%VARNG='sediment_grain_size'
    META(1)%VMIN = -10.0
    META(1)%VMAX = 32.0
#endif
    ! IFI=1, IFJ=11, IC1
#ifdef W3_IS2
    META => GROUP(1)%FIELD(11)%META
    META(1)%FSC = 0.001
    META(1)%UNITS = 'm'
    META(1)%ENAME = '.ic1'
    META(1)%VARNM='ic1'
    META(1)%VARNL='ice thickness'
    META(1)%VARNS='sea_ice_thickness'
    META(1)%VARNG='ice_thickness'
    META(1)%VMIN = 0
    META(1)%VMAX = 30
    ! IFI=1, IFJ=12, IC5
    META => GROUP(1)%FIELD(12)%META
    META(1)%FSC = 0.05
    META(1)%UNITS = 'm'
    META(1)%ENAME = '.ic5'
    META(1)%VARNM='ic5'
    META(1)%VARNL='maximum floe diameter'
    !META(1)%VARNS='maximum_ice_floe_diameter'
    META(1)%VARNS=''
    META(1)%VARNG='maximum_ice_floe_diameter'
    META(1)%VMIN = 0
    META(1)%VMAX = 1500
#endif
    !
    !----------GROUP 2 ----------------
    !
    ! IFI=2, IFJ=1, HS
    META => GROUP(2)%FIELD(1)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.hs'
    META(1)%VARNM='hs'
    META(1)%VARNL='significant height of wind and swell waves'
    META(1)%VARNS='sea_surface_wave_significant_height'
    META(1)%VARNG='significant_wave_height'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=2, IFJ=2, LM
    META => GROUP(2)%FIELD(2)%META
    META(1)%FSC    = 1.
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.lm'
    META(1)%VARNM='lm'
    META(1)%VARNL='mean wave length'
    !META(1)%VARNS='mean_wave_length'
    META(1)%VARNS=''
    META(1)%VARNG='mean_wave_length'
    META(1)%VMIN = 0
    META(1)%VMAX = 3200
    ! IFI=2, IFJ=3, T02
    META => GROUP(2)%FIELD(3)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = 's'
    META(1)%ENAME  = '.t02'
    META(1)%VARNM='t02'
    META(1)%VARNL='mean period T02'
    META(1)%VARNS='sea_surface_wind_wave_mean_period' // &
         '_from_variance_spectral_density_second_frequency_moment'
    META(1)%VARNG='mean_period_t02'
    META(1)%VMIN = 0
    META(1)%VMAX = 50
    ! IFI=2, IFJ=4, T0M1
    META => GROUP(2)%FIELD(4)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = 's'
    META(1)%ENAME  = '.t0m1'
    META(1)%VARNM='t0m1'
    META(1)%VARNL='mean period T0m1'
    META(1)%VARNS='sea_surface_wind_wave_mean_period_from_variance' // &
         '_spectral_density_inverse_frequency_moment'
    META(1)%VARNG='mean_period_t0m1'
    META(1)%VMIN = 0
    META(1)%VMAX = 50
    ! IFI=2, IFJ=5, T01
    META => GROUP(2)%FIELD(5)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = 's'
    META(1)%ENAME  = '.t01'
    META(1)%VARNM='t01'
    META(1)%VARNL='mean period T01'
    META(1)%VARNS='sea_surface_wind_wave_mean_period_from_variance' // &
         '_spectral_density_first_frequency_moment'
    META(1)%VARNG='mean_period_t01'
    META(1)%VMIN = 0
    META(1)%VMAX = 50
    ! IFI=2, IFJ=6, FP
    META => GROUP(2)%FIELD(6)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 's-1'
    META(1)%ENAME  = '.fp'
    META(1)%VARNM='fp'
    META(1)%VARNL='wave peak frequency'
    !META(1)%VARNS='sea_surface_wave_peak_frequency'
    META(1)%VARNS=''
    META(1)%VARNG='dominant_wave_frequency'
    META(1)%VMIN = 0
    META(1)%VMAX = 10
    ! IFI=2, IFJ=7, DIR
    META => GROUP(2)%FIELD(7)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.dir'
    META(1)%VARNM='dir'
    META(1)%VARNL='wave mean direction'
    META(1)%VARNS='sea_surface_wave_from_direction'
    META(1)%VARNG='wave_from_direction'
    META(1)%VARND=DIRCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=2, IFJ=8, SPR
    META => GROUP(2)%FIELD(8)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.spr'
    META(1)%VARNM='spr'
    META(1)%VARNL='directional spread'
    META(1)%VARNS='sea_surface_wave_directional_spread'
    META(1)%VARNG='directional_spread'
    META(1)%VMIN = 0
    META(1)%VMAX = 90
    ! IFI=2, IFJ=9, DP
    META => GROUP(2)%FIELD(9)%META
    META(1)%FSC    = 1.
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.dp'
    META(1)%VARNM='dp'
    META(1)%VARNL='peak direction'
    META(1)%VARNS='sea_surface_wave_peak_direction'
    META(1)%VARNG='dominant_wave_direction'
    META(1)%VARND=DIRCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=2, IFJ=10, HIG
    META => GROUP(2)%FIELD(10)%META
    META(1)%FSC    = 0.0002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.hig'
    META(1)%VARNM='hig'
    META(1)%VARNL='infragravity_wave_height'
    !META(1)%VARNS='sea_surface_wave_infragravity_significant_height'
    META(1)%VARNS=''
    META(1)%VARNG='infragravity_significant_wave_height'
    META(1)%VMIN = 0
    META(1)%VMAX = 1.0
    ! IFI=2, IFJ=11, MXE
    META => GROUP(2)%FIELD(11)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.mxe'
    META(1)%VARNM='stmaxe'
    META(1)%VARNL='expected maximum sea surface elevation (nonlinear,2nd order)'
    !META(1)%VARNS='expected maximum sea surface elevation (nonlinear,2nd order)'
    META(1)%VARNS=''
    META(1)%VARNG='expected maximum sea surface elevation (nonlinear,2nd order)'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=2, IFJ=12, MXES
    META => GROUP(2)%FIELD(12)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.mxes'
    META(1)%VARNM='stmaxd'
    META(1)%VARNL='standard deviation of maximum sea surface elevation (nonlinear,2nd order)'
    !META(1)%VARNS='std of expected maximum sea surface elevation (nonlinear,2nd order)'
    META(1)%VARNS=''
    META(1)%VARNG='standard deviation of maximum sea surface elevation (nonlinear,2nd order)'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=2, IFJ=13, MXH
    META => GROUP(2)%FIELD(13)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.mxh'
    META(1)%VARNM='hmaxe'
    META(1)%VARNL='expected maximum wave height (linear, 1st order)'
    !META(1)%VARNS='expected maximum wave height (linear, 1st order)'
    META(1)%VARNS=''
    META(1)%VARNG='expected maximum wave height (linear, 1st order)'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=2, IFJ=14, MXHC
    META => GROUP(2)%FIELD(14)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.mxhc'
    META(1)%VARNM='hcmaxe'
    META(1)%VARNL='expected maximum wave height from crest (linear, 1st order)'
    !META(1)%VARNS='expected maximum wave height from crest (linear, 1st order)'
    META(1)%VARNS=''
    META(1)%VARNG='expected maximum wave height from crest (linear, 1st order)'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=2, IFJ=15, SDMH
    META => GROUP(2)%FIELD(15)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.sdmh'
    META(1)%VARNM='hmaxd'
    META(1)%VARNL='STD of maximum wave height (linear, 1st order)'
    !META(1)%VARNS='STD of maximum wave height (linear, 1st order)'
    META(1)%VARNS=''
    META(1)%VARNG='STD of maximum wave height (linear, 1st order)'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=2, IFJ=16, SDMHC
    META => GROUP(2)%FIELD(16)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.sdmhc'
    META(1)%VARNM='hcmaxd'
    META(1)%VARNL='STD of maximum wave height from crest (linear, 1st order)'
    !META(1)%VARNS='STD of maximum wave height from crest (linear, 1st order)'
    META(1)%VARNS=''
    META(1)%VARNG='STD of maximum wave height from crest (linear, 1st order)'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=2, IFJ=17, WBT
    META => GROUP(2)%FIELD(17)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.wbt'
    META(1)%VARNM='wbt'
    META(1)%VARNL='dominant wave breaking probability'
    !META(1)%VARNS='dominant_wave_breaking_probability'
    META(1)%VARNS=''
    META(1)%VARNG='dominant_wave_breaking_probability'
    META(1)%VMIN = 0
    META(1)%VMAX = 1
    ! IFI=2, IFJ=18, TP
    META => GROUP(2)%FIELD(18)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = 's'
    META(1)%ENAME  = '.tp'
    META(1)%VARNM='tp'
    META(1)%VARNL='wave peak period'
    META(1)%VARNS='sea_surface_wave_peak_period'
    META(1)%VARNG='dominant_wave_period'
    META(1)%VMIN = 0
    META(1)%VMAX = 50
    ! IFI=2, IFJ=19
    META => GROUP(2)%FIELD(19)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 'm-1'
    META(1)%ENAME  = '.wnm'
    META(1)%VARNM='wnm'
    META(1)%VARNL='mean wave number'
    META(1)%VARNS=''
    META(1)%VARNG=''
    META(1)%VMIN = 0
    META(1)%VMAX = 32
    !
    !---------- GROUP 3 ----------------
    !
    ! IFI=3, IFJ=1, EF
    META => GROUP(3)%FIELD(1)%META
    META(1)%VARNM='ef'
    META(1)%VARNL='wave_elevation_spectrum'
    META(1)%VARNS='sea_surface_wave_variance_spectral_density'
    IF (NCVARTYPE.LE.3) THEN
      META(1)%UNITS  = 'log10(m2 s+1E-12)'
      !META(1)%VARNS='base_ten_logarithm_of_power_spectral_density_of_surface_elevation'
      META(1)%VARNC='base_ten_logarithm'
      META(1)%FSC = 0.0004
      META(1)%VMIN = -12.
      META(1)%VMAX = 12.
    ELSE
      META(1)%UNITS  = 'm2 s'
      META(1)%FSC = 1.
      META(1)%VMIN = 0.
      META(1)%VMAX = 1.e12
    END IF
    META(1)%ENAME  = '.ef'
    META(1)%VARNG = META(1)%VARNS
    ! IFI=3, IFJ=2, TH1M
    META => GROUP(3)%FIELD(2)%META
    ! Information for spectral
    META(1)%FSC     = 0.1
    META(1)%VARNM='th1m'
    META(1)%VARNL='mean wave direction frequency spectrum'
    !META(1)%VARNS='sea_surface_wave_from_direction_frequency_spectrum'
    META(1)%VARNS=''
    META(1)%VARNG = META(1)%VARNS
    META(1)%VARND=DIRCOM
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.th1m'
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=3, IFJ=3, STH1M
    META => GROUP(3)%FIELD(3)%META
    ! Information for spectral
    META(1)%FSC     = 0.01
    META(1)%VARNM='sth1m'
    META(1)%VARNL='spreading frequency spectrum'
    !META(1)%VARNS='sea_surface_wave_spreading_spectrum'
    META(1)%VARNS=''
    META(1)%VARNG = META(1)%VARNS
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.sth1m'
    META(1)%VMIN = 0
    META(1)%VMAX = 90
    ! IFI=3, IFJ=4, TH2M
    META => GROUP(3)%FIELD(4)%META
    META(1)%FSC     = 0.1
    META(1)%VARNM='th2m'
    META(1)%VARNL='second mean wave direction frequency spectrum'
    !META(1)%VARNS='sea_surface_wave_from_direction_frequency_spectrum_from_second_moments'
    META(1)%VARNS=''
    META(1)%VARNG = META(1)%VARNS
    META(1)%VARND=DIRCOM
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.th2m'
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=3, IFJ=5, STH2M
    META => GROUP(3)%FIELD(5)%META
    META(1)%FSC     = 0.01
    META(1)%VARNM='sth2m'
    META(1)%VARNL='second spreading frequency spectrum'
    !META(1)%VARNS='sea_surface_wave_spreading_spectrum_from_second_moments'
    META(1)%VARNS=''
    META(1)%VARNG = META(1)%VARNS
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.sth2m'
    META(1)%VMIN = 0
    META(1)%VMAX = 90
    ! IFI=3, IFJ=6, WN
    META => GROUP(3)%FIELD(6)%META
    ! Information for spectral
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 'm-1'
    META(1)%ENAME  = '.wn'
    META(1)%VARNM='wn'
    META(1)%VARNL='wave numbers'
    !META(1)%VARNS='wave_numbers'
    META(1)%VARNS=''
    META(1)%VARNG='wave_numbers'
    META(1)%VMIN = 0
    META(1)%VMAX = 32
    !
    !---------- GROUP 4 ----------------
    !
    ! IFI=4, IFJ=1, PHS
    META => GROUP(4)%FIELD(1)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME = '.phs'// IPART_TOKEN
    META(1)%VARNM = 'phs'// IPART_TOKEN
    META(1)%VARNL = 'wave significant height partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_significant_height'
    META(1)%VARNG = 'significant_wave_height_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=4, IFJ=2, PTP
    META => GROUP(4)%FIELD(2)%META
    META(1)%FSC = 0.01
    META(1)%UNITS = 's'
    META(1)%ENAME = '.ptp'// IPART_TOKEN
    META(1)%VARNM = 'ptp'// IPART_TOKEN
    META(1)%VARNL = 'peak period partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_period_at_variance' // &
         '_spectral_density_maximum'
    META(1)%VARNG = 'dominant_wave_period_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 100
    ! IFI=4, IFJ=3, PLP
    META => GROUP(4)%FIELD(3)%META
    META(1)%FSC = 1.
    META(1)%UNITS = 'm'
    META(1)%ENAME = '.plp'// IPART_TOKEN
    META(1)%VARNM = 'plp'// IPART_TOKEN
    META(1)%VARNL = 'peak wave length partition '// IPART_TOKEN
    !META(1)%VARNS = 'peak_wave_length_partition_'// SPART_TOKEN_
    META(1)%VARNS = ''
    META(1)%VARNG = 'peak_wave_length_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 10000
    ! IFI=4, IFJ=4, PDIR
    META => GROUP(4)%FIELD(4)%META
    META(1)%FSC = 0.1
    META(1)%UNITS = 'degree'
    META(1)%ENAME =  '.pdir'// IPART_TOKEN
    META(1)%VARNM =  'pdir'// IPART_TOKEN
    META(1)%VARNL = 'wave mean direction partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_from_direction'
    META(1)%VARNG = 'wave_from_direction_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VARND = DIRCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=4, IFJ=5, PSPR
    META => GROUP(4)%FIELD(5)%META
    META(1)%FSC = 0.1
    META(1)%UNITS = 'degree'
    META(1)%ENAME = '.pspr'// IPART_TOKEN
    META(1)%VARNM = 'pspr'// IPART_TOKEN
    META(1)%VARNL = 'directional spread partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_diectional_spread'
    META(1)%VARNG = 'directional_spread_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 90
    ! IFI=4, IFJ=6, PWS
    META => GROUP(4)%FIELD(6)%META
    META(1)%FSC = 0.001
    META(1)%UNITS = '1'
    META(1)%ENAME = '.pws'// IPART_TOKEN
    META(1)%VARNM = 'pws'// IPART_TOKEN
    META(1)%VARNL = 'wind sea fraction in partition '// IPART_TOKEN
    !META(1)%VARNS = 'wind_sea_fraction_in_partition_'// IPART_TOKEN
    META(1)%VARNS = ''
    META(1)%VARNG = 'wind_sea_fraction_in_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 1
    ! IFI=4, IFJ=7, PDP
    META => GROUP(4)%FIELD(7)%META
    META(1)%FSC = 0.1
    META(1)%UNITS = 'degree'
    META(1)%ENAME = '.pdp'// IPART_TOKEN
    META(1)%VARNM = 'pdp'// IPART_TOKEN
    META(1)%VARNL = 'peak direction partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_from_direction_at_variance' // &
         '_spectral_density_maximum'
    META(1)%VARNG = 'dominant_wave_from_direction_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VARND = DIRCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=4, IFJ=8, PQP
    META => GROUP(4)%FIELD(8)%META
    META(1)%FSC = 0.01
    META(1)%UNITS = '1'
    META(1)%ENAME = '.pqp'// IPART_TOKEN
    META(1)%VARNM = 'pqp'// IPART_TOKEN
    META(1)%VARNL = 'peakedness partition '// IPART_TOKEN
    !META(1)%VARNS = 'sea_surface_wave_peakedness_partition_'// IPART_TOKEN
    META(1)%VARNS = ''
    META(1)%VARNG = 'wave_peakedness_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 320
    ! IFI=4, IFJ=9, PPE
    META => GROUP(4)%FIELD(9)%META
    META(1)%FSC = 0.01
    META(1)%UNITS = '1'
    META(1)%ENAME = '.ppe'// IPART_TOKEN
    META(1)%VARNM = 'ppe'// IPART_TOKEN
    META(1)%VARNL = 'peak enhancement factor partition '// IPART_TOKEN
    !META(1)%VARNS = 'wave_peak_enhancement_factor_partition_'// IPART_TOKEN
    META(1)%VARNS = ''
    META(1)%VARNG = 'wave_peak_enhancement_factor_partition_'// IPART_TOKEN
    META(1)%VARNC = 'JONSWAP peak enhancement factor; ' // PARTCOM
    META(1)%VARND = ''
    META(1)%VMIN = 0
    META(1)%VMAX = 320
    ! IFI=4, IFJ=10, PGW
    META => GROUP(4)%FIELD(10)%META
    META(1)%FSC = 0.0001
    META(1)%UNITS = 's-1'
    META(1)%ENAME =  '.pgw'// IPART_TOKEN
    META(1)%VARNM = 'pgw'// IPART_TOKEN
    META(1)%VARNL = 'frequency width partition '// IPART_TOKEN
    !META(1)%VARNS = 'Gaussian_frequency_spread_partition_'// IPART_TOKEN
    META(1)%VARNS = ''
    META(1)%VARNG = 'Gaussian_frequency_spread_partition_'// IPART_TOKEN
    META(1)%VARNC = 'Gaussian least-square fit to ' // &
         'omni-directional spectral partition; ' // PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 3.2
    ! IFI=4, IFJ=11, PSW
    META => GROUP(4)%FIELD(11)%META
    META(1)%FSC = 0.0001
    META(1)%UNITS = '1'
    META(1)%ENAME = '.psw'// IPART_TOKEN
    META(1)%VARNM = 'psw'// IPART_TOKEN
    META(1)%VARNL = 'spectral width partition '// IPART_TOKEN
    !META(1)%VARNS = 'sea_surface_wave_spectral_width_partition_'// IPART_TOKEN
    META(1)%VARNS = ''
    META(1)%VARNG = 'wave_spectral_width_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 3.2
    ! IFI=4, IFJ=12, PTM10
    META => GROUP(4)%FIELD(12)%META
    META(1)%FSC = 0.01
    META(1)%UNITS = 's'
    META(1)%ENAME = '.ptm10c'// IPART_TOKEN
    META(1)%VARNM = 'ptm10c'// IPART_TOKEN
    META(1)%VARNL = 'mean period Tm10 partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_mean_period_from_variance' // &
         '_spectral_density_inverse_frequency_moment'
    META(1)%VARNG = 'mean_wave_period_Tm10_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 100
    ! IFI=4, IFJ=13, PT01
    META => GROUP(4)%FIELD(13)%META
    META(1)%FSC = 0.01
    META(1)%UNITS = 's'
    META(1)%ENAME = '.pt01c'// IPART_TOKEN
    META(1)%VARNM = 'pt01c'// IPART_TOKEN
    META(1)%VARNL = 'mean period T01 partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_mean_period_from_variance' // &
         '_spectral_density_first_frequency_moment'
    META(1)%VARNG = 'mean_wave_period_T01_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 100
    ! IFI=4, IFJ=14, PT02
    META => GROUP(4)%FIELD(14)%META
    META(1)%FSC = 0.01
    META(1)%UNITS = 's'
    META(1)%ENAME = '.pt02c'// IPART_TOKEN
    META(1)%VARNM = 'pt02c'// IPART_TOKEN
    META(1)%VARNL = 'mean period T02 partition '// IPART_TOKEN
    META(1)%VARNS = 'sea_surface_'// SPART_TOKEN_ //'_wave_mean_period_from_variance' // &
         '_spectral_density_second_frequency_moment'
    META(1)%VARNG = 'mean_wave_period_T02_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 100
    ! IFI=4, IFJ=15, PEP
    META => GROUP(4)%FIELD(15)%META
    META(1)%FSC = 0.02
    META(1)%UNITS = 'm2 s rad-1'
    META(1)%ENAME = '.pep'// IPART_TOKEN
    META(1)%VARNM = 'pep'// IPART_TOKEN
    META(1)%VARNL = 'energy at peak frequency partition '// IPART_TOKEN
    !META(1)%VARNS = 'sea_surface_wave_energy_at_variance_spectral_density_maximum_partition_'// IPART_TOKEN
    META(1)%VARNS = ''
    META(1)%VARNG = 'wave_energy_at_variance_spectral_density_maximum_partition_'// IPART_TOKEN
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 200
    ! IFI=4, IFJ=16, TWS
    META => GROUP(4)%FIELD(16)%META
    META(1)%FSC = 0.001
    META(1)%UNITS = '1'
    META(1)%ENAME = '.tws'
    META(1)%VARNM = 'tws'
    META(1)%VARNL = 'wind sea fraction'
    !META(1)%VARNS = 'wind_sea_fraction'
    META(1)%VARNS = ''
    META(1)%VARNG = 'wind_sea_fraction'
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 1
    ! IFI=4, IFJ=17, PNR
    META => GROUP(4)%FIELD(17)%META
    META(1)%FSC = 1.
    META(1)%UNITS = '1'
    META(1)%ENAME = '.pnr'
    META(1)%VARNM = 'pnr'
    META(1)%VARNL = 'number of wave partitions'
    !META(1)%VARNS = 'number_of_wave_partitions'
    META(1)%VARNS = ''
    META(1)%VARNG = 'number_of_wave_partitions'
    META(1)%VARNC = PARTCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 100
    !
    !---------- GROUP 5 ----------------
    !
    ! IFI=5, IFJ=1, UST
    META => GROUP(5)%FIELD(1)%META
    ! First component
    META(1)%FSC    = 0.001
    META(1)%ENAME  = '.ust'
    META(1)%UNITS  = 'm s-1'
    META(1)%VARNM='uust'
    META(1)%VARNL='eastward friction velocity'
    !META(1)%VARNS='eastward_friction_velocity'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_friction_velocity'
    META(1)%VARNC='ust=sqrt(uust**2+vust**2)'
    META(1)%VARND=DIRCOM
    META(1)%VMIN = -99.0
    META(1)%VMAX =  99.0

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vust'
    META(2)%VARNL='northward friction velocity'
    !META(2)%VARNS='northward_friction_velocity'
    META(2)%VARNS=''
    META(2)%VARNG='northward_friction_velocity'
    ! IFI=5, IFJ=2, CHA
    META => GROUP(5)%FIELD(2)%META
    META(1)%FSC    = 1.E-5
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.cha'
    META(1)%VARNM='cha'
    META(1)%VARNL='charnock coefficient for surface roughness length for momentum in air'
    META(1)%VARNS='charnock_coefficient_for_surface_roughness_length_for_momentum_in_air'
    META(1)%VARNG='charnock_coefficient'
    META(1)%VMIN = 0
    META(1)%VMAX = 0.327
    ! IFI=5, IFJ=3, CGE
    META => GROUP(5)%FIELD(3)%META
    META(1)%FSC    = 0.1           !0.01
    META(1)%UNITS  = 'kW m-1'
    META(1)%ENAME  = '.cge'
    META(1)%VARNM='cge'
    META(1)%VARNL='wave energy flux'
    !META(1)%VARNS='sea_surface_wind_wave_energy_flux'
    META(1)%VARNS=''
    META(1)%VARNG='wave_energy_flux'
    META(1)%VMIN = 0
    META(1)%VMAX = 999
    ! IFI=5, IFJ=4, FAW
    META => GROUP(5)%FIELD(4)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'W m-2'
    META(1)%ENAME  = '.faw'
    META(1)%VARNM='faw'
    META(1)%VARNL='wind to wave energy flux'
    META(1)%VARNS='wind_mixing_energy_flux_into_sea_water'
    META(1)%VARNG='wind_to_wave_energy_flux'
    META(1)%VMIN = 0
    META(1)%VMAX = 999
    ! IFI=5, IFJ=5, TAW
    META => GROUP(5)%FIELD(5)%META
    ! First component
    META(1)%FSC    = 0.000001
    META(1)%UNITS  = 'm2 s-2'
    META(1)%ENAME  = '.taw'
    META(1)%VARNM='utaw'
    META(1)%VARNL='eastward wave supported wind stress'
    !META(1)%VARNS='eastward_wave_supported_wind_stress'
    META(1)%VARNS=''
    META(1)%VARNC='taw=sqrt(utaw**2+vtaw**2)'
    META(1)%VARNG='eastward_wave_supported_wind_stress'
    META(1)%VARND=DIRCOM
    META(1)%VMIN = -0.032
    META(1)%VMAX =  0.032

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtaw'
    META(2)%VARNL='northward wave supported wind stress'
    !META(2)%VARNS='northward_wave_supported_wind_stress'
    META(2)%VARNS=''
    META(2)%VARNG='northward_wave_supported_wind_stress'
    META(2)%VARNC='taw=sqrt(utaw**2+vtaw**2)'
    ! IFI=5, IFJ=6, TWA
    META => GROUP(5)%FIELD(6)%META
    ! First component
    META(1)%FSC    = 0.0001
    META(1)%ENAME  = '.twa'
    META(1)%UNITS  = 'm2 s-2'
    META(1)%VARNM='utwa'
    META(1)%VARNL='eastward wave to wind stress'
    !META(1)%VARNS='eastward_wave_to_wind_stress'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_wave_to_wind_stress'
    META(1)%VARNC='twa=sqrt(utwa**2+vtwa**2)'
    META(1)%VARND=DIRCOM
    META(1)%VMIN = -3.2
    META(1)%VMAX =  3.2

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtwa'
    META(2)%VARNL='northward wave to wind stress'
    !META(2)%VARNS='northward_wave_to_wind_stress'
    META(2)%VARNS=''
    META(2)%VARNG='northward_wave_to_wind_stress'
    META(2)%VARNC='twa=sqrt(utwa**2+vtwa**2)'
    ! IFI=5, IFJ=7, WCC
    META => GROUP(5)%FIELD(7)%META
    META(1)%FSC    = 0.0001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.wcc'
    META(1)%VARNM='wcc'
    META(1)%VARNL='whitecap coverage'
    !META(1)%VARNS='whitecap_coverage'
    META(1)%VARNS=''
    META(1)%VARNG='whitecap_coverage'
    META(1)%VARNC=''
    META(1)%VARND=''
    META(1)%VMIN = 0
    META(1)%VMAX = 1
    ! IFI=5, IFJ=8, WCF
    META => GROUP(5)%FIELD(8)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.wcf'
    META(1)%VARNM='wcf'
    META(1)%VARNL='whitecap foam thickness'
    !META(1)%VARNS='whitecap_foam_thickness'
    META(1)%VARNS=''
    META(1)%VARNG='whitecap_foam_thickness'
    META(1)%VMIN = 0
    META(1)%VMAX = 10
    ! IFI=5, IFJ=9, WCH
    META => GROUP(5)%FIELD(9)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.wch'
    META(1)%VARNM='wch'
    META(1)%VARNL='significant breaking wave height'
    !META(1)%VARNS='significant_breaking_wave_height'
    META(1)%VARNS=''
    META(1)%VARNG='significant_breaking_wave_height'
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    ! IFI=5, IFJ=10, WCM
    META => GROUP(5)%FIELD(10)%META
    META(1)%FSC    = 0.0001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.wcm'
    META(1)%VARNM='wcm'
    META(1)%VARNL='whitecap moment'
    !META(1)%VARNS='whitecap_moment'
    META(1)%VARNS=''
    META(1)%VARNG='whitecap_moment'
    META(1)%VMIN = 0
    META(1)%VMAX = 1
    ! IFI=5, IFJ=11, FWS
    META => GROUP(5)%FIELD(11)%META
    META(1)%FSC    = 0.002
    META(1)%UNITS  = 's'
    META(1)%ENAME  = '.fws'
    META(1)%VARNM='fws'
    META(1)%VARNL='Wind_sea_mean_period_T0M1'
    META(1)%VARNS='sea_surface_wind_wave_mean_period_from_variance' // &
         '_spectral_density_inverse_frequency_moment'
    META(1)%VARNG='Wind_sea_mean_period_T0M1'
    META(1)%VARNC=''
    META(1)%VARND=''
    META(1)%VMIN = 0
    META(1)%VMAX = 64
    !
    !---------- GROUP 6 ----------------
    !
    ! IFI=6, IFJ=1, SXY
    META => GROUP(6)%FIELD(1)%META
    META(1)%FSC  = 10.
    META(1)%UNITS = 'N m-1'
    META(1)%ENAME = '.sxy'
    META(1)%VARND = DIRCOM
    META(1)%VMIN = -30000
    META(1)%VMAX = 30000

    ! First component
    META(1)%VARNM='sxx'
    META(1)%VARNL='Radiation stress component Sxx'
    !META(1)%VARNS='radiation_stress_component_sxx'
    META(1)%VARNS=''

    ! S6cond component
    META(2) = META(1)
    META(2)%VARNM='syy'
    META(2)%VARNL='Radiation stress component Syy'
    !META(2)%VARNS='radiation_stress_component_syy'
    META(2)%VARNS=''

    ! Third component
    META(3) = META(1)
    META(3)%FSC = 1.
    META(3)%VARNM='sxy'
    META(3)%VARNL='Radiation stress component Sxy'
    !META(3)%VARNS='radiation_stress_component_sxy'
    META(3)%VARNS=''
    ! IFI=6, IFJ=2, TWO
    META => GROUP(6)%FIELD(2)%META
    META(1)%FSC    = 0.000001
    META(1)%UNITS  = 'm2 s-2'
    META(1)%ENAME  = '.two'
    META(1)%VMIN = -0.032
    META(1)%VMAX =  0.032
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNM='utwo'
    META(1)%VARNL='eastward wave to ocean stress'
    !META(1)%VARNS='eastward_wave_to_ocean_stress'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_wave_to_ocean_stress'
    META(1)%VARNC='two=sqrt(utwo**2+vtwo**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtwo'
    META(2)%VARNL='northward wave to ocean stress'
    !META(2)%VARNS='northward_wave_to_ocean_stress'
    META(2)%VARNS=''
    META(2)%VARNG='northward_wave_to_ocean_stress'
    META(2)%VARNC='two=sqrt(utwo**2+vtwo**2)'
    ! IFI=6, IFJ=3, BHD
    META => GROUP(6)%FIELD(3)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'm2 s-2'
    META(1)%ENAME  = '.bhd'
    META(1)%VARNM='bhd'
    META(1)%VARNL='radiation pressure (Bernouilli Head)'
    !META(1)%VARNS='radiation_pressure'
    META(1)%VARNS=''
    META(1)%VARNG='radiation_pressure'
    META(1)%VMIN = 0
    META(1)%VMAX = 100
    ! IFI=6, IFJ=4, FOC
    META => GROUP(6)%FIELD(4)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'W m-2'
    META(1)%ENAME  = '.foc'
    META(1)%VARNM='foc'
    META(1)%VARNL='wave to ocean energy flux'
    !META(1)%VARNS='wave_to_ocean_energy_flux'
    META(1)%VARNS=''
    META(1)%VARNG='wave_to_ocean_energy_flux'
    META(1)%VMIN = 0
    META(1)%VMAX = 999
    ! IFI=6, IFJ=5, TUS
    META => GROUP(6)%FIELD(5)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 'm2 s-1'
    META(1)%ENAME  = '.tus'
    META(1)%VARND = DIRCOM
    META(1)%VMIN = -32.0 ! C Hansen: The former values of +-9.9 might be
    META(1)%VMAX = 32.0  ! exceeded more frequently in real storms

    ! First component
    META(1)%VARNM='utus'
    META(1)%VARNL='eastward stokes transport'
    !META(1)%VARNS='eastward_stokes_transport'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_stokes_transport'
    META(1)%VARNC='tus=sqrt(utus**2+vtus**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtus'
    META(2)%VARNL='northward stokes transport'
    !META(2)%VARNS='northward_stokes_transport'
    META(2)%VARNS=''
    META(2)%VARNG='northward_stokes_transport'
    META(2)%VARNC='tus=sqrt(utus**2+vtus**2)'

    ! IFI=6, IFJ=6, USS
    META => GROUP(6)%FIELD(6)%META
    META(1)%FSC    = 0.0005
    META(1)%UNITS  = 'm s-1'
    META(1)%ENAME  = '.uss'

    ! First component
    META(1)%VARNM='uuss'
    META(1)%VARNL='eastward surface stokes drift'
    META(1)%VARNS='sea_surface_wave_stokes_drift_eastward_velocity'
    META(1)%VARNG='eastward_surface_stokes_drift'
    META(1)%VARNC='uss=sqrt(uuss**2+vuss**2)'
    META(1)%VARND=DIRCOM
    META(1)%VMIN = -4.95
    META(1)%VMAX =  4.95

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vuss'
    META(2)%VARNL='northward surface stokes drift'
    META(2)%VARNS='sea_surface_wave_stokes_drift_northward_velocity'
    META(2)%VARNG='northward_surface_stokes_drift'
    WRITE(META(2)%VARNC,'(A,F8.4,A,F8.4,A)') 'Frequency range ',SIG(1)*TPIINV,' to ',SIG(NK)*TPIINV,' Hz'
    ! IFI=6, IFJ=7, P2S
    META => GROUP(6)%FIELD(7)%META
    META(1)%FSC    = 0.01
    META(1)%ENAME  = '.p2s'
    META(1)%UNITS  = 'm4'
    META(1)%VMIN = -150
    META(1)%VMAX = 320

    ! First component
    META(1)%VARNL='power spectral density of equivalent surface pressure'
    !META(1)%VARNS='power_spectral_density_of_equivalent_surface_pressure'
    META(1)%VARNS=''
    META(1)%VARNG='power_spectral_density_of_equivalent_surface_pressure'
    META(1)%VARNM='fp2s'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='pp2s'
    META(2)%UNITS= 's-1'
    META(2)%VARNL='peak period of power spectral density of equivalent surface pressure'
    !META(2)%VARNS='peak_period_of_power_spectral_density_of_equivalent_surface_pressure'
    META(2)%VARNS=''
    META(2)%VARNG='peak_period_of_power_spectral_density_of_equivalent_surface_pressure'

    ! IFI=6, IFJ=8, USF
    META => GROUP(6)%FIELD(8)%META
    META(1)%UNITS = 'm s-1 Hz-1'
    META(1)%FSC = 0.0005
    META(1)%ENAME = '.usf'
    META(1)%VMIN = -4.95
    META(1)%VMAX =  4.95
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNM='uusf'
    META(1)%VARNL='eastward spectral variance of surface stokes drift'
    !META(1)%VARNS='eastward_spectral_variance_of_surface_stokes_drift'
    META(1)%VARNS=''
    META(1)%VARNC='usf=sqrt(uusf**2+vusf**2)'
    META(1)%VARNG='eastward_spectral_variance_of_surface_stokes_drift'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vusf'
    META(2)%VARNL='northward spectral variance of surface stokes drift'
    !META(2)%VARNS='northward_spectral_variance_of_surface_stokes_drift'
    META(2)%VARNS=''
    META(2)%VARNG='northward_spectral_variance_of_surface_stokes_drift'
    META(2)%VARNC='usf=sqrt(uusf**2+vusf**2)'
    ! IFI=6, IFJ=9, P2L
    META => GROUP(6)%FIELD(9)%META
    ! Information for spectral microseismic generation data (2nd file)
    META(1)%FSC = 0.0004
    META(1)%VARNM='p2l'
    META(1)%VARNL='base ten logarithm of power spectral density of equivalent surface pressure'
    !META(1)%VARNS='base_ten_logarithm_of_power_spectral_density_of_equivalent_surface_pressure'
    META(1)%VARNS=''
    META(1)%VARNG='base_ten_logarithm_of_power_spectral_density_of_equivalent_surface_pressure'
    IF (NCVARTYPE.EQ.2) THEN
      META(1)%UNITS='log10(Pa2 m2 s+1E-12)'
      META(1)%VMIN = -12.
      META(1)%VMAX = 12.
    ELSE
      META(1)%UNITS='Pa2 m2 s'
      META(1)%VARNL='power spectral density of equivalent surface pressure'
      !META(1)%VARNS='power_spectral_density_of_equivalent_surface_pressure'
      META(1)%VARNG='power_spectral_density_of_equivalent_surface_pressure'
      META(1)%VMIN = 0.
      META(1)%VMAX = 1.e12
    ENDIF
    META(1)%VARNC=''
    META(1)%VARND=''
    META(1)%ENAME='.p2l'
    ! IFI=6, IFJ=10, TWI
    META => GROUP(6)%FIELD(10)%META
    META(1)%FSC = 0.000001
    META(1)%UNITS = 'm2 s-2'
    META(1)%ENAME = '.tic'
    META(1)%VMIN = -0.032
    META(1)%VMAX =  0.032
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNL='eastward wave to sea ice stress'
    META(1)%VARNM='utic'
    !META(1)%VARNS='eastward_wave_to_sea_ice_stress'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_wave_to_sea_ice_stress'
    META(1)%VARNC='two=sqrt(utwo**2+vtwo**2)'
    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtic'
    META(2)%VARNL='northward wave to sea ice stress'
    !META(2)%VARNS='northward_wave_to_sea_ice_stress'
    META(2)%VARNS=''
    META(2)%VARNG='northward_wave_to_sea_ice_stress'
    META(2)%VARNC='two=sqrt(utwo**2+vtwo**2)'
    ! IFI=6, IFJ=11, FIC
    META => GROUP(6)%FIELD(11)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'W m-2'
    META(1)%ENAME  = '.fic'
    META(1)%VARNM='fic'
    META(1)%VARNL='wave to sea ice energy flux'
    !META(1)%VARNS='wave_to_sea_ice_energy_flux'
    META(1)%VARNS=''
    META(1)%VARNG='wave_to_sea_ice_energy_flux'
    META(1)%VMIN = 0
    META(1)%VMAX = 999
    ! IFI=6, IFJ=12, USP
    META => GROUP(6)%FIELD(12)%META
    META(1)%UNITS   = 'm s-1'
    META(1)%FSC    = 0.0005
    META(1)%ENAME  = '.usp'
    META(1)%VARND  = DIRCOM
    META(1)%VMIN = -9.99
    META(1)%VMAX = 9.98

    ! First component
    META(1)%VARNM='ussp'
    META(1)%VARNL='eastward partitioned surface stokes drift'
    !META(1)%VARNS='eastward_partitioned_surface_stokes_drift'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_partitioned_surface_stokes_drift'
    META(1)%VARNC='usp=sqrt(ussp**2+vssp**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vssp'
    META(2)%VARNL='northward partitioned surface stokes drift'
    !META(2)%VARNS='northward_partitioned_surface_stokes_drift'
    META(2)%VARNS=''
    META(2)%VARNG='northward_partitioned_surface_stokes_drift'
    META(2)%VARNC='usp=sqrt(ussp**2+vssp**2)'
    ! IFI=6, IFJ=13
    META => GROUP(6)%FIELD(13)%META
    META(1)%UNITS  = 'Pa'
    META(1)%FSC    = 0.01
    META(1)%ENAME  = '.toc'
    META(1)%VMIN   = -320
    META(1)%VMAX   =  320
    META(1)%VARND  = DIRCOM

    ! First component
    META(1)%VARNM='utoc'
    META(1)%VARNL='eastward total wave to ocean stres'
    META(1)%VARNS=''
    META(1)%VARNG=''
    META(1)%VARNC='toc=sqrt(utoc**2+vtoc**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtoc'
    META(2)%VARNL='northward total wave to ocean stres'
    META(2)%VARNS=''
    META(2)%VARNG=''
    META(2)%VARNC='toc=sqrt(utoc**2+vtoc**2)'
    !
    !---------- GROUP 7 ----------------
    !
    ! IFI=7, IFJ=1, ABR
    META => GROUP(7)%FIELD(1)%META
    META(1)%FSC    = 0.01
    META(1)%ENAME  = '.abr'
    META(1)%UNITS  = 'm'
    META(1)%VMIN = -180
    META(1)%VMAX = 180
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNM='uabr'
    META(1)%VARNL='rms of bottom displacement amplitude zonal'
    !META(1)%VARNS='rms_of_bottom_displacement_amplitude_zonal'
    META(1)%VARNS=''
    META(1)%VARNG='rms_of_bottom_displacement_amplitude_zonal'
    META(1)%VARNC='abr=sqrt(uabr**2+vabr**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vabr'
    META(2)%VARNL='rms of bottom displacement amplitude meridional'
    !META(2)%VARNS='rms_of_bottom_displacement_amplitude_meridional'
    META(2)%VARNS=''
    META(2)%VARNG='rms_of_bottom_displacement_amplitude_meridional'
    META(2)%VARNC='abr=sqrt(uabr**2+vabr**2)'
    ! IFI=7, IFJ=2, UBR
    META => GROUP(7)%FIELD(2)%META
    META(1)%FSC    = 0.01
    META(1)%ENAME  = '.ubr'
    META(1)%UNITS  = 'm s-1'
    META(1)%VMIN = -180
    META(1)%VMAX = 180
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNM='uubr'
    META(1)%VARNL='rms of bottom velocity amplitude zonal'
    !META(1)%VARNS='rms_of_bottom_velocity_amplitude_zonal'
    META(1)%VARNS=''
    META(1)%VARNG='rms_of_bottom_velocity_amplitude_zonal'
    META(1)%VARNC='ubr=sqrt(uubr**2+vubr**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vubr'
    META(2)%VARNL='rms of bottom velocity amplitude meridional'
    !META(2)%VARNS='rms_of_bottom_velocity_amplitude_meridional'
    META(2)%VARNS=''
    META(2)%VARNG='rms_of_bottom_velocity_amplitude_meridional'
    ! IFI=7, IFJ=3, BED
    META => GROUP(7)%FIELD(3)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 'm'
    META(1)%ENAME  = '.bed'
    META(1)%VMIN = 0
    META(1)%VMAX = 30
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNM='bed'
    META(1)%VARNL='bottom roughness'
    !META(1)%VARNS='sea bottom roughness length'
    META(1)%VARNS=''
    META(1)%VARNG='ripple_wavelength'
    META(1)%VARNC='ripple_length=sqrt(ripplex**2+rippley**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='ripplex'
    META(2)%VARNL='eastward sea bottom ripple wavelength'
    !META(2)%VARNS='eastward_ripple_wavelength'
    META(2)%VARNS=''
    META(2)%VARNG='eastward_ripple_wavelength'
    META(2)%VARNC='ripple_length=sqrt(ripplex**2+rippley**2)'

    ! Third component
    META(3) = META(1)
    META(3)%VARNM='rippley'
    META(3)%VARNL='northward sea bottom ripple wavelength'
    !META(3)%VARNS='northward_ripple_wavelength'
    META(3)%VARNS=''
    META(3)%VARNG='northward_ripple_wavelength'
    META(3)%VARNC='ripple_length=sqrt(ripplex**2+rippley**2)'
    ! IFI=7, IFJ=4, FBB
    META => GROUP(7)%FIELD(4)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'W m-2'
    META(1)%ENAME  = '.fbb'
    META(1)%VARNM='fbb'
    META(1)%VARNL='wave dissipation in bbl'
    !META(1)%VARNS='wave_energy_dissipation_in_bottom_boundary_layer'
    META(1)%VARNS=''
    META(1)%VARNG='wave_dissipation_in_bbl'
    META(1)%VMIN = 0
    META(1)%VMAX = 999
    ! IFI=7, IFJ=5, TBB
    META => GROUP(7)%FIELD(5)%META
    META(1)%FSC    = 0.000001
    META(1)%UNITS  = 'm2 s-2'
    META(1)%ENAME  = '.tbb'
    META(1)%VMIN = -0.032
    META(1)%VMAX =  0.032
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNM='utbb'
    META(1)%VARNL='eastward wave to bbl stress'
    !META(1)%VARNS='eastward_wave_to_bottom_boundary_layer_stress'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_wave_to_bbl_stress'
    META(1)%VARNC='tbb=sqrt(utbb**2+vtbb**2)'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='vtbb'
    META(2)%VARNL='northward wave to bbl stress'
    !META(2)%VARNS='northward_wave_to_bottom_boundary_layer_stress'
    META(2)%VARNS=''
    META(2)%VARNG='northward_wave_to_bbl_stress'
    META(2)%VARNC='tbb=sqrt(utbb**2+vtbb**2)'
    !
    !---------- GROUP 8 ----------------
    ! IFI=8, IFJ=1, MSS
    META => GROUP(8)%FIELD(1)%META
    META(1)%FSC    = 0.00001
    META(1)%ENAME  = '.mss'
    META(1)%UNITS  = '1'
    META(1)%VMIN = 0
    META(1)%VMAX = 0.3
    META(1)%VARND = DIRCOM
    WRITE(META(1)%VARNC,'(A,F8.4,A,F8.4,A)') 'Frequency range ',SIG(1)*TPIINV,' to ',SIG(NK)*TPIINV,' Hz'

    ! First component
    META(1)%VARNM='mssu'
    META(1)%VARNL='downwave mean square slope'
    META(1)%VARNS='sea_surface_wave_mean_square_upwave_slope'
    META(1)%VARNG='x_mean_square_slope'
    META(1)%VARNC='mss=mssu+mssc'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='mssc'
    META(2)%VARNL='crosswave mean square slope'
    META(2)%VARNS='sea_surface_wave_mean_square_crosswave_slope'
    META(2)%VARNG='y_mean_square_slope'
    ! IFI=8, IFJ=2, MSC
    META => GROUP(8)%FIELD(2)%META
    META(1)%FSC    = 1E-7
    META(1)%ENAME  = '.msc'
    META(1)%UNITS  = '1'
    META(1)%VMIN = 0
    META(1)%VMAX = 0.003
    META(1)%VARND = DIRCOM

    ! First component
    META(1)%VARNM='mscx'
    META(1)%VARNL='eastward phillips constant'
    !META(1)%VARNS='eastward_phillips_constant'
    META(1)%VARNS=''
    META(1)%VARNG='eastward_phillips_constant'
    META(1)%VARNC='msc=mscx+mscy'

    ! Second component
    META(2) = META(1)
    META(2)%VARNM='mscy'
    META(2)%VARNL='northward phillips constant'
    !META(2)%VARNS='northward_phillips_constant'
    META(2)%VARNS=''
    META(2)%VARNG='northward_phillips_constant'
    META(2)%VARNC='msc=mscx+mscy'
    ! IFI=8, IFJ=3, MSD
    META => GROUP(8)%FIELD(3)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.msd'
    META(1)%VARNM='mssd'
    META(1)%VARNL='u direction for mss'
    META(1)%VARNS='sea_surface_mean_square_upwave_slope_direction'
    META(1)%VARNG='sea_surface_wave_dominant_mean_square_slope_direction'
    WRITE(META(1)%VARNC,'(A,F8.4,A,F8.4,A)') 'Frequency range ',SIG(1)*TPIINV,' to ',SIG(NK)*TPIINV,' Hz'
    META(1)%VARND = DIRCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=8, IFJ=4, MCD
    META => GROUP(8)%FIELD(4)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'degree'
    META(1)%ENAME  = '.mcd'
    META(1)%VARNM='mscd'
    META(1)%VARNL='x direction for msc'
    !META(1)%VARNS='sea_surface_wave_dominant_mean_square_slope_direction_in_highest_frequency'
    META(1)%VARNS=''
    META(1)%VARNG='sea_surface_wave_dominant_mean_square_slope_direction_in_highest_frequency'
    META(1)%VARND = DIRCOM
    META(1)%VMIN = 0
    META(1)%VMAX = 360
    ! IFI=8, IFJ=5, QP
    META => GROUP(8)%FIELD(5)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.qp'
    META(1)%VARNM='qp'
    META(1)%VARNL='peakedness'
    !META(1)%VARNS='sea_surface_wave_peakedness'
    META(1)%VARNS=''
    META(1)%VARNG='wave_peakedness'
    META(1)%VARNC='Goda wave peakedness parameter'
    META(1)%VMIN = 0
    META(1)%VMAX = 32
    ! IFI=8, IFJ=6, QKK
    META => GROUP(8)%FIELD(6)%META
    META(1)%FSC    = 0.05
    META(1)%UNITS  = 'm/rad'
    META(1)%ENAME  = '.qkk'
    META(1)%VARNM='qkk'
    META(1)%VARNL='k-peakedness'
    !META(1)%VARNS='sea_surface_wave_peakedness'
    META(1)%VARNS=''
    META(1)%VARNG='wavenumber_peakedness'
    META(1)%VARNC='2D wavenumber peakedness'
    META(1)%VMIN = 0
    META(1)%VMAX = 1600
    ! IFI=8, IFJ=7, SKW
    META => GROUP(8)%FIELD(7)%META
    META(1)%FSC    = 0.00001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.skw'
    META(1)%VARNM='skw'
    META(1)%VARNL='skewness'
    !META(1)%VARNS='sea_surface_wave_peakedness'
    META(1)%VARNS=''
    META(1)%VARNG='skewness of P(z,sx,sy=0)'
    META(1)%VARNC='skewness of P(z,sx,sy=0)'
    META(1)%VMIN = 0
    META(1)%VMAX = 1
    ! IFI=8, IFJ=8, EMB
    META => GROUP(8)%FIELD(8)%META
    META(1)%FSC    = 0.00001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.emb'
    META(1)%VARNM='emb'
    META(1)%VARNL='EM-bias'
    !META(1)%VARNS='sea_surface_wave_peakedness'
    META(1)%VARNS=''
    META(1)%VARNG='EM bias coefficient'
    META(1)%VARNC='EM bias coefficient'
    META(1)%VMIN = -1
    META(1)%VMAX = 1    
    ! IFI=8, IFJ=7, SKW
    META => GROUP(8)%FIELD(9)%META
    META(1)%FSC    = 0.00001
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.emc'
    META(1)%VARNM='emc'
    META(1)%VARNL='trackerbias'
    !META(1)%VARNS='sea_surface_wave_peakedness'
    META(1)%VARNS=''
    META(1)%VARNG='tracker bias coefficient'
    META(1)%VARNC='tracker bias coefficient'
    META(1)%VMIN = -1
    META(1)%VMAX = 1    !
    !
    !---------- GROUP 9 ----------------
    !
    ! IFI=9, IFJ=1, DTD
    META => GROUP(9)%FIELD(1)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'min.'
    META(1)%ENAME  = '.dtd'
    META(1)%VARNM='dtd'
    META(1)%VARNL='dynamic time step'
    !META(1)%VARNS='dynamic_time_step'
    META(1)%VARNS=''
    META(1)%VARNG='dynamic_time_step'
    META(1)%VMIN = 0
    META(1)%VMAX = 3200
    ! IFI=9, IFJ=2, FC
    META => GROUP(9)%FIELD(2)%META
    META(1)%FSC    = 0.001
    META(1)%UNITS  = 's-1'
    META(1)%ENAME  = '.fc'
    META(1)%VARNM='fc'
    META(1)%VARNL='cut off frequency'
    !META(1)%VARNS='cut_off_frequency'
    META(1)%VARNS=''
    META(1)%VARNG='cut_off_frequency'
    META(1)%VMIN = 0
    META(1)%VMAX = 8
    ! IFI=9, IFJ=3, CFX
    META => GROUP(9)%FIELD(3)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.cfx'
    META(1)%VARNM='cfx'
    META(1)%VARNL='maximum cfl for spatial advection'
    !META(1)%VARNS='maximum_cfl_for_spatial_advection'
    META(1)%VARNS=''
    META(1)%VARNG='maximum_cfl_for_spatial_advection'
    META(1)%VMIN = 0
    META(1)%VMAX = 320
    ! IFI=9, IFJ=4, CFD
    META => GROUP(9)%FIELD(4)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.cfd'
    META(1)%VARNM='cfd'
    META(1)%VARNL='maximum cfl for direction advection'
    !META(1)%VARNS='maximum_cfl_for_direction_advection'
    META(1)%VARNS=''
    META(1)%VARNG='maximum_cfl_for_direction_advection'
    META(1)%VMIN = 0
    META(1)%VMAX = 320
    ! IFI=9, IFJ=5, CFK
    META => GROUP(9)%FIELD(5)%META
    META(1)%FSC    = 0.01
    META(1)%UNITS  = '1'
    META(1)%ENAME  = '.cfk'
    META(1)%VARNM='cfk'
    META(1)%VARNL='maximum cfl for frequency advection'
    !META(1)%VARNS='maximum_cfl_for_frequency_advection'
    META(1)%VARNS=''
    META(1)%VARNG='maximum_cfl_for_frequency_advection'
    META(1)%VMIN = 0
    META(1)%VMAX = 320
    !
    ! ------ Group 10 (User defined) -------
    !
    ! IFI=10, IFJ=1
    META => GROUP(10)%FIELD(1)%META
    META(1)%FSC    = 0.1
    META(1)%UNITS  = 'm'
    META(1)%VMIN = 0
    META(1)%VMAX = 0
    WRITE (META(1)%ENAME,'(A2,I2.2)') '.u'
    WRITE (META(1)%VARNM,'(A1,I2.2)') 'u'
    WRITE (META(1)%VARNL,'(A12,I2.2)') 'User_defined'
    WRITE (META(1)%VARNS,'(A12,I2.2)') 'User_defined'
    WRITE (META(1)%VARNG,'(A12,I2.2)') 'user_defined'
    !
  END SUBROUTINE DEFAULT_META
  !/ ------------------------------------------------------------------- /

END MODULE W3OUNFMETAMD
