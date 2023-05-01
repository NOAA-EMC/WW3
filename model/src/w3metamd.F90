!> @file
!> @brief Dynamic storage for meta data attribute/value pairs.
!> @author Chris Bunney @date 16-Dec-2020
!/ ------------------------------------------------------------------- /
!/
!> @brief Dynamic storage for meta data attribute/value pairs.
!>
!> @details Provides types for handling "meta data" (attribute/value pairs)
!>     and a linked list construct for dynamic storage.
!>
!> ### Change log
!>   Date      | Ver  | Comments
!> ------------|------|---------
!> 16-Dec-2020 | 7.12 | Creation
!>
!> @author Chris Bunney @date 16-Dec-2020
!>
!/ ------------------------------------------------------------------- /
MODULE W3METAMD
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
  !  1. Purpose :
  !
  !     Provides types for handling "meta data" (attribute/value pairs)
  !     and a linked list construct for dynamic storage.
  !/ ------------------------------------------------------------------- /

  !> Value to represent "unset" character variable
  CHARACTER(LEN=*), PARAMETER :: UNSETC = "unset"
  !> Value to represent "unset" real variable
  REAL, PARAMETER             :: UNSETR = HUGE(1.0)

  !> Type for storing a user defined metadata pair as linked list element
  TYPE META_PAIR_T
    CHARACTER(LEN=64)  :: ATTNAME = UNSETC  !< Attribute name
    CHARACTER(LEN=120) :: ATTVAL = UNSETC   !< Attribute value
    CHARACTER          :: TYPE = 'c'        !< Attribute type (c,i,f/r)
    TYPE(META_PAIR_T), POINTER  :: NEXT     !< Pointer to next node
  END TYPE META_PAIR_T

  !> Linked list of meta data pairs
  TYPE META_LIST_T
    TYPE (META_PAIR_T), POINTER :: HEAD => NULL(), TAIL => NULL()
    INTEGER                     :: N = 0    !< Num elements in list
  END TYPE META_LIST_T

  !> Interface to facilitate adding real/int/character values to list
  INTERFACE META_LIST_APPEND
    MODULE PROCEDURE META_LIST_APPEND_M !< Append a META_PAIR_T
    MODULE PROCEDURE META_LIST_APPEND_R !< Append a REAL value
    MODULE PROCEDURE META_LIST_APPEND_I !< Append an INTEGER value
    MODULE PROCEDURE META_LIST_APPEND_C !< Append a CHARACTER value
  END INTERFACE META_LIST_APPEND

CONTAINS


  !/ ------------------------------------------------------------------- /
  !> @brief Deletes all entries in list.
  !>
  !> @param[in,out] LIST The list to clear.
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  SUBROUTINE DEL_META_LIST(LIST)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(INOUT) :: LIST
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T), POINTER :: P

    IF(.NOT. ASSOCIATED(LIST%HEAD)) RETURN

    DO
      NULLIFY(P)
      IF(ASSOCIATED(LIST%HEAD%NEXT)) P => LIST%HEAD%NEXT
      DEALLOCATE(LIST%HEAD)
      IF(.NOT. ASSOCIATED(P)) EXIT
      LIST%HEAD => P
    ENDDO

    NULLIFY(LIST%HEAD)
    NULLIFY(LIST%TAIL)
    LIST%N = 0

  END SUBROUTINE DEL_META_LIST


  !/ ------------------------------------------------------------------- /
  !> @brief Create a deep copy of a meta data list
  !>
  !> @details A "deep copy" ensures that a copy is made of the underlying
  !>    linked list, rather than a simply copy of the pointers to the
  !>    existing list.
  !>
  !> @param[in] LIST The list to copy
  !>
  !> @returns A new META_LIST_T
  !>
  !> @author Chris Bunney
  !>
  !/ ------------------------------------------------------------------- /
  FUNCTION COPY_META_LIST(LIST) RESULT(COPY)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(IN) :: LIST
    TYPE(META_LIST_T)             :: COPY
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T), POINTER :: P

    NULLIFY(COPY%HEAD)
    NULLIFY(COPY%TAIL)
    COPY%N = 0
    IF(LIST%N .EQ. 0) RETURN

    ! Deep copy list
    P => LIST%HEAD
    DO
      CALL META_LIST_APPEND_M(COPY, P)
      IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
      P => P%NEXT
    ENDDO

  END FUNCTION COPY_META_LIST


  !/ ------------------------------------------------------------------- /
  !> @brief Prints meta pairs in list to screen
  !>
  !> @param[in] LIST  Linked list of meta data to print
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PRINT_META_LIST(LIST)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(IN) :: LIST
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T), POINTER :: P

    IF(.NOT. ASSOCIATED(LIST%HEAD)) THEN
      WRITE(*,*) 'List empty.'
      RETURN
    ENDIF

    P => LIST%HEAD
    DO
      WRITE(*, '(A," [",A1,"] : ", A)') TRIM(P%ATTNAME), P%TYPE,      &
           TRIM(P%ATTVAL)
      IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
      P => P%NEXT
    ENDDO

  END SUBROUTINE PRINT_META_LIST


  !/ ------------------------------------------------------------------- /
  !> @brief Append META_PAIR_T object to list
  !>
  !> @param[in,out] LIST  The list to append to
  !> @param[in]     META  The META_PAIR_T object to append.
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  SUBROUTINE META_LIST_APPEND_M(LIST, META)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(INOUT) :: LIST
    TYPE(META_PAIR_T), INTENT(IN) :: META
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T), POINTER :: P

    ALLOCATE(P)

    ! Empty list?
    IF(LIST%N .EQ. 0) THEN
      !IF(.NOT. ASSOCIATED(LIST%HEAD)) THEN
      LIST%HEAD => P
    ELSE
      LIST%TAIL%NEXT => P
    ENDIF
    LIST%TAIL => P

    P%ATTNAME = META%ATTNAME
    P%ATTVAL = META%ATTVAL
    P%TYPE = META%TYPE

    NULLIFY(P%NEXT)

    LIST%N = LIST%N + 1

  END SUBROUTINE META_LIST_APPEND_M


  !/ ------------------------------------------------------------------- /
  !> @brief Append REAL value attribute to list
  !>
  !> @param[in,out] LIST     The list to append to
  !> @param[in]     ATTNAME  The attribute name
  !> @param[in]     RVAL     The attribute value (REAL)
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  SUBROUTINE META_LIST_APPEND_R(LIST, ATTNAME, RVAL)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(INOUT) :: LIST
    CHARACTER(*), INTENT(IN)         :: ATTNAME
    REAL, INTENT(IN)                 :: RVAL
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T) :: META

    META%ATTNAME = ATTNAME
    WRITE(META%ATTVAL,*) RVAL
    META%TYPE = 'r'
    CALL META_LIST_APPEND(LIST, META)

  END SUBROUTINE META_LIST_APPEND_R


  !/ ------------------------------------------------------------------- /
  !> @brief Append INTEGER value attribute to list
  !>
  !> @param[in,out] LIST     The list to append to
  !> @param[in]     ATTNAME  The attribute name
  !> @param[in]     IVAL     The attribute value (INTEGER)
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  SUBROUTINE META_LIST_APPEND_I(LIST, ATTNAME, IVAL)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(INOUT) :: LIST
    CHARACTER(*), INTENT(IN)         :: ATTNAME
    INTEGER, INTENT(IN)              :: IVAL
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T) :: META

    META%ATTNAME = ATTNAME
    WRITE(META%ATTVAL,*) IVAL
    META%TYPE = 'i'
    CALL META_LIST_APPEND(LIST, META)

  END SUBROUTINE META_LIST_APPEND_I


  !/ ------------------------------------------------------------------- /
  !> @brief Append CHARACTER string value attribute to list
  !>
  !> @param[in,out] LIST     The list to append to
  !> @param[in]     ATTNAME  The attribute name
  !> @param[in]     SVAL     The attribute value (CHARACTER string)
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  SUBROUTINE META_LIST_APPEND_C(LIST, ATTNAME, SVAL)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(INOUT) :: LIST
    CHARACTER(*), INTENT(IN)         :: ATTNAME, SVAL
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T) :: META

    META%ATTNAME = ATTNAME
    META%ATTVAL = SVAL
    META%TYPE = 'c'
    CALL META_LIST_APPEND(LIST, META)

  END SUBROUTINE META_LIST_APPEND_C


  !/ ------------------------------------------------------------------- /
  !> @brief Find (first) entry in list with matching attname
  !>
  !> @param[in]   LIST   List to search
  !> @param[in]   ATTN   Attribute name to search for
  !> @param[out]  META   Meta data type to store matched result in
  !> @param[out]  ERR    Error status (0=Found, 1=Empty list, 2=Not found)
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  SUBROUTINE META_LIST_FIND_ATTR(LIST, ATTN, META, ERR)
    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(IN)           :: LIST
    CHARACTER(*), INTENT(IN)                :: ATTN
    TYPE(META_PAIR_T), POINTER, INTENT(OUT) :: META
    INTEGER, INTENT(OUT)                    :: ERR

    ERR = 0

    ! Empty list?
    IF(.NOT. ASSOCIATED(LIST%HEAD)) THEN
      ERR = 1
      RETURN
    ENDIF

    META => LIST%HEAD

    DO
      IF(TRIM(META%ATTNAME) == TRIM(ATTN)) RETURN
      IF(.NOT. ASSOCIATED(META%NEXT)) EXIT
      META => META%NEXT
    ENDDO

    ! Not found
    NULLIFY(META)
    ERR = 2

  END SUBROUTINE META_LIST_FIND_ATTR


  !/ ------------------------------------------------------------------- /
  !> @brief Tests whether list contains an entry with specified attname
  !>
  !> @param[in]  LIST  The list to search
  !> @param[in]  ATTN  Attribute name to search for
  !>
  !> @returns LOGICAL: True if match found, False otherwise.
  !>
  !> @author Chris Bunney
  !/ ------------------------------------------------------------------- /
  FUNCTION META_LIST_HAS_ATTR(LIST, ATTN) RESULT(FOUND)

    IMPLICIT NONE

    TYPE(META_LIST_T), INTENT(IN)           :: LIST
    CHARACTER(*), INTENT(IN)                :: ATTN
    LOGICAL                                 :: FOUND
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    TYPE(META_PAIR_T), POINTER :: P

    FOUND = .FALSE.

    ! Empty list?
    IF(.NOT. ASSOCIATED(LIST%HEAD)) THEN
      RETURN
    ENDIF

    P => LIST%HEAD

    DO
      IF(TRIM(P%ATTNAME) == TRIM(ATTN)) THEN
        FOUND = .TRUE.
        RETURN
      ENDIF

      IF(.NOT. ASSOCIATED(P%NEXT)) EXIT
      P => P%NEXT
    ENDDO

  END FUNCTION META_LIST_HAS_ATTR

  !/ ------------------------------------------------------------------- /
END MODULE W3METAMD
!/ ------------------------------------------------------------------- /
