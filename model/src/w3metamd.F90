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

      ! Values to represent "unset" data:
      CHARACTER(LEN=*), PARAMETER :: UNSETC = "unset"
      REAL, PARAMETER             :: UNSETR = HUGE(1.0)

      ! Type for storing a user defined metadata pair:
      TYPE META_PAIR_T
        CHARACTER(LEN=64)  :: ATTNAME = UNSETC
        CHARACTER(LEN=120) :: ATTVAL = UNSETC
        CHARACTER          :: TYPE = 'c'    ! c,i,f/r
        TYPE(META_PAIR_T), POINTER  :: NEXT
      END TYPE META_PAIR_T

      TYPE META_LIST_T
        TYPE (META_PAIR_T), POINTER :: HEAD => NULL(), TAIL => NULL()
        INTEGER                     :: N = 0
      END TYPE META_LIST_T

      ! Interface to facilitate adding real/int/character values to list
      INTERFACE META_LIST_APPEND
        MODULE PROCEDURE META_LIST_APPEND_M
        MODULE PROCEDURE META_LIST_APPEND_R
        MODULE PROCEDURE META_LIST_APPEND_I
        MODULE PROCEDURE META_LIST_APPEND_C
      END INTERFACE META_LIST_APPEND


      CONTAINS
      

!/ ------------------------------------------------------------------- /
      SUBROUTINE DEL_META_LIST(LIST)
!     Deletes all entries in list
!/ ------------------------------------------------------------------- /
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
      FUNCTION COPY_META_LIST(LIST) RESULT(COPY)
!     Create a deep copy of a meta data list
!/ ------------------------------------------------------------------- /
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
      SUBROUTINE PRINT_META_LIST(LIST)
!     Prints meta pairs in list to screen
!/ ------------------------------------------------------------------- /
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
      SUBROUTINE META_LIST_APPEND_M(LIST, META)
!     Append META_PAIR_T object to list
!/ ------------------------------------------------------------------- /
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
      SUBROUTINE META_LIST_APPEND_R(LIST, ATTNAME, RVAL)
!     Append real value meta pair to list
!/ ------------------------------------------------------------------- /
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
      SUBROUTINE META_LIST_APPEND_I(LIST, ATTNAME, IVAL)
!     Append integer value meta pair to list
!/ ------------------------------------------------------------------- /
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
      SUBROUTINE META_LIST_APPEND_C(LIST, ATTNAME, SVAL)
!     Append character value meta pair to list
!/ ------------------------------------------------------------------- /
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

!     Append pair to list


!/ ------------------------------------------------------------------- /
      SUBROUTINE META_LIST_FIND_ATTR(LIST, ATTN, META, ERR)
!     Find (first) entry in list with matching attname
!/ ------------------------------------------------------------------- /
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
      FUNCTION META_LIST_HAS_ATTR(LIST, ATTN) RESULT(FOUND)
!     Tests whether list contains an entry with specified attname
!/ ------------------------------------------------------------------- /
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
