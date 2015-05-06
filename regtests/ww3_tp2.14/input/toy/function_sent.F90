SUBROUTINE function_sent(IOUTDIAG_UNIT,ni,nj, &
                         coords_1,coords_2, &
                         fnc_ana,ib, CTYPE_FCT, VALUE)
  !*********************************************************************************************************************
  !
  IMPLICIT NONE
  !
#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(6,37)   ! real
#elif USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307) ! double
#endif
  !
  ! Constants
  !
  REAL (kind=wp), PARAMETER    :: dp_pi=3.14159265359
  REAL (kind=wp), PARAMETER    :: dp_length= 1000.0
  REAL (kind=wp), PARAMETER    :: dp_conv = dp_pi/180.
  !
  INTEGER, INTENT(in) :: ni,nj,ib
  INTEGER, INTENT(in) :: IOUTDIAG_UNIT
  !
  INTEGER             :: i,j
  !
  REAL (kind=wp), INTENT(out)         :: fnc_ana(ni,nj)
  !
  REAL (kind=wp), INTENT(IN)          :: coords_1(ni,nj)
  REAL (kind=wp), INTENT(IN)          :: coords_2(ni,nj)
  !
  CHARACTER(LEN=5), INTENT(IN)        :: CTYPE_FCT
  REAL, INTENT(IN)                    :: VALUE
  !
  !
  IF (CTYPE_FCT .EQ. 'CNSTE') THEN
    FNC_ANA(:,:)=VALUE
  ELSE IF (CTYPE_FCT .EQ. 'SINUS') THEN
    DO j=1,nj
      DO i=1,ni
        FNC_ANA(i,j) = VALUE*SIN(coords_2(i,j)*dp_conv*dp_length + dp_pi/100.0*ib)
      ENDDO
    ENDDO
  ELSE
    WRITE(IOUTDIAG_UNIT,*) 'PROBLEM DURING DEFINITION OF THE FUNCTION ANALYTIC : ', CTYPE_FCT
    CALL ABORT
  END IF
  !
  !
END SUBROUTINE function_sent
!
