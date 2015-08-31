!-------------------------------------------------------------------
! BOP
!
! !MODULE: mod_oasis_kinds
MODULE mod_oasis_kinds
!
! !USES:
!
! !PUBLIC TYPES:
IMPLICIT NONE
public
SAVE
!
! !PUBLIC MEMBER FUNCTIONS:
!
! !PARAMETERS:
  INTEGER, PARAMETER :: ic_lvar = 128
  INTEGER, PARAMETER :: ic_sm   = 8
  INTEGER, PARAMETER :: ic_med  = 64
  INTEGER, PARAMETER :: ic_long = 256
  INTEGER, PARAMETER :: ic_xl   = 1024
  INTEGER, PARAMETER :: ic_field = 1000
  INTEGER, PARAMETER :: ip_single_p = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER :: ip_double_p = SELECTED_REAL_KIND(12,307)
  INTEGER, PARAMETER :: ip_realwp_p = ip_double_p
  LOGICAL, PARAMETER :: ll_single = .FALSE.  
  INTEGER, PARAMETER :: ip_i2_p = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: ip_i4_p = SELECTED_INT_KIND(9)
#ifdef SX
  INTEGER, PARAMETER :: ip_i8_p = SELECTED_INT_KIND(15)
#else
  INTEGER, PARAMETER :: ip_i8_p = SELECTED_INT_KIND(18)
#endif
  INTEGER, PARAMETER :: ip_intwp_p = ip_i4_p 
  INTEGER, PARAMETER :: ip_r8_p = ip_double_p
!
! !PUBLIC DATA MEMBERS:
!
  INTEGER(kind=ip_intwp_p)  :: nulprt, nulprt1
  INTEGER(kind=ip_i4_p)	    :: OASIS_debug
  INTEGER(kind=ip_i4_p)     :: TIMER_debug
!
! !DESCRIPTION:
! This modules contains the parameters defining the precision used for 
! real and integer variables
!   
END MODULE mod_oasis_kinds

