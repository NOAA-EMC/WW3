!> @file
!> @brief Flux/stress computations according to Hwang (2011).
!>
!> @author H. L. Tolman
!> @author S. Zieger
!> @author Q. Liu
!> @date   24-Nov-2017
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Flux/stress computations according to Hwang ( 2011).
!>
!> @details Hwang 2011: J Atmos Ocean Tech 28(3)  436-443.
!>
!> @author H. L. Tolman
!> @author S. Zieger
!> @author Q. Liu
!> @date   24-Nov-2017
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3FLX4MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |           S. Zieger               |
  !/                  |           Q. Liu                  |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         24-Nov-2017 |
  !/                  +-----------------------------------+
  !/
  !/    03-Jul-2006 : Origination.                        ( version 3.09 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    15-Mar-2011 : Implementation of Hwang (2011)
  !/                  parameterization.
  !/    24-Nov_2017 : Modifying CDFAC                     ( Q. Liu)
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Flux/stress computations according to Hwang ( 2011).
  !
  !     References:
  !          Hwang 2011: J Atmos Ocean Tech 28(3)  436-443
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3FLX4    Subr. Public   Stresses according to Hwang (JTech, 2011)
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      STRACE    Subr. W3SERVMD Subroutine tracing.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !  6. Switches :
  !
  !     !/S  Enable subroutine tracing.
  !
  !  7. Source code :
  !/
  !/ ------------------------------------------------------------------- /
  !/
  PUBLIC
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Flux/stress computations according to Hwang (JTECH, 2011).
  !>
  !> @verbatim
  !>     CD    = 1E-4 ( -0.016 U10**2 + 0.967U10 + 8.058)
  !>     USTAR = U10 * SQRT( U10 )
  !> @endverbatim
  !>
  !> @param[in]  ZWND Wind height.
  !> @param[in]  U10  Wind speed.
  !> @param[in]  U10D Wind direction.
  !> @param[out] UST  Friction velocity.
  !> @param[out] USTD Direction of friction velocity.
  !> @param[out] Z0   Z0 in profile law.
  !> @param[out] CD   Drag coefficient.
  !>
  !> @author H. L. Tolman
  !> @date   03-Jul-2006
  !>
  SUBROUTINE W3FLX4 ( ZWND, U10, U10D, UST, USTD, Z0, CD )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         03-Jul-2006 |
    !/                  +-----------------------------------+
    !/
    !/    03-Jul-2006 : Origination.                        ( version 3.09 )
    !/
    !  1. Purpose :
    !
    !     Flux/stress computations according to Hwang (JTECH, 2011)
    !
    !  2. Method :
    !
    !     CD    = 1E-4 ( -0.016 U10**2 + 0.967U10 + 8.058)
    !     USTAR = U10 * SQRT( U10 )
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       ZWND    Real   I   Wind height.
    !       U10     Real   I   Wind speed.
    !       U10D    Real   I   Wind direction.
    !       UST     Real   O   Friction velocity.
    !       USTD    Real   0   Direction of friction velocity.
    !       Z0      Real   O   z0 in profile law.
    !       CD      Real   O   Drag coefficient.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SRCE    Subr. W3SRCEMD Source term integration.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3ODATMD, ONLY: NDSE, IAPROC, NAPERR
    USE W3GDATMD, ONLY: FLX4A0
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !/
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    REAL, INTENT(IN)        :: ZWND, U10, U10D
    REAL, INTENT(OUT)       :: UST, USTD, Z0, CD
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3FLX4')
#endif
    !
    ! 1.  Tests ---------------------------------------------------------- *
    !
    IF ( ABS(ZWND-10.) .GT. 0.01 ) THEN
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) ZWND
      CALL EXTCDE (1)
    END IF
    !
    ! 2.  Computation ---------------------------------------------------- *
    !
    !     To prevent the drag coefficient from dropping to zero at extreme
    !     wind speeds, we use a simple modification UST = 2.026 m/s for
    !     U10 greater than 50.33 m/s.
    !
    IF (U10 .GE. 50.33) THEN
      UST = 2.026 * SQRT(FLX4A0)
      CD  = (UST/U10)**2
    ELSE
      CD  = FLX4A0 * ( 8.058 + 0.967*U10 - 0.016*U10**2 ) * 1E-4
      UST = U10 * SQRT(CD)
    END IF
    !
    Z0     = ZWND * EXP ( -0.4 / SQRT(CD) )
    USTD   = U10D
    !
    RETURN
    !
    ! Formats
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLX4 : '/               &
         '     HIGHT OF WIND SHOULD BE 10m IN THIS APPRACH '/   &
         '     ZWND =',F8.2,'m'/)
    !/
    !/ End of W3FLX4 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3FLX4
  !/
  !/ End of module W3FLX4MD -------------------------------------------- /
  !/
END MODULE W3FLX4MD
