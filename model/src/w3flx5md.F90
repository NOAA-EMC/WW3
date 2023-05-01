!> @file
!> @brief Unified process to obtain friction velocity and drag when stresses
!>  are an input (from atmospheric model).
!>
!> @author N.G. Valiente
!> @author J. Edward
!> @author A. Saulter
!> @date   01-Jul-2021
!>

#include "w3macros.h"

!/ ------------------------------------------------------------------- /
!>
!> @brief Unified process to obtain friction velocity and drag when stresses
!>  are an input (from atmospheric model).
!>
!> @author N.G. Valiente
!> @author J. Edward
!> @author A. Saulter
!> @date   01-Jul-2021
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3FLX5MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |                                   |
  !/                  |           N.G. Valiente           |
  !/                  |           J. Edward               |
  !/                  |           A. Saulter              |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         01-Jul-2021 |
  !/                  +-----------------------------------+
  !/
  !/    22-Mar-2021 : Origination.                        ( version 7.14 )
  !/    22-Mar-2021 : Enable direct use of atmospheric model wind stress
  !/                  by source terms ST6
  !/    01-Jul-2021 : Enable direct use of atmospheric model wind stress
  !/                  by source terms ST4
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Unified process to obtain friction velocity and drag when stresses are an
  !     input (from atmospheric model).
  !
  !     References:
  !          XX
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3FLX5    Subr. Public   Stresses closure
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
  !> @brief Unified process to obtain friction velocity and drag when
  !>  stresses are an input (from atmospheric model).
  !>
  !> @verbatim
  !>     UST       = SQRT(TAUA / RHOAIR)
  !>     USTD      = TAUADIR
  !>     CD        = (UST/U10)**2
  !>     SQRTCDM1  = MIN(U10/UST,100.0)
  !>     Z0        = ZWND*EXP(-KAPPA*SQRTCDM1)
  !> @endverbatim
  !>
  !> @param[in]  ZWND    Wind height.
  !> @param[in]  U10     Wind speed.
  !> @param[in]  U10D    Wind direction.
  !> @param[in]  TAUA    Atmosphere total stress.
  !> @param[in]  TAUADIR Atmosphere total stress directions.
  !> @param[in]  RHOAIR  Air density.
  !> @param[out] UST     Friction velocity.
  !> @param[out] USTD    Direction of friction velocity.
  !> @param[out] Z0      Z0 in profile law.
  !> @param[out] CD      Drag coefficient.
  !> @param[out] CHARN   Charnock coefficient.
  !>
  !> @author N.G. Valiente
  !> @author J. Edward
  !> @author A. Saulter
  !> @date   01-Jul-2021
  !>
  SUBROUTINE W3FLX5 ( ZWND, U10, U10D, TAUA, TAUADIR, RHOAIR, UST, USTD, Z0, CD, CHARN )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                     |
    !/                  |           N.G. Valiente           |
    !/                  |           J. Edward               |
    !/                  |           A. Saulter              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         01-Jul-2021 |
    !/                  +-----------------------------------+
    !/
    !/    22-Mar-2021 : Origination.                        ( version 7.14 )
    !/    22-Mar-2021 : Enable direct use of atmospheric model wind stress
    !/                  by source terms ST6
    !/    01-Jul-2021 : Enable direct use of atmospheric model wind stress
    !/                  by source terms ST4
    !/
    !  1. Purpose :
    !
    !     Unified process to obtain friction velocity and drag when stresses are an
    !     input (from atmospheric model).
    !
    !  2. Method :
    !
    !     UST       = SQRT(TAUA / RHOAIR)
    !     USTD      = TAUADIR
    !     CD        = (UST/U10)**2
    !     SQRTCDM1  = MIN(U10/UST,100.0)
    !     Z0        = ZWND*EXP(-KAPPA*SQRTCDM1)
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       ZWND    Real   I   Wind height.
    !       U10     Real   I   Wind speed.
    !       U10D    Real   I   Wind direction.
    !       TAUA    Real   I   Atm. total stress.
    !       TAUADIR Real   I   Atm. total stress direction.
    !       RHOAIR  Real   I   Air density.
    !       UST     Real   O   Friction velocity.
    !       USTD    Real   0   Direction of friction velocity.
    !       Z0      Real   O   z0 in profile law.
    !       CD      Real   O   Drag coefficient.
    !       CHARN   Real   O   Charnock coefficient
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
    USE CONSTANTS, ONLY: KAPPA, GRAV, nu_air
    USE W3ODATMD, ONLY: NDSE, IAPROC, NAPERR
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
    REAL, INTENT(IN)        :: ZWND, U10, U10D, TAUA, TAUADIR, RHOAIR
    REAL, INTENT(OUT)       :: UST, USTD, Z0, CD, CHARN
    REAL                    :: UNZ, SQRTCDM1
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
    CALL STRACE (IENT, 'W3FLX5')
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
    !
    UST    = MAX ( 1E-4, SQRT(TAUA/RHOAIR) )
    UNZ    = MAX ( 0.01 , U10 )
    CD     = (UST/UNZ)**2
    USTD   = TAUADIR
    SQRTCDM1  = MIN(UNZ/UST,100.0)
    Z0        = ZWND*EXP(-KAPPA*SQRTCDM1)
    IF (UNZ.GT.2.5) THEN
      CHARN = (Z0 - 0.11 * NU_AIR / UST) * GRAV / UST**2
      CHARN = MAX( CHARN , 0.0095 )
      CHARN = MIN( 0.035 , CHARN )
    ELSE
      CHARN = 0.0095
    END IF
    !
    RETURN
    !
    ! Formats
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3FLX5 : '/                 &
         '     HEIGHT OF WIND SHOULD BE 10m IN THIS APPROACH '/   &
         '     ZWND =',F8.2,'m'/)
    !/
    !/ End of W3FLX5 ----------------------------------------------------- /
    !/
  END SUBROUTINE W3FLX5
  !/
  !/ End of module W3FLX5MD -------------------------------------------- /
  !/
END MODULE W3FLX5MD
