!> @file
!> @brief Contains MODULE W3UNO2MD, with UNO2 scheme.
!>
!> @author Jain-Guo Li  @date 1-Jul-2013
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Portable UNO2 scheme on irregular grid.
!>
!> @author Jain-Guo Li  @date 1-Jul-2013
!>
MODULE W3UNO2MD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           MetOffice |
  !/                  |           Jian-Guo Li             |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         01-Jul-2013 |
  !/                  +-----------------------------------+
  !/
  !/    Adapted from   WAVEWATCH-III  W3UQCKMD
  !/            for    UNO2 advection scheme.
  !/
  !/    18-Mar-2008 : Origination.                        ( version 3.14 )
  !/    ..-...-...  : .....                               ( version 3.14 )
  !/    19-Mar-2008 : last modified by Jian-Guo           ( version 3.14 )
  !/    01-Jul-2013 : Put in NCEP branch (Tolman).        ( version 4.12 )
  !/    08-Jan-2018 : Added OMPH switches in W3UNO2.      ( version 6.02 )
  !/
  !  1. Purpose :
  !
  !     Portable UNO2 scheme on irregular grid.
  !
  !  2. Variables and types :
  !
  !     None.
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3UNO2    Subr. Public   UNO2 scheme for irregular grid.
  !      W3UNO2r   Subr. Public   UNO2 scheme reduced to regular grid.
  !      W3UNO2s   Subr. Public   UNO2 regular grid with subgrid obstruction.
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
  !     - STRACE and !/S irrelevant for running code. The module is
  !       therefore fully portable to any other model.
  !
  !  6. Switches :
  !
  !       !/OMPH  Ading OMP directves for hybrid paralellization.
  !
  !       !/S     Enable subroutine tracing.
  !       !/Tn    Enable test output.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
#ifdef W3_S
  USE W3SERVMD, ONLY: STRACE
#endif
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief UNO2 scheme for irregular grid.
  !>
  !> @param[in] MX Field dimensions, if grid is 'closed' or circular, MX is the closed dimension.
  !> @param[in] MY       Field dimensions
  !> @param[in] NX       Part of field actually used
  !> @param[in] NY       Part of field actually used
  !> @param[inout] VELO  Local velocities            (MY,  MX+1).
  !> @param[in] DT       Time step.
  !> @param[inout] DX1   Band width at points        (MY,  MX+1).
  !> @param[inout] DX2   Band width between points   (MY,0:MX+1).
  !> @param[inout] Q     Propagated quantity.
  !> @param[in] BCLOSE   Flag for closed 'X' dimension.
  !> @param[in] INC      Increment in 1-D array corresponding to increment in 2-D space.
  !> @param[in] MAPACT   List of active grid points.
  !> @param[in] NACT     Size of MAPACT.
  !> @param[in] MAPBOU   Map with boundary information (see W3MAP2).
  !> @param[in] NB0      Counter in MAPBOU
  !> @param[in] NB1      Counter in MAPBOU
  !> @param[in] NB2      Counter in MAPBOU
  !> @param[in] NDSE     Error output unit number.
  !> @param[in] NDST     Test output unit number.
  !>
  !> @author Jain-Guo Li  @date 1-Jul-2013
  !>
  SUBROUTINE W3UNO2 (MX, MY, NX, NY, VELO, DT, DX1, DX2, Q,BCLOSE,&
       INC,  MAPACT, NACT, MAPBOU, NB0, NB1, NB2,    &
       NDSE, NDST )
    !/
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       MX,MY   Int.   I   Field dimensions, if grid is 'closed' or
    !                          circular, MX is the closed dimension.
    !       NX,NY   Int.   I   Part of field actually used.
    !       VELO    R.A.   I   Local velocities.              (MY,  MX+1)
    !       DT      Real   I   Time step.
    !       DX1     R.A.  I/O  Band width at points.          (MY,  MX+1)
    !       DX2     R.A.  I/O  Band width between points.     (MY,0:MX+1)
    !                          (local counter and counter+INC)
    !       Q       R.A.  I/O  Propagated quantity.           (MY,0:MX+2)
    !       BCLOSE  Log.   I   Flag for closed 'X' dimension'
    !       INC     Int.   I   Increment in 1-D array corresponding to
    !                          increment in 2-D space.
    !       MAPACT  I.A.   I   List of active grid points.
    !       NACT    Int.   I   Size of MAPACT.
    !       MAPBOU  I.A.   I   Map with boundary information (see W3MAP2).
    !       NBn     Int.   I   Counters in MAPBOU.
    !       NDSE    Int.   I   Error output unit number.
    !       NDST    Int.   I   Test output unit number.
    !     ----------------------------------------------------------------
    !           - VELO amd Q need only bee filled in the (MY,MX) range,
    !             extension is used internally for closure.
    !           - VELO and Q are defined as 1-D arrays internally.
    !
    !  4. Subroutines used :
    !
    !       STRACE   Service routine.
    !
    !  5. Called by :
    !
    !       W3XYP2   Propagation in physical space
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !     - This routine can be used independently from WAVEWATCH-III.
    !
    !  8. Structure :
    !
    !     ------------------------------------------------------
    !       1. Initialize aux. array FLA.
    !       2. Fluxes for central points (3rd order + limiter).
    !       3. Fluxes boundary point above (1st order).
    !       4. Fluxes boundary point below (1st order).
    !       5. Closure of 'X' if required
    !       6. Propagate.
    !     ------------------------------------------------------
    !
    !  9. Switches :
    !
    !     !/S   Enable subroutine tracing.
    !     !/T   Enable test output.
    !     !/T0  Test output input/output fields.
    !     !/T1  Test output fluxes.
    !     !/T2  Test output integration.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: MX, MY, NX, NY, INC, MAPACT(MY*MX),  &
         NACT, MAPBOU(MY*MX), NB0, NB1, NB2,  &
         NDSE, NDST
    REAL, INTENT(IN)        :: DT
    REAL, INTENT(INOUT)     :: VELO(MY*(MX+1)), DX1(MY*(MX+1)),     &
         DX2(1-MY:MY*(MX+1)), Q(1-MY:MY*(MX+2))
    LOGICAL, INTENT(IN)     :: BCLOSE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IXY, IP, IXYC, IXYU, IXYD, IY, IX,   &
         IAD00, IAD02, IADN0, IADN1, IADN2
#ifdef W3_S
    INTEGER, SAVE           :: IENT
#endif
#ifdef W3_T1
    INTEGER                 :: IX2, IY2
#endif
    REAL                    :: CFL, VEL, QB, DQ, DQNZ, QCN, QBN,    &
         QBR, CFAC, FLA(1-MY:MY*MX)
#ifdef W3_T0
    REAL                    :: QMAX
#endif
#ifdef W3_T1
    REAL                    :: QBO, QN, XCFL
#endif
#ifdef W3_T2
    REAL                    :: QOLD
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3UNO2')
#endif
    !
#ifdef W3_T
    WRITE (NDST,9000) MX, MY, NX, NY, DT, BCLOSE, INC, NB0, NB1, NB2
#endif
    !
#ifdef W3_T0
    QMAX   = 0.
    DO IY=1, NY
      DO IX=1, NX
        QMAX   = MAX ( QMAX , Q(IY+(IX-1)*MY) )
      END DO
    END DO
    QMAX   = MAX ( 0.01*QMAX , 1.E-10 )
#endif
    !
#ifdef W3_T0
    WRITE (NDST,9001) 'VELO'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(100.*VELO(IY+(IX-1)*MY)           &
           *DT/DX1(IY+(IX-1)*MY)),IX=1,NX)
    END DO
    WRITE (NDST,9001) 'Q'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(Q(IY+(IX-1)*MY)/QMAX),IX=1,NX)
    END DO
    WRITE (NDST,9001) 'MAPACT'
    WRITE (NDST,9003) (MAPACT(IXY),IXY=1,NACT)
#endif
    !
    ! 1.  Initialize aux. array FLA and closure ------------------------- *
    !
    FLA = 0.
    !
    IF ( BCLOSE ) THEN
#ifdef W3_T
      WRITE (NDST,9005)
#endif
      IAD00  = -MY
      IAD02  =  MY
      IADN0  = IAD00 + MY*NX
      IADN1  =         MY*NX
      IADN2  = IAD02 + MY*NX
      DO IY=1, NY
        Q   (IY+IAD00) = Q   (IY+IADN0)
        Q   (IY+IADN1) = Q   (   IY   )
        Q   (IY+IADN2) = Q   (IY+IAD02)
        VELO(IY+IADN1) = VELO(   IY   )
        DX1 (IY+IADN1) = DX1 (   IY   )
        DX2 (IY+IAD00) = DX1 (IY+IADN0)
        DX2 (IY+IADN1) = DX1 (   IY   )
      END DO
    END IF
    !
    ! 2.  Fluxes for central points ------------------------------------- *
    !     ( 2rd order UNO2 scheme )
    !
#ifdef W3_T1
    WRITE (NDST,9010)
    WRITE (NDST,9011) NB0, 'CENTRAL'
#endif
    !
    DO IP=1, NB0
      !
      IXY  = MAPBOU(IP)
      VEL  = 0.5 * ( VELO(IXY) + VELO(IXY+INC) )
      !  Assuming velocity is at cell centre, so face velocity is an average.
      CFL  = DT *  VEL
      !  Courant number without gradient distance (between IXY and IXY+INC cells)
      IXYC = IXY - INC * INT( MIN ( 0. , SIGN(1.1,CFL) ) )
      !  Central cell index, depending on flow direction.
      !          IXY for positive CFL, IXY+INC for negative CFL
      !  Upstream and downstream cell numbers
      IXYD = IXYC + INC * INT ( SIGN (1.1,CFL) )
      !  Minimum gradient is derived from the two sides of the central cell
      !
      QB   = Q(IXYC)+SIGN(0.5, Q(IXYD)-Q(IXYC))*(DX1(IXYC)-ABS(CFL)) &
           *MIN(ABS(Q(IXYC+INC)-Q(IXYC))/DX2(IXYC),         &
           ABS(Q(IXYC)-Q(IXYC-INC))/DX2(IXYC-INC) )
      !
#ifdef W3_T1
      QBO    = QB
#endif
      !
      FLA(IXY) = CFL * QB
      !
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( QB, QBO, Q(IXY-INC), Q(   IXY   ),         &
           Q(IXY+INC), Q(IXY+2*INC) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9012) IP, IX, IY, IX2, IY2,               &
             CFL, DT*VELO(IXY)/DX1(IXY),                 &
             DT*VELO(IXY+INC)/DX1(IXY+INC),         &
             QBO*QN, QB*QN, Q(IXY-INC)*QN, Q(   IXY   )*QN,  &
             Q(IXY+INC)*QN, Q(IXY+2*INC)*QN
      END IF
#endif
      !
    END DO
    !
    ! 3.  Fluxes for points with boundary above ------------------------- *
    !     ( 1st order without limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9011) NB1-NB0, 'BOUNDARY ABOVE'
#endif
    !
    DO IP=NB0+1, NB1
      IXY    = MAPBOU(IP)
      VEL    = VELO(IXY)
      IXYC   = IXY - INC * INT( MIN ( 0. , SIGN(1.1,VEL) ) )
      FLA(IXY) = VEL * DT * Q(IXYC)
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( Q(IXY+INC), Q(IXY) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9013) IP, IX, IY, IX2, IY2, XCFL,             &
             DT*VELO(IXY)/DX2(IXY),                  &
             Q(IXYC)*QN, Q(IXY)*QN, Q(IXY+INC)*QN
      END IF
#endif
    END DO
    !
    ! 4.  Fluxes for points with boundary below ------------------------- *
    !     ( 1st order without limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9011) NB2-NB1, 'BOUNDARY BELOW'
#endif
    !
    DO IP=NB1+1, NB2
      IXY    = MAPBOU(IP)
      VEL    = VELO(IXY+INC)
      IXYC   = IXY - INC * INT( MIN ( 0. , SIGN(1.1,VEL) ) )
      FLA(IXY) = VEL * DT * Q(IXYC)
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( Q(IXY+INC), Q(IXY) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9014) IP, IX, IY, IX2, IY2, XCFL,         &
             DT*VELO(IXY+INC)/DX2(IXY),          &
             Q(IXYC)*QN, Q(IXY)*QN, Q(IXY+INC)*QN
      END IF
#endif
    END DO
    !
    ! 5.  Global closure ----------------------------------------------- *
    !
    IF ( BCLOSE ) THEN
#ifdef W3_T
      WRITE (NDST,9015)
#endif
      DO IY=1, NY
        FLA (IY+IAD00) = FLA (IY+IADN0)
      END DO
    END IF
    !
    ! 6.  Propagation -------------------------------------------------- *
    !
#ifdef W3_T2
    WRITE (NDST,9020)
#endif
    DO IP=1, NACT
      IXY    = MAPACT(IP)
#ifdef W3_T2
      QOLD   = Q(IXY)
#endif
      ! Li    Update transported quantity with fluxes
      Q(IXY) = MAX( 0., Q(IXY)+( FLA(IXY-INC)-FLA(IXY) )/DX1(IXY) )
      ! Li    This positive filter is not necessary for UNO2 scheme but kept here.
#ifdef W3_T2
      IF ( QOLD + Q(IXY) .GT. 1.E-10 )                          &
           WRITE (NDST,9021) IP, IXY, QOLD, Q(IXY),             &
           DT*FLA(IXY-INC)/DX1(IXY),          &
           DT*FLA(IXY)/DX1(IXY)
#endif
    END DO
    !
#ifdef W3_T0
    WRITE (NDST,9001) 'Q'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(Q(IY+(IX-1)*MY)/QMAX),IX=1,NX)
    END DO
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST W3UNO2 : ARRAY DIMENSIONS  :',2I6/           &
         '               USED              :',2I6/           &
         '               TIME STEP         :',F8.1/          &
         '              BCLOSE, INC        :',L6,I6/         &
         '               NB0, NB1, NB2     :',3I6)
#endif
#ifdef W3_T0
9001 FORMAT ( ' TEST W3UNO2 : DUMP ARRAY ',A,' :')
9002 FORMAT ( 1X,43I3)
9003 FORMAT ( 1X,21I6)
#endif
#ifdef W3_T
9005 FORMAT (' TEST W3UNO2 : GLOBAL CLOSURE (1)')
#endif
    !
#ifdef W3_T1
9010 FORMAT (' TEST W3UNO2 : IP, 2x(IX,IY), CFL (b,i,i+1), ',    &
         ' Q (b,b,i-1,i,i+1,i+2)')
9011 FORMAT (' TEST W3UNO2 :',I6,' POINTS OF TYPE ',A)
9012 FORMAT (10X,I6,4I4,1X,3F6.2,1X,F7.2,F6.2,1X,4F6.2)
9013 FORMAT (10X,I6,4I4,1X,F6.2,F6.2,'  --- ',1X,F7.2,1X,'  --- ',&
         2F6.2,'  --- ')
9014 FORMAT (10X,I6,4I4,1X,F6.2,'  --- ',F6.2,1X,F7.2,1X,'  --- ',&
         2F6.2,'  --- ')
#endif
#ifdef W3_T
9015 FORMAT (' TEST W3UNO2 : GLOBAL CLOSURE (2)')
#endif
    !
#ifdef W3_T2
9020 FORMAT (' TEST W3UNO2 : IP, IXY, 2Q, 2FL')
9021 FORMAT ('            ',2I6,2(1X,2E11.3))
#endif
  END SUBROUTINE W3UNO2
  !/
  !/ End of W3UNO2 ----------------------------------------------------- /
  !>
  !> @brief Preform one-dimensional propagation in a two-dimensional space
  !>  with irregular boundaries and regular grid.
  !>
  !> @param[in] MX Field dimensions, if grid is 'closed' or circular, MX is the closed dimension.
  !> @param[in] MY       Field dimensions
  !> @param[in] NX       Part of field actually used
  !> @param[in] NY       Part of field actually used
  !> @param[inout] CFLL  Local Courant numbers         (MY,  MX+1).
  !> @param[inout] Q     Propagated quantity           (MY,0:MX+2).
  !> @param[in] BCLOSE   Flag for closed 'X' dimension.
  !> @param[in] INC      Increment in 1-D array corresponding to increment in 2-D space.
  !> @param[in] MAPACT   List of active grid points.
  !> @param[in] NACT     Size of MAPACT.
  !> @param[in] MAPBOU   Map with boundary information (see W3MAP2).
  !> @param[in] NB0      Counter in MAPBOU
  !> @param[in] NB1      Counter in MAPBOU
  !> @param[in] NB2      Counter in MAPBOU
  !> @param[in] NDSE     Error output unit number.
  !> @param[in] NDST     Test output unit number.
  !>
  !> @author Jain-Guo Li  @date 8-Jan-2018
  !>
  SUBROUTINE W3UNO2r (MX, MY, NX, NY, CFLL, Q, BCLOSE, INC,       &
       MAPACT, NACT, MAPBOU, NB0, NB1, NB2,         &
       NDSE, NDST )
    !/
    !/    Adapted from W3QCK1 for UNO2 regular grid scheme.
    !/            First created:    19 Mar 2008   Jian-Guo Li
    !/            Last modified:     8 Jan 2018   Jian-Guo Li
    !/
    !  1. Purpose :
    !
    !     Preform one-dimensional propagation in a two-dimensional space
    !     with irregular boundaries and regular grid.
    !
    !  2. Method :
    !
    !     UNO2 regular grid scheme
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       MX,MY   Int.   I   Field dimensions, if grid is 'closed' or
    !                          circular, MX is the closed dimension.
    !       NX,NY   Int.   I   Part of field actually used.
    !       CFLL    R.A.   I   Local Courant numbers.         (MY,  MX+1)
    !       Q       R.A.  I/O  Propagated quantity.           (MY,0:MX+2)
    !       BCLOSE  Log.   I   Flag for closed 'X' dimension'
    !       INC     Int.   I   Increment in 1-D array corresponding to
    !                          increment in 2-D space.
    !       MAPACT  I.A.   I   List of active grid points.
    !       NACT    Int.   I   Size of MAPACT.
    !       MAPBOU  I.A.   I   Map with boundary information (see W3MAP2).
    !       NBn     Int.   I   Counters in MAPBOU.
    !       NDSE    Int.   I   Error output unit number.
    !       NDST    Int.   I   Test output unit number.
    !     ----------------------------------------------------------------
    !           - CFLL amd Q need only bee filled in the (MY,MX) range,
    !             extension is used internally for closure.
    !           - CFLL and Q are defined as 1-D arrays internally.
    !
    !  4. Subroutines used :
    !
    !       STRACE   Service routine.
    !
    !  5. Called by :
    !
    !       W3XYP2   Propagation in physical space
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !     - This routine can be used independently from WAVEWATCH-III.
    !
    !  8. Structure :
    !
    !     ------------------------------------------------------
    !       1. Initialize aux. array FLA.
    !       2. Fluxes for central points (3rd order + limiter).
    !       3. Fluxes boundary point above (1st order).
    !       4. Fluxes boundary point below (1st order).
    !       5. Closure of 'X' if required
    !       6. Propagate.
    !     ------------------------------------------------------
    !
    !  9. Switches :
    !
    !     !/S   Enable subroutine tracing.
    !     !/T   Enable test output.
    !     !/T0  Test output input/output fields.
    !     !/T1  Test output fluxes.
    !     !/T2  Test output integration.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: MX, MY, NX, NY, INC, MAPACT(MY*MX),  &
         NACT, MAPBOU(MY*MX), NB0, NB1, NB2,  &
         NDSE, NDST
    REAL, INTENT(INOUT)     :: CFLL(MY*(MX+1)), Q(1-MY:MY*(MX+2))
    LOGICAL, INTENT(IN)     :: BCLOSE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IXY, IP, IXYC, IXYU, IXYD, IY, IX,   &
         IAD00, IAD02, IADN0, IADN1, IADN2
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T1
    INTEGER                 :: IX2, IY2
#endif
    REAL                    :: CFL, QB, DQ, DQNZ, QCN, QBN, QBR, CFAC
    REAL                    :: FLA(1-MY:MY*MX)
#ifdef W3_T0
    REAL                    :: QMAX
#endif
#ifdef W3_T1
    REAL                    :: QBO, QN
#endif
#ifdef W3_T2
    REAL                    :: QOLD
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3UNO2r')
#endif
    !
#ifdef W3_T
    WRITE (NDST,9000) MX, MY, NX, NY, BCLOSE, INC, NB0, NB1, NB2
#endif
    !
#ifdef W3_T0
    QMAX   = 0.
    DO IY=1, NY
      DO IX=1, NX
        QMAX   = MAX ( QMAX , Q(IY+(IX-1)*MY) )
      END DO
    END DO
    QMAX   = MAX ( 0.01*QMAX , 1.E-10 )
#endif
    !
#ifdef W3_T0
    WRITE (NDST,9001) 'CFLL'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(100.*CFLL(IY+(IX-1)*MY)),IX=1,NX)
    END DO
    WRITE (NDST,9001) 'Q'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(Q(IY+(IX-1)*MY)/QMAX),IX=1,NX)
    END DO
    WRITE (NDST,9001) 'MAPACT'
    WRITE (NDST,9003) (MAPACT(IXY),IXY=1,NACT)
#endif
    !
    ! 1.  Initialize aux. array FLA and closure ------------------------- *
    !
    FLA = 0.
    !
    IF ( BCLOSE ) THEN
#ifdef W3_T
      WRITE (NDST,9005)
#endif
      IAD00  = -MY
      IAD02  =  MY
      IADN0  = IAD00 + MY*NX
      IADN1  =         MY*NX
      IADN2  = IAD02 + MY*NX
      DO IY=1, NY
        Q   (IY+IAD00) = Q   (IY+IADN0)
        Q   (IY+IADN1) = Q   (   IY   )
        Q   (IY+IADN2) = Q   (IY+IAD02)
        CFLL(IY+IADN1) = CFLL(   IY   )
      END DO
    END IF
    !
    ! 2.  Fluxes for central points ------------------------------------- *
    !     ( 3rd order + limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9010)
    WRITE (NDST,9011) NB0, 'CENTRAL'
#endif
    !
    DO IP=1, NB0
      !
      IXY  = MAPBOU(IP)
      CFL  = 0.5 * ( CFLL(IXY) + CFLL(IXY+INC) )
      IXYC = IXY  - INC * INT( MIN ( 0. , SIGN(1.1,CFL) ) )
      IXYD = IXYC + INC * INT( SIGN (1.1,CFL) )
      QB   = Q(IXYC)+SIGN(0.5, Q(IXYD)-Q(IXYC))*(1.0-ABS(CFL)) &
           *MIN(ABS(Q(IXYC+INC)-Q(IXYC)),             &
           ABS(Q(IXYC)-Q(IXYC-INC)) )
#ifdef W3_T1
      QBO    = QB
#endif
      !
      FLA(IXY) = CFL * QB
      !
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( QB, QBO, Q(IXY-INC), Q(   IXY   ),         &
           Q(IXY+INC), Q(IXY+2*INC) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9012) IP, IX, IY, IX2, IY2,               &
             CFL, CFLL(IXY), CFLL(IXY+INC),      &
             QBO*QN, QB*QN, Q(IXY-INC)*QN, Q(   IXY   )*QN, &
             Q(IXY+INC)*QN, Q(IXY+2*INC)*QN
      END IF
#endif
      !
    END DO
    !
    ! 3.  Fluxes for points with boundary above ------------------------- *
    !     ( 1st order without limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9011) NB1-NB0, 'BOUNDARY ABOVE'
#endif
    !
    DO IP=NB0+1, NB1
      IXY    = MAPBOU(IP)
      CFL    = CFLL(IXY)
      IXYC   = IXY - INC * INT( MIN ( 0. , SIGN(1.1,CFL) ) )
      FLA(IXY) = CFL * Q(IXYC)
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( Q(IXY+INC), Q(IXY) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9013) IP, IX, IY, IX2, IY2, CFL,          &
             CFLL(IXY), Q(IXYC)*QN, Q(IXY)*QN, Q(IXY+INC)*QN
      END IF
#endif
    END DO
    !
    ! 4.  Fluxes for points with boundary below ------------------------- *
    !     ( 1st order without limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9011) NB2-NB1, 'BOUNDARY BELOW'
#endif
    !
    DO IP=NB1+1, NB2
      IXY    = MAPBOU(IP)
      CFL    = CFLL(IXY+INC)
      IXYC   = IXY - INC * INT( MIN ( 0. , SIGN(1.1,CFL) ) )
      FLA(IXY) = CFL * Q(IXYC)
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( Q(IXY+INC), Q(IXY) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9014) IP, IX, IY, IX2, IY2, CFL,         &
             CFLL(IXY+INC), Q(IXYC)*QN, Q(IXY)*QN, Q(IXY+INC)*QN
      END IF
#endif
    END DO
    !
    ! 5.  Global closure ----------------------------------------------- *
    !
    IF ( BCLOSE ) THEN
#ifdef W3_T
      WRITE (NDST,9015)
#endif
      DO IY=1, NY
        FLA (IY+IAD00) = FLA (IY+IADN0)
      END DO
    END IF
    !
    ! 6.  Propagation -------------------------------------------------- *
    !
#ifdef W3_T2
    WRITE (NDST,9020)
#endif
    DO IP=1, NACT
      IXY    = MAPACT(IP)
#ifdef W3_T2
      QOLD   = Q(IXY)
#endif
      Q(IXY) = MAX ( 0. , Q(IXY) + FLA(IXY-INC) - FLA(IXY) )
#ifdef W3_T2
      IF ( QOLD + Q(IXY) .GT. 1.E-10 )                          &
           WRITE (NDST,9021) IP, IXY, QOLD, Q(IXY),             &
           FLA(IXY-INC), FLA(IXY)
#endif
    END DO
    !
#ifdef W3_T0
    WRITE (NDST,9001) 'Q'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(Q(IY+(IX-1)*MY)/QMAX),IX=1,NX)
    END DO
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST W3UNO2r : ARRAY DIMENSIONS  :',2I6/           &
         '               USED              :',2I6/           &
         '              BCLOSE, INC        :',L6,I6/         &
         '               NB0, NB1, NB2     :',3I6)
#endif
#ifdef W3_T0
9001 FORMAT ( ' TEST W3UNO2r : DUMP ARRAY ',A,' :')
9002 FORMAT ( 1X,43I3)
9003 FORMAT ( 1X,21I6)
#endif
#ifdef W3_T
9005 FORMAT (' TEST W3UNO2r : GLOBAL CLOSURE (1)')
#endif
    !
#ifdef W3_T1
9010 FORMAT (' TEST W3UNO2r : IP, 2x(IX,IY), CFL (b,i,i+1), ',    &
         ' Q (b,b,i-1,i,i+1,i+2)')
9011 FORMAT (' TEST W3UNO2r :',I6,' POINTS OF TYPE ',A)
9012 FORMAT (10X,I6,4I4,1X,3F6.2,1X,F7.2,F6.2,1X,4F6.2)
9013 FORMAT (10X,I6,4I4,1X,F6.2,F6.2,'  --- ',1X,F7.2,1X,'  --- ',&
         2F6.2,'  --- ')
9014 FORMAT (10X,I6,4I4,1X,F6.2,'  --- ',F6.2,1X,F7.2,1X,'  --- ',&
         2F6.2,'  --- ')
#endif
#ifdef W3_T
9015 FORMAT (' TEST W3UNO2r : GLOBAL CLOSURE (2)')
#endif
    !
#ifdef W3_T2
9020 FORMAT (' TEST W3UNO2r : IP, IXY, 2Q, 2FL')
9021 FORMAT ('            ',2I6,2(1X,2E11.3))
#endif
    !/
  END SUBROUTINE W3UNO2r
  !/
  !/ End of W3UNO2r ---------------------------------------------------- /
  !/
  !/

  !>
  !> @brief Like W3UNO2r with cell transparencies added.
  !>
  !> @details Adapted from W3QCK3 for UNO2 regular grid scheme with subgrid obstruction.
  !>
  !> @param[in] MX Field dimensions, if grid is 'closed' or circular, MX is the closed dimension.
  !> @param[in] MY       Field dimensions
  !> @param[in] NX       Part of field actually used
  !> @param[in] NY       Part of field actually used
  !> @param[in] TRANS
  !> @param[inout] CFLL  Local Courant numbers         (MY,  MX+1).
  !> @param[inout] Q     Propagated quantity           (MY,0:MX+2).
  !> @param[in] BCLOSE   Flag for closed 'X' dimension.
  !> @param[in] INC      Increment in 1-D array corresponding to increment in 2-D space.
  !> @param[in] MAPACT   List of active grid points.
  !> @param[in] NACT     Size of MAPACT.
  !> @param[in] MAPBOU   Map with boundary information (see W3MAP2).
  !> @param[in] NB0      Counter in MAPBOU
  !> @param[in] NB1      Counter in MAPBOU
  !> @param[in] NB2      Counter in MAPBOU
  !> @param[in] NDSE     Error output unit number.
  !> @param[in] NDST     Test output unit number.
  !>
  !> @author Jain-Guo Li  @date 8-Jan-2018
  !>
  SUBROUTINE W3UNO2s (MX, MY, NX, NY, CFLL, TRANS, Q, BCLOSE,     &
       INC, MAPACT, NACT, MAPBOU, NB0, NB1, NB2,    &
       NDSE, NDST )
    !/
    !/
    !/    Adapted from W3QCK3 for UNO2 regular grid scheme with
    !/            subgrid obstruction.
    !/            First created:    19 Mar 2008   Jian-Guo Li
    !/            Last modified:     8 Jan 2018   Jian-Guo Li
    !/
    !  1. Purpose :
    !
    !     Like W3UNO2r with cell transparencies added.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       MX,MY   Int.   I   Field dimensions, if grid is 'closed' or
    !                          circular, MX is the closed dimension.
    !       NX,NY   Int.   I   Part of field actually used.
    !       CFLL    R.A.   I   Local Courant numbers.         (MY,  MX+1)
    !       Q       R.A.  I/O  Propagated quantity.           (MY,0:MX+2)
    !       BCLOSE  Log.   I   Flag for closed 'X' dimension'
    !       INC     Int.   I   Increment in 1-D array corresponding to
    !                          increment in 2-D space.
    !       MAPACT  I.A.   I   List of active grid points.
    !       NACT    Int.   I   Size of MAPACT.
    !       MAPBOU  I.A.   I   Map with boundary information (see W3MAP2).
    !       NBn     Int.   I   Counters in MAPBOU.
    !       NDSE    Int.   I   Error output unit number.
    !       NDST    Int.   I   Test output unit number.
    !     ----------------------------------------------------------------
    !           - CFLL amd Q need only bee filled in the (MY,MX) range,
    !             extension is used internally for closure.
    !           - CFLL and Q are defined as 1-D arrays internally.
    !
    !  4. Subroutines used :
    !
    !       STRACE   Service routine.
    !
    !  5. Called by :
    !
    !       W3XYP2   Propagation in physical space
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !     - This routine can be used independently from WAVEWATCH-III.
    !
    !  8. Structure :
    !
    !     ------------------------------------------------------
    !       1. Initialize aux. array FLA.
    !       2. Fluxes for central points (3rd order + limiter).
    !       3. Fluxes boundary point above (1st order).
    !       4. Fluxes boundary point below (1st order).
    !       5. Closure of 'X' if required
    !       6. Propagate.
    !     ------------------------------------------------------
    !
    !  9. Switches :
    !
    !     !/OMPH  Ading OMP directves for hybrid paralellization.
    !
    !     !/S   Enable subroutine tracing.
    !     !/T   Enable test output.
    !     !/T0  Test output input/output fields.
    !     !/T1  Test output fluxes.
    !     !/T2  Test output integration.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: MX, MY, NX, NY, INC, MAPACT(MY*MX),  &
         NACT, MAPBOU(MY*MX), NB0, NB1, NB2,  &
         NDSE, NDST
    REAL, INTENT(IN)        :: TRANS(MY*MX,-1:1)
    REAL, INTENT(INOUT)     :: CFLL(MY*(MX+1)), Q(1-MY:MY*(MX+2))
    LOGICAL, INTENT(IN)     :: BCLOSE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IXY, IP, IXYC, IXYU, IXYD, IY, IX,   &
         IAD00, IAD02, IADN0, IADN1, IADN2,   &
         JN, JP
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T1
    INTEGER                 :: IX2, IY2
#endif
    REAL                    :: CFL, QB, DQ, DQNZ, QCN, QBN, QBR, CFAC
    REAL                    :: FLA(1-MY:MY*MX)
#ifdef W3_T0
    REAL                    :: QMAX
#endif
#ifdef W3_T1
    REAL                    :: QBO, QN
#endif
#ifdef W3_T2
    REAL                    :: QOLD
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3UNO2s')
#endif
    !
#ifdef W3_T
    WRITE (NDST,9000) MX, MY, NX, NY, BCLOSE, INC, NB0, NB1, NB2
#endif
    !
#ifdef W3_T0
    QMAX   = 0.
    DO IY=1, NY
      DO IX=1, NX
        QMAX   = MAX ( QMAX , Q(IY+(IX-1)*MY) )
      END DO
    END DO
    QMAX   = MAX ( 0.01*QMAX , 1.E-10 )
#endif
    !
#ifdef W3_T0
    WRITE (NDST,9001) 'CFLL'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(100.*CFLL(IY+(IX-1)*MY)),IX=1,NX)
    END DO
    WRITE (NDST,9001) 'Q'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(Q(IY+(IX-1)*MY)/QMAX),IX=1,NX)
    END DO
    WRITE (NDST,9001) 'MAPACT'
    WRITE (NDST,9003) (MAPACT(IXY),IXY=1,NACT)
#endif
    !
    ! 1.  Initialize aux. array FLA and closure ------------------------- *
    !
    FLA = 0.
    !
    IF ( BCLOSE ) THEN
#ifdef W3_T
      WRITE (NDST,9005)
#endif
      IAD00  = -MY
      IAD02  =  MY
      IADN0  = IAD00 + MY*NX
      IADN1  =         MY*NX
      IADN2  = IAD02 + MY*NX
      !
#ifdef W3_OMPH
      !$OMP PARALLEL DO PRIVATE (IY)
#endif
      !
      DO IY=1, NY
        Q   (IY+IAD00) = Q   (IY+IADN0)
        Q   (IY+IADN1) = Q   (   IY   )
        Q   (IY+IADN2) = Q   (IY+IAD02)
        CFLL(IY+IADN1) = CFLL(   IY   )
      END DO
      !
#ifdef W3_OMPH
      !$OMP END PARALLEL DO
#endif
      !
    END IF
    !
    ! 2.  Fluxes for central points ------------------------------------- *
    !     ( 3rd order + limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9010)
    WRITE (NDST,9011) NB0, 'CENTRAL'
#endif
    !
#ifdef W3_OMPH
    !$OMP PARALLEL DO PRIVATE (IP, IXY, CFL,                       &
#ifdef W3_T1
    !$OMP QBO, IX, IY, IY2, IX2, QN                   &
#endif
    !$OMP IXYC, IXYD, QB)
#endif
    !
    DO IP=1, NB0
      !
      IXY  = MAPBOU(IP)
      CFL  = 0.5 * ( CFLL(IXY) + CFLL(IXY+INC) )
      IXYC = IXY  - INC * INT( MIN ( 0. , SIGN(1.1,CFL) ) )
      IXYD = IXYC + INC * INT( SIGN (1.1,CFL) )
      QB   = Q(IXYC)+SIGN(0.5, Q(IXYD)-Q(IXYC))*(1.0-ABS(CFL)) &
           *MIN(ABS(Q(IXYC+INC)-Q(IXYC)),             &
           ABS(Q(IXYC)-Q(IXYC-INC)) )
      !
#ifdef W3_T1
      QBO    = QB
#endif
      !
      FLA(IXY) = CFL * QB
      !
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( QB, QBO, Q(IXY-INC), Q(   IXY   ),         &
           Q(IXY+INC), Q(IXY+2*INC) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9012) IP, IX, IY, IX2, IY2,               &
             CFL, CFLL(IXY), CFLL(IXY+INC),      &
             QBO*QN, QB*QN, Q(IXY-INC)*QN, Q(   IXY   )*QN,  &
             Q(IXY+INC)*QN, Q(IXY+2*INC)*QN
      END IF
#endif
      !
    END DO
    !
#ifdef W3_OMPH
    !$OMP END PARALLEL DO
#endif
    !
    ! 3.  Fluxes for points with boundary above ------------------------- *
    !     ( 1st order without limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9011) NB1-NB0, 'BOUNDARY ABOVE'
#endif
    !
    DO IP=NB0+1, NB1
      IXY    = MAPBOU(IP)
      CFL    = CFLL(IXY)
      IXYC   = IXY - INC * INT( MIN ( 0. , SIGN(1.1,CFL) ) )
      FLA(IXY) = CFL * Q(IXYC)
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( Q(IXY+INC), Q(IXY) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9013) IP, IX, IY, IX2, IY2, CFL,          &
             CFLL(IXY), Q(IXYC)*QN, Q(IXY)*QN, Q(IXY+INC)*QN
      END IF
#endif
    END DO
    !
    ! 4.  Fluxes for points with boundary below ------------------------- *
    !     ( 1st order without limiter )
    !
#ifdef W3_T1
    WRITE (NDST,9011) NB2-NB1, 'BOUNDARY BELOW'
#endif
    !
    DO IP=NB1+1, NB2
      IXY    = MAPBOU(IP)
      CFL    = CFLL(IXY+INC)
      IXYC   = IXY - INC * INT( MIN ( 0. , SIGN(1.1,CFL) ) )
      FLA(IXY) = CFL * Q(IXYC)
#ifdef W3_T1
      IY     = MOD ( IXY , MY )
      IX     = 1 + IXY/MY
      IY2    = MOD ( IXY+INC , MY )
      IX2    = 1 + (IXY+INC)/MY
      QN     = MAX ( Q(IXY+INC), Q(IXY) )
      IF ( QN .GT. 1.E-10 ) THEN
        QN     = 1. /QN
        WRITE (NDST,9014) IP, IX, IY, IX2, IY2, CFL, CFLL(IXY+INC), &
             Q(IXYC)*QN, Q(IXY)*QN, Q(IXY+INC)*QN
      END IF
#endif
    END DO
    !
    ! 5.  Global closure ----------------------------------------------- *
    !
    IF ( BCLOSE ) THEN
#ifdef W3_T
      WRITE (NDST,9015)
#endif
      DO IY=1, NY
        FLA (IY+IAD00) = FLA (IY+IADN0)
      END DO
    END IF
    !
    ! 6.  Propagation -------------------------------------------------- *
    !
#ifdef W3_T2
    WRITE (NDST,9020)
#endif
    !
#ifdef W3_OMPH
    !$OMP PARALLEL DO  &
#ifdef W3_T2
    !$OMP       PRIVATE(QOLD), &
#endif
    !$OMP       PRIVATE (IP, IXY, JN, JP)
#endif
    !
    DO IP=1, NACT
      !
      IXY    = MAPACT(IP)
      IF ( FLA(IXY-INC) .GT. 0. ) THEN
        JN    = -1
      ELSE
        JN    =  0
      END IF
      IF ( FLA(IXY    ) .LT. 0. ) THEN
        JP    =  1
      ELSE
        JP    =  0
      END IF
      !
#ifdef W3_T2
      QOLD   = Q(IXY)
#endif
      Q(IXY) = MAX ( 0. , Q(IXY) + TRANS(IXY,JN) * FLA(IXY-INC)     &
           - TRANS(IXY,JP) * FLA(IXY) )

#ifdef W3_T2
      IF ( QOLD + Q(IXY) .GT. 1.E-10 )                          &
           WRITE (NDST,9021) IP, IXY, QOLD, Q(IXY),             &
           FLA(IXY-INC), FLA(IXY)
#endif
    END DO
    !
#ifdef W3_OMPH
    !$OMP END PARALLEL DO
#endif
    !
#ifdef W3_T0
    WRITE (NDST,9001) 'Q'
    DO IY=NY,1,-1
      WRITE (NDST,9002) (NINT(Q(IY+(IX-1)*MY)/QMAX),IX=1,NX)
    END DO
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT ( ' TEST W3UNO2s : ARRAY DIMENSIONS  :',2I6/           &
         '               USED              :',2I6/           &
         '               BCLOSE, INC       :',L6,I6/         &
         '               NB0, NB1, NB2     :',3I6)
#endif
#ifdef W3_T0
9001 FORMAT ( ' TEST W3UNO2s : DUMP ARRAY ',A,' :')
9002 FORMAT ( 1X,43I3)
9003 FORMAT ( 1X,21I6)
#endif
#ifdef W3_T
9005 FORMAT (' TEST W3UNO2s : GLOBAL CLOSURE (1)')
#endif
    !
#ifdef W3_T1
9010 FORMAT (' TEST W3UNO2s : IP, 2x(IX,IY), CFL (b,i,i+1), ',    &
         ' Q (b,b,i-1,i,i+1,i+2)')
9011 FORMAT (' TEST W3UNO2s :',I6,' POINTS OF TYPE ',A)
9012 FORMAT (10X,I6,4I4,1X,3F6.2,1X,F7.2,F6.2,1X,4F6.2)
9013 FORMAT (10X,I6,4I4,1X,F6.2,F6.2,'  --- ',1X,F7.2,1X,'  --- ',&
         2F6.2,'  --- ')
9014 FORMAT (10X,I6,4I4,1X,F6.2,'  --- ',F6.2,1X,F7.2,1X,'  --- ',&
         2F6.2,'  --- ')
#endif
#ifdef W3_T
9015 FORMAT (' TEST W3UNO2s : GLOBAL CLOSURE (2)')
#endif
    !
#ifdef W3_T2
9020 FORMAT (' TEST W3UNO2s : IP, IXY, 2Q, 2FL')
9021 FORMAT ('            ',2I6,2(1X,2E11.3))
#endif
    !/
    !/ End of W3UNO2s ---------------------------------------------------- /
    !/
  END SUBROUTINE W3UNO2s
  !/
  !/ End of module W3UNO2MD -------------------------------------------- /
  !/
END MODULE W3UNO2MD
!/
