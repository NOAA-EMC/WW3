!> @file
!> @brief Contains MODULE W3WAVSET for implicit solution of wave
!>        setup problem.
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date 1-Jun-2018
!>
!     ----------------------------------------------------------------
!>
!> @brief Implicit solution of wave setup problem following
!>        Dingemans for structured and unstructured grids.
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date 1-Jun-2018
!>
      MODULE W3WAVSET
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 95 |
!/                  | Last update :         1-June-2018 |
!/                  +-----------------------------------+
!/
!/    01-June-2016 : Origination                        ( version 6.04 )
!/
!  1. Purpose : Implicit solution of wave setup problem following
!               Dingemans for structured and unstructured grids
!
!  2. Method :  To be described
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
      use yowDatapool, only: rkind
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/

      LOGICAL :: DO_WAVE_SETUP = .TRUE.
      CONTAINS
!/ ------------------------------------------------------------------- /
!>
!> @brief Differentiate xy, using linear shape function.
!>
!> @param[in]  VAR
!> @param[out] DVDX
!> @param[out] DVDY
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date 1-May-2018
!>
      SUBROUTINE DIFFERENTIATE_XYDIR_NATIVE(VAR, DVDX, DVDY)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : differentiate xy
!  2. Method : linear shape function
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      use yowExchangeModule, only : PDLIB_exchange1Dreal
      use yowNodepool,    only : PDLIB_IEN, PDLIB_TRIA, npa
      use yowElementpool, only : INE, NE
      USE W3GDATMD, ONLY : MAPSTA
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
!
      REAL(rkind), INTENT(IN)  :: VAR(npa)
      REAL(rkind), INTENT(OUT) :: DVDX(npa), DVDY(npa)
      INTEGER           :: NI(3)
      INTEGER           :: IE, I1, I2, I3, IP
      REAL(rkind)           :: DEDY(3),DEDX(3)
      REAL(rkind)           :: DVDXIE, DVDYIE
      REAL(rkind)           :: WEI(npa), eW
      INTEGER           :: IX
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      WEI  = 0.0
      DVDX = 0.0
      DVDY = 0.0

      DO IE = 1, NE
        NI = INE(:,IE)
        I1 = INE(1,IE)
        I2 = INE(2,IE)
        I3 = INE(3,IE)
        WEI(NI) = WEI(NI) + 2.*PDLIB_TRIA(IE)
        DEDX(1) = PDLIB_IEN(1,IE)
        DEDX(2) = PDLIB_IEN(3,IE)
        DEDX(3) = PDLIB_IEN(5,IE)
        DEDY(1) = PDLIB_IEN(2,IE)
        DEDY(2) = PDLIB_IEN(4,IE)
        DEDY(3) = PDLIB_IEN(6,IE)
        DVDXIE  = DOT_PRODUCT( VAR(NI),DEDX)
        DVDYIE  = DOT_PRODUCT( VAR(NI),DEDY)
        DVDX(NI) = DVDX(NI) + DVDXIE
        DVDY(NI) = DVDY(NI) + DVDYIE
      END DO
      DO IX=1,npa
        eW=WEI(IX)
        DVDX(IX)=DVDX(IX) / eW
        DVDY(IX)=DVDY(IX) / eW
      END DO
      CALL PDLIB_exchange1Dreal(DVDX)
      CALL PDLIB_exchange1Dreal(DVDY)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Differentiate xy based on mapsta, using linear shape function.
!>
!> @param[in]  VAR
!> @param[out] DVDX
!> @param[out] DVDY
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date 1-May-2018
!>
      SUBROUTINE DIFFERENTIATE_XYDIR_MAPSTA(VAR, DVDX, DVDY)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : differentiate xy based on mapsta
!  2. Method : linear shape function
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      use yowExchangeModule, only : PDLIB_exchange1Dreal
      use yowNodepool,    only : PDLIB_IEN, PDLIB_TRIA, npa, iplg
      use yowElementpool, only : INE, NE
      USE W3GDATMD, ONLY : MAPSTA
      USE W3PARALL, only: INIT_GET_ISEA
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      REAL(rkind), INTENT(IN)  :: VAR(npa)
      REAL(rkind), INTENT(OUT) :: DVDX(npa), DVDY(npa)
      INTEGER           :: NI(3)
      INTEGER           :: IE, I1, I2, I3, IP, IX
      REAL(rkind)           :: DEDY(3),DEDX(3)
      REAL(rkind)           :: DVDXIE, DVDYIE
      REAL(rkind)           :: WEI(npa), eW
      INTEGER           :: IX1, IX2, IX3, ISEA
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      WEI  = 0.0
      DVDX = 0.0
      DVDY = 0.0

      DO IE = 1, NE
        NI = INE(:,IE)
        I1 = INE(1,IE)
        I2 = INE(2,IE)
        I3 = INE(3,IE)
        IX1=iplg(I1)
        IX2=iplg(I2)
        IX3=iplg(I3)
        IF ((MAPSTA(1,IX1) .gt. 0).and.(MAPSTA(1,IX2) .gt. 0).and.(MAPSTA(1,IX3) .gt. 0)) THEN
          WEI(NI) = WEI(NI) + 2.*PDLIB_TRIA(IE)
          DEDX(1) = PDLIB_IEN(1,IE)
          DEDX(2) = PDLIB_IEN(3,IE)
          DEDX(3) = PDLIB_IEN(5,IE)
          DEDY(1) = PDLIB_IEN(2,IE)
          DEDY(2) = PDLIB_IEN(4,IE)
          DEDY(3) = PDLIB_IEN(6,IE)
          DVDXIE  = DOT_PRODUCT( VAR(NI),DEDX)
          DVDYIE  = DOT_PRODUCT( VAR(NI),DEDY)
          DVDX(NI) = DVDX(NI) + DVDXIE
          DVDY(NI) = DVDY(NI) + DVDYIE
        END IF
      END DO
      DO IP=1,npa
        IX=iplg(IP)
        eW=WEI(IP)
        IF (eW .gt. 0 .and. MAPSTA(1,IX) .gt. 0) THEN
          DVDX(IP)=DVDX(IP) / eW
          DVDY(IP)=DVDY(IP) / eW
        ELSE
          DVDX(IP)=0.
          DVDY(IP)=0.
        ENDIF
      END DO
      DO IP=1,npa
        IX=iplg(IP)
        IF (MAPSTA(1,IX) .lt. 0) THEN
          DVDX(IP)=0.
          DVDY(IP)=0.
        END IF
      END DO
      CALL PDLIB_exchange1Dreal(DVDX)
      CALL PDLIB_exchange1Dreal(DVDY)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Driver routine for xydir.
!>
!> @param[in]  VAR
!> @param[out] DVDX
!> @param[out] DVDY
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE DIFFERENTIATE_XYDIR(VAR, DVDX, DVDY)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Driver routine for xydir
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      use yowNodepool, only: npa
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
      REAL(rkind), INTENT(IN)  :: VAR(npa)
      REAL(rkind), INTENT(OUT) :: DVDX(npa), DVDY(npa)
!/
!/ ------------------------------------------------------------------- /
!/
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
!

      CALL DIFFERENTIATE_XYDIR_MAPSTA(VAR, DVDX, DVDY)
!      CALL DIFFERENTIATE_XYDIR_NATIVE(VAR, DVDX, DVDY)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Setup boundary pointer.
!>
!> @param[out] F_X
!> @param[out] F_Y
!> @param[out] DWNX
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_COMPUTE_LH_STRESS(F_X, F_Y, DWNX)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Setup boundary pointer
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE CONSTANTS, ONLY: GRAV, DWAT
      use yowNodepool, only: npa, iplg
      USE W3GDATMD, only : MAPFS
      USE W3ADATMD, ONLY: SXX, SXY, SYY, WN, CG
      USE W3PARALL, only: INIT_GET_ISEA
      USE W3ODATMD, only : IAPROC
      USE W3GDATMD, ONLY : NSEAL, MAPSTA
      USE W3ADATMD, ONLY: DW
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(out) :: F_X(npa), F_Y(npa), DWNX(npa)
      REAL(rkind) :: h
      REAL(rkind) :: SXX_X, SXX_Y
      REAL(rkind) :: SXY_X, SXY_Y
      REAL(rkind) :: SYY_X, SYY_Y
      INTEGER I, IP, IX
      INTEGER JSEA, ISEA
      real(rkind) :: U_X1(npa), U_Y1(npa)
      real(rkind) :: U_X2(npa), U_Y2(npa)
      real(rkind) :: SXX_p(npa), SXY_p(npa), SYY_p(npa)
      real(rkind) :: eSXX, eSXY, eSYY
      integer :: SXXmethod = 1
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      SXX_p=0
      SXY_p=0
      SYY_p=0
      DWNX=0
      DO JSEA=1,NSEAL
        IP = JSEA ! We remove the Z_status because now NX = NSEA
        IX=iplg(IP)
        ISEA=MAPFS(1,IX)
        IF (SXXmethod .eq. 1) THEN
          eSXX=SXX(JSEA)/(DWAT*GRAV)
          eSXY=SXY(JSEA)/(DWAT*GRAV)
          eSYY=SYY(JSEA)/(DWAT*GRAV)
        END IF
        SXX_p(IP)=DBLE(eSXX)
        SXY_p(IP)=DBLE(eSXY)
        SYY_p(IP)=DBLE(eSYY)
        DWNX(IP)=DW(ISEA)
      END DO
      !
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'min/max(DEP)=', minval(DWNX), maxval(DWNX)
      WRITE(740+IAPROC,*) 'sum(abs(SXX))=', sum(abs(SXX_p))
      WRITE(740+IAPROC,*) 'sum(abs(SXY))=', sum(abs(SXY_p))
      WRITE(740+IAPROC,*) 'sum(abs(SYY))=', sum(abs(SYY_p))
      FLUSH(740+IAPROC)
#endif

      CALL DIFFERENTIATE_XYDIR(SXX_p, U_X1, U_Y1)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'sum(absU_XY1)=', sum(abs(U_X1)), sum(abs(U_Y1))
      FLUSH(740+IAPROC)
#endif
      CALL DIFFERENTIATE_XYDIR(SXY_p, U_X2, U_Y2)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'sum(absU_XY2)=', sum(abs(U_X2)), sum(abs(U_Y2))
      FLUSH(740+IAPROC)
#endif
      F_X = -U_X1 - U_Y2
      !
      CALL DIFFERENTIATE_XYDIR(SYY_p, U_X1, U_Y1)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'sum(absU_XY1)=', sum(abs(U_X1)), sum(abs(U_Y1))
      FLUSH(740+IAPROC)
#endif
      F_Y = -U_Y1 - U_X2
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'sum(F_X)=', sum(F_X)
      WRITE(740+IAPROC,*) 'sum(F_Y)=', sum(F_Y)
      FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Differentiate other way around.
!>
!> @param[in] IE
!> @param[in] I1
!> @param[inout] UGRAD
!> @param[inout] VGRAD
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_COMPUTE_DIFF(IE, I1, UGRAD, VGRAD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : differentiate other way around ...
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      use yowElementpool, only: INE
      use yowNodepool,    only: x, y, PDLIB_TRIA
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER, intent(in) :: IE, I1
      REAL(rkind), intent(inout) :: UGRAD, VGRAD
      REAL(rkind) :: h
      integer I2, I3, IP1, IP2, IP3
      INTEGER :: POS_TRICK(3,2)
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      POS_TRICK(1,1) = 2
      POS_TRICK(1,2) = 3
      POS_TRICK(2,1) = 3
      POS_TRICK(2,2) = 1
      POS_TRICK(3,1) = 1
      POS_TRICK(3,2) = 2
      I2=POS_TRICK(I1, 1)
      I3=POS_TRICK(I1, 2)
      IP1=INE(I1, IE)
      IP2=INE(I2, IE)
      IP3=INE(I3, IE)
      h=2.0*PDLIB_TRIA(IE)
      UGRAD=-(y(IP3) - y(IP2))/h
      VGRAD= (x(IP3) - x(IP2))/h
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Setup system matrix for solutions of wave setup eq.
!>
!> @param[in]  FX
!> @param[in]  FY
!> @param[in]  DWNX
!> @param[out] ASPAR
!> @param[out] B
!> @param[in]  ACTIVE
!> @param[out] ACTIVESEC
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_WAVE_SETUP_COMPUTE_SYSTEM(ASPAR, B, FX, FY, DWNX, ACTIVE, ACTIVESEC)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Setup system matrix for solutions of wave setup eq.
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      use yowElementpool, only: INE, NE
      use yowNodepool, only: PDLIB_NNZ, PDLIB_JA_IE, PDLIB_TRIA, npa, np
      use yowNodepool, only: PDLIB_I_DIAG
      USE yowNodepool, only: iplg
      USE W3ODATMD, only : IAPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(in)  :: FX(npa), FY(npa), DWNX(npa)
      real(rkind), intent(out) :: ASPAR(PDLIB_NNZ)
      real(rkind), intent(out) :: B(npa)
      integer, intent(in)  :: ACTIVE(npa)
      integer, intent(out)  :: ACTIVESEC(npa)
      INTEGER :: POS_TRICK(3,2), POS_SHIFT(3,3)
      integer I1, I2, I3, IP1, IP2, IP3
      integer IDX, IDX1, IDX2, IDX3
      INTEGER IE, IP, I, J, K, IPp, JPp
      real(rkind) :: eDep, eFX, eFY, eScal, eFact, eArea
      real(rkind) :: UGRAD, VGRAD, UGRAD1, VGRAD1
      real(rkind) :: eOff
      logical DoPrintOut
      INTEGER sumActive
      INTEGER LIDX(2), KIDX(2), jdx
      INTEGER IPglob1, IPglob2, IPglob3
#ifdef W3_DEBUGSTP
      REAL(rkind) :: ListDiag(npa)
#endif
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      POS_TRICK(1,1) = 2
      POS_TRICK(1,2) = 3
      POS_TRICK(2,1) = 3
      POS_TRICK(2,2) = 1
      POS_TRICK(3,1) = 1
      POS_TRICK(3,2) = 2
      ASPAR=0
      B=0
      DO I=1,3
        DO J=1,3
          K= I-J+1
          IF (K .le. 0) THEN
            K=K+3
          END IF
          IF (K .ge. 4) THEN
            K=K-3
          END IF
          POS_SHIFT(I,J)=K
        END DO
      END DO
      DO I=1,3
        jdx=0
        DO IDX=1,3
          K=POS_SHIFT(I,IDX)
          IF (K .ne. I) THEN
            jdx=jdx+1
            LIDX(jdx)=IDX
            KIDX(jdx)=K
          END IF
        END DO
        POS_SHIFT(I,LIDX(1))=KIDX(2)
        POS_SHIFT(I,LIDX(2))=KIDX(1)
      END DO
      ACTIVESEC=0
      DO IE=1,ne
        IP1=INE(1,IE)
        IP2=INE(2,IE)
        IP3=INE(3,IE)
        eFX =(FX(IP1) + FX(IP2) + FX(IP3))/3
        eFY =(FY(IP1) + FY(IP2) + FY(IP3))/3
        sumActive=ACTIVE(IP1) + ACTIVE(IP2) + ACTIVE(IP3)
        IF (sumActive .eq. 3) THEN
          ACTIVESEC(IP1)=1
          ACTIVESEC(IP2)=1
          ACTIVESEC(IP3)=1
          eDep=(DWNX(IP1) + DWNX(IP2) + DWNX(IP3))/3.0
          eArea=PDLIB_TRIA(IE)
          eFact=eDep*eArea
          DO I1=1,3
            I2=POS_TRICK(I1,1)
            I3=POS_TRICK(I1,2)
            IP1=INE(I1,IE)
            IP2=INE(I2,IE)
            IP3=INE(I3,IE)
            IDX1=PDLIB_JA_IE(I1,1,IE)
            IDX2=PDLIB_JA_IE(I1,2,IE)
            IDX3=PDLIB_JA_IE(I1,3,IE)
            CALL TRIG_COMPUTE_DIFF(IE, I1, UGRAD1, VGRAD1)
            eScal=UGRAD1*eFX + VGRAD1*eFY
            B(IP1) = B(IP1) + eScal*eArea
            !
            DO IDX=1,3
              K=POS_SHIFT(I1, IDX)
              CALL TRIG_COMPUTE_DIFF(IE, K, UGRAD, VGRAD)
              eScal=UGRAD*UGRAD1 + VGRAD*VGRAD1
              J=PDLIB_JA_IE(I1,IDX,IE)
              ASPAR(J)=ASPAR(J) + eFact*eScal
            END DO
          END DO
        END IF
      END DO
      DoPrintOut=.TRUE.
      IF (DoPrintOut .eqv. .TRUE.) THEN
        DO IP=1,NP
          eOff=0
        END DO
      END IF
#ifdef W3_DEBUGSTP
      DO IP=1,npa
         J=PDLIB_I_DIAG(IP)
         ListDiag(IP)=ASPAR(J)
      END DO
      WRITE(740+IAPROC,*) 'Diag, min=', minval(ListDiag), ' max=', maxval(ListDiag)
      WRITE(740+IAPROC,*) 'Diag, quot=', maxval(ListDiag)/minval(ListDiag)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Preconditioner.
!>
!> @param[in]  ASPAR
!> @param[in]  TheIn
!> @param[out] TheOut
!> @param[in]  ACTIVE
!> @param[in]  ACTIVESEC
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_WAVE_SETUP_APPLY_PRECOND(ASPAR, TheIn, TheOut, ACTIVE, ACTIVESEC)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : preconditioner
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      use yowExchangeModule, only : PDLIB_exchange1Dreal
      use yowNodepool, only: PDLIB_NNZ, PDLIB_IA, PDLIB_JA, PDLIB_I_DIAG
      use yowNodepool, only: npa
      USE W3ODATMD, only : IAPROC
      USE W3ODATMD, only : IAPROC
      USE yowNodepool, only: iplg
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      REAL(rkind), intent(in) :: ASPAR(PDLIB_NNZ)
      REAL(rkind), intent(in) :: TheIn(npa)
      REAL(rkind), intent(out) :: TheOut(npa)
      INTEGER, intent(IN) :: ACTIVE(npa), ACTIVESEC(npa)
      integer IP, J1, J, JP, J2
      REAL(rkind) :: eCoeff
      INTEGER :: ThePrecond = 2
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      IF (ThePrecond .eq. 0) THEN
        TheOut=TheIn
      END IF
      IF (ThePrecond .eq. 1) THEN
        TheOut=0
        DO IP=1,npa
          IF (ACTIVE(IP) .eq. 1) THEN
            J1=PDLIB_I_DIAG(IP)
            DO J=PDLIB_IA(IP),PDLIB_IA(IP+1)-1
              JP=PDLIB_JA(J)
              IF (ACTIVESEC(JP) .eq. 1) THEN
                IF (J .eq. J1) THEN
                  eCoeff=1.0/ASPAR(J)
                ELSE
                  J2=PDLIB_I_DIAG(JP)
                  eCoeff=-ASPAR(J) /(ASPAR(J1)*ASPAR(J2))
                END IF
                TheOut(IP)=TheOut(IP) + eCoeff*TheIn(JP)
              END IF
            END DO
          END IF
        END DO
      END IF
      IF (ThePrecond .eq. 2) THEN
        DO IP=1,npa
          IF (ACTIVESEC(IP) .eq. 1) THEN
            J=PDLIB_I_DIAG(IP)
            TheOut(IP)=TheIn(IP)/ASPAR(J)
          ELSE
            TheOut(IP)=TheIn(IP)
          END IF
        END DO
      END IF
      CALL PDLIB_exchange1Dreal(TheOut)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief
!>
!> @param[in]  ASPAR
!> @param[in]  TheIn
!> @param[out] TheOut
!> @param[in]  ACTIVE
!> @param[in]  ACTIVESEC
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_WAVE_SETUP_APPLY_FCT(ASPAR, TheIn, TheOut, ACTIVE, ACTIVESEC)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : compute off diagonal contr.
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      use yowExchangeModule, only : PDLIB_exchange1Dreal
      USE yowNodepool, only: PDLIB_IA, PDLIB_JA, PDLIB_NNZ
      use yowNodepool, only: np, npa
      USE W3GDATMD, ONLY: NSEAL
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      REAL(rkind), intent(in) :: ASPAR(PDLIB_NNZ)
      REAL(rkind), intent(in) :: TheIn(npa)
      REAL(rkind), intent(out) :: TheOut(npa)
      INTEGER, intent(in) :: ACTIVE(npa), ACTIVESEC(npa)
      integer IP, J, JP
      REAL(rkind) :: eCoeff
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      TheOut=0
      DO IP=1,npa
        IF (ACTIVESEC(IP) .eq. 1) THEN
          DO J=PDLIB_IA(IP),PDLIB_IA(IP+1)-1
            JP=PDLIB_JA(J)
            eCoeff=ASPAR(J)
            TheOut(IP)=TheOut(IP) + eCoeff*TheIn(JP)
          END DO
        END IF
      END DO
      CALL PDLIB_exchange1Dreal(TheOut)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Scalar product plus exchange.
!>
!> @param[in]     V1
!> @param[in]     V2
!> @param[inout]  eScal
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_WAVE_SETUP_SCALAR_PROD(V1, V2, eScal)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : scalar prod. + exchange
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: NX
      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      use yowDatapool, only: rtype, istatus
      use yowNodepool, only: np, npa
      USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
      USE W3GDATMD, ONLY: NSEAL
      USE MPI, only : MPI_SUM
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(in) :: V1(npa), V2(npa)
      real(rkind), intent(inout) :: eScal
      integer IP
      real(rkind) :: lScal_loc(1), lScal_gl(1)
      integer ierr
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      lScal_loc = 0
      DO IP=1,np
        lScal_loc(1) = lScal_loc(1) + V1(IP)*V2(IP)
      END DO
      CALL MPI_ALLREDUCE(lScal_loc,lScal_gl,1,rtype,MPI_SUM,MPI_COMM_WCMP,ierr)
      eScal = lScal_gl(1)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Poisson equation solver.
!>
!> @param[in]  ASPAR
!> @param[in]  B
!> @param[out] TheOut
!> @param[in]  ACTIVE
!> @param[in]  ACTIVESEC
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_WAVE_SETUP_SOLVE_POISSON_NEUMANN_DIR(ASPAR, B, TheOut, ACTIVE, ACTIVESEC)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : poisson eq. solver
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_NNZ
      USE W3GDATMD, ONLY: NSEAL, SOLVERTHR_STP
      USE W3ODATMD, only : IAPROC
      use yowNodepool, only: np, npa
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(in) :: ASPAR(PDLIB_NNZ)
      real(rkind), intent(in) :: B(npa)
      real(rkind), intent(out) :: TheOut(npa)
      integer, intent(in) :: ACTIVE(npa), ACTIVESEC(npa)
      real(rkind) :: V_X(npa), V_R(npa), V_Z(npa), V_P(npa), V_Y(npa)
      real(rkind) :: uO, uN, alphaV, h1, h2
      real(rkind) :: eNorm, beta
      real(rkind) :: SOLVERTHR
      integer IP, nbIter
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      SOLVERTHR = SOLVERTHR_STP

#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'Begin TRIG_WAVE_SETUP_SOLVE ....'
      FLUSH(740+IAPROC)
#endif
      nbIter=0
      V_X=0
      V_R=B
      CALL TRIG_WAVE_SETUP_APPLY_PRECOND(ASPAR, V_R, V_Z, ACTIVE, ACTIVESEC)
      V_P=V_Z
      CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_Z, V_R, uO)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'uO=', uO
      FLUSH(740+IAPROC)
#endif
      CALL TRIG_WAVE_SETUP_SCALAR_PROD(B, B, eNorm)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'eNorm(B)=', eNorm
      WRITE(740+IAPROC,*) 'SOLVERTHR=', SOLVERTHR
      WRITE(740+IAPROC,*) 'SOLVERTHR=', SOLVERTHR, ' eNorm(B)=', eNorm
      FLUSH(740+IAPROC)
#endif
      IF (eNorm .le. SOLVERTHR) THEN
#ifdef W3_DEBUGSTP
        WRITE(740+IAPROC,*) 'Leaving here, zero solution'
        FLUSH(740+IAPROC)
#endif
        TheOut=V_X
        RETURN
      END IF
      DO
        nbIter=nbIter + 1
        CALL TRIG_WAVE_SETUP_APPLY_FCT(ASPAR, V_P, V_Y, ACTIVE, ACTIVESEC)
        CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_P, V_Y, h2)
        alphaV=uO/h2
        !
        DO IP=1,npa
          V_X(IP) = V_X(IP) + alphaV * V_P(IP)
          V_R(IP) = V_R(IP) - alphaV * V_Y(IP)
        END DO
        !
        CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_R, V_R, eNorm)
#ifdef W3_DEBUGSTP
        WRITE(740+IAPROC,*) 'nbIter=', nbIter, ' eNorm(res)=', eNorm
        FLUSH(740+IAPROC)
#endif
        IF (eNorm .le. SOLVERTHR) THEN
          EXIT
        END IF
        !
        CALL TRIG_WAVE_SETUP_APPLY_PRECOND(ASPAR, V_R, V_Z, ACTIVE, ACTIVESEC)
        CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_Z, V_R, uN)
        !
        beta=uN/uO
        uO=uN
#ifdef W3_DEBUGSTP
        WRITE(740+IAPROC,*) '  beta=', beta, ' uN=', uN, ' alphaV=', alphaV, ' h2=', h2
        FLUSH(740+IAPROC)
#endif
        !
        DO IP=1,npa
          V_P(IP)=V_Z(IP) + beta * V_P(IP)
        END DO
      END DO
      TheOut=V_X
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'TRIG_WAVE_SETUP_SOLVE_POISSON_NEUMANN_DIR, max/min=', maxval(TheOut), minval(TheOut)
      FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Set mean value.
!>
!> @param[inout] TheVar
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_SET_MEANVALUE_TO_ZERO(TheVar)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : set. mean value
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_SI
      USE W3GDATMD, ONLY: NX, SI
      USE W3GDATMD, ONLY: NSEAL
      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
      use yowDatapool, only: rtype, istatus
      use yowNodepool, only: np, npa
      USE MPI, only : MPI_SUM
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(inout) :: TheVar(npa)
      real(rkind) :: SUM_SI_Var, SUM_SI, TheMean
      INTEGER IP, ierr
      real(rkind) :: eVect_loc(2), eVect_gl(2)
      integer iProc
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      SUM_SI_Var=0
      SUM_SI=0
      DO IP=1,np
        SUM_SI_Var = SUM_SI_Var + PDLIB_SI(IP)*TheVar(IP)
        SUM_SI     = SUM_SI     + PDLIB_SI(IP)
      END DO
      eVect_loc(1)=SUM_SI_Var
      eVect_loc(2)=SUM_SI
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'SUM_SI_Var=', SUM_SI_Var, 'SUM_SI=', SUM_SI
      FLUSH(740+IAPROC)
#endif
      CALL MPI_ALLREDUCE(eVect_loc,eVect_gl,2,rtype,MPI_SUM,MPI_COMM_WCMP,ierr)
      SUM_SI_Var=eVect_gl(1)
      SUM_SI    =eVect_gl(2)
      TheMean=SUM_SI_Var/SUM_SI
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'TheMean=', TheMean
      FLUSH(740+IAPROC)
#endif
      DO IP=1,npa
        TheVar(IP)=TheVar(IP) - TheMean
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Compute active node for setup comp.
!>
!> @param[in] DWNX
!> @param[out] ACTIVE
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date 1-May-2018
!>
      SUBROUTINE COMPUTE_ACTIVE_NODE(DWNX, ACTIVE)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Compute active node for setup comp.
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY : CRIT_DEP_STP
      USE yowNodepool, only: PDLIB_NNZ, PDLIB_IA, PDLIB_JA, iplg, npa, np
      USE W3ODATMD, only : IAPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      REAL(rkind), INTENT(in) :: DWNX(npa)
      INTEGER, INTENT(out) :: ACTIVE(npa)
      INTEGER IP, eAct
#ifdef W3_DEBUGSTP
      INTEGER nbActive
#endif
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
#ifdef W3_DEBUGSTP
      nbActive=0
#endif
      DO IP=1,NPA
        IF (DWNX(IP) .ge. CRIT_DEP_STP) THEN
          eAct=1
        ELSE
          eAct=0
        END IF
#ifdef W3_DEBUGSTP
        nbActive=nbActive + eAct
#endif
        ACTIVE(IP)=eAct
      END DO
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'min/max(DWNX)=', minval(DWNX), maxval(DWNX)
      WRITE(740+IAPROC,*) 'CRIT_DEP_STP=', CRIT_DEP_STP
      WRITE(740+IAPROC,*) 'nbActive=', nbActive, ' npa=', npa
      FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Setup computation.
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE TRIG_WAVE_SETUP_COMPUTATION
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Setup computation
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_NNZ, PDLIB_IA, PDLIB_JA, iplg, npa, np
      USE W3GDATMD, only : MAPFS
      USE W3PARALL, only : SYNCHRONIZE_GLOBAL_ARRAY
      USE W3ADATMD, ONLY: DW
      USE W3GDATMD, ONLY: NSEAL, NSEA, NX
      USE W3WDATMD, ONLY: ZETA_SETUP
      USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
      USE W3PARALL, only: INIT_GET_ISEA
      use yowExchangeModule, only : PDLIB_exchange1Dreal
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
!
!      CALL W3SETG
      REAL(rkind) :: ZETA_WORK(npa)
      REAL(rkind) :: ZETA_WORK_ALL(NX)
      REAL(rkind) :: F_X(npa), F_Y(npa), DWNX(npa)
      REAL(rkind) :: ASPAR(PDLIB_NNZ), B(npa)
      INTEGER I, ISEA, JSEA, IX, IP, IP_glob
      INTEGER :: ACTIVE(npa), ACTIVESEC(npa)
      REAL(rkind) max_val, min_val
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
!   ZETA_SETUP is allocated on 1:NSEA
!   ZETA_WORK is on 1:npa
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'NAPROC=', NAPROC, ' NTPROC=', NTPROC
      WRITE(740+IAPROC,*) 'NSEAL=', NSEAL
      WRITE(740+IAPROC,*) 'npa=', npa, ' np=', np
      FLUSH(740+IAPROC)
#endif
      ZETA_WORK=0
      DO IP=1,npa
        IX=iplg(IP)
        ISEA=MAPFS(1,IX)
        IF (ISEA .gt. 0) THEN
          ZETA_WORK(IP)=ZETA_SETUP(ISEA)
        END IF
      END DO
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'Before TRIG_COMPUTE_LH_STRESS'
      FLUSH(740+IAPROC)
#endif

      CALL TRIG_COMPUTE_LH_STRESS(F_X, F_Y, DWNX)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'After TRIG_COMPUTE_LH_STRESS'
      FLUSH(740+IAPROC)
#endif
      CALL COMPUTE_ACTIVE_NODE(DWNX, ACTIVE)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'After COMPUTE_ACTIVE_NODE'
      FLUSH(740+IAPROC)
#endif
      CALL TRIG_WAVE_SETUP_COMPUTE_SYSTEM(ASPAR, B, F_X, F_Y, DWNX, ACTIVE, ACTIVESEC)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'Before,B,min=', minval(B), ' max=', maxval(B)
      FLUSH(740+IAPROC)
#endif


!      CALL TRIG_SET_MEANVALUE_TO_ZERO(B)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'After,B,min=', minval(B), ' max=', maxval(B)
      FLUSH(740+IAPROC)
#endif


      CALL TRIG_WAVE_SETUP_SOLVE_POISSON_NEUMANN_DIR(ASPAR, B, ZETA_WORK, ACTIVE, ACTIVESEC)

      CALL TRIG_SET_MEANVALUE_TO_ZERO(ZETA_WORK)
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'After SET_MEAN ZETA_WORK(min/max)=', minval(ZETA_WORK), maxval(ZETA_WORK)
      FLUSH(740+IAPROC)
#endif
      CALL PDLIB_exchange1Dreal(ZETA_WORK)
      max_val = -100000000
      min_val = -100000000
      DO IP=1,npa
        IX=iplg(IP)
        ISEA=MAPFS(1,IX)
        IF (ISEA .gt. 0) THEN
           ZETA_SETUP(ISEA) = ZETA_WORK(IP)
           max_val = MAX(max_Val, ZETA_WORK(IP))
           min_val = MAX(min_Val, ZETA_WORK(IP))
        END IF
      END DO
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'TRIG_WAVE_SETUP_COMPUTATION, max/min=', max_val, min_val
      FLUSH(740+IAPROC)
#endif
      ZETA_WORK_ALL = 0.
      DO IP = 1, npa
        isea = iplg(IP)
        ZETA_WORK_ALL(isea) = ZETA_WORK(IP)
      END DO
      CALL SYNCHRONIZE_GLOBAL_ARRAY(ZETA_WORK_ALL)
      DO IX = 1, NX
        ZETA_SETUP(IX) = ZETA_WORK_ALL(IX)
      END DO
      IF (IAPROC .EQ. 1) THEN
        write(6666)  1. 
        write(6666)  (ZETA_WORK_ALL(IX), ZETA_WORK_ALL(IX), ZETA_WORK_ALL(IX), IX = 1, NX)
      ENDIF
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'Now exiting TRIG_WAVE_SETUP_COMPUTATION'
      FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Wave setup for FD grids.
!>
!> @param[in] IMOD
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE PREPARATION_FD_SCHEME(IMOD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Wave setup for FD grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_NNZ, PDLIB_IA, PDLIB_JA, PDLIB_I_DIAG
      USE W3GDATMD, ONLY: NX, NY, NSEA, MAPSF, GRIDS
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      integer, intent(in) :: IMOD
      integer IN, ISEA, nbEdge
      integer IX, IY, idx
      integer NeighMat(4,2)
      integer, allocatable :: STAT_SeaLand(:,:)
      integer, allocatable :: EDGES(:,:)
      integer IXN, JXN, JSEA, J
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      !
      allocate(GRIDS(IMOD)%NEIGH(NSEA,4))
      GRIDS(IMOD)%NEIGH=0
      allocate(STAT_SeaLand(NX,NY))
      STAT_SeaLand=0
      DO ISEA=1,NSEA
        IX=MAPSF(ISEA,1)
        IY=MAPSF(ISEA,2)
        STAT_SeaLand(IX,IY)=ISEA
      END DO
      NeighMat(1,1)=1
      NeighMat(1,2)=0
      NeighMat(2,1)=-1
      NeighMat(2,2)=0
      NeighMat(3,1)=0
      NeighMat(3,2)=1
      NeighMat(4,1)=0
      NeighMat(4,2)=-1
      nbEdge=0
      PDLIB_NNZ=0
      DO ISEA=1,NSEA
        IX=MAPSF(ISEA,1)
        IY=MAPSF(ISEA,2)
        idx=0
        DO IN=1,4
          IXN=IX+NeighMat(IN,1)
          JXN=IX+NeighMat(IN,2)
          JSEA=STAT_SeaLand(IXN,JXN)
          IF (JSEA .gt. 0) THEN
            idx=idx+1
            GRIDS(IMOD)%NEIGH(ISEA,idx)=JSEA
            IF (JSEA < ISEA) THEN
              nbEdge=nbEdge+1
            END IF
            PDLIB_NNZ=PDLIB_NNZ+1
          END IF
        END DO
        PDLIB_NNZ=PDLIB_NNZ+1
      END DO
      !
      GRIDS(IMOD)%NBEDGE=NBEDGE
      ALLOCATE(GRIDS(IMOD)%EDGES(NBEDGE,2))
      idx=0
      DO ISEA=1,NSEA
        IX=MAPSF(ISEA,1)
        IY=MAPSF(ISEA,2)
        DO IN=1,4
          IXN=IX+NeighMat(IN,1)
          JXN=IX+NeighMat(IN,2)
          JSEA=STAT_SeaLand(IXN,JXN)
          IF (JSEA .gt. 0) THEN
            IF (JSEA < ISEA) THEN
              idx=idx+1
              GRIDS(IMOD)%EDGES(idx,1)=JSEA
              GRIDS(IMOD)%EDGES(idx,2)=ISEA
            END IF
          END IF
        END DO
      END DO
      !
      ALLOCATE(PDLIB_IA(NSEA+1))
      ALLOCATE(PDLIB_JA(PDLIB_NNZ))
      ALLOCATE(PDLIB_I_DIAG(NSEA))
      PDLIB_IA(1)=1
      J=0
      DO ISEA=1,NSEA
        DO IN=1,4
          IXN=IX+NeighMat(IN,1)
          JXN=IX+NeighMat(IN,2)
          JSEA=STAT_SeaLand(IXN,JXN)
          IF (JSEA .gt. 0) THEN
            J=J+1
            PDLIB_JA(J)=JSEA
          END IF
        END DO
        J=J+1
        PDLIB_JA(J)=ISEA
        PDLIB_I_DIAG(ISEA)=J
        PDLIB_IA(ISEA+1)=J+1
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Compute off diagonal for FD grids.
!>
!> @param[in]  ASPAR
!> @param[in]  TheIn
!> @param[out] TheOut
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_WAVE_SETUP_APPLY_FCT(ASPAR, TheIn, TheOut)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : comp. off diagonal for FD grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: NX, NNZ, IAA, JAA, NSEA
      use yowNodepool, only: PDLIB_IA, PDLIB_JA
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      REAL(rkind), intent(in) :: ASPAR(NNZ)
      REAL(rkind), intent(in) :: TheIn(NSEA)
      REAL(rkind), intent(out) :: TheOut(NSEA)
      integer IP, J, JP
      REAL(rkind) :: eCoeff
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      TheOut=0
      DO IP=1,NSEA
        DO J=PDLIB_IA(IP),PDLIB_IA(IP+1)-1
          JP=PDLIB_JA(J)
          eCoeff=ASPAR(J)
          TheOut(IP)=TheOut(IP) + eCoeff*TheIn(JP)
        END DO
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Preconditioning for FD grids.
!>
!> @param[in]  ASPAR
!> @param[in]  TheIn
!> @param[out] TheOut
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_WAVE_SETUP_APPLY_PRECOND(ASPAR, TheIn, TheOut)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Precond. for FD grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_NNZ, PDLIB_IA, PDLIB_JA, PDLIB_I_DIAG
      USE W3GDATMD, ONLY: NSEA
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      REAL(rkind), intent(in) :: ASPAR(PDLIB_NNZ)
      REAL(rkind), intent(in) :: TheIn(NSEA)
      REAL(rkind), intent(out) :: TheOut(NSEA)
      integer IP, J1, J, JP, J2
      REAL(rkind) :: eCoeff
      INTEGER :: ThePrecond = 0
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      IF (ThePrecond .eq. 0) THEN
        TheOut=TheIn
      END IF
      IF (ThePrecond .eq. 1) THEN
        TheOut=0
        DO IP=1,NSEA
          J1=PDLIB_I_DIAG(IP)
          DO J=PDLIB_IA(IP),PDLIB_IA(IP+1)-1
            JP=PDLIB_JA(J)
            IF (J .eq. J1) THEN
              eCoeff=1.0/ASPAR(J)
            ELSE
              J2=PDLIB_I_DIAG(JP)
              eCoeff=-ASPAR(J) /(ASPAR(J1)*ASPAR(J2))
            END IF
            TheOut(IP)=TheOut(IP) + eCoeff*TheIn(JP)
          END DO
        END DO
      END IF
      IF (ThePrecond .eq. 2) THEN

        DO IP=1,NSEA
          J=PDLIB_I_DIAG(IP)
          TheOut(IP)=TheIn(IP)/ASPAR(J)
        END DO
      END IF
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Radiation stresses for FD grids.
!>
!> @param[out] SXX_t
!> @param[out] SXY_t
!> @param[out] SYY_t
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_COLLECT_SXX_XY_YY(SXX_t, SXY_t, SYY_t)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Rad. stresses for FD grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3ADATMD, ONLY: SXX, SXY, SYY
      USE W3GDATMD, ONLY: NSEA, NSEAL
      USE W3ODATMD, only : IAPROC, NAPROC
      use yowDatapool, only: rtype, istatus
      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      integer ISEA, JSEA
      integer ierr
      real(rkind), intent(out) :: SXX_t(NSEA), SXY_t(NSEA), SYY_t(NSEA)
      real(rkind) :: SXX_p(NSEAL), SXY_p(NSEAL), SYY_p(NSEAL)
      real(rkind), allocatable :: rVect(:)
      integer IPROC, NSEAL_loc
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      DO ISEA=1,NSEAL
        SXX_p(ISEA)=SXX(ISEA)
        SXY_p(ISEA)=SXY(ISEA)
        SYY_p(ISEA)=SYY(ISEA)
      END DO
      IF (IAPROC .eq. 1) THEN
        DO JSEA=1,NSEAL
          ISEA=1 + (JSEA-1)*NAPROC
          SXX_t(ISEA)=SXX_p(JSEA)
          SXY_t(ISEA)=SXY_p(JSEA)
          SYY_t(ISEA)=SYY_p(JSEA)
        END DO
        DO IPROC=2,NAPROC
          NSEAL_loc=1 + (NSEA-IPROC)/NAPROC
          allocate(rVect(NSEAL_loc))
          CALL MPI_RECV(rVect,NSEAL_loc,rtype, iProc-1, 83, MPI_COMM_WCMP, istatus, ierr)
          DO JSEA=1,NSEAL_loc
            ISEA = IPROC + (JSEA-1)*NAPROC
            SXX_t(ISEA)=rVect(JSEA)
          END DO
          CALL MPI_RECV(rVect,NSEAL_loc,rtype, iProc-1, 89, MPI_COMM_WCMP, istatus, ierr)
          DO JSEA=1,NSEAL_loc
            ISEA = IPROC + (JSEA-1)*NAPROC
            SXY_t(ISEA)=rVect(JSEA)
          END DO
          CALL MPI_RECV(rVect,NSEAL_loc,rtype, iProc-1, 97, MPI_COMM_WCMP, istatus, ierr)
          DO JSEA=1,NSEAL_loc
            ISEA = IPROC + (JSEA-1)*NAPROC
            SYY_t(ISEA)=rVect(JSEA)
          END DO
          deallocate(rVect)
        END DO
      ELSE
        CALL MPI_SEND(SXX_p,NSEAL,rtype, 0, 83, MPI_COMM_WCMP, ierr)
        CALL MPI_SEND(SXY_p,NSEAL,rtype, 0, 83, MPI_COMM_WCMP, ierr)
        CALL MPI_SEND(SYY_p,NSEAL,rtype, 0, 83, MPI_COMM_WCMP, ierr)
      END IF
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Setup fluxes.
!>
!> @param[in] SXX_t
!> @param[in] SXY_t
!> @param[in] SYY_t
!> @param[out] FX
!> @param[out] FY
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_COMPUTE_LH_STRESS(SXX_t, SXY_t, SYY_t, FX, FY)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : setup fluxes
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: NX, NY, NSEA, NEIGH
      USE W3ADATMD, ONLY: SXX, SXY, SYY
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(in) :: SXX_t(NSEA), SXY_t(NSEA), SYY_t(NSEA)
      real(rkind), intent(out) :: FX(NSEA), FY(NSEA)
      REAL(rkind) :: h
      REAL(rkind) :: SXX_X, SXX_Y
      REAL(rkind) :: SXY_X, SXY_Y
      REAL(rkind) :: SYY_X, SYY_Y
      REAL(rkind) :: eFX, eFY
      REAL(rkind) :: UGRAD, VGRAD
      INTEGER IE, I1, I2, I3, IP1, IP2, IP3
      integer ISEA, JSEA1, JSEA2, JSEA3, JSEA4
      integer NeighMat(4,2)
      real(rkind) dist_X, dist_Y
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      !
      NeighMat(1,1)=1
      NeighMat(1,2)=0
      NeighMat(2,1)=-1
      NeighMat(2,2)=0
      NeighMat(3,1)=0
      NeighMat(3,2)=1
      NeighMat(4,1)=0
      NeighMat(4,2)=-1
      FX=0
      FY=0
      DO ISEA=1,NSEA
        JSEA1=NEIGH(ISEA,1)
        JSEA2=NEIGH(ISEA,2)
        JSEA3=NEIGH(ISEA,3)
        JSEA4=NEIGH(ISEA,4)
        SXX_X=0
        SXX_Y=0
        SXY_X=0
        SXY_Y=0
        SYY_X=0
        SYY_Y=0
        IF ((JSEA1 .gt. 0).and.(JSEA2 .gt. 0)) THEN
          SXX_X=(SXX(JSEA1) - SXX(JSEA2))/(2*dist_X)
          SXY_X=(SXY(JSEA1) - SXY(JSEA2))/(2*dist_X)
          SYY_X=(SXY(JSEA1) - SYY(JSEA2))/(2*dist_X)
        END IF
        IF ((JSEA1 .gt. 0).and.(JSEA2 .eq. 0)) THEN
          SXX_X=(SXX(JSEA1) - SXX(ISEA ))/dist_X
          SXY_X=(SXY(JSEA1) - SXY(ISEA ))/dist_X
          SYY_X=(SXY(JSEA1) - SYY(ISEA ))/dist_X
        END IF
        IF ((JSEA1 .eq. 0).and.(JSEA2 .gt. 0)) THEN
          SXX_X=(SXX(ISEA ) - SXX(JSEA2))/dist_X
          SXY_X=(SXY(ISEA ) - SXY(JSEA2))/dist_X
          SYY_X=(SXY(ISEA ) - SYY(JSEA2))/dist_X
        END IF
        IF ((JSEA3 .gt. 0).and.(JSEA4 .gt. 0)) THEN
          SXX_X=(SXX(JSEA3) - SXX(JSEA4))/(2*dist_Y)
          SXY_X=(SXY(JSEA3) - SXY(JSEA4))/(2*dist_Y)
          SYY_X=(SXY(JSEA3) - SYY(JSEA4))/(2*dist_Y)
        END IF
        IF ((JSEA3 .eq. 0).and.(JSEA4 .gt. 0)) THEN
          SXX_X=(SXX(ISEA ) - SXX(JSEA4))/dist_Y
          SXY_X=(SXY(ISEA ) - SXY(JSEA4))/dist_Y
          SYY_X=(SXY(ISEA ) - SYY(JSEA4))/dist_Y
        END IF
        IF ((JSEA3 .gt. 0).and.(JSEA4 .gt. 0)) THEN
          SXX_X=(SXX(JSEA3) - SXX(ISEA ))/dist_Y
          SXY_X=(SXY(JSEA3) - SXY(ISEA ))/dist_Y
          SYY_X=(SXY(JSEA3) - SYY(ISEA ))/dist_Y
        END IF
        eFX=-SXX_X - SXY_Y
        eFY=-SYY_Y - SXY_X
        FX(ISEA)=eFX
        FY(ISEA)=eFY
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Differences on FD grids.
!>
!> @param[in]  IEDGE
!> @param[in]  ISEA
!> @param[inout] UGRAD
!> @param[inout] VGRAD
!> @param[inout] dist
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_COMPUTE_DIFF(IEDGE, ISEA, UGRAD, VGRAD, dist)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : differences on FD grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: MAPSF, EDGES
      USE W3GDATMD, ONLY: XGRD, YGRD
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER, intent(in) :: IEDGE, ISEA
      REAL(rkind), intent(inout) :: UGRAD, VGRAD, dist
      REAL(rkind) :: h
      integer I2, I3, IP1, IP2, IP3
      integer IX1, IY1, IX2, IY2
      integer ISEA1, ISEA2
      REAL(rkind) deltaX, deltaY
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      !
      ISEA1=EDGES(IEDGE,1)
      ISEA2=EDGES(IEDGE,2)
      IX1=MAPSF(ISEA1,1)
      IY1=MAPSF(ISEA1,2)
      IX2=MAPSF(ISEA2,1)
      IY2=MAPSF(ISEA2,2)
      deltaX=XGRD(IX1,IY1) - XGRD(IX2,IY2)
      deltaY=YGRD(IX1,IY1) - YGRD(IX2,IY2)
      dist=SQRT(deltaX*deltaX + deltaY*deltaY)
      IF (ISEA .eq. ISEA1) THEN
        UGRAD= deltaX/dist
        VGRAD= deltaY/dist
      ELSE
        UGRAD=-deltaX/dist
        VGRAD=-deltaY/dist
      END IF
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Setup matrix on FD grids.
!>
!> @param[out]  ASPAR
!> @param[out]  B
!> @param[in] FX
!> @param[in] FY
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_WAVE_SETUP_COMPUTE_SYSTEM(ASPAR, B, FX, FY)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : Setup matrix on FD grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_NNZ
      USE W3GDATMD, ONLY: NX, NY, NSEA, NBEDGE, EDGES
      USE W3ADATMD, ONLY: DW
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(in)  :: FX(NSEA), FY(NSEA)
      real(rkind), intent(out) :: ASPAR(PDLIB_NNZ)
      real(rkind), intent(out) :: B(NX)
      INTEGER :: POS_TRICK(3,2), POS_SHIFT(3,3)
      integer I1, I2, I3, IP1, IP2, IP3
      integer IDX, IDX1, IDX2, IDX3
      INTEGER IE, IP, I, J, K, IPp, JPp
      real(rkind) :: eDep, eFX, eFY, eScal, eFact, eLen
      real(rkind) :: UGRAD, VGRAD, UGRAD1, VGRAD1, dist1, dist2
      INTEGER LIDX(2), KIDX(2), jdx
      INTEGER ISEAREL, JSEAREL, ISEA, JSEA, IEDGE
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      !
      ASPAR=0
      B=0
      DO IEDGE=1,NBEDGE
        ISEA=EDGES(IEDGE,1)
        JSEA=EDGES(IEDGE,2)
        eDep=(DW(ISEA) + DW(JSEA))/2.0
        eFX =(FX(ISEA) + FX(JSEA))/2.0
        eFY =(FY(ISEA) + FY(JSEA))/2.0
        DO I=1,2
          ISEAREL=EDGES(IEDGE,I)
          CALL FD_COMPUTE_DIFF(IEDGE, ISEAREL, UGRAD1, VGRAD1, dist1)
          eScal=UGRAD1*eFX + VGRAD1*eFY
          B(ISEAREL) = B(ISEAREL) + eScal*dist1
          !
          DO J=1,2
            JSEAREL=EDGES(IEDGE,J)
            CALL FD_COMPUTE_DIFF(IEDGE, JSEAREL, UGRAD, VGRAD, dist2)
            eScal=UGRAD*UGRAD1 + VGRAD*VGRAD1
            ASPAR(J)=ASPAR(J)+eFact*eScal
          END DO
        END DO
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Scalar product.
!>
!> @param[in]    V1
!> @param[in]    V2
!> @param[inout] eScal
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_WAVE_SETUP_SCALAR_PROD(V1, V2, eScal)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : scalar prod.
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: NX
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(in) :: V1(NX), V2(NX)
      real(rkind), intent(inout) :: eScal
      integer IP
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      eScal=0
      DO IP=1,NX
        eScal=eScal + V1(IP)*V2(IP)
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Poisson solver on FD grids.
!>
!> @param[in]    ASPAR
!> @param[in]    B
!> @param[out] TheOut
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_WAVE_SETUP_SOLVE_POISSON_NEUMANN_DIR(ASPAR, B, TheOut)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : possoin solver on fd grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_NNZ
      USE W3GDATMD, ONLY: NX
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(in) :: ASPAR(PDLIB_NNZ)
      real(rkind), intent(in) :: B(NX)
      real(rkind), intent(out) :: TheOut(NX)
      real(rkind) :: V_X(NX), V_R(NX), V_Z(NX), V_P(NX), V_Y(NX)
      real(rkind) :: uO, uN, alphaV, h1, h2
      real(rkind) :: eNorm, beta
      real(rkind) :: SOLVERTHR
      integer IP, nbIter
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      SOLVERTHR=0.00000001
      nbIter=0
      V_X=0
      V_R=B
      CALL FD_WAVE_SETUP_APPLY_PRECOND(ASPAR, V_R, V_Z)
      V_P=V_Z
      CALL FD_WAVE_SETUP_SCALAR_PROD(V_Z, V_R, uO)
      DO
        nbIter=nbIter + 1
        CALL FD_WAVE_SETUP_APPLY_FCT(ASPAR, V_P, V_Y)
        CALL FD_WAVE_SETUP_SCALAR_PROD(V_P, V_Y, h2)
        alphaV=uO/h2
        !
        DO IP=1,NX
          V_X(IP) = V_X(IP) + alphaV * V_P(IP)
          V_R(IP) = V_R(IP) - alphaV * V_Y(IP)
        END DO
        !
        CALL FD_WAVE_SETUP_SCALAR_PROD(V_R, V_R, eNorm)
        IF (eNorm .le. SOLVERTHR) THEN
          EXIT
        END IF
        !
        CALL FD_WAVE_SETUP_APPLY_PRECOND(ASPAR, V_R, V_Z)
        CALL FD_WAVE_SETUP_SCALAR_PROD(V_Z, V_R, uN)
        !
        beta=uN/uO
        uO=uN
        !
        DO IP=1,NX
          V_P(IP)=V_Z(IP) + beta * V_P(IP)
        END DO
      END DO
      TheOut=V_X
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Set mean value.
!>
!> @param[inout] TheVar
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_SET_MEANVALUE_TO_ZERO(TheVar)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : set meanvalue
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: NX, SI
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      real(rkind), intent(inout) :: TheVar(NX)
      real(rkind) :: SUM_SI_Var, SUM_SI, TheMean
      INTEGER IP
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      SUM_SI_Var=0
      SUM_SI=0
      DO IP=1,NX
        SUM_SI_Var = SUM_SI_Var + SI(IP)*TheVar(IP)
        SUM_SI     = SUM_SI     + SI(IP)
      END DO
      TheMean=SUM_SI_Var/SUM_SI
      DO IP=1,NX
        TheVar(IP)=TheVar(IP) - TheMean
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief Wave setup comp on FD grids.
!>
!> @param[inout] TheVar
!>
!> @author Mathieu Dutour-Sikiric
!> @author Aron Roland
!> @date 1-May-2018
!>
      SUBROUTINE FD_WAVE_SETUP_COMPUTATION
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : wave setup comp. on fd grids
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE yowNodepool, only: PDLIB_NNZ
      USE W3GDATMD, ONLY: NX, NSEA, NSEAL
      USE W3WDATMD, ONLY: ZETA_SETUP
      use yowDatapool, only: rtype, istatus
      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      USE W3ODATMD, only : IAPROC, NAPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      REAL(rkind) :: ZETA_WORK(NSEA)
      REAL(rkind) :: F_X(NSEA), F_Y(NSEA)
      REAL(rkind) :: ASPAR(PDLIB_NNZ), B(NX)
      INTEGER ISEA, IPROC
      real(rkind) :: SXX_t(NSEA), SXY_t(NSEA), SYY_t(NSEA)
      integer ierr
#ifdef W3_DEBUGSTP
      real(rkind) max_val, min_val
#endif
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      CALL FD_COLLECT_SXX_XY_YY(SXX_t, SXY_t, SYY_t)
      IF (IAPROC .eq. 1) THEN
        CALL FD_COMPUTE_LH_STRESS(SXX_t, SXY_t, SYY_t, F_X, F_Y)
        DO ISEA=1,NSEA
          ZETA_WORK(ISEA)=ZETA_SETUP(ISEA)
        END DO
        CALL FD_WAVE_SETUP_COMPUTE_SYSTEM(ASPAR, B, F_X, F_Y)
        CALL FD_WAVE_SETUP_SOLVE_POISSON_NEUMANN_DIR(ASPAR, B, ZETA_WORK)
        CALL FD_SET_MEANVALUE_TO_ZERO(ZETA_WORK)
        DO IPROC=2,NAPROC
          CALL MPI_SEND(ZETA_WORK,NSEA,rtype, IPROC-1, 23, MPI_COMM_WCMP, ierr)
        END DO
      ELSE
        CALL MPI_RECV(ZETA_WORK,NSEAL,rtype, 0, 23, MPI_COMM_WCMP, istatus, ierr)
      END IF
#ifdef W3_DEBUGSTP
      max_val = ZETA_WORK(ISEA)
      min_val = ZETA_WORK(ISEA)
#endif
      DO ISEA=1,NSEA
         ZETA_SETUP(ISEA)=ZETA_WORK(ISEA)
#ifdef W3_DEBUGSTP
         max_val = MAX(max_val, ZETA_WORK(ISEA))
         min_val = MIN(min_val, ZETA_WORK(ISEA))
#endif
      END DO
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'FD_WAVE_SETUP_COMPUTATION, max/min=', max_val, min_val
      FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
!>
!> @brief General driver.
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date 1-May-2018
!>
      SUBROUTINE WAVE_SETUP_COMPUTATION
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : general driver
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks
!  8. Structure :
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      USE W3GDATMD, ONLY: NSEA, NSEAL
      USE W3GDATMD, ONLY: GTYPE, UNGTYPE
      USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      INTEGER ISEA, JSEA
      REAL(rkind), allocatable :: ZETA_WORK(:)
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'NAPROC=', NAPROC
      WRITE(740+IAPROC,*) 'NTPROC=', NTPROC
      FLUSH(740+IAPROC)
#endif
      IF (IAPROC .le. NAPROC) THEN
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'Begin WAVE_SETUP_COMPUTATION'
      FLUSH(740+IAPROC)
#endif
        IF (DO_WAVE_SETUP) THEN
          IF (GTYPE .EQ. UNGTYPE) THEN
            CALL TRIG_WAVE_SETUP_COMPUTATION
          ELSE
            CALL FD_WAVE_SETUP_COMPUTATION
          END IF
        END IF
      END IF
#ifdef W3_DEBUGSTP
      WRITE(740+IAPROC,*) 'End WAVE_SETUP_COMPUTATION'
      FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      END MODULE
!/ ------------------------------------------------------------------- /
