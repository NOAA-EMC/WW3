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
!/S      USE W3SERVMD, ONLY: STRACE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      LOGICAL :: DO_WAVE_SETUP = .TRUE.
      CONTAINS
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      REAL(8), INTENT(IN)  :: VAR(npa)
      REAL(8), INTENT(OUT) :: DVDX(npa), DVDY(npa)
      INTEGER           :: NI(3)
      INTEGER           :: IE, I1, I2, I3, IP
      REAL(8)           :: DEDY(3),DEDX(3)
      REAL(8)           :: DVDXIE, DVDYIE
      REAL(8)           :: WEI(npa), eW
      INTEGER           :: IX
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      REAL(8), INTENT(IN)  :: VAR(npa)
      REAL(8), INTENT(OUT) :: DVDX(npa), DVDY(npa)
      INTEGER           :: NI(3)
      INTEGER           :: IE, I1, I2, I3, IP, IX
      REAL(8)           :: DEDY(3),DEDX(3)
      REAL(8)           :: DVDXIE, DVDYIE
      REAL(8)           :: WEI(npa), eW
      INTEGER           :: IX1, IX2, IX3, ISEA
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      REAL(8), INTENT(IN)  :: VAR(npa)
      REAL(8), INTENT(OUT) :: DVDX(npa), DVDY(npa)
      CALL DIFFERENTIATE_XYDIR_MAPSTA(VAR, DVDX, DVDY)
!      CALL DIFFERENTIATE_XYDIR_NATIVE(VAR, DVDX, DVDY)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(out) :: F_X(npa), F_Y(npa), DWNX(npa)
      REAL(8) :: h
      REAL(8) :: SXX_X, SXX_Y
      REAL(8) :: SXY_X, SXY_Y
      REAL(8) :: SYY_X, SYY_Y
      INTEGER I, IP, IX
      INTEGER JSEA, ISEA
      real(8) :: U_X1(npa), U_Y1(npa)
      real(8) :: U_X2(npa), U_Y2(npa)
      real(8) :: SXX_p(npa), SXY_p(npa), SYY_p(npa)
      real(8) :: eSXX, eSXY, eSYY
      integer :: SXXmethod = 1
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
!/DEBUGSTP      WRITE(740+IAPROC,*) 'min/max(DEP)=', minval(DWNX), maxval(DWNX)

!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(abs(SXX))=', sum(abs(SXX_p))
!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(abs(SXY))=', sum(abs(SXY_p))
!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(abs(SYY))=', sum(abs(SYY_p))
!/DEBUGSTP      FLUSH(740+IAPROC)

      CALL DIFFERENTIATE_XYDIR(SXX_p, U_X1, U_Y1)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(absU_XY1)=', sum(abs(U_X1)), sum(abs(U_Y1))
!/DEBUGSTP      FLUSH(740+IAPROC)
      CALL DIFFERENTIATE_XYDIR(SXY_p, U_X2, U_Y2)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(absU_XY2)=', sum(abs(U_X2)), sum(abs(U_Y2))
!/DEBUGSTP      FLUSH(740+IAPROC)
      F_X = -U_X1 - U_Y2
      !
      CALL DIFFERENTIATE_XYDIR(SYY_p, U_X1, U_Y1)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(absU_XY1)=', sum(abs(U_X1)), sum(abs(U_Y1))
!/DEBUGSTP      FLUSH(740+IAPROC)
      F_Y = -U_Y1 - U_X2
!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(F_X)=', sum(F_X)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'sum(F_Y)=', sum(F_Y)
!/DEBUGSTP      FLUSH(740+IAPROC)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      INTEGER, intent(in) :: IE, I1
      REAL(8), intent(inout) :: UGRAD, VGRAD
      REAL(8) :: h
      integer I2, I3, IP1, IP2, IP3
      INTEGER :: POS_TRICK(3,2)
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      use yowElementpool, only: INE, NE
      use yowNodepool, only: PDLIB_NNZ, PDLIB_JA_IE, PDLIB_TRIA, npa, np
      USE yowNodepool, only: iplg
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(in)  :: FX(npa), FY(npa), DWNX(npa)
      real(8), intent(out) :: ASPAR(PDLIB_NNZ)
      real(8), intent(out) :: B(npa)
      integer, intent(in)  :: ACTIVE(npa)
      integer, intent(out)  :: ACTIVESEC(npa)
      INTEGER :: POS_TRICK(3,2), POS_SHIFT(3,3)
      integer I1, I2, I3, IP1, IP2, IP3
      integer IDX, IDX1, IDX2, IDX3
      INTEGER IE, IP, I, J, K, IPp, JPp
      real(8) :: eDep, eFX, eFY, eScal, eFact, eArea
      real(8) :: UGRAD, VGRAD, UGRAD1, VGRAD1
      real(8) :: eOff
      logical DoPrintOut
      INTEGER sumActive
      INTEGER LIDX(2), KIDX(2), jdx
      INTEGER IPglob1, IPglob2, IPglob3
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
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      REAL(8), intent(in) :: ASPAR(PDLIB_NNZ)
      REAL(8), intent(in) :: TheIn(npa)
      REAL(8), intent(out) :: TheOut(npa)
      INTEGER, intent(IN) :: ACTIVE(npa), ACTIVESEC(npa)
      REAL(8) :: ListDiag(npa)
      integer IP, J1, J, JP, J2
      REAL(8) :: eCoeff
      INTEGER :: ThePrecond = 2
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
            ListDiag(IP)=ASPAR(J)
            TheOut(IP)=TheIn(IP)/ASPAR(J)
          ELSE
            ListDiag(IP)=1
            TheOut(IP)=TheIn(IP)
          END IF
        END DO
        WRITE(740+IAPROC,*) 'Diag, min=', minval(ListDiag), ' max=', maxval(ListDiag)
        WRITE(740+IAPROC,*) 'Diag, quot=', maxval(ListDiag)/minval(ListDiag)
      END IF
      CALL PDLIB_exchange1Dreal(TheOut)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      REAL(8), intent(in) :: ASPAR(PDLIB_NNZ)
      REAL(8), intent(in) :: TheIn(npa)
      REAL(8), intent(out) :: TheOut(npa)
      INTEGER, intent(in) :: ACTIVE(npa), ACTIVESEC(npa)
      integer IP, J, JP
      REAL(8) :: eCoeff
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      USE W3GDATMD, ONLY: NX
      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      use yowDatapool, only: rtype, istatus
      use yowNodepool, only: np, npa
      USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
      USE W3GDATMD, ONLY: NSEAL
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(in) :: V1(npa), V2(npa)
      real(8), intent(inout) :: eScal
      integer IP, myrank, myproc
      real(8) :: rScal(1), lScal(1)
      integer iProc
      integer ierr
      CALL MPI_COMM_RANK(MPI_COMM_WCMP, myrank, ierr)
      CALL MPI_COMM_SIZE(MPI_COMM_WCMP, myproc, ierr)
      lScal=0
      DO IP=1,np
        lScal(1)=lScal(1) + V1(IP)*V2(IP)
      END DO
      IF (IAPROC .eq. 1) THEN
        DO iProc=2,NAPROC
          CALL MPI_RECV(rScal,1,rtype, iProc-1, 19, MPI_COMM_WCMP, istatus, ierr)
          lScal = lScal + rScal
        END DO
        DO iProc=2,NAPROC
          CALL MPI_SEND(lScal,1,rtype, iProc-1, 23, MPI_COMM_WCMP, ierr)
        END DO
      ELSE
        CALL MPI_SEND(lScal,1,rtype, 0, 19, MPI_COMM_WCMP, ierr)
        CALL MPI_RECV(lScal,1,rtype, 0, 23, MPI_COMM_WCMP, istatus, ierr)
      END IF
      eScal=lScal(1)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(in) :: ASPAR(PDLIB_NNZ)
      real(8), intent(in) :: B(npa)
      real(8), intent(out) :: TheOut(npa)
      integer, intent(in) :: ACTIVE(npa), ACTIVESEC(npa)
      real(8) :: V_X(npa), V_R(npa), V_Z(npa), V_P(npa), V_Y(npa)
      real(8) :: uO, uN, alphaV, h1, h2
      real(8) :: eNorm, beta
      real(8) :: SOLVERTHR
      integer IP, nbIter
      SOLVERTHR=SOLVERTHR_STP

!/DEBUGSTP      WRITE(740+IAPROC,*) 'Begin TRIG_WAVE_SETUP_SOLVE ....'
!/DEBUGSTP      FLUSH(740+IAPROC)
      nbIter=0
      V_X=0
      V_R=B
      CALL TRIG_WAVE_SETUP_APPLY_PRECOND(ASPAR, V_R, V_Z, ACTIVE, ACTIVESEC)
      V_P=V_Z
      CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_Z, V_R, uO)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'uO=', uO
!/DEBUGSTP      FLUSH(740+IAPROC)
      CALL TRIG_WAVE_SETUP_SCALAR_PROD(B, B, eNorm)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'eNorm(B)=', eNorm
!/DEBUGSTP      WRITE(740+IAPROC,*) 'SOLVERTHR=', SOLVERTHR
!/DEBUGSTP      FLUSH(740+IAPROC)
      WRITE(740+IAPROC,*) 'SOLVERTHR=', SOLVERTHR, ' eNorm(B)=', eNorm
      IF (eNorm .le. SOLVERTHR) THEN
!/DEBUGSTP      WRITE(740+IAPROC,*) 'Leaving here, zero solution'
!/DEBUGSTP      FLUSH(740+IAPROC)
        TheOut=V_X
        RETURN
      END IF
      DO
        nbIter=nbIter + 1
!/DEBUGSTP        WRITE(740+IAPROC,*) ' nbIter=', nbIter
!/DEBUGSTP        FLUSH(740+IAPROC)
        CALL TRIG_WAVE_SETUP_APPLY_FCT(ASPAR, V_P, V_Y, ACTIVE, ACTIVESEC)
        CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_P, V_Y, h2)
!/DEBUGSTP        WRITE(740+IAPROC,*) ' h2=', h2
!/DEBUGSTP        FLUSH(740+IAPROC)
        alphaV=uO/h2
!/DEBUGSTP        WRITE(740+IAPROC,*) ' alphaV=', alphaV
!/DEBUGSTP        FLUSH(740+IAPROC)

        !
        DO IP=1,npa
          V_X(IP) = V_X(IP) + alphaV * V_P(IP)
          V_R(IP) = V_R(IP) - alphaV * V_Y(IP)
        END DO
        !
        CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_R, V_R, eNorm)
!/DEBUGSTP        WRITE(740+IAPROC,*) 'eNorm=', eNorm
!/DEBUGSTP        FLUSH(740+IAPROC)
        WRITE(740+IAPROC,*) 'nbIter=', nbIter, ' eNorm(res)=', eNorm
        FLUSH(740+IAPROC)
        IF (eNorm .le. SOLVERTHR) THEN
          EXIT
        END IF
        !
        CALL TRIG_WAVE_SETUP_APPLY_PRECOND(ASPAR, V_R, V_Z, ACTIVE, ACTIVESEC)
        CALL TRIG_WAVE_SETUP_SCALAR_PROD(V_Z, V_R, uN)
!/DEBUGSTP        WRITE(740+IAPROC,*) ' uN=', uN
!/DEBUGSTP        FLUSH(740+IAPROC)
        !
        beta=uN/uO
        uO=uN
!/DEBUGSTP        WRITE(740+IAPROC,*) 'beta=', beta
!/DEBUGSTP        FLUSH(740+IAPROC)
        !
        DO IP=1,npa
          V_P(IP)=V_Z(IP) + beta * V_P(IP)
        END DO
      END DO
      TheOut=V_X
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      USE yowNodepool, only: PDLIB_SI
      USE W3GDATMD, ONLY: NX, SI
      USE W3GDATMD, ONLY: NSEAL
      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
      use yowDatapool, only: rtype, istatus
      use yowNodepool, only: np, npa
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(inout) :: TheVar(npa)
      real(8) :: SUM_SI_Var, SUM_SI, TheMean
      INTEGER IP, ierr
      real(8) :: eVect(2), rVect(2)
      integer iProc
      SUM_SI_Var=0
      SUM_SI=0
      DO IP=1,np
        SUM_SI_Var = SUM_SI_Var + PDLIB_SI(IP)*TheVar(IP)
        SUM_SI     = SUM_SI     + PDLIB_SI(IP)
      END DO
      eVect(1)=SUM_SI_Var
      eVect(2)=SUM_SI
!/DEBUGSTP      WRITE(740+IAPROC,*) 'SUM_SI_Var=', SUM_SI_Var, 'SUM_SI=', SUM_SI
!/DEBUGSTP      FLUSH(740+IAPROC)
      IF (IAPROC .eq. 1) THEN
        DO iProc=2,NAPROC
          CALL MPI_RECV(rVect,2,rtype, iProc-1, 367, MPI_COMM_WCMP, istatus, ierr)
          eVect=eVect + rVect
        END DO
        DO iProc=2,NAPROC
          CALL MPI_SEND(eVect,2,rtype, iProc-1, 37, MPI_COMM_WCMP, ierr)
        END DO
      ELSE
        CALL MPI_SEND(eVect,2,rtype, 0, 367, MPI_COMM_WCMP, ierr)
        CALL MPI_RECV(eVect,2,rtype, 0, 37, MPI_COMM_WCMP, istatus, ierr)
      END IF
      SUM_SI_Var=eVect(1)
      SUM_SI    =eVect(2)
      TheMean=SUM_SI_Var/SUM_SI
!/DEBUGSTP      WRITE(740+IAPROC,*) 'TheMean=', TheMean
!/DEBUGSTP      FLUSH(740+IAPROC)
      DO IP=1,npa
        TheVar(IP)=TheVar(IP) - TheMean
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      REAL*8, INTENT(in) :: DWNX(npa)
      INTEGER, INTENT(out) :: ACTIVE(npa)
      INTEGER IP, eAct
!/DEBUGSTP     INTEGER nbActive
!/DEBUGSTP     nbActive=0
      DO IP=1,NPA
        IF (DWNX(IP) .ge. CRIT_DEP_STP) THEN
          eAct=1
        ELSE
          eAct=0
        END IF
!/DEBUGSTP     nbActive=nbActive + eAct
        ACTIVE(IP)=eAct
      END DO
!/DEBUGSTP      WRITE(740+IAPROC,*) 'min/max(DWNX)=', minval(DWNX), maxval(DWNX)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'CRIT_DEP_STP=', CRIT_DEP_STP
!/DEBUGSTP      WRITE(740+IAPROC,*) 'nbActive=', nbActive, ' npa=', npa
!/DEBUGSTP      FLUSH(740+IAPROC)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
!      CALL W3SETG
      REAL(8) :: ZETA_WORK(npa), ZETA_WORK_ALL(NX)
      REAL(8) :: F_X(npa), F_Y(npa), DWNX(npa)
      REAL(8) :: ASPAR(PDLIB_NNZ), B(npa)
      INTEGER I, ISEA, JSEA, IX, IP, IP_glob
      INTEGER :: ACTIVE(npa), ACTIVESEC(npa)
!   ZETA_SETUP is allocated on 1:NSEA
!   ZETA_WORK is on 1:npa
!/DEBUGSTP      WRITE(740+IAPROC,*) 'NAPROC=', NAPROC, ' NTPROC=', NTPROC
!/DEBUGSTP      WRITE(740+IAPROC,*) 'NSEAL=', NSEAL
!/DEBUGSTP      WRITE(740+IAPROC,*) 'npa=', npa, ' np=', np
      FLUSH(740+IAPROC)
      ZETA_WORK=0
      DO IP=1,npa
        IX=iplg(IP)
        ISEA=MAPFS(1,IX)
        IF (ISEA .gt. 0) THEN
          ZETA_WORK(IP)=ZETA_SETUP(ISEA)
        END IF
      END DO
!/DEBUGSTP      WRITE(740+IAPROC,*) 'Before TRIG_COMPUTE_LH_STRESS'
!/DEBUGSTP      FLUSH(740+IAPROC)

      CALL TRIG_COMPUTE_LH_STRESS(F_X, F_Y, DWNX)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'After TRIG_COMPUTE_LH_STRESS'
!/DEBUGSTP      FLUSH(740+IAPROC)
      CALL COMPUTE_ACTIVE_NODE(DWNX, ACTIVE)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'After COMPUTE_ACTIVE_NODE'
!/DEBUGSTP      FLUSH(740+IAPROC)
      CALL TRIG_WAVE_SETUP_COMPUTE_SYSTEM(ASPAR, B, F_X, F_Y, DWNX, ACTIVE, ACTIVESEC)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'Before,B,min=', minval(B), ' max=', maxval(B)
!/DEBUGSTP      FLUSH(740+IAPROC)


!      CALL TRIG_SET_MEANVALUE_TO_ZERO(B)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'After,B,min=', minval(B), ' max=', maxval(B)
!/DEBUGSTP      FLUSH(740+IAPROC)


      CALL TRIG_WAVE_SETUP_SOLVE_POISSON_NEUMANN_DIR(ASPAR, B, ZETA_WORK, ACTIVE, ACTIVESEC)

      CALL TRIG_SET_MEANVALUE_TO_ZERO(ZETA_WORK)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'After SET_MEAN min=', minval(ZETA_WORK), ' max=', maxval(ZETA_WORK)
!/DEBUGSTP      FLUSH(740+IAPROC)
      DO IP=1,npa
        IX=iplg(IP)
        ZETA_WORK_ALL(IX)=ZETA_WORK(IP)
      END DO
      CALL SYNCHRONIZE_GLOBAL_ARRAY(ZETA_WORK_ALL)
      DO IX=1,NX
        ISEA=MAPFS(1,IX)
        IF (ISEA .gt. 0) THEN
          ZETA_SETUP(ISEA) = ZETA_WORK_ALL(IX)
        END IF
      END DO
!/DEBUGSTP      WRITE(740+IAPROC,*) 'Now exiting TRIG_WAVE_SETUP_COMPUTATION'
!/DEBUGSTP      FLUSH(740+IAPROC)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      integer, intent(in) :: IMOD
      integer IN, ISEA, nbEdge
      integer IX, IY, idx
      integer NeighMat(4,2)
      integer, allocatable :: STAT_SeaLand(:,:)
      integer, allocatable :: EDGES(:,:)
      integer IXN, JXN, JSEA, J
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      REAL(8), intent(in) :: ASPAR(NNZ)
      REAL(8), intent(in) :: TheIn(NSEA)
      REAL(8), intent(out) :: TheOut(NSEA)
      integer IP, J, JP
      REAL(8) :: eCoeff
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      REAL(8), intent(in) :: ASPAR(PDLIB_NNZ)
      REAL(8), intent(in) :: TheIn(NSEA)
      REAL(8), intent(out) :: TheOut(NSEA)
      integer IP, J1, J, JP, J2
      REAL(8) :: eCoeff
      INTEGER :: ThePrecond = 0
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      integer ISEA, JSEA
      integer ierr
      real(8), intent(out) :: SXX_t(NSEA), SXY_t(NSEA), SYY_t(NSEA)
      real(8) :: SXX_p(NSEAL), SXY_p(NSEAL), SYY_p(NSEAL)
      real(8), allocatable :: rVect(:)
      integer IPROC, NSEAL_loc
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(in) :: SXX_t(NSEA), SXY_t(NSEA), SYY_t(NSEA)
      real(8), intent(out) :: FX(NSEA), FY(NSEA)
      REAL(8) :: h
      REAL(8) :: SXX_X, SXX_Y
      REAL(8) :: SXY_X, SXY_Y
      REAL(8) :: SYY_X, SYY_Y
      REAL(8) :: eFX, eFY
      REAL(8) :: UGRAD, VGRAD
      INTEGER IE, I1, I2, I3, IP1, IP2, IP3
      integer ISEA, JSEA1, JSEA2, JSEA3, JSEA4
      integer NeighMat(4,2)
      real(8) dist_X, dist_Y
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      INTEGER, intent(in) :: IEDGE, ISEA
      REAL(8), intent(inout) :: UGRAD, VGRAD, dist
      REAL(8) :: h
      integer I2, I3, IP1, IP2, IP3
      integer IX1, IY1, IX2, IY2
      integer ISEA1, ISEA2
      REAL(8) deltaX, deltaY
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(in)  :: FX(NSEA), FY(NSEA)
      real(8), intent(out) :: ASPAR(PDLIB_NNZ)
      real(8), intent(out) :: B(NX)
      INTEGER :: POS_TRICK(3,2), POS_SHIFT(3,3)
      integer I1, I2, I3, IP1, IP2, IP3
      integer IDX, IDX1, IDX2, IDX3
      INTEGER IE, IP, I, J, K, IPp, JPp
      real(8) :: eDep, eFX, eFY, eScal, eFact, eLen
      real(8) :: UGRAD, VGRAD, UGRAD1, VGRAD1, dist1, dist2
      INTEGER LIDX(2), KIDX(2), jdx
      INTEGER ISEAREL, JSEAREL, ISEA, JSEA, IEDGE
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(in) :: V1(NX), V2(NX)
      real(8), intent(inout) :: eScal
      integer IP
      eScal=0
      DO IP=1,NX
        eScal=eScal + V1(IP)*V2(IP)
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(in) :: ASPAR(PDLIB_NNZ)
      real(8), intent(in) :: B(NX)
      real(8), intent(out) :: TheOut(NX)
      real(8) :: V_X(NX), V_R(NX), V_Z(NX), V_P(NX), V_Y(NX)
      real(8) :: uO, uN, alphaV, h1, h2
      real(8) :: eNorm, beta
      real(8) :: SOLVERTHR
      integer IP, nbIter
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), intent(inout) :: TheVar(NX)
      real(8) :: SUM_SI_Var, SUM_SI, TheMean
      INTEGER IP
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
!      CALL W3SETG
      REAL(8) :: ZETA_WORK(NSEA)
      REAL(8) :: F_X(NSEA), F_Y(NSEA)
      REAL(8) :: ASPAR(PDLIB_NNZ), B(NX)
      INTEGER ISEA, IPROC
      real(8) :: SXX_t(NSEA), SXY_t(NSEA), SYY_t(NSEA)
      integer ierr
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
      DO ISEA=1,NSEA
        ZETA_SETUP(ISEA)=ZETA_WORK(ISEA)
      END DO
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
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
!/S      USE W3SERVMD, ONLY: STRACE
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
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      INTEGER ISEA, JSEA
      REAL*8, allocatable :: ZETA_WORK(:)
!/DEBUGSTP      WRITE(740+IAPROC,*) 'NAPROC=', NAPROC
!/DEBUGSTP      WRITE(740+IAPROC,*) 'NTPROC=', NTPROC
!/DEBUGSTP      FLUSH(740+IAPROC)
      IF (IAPROC .le. NAPROC) THEN
!/DEBUGSTP      WRITE(740+IAPROC,*) 'Begin WAVE_SETUP_COMPUTATION'
!/DEBUGSTP      FLUSH(740+IAPROC)
        IF (DO_WAVE_SETUP) THEN
          IF (GTYPE .EQ. UNGTYPE) THEN
            CALL TRIG_WAVE_SETUP_COMPUTATION
          ELSE
            CALL FD_WAVE_SETUP_COMPUTATION
          END IF
        END IF
      END IF
!/DEBUGSTP      WRITE(740+IAPROC,*) 'Begin WAVE_SETUP_COMPUTATION'
!/DEBUGSTP      FLUSH(740+IAPROC)
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      END MODULE
!/ ------------------------------------------------------------------- /
