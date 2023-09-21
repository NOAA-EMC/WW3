#include "w3macros.h"
!/
!/ ------------------------------------------------------------------- /
MODULE PDLIB_W3PROFSMD
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
  !  1. Purpose : PDLIB version of UGTYPE including fully implicit
  !               discretization. This works is based on the thesis
  !               of Roland, 2008 and represents the continues
  !               development of the solution of the WAE on unstructured
  !               grids. Following the quest since one decade we
  !               continuesly improve the aplicability and robustness of
  !               the source code and the methods. The development and
  !               implementation of the involved schemes was funded over
  !               the past decade by IFREMER, SHOM, USACE, NCEP/NOAA,
  !               BGS IT&E GmbH, Zanke & Partner and Roland & Partner.
  !               The PDLIB (Parallel Decomposition Library) library,
  !               which is used here is courtesy to BGS IT&E GmbH and
  !               has it's own license, which is the same as WW3. As of
  !               the origin of the methods, ideas and source code. This
  !               code was 1st developed in the WWM-III (Roland, 2008) and
  !               the ported to WW3. This is true for all source code
  !               related to UGTYPE.
  !
  !
  !  2. Method :  We apply here the framework of Residual Distributions
  !               schemes for hyperbolic problems for nonlinear propagation
  !               laws based on the work of Richiuotto et al. 2005.
  !               We supply the N-scheme, PSI-scheme and Lax-FCT-scheme
  !               as explicit methods ranging from 1st order time space
  !               to most optimal PSI method up to 2nd order Lax-FCT-scheme.
  !               For the implicit implementation we used up to now only
  !               the N-Scheme. Higher order schemes are up to now rather
  !               a research feature than for practical application. The
  !               reason is given in Cavalleri et al. 2018, we do not
  !               resolve enough physics in order to be able to run
  !               2nd or even higher order schemes.
  !               Use the numerical schemes with the needed care and
  !               do proper convergence analysis on the time step and
  !               grid size as well as solver threshold depedency.
  !               Think about the time and spatial scales of your
  !               u r intending to resolve! Multiscale modelling needs
  !               much work on the side of the modeler and much more
  !               time for the grid generation and the validation of the
  !               final model
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
  !
  use w3servmd, only : print_memcheck
#ifdef W3_S
  USE W3SERVMD, only: STRACE
#endif
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
  !/ ------------------------------------------------------------------- /
  !/
  ! module default
  implicit none

  PUBLIC
  !/
  !/ Public variables
  !/
  LOGICAL               :: MAPSTA_HACK = .FALSE.
  REAL, ALLOCATABLE     :: ASPAR_JAC(:,:), ASPAR_DIAG_SOURCES(:,:), ASPAR_DIAG_ALL(:,:), B_JAC(:,:)
  REAL, ALLOCATABLE     :: CAD_THE(:,:), CAS_SIG(:,:)
  REAL, ALLOCATABLE     :: CWNB_SIG_M2(:,:)
  REAL, ALLOCATABLE     :: U_JAC(:,:)
  REAL, ALLOCATABLE     :: COFRM4(:)
  REAL*8, ALLOCATABLE   :: FLALL1(:,:,:), KELEM1(:,:,:)
  REAL*8, ALLOCATABLE   :: FLALL2(:,:,:), KELEM2(:,:,:)
  REAL*8, ALLOCATABLE   :: FLALL3(:,:,:), KELEM3(:,:,:)
  REAL*8, ALLOCATABLE   :: NM(:,:,:), DTSI(:)
  INTEGER, ALLOCATABLE  :: ITER(:)
  INTEGER, ALLOCATABLE  :: IS0_pdlib(:)
  INTEGER               :: FreqShiftMethod = 2
  LOGICAL               :: FSGEOADVECT
  LOGICAL, SAVE         :: LINIT_OUTPUT = .TRUE.
  REAL, SAVE            :: RTIME = 0.d0
  INTEGER               :: POS_TRICK(3,2)

#ifdef W3_DEBUGSRC
  INTEGER  :: TESTNODE = 1
#endif
  integer  :: memunit
  !
  !/ ------------------------------------------------------------------- /
  !
CONTAINS
  !
  !/ ------------------------------------------------------------------- /
  !
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_INIT(IMOD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Init pdlib part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: FLCX, FLCY
    USE CONSTANTS, only : GRAV, TPI
    USE W3GDATMD, only: XGRD, YGRD, NX, NSEA, NTRI, TRIGP, NSPEC, NSEAL
    USE W3GDATMD, only: MAPSTA, MAPFS, GRIDS, NTH, SIG, NK
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3GDATMD, only: CCON, COUNTCON, INDEX_CELL, IE_CELL
    USE W3GDATMD, only: IOBP, IOBPA, IOBPD, IOBDP, SI

    USE W3ADATMD, only: MPI_COMM_WCMP, MPI_COMM_WAVE
    USE W3ODATMD, only: IAPROC, NAPROC, NTPROC
    USE yowDatapool, only: istatus
    USE yowpdlibMain, only: initFromGridDim
    USE YOWNODEPOOL, only: npa, np, iplg
    USE W3PARALL, only : PDLIB_NSEAL, PDLIB_NSEALM
    USE W3PARALL, only : JX_TO_JSEA, ISEA_TO_JSEA
    USE yowfunction, only : ComputeListNP_ListNPA_ListIPLG, pdlib_abort
    USE W3GDATMD, only: FSTOTALIMP, FSTOTALEXP, FSNIMP, FSN, FSPSI, FSFCT
    USE W3GDATMD, only: FSREFRACTION, FSFREQSHIFT, FSSOURCE

    !/
    INCLUDE "mpif.h"
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
    !!      INCLUDE "mpif.h"
    INTEGER :: istat
    INTEGER :: I, J, IBND_MAP, ISEA, IP, IX, JSEA, nb
    INTEGER :: IP_glob
    INTEGER :: myrank, ierr, iproc
    INTEGER, ALLOCATABLE :: NSEAL_arr(:)
    INTEGER :: IERR_MPI
    INTEGER :: IScal(1)
    INTEGER, INTENT(in) :: IMOD
    INTEGER :: IK, ISP
    INTEGER IK0, ISP0, ITH
    REAL :: eSIG, eFR
    REAL, PARAMETER :: COEF4 = 5.0E-7
#ifdef W3_S
    CALL STRACE (IENT, 'PDLIB_INIT')
#endif
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_INIT, IMOD (no print)'
    WRITE(740+IAPROC,*) 'NAPROC=', NAPROC
    WRITE(740+IAPROC,*) 'NTPROC=', NTPROC
    FLUSH(740+IAPROC)
#endif

    PDLIB_NSEAL = 0

    IF (IAPROC .le. NAPROC) THEN

      CALL MPI_COMM_RANK(MPI_COMM_WCMP, myrank, ierr)
      !
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'PDLIB_INIT, IAPROC=', IAPROC
      WRITE(740+IAPROC,*) 'PDLIB_INIT, NAPROC=', NAPROC
      WRITE(740+IAPROC,*) 'PDLIB_INIT, myrank=', myrank
      FLUSH(740+IAPROC)
#endif
      !
      IF (FSTOTALEXP) THEN
        CALL initFromGridDim(NX,NTRI,TRIGP,NTH,MPI_COMM_WCMP)
      ELSE
        CALL initFromGridDim(NX,NTRI,TRIGP,NSPEC,MPI_COMM_WCMP)
      ENDIF
      !
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'After initFromGridDim'
      FLUSH(740+IAPROC)
#endif
      !
      !
      ! Now the computation of NSEAL
      !
      !
      DO IP = 1, npa
        IX = iplg(IP)
        ISEA = MAPFS(1,IX)
        IF (ISEA .gt. 0) PDLIB_NSEAL = PDLIB_NSEAL + 1
      END DO
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'npa is augmented domain over NX'
      WRITE(740+IAPROC,*) 'PDLIB_NSEAL is basicall npa but only over the wet points'
      WRITE(740+IAPROC,*) 'NSEAL is set to PDLIB_NSEAL'
      WRITE(740+IAPROC,*) 'PDLIB_NSEAL=', PDLIB_NSEAL
      WRITE(740+IAPROC,*) 'NSEAL =', NSEAL, 'NP    =', NP, 'NPA   =', NPA
      FLUSH(740+IAPROC)
#endif
      ALLOCATE(JX_TO_JSEA(npa), ISEA_TO_JSEA(NSEA), stat=istat)
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'ISEA_TO_JSEA ALLOCATEd'
      FLUSH(740+IAPROC)
#endif
      if(istat /= 0) CALL PDLIB_ABORT(3)
      JSEA         = 0
      JX_TO_JSEA   = 0
      ISEA_TO_JSEA = 0
      DO IP = 1, npa
        IX = iplg(IP)
        ISEA = MAPFS(1,IX)
        IF (ISEA .gt. 0) THEN
          JSEA=JSEA+1
          JX_TO_JSEA(IP)=JSEA
          ISEA_TO_JSEA(ISEA)=JSEA
        END IF
      END DO
      !
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'After JX_TO_JSEA, ISEA_TO_JSEA and friend computation'
      FLUSH(740+IAPROC)
#endif
      !
      ! Map a point in (1:PDLIB_NSEAL) to a point in (1:NSEA)
      !
      nb=0
      DO IX=1,NX
        IF (MAPFS(1,IX) .gt. 0) nb = nb + 1
      END DO

      IF (nb .ne. NSEA) THEN
        WRITE(*,*) 'Logical error in computation of NSEA / nb'
        WRITE(*,*) 'nb=', nb, ' NSEA=', NSEA
        STOP
      END IF
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'nb / NSEA consistency check'
      FLUSH(740+IAPROC)
#endif
    END IF
    FSGEOADVECT = .FALSE.
    IF ((FLCX .eqv. .TRUE.).and.(FLCY .eqv. .TRUE.)) THEN
      FSGEOADVECT =.TRUE.
    END IF
    !
    ! Compute NSEALM
    !
    IF (IAPROC .le. NAPROC) THEN
      IF (IAPROC .eq. 1) THEN
        ALLOCATE(NSEAL_arr(NAPROC))
        NSEAL_arr(1)=PDLIB_NSEAL
        DO IPROC=2,NAPROC
          CALL MPI_RECV(IScal,1,MPI_INT, IPROC-1, 23, MPI_COMM_WAVE, istatus, IERR_MPI)
          NSEAL_arr(IPROC)=IScal(1)
        END DO
        PDLIB_NSEALM=maxval(NSEAL_arr)
        DEALLOCATE(NSEAL_arr)
      ELSE
        IScal(1)=PDLIB_NSEAL
        CALL MPI_SEND(IScal,1,MPI_INT, 0, 23, MPI_COMM_WAVE, IERR_MPI)
      END IF
    END IF
    !
    IF (IAPROC .eq. 1) THEN
      IScal(1)=PDLIB_NSEALM
      DO IPROC = 2 , NTPROC
        CALL MPI_SEND(IScal,1,MPI_INT, IPROC-1, 24, MPI_COMM_WAVE, IERR_MPI)
      END DO
    ELSE
      CALL MPI_RECV(IScal,1,MPI_INT, 0, 24, MPI_COMM_WAVE, istatus, IERR_MPI)
      PDLIB_NSEALM=IScal(1)
    END IF

#ifdef W3_DEBUGINIT
    WRITE(740+IAPROC,*) 'ALLOCATEd(ISEA_TO_JSEA)=', allocated(ISEA_TO_JSEA)
    WRITE(740+IAPROC,*) 'PDLIB_NSEALM=', PDLIB_NSEALM
    FLUSH(740+IAPROC)
#endif
    !
    CALL ComputeListNP_ListNPA_ListIPLG
    ALLOCATE(COFRM4(NK))
    DO IK=1,NK
      eSIG=SIG(IK)
      eFR=eSIG/TPI
      COFRM4(IK)=COEF4*GRAV/(eFR**4)
    END DO
    ALLOCATE(IS0_pdlib(NSPEC))
    DO ISP=1, NSPEC
      IS0_pdlib(ISP) = ISP - 1
    END DO
    DO ISP=1, NSPEC, NTH
      IS0_pdlib(ISP) = IS0_pdlib(ISP) + NTH
    END DO

    DO JSEA=1, PDLIB_NSEAL
      IP      = JSEA
      IP_glob = iplg(IP)
      ISEA    = MAPFS(1,IP_glob)
      IF (ISEA .ne. IP_glob) THEN
        WRITE(*,*) JSEA, PDLIB_NSEAL, IP, IP_glob, ISEA
        WRITE(*,*) 'ISEA .ne. IP_glob'
        CALL PDLIB_ABORT(20)
      ENDIF
    ENDDO
    !
    !
    !/
    !/ End of PDLIB_INIT ------------------------------------------- /
    !/
  END SUBROUTINE PDLIB_INIT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_MAPSTA_INIT(IMOD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Init mapsta part for pdlib
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only : INDEX_MAP, NBND_MAP, NSEA, NSEAL, MAPSTA, GRIDS, NX, NTH
    USE W3GDATMD, only : MAPSTA_LOC, NBND_MAP, INDEX_MAP
    USE W3ODATMD, only : IAPROC, NAPROC
    USE YOWNODEPOOL, only: iplg, npa
    USE yowfunction, only: pdlib_abort
    USE W3ODATMD, only: IAPROC
    !/
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
    INTEGER :: IBND_MAP, ISEA, JSEA, IX, IP, IP_glob
    INTEGER, INTENT(in) :: IMOD
    INTEGER :: Status(NX), istat
    REAL :: rtmp(nseal)
#ifdef W3_S
    CALL STRACE (IENT, 'PDLIB_MAPSTA_INIT')
#endif
#ifdef W3_DEBUGINIT
    WRITE(*,*) 'Passing by PDLIB_MAPSTA_INIT IAPROC=', IAPROC
#endif
    IF (IAPROC .gt. NAPROC) THEN
      RETURN
    END IF

    ALLOCATE(GRIDS(IMOD)%MAPSTA_LOC(npa), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(5)
    MAPSTA_LOC => GRIDS(IMOD)%MAPSTA_LOC
    NBND_MAP => GRIDS(IMOD)%NBND_MAP
    Status = 0
    DO IP=1,npa
      IP_glob=iplg(IP)
      Status(IP_glob)=IP
      MAPSTA_LOC(IP)=MAPSTA(1,IP_glob)
    END DO
    NBND_MAP = 0
    DO IX=1,NX
      IF ((MAPSTA(1,IX) .lt. 1).and.(Status(IX).gt.0)) THEN
        NBND_MAP = NBND_MAP + 1
      END IF
    END DO

    ALLOCATE(GRIDS(IMOD)%INDEX_MAP(NBND_MAP), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(6)
    INDEX_MAP => GRIDS(IMOD)%INDEX_MAP
    IBND_MAP = 0
    DO IX = 1, NX
      IF ((MAPSTA(1,IX) .lt. 1).and.(Status(IX).gt.0)) THEN
        IBND_MAP = IBND_MAP + 1
        INDEX_MAP(IBND_MAP) = Status(IX)
      END IF
    END DO
    !/
    !/ End of W3SPR4 ----------------------------------------------------- /
    !/
  END SUBROUTINE PDLIB_MAPSTA_INIT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_IOBP_INIT(IMOD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Init mapsta part for pdlib
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only : INDEX_MAP, NBND_MAP, NSEA, NSEAL, GRIDS, NX, NTH
    USE W3GDATMD, only : IOBP, IOBDP, IOBPA, IOBPD, NBND_MAP, INDEX_MAP
    USE W3GDATMD, only : IOBP_LOC, IOBPD_LOC, IOBDP_LOC, IOBPA_LOC
    USE W3ODATMD, only : IAPROC, NAPROC
    USE YOWNODEPOOL, only: iplg, npa
    USE yowfunction, only: pdlib_abort
    USE W3ODATMD, only: IAPROC
    !/
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
    INTEGER             :: IBND_MAP, ISEA, JSEA, IX, IP, IP_glob
    INTEGER, INTENT(in) :: IMOD
    INTEGER             :: Status(NX), istat
    REAL                :: rtmp(nseal)
#ifdef W3_S
    CALL STRACE (IENT, 'PDLIB_MAPSTA_INIT')
#endif
#ifdef W3_DEBUGINIT
    WRITE(*,*) 'Passing by PDLIB_MAPSTA_INIT IAPROC=', IAPROC
#endif
    IF (IAPROC .gt. NAPROC) THEN
      RETURN
    END IF

    ALLOCATE(GRIDS(IMOD)%IOBP_LOC(NPA), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(7)
    ALLOCATE(GRIDS(IMOD)%IOBPD_LOC(NTH,NPA), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(8)
    ALLOCATE(GRIDS(IMOD)%IOBDP_LOC(NPA), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(9)
    ALLOCATE(GRIDS(IMOD)%IOBPA_LOC(NPA), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(9)

    IOBP_loc  => GRIDS(IMOD)%IOBP_LOC
    IOBPA_loc => GRIDS(IMOD)%IOBPA_LOC
    IOBPD_loc => GRIDS(IMOD)%IOBPD_LOC
    IOBDP_loc => GRIDS(IMOD)%IOBDP_LOC

    DO IP = 1, npa
      IP_glob         = iplg(IP)
      IOBP_loc(IP)    = IOBP(IP_glob)
      IOBPD_loc(:,IP) = IOBPD(:,IP_glob)
    END DO

    IOBDP_loc = 0
    IOBP => NULL()
    IOBPD => NULL()
    DEALLOCATE(GRIDS(IMOD)%IOBP,GRIDS(IMOD)%IOBPD)
    CALL SET_IOBPA_PDLIB
    !/
    !/ End of W3SPR4 ----------------------------------------------------- /
    !/
  END SUBROUTINE PDLIB_IOBP_INIT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_W3XYPUG ( ISP, FACX, FACY, DTG, VGX, VGY, LCALC )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         10-Jan-2011 |
    !/                  +-----------------------------------+
    !/
    !/    10-Jan-2008 : Origination.                        ( version 3.13 )
    !/    10-Jan-2011 : Addition of implicit scheme         ( version 3.14.4 )
    !/    06-Feb-2014 : PDLIB parallelization
    !/
    !  1. Purpose : Explicit advection schemes driver
    !
    !     Propagation in physical space for a given spectral component.
    !     Gives the choice of scheme on unstructured grid
    !     Use the geographical parall algorithms for further speed.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !     Local variables.
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       W3WAVE   Wave model routine.
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  7. Remarks :
    !              make the interface between the WAVEWATCH and the WWM code.
    !
    !  8. Structure :
    !
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !
    !
    ! 10. Source code :
    !/ ------------------------------------------------------------------- /
    !/
    !
    USE CONSTANTS
    !
    USE W3TIMEMD, only: DSEC21
    !
    USE W3GDATMD, only: NX, NY, MAPFS, CLATS,        &
         FLCX, FLCY, NK, NTH, DTH, XFR,              &
         ECOS, ESIN, SIG,  PFMOVE,                   &
         IOBP, IOBPD,                                &
         FSN, FSPSI, FSFCT, FSNIMP,                  &
         GTYPE, UNGTYPE, NBND_MAP, INDEX_MAP
    USE YOWNODEPOOL, only: PDLIB_IEN, PDLIB_TRIA
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE YOWNODEPOOL, only: iplg, npa
    USE W3WDATMD, only: TIME, VA
    USE W3ODATMD, only: TBPI0, TBPIN, FLBPI
    USE W3ADATMD, only: CG, CX, CY, ITIME, DW
    USE W3IDATMD, only: FLCUR, FLLEV
    USE W3GDATMD, only: NSEAL
    USE W3ODATMD, only: IAPROC
    USE W3DISPMD, only : WAVNU_LOCAL
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: ISP
    REAL, INTENT(IN)        :: FACX, FACY, DTG, VGX, VGY
    LOGICAL, INTENT(IN)     :: LCALC
    LOGICAL                 :: SCHEME
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
    INTEGER                 :: ITH, IK, ISEA
    INTEGER                 :: I, J, IE, IBND_MAP
    INTEGER                 :: IP_glob
    REAL                    :: CCOS, CSIN, CCURX, CCURY, WN1, CG1
    REAL                    :: C(npa,2)
    REAL                    :: RD1, RD2
    !/
    !/ Automatic work arrays
    !/
    REAL                    :: VLCFLX(npa), VLCFLY(npa)
    REAL                    :: AC(npa)
    REAL                    :: AC_MAP(NBND_MAP)
    INTEGER                 :: JSEA, IP
    !/ ------------------------------------------------------------------- /
    !
    ! 1.  Preparations --------------------------------------------------- *
    ! 1.a Set constants
    !
#ifdef W3_S
    CALL STRACE (IENT, 'W3XYPUG')
#endif
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'Begin of PDLIB_W3XYPUG'
    FLUSH(740+IAPROC)
#endif
    ITH    = 1 + MOD(ISP-1,NTH)
    IK     = 1 + (ISP-1)/NTH
    CCOS   = FACX * ECOS(ITH)
    CSIN   = FACY * ESIN(ITH)
    CCURX  = FACX
    CCURY  = FACY
    !
    ! 1.b Initialize arrays
    !
    VLCFLX = 0.
    VLCFLY = 0.
    AC     = 0.
    !
    ! 2.  Calculate velocities ---------------- *
    !
    DO JSEA = 1, NSEAL
      IP      = JSEA
      IP_glob = iplg(IP)
      ISEA    = MAPFS(1,IP_glob)
#ifdef NOCGTABLE
      CALL WAVNU_LOCAL(SIG(IK),DW(ISEA),WN1,CG1)
      AC(IP)  = VA(ISP,JSEA) / CG1 * CLATS(ISEA)
      VLCFLX(IP) = CCOS * CG1 / CLATS(ISEA)
      VLCFLY(IP) = CSIN * CG1
#else
      AC(IP)  = VA(ISP,JSEA) / CG(IK,ISEA) * CLATS(ISEA)
      VLCFLX(IP) = CCOS * CG(IK,ISEA) / CLATS(ISEA)
      VLCFLY(IP) = CSIN * CG(IK,ISEA)
#endif
#ifdef W3_MGP
      VLCFLX(IP) = VLCFLX(IP) - CCURX*VGX/CLATS(ISEA)
      VLCFLY(IP) = VLCFLY(IP) - CCURY*VGY
#endif
    END DO

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'ISP=', ISP, ' ITH=', ITH, ' IK=', IK
    WRITE(740+IAPROC,*) '1: maxval(VLCFLX)=', maxval(VLCFLX)
    WRITE(740+IAPROC,*) '1: maxval(VLCFLY)=', maxval(VLCFLY)
    WRITE(740+IAPROC,*) 'FLCUR=', FLCUR
    FLUSH(740+IAPROC)
#endif
    IF ( FLCUR ) THEN
      DO JSEA=1, NSEAL
        IP      = JSEA
        IP_glob = iplg(IP)
        ISEA    = MAPFS(1,IP_glob)
        !
        ! Currents are not included on coastal boundaries (COUNTSEACON(IXY) .NE. PDLIB_CCON(IXY))
        !
        IF (IOBP_LOC(IP) .GT. 0) THEN
          VLCFLX(IP) = VLCFLX(IP) + CCURX*CX(ISEA)/CLATS(ISEA)
          VLCFLY(IP) = VLCFLY(IP) + CCURY*CY(ISEA)
        END IF
      END DO
    END IF

    C(:,1) = VLCFLX(:) * IOBDP_LOC
    C(:,2) = VLCFLY(:) * IOBDP_LOC
    !
    ! 4. Prepares boundary update
    !
    IF ( FLBPI ) THEN
      RD1    = DSEC21 ( TBPI0, TIME )
      RD2    = DSEC21 ( TBPI0, TBPIN )
    ELSE
      RD1=1.
      RD2=0.
    END IF
    !
    ! Saving data for MAPSTA business
    !
    IF (MAPSTA_HACK) THEN
      DO IBND_MAP=1,NBND_MAP
        IP=INDEX_MAP(IBND_MAP)
        AC_MAP(IBND_MAP) = AC(IP)
      END DO
    END IF
    !
    ! 4. propagate using the selected scheme
    !
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'maxval(C)=', maxval(C)
    FLUSH(740+IAPROC)
#endif
    IF (FSN) THEN
      CALL PDLIB_W3XYPFSN2(ISP, C, LCALC, RD1, RD2, DTG, AC)
    ELSE IF (FSPSI) THEN
      CALL PDLIB_W3XYPFSPSI2(ISP, C, LCALC, RD1, RD2, DTG, AC)
    ELSE IF (FSFCT) THEN
      CALL PDLIB_W3XYPFSFCT2(ISP, C, LCALC, RD1, RD2, DTG, AC)
    ELSE IF (FSNIMP) THEN
      STOP 'For PDLIB and FSNIMP, no function has been programmed yet'
    ENDIF
    !
    IF (MAPSTA_HACK) THEN
      DO IBND_MAP=1,NBND_MAP
        IP=INDEX_MAP(IBND_MAP)
        AC(IP) = AC_MAP(IBND_MAP)
      END DO
    END IF
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'After solutioning'
    FLUSH(740+IAPROC)
#endif

    ! 6.  Store results in VQ in proper format --------------------------- *
    !
    DO JSEA=1, NSEAL
      IP      = JSEA
      IP_glob = iplg(IP)
      ISEA=MAPFS(1,IP_glob)
      VA(ISP,JSEA) = MAX ( 0. , CG(IK,ISEA)/CLATS(ISEA)*AC(IP) )
    END DO
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'Leaving PDLIB_W3XYPUG'
    FLUSH(740+IAPROC)
#endif
    !/
    !/ End of W3SPR4 ----------------------------------------------------- /
    !/
  END SUBROUTINE PDLIB_W3XYPUG
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_W3XYPFSN2(ISP, C, LCALC, RD10, RD20, DT, AC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Explicit N-Scheme
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: NK, NTH, NX,  IEN, CLATS, MAPSF
    USE W3GDATMD, only: IOBPD_LOC, IOBP_LOC, IOBDP_LOC, IOBPA_LOC, FSBCCFL
    USE W3WDATMD, only: TIME
    USE W3ADATMD, only: CG, ITER, DW , CFLXYMAX, NSEALM
    USE W3ODATMD, only: NDSE, NDST, FLBPI, NBI, TBPIN, ISBPI, BBPI0, BBPIN
    USE W3TIMEMD, only: DSEC21
    USE W3ADATMD, only: MPI_COMM_WCMP
    USE W3GDATMD, only: NSEAL, DMIN, NSEA
#ifdef W3_REF1
    USE W3GDATMD, only: REFPARS
#endif
    USE YOWNODEPOOL,    only: PDLIB_SI, PDLIB_IEN, PDLIB_TRIA, ipgl, iplg, npa, np
    use yowElementpool, only: ne, INE
    use yowDatapool, only: rtype
    use yowExchangeModule, only : PDLIB_exchange1DREAL
    USE W3ODATMD, only : IAPROC
    USE MPI, only : MPI_MIN
    USE W3PARALL, only : INIT_GET_JSEA_ISPROC
    USE W3PARALL, only : ONESIXTH, ZERO, THR
    USE yowRankModule, only : IPGL_npa

    INTEGER, INTENT(IN)    :: ISP        ! Actual Frequency/Wavenumber,
                                         ! actual Wave Direction
    REAL,    INTENT(IN)    :: DT         ! Time intervall for which the
                                         ! advection should be computed
                                         ! for the given velocity field
    REAL,    INTENT(IN)    :: C(npa,2)   ! Velocity field in it's
                                         ! X- and Y- Components,
    REAL,    INTENT(INOUT) :: AC(npa)    ! Wave Action before and
                                         ! after advection
    REAL,    INTENT(IN)    :: RD10, RD20 ! Time interpolation
                                         ! coefficients for boundary
                                         ! conditions
    LOGICAL, INTENT(IN)    :: LCALC      ! Switch for the calculation of
                                         ! the max. Global Time step
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_REF1
    INTEGER(KIND=1)    :: IOBPDR(NX)
#endif
    INTEGER :: IP, IE, POS, IT, I1, I2, I3, I, J, ITH, IK
    INTEGER :: IBI, NI(3)
    INTEGER :: JX
    !
    ! local REAL
    !
    REAL    :: RD1, RD2
    !:
    ! local double
    !
    REAL  :: UTILDE
    REAL  :: SUMTHETA
    REAL  :: FT, CFLXY
    REAL  :: FL11, FL12, FL21, FL22, FL31, FL32
    REAL  :: FL111, FL112, FL211, FL212, FL311, FL312
    REAL  :: DTSI(npa), U(npa)
    REAL  :: DTMAX_GL, DTMAX, DTMAXEXP, REST
    REAL  :: LAMBDA(2), KTMP(3)
    REAL  :: KELEM(3,NE), FLALL(3,NE)
    REAL  :: KKSUM(npa), ST(npa)
    REAL  :: NM(NE)
    INTEGER :: ISPROC, JSEA, IP_glob, ierr, IX
    REAL  :: eSumAC, sumAC, sumBPI0, sumBPIN, sumCG, sumCLATS
    LOGICAL :: testWrite
    REAL  :: FIN(1), FOUT(1)
#ifdef W3_S
    CALL STRACE (IENT, 'W3XYPFSN')
#endif
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 1'
    FLUSH(740+IAPROC)
    CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC in input")
#endif

    ITH    = 1 + MOD(ISP-1,NTH)
    IK     = 1 + (ISP-1)/NTH
    DTMAX  = DBLE(10.E10)

    !
#ifdef W3_REF1
    IOBPDR(:)=(1-IOBP_LOC(:))*(1-IOBPD_LOC(ITH,:))
#endif

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'NX=', NX
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 2'
    FLUSH(740+IAPROC)
#endif
    !
    !2       Propagation
    !2.a     Calculate K-Values and contour based quantities ...
    !
    DO IE = 1, ne
      I1 = INE(1,IE)
      I2 = INE(2,IE)
      I3 = INE(3,IE)
      LAMBDA(1) = ONESIXTH *(C(I1,1)+C(I2,1)+C(I3,1)) ! Linearized advection speed in X and Y direction
      LAMBDA(2) = ONESIXTH *(C(I1,2)+C(I2,2)+C(I3,2))
      KELEM(1,IE) = LAMBDA(1) * PDLIB_IEN(1,IE) + LAMBDA(2) * PDLIB_IEN(2,IE) ! K-Values - so called Flux Jacobians
      KELEM(2,IE) = LAMBDA(1) * PDLIB_IEN(3,IE) + LAMBDA(2) * PDLIB_IEN(4,IE)
      KELEM(3,IE) = LAMBDA(1) * PDLIB_IEN(5,IE) + LAMBDA(2) * PDLIB_IEN(6,IE)
      KTMP        = KELEM(:,IE) ! Copy
      NM(IE)      = - 1.D0/MIN(-THR,SUM(MIN(ZERO,KTMP))) ! N-Values
      KELEM(:,IE) = MAX(ZERO,KTMP)
      FL11  = C(I2,1) * PDLIB_IEN(1,IE) + C(I2,2) * PDLIB_IEN(2,IE) ! Weights for Simpson Integration
      FL12  = C(I3,1) * PDLIB_IEN(1,IE) + C(I3,2) * PDLIB_IEN(2,IE)
      FL21  = C(I3,1) * PDLIB_IEN(3,IE) + C(I3,2) * PDLIB_IEN(4,IE)
      FL22  = C(I1,1) * PDLIB_IEN(3,IE) + C(I1,2) * PDLIB_IEN(4,IE)
      FL31  = C(I1,1) * PDLIB_IEN(5,IE) + C(I1,2) * PDLIB_IEN(6,IE)
      FL32  = C(I2,1) * PDLIB_IEN(5,IE) + C(I2,2) * PDLIB_IEN(6,IE)
      FL111 = 2.d0*FL11+FL12
      FL112 = 2.d0*FL12+FL11
      FL211 = 2.d0*FL21+FL22
      FL212 = 2.d0*FL22+FL21
      FL311 = 2.d0*FL31+FL32
      FL312 = 2.d0*FL32+FL31
      FLALL(1,IE) = (FL311 + FL212) * ONESIXTH + KELEM(1,IE)
      FLALL(2,IE) = (FL111 + FL312) * ONESIXTH + KELEM(2,IE)
      FLALL(3,IE) = (FL211 + FL112) * ONESIXTH + KELEM(3,IE)
    END DO
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 3'
    FLUSH(740+IAPROC)
#endif
    IF (LCALC) THEN
      KKSUM = ZERO
      DO IE = 1, NE
        NI = INE(:,IE)
        KKSUM(NI) = KKSUM(NI) + KELEM(:,IE)
      END DO
      DTMAXEXP = 1.E10
      DO IP = 1, np
        IP_glob      = iplg(IP)
        IF (IOBP_LOC(IP) .EQ. 1 .OR. FSBCCFL) THEN
          DTMAXEXP     = PDLIB_SI(IP)/MAX(DBLE(10.E-10),KKSUM(IP)*IOBDP_LOC(IP))
          DTMAX        = MIN( DTMAX, DTMAXEXP)
        ENDIF
        CFLXYMAX(IP) = MAX(CFLXYMAX(IP),DBLE(DT)/DTMAXEXP)
      END DO
      FIN(1)=DTMAX
      CALL MPI_ALLREDUCE(FIN,FOUT,1,rtype,MPI_MIN,MPI_COMM_WCMP,ierr)
      DTMAX_GL=FOUT(1)
      CFLXY = DBLE(DT)/DTMAX_GL
      REST  = ABS(MOD(CFLXY,1.0d0))
      IF (REST .LT. THR) THEN
        ITER(IK,ITH) = ABS(NINT(CFLXY))
      ELSE IF (REST .GT. THR .AND. REST .LT. 0.5d0) THEN
        ITER(IK,ITH) = ABS(NINT(CFLXY)) + 1
      ELSE
        ITER(IK,ITH) = ABS(NINT(CFLXY))
      END IF
    END IF ! LCALC

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 4'
    FLUSH(740+IAPROC)
#endif
    DO IP = 1, npa
      DTSI(IP) = DBLE(DT)/DBLE(ITER(IK,ITH))/PDLIB_SI(IP) ! Some precalculations for the time integration.
    END DO
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 4.1'
    FLUSH(740+IAPROC)
    CALL SCAL_INTEGRAL_PRINT_R4(PDLIB_SI, "PDLIB_SI in input")
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 4.2'
    FLUSH(740+IAPROC)
    CALL SCAL_INTEGRAL_PRINT_R4(DTSI, "DTSI in input")
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 5'
    WRITE(740+IAPROC,*) 'IK=', IK, ' ITH=', ITH
    WRITE(740+IAPROC,*) 'ITER=', ITER(IK,ITH)
    FLUSH(740+IAPROC)
#endif


    DO IT = 1, ITER(IK,ITH)
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'IK=', IK, ' ITH=', ITH
      WRITE(740+IAPROC,*) 'IT=', IT, ' ITER=', ITER(IK,ITH)
      FLUSH(740+IAPROC)
      IF (testWrite) THEN
        WRITE(740+IAPROC,*) 'IT=', IT
        FLUSH(740+IAPROC)
      END IF
#endif
      U = DBLE(AC)
      ST = ZERO
      DO IE = 1, NE
        NI     = INE(:,IE)
        UTILDE = NM(IE) * (DOT_PRODUCT(FLALL(:,IE),U(NI)))
        ST(NI) = ST(NI) + KELEM(:,IE) * (U(NI) - UTILDE) ! the 2nd term are the theta values of each node ...
      END DO ! IE
#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(ST, "ST in loop")
      END IF
#endif
      !
      ! IOBPD=0  : waves coming from land
      ! IOBPD=1 : waves coming from the coast
      !
      DO IP = 1, npa
        U(IP) = MAX(ZERO,U(IP)-DTSI(IP)*ST(IP)*(1-IOBPA_LOC(IP)))*DBLE(IOBPD_LOC(ITH,IP))*IOBDP_LOC(IP)
#ifdef W3_REF1
        IF (REFPARS(3).LT.0.5.AND.IOBPD_LOC(ITH,IP).EQ.0.AND.IOBPA_LOC(IP).EQ.0) U(IP) = AC(IP) ! restores reflected boundary values
#endif
      END DO
#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(U, "U in loop")
      END IF
#endif
      AC = REAL(U)

#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC in loop")
      END IF
#endif
      !
      ! 5 Update boundaries ... would be better to omit any if clause in this loop ...
      !   a possibility would be to use NBI = 0 when FLBPI is FALSE and loop on IBI whatever the value of NBI
      !
      IF ( FLBPI ) THEN
        RD1=RD10 - DT * REAL(ITER(IK,ITH)-IT)/REAL(ITER(IK,ITH))
        RD2=RD20
        IF ( RD2 .GT. 0.001 ) THEN
          RD2    = MIN(1.,MAX(0.,RD1/RD2))
          RD1    = 1. - RD2
        ELSE
          RD1    = 0.
          RD2    = 1.
        END IF
#ifdef W3_DEBUGSOLVER
        sumAC=0
        sumBPI0=0
        sumBPIN=0
        sumCG=0
        sumCLATS=0
#endif
        DO IBI = 1, NBI
          IP_glob = MAPSF(ISBPI(IBI),1)
          JX=IPGL_npa(IP_glob)
          IF (JX .gt. 0) THEN
            AC(JX) = ( RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI) )   &
                 / CG(IK,ISBPI(IBI)) * CLATS(ISBPI(IBI))
#ifdef W3_DEBUGSOLVER
            sumAC=sumAC + AC(JX)
            sumBPI0=sumBPI0 + BBPI0(ISP,IBI)
            sumBPIN=sumBPIN + BBPIN(ISP,IBI)
            sumCG=sumCG + CG(IK,ISBPI(IBI))
            sumCLATS=sumCLATS + CLATS(ISBPI(IBI))
#endif
          END IF
        END DO
      END IF

#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'NBI=', NBI
      WRITE(740+IAPROC,*) 'RD1=', RD1, ' RD2=', RD2
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumAC=', sumAC
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPI0=', sumBPI0
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPIN=', sumBPIN
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCG=', sumCG
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCLATS=', sumCLATS
      FLUSH(740+IAPROC)
#endif
      CALL PDLIB_exchange1DREAL(AC)

#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC after FLBPI")
      END IF
#endif
    END DO
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 6'
    FLUSH(740+IAPROC)
#endif
  END SUBROUTINE PDLIB_W3XYPFSN2
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_W3XYPFSPSI2 ( ISP, C, LCALC, RD10, RD20, DT, AC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Explicit PSI-Scheme
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


    USE W3GDATMD, only: NK, NTH, NX,  IEN, CLATS, MAPSF
    USE W3GDATMD, only: IOBPD_LOC, IOBP_LOC, IOBDP_LOC, IOBPA_LOC, FSBCCFL
    USE W3WDATMD, only: TIME
    USE W3ADATMD, only: CG, ITER, DW , CFLXYMAX, NSEALM
    USE W3ODATMD, only: NDSE, NDST, FLBPI, NBI, TBPIN, ISBPI, BBPI0, BBPIN
    USE W3TIMEMD, only: DSEC21
    USE W3ADATMD, only: MPI_COMM_WCMP
    USE W3GDATMD, only: NSEAL, DMIN, NSEA
#ifdef W3_REF1
    USE W3GDATMD, only: REFPARS
#endif
    USE YOWNODEPOOL,    only: PDLIB_SI, PDLIB_IEN, PDLIB_TRIA, ipgl, iplg, npa, np
    use yowElementpool, only: ne, INE
    use yowDatapool, only: rtype
    use yowExchangeModule, only : PDLIB_exchange1DREAL
    USE W3ODATMD, only : IAPROC
    USE MPI, only : MPI_MIN
    USE W3PARALL, only : INIT_GET_JSEA_ISPROC
    USE W3PARALL, only : ONESIXTH, ZERO, THR
    USE yowRankModule, only : IPGL_npa
    IMPLICIT NONE
    INTEGER, INTENT(IN)    :: ISP        ! Actual Frequency/Wavenumber,
                                         ! actual Wave Direction
    REAL,    INTENT(IN)    :: DT         ! Time intervall for which the
                                         ! advection should be computed
                                         ! for the given velocity field
    REAL,    INTENT(IN)    :: C(npa,2)   ! Velocity field in it's
                                         ! X- and Y- Components,
    REAL,    INTENT(INOUT) :: AC(npa)    ! Wave Action before and
                                         ! after advection
    REAL,    INTENT(IN)    :: RD10, RD20 ! Time interpolation
                                         ! coefficients for boundary
                                         ! conditions
    LOGICAL, INTENT(IN)    :: LCALC      ! Switch for the calculation of
                                         ! the max. Global Time step
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_REF1
    INTEGER(KIND=1)    :: IOBPDR(NX)
#endif
    INTEGER :: IP, IE, POS, IT, I1, I2, I3, I, J, ITH, IK
    INTEGER :: IBI, NI(3), JX
    INTEGER :: ISPROC, IP_glob, JSEA, ierr
    REAL    :: RD1, RD2
    REAL  :: UTILDE
    REAL  :: SUMTHETA
    REAL  :: FL1, FL2, FL3
    REAL  :: FT, CFLXY
    REAL  :: FL11, FL12, FL21, FL22, FL31, FL32
    REAL  :: FL111, FL112, FL211, FL212, FL311, FL312
    REAL  :: DTSI(npa), U(npa)
    REAL  :: DTMAX, DTMAX_GL, DTMAXEXP, REST
    REAL  :: LAMBDA(2), KTMP(3), TMP(3)
    REAL  :: THETA_L(3), BET1(3), BETAHAT(3)
    REAL  :: KELEM(3,NE), FLALL(3,NE)
    REAL  :: KKSUM(npa), ST(npa)
    REAL  :: NM(NE), FIN(1), FOUT(1)
#ifdef W3_S
    CALL STRACE (IENT, 'W3XYPFSN')
#endif
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 1'
    FLUSH(740+IAPROC)
    CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC in input")
#endif

    ITH    = 1 + MOD(ISP-1,NTH)
    IK     = 1 + (ISP-1)/NTH
    DTMAX  = DBLE(10.E10)
    !
#ifdef W3_REF1
    IOBPDR(:)=(1-IOBP_LOC(:))*(1-IOBPD_LOC(ITH,:))
#endif

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'NX=', NX
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 2'
    FLUSH(740+IAPROC)
#endif
    !
    !2       Propagation
    !2.a     Calculate K-Values and contour based quantities ...
    !

    DO IE = 1, NE
      I1 = INE(1,IE)
      I2 = INE(2,IE)
      I3 = INE(3,IE)
      LAMBDA(1) = ONESIXTH *(C(I1,1)+C(I2,1)+C(I3,1)) ! Linearized advection speed in X and Y direction
      LAMBDA(2) = ONESIXTH *(C(I1,2)+C(I2,2)+C(I3,2))
      KELEM(1,IE) = LAMBDA(1) * PDLIB_IEN(1,IE) + LAMBDA(2) * PDLIB_IEN(2,IE) ! K-Values - so called Flux Jacobians
      KELEM(2,IE) = LAMBDA(1) * PDLIB_IEN(3,IE) + LAMBDA(2) * PDLIB_IEN(4,IE)
      KELEM(3,IE) = LAMBDA(1) * PDLIB_IEN(5,IE) + LAMBDA(2) * PDLIB_IEN(6,IE)
      KTMP        = KELEM(:,IE) ! Copy
      NM(IE)      = - 1.D0/MIN(-THR,SUM(MIN(ZERO,KTMP))) ! N-Values
      KELEM(:,IE) = MAX(ZERO,KTMP)
      FL11  = C(I2,1) * PDLIB_IEN(1,IE) + C(I2,2) * PDLIB_IEN(2,IE) ! Weights for Simpson Integration
      FL12  = C(I3,1) * PDLIB_IEN(1,IE) + C(I3,2) * PDLIB_IEN(2,IE)
      FL21  = C(I3,1) * PDLIB_IEN(3,IE) + C(I3,2) * PDLIB_IEN(4,IE)
      FL22  = C(I1,1) * PDLIB_IEN(3,IE) + C(I1,2) * PDLIB_IEN(4,IE)
      FL31  = C(I1,1) * PDLIB_IEN(5,IE) + C(I1,2) * PDLIB_IEN(6,IE)
      FL32  = C(I2,1) * PDLIB_IEN(5,IE) + C(I2,2) * PDLIB_IEN(6,IE)
      FL111 = 2.d0*FL11+FL12
      FL112 = 2.d0*FL12+FL11
      FL211 = 2.d0*FL21+FL22
      FL212 = 2.d0*FL22+FL21
      FL311 = 2.d0*FL31+FL32
      FL312 = 2.d0*FL32+FL31
      FLALL(1,IE) = (FL311 + FL212)! * ONESIXTH + KELEM(1,IE)
      FLALL(2,IE) = (FL111 + FL312)! * ONESIXTH + KELEM(2,IE)
      FLALL(3,IE) = (FL211 + FL112)! * ONESIXTH + KELEM(3,IE)
    END DO

    IF (LCALC) THEN
      KKSUM = ZERO
      DO IE = 1, NE
        NI = INE(:,IE)
        KKSUM(NI) = KKSUM(NI) + KELEM(:,IE)
      END DO
      DTMAXEXP = 1.E10
      DO IP = 1, npa
        IP_glob      = iplg(IP)
        IF (IOBP_LOC(IP) .EQ. 1 .OR. FSBCCFL) THEN
          DTMAXEXP     = PDLIB_SI(IP)/MAX(DBLE(10.E-10),KKSUM(IP)*IOBDP_LOC(IP))
          DTMAX        = MIN( DTMAX, DTMAXEXP)
        ENDIF
        CFLXYMAX(IP) = MAX(CFLXYMAX(IP),DBLE(DT)/DTMAXEXP)
      END DO
      FIN(1)=DTMAX
      CALL MPI_ALLREDUCE(FIN,FOUT,1,rtype,MPI_MIN,MPI_COMM_WCMP,ierr)
      DTMAX_GL=FOUT(1)
      CFLXY = DBLE(DT)/DTMAX_GL
      REST  = ABS(MOD(CFLXY,1.0d0))
      IF (REST .LT. THR) THEN
        ITER(IK,ITH) = ABS(NINT(CFLXY))
      ELSE IF (REST .GT. THR .AND. REST .LT. 0.5d0) THEN
        ITER(IK,ITH) = ABS(NINT(CFLXY)) + 1
      ELSE
        ITER(IK,ITH) = ABS(NINT(CFLXY))
      END IF
    END IF ! LCALC

    DO IP = 1, npa
      DTSI(IP) = DBLE(DT)/DBLE(ITER(IK,ITH))/PDLIB_SI(IP) ! Some precalculations for the time integration.
    END DO

    DO IT = 1, ITER(IK,ITH)

      U  = DBLE(AC)
      ST = ZERO

      DO IE = 1, NE
        NI   =  INE(:,IE)
        FT   = - ONESIXTH*DOT_PRODUCT(U(NI),FLALL(:,IE))
        UTILDE = NM(IE) * ( DOT_PRODUCT(KELEM(:,IE),U(NI)) - FT )
        THETA_L(:) = KELEM(:,IE) * (U(NI) - UTILDE)
        IF (ABS(FT) .GT. 0.0d0) THEN
          BET1(:) = THETA_L(:)/FT
          IF (ANY( BET1 .LT. 0.0d0) ) THEN
            BETAHAT(1)    = BET1(1) + 0.5d0 * BET1(2)
            BETAHAT(2)    = BET1(2) + 0.5d0 * BET1(3)
            BETAHAT(3)    = BET1(3) + 0.5d0 * BET1(1)
            BET1(1)       = MAX(ZERO,MIN(BETAHAT(1),1.d0-BETAHAT(2),1.d0))
            BET1(2)       = MAX(ZERO,MIN(BETAHAT(2),1.d0-BETAHAT(3),1.d0))
            BET1(3)       = MAX(ZERO,MIN(BETAHAT(3),1.d0-BETAHAT(1),1.d0))
            THETA_L(:) = FT * BET1
          END IF
        ELSE
          THETA_L(:) = ZERO
        END IF
        ST(NI) = ST(NI) + THETA_L ! the 2nd term are the theta values of each node ...
      END DO

#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(ST, "ST in loop")
      END IF
#endif
      !
      DO IP = 1, npa
        U(IP) = MAX(ZERO,U(IP)-DTSI(IP)*ST(IP)*(1-IOBPA_LOC(IP)))*IOBPD_LOC(ITH,IP)*IOBDP_LOC(IP)
#ifdef W3_REF1
        IF (REFPARS(3).LT.0.5.AND.IOBPD_LOC(ITH,IP).EQ.0.AND.IOBPA_LOC(IP).EQ.0) U(IP) = AC(IP) ! restores reflected boundary values
#endif
      END DO
      AC = REAL(U)
      !
      ! 5 Update boundaries ... this should be implemented differently ... it is better to omit any if clause in this loop ...
      !
      IF ( FLBPI ) THEN
        RD1=RD10 - DT * REAL(ITER(IK,ITH)-IT)/REAL(ITER(IK,ITH))
        RD2=RD20
        IF ( RD2 .GT. 0.001 ) THEN
          RD2    = MIN(1.,MAX(0.,RD1/RD2))
          RD1    = 1. - RD2
        ELSE
          RD1    = 0.
          RD2    = 1.
        END IF
        !
        ! NB: this treatment of the open boundary (time interpolation) is different from
        ! the constant boundary in the structured grids ... which restores the boundary
        ! to the initial value: IF ( MAPSTA(IXY).EQ.2 ) VQ(IXY) = AQ(IXY)
        ! Why this difference ?
        !
        DO IBI=1, NBI
          IP_glob    = MAPSF(ISBPI(IBI),1)
          JX=IPGL_npa(IP_glob)
          IF (JX .gt. 0) THEN
            AC(JX) = ( RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI) )   &
                 / CG(IK,ISBPI(IBI)) * CLATS(ISBPI(IBI))
#ifdef W3_DEBUGSOLVER
            sumAC=sumAC + AC(JX)
            sumBPI0=sumBPI0 + BBPI0(ISP,IBI)
            sumBPIN=sumBPIN + BBPIN(ISP,IBI)
            sumCG=sumCG + CG(IK,ISBPI(IBI))
            sumCLATS=sumCLATS + CLATS(ISBPI(IBI))
#endif
          END IF
        ENDDO
      END IF

#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'NBI=', NBI
      WRITE(740+IAPROC,*) 'RD1=', RD1, ' RD2=', RD2
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumAC=', sumAC
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPI0=', sumBPI0
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPIN=', sumBPIN
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCG=', sumCG
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCLATS=', sumCLATS
      FLUSH(740+IAPROC)
#endif
      CALL PDLIB_exchange1DREAL(AC)

#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC after FLBPI")
      END IF
#endif
    END DO ! IT

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 6'
    FLUSH(740+IAPROC)
#endif

  END SUBROUTINE PDLIB_W3XYPFSPSI2
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_W3XYPFSFCT2 ( ISP, C, LCALC, RD10, RD20, DT, AC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Explicit PSI-Scheme
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


    USE W3GDATMD, only: NK, NTH, NX,  IEN, CLATS, MAPSF
    USE W3GDATMD, only: IOBPD_LOC, IOBP_LOC, IOBDP_LOC, IOBPA_LOC, FSBCCFL
    USE W3WDATMD, only: TIME
    USE W3ADATMD, only: CG, ITER, DW , CFLXYMAX, NSEALM
    USE W3ODATMD, only: NDSE, NDST, FLBPI, NBI, TBPIN, ISBPI, BBPI0, BBPIN
    USE W3TIMEMD, only: DSEC21
    USE W3ADATMD, only: MPI_COMM_WCMP
    USE W3GDATMD, only: NSEAL, DMIN, NSEA
#ifdef W3_REF1
    USE W3GDATMD, only: REFPARS
#endif
    USE YOWNODEPOOL,    only: PDLIB_SI, PDLIB_IEN, PDLIB_TRIA, PDLIB_CCON, PDLIB_IE_CELL2, ipgl, iplg, npa, np
    use yowElementpool, only: ne, INE
    use yowDatapool, only: rtype
    use yowExchangeModule, only : PDLIB_exchange1DREAL
    USE W3ODATMD, only : IAPROC
    USE MPI, only : MPI_MIN
    USE W3PARALL, only : INIT_GET_JSEA_ISPROC
    USE W3PARALL, only : ONESIXTH, ZERO, THR
    USE yowRankModule, only : IPGL_npa

    IMPLICIT NONE
    INTEGER, INTENT(IN)    :: ISP        ! Actual Frequency/Wavenumber,
                                         ! actual Wave Direction
    REAL,    INTENT(IN)    :: DT         ! Time intervall for which the
                                         ! advection should be computed
                                         ! for the given velocity field
    REAL,    INTENT(IN)    :: C(npa,2)   ! Velocity field in it's
                                         ! X- and Y- Components,
    REAL,    INTENT(INOUT) :: AC(npa)    ! Wave Action before and
                                         ! after advection
    REAL,    INTENT(IN)    :: RD10, RD20 ! Time interpolation
                                         ! coefficients for boundary
                                         ! conditions
    LOGICAL, INTENT(IN)    :: LCALC      ! Switch for the calculation of
                                         ! the max. Global Time step
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_REF1
    INTEGER(KIND=1)    :: IOBPDR(NX)
#endif
    INTEGER :: IP, IE, POS, IT, I1, I2, I3, I, J, ITH, IK
    INTEGER :: IBI, NI(3)
    INTEGER :: JX
    !
    ! local REAL
    !
    REAL    :: RD1, RD2
    !:
    ! local double
    !
    REAL  :: SUMTHETA, CFLXY
    REAL*8  :: FT, UTILDE
    REAL*8  :: FL11, FL12, FL21, FL22, FL31, FL32
    REAL*8  :: FL111, FL112, FL211, FL212, FL311, FL312
    REAL  :: DTSI(npa), U(npa), UL(npa)
    REAL  :: DTMAX_GL, DTMAX, DTMAXEXP, REST
    REAL*8  :: LAMBDA(2), KTMP(3)
    REAL*8  :: KELEM(3,NE), FLALL(3,NE)
    REAL*8  :: KKSUM(npa), ST(npa)
    REAL*8  :: NM(NE), BET1(3), BETAHAT(3), TMP(3), TMP1
    INTEGER :: ISPROC, JSEA, IP_glob, ierr, IX
    REAL  :: eSumAC, sumAC, sumBPI0, sumBPIN, sumCG, sumCLATS
    LOGICAL :: testWrite
    REAL  :: FIN(1), FOUT(1)
    REAL  :: UIP(NE), UIPIP(NPA), UIMIP(NPA), U3(3)
    REAL*8 :: THETA_H(3), THETA_ACE(3,NE), THETA_L(3,NE)
    REAL*8 :: PM(NPA), PP(NPA), UIM(NE), WII(2,NPA)
    REAL   :: USTARI(2,NPA)

#ifdef W3_S
    CALL STRACE (IENT, 'W3XYPFSN')
#endif
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 1'
    FLUSH(740+IAPROC)
    CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC in input")
#endif

    ITH    = 1 + MOD(ISP-1,NTH)
    IK     = 1 + (ISP-1)/NTH
    DTMAX  = DBLE(10.E10)
    !
#ifdef W3_REF1
    IOBPDR(:)=(1-IOBP_LOC(:))*(1-IOBPD_LOC(ITH,:))
#endif

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'NX=', NX
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 2'
    FLUSH(740+IAPROC)
#endif
    !
    !2       Propagation
    !2.a     Calculate K-Values and contour based quantities ...
    !

    DO IE = 1, ne
      I1 = INE(1,IE)
      I2 = INE(2,IE)
      I3 = INE(3,IE)
      LAMBDA(1) = ONESIXTH *(C(I1,1)+C(I2,1)+C(I3,1)) ! Linearized advection speed in X and Y direction
      LAMBDA(2) = ONESIXTH *(C(I1,2)+C(I2,2)+C(I3,2))
      KELEM(1,IE) = LAMBDA(1) * PDLIB_IEN(1,IE) + LAMBDA(2) * PDLIB_IEN(2,IE) ! K-Values - so called Flux Jacobians
      KELEM(2,IE) = LAMBDA(1) * PDLIB_IEN(3,IE) + LAMBDA(2) * PDLIB_IEN(4,IE)
      KELEM(3,IE) = LAMBDA(1) * PDLIB_IEN(5,IE) + LAMBDA(2) * PDLIB_IEN(6,IE)
      KTMP        = KELEM(:,IE) ! Copy
      NM(IE)      = - 1.D0/MIN(-THR,SUM(MIN(ZERO,KTMP))) ! N-Values
      KELEM(:,IE) = MAX(ZERO,KTMP)
      FL11  = C(I2,1) * PDLIB_IEN(1,IE) + C(I2,2) * PDLIB_IEN(2,IE) ! Weights for Simpson Integration
      FL12  = C(I3,1) * PDLIB_IEN(1,IE) + C(I3,2) * PDLIB_IEN(2,IE)
      FL21  = C(I3,1) * PDLIB_IEN(3,IE) + C(I3,2) * PDLIB_IEN(4,IE)
      FL22  = C(I1,1) * PDLIB_IEN(3,IE) + C(I1,2) * PDLIB_IEN(4,IE)
      FL31  = C(I1,1) * PDLIB_IEN(5,IE) + C(I1,2) * PDLIB_IEN(6,IE)
      FL32  = C(I2,1) * PDLIB_IEN(5,IE) + C(I2,2) * PDLIB_IEN(6,IE)
      FL111 = 2.d0*FL11+FL12
      FL112 = 2.d0*FL12+FL11
      FL211 = 2.d0*FL21+FL22
      FL212 = 2.d0*FL22+FL21
      FL311 = 2.d0*FL31+FL32
      FL312 = 2.d0*FL32+FL31
      FLALL(1,IE) = (FL311 + FL212)! * ONESIXTH + KELEM(1,IE)
      FLALL(2,IE) = (FL111 + FL312)! * ONESIXTH + KELEM(2,IE)
      FLALL(3,IE) = (FL211 + FL112)! * ONESIXTH + KELEM(3,IE)
    END DO

    IF (LCALC) THEN
      KKSUM = ZERO
      DO IE = 1, NE
        NI = INE(:,IE)
        KKSUM(NI) = KKSUM(NI) + KELEM(:,IE)
      END DO
      DTMAXEXP = 1.E10
      DO IP = 1, np
        IP_glob      = iplg(IP)
        IF (IOBP_LOC(IP) .EQ. 1 .OR. FSBCCFL) THEN
          DTMAXEXP     = PDLIB_SI(IP)/MAX(DBLE(10.E-10),KKSUM(IP)*IOBDP_LOC(IP))
          DTMAX        = MIN( DTMAX, DTMAXEXP)
        ENDIF
        CFLXYMAX(IP) = MAX(CFLXYMAX(IP),DBLE(DT)/DTMAXEXP)
      END DO
      FIN(1)=DTMAX
      CALL MPI_ALLREDUCE(FIN,FOUT,1,rtype,MPI_MIN,MPI_COMM_WCMP,ierr)
      DTMAX_GL=FOUT(1)
      CFLXY = DBLE(DT)/DTMAX_GL
      REST  = ABS(MOD(CFLXY,1.0d0))
      IF (REST .LT. THR) THEN
        ITER(IK,ITH) = ABS(NINT(CFLXY))
      ELSE IF (REST .GT. THR .AND. REST .LT. 0.5d0) THEN
        ITER(IK,ITH) = ABS(NINT(CFLXY)) + 1
      ELSE
        ITER(IK,ITH) = ABS(NINT(CFLXY))
      END IF
    END IF ! LCALC

    DO IP = 1, npa
      DTSI(IP) = DBLE(DT)/DBLE(ITER(IK,ITH))/PDLIB_SI(IP) ! Some precalculations for the time integration.
    END DO

    DO IT = 1, ITER(IK,ITH)

      U  = DBLE(AC)
      ST = ZERO
      PM = ZERO
      PP = ZERO
      DO IE = 1, NE
        NI   =  INE(:,IE)
        FT   = - ONESIXTH*DOT_PRODUCT(U(NI),FLALL(:,IE))
        UTILDE = NM(IE) * ( DOT_PRODUCT(KELEM(:,IE),U(NI)) - FT )
        THETA_L(:,IE) = KELEM(:,IE) * (U(NI) - UTILDE)
        IF (ABS(FT) .GT. 0.0d0) THEN
          BET1(:) = THETA_L(:,IE)/FT
          IF (ANY( BET1 .LT. 0.0d0) ) THEN
            BETAHAT(1)    = BET1(1) + 0.5d0 * BET1(2)
            BETAHAT(2)    = BET1(2) + 0.5d0 * BET1(3)
            BETAHAT(3)    = BET1(3) + 0.5d0 * BET1(1)
            BET1(1)       = MAX(ZERO,MIN(BETAHAT(1),1.d0-BETAHAT(2),1.d0))
            BET1(2)       = MAX(ZERO,MIN(BETAHAT(2),1.d0-BETAHAT(3),1.d0))
            BET1(3)       = MAX(ZERO,MIN(BETAHAT(3),1.d0-BETAHAT(1),1.d0))
            THETA_L(:,IE) = FT * BET1
          END IF
        END IF
        ST(NI) = ST(NI) + THETA_L(:,IE) ! the 2nd term are the theta values of each node ...
        THETA_H         = (1./3.+DT/(2.*PDLIB_TRIA(IE)) * KELEM(:,IE) ) * FT ! LAX
        !        THETA_H = (1./3.+2./3.*KELEM(:,IE)/SUM(MAX(ZERO,KELEM(:,IE))))*FT  ! CENTRAL ... can be tested as well a bit more dispersive then LAX
        THETA_ACE(:,IE) = THETA_H-THETA_L(:,IE)
        PP(NI) =  PP(NI) + MAX(ZERO, -THETA_ACE(:,IE)) * DTSI(NI)
        PM(NI) =  PM(NI) + MIN(ZERO, -THETA_ACE(:,IE)) * DTSI(NI)
      END DO

#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(ST, "ST in loop")
      END IF
#endif

      DO IP = 1, npa
        UL(IP) = MAX(ZERO,U(IP)-DTSI(IP)*ST(IP)*(1-IOBPA_LOC(IP)))*DBLE(IOBPD_LOC(ITH,IP))*IOBDP_LOC(IP)
      END DO

#ifdef MPI_PARALL_GRID
      CALL PDLIB_exchange1DREAL(UL)
#endif

      USTARI(1,:) = MAX(UL,U)
      USTARI(2,:) = MIN(UL,U)

      UIP = 0.
      UIM = 0.
      DO IE = 1, NE
        NI = INE(:,IE)
        UIP(NI) = MAX (UIP(NI), MAXVAL( USTARI(1,NI) ))
        UIM(NI) = MIN (UIM(NI), MINVAL( USTARI(2,NI) ))
      END DO

      WII(1,:) = MIN(1.0d0,(UIP-UL)/MAX( THR,PP))
      WII(2,:) = MIN(1.0d0,(UIM-UL)/MIN(-THR,PM))

      ST = ZERO
      DO IE = 1, NE
        I1 = INE(1,IE)
        I2 = INE(2,IE)
        I3 = INE(3,IE)
        IF (THETA_ACE(1,IE) .LT. ZERO) THEN
          TMP(1) = WII(1,I1)
        ELSE
          TMP(1) = WII(2,I1)
        END IF
        IF (THETA_ACE(2,IE) .LT. ZERO) THEN
          TMP(2) = WII(1,I2)
        ELSE
          TMP(2) = WII(2,I2)
        END IF
        IF (THETA_ACE(3,IE) .LT. ZERO) THEN
          TMP(3) = WII(1,I3)
        ELSE
          TMP(3) = WII(2,I3)
        END IF
        TMP1 = MINVAL(TMP)
        ST(I1) = ST(I1) + THETA_ACE(1,IE) * TMP1! * (ONE - BL) + BL * THETA_L(1,IE)
        ST(I2) = ST(I2) + THETA_ACE(2,IE) * TMP1! * (ONE - BL) + BL * THETA_L(2,IE)
        ST(I3) = ST(I3) + THETA_ACE(3,IE) * TMP1! * (ONE - BL) + BL * THETA_L(3,IE)
      END DO

      DO IP = 1, npa
        U(IP) = MAX(ZERO,UL(IP)-DTSI(IP)*ST(IP)*(1-IOBPA_LOC(IP)))*DBLE(IOBPD_LOC(ITH,IP))*IOBDP_LOC(IP)
#ifdef W3_REF1
        IF (REFPARS(3).LT.0.5.AND.IOBPD_LOC(ITH,IP).EQ.0.AND.IOBPA_LOC(IP).EQ.0) U(IP) = AC(IP) ! restores reflected boundary values
#endif
      END DO

      AC = REAL(U)

#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC in loop")
      END IF
#endif
      !
      ! 5 Update boundaries ... would be better to omit any if clause in this loop ...
      !   a possibility would be to use NBI = 0 when FLBPI is FALSE and loop on IBI whatever the value of NBI
      !
      IF ( FLBPI ) THEN
        RD1=RD10 - DT * REAL(ITER(IK,ITH)-IT)/REAL(ITER(IK,ITH))
        RD2=RD20
        IF ( RD2 .GT. 0.001 ) THEN
          RD2    = MIN(1.,MAX(0.,RD1/RD2))
          RD1    = 1. - RD2
        ELSE
          RD1    = 0.
          RD2    = 1.
        END IF
#ifdef W3_DEBUGSOLVER
        sumAC=0
        sumBPI0=0
        sumBPIN=0
        sumCG=0
        sumCLATS=0
#endif
        DO IBI = 1, NBI
          IP_glob = MAPSF(ISBPI(IBI),1)
          JX=IPGL_npa(IP_glob)
          IF (JX .gt. 0) THEN
            AC(JX) = ( RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI) )   &
                 / CG(IK,ISBPI(IBI)) * CLATS(ISBPI(IBI))
#ifdef W3_DEBUGSOLVER
            sumAC=sumAC + AC(JX)
            sumBPI0=sumBPI0 + BBPI0(ISP,IBI)
            sumBPIN=sumBPIN + BBPIN(ISP,IBI)
            sumCG=sumCG + CG(IK,ISBPI(IBI))
            sumCLATS=sumCLATS + CLATS(ISBPI(IBI))
#endif
          END IF
        END DO
      END IF

#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'NBI=', NBI
      WRITE(740+IAPROC,*) 'RD1=', RD1, ' RD2=', RD2
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumAC=', sumAC
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPI0=', sumBPI0
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPIN=', sumBPIN
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCG=', sumCG
      WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCLATS=', sumCLATS
      FLUSH(740+IAPROC)
#endif
      CALL PDLIB_exchange1DREAL(AC)

#ifdef W3_DEBUGSOLVER
      IF (testWrite) THEN
        CALL SCAL_INTEGRAL_PRINT_R4(AC, "AC after FLBPI")
      END IF
#endif
    END DO ! IT

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_W3XYPFSN2, step 6'
    FLUSH(740+IAPROC)
#endif

  END SUBROUTINE PDLIB_W3XYPFSFCT2
  !/ ------------------------------------------------------------------- /

  SUBROUTINE TEST_MPI_STATUS(string)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Check mpi status
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

    USE W3ADATMD, only : MPI_COMM_WCMP
    USE W3GDATMD, only : GTYPE, UNGTYPE
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    use yowDatapool, only: rtype, istatus

    INCLUDE "mpif.h"
    CHARACTER(*), INTENT(in) :: string
    REAL VcollExp(1)
    REAL rVect(1)
    INTEGER iProc, ierr
    WRITE(740+IAPROC,*) 'TEST_MPI_STATUS, at string=', string
    FLUSH(740+IAPROC)
    IF (IAPROC .gt. NAPROC) THEN
      RETURN
    END IF
    WRITE(740+IAPROC,*) 'After status settings'
    FLUSH(740+IAPROC)
    !
    ! Now find global arrays
    !
    IF (IAPROC .eq. 1) THEN
      DO iProc=2,NAPROC
        CALL MPI_RECV(rVect,1,MPI_REAL, iProc-1, 37, MPI_COMM_WCMP, istatus, ierr)
      END DO
    ELSE
      CALL MPI_SEND(VcollExp,1,MPI_REAL, 0, 37, MPI_COMM_WCMP, ierr)
    END IF
    WRITE(740+IAPROC,*) 'Leaving the TEST_MPI_STATUS'
    FLUSH(740+IAPROC)
  END SUBROUTINE TEST_MPI_STATUS
  !/ ------------------------------------------------------------------- /

  SUBROUTINE SCAL_INTEGRAL_PRINT_GENERAL(V, string, maxidx, CheckUncovered, PrintFullValue)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
    !  2. Method : maxidx = npa or np for arrays that have been synchronized or not
    !              CheckUncovered is because some the triangulation may not cover all nodes
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
    !
    USE W3GDATMD, only : NK, NTH, FTE
    USE W3GDATMD, only : NSPEC, NX, NY, NSEAL, MAPFS
    USE W3ADATMD, only : MPI_COMM_WCMP
    USE W3GDATMD, only : GTYPE, UNGTYPE
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    use yowDatapool, only: rtype, istatus
    USE YOWNODEPOOL, only: npa, iplg
    USE W3PARALL, only: INIT_GET_ISEA

    INCLUDE "mpif.h"
    !
    REAL*8, INTENT(in) :: V(NSEAL)
    CHARACTER(*), INTENT(in) :: string
    INTEGER, INTENT(IN) :: maxidx
    LOGICAL, INTENT(in) :: CheckUncovered
    LOGICAL, INTENT(in) :: PrintFullValue
    !
    REAL*8, allocatable :: Vcoll(:)
    INTEGER, allocatable :: Status(:)
    REAL*8, allocatable :: ListVal(:)
    INTEGER, allocatable :: ListIdx(:)
    INTEGER singV(2)
    REAL CoherencyError, eVal1, eVal2, eErr
    INTEGER NSEAL_dist, maxidx_dist
    INTEGER JSEA, ISEA, iProc, I, IX, ierr, ISP, IP, IP_glob
    INTEGER nbIncorr, idx
    INTEGER ITH, IK

    IF (IAPROC .gt. NAPROC) THEN
      RETURN
    END IF
    IF (GTYPE .ne. UNGTYPE) THEN
      RETURN
    END IF
    !
    ! Now find global arrays
    !
    IF (IAPROC .eq. 1) THEN
      CoherencyError=0
      allocate(Vcoll(NX), Status(NX))
      Vcoll=0
      Status=0
      DO JSEA=1,maxidx
        IP      = JSEA
        IP_glob = iplg(IP)
        ISEA=MAPFS(1,IP_glob)
        Vcoll(IP_glob)=V(JSEA)
        Status(IP_glob)=1
      END DO
      DO iProc=2,NAPROC
        CALL MPI_RECV(singV,2,MPI_INTEGER, iProc-1, 360, MPI_COMM_WCMP, istatus, ierr)
        NSEAL_dist = singV(1)
        maxidx_dist = singV(2)
        allocate(ListVal(NSEAL_dist), ListIdx(NSEAL_dist))
        CALL MPI_RECV(ListVal, NSEAL_dist, MPI_REAL8,   iProc-1, 370, MPI_COMM_WCMP, istatus, ierr)
        CALL MPI_RECV(ListIdx, NSEAL_dist, MPI_INTEGER, iProc-1, 430, MPI_COMM_WCMP, istatus, ierr)
        DO idx=1,maxidx_dist
          IP_glob = ListIdx(idx)
          eVal1 = Vcoll(IP_glob)
          eVal2 = ListVal(idx)
          Vcoll(IP_glob) = eVal2
          IF (Status(IP_glob) .eq. 1) THEN
            eErr=abs(eVal1 - eVal2)
            CoherencyError = CoherencyError + eErr
          END IF
          Status(IP_glob) = 1
        END DO
        deallocate(ListVal, ListIdx)
      END DO
      WRITE(740+IAPROC,'(a,f14.7,f14.7,a,a)') 'sum,coh=', sum(Vcoll), CoherencyError, ' ', TRIM(string)
      nbIncorr=0
      DO IX=1,NX
        ISEA=MAPFS(1,IX)
        IF (ISEA .gt. 0) THEN
          IF (Status(IX) .eq. 0) THEN
            nbIncorr=nbIncorr+1
          END IF
        END IF
      END DO
      IF (CheckUncovered) THEN
        IF (nbIncorr .gt. 0) THEN
          WRITE(*,*) '    nbIncorr=', nbIncorr
          WRITE(*,*) '          NX=', NX
          WRITE(*,*) '       NSEAL=', NSEAL
          WRITE(*,*) '         npa=', npa
          STOP
        END IF
      END IF
      IF (PrintFullValue) THEN
        WRITE(740+IAPROC,*) 'Value of V at nodes'
        DO IX=1,NX
          WRITE(740+IAPROC,*) 'IX=', IX, ' V=', Vcoll(IX)
        END DO
      END IF
      FLUSH(740+IAPROC)
      deallocate(Vcoll, Status)
    ELSE
      singV(1) = NSEAL
      singV(2) = maxidx
      CALL MPI_SEND(singV,2,MPI_INTEGER, 0, 360, MPI_COMM_WCMP, ierr)
      allocate(ListVal(NSEAL), ListIdx(NSEAL))
      DO JSEA=1,NSEAL
        IP      = JSEA
        IP_glob = iplg(IP)
        ISEA=MAPFS(1,IP_glob)
        ListVal(JSEA) = V(JSEA)
        ListIdx(JSEA) = IP_glob
      END DO
      CALL MPI_SEND(ListVal, NSEAL, MPI_REAL8,   0, 370, MPI_COMM_WCMP, ierr)
      CALL MPI_SEND(ListIdx, NSEAL, MPI_INTEGER, 0, 430, MPI_COMM_WCMP, ierr)
      deallocate(ListVal, ListIdx)
    END IF
  END SUBROUTINE SCAL_INTEGRAL_PRINT_GENERAL
  !/ ------------------------------------------------------------------- /
  SUBROUTINE SCAL_INTEGRAL_PRINT_R8(V, string)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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

    USE W3GDATMD, only : NSEAL

    REAL*8, INTENT(in) :: V(NSEAL)
    CHARACTER(*), INTENT(in) :: string
    REAL*8 :: V8(NSEAL)
    LOGICAL :: CheckUncovered = .FALSE.
    LOGICAL :: PrintFullValue = .FALSE.
    V8 = V
    CALL SCAL_INTEGRAL_PRINT_GENERAL(V8, string, NSEAL, CheckUncovered, PrintFullValue)
  END SUBROUTINE SCAL_INTEGRAL_PRINT_R8
  !/ ------------------------------------------------------------------- /
  SUBROUTINE SCAL_INTEGRAL_PRINT_R4(V, string)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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

    USE W3GDATMD, only : NSEAL

    REAL, INTENT(in) :: V(NSEAL)
    CHARACTER(*), INTENT(in) :: string
    LOGICAL :: CheckUncovered = .FALSE.
    LOGICAL :: PrintFullValue = .FALSE.
    REAL*8 V8(NSEAL)
    V8 = DBLE(V)
    CALL SCAL_INTEGRAL_PRINT_GENERAL(V8, string, NSEAL, CheckUncovered, PrintFullValue)
  END SUBROUTINE SCAL_INTEGRAL_PRINT_R4
  !/ ------------------------------------------------------------------- /
  SUBROUTINE ALL_VAOLD_INTEGRAL_PRINT(string, choice)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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

    USE W3GDATMD, only : NSEAL
    USE W3WDATMD, only : VAOLD
    USE W3ODATMD, only : IAPROC
    USE W3GDATMD, only : NSPEC
    USE YOWNODEPOOL, only: np, npa

    CHARACTER(*), INTENT(in) :: string
    INTEGER, INTENT(in) :: choice
    REAL :: FIELD(NSPEC,NSEAL)
    INTEGER ISPEC, JSEA, maxidx
    LOGICAL :: PrintMinISP = .FALSE.
    LOGICAL :: LocalizeMaximum = .FALSE.
    DO JSEA=1,NSEAL
      DO ISPEC=1,NSPEC
        FIELD(ISPEC,JSEA) = VAOLD(ISPEC,JSEA)
      END DO
    END DO
    IF (choice .eq. 1) THEN
      maxidx = npa
    ELSE
      maxidx = np
    END IF
    !      CALL ALL_FIELD_INTEGRAL_PRINT_GENERAL(FIELD, string)
    CALL CHECK_ARRAY_INTEGRAL_NX_R8_MaxFunct(FIELD, string, maxidx, PrintMinISP, LocalizeMaximum)
  END SUBROUTINE ALL_VAOLD_INTEGRAL_PRINT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE ALL_VA_INTEGRAL_PRINT(IMOD, string, choice)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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

    USE W3GDATMD, only : NSEAL, NSEA, NX, NY
    USE W3WDATMD, only : VA
    USE W3ODATMD, only : IAPROC, NAPROC
    USE W3GDATMD, only : NSPEC, GRIDS, GTYPE, UNGTYPE
    USE YOWNODEPOOL, only: npa, np, iplg

    INTEGER, INTENT(in) :: IMOD
    CHARACTER(*), INTENT(in) :: string
    INTEGER, INTENT(in) :: choice
    REAL :: FIELD(NSPEC,NSEAL)
    INTEGER ISPEC, JSEA, IP_glob, maxidx
    LOGICAL :: PrintMinISP = .FALSE.
    LOGICAL :: LocalizeMaximum = .FALSE.
    INTEGER :: TEST_IP = 46
    INTEGER :: TEST_ISP = 370
    IF (GRIDS(IMOD)%GTYPE .ne. UNGTYPE) THEN
      RETURN
    END IF
    IF (IAPROC .gt. NAPROC) THEN
      RETURN
    END IF
    WRITE(740+IAPROC,*) 'Entering ALL_INTEGRAL_PRINT, NSEAL=', NSEAL
    FLUSH(740+IAPROC)
    IF (NSEAL .ne. npa) THEN
      Print *, 'NSEAL=', NSEAL, " npa=", npa
      STOP
    END IF
    DO JSEA=1,NSEAL
      IP_glob=iplg(JSEA)
      DO ISPEC=1,NSPEC
        FIELD(ISPEC,JSEA) = VA(ISPEC,JSEA)
        IF ((IP_glob .eq. TEST_IP).and.(ISPEC .eq. TEST_ISP)) THEN
          WRITE(740+IAPROC,*) 'ASS TEST_IP=', TEST_IP, ' TEST_ISP=', TEST_ISP, ' val=', VA(ISPEC,JSEA)
        END IF
      END DO
    END DO
    WRITE(740+IAPROC,*) 'Before call to ALL_FIELD_INTEGRAL'
    WRITE(740+IAPROC,*) 'NSPEC=', NSPEC, ' NX=', NX
    FLUSH(740+IAPROC)
    IF (choice .eq. 1) THEN
      maxidx = npa
    ELSE
      maxidx = np
    END IF
    CALL CHECK_ARRAY_INTEGRAL_NX_R8_MaxFunct(FIELD, string, maxidx, PrintMinISP, LocalizeMaximum)
    WRITE(740+IAPROC,*) 'After call to ALL_FIELD_INTEGRAL'
    FLUSH(740+IAPROC)
  END SUBROUTINE ALL_VA_INTEGRAL_PRINT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE ALL_FIELD_INTEGRAL_PRINT(FIELD, string)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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

    USE W3GDATMD, only : NSEAL
    USE W3WDATMD, only : VA
    USE W3ODATMD, only : IAPROC
    USE W3GDATMD, only : NSPEC

    INTEGER maxidx
    REAL, INTENT(in) :: FIELD(NSPEC,NSEAL)
    CHARACTER(*), INTENT(in) :: string
    LOGICAL :: PrintMinISP = .FALSE.
    LOGICAL :: LocalizeMaximum = .FALSE.
    maxidx = NSEAL
    CALL CHECK_ARRAY_INTEGRAL_NX_R8_MaxFunct(FIELD, string, maxidx, PrintMinISP, LocalizeMaximum)
  END SUBROUTINE ALL_FIELD_INTEGRAL_PRINT
  !/ ------------------------------------------------------------------- /
  !/ ------- Coherency info for TheARR(NSPEC,npa) ---------------------- /
  !/ ----------- maxidx is np or npa ----------------------------------- /
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CHECK_ARRAY_INTEGRAL_NX_R8_MaxFunct(TheARR, string, maxidx, PrintMinISP, LocalizeMaximum)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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

    USE W3GDATMD, only : NK, NTH
    USE W3GDATMD, only : NSPEC, NX, NY, NSEAL, MAPFS
    USE W3ADATMD, only : MPI_COMM_WCMP
    USE W3GDATMD, only : GTYPE, UNGTYPE
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    use yowDatapool, only: rtype, istatus
    USE YOWNODEPOOL, only: npa, iplg
    USE W3PARALL, only: INIT_GET_ISEA

    INCLUDE "mpif.h"
    CHARACTER(*), INTENT(in) :: string
    INTEGER, INTENT(in) :: maxidx
    REAL, INTENT(in) :: TheARR(NSPEC, npa)
    LOGICAL, INTENT(in) :: PrintMinISP, LocalizeMaximum
    !
    REAL Vcoll(NSPEC,NX), VcollExp(NSPEC*NX), rVect(NSPEC*NX)
    REAL CoherencyError_Max, CoherencyError_Sum
    REAL eVal1, eVal2, eErr
    INTEGER LocateMax_I, LocateMax_ISP
    INTEGER rStatus(NX), Status(NX)
    INTEGER JSEA, ISEA, iProc, I, IX, ierr, ISP, IP, IP_glob
    REAL :: mval, eVal, eSum
    REAL :: TheMax, TheSum, TheNb, TheAvg
    REAL :: eFact, Threshold
    LOGICAL :: IsFirst
    INTEGER nbIncorr, n_control
    INTEGER ITH, IK
    INTEGER :: TEST_IP = 46
    INTEGER :: TEST_ISP = 370
    IF (IAPROC .gt. NAPROC) THEN
      RETURN
    END IF
    IF (GTYPE .ne. UNGTYPE) THEN
      RETURN
    END IF
    WRITE(740+IAPROC,*) 'CHECK_ARRAY_INTEGRAL NSEAL=', NSEAL, ' npa=', npa, ' maxidx=', maxidx
    VcollExp=0
    Status=0
    DO IP=1,maxidx
      IP_glob=iplg(IP)
      DO ISP=1,NSPEC
        VcollExp(ISP+NSPEC*(IP_glob-1)) = TheARR(ISP,IP)
        IF ((IP_glob .eq. TEST_IP).and.(ISP .eq. TEST_ISP)) THEN
          WRITE(740+IAPROC,*) 'TEST_IP=', TEST_IP, ' TEST_ISP=', TEST_ISP, ' val=', TheARR(ISP,IP)
        END IF
      END DO
      Status(IP_glob)=1
    END DO
    !
    ! Now find global arrays
    !
    CoherencyError_Max = 0
    CoherencyError_Sum = 0
    LocateMax_I = -1
    LocateMax_ISP = -1

    n_control = 0
    IF (IAPROC .eq. 1) THEN
      DO iProc=2,NAPROC
        CALL MPI_RECV(rVect  ,NSPEC*NX,MPI_REAL   , iProc-1, 37, MPI_COMM_WCMP, istatus, ierr)
        CALL MPI_RECV(rStatus,NX      ,MPI_INTEGER, iProc-1, 43, MPI_COMM_WCMP, istatus, ierr)
        DO I=1,NX
          IF (rStatus(I) .eq. 1) THEN
            DO ISP=1,NSPEC
              eVal1 = VcollExp(ISP+NSPEC*(I-1))
              eVal2 = rVect(ISP+NSPEC*(I-1))
              IF (Status(I) .eq. 1) THEN
                eErr=abs(eVal1 - eVal2)
                CoherencyError_Sum = CoherencyError_Sum + eErr
                IF (eErr .gt. CoherencyError_Max) THEN
                  CoherencyError_Max = eErr
                  LocateMax_I = I
                  LocateMax_ISP = ISP
                END IF
                IF (ISP .eq. 1) THEN
                  n_control = n_control + 1
                END IF
              ELSE
                VcollExp(ISP+NSPEC*(I-1))=eVal2
              END IF
            END DO
            Status(I)=1
          END IF
        END DO
      END DO
    ELSE
      CALL MPI_SEND(VcollExp,NSPEC*NX,MPI_REAL   , 0, 37, MPI_COMM_WCMP, ierr)
      CALL MPI_SEND(Status  ,NX      ,MPI_INTEGER, 0, 43, MPI_COMM_WCMP, ierr)
    END IF
    IF (IAPROC .eq. 1) THEN
      DO I=1,NX
        DO ISP=1,NSPEC
          Vcoll(ISP,I)=VcollExp(ISP + NSPEC*(I-1))
        END DO
      END DO
      nbIncorr=0
      DO IX=1,NX
        ISEA=MAPFS(1,IX)
        IF (ISEA .gt. 0) THEN
          IF (Status(IX) .eq. 0) THEN
            nbIncorr=nbIncorr+1
          END IF
        END IF
      END DO
      IF (nbIncorr .gt. 0) THEN
        WRITE(*,*) '    nbIncorr=', nbIncorr
        WRITE(*,*) '          NX=', NX
        WRITE(*,*) '         npa=', npa
        STOP
      END IF
      WRITE(740+IAPROC,*) 'CHECK_ARRAY_INTEGRAL n_control=', n_control
      WRITE(740+IAPROC,*) 'ARRAY_NX sum,coh=', sum(Vcoll), CoherencyError_Sum, TRIM(string)
      WRITE(740+IAPROC,*) 'ARRAY_NX max,loc=', CoherencyError_Max,LocateMax_I,LocateMax_ISP, TRIM(string)
      IF (PrintMinISP) THEN
        DO ISP=1,NSPEC
          IsFirst=.true.
          eSum=0
          DO IP=1,maxidx
            eVal=abs(Vcoll(ISP, IP))
            eSum=eSum + eVal
            IF (IsFirst.eqv. .true.) then
              mval=eVal
            ELSE
              IF (eVal .lt. mval) THEN
                mval=eVal
              ENDIF
            ENDIF
            IsFirst=.false.
          END DO
          WRITE(740+IAPROC,*) 'ISP=', ISP, ' mval/sum=', mval, eSum
        END DO
        FLUSH(740+IAPROC)
      END IF
      IF (LocalizeMaximum) THEN
        TheMax=0
        TheNb=0
        TheSum=0
        DO IP=1,maxidx
          DO ISP=1,NSPEC
            eVal = abs(Vcoll(ISP, IP))
            TheSum = TheSum + eVal
            TheNb = TheNb + 1
            IF (eVal .gt. TheMax) THEN
              TheMax=eVal
            END IF
          END DO
        END DO
        TheAvg = TheSum / TheNb
        WRITE(740+IAPROC,*) 'TheAvg/TheMax=', TheAvg, TheMax
        eFact=0.5
        Threshold=eFact * TheMax
        DO IP=1,maxidx
          DO ISP=1,NSPEC
            eVal = abs(Vcoll(ISP, IP))
            IF (eVal .gt. Threshold) THEN
              WRITE(740+IAPROC,*) 'ISP/IP/val=', ISP, IP, eVal
            END IF
          END DO
        END DO
        FLUSH(740+IAPROC)
      END IF
    END IF
  END SUBROUTINE CHECK_ARRAY_INTEGRAL_NX_R8_MaxFunct
  !/ ------------------------------------------------------------------- /
  !*    maxidx should be "np" or "npa"                                  *
  SUBROUTINE CHECK_ARRAY_INTEGRAL_NX_R8(TheARR, string, maxidx)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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

    USE W3GDATMD, only : NSPEC
    USE YOWNODEPOOL, only: npa
    CHARACTER(*), INTENT(in) :: string
    INTEGER, INTENT(in) :: maxidx
    REAL, INTENT(in) :: TheARR(NSPEC, npa)
    REAL*8 :: TheARR_red(npa)
    !      LOGICAL :: FULL_NSPEC = .FALSE.
    !      LOGICAL :: PrintMinISP = .FALSE.
    !      LOGICAL :: LocalizeMaximum = .FALSE.
    !      LOGICAL :: CheckUncovered = .FALSE.
    !      LOGICAL :: PrintFullValue = .FALSE.
    LOGICAL :: FULL_NSPEC = .TRUE.
    LOGICAL :: PrintMinISP = .TRUE.
    LOGICAL :: LocalizeMaximum = .TRUE.
    LOGICAL :: CheckUncovered = .TRUE.
    LOGICAL :: PrintFullValue = .TRUE.
    integer :: ip

    IF (FULL_NSPEC) THEN
      CALL CHECK_ARRAY_INTEGRAL_NX_R8_MaxFunct(TheARR, string, maxidx, PrintMinISP, LocalizeMaximum)
    ELSE
      DO IP=1,npa
        TheARR_red(IP) = SUM(ABS(TheArr(:,IP)))
      END DO
      CALL SCAL_INTEGRAL_PRINT_GENERAL(TheARR_red, string, maxidx, CheckUncovered, PrintFullValue)
    END IF
  END SUBROUTINE CHECK_ARRAY_INTEGRAL_NX_R8
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_W3XYPUG_BLOCK_IMPLICIT(IMOD, FACX, FACY, DTG, VGX, VGY, LCALC )
    !/ ------------------------------------------------------------------- /
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Block Explicit N-Scheme
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3ODATMD, only: IAPROC
    USE W3GDATMD, only: B_JGS_USE_JACOBI

    LOGICAL, INTENT(IN) :: LCALC
    INTEGER, INTENT(IN) :: IMOD
    REAL, INTENT(IN)        :: FACX, FACY, DTG, VGX, VGY
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'B_JGS_USE_JACOBI=', B_JGS_USE_JACOBI
    FLUSH(740+IAPROC)
#endif
    IF (B_JGS_USE_JACOBI) THEN
      CALL PDLIB_JACOBI_GAUSS_SEIDEL_BLOCK(IMOD, FACX, FACY, DTG, VGX, VGY, LCALC)
      RETURN
    END IF
    WRITE(*,*) 'Error: You need to use with JGS_USE_JACOBI'
    STOP 'Correct your implicit solver options'
    !/
    !/ End of W3XYPFSN --------------------------------------------------- /
    !/
  END SUBROUTINE PDLIB_W3XYPUG_BLOCK_IMPLICIT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_W3XYPUG_BLOCK_EXPLICIT(IMOD, FACX, FACY, DTG, VGX, VGY, LCALC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Driver for block explicit routine
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3ODATMD, only: IAPROC
    USE W3GDATMD, only: B_JGS_USE_JACOBI

    LOGICAL, INTENT(IN) :: LCALC
    INTEGER, INTENT(IN) :: IMOD
    REAL, INTENT(IN) :: FACX, FACY, DTG, VGX, VGY

    CALL PDLIB_EXPLICIT_BLOCK(IMOD, FACX, FACY, DTG, VGX, VGY, LCALC)
    !/
    !/ End of W3XYPFSN ----------------------------------------------------- /
    !/
  END SUBROUTINE PDLIB_W3XYPUG_BLOCK_EXPLICIT
  !/ --------------------------------------------------------------------- /
  SUBROUTINE PRINT_WN_STATISTIC(string)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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
    USE W3SERVMD, only: STRACE
#endif
    !

    USE W3ODATMD, only : IAPROC
    USE W3GDATMD, only: NK
    USE W3ADATMD, only: WN
    USE W3GDATMD, only: NSEAL
    USE YOWNODEPOOL, only: NP

    CHARACTER(*), INTENT(in) :: string
    REAL TotalSumDMM, eDMM, sumDMM
    INTEGER IP, IK, ISEA
    WRITE(740+IAPROC,*) 'PRINT_WN_STATISTIC'
    TotalSumDMM=0
    DO ISEA=1,NSEAL
      sumDMM=0
      DO IK=0, NK
        eDMM = WN(IK+1,ISEA) - WN(IK,ISEA)
        sumDMM=sumDMM + abs(eDMM)
      END DO
      IF (ISEA .eq. 1) THEN
        WRITE(740+IAPROC,*) 'ISEA=', ISEA
        WRITE(740+IAPROC,*) 'sumDMM=', sumDMM
      END IF
      TotalSumDMM = TotalSumDMM + sumDMM
    END DO
    WRITE(740+IAPROC,*) 'string=', string
    WRITE(740+IAPROC,*) 'TotalSumDMM=', TotalSumDMM
    FLUSH(740+IAPROC)
    !/
    !/ End of W3XYPFSN --------------------------------------------------- /
    !/
  END SUBROUTINE PRINT_WN_STATISTIC
  !/ ------------------------------------------------------------------- /
  SUBROUTINE WRITE_VAR_TO_TEXT_FILE(TheArr, eFile)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only : NK, NTH
    USE W3WDATMD, only : VA
    USE W3GDATMD, only : NSPEC, NX, NY, NSEAL, MAPFS
    USE W3ADATMD, only : MPI_COMM_WCMP
    USE W3GDATMD, only : GTYPE, UNGTYPE
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    use yowDatapool, only: rtype, istatus
    USE YOWNODEPOOL, only: npa, iplg, np
    USE W3PARALL, only: INIT_GET_ISEA

    INCLUDE "mpif.h"
    CHARACTER(*), INTENT(in) :: eFile
    REAL, INTENT(in) :: TheARR(NSPEC, npa)
    !
    REAL Vcoll(NSPEC,NX), VcollExp(NSPEC*NX), rVect(NSPEC*NX)
    REAL CoherencyError, eVal1, eVal2, eErr
    INTEGER rStatus(NX), Status(NX)
    INTEGER JSEA, ISEA, iProc, I, IX, ierr, ISP, IP, IP_glob
    INTEGER nbIncorr
    INTEGER ITH, IK
    INTEGER fhndl
    REAL eSum
    IF (IAPROC .gt. NAPROC) THEN
      RETURN
    END IF
    IF (GTYPE .ne. UNGTYPE) THEN
      RETURN
    END IF
    VcollExp=0
    Status=0
    DO IP=1,np
      IP_glob=iplg(IP)
      DO ISP=1,NSPEC
        VcollExp(ISP+NSPEC*(IP_glob-1))=TheARR(ISP,IP)
      END DO
      Status(IP_glob)=1
    END DO
    !
    ! Now find global arrays
    !
    CoherencyError=0
    IF (IAPROC .eq. 1) THEN
      DO iProc=2,NAPROC
        CALL MPI_RECV(rVect  ,NSPEC*NX,MPI_DOUBLE , iProc-1, 37, MPI_COMM_WCMP, istatus, ierr)
        CALL MPI_RECV(rStatus,NX      ,MPI_INTEGER, iProc-1, 43, MPI_COMM_WCMP, istatus, ierr)
        DO I=1,NX
          IF (rStatus(I) .eq. 1) THEN
            DO ISP=1,NSPEC
              eVal1=VcollExp(ISP+NSPEC*(I-1))
              eVal2=rVect(ISP+NSPEC*(I-1))
              VcollExp(ISP+NSPEC*(I-1))=rVect(ISP+NSPEC*(I-1))
              IF (Status(I) .eq. 1) THEN
                eErr=abs(eVal1 - eVal2)
                CoherencyError = CoherencyError + eErr
              ELSE
                VcollExp(ISP+NSPEC*(I-1))=eVal2
              END IF
            END DO
            Status(I)=1
          END IF
        END DO
      END DO
    ELSE
      CALL MPI_SEND(VcollExp,NSPEC*NX,MPI_DOUBLE , 0, 37, MPI_COMM_WCMP, ierr)
      CALL MPI_SEND(Status  ,NX      ,MPI_INTEGER, 0, 43, MPI_COMM_WCMP, ierr)
    END IF
    IF (IAPROC .eq. 1) THEN
      DO I=1,NX
        DO ISP=1,NSPEC
          Vcoll(ISP,I)=VcollExp(ISP + NSPEC*(I-1))
        END DO
      END DO
      OPEN(fhndl, FILE=eFile)
      DO IX=1,NX
        eSum=sum(VColl(:,IX))
        WRITE(fhndl,*) 'IX=', IX, 'eSum=', eSum
      END DO
      CLOSE(fhndl)
    END IF
    !/
    !/ End of W3XYPFSN ----------------------------------------------------- /
    !/
  END SUBROUTINE WRITE_VAR_TO_TEXT_FILE
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PrintTotalOffContrib(string)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Source code for parallel debugging
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE YOWNODEPOOL,    only: PDLIB_CCON, NPA, PDLIB_I_DIAG, PDLIB_JA, PDLIB_IA_P
    USE W3GDATMD, only: NSPEC
    USE W3ODATMD, only : IAPROC

    CHARACTER(*), INTENT(in) :: string
    INTEGER J, IP, JP, I, ISP
    REAL TheSum1, TheSum2
    J = 0
    TheSum1=0
    DO IP = 1, npa
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        IF (J .ne. PDLIB_I_DIAG(IP)) THEN
          DO ISP=1,NSPEC
            TheSum1=TheSum1 + abs(ASPAR_JAC(ISP,J))
          END DO
        END IF
      END DO
    END DO
    !
    TheSum2=0
    DO IP = 1, npa
      DO i = PDLIB_IA_P(IP)+1, PDLIB_IA_P(IP+1)
        JP=PDLIB_JA(I)
        IF (JP .ne. IP) THEN
          DO ISP=1,NSPEC
            TheSum2=TheSum2 + abs(ASPAR_JAC(ISP,I))
          END DO
        END IF
      END DO
    END DO
    WRITE(740+IAPROC,'(a,f14.7,f14.7,a,a)') 'TheSum12=', TheSum1, TheSum2, ' ', string
    FLUSH(740+IAPROC)
    !/
    !/ End of W3XYPFSN --------------------------------------------------- /
    !/
  END SUBROUTINE PrintTotalOffContrib
  !/ ------------------------------------------------------------------- /
  SUBROUTINE COMPUTE_MEAN_PARAM (A, CG, WN, EMEAN, FMEAN, WNMEAN, AMAX)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute mean prarameter
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE CONSTANTS
    USE W3GDATMD, only: NK, NTH, SIG, DDEN, FTE, FTF, FTWN
#ifdef W3_T
    USE W3ODATMD, only: NDST
#endif
#ifdef W3_S
    USE W3SERVMD, only: STRACE
#endif
    !
    REAL, INTENT(IN)        :: A(NTH,NK), CG(NK), WN(NK)
    REAL, INTENT(OUT)       :: EMEAN, FMEAN, WNMEAN, AMAX
    INTEGER                 :: IK, ITH
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    :: EB(NK), EBAND
#ifdef W3_S
    CALL STRACE (IENT, 'W3SPR0')
#endif
    !
    EMEAN  = 0.
    FMEAN  = 0.
    WNMEAN = 0.
    AMAX   = 0.
    !
    ! 1.  Integral over directions
    !
    DO IK=1, NK
      EB(IK) = 0.
      DO ITH=1, NTH
        EB(IK) = EB(IK) + A(ITH,IK)
        AMAX   = MAX ( AMAX , A(ITH,IK) )
      END DO
    END DO
    !
    ! 2.  Integrate over directions
    !
    DO IK=1, NK
      EB(IK) = EB(IK) * DDEN(IK) / CG(IK)
      EMEAN  = EMEAN  + EB(IK)
      FMEAN  = FMEAN  + EB(IK) / SIG(IK)
      WNMEAN = WNMEAN + EB(IK) / SQRT(WN(IK))
    END DO
    !
    ! 3.  Add tail beyond discrete spectrum
    !     ( DTH * SIG absorbed in FTxx )
    !
    EBAND  = EB(NK) / DDEN(NK)
    EMEAN  = EMEAN  + EBAND * FTE
    FMEAN  = FMEAN  + EBAND * FTF
    WNMEAN = WNMEAN + EBAND * FTWN
    !
    ! 4.  Final processing
    !
    FMEAN  = TPIINV * EMEAN / MAX ( 1.E-7 , FMEAN )
    WNMEAN = ( EMEAN / MAX ( 1.E-7 , WNMEAN ) )**2
    !
#ifdef W3_T
    WRITE (NDST,9000) EMEAN, FMEAN, WNMEAN
#endif
    !
    RETURN
    !
    ! Formats
    !
#ifdef W3_T
9000 FORMAT (' TEST W3SPR0 : E,F,WN MEAN ',3E10.3)
#endif
    !/
    !/ End of W3SPR0 ----------------------------------------------------- /
    !/
  END SUBROUTINE COMPUTE_MEAN_PARAM
  !/ ------------------------------------------------------------------- /
  SUBROUTINE calcARRAY_JACOBI(DTG,FACX,FACY,VGX,VGY)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for advection part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: NK, NK2, NTH, NSPEC, FACHFA, DMIN
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3GDATMD, only: NSEAL, CLATS
    USE W3GDATMD, only: MAPSTA
    USE W3WDATMD, only: VA
    USE W3ADATMD, only: CG, DW, WN, CX, CY
    USE W3IDATMD, only: FLCUR, FLLEV
    USE W3GDATMD, only: ECOS, ESIN, MAPFS
    USE W3PARALL, only : ONESIXTH, ZERO, THR
    use yowElementpool, only: ne, INE
    USE YOWNODEPOOL,    only: PDLIB_IEN, PDLIB_TRIA,                  &
         PDLIB_CCON, PDLIB_POS_CELL2, PDLIB_IE_CELL2, NP, NPA,          &
         PDLIB_IA_P, PDLIB_POSI, PDLIB_IA, PDLIB_NNZ, iplg,           &
         PDLIB_I_DIAG, PDLIB_JA
    USE W3ODATMD, only : IAPROC
    USE W3PARALL, only : ZERO
#ifdef W3_DB1
    USE W3SDB1MD
    USE W3GDATMD, only: SDBSC
#endif
#ifdef W3_BT1
    USE W3SBT1MD
#endif
#ifdef W3_BT4
    USE W3SBT4MD
#endif
#ifdef W3_BT8
    USE W3SBT8MD
#endif
#ifdef W3_BT9
    USE W3SBT9MD
#endif
#ifdef W3_IC1
    USE W3SIC1MD
#endif
#ifdef W3_IC2
    USE W3SIC2MD
#endif
#ifdef W3_IC3
    USE W3SIC3MD
#endif
#ifdef W3_TR1
    USE W3STR1MD
#endif
    REAL, INTENT(in) :: DTG, FACX, FACY, VGX, VGY
    INTEGER :: IP, ISP, ISEA, IP_glob
    INTEGER :: idx, IS
    INTEGER :: I, J, ITH, IK, J2
    INTEGER :: IE, POS, JSEA
    INTEGER :: I1, I2, I3, NI(3)
    INTEGER :: counter
#ifdef W3_REF1
    INTEGER :: eIOBPDR
#endif
    REAL :: DTK, TMP3
    REAL :: LAMBDA(2)
    REAL :: FL11, FL12
    REAL :: FL21, FL22
    REAL :: FL31, FL32
    REAL :: CRFS(3), CXY(3,2)
    REAL :: KP(3,NSPEC,NE)
    REAL :: KM(3), K(3)
    REAL :: K1, eSI, eVS, eVD
    REAL :: eVal1, eVal2, eVal3
    REAL :: DELTAL(3,NSPEC,NE)
    REAL :: NM(NSPEC,NE)
    REAL :: TRIA03, SIDT, CCOS, CSIN
    REAL :: SPEC(NSPEC), DEPTH

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'calcARRAY_JACOBI, begin'
    FLUSH(740+IAPROC)
#endif
    memunit = 50000+IAPROC

    I      = 0
    IE     = 0
    POS    = 0
    I1     = 0
    I2     = 0
    I3     = 0
    DTK    = 0
    TMP3   = 0

    call print_memcheck(memunit, 'memcheck_____:'//' WW3_JACOBI SECTION 0')

    DO IE = 1, NE
      I1 = INE(1,IE)
      I2 = INE(2,IE)
      I3 = INE(3,IE)
      NI = INE(:,IE)
      DO IS = 1, NSPEC
        ITH    = 1 + MOD(IS-1,NTH)
        IK     = 1 + (IS-1)/NTH
        CCOS   = FACX * ECOS(ITH)
        CSIN   = FACY * ESIN(ITH)
        CXY(:,1) = CCOS * CG(IK,NI) / CLATS(NI)
        CXY(:,2) = CSIN * CG(IK,NI)
        IF (FLCUR) THEN
          CXY(:,1) = CXY(:,1) + FACX * CX(NI)/CLATS(NI)
          CXY(:,2) = CXY(:,2) + FACY * CY(NI)
        ENDIF
#ifdef W3_MGP
        CXY(:,1) = CXY(:,1) - CCURX*VGX/CLATS(ISEA)
        CXY(:,2) = CXY(:,2) - CCURY*VGY
#endif
        FL11 = CXY(2,1)*PDLIB_IEN(1,IE)+CXY(2,2)*PDLIB_IEN(2,IE)
        FL12 = CXY(3,1)*PDLIB_IEN(1,IE)+CXY(3,2)*PDLIB_IEN(2,IE)
        FL21 = CXY(3,1)*PDLIB_IEN(3,IE)+CXY(3,2)*PDLIB_IEN(4,IE)
        FL22 = CXY(1,1)*PDLIB_IEN(3,IE)+CXY(1,2)*PDLIB_IEN(4,IE)
        FL31 = CXY(1,1)*PDLIB_IEN(5,IE)+CXY(1,2)*PDLIB_IEN(6,IE)
        FL32 = CXY(2,1)*PDLIB_IEN(5,IE)+CXY(2,2)*PDLIB_IEN(6,IE)
        CRFS(1) = - ONESIXTH *  (2.0d0 *FL31 + FL32 + FL21 + 2.0d0 * FL22 )
        CRFS(2) = - ONESIXTH *  (2.0d0 *FL32 + 2.0d0 * FL11 + FL12 + FL31 )
        CRFS(3) = - ONESIXTH *  (2.0d0 *FL12 + 2.0d0 * FL21 + FL22 + FL11 )
        LAMBDA(1) = ONESIXTH * SUM(CXY(:,1))
        LAMBDA(2) = ONESIXTH * SUM(CXY(:,2))
        K(1)  = LAMBDA(1) * PDLIB_IEN(1,IE) + LAMBDA(2) * PDLIB_IEN(2,IE)
        K(2)  = LAMBDA(1) * PDLIB_IEN(3,IE) + LAMBDA(2) * PDLIB_IEN(4,IE)
        K(3)  = LAMBDA(1) * PDLIB_IEN(5,IE) + LAMBDA(2) * PDLIB_IEN(6,IE)
        KP(:,IS,IE) = MAX(ZERO,K(:))
        DELTAL(:,IS,IE) = CRFS(:) - KP(:,IS,IE)
        KM(:) = MIN(ZERO,K(:))
        NM(IS,IE) = 1.d0/MIN(-THR,SUM(KM))
      ENDDO
    END DO

    J = 0
    DO IP = 1, npa
      IP_glob=iplg(IP)
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        IE    =  PDLIB_IE_CELL2(I,IP)
        POS   =  PDLIB_POS_CELL2(I,IP)
        I1    =  PDLIB_POSI(1,J)
        I2    =  PDLIB_POSI(2,J)
        I3    =  PDLIB_POSI(3,J)
#ifdef W3_DEBUGSRC
        WRITE(740+IAPROC,*) 'I1=', I1, ' PDLIB_I_DIAG=', PDLIB_I_DIAG(IP)
#endif
        DO ISP=1,NSPEC
          ITH    = 1 + MOD(ISP-1,NTH)
          IK     = 1 + (ISP-1)/NTH
          K1    =  KP(POS,ISP,IE)
#ifdef W3_REF1
          eIOBPDR=(1-IOBP_LOC(IP_glob))*(1-IOBPD_LOC(ITH,IP_glob))
          IF (eIOBPDR .eq. 1) THEN
            K1=ZERO
          END IF
#endif
          TRIA03        =  1./3. * PDLIB_TRIA(IE)
          DTK           =  K1 * DTG * IOBDP_LOC(IP) * (1-IOBPA_LOC(IP)) * IOBPD_LOC(ITH,IP)
          B_JAC(ISP,IP) =  B_JAC(ISP,IP) + TRIA03 * VA(ISP,IP) * IOBDP_LOC(IP) * (1-IOBPA_LOC(IP)) * IOBPD_LOC(ITH,IP)
          TMP3          =  DTK * NM(ISP,IE)
          IF (FSGEOADVECT) THEN
            ASPAR_JAC(ISP,I1) = ASPAR_JAC(ISP,I1) + TRIA03 + DTK - TMP3*DELTAL(POS,ISP,IE)
            ASPAR_JAC(ISP,I2) = ASPAR_JAC(ISP,I2)                - TMP3*DELTAL(POS_TRICK(POS,1),ISP,IE)
            ASPAR_JAC(ISP,I3) = ASPAR_JAC(ISP,I3)                - TMP3*DELTAL(POS_TRICK(POS,2),ISP,IE)
          ELSE
            ASPAR_JAC(ISP,I1) = ASPAR_JAC(ISP,I1) + TRIA03
          END IF
        END DO
      END DO
    END DO
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_JACOBI SECTION 1')
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'sum(VA)=', sum(VA)
    CALL PrintTotalOffContrib("Offdiag after the geo advection")
#endif
    !/
    !/ End of W3XYPFSN ----------------------------------------------------- /
    !/
  END SUBROUTINE calcARRAY_JACOBI
  !/ ------------------------------------------------------------------- /
  SUBROUTINE calcARRAY_JACOBI_VEC(DTG,FACX,FACY,VGX,VGY)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for advection part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: NK, NK2, NTH, NSPEC, FACHFA, DMIN
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3GDATMD, only: NSEAL, CLATS
    USE W3GDATMD, only: MAPSTA, SIG
    USE W3WDATMD, only: VA
    USE W3ADATMD, only: CG, DW, WN, CX, CY
    USE W3IDATMD, only: FLCUR, FLLEV
    USE W3GDATMD, only: ECOS, ESIN, MAPFS
    USE W3PARALL, only : ONESIXTH, ZERO, THR
    use yowElementpool, only: ne, INE
    USE YOWNODEPOOL,    only: PDLIB_IEN, PDLIB_TRIA,                  &
         PDLIB_IE_CELL2, PDLIB_POS_CELL2, PDLIB_CCON, NP, NPA,          &
         PDLIB_IA_P, PDLIB_POSI, PDLIB_IA, PDLIB_NNZ, iplg,           &
         PDLIB_I_DIAG, PDLIB_JA, PDLIB_TRIA03, PDLIB_SI
    USE W3ODATMD, only : IAPROC
    USE W3PARALL, only : ZERO
    USE W3DISPMD, only : WAVNU_LOCAL
#ifdef W3_DB1
    USE W3SDB1MD
    USE W3GDATMD, only: SDBSC
#endif
#ifdef W3_BT1
    USE W3SBT1MD
#endif
#ifdef W3_BT4
    USE W3SBT4MD
#endif
#ifdef W3_BT8
    USE W3SBT8MD
#endif
#ifdef W3_BT9
    USE W3SBT9MD
#endif
#ifdef W3_IC1
    USE W3SIC1MD
#endif
#ifdef W3_IC2
    USE W3SIC2MD
#endif
#ifdef W3_IC3
    USE W3SIC3MD
#endif
#ifdef W3_TR1
    USE W3STR1MD
#endif
    REAL, INTENT(in) :: DTG, FACX, FACY, VGX, VGY
    INTEGER :: IP, ISP, ISEA, IP_glob
    INTEGER :: idx, IS
    INTEGER :: I, J, ITH, IK, J2
    INTEGER :: IE, POS, JSEA
    INTEGER :: I1, I2, I3, NI(3)
    INTEGER :: counter, IB1, IB2, IBR
    REAL    :: DTK, TMP3
    REAL    :: LAMBDA(2), CXYY(2,3), CXY(2,NPA)
    REAL    :: FL11, FL12
    REAL    :: FL21, FL22
    REAL    :: FL31, FL32
    REAL    :: CRFS(3), K(3)
    REAL    :: KP(3,NE)
    REAL    :: KM(3), DELTAL(3,NE)
    REAL    :: K1, eSI, eVS, eVD
    REAL    :: eVal1, eVal2, eVal3
    REAL    :: CG1, WN1
    REAL    :: TRIA03, SIDT, CCOS, CSIN
    REAL    :: SPEC(NSPEC), DEPTH, CCOSA(NTH), CSINA(NTH)
    INTEGER :: IOBPTH1(NTH), IOBPTH2(NTH)

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'calcARRAY_JACOBI, begin'
    FLUSH(740+IAPROC)
#endif
    memunit = 50000+IAPROC

    I      = 0
    IE     = 0
    POS    = 0
    I1     = 0
    I2     = 0
    I3     = 0
    DTK    = 0
    TMP3   = 0

    CCOSA = FACX * ECOS(1:NTH)
    CSINA = FACX * ESIN(1:NTH)
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_JACOBI SECTION 0')

    DO ISP = 1, NSPEC

      ITH    = 1 + MOD(ISP-1,NTH)
      IK     = 1 + (ISP-1)/NTH
      CCOS   = CCOSA(ITH)
      CSIN   = CSINA(ITH)

      DO IP = 1, NPA

        IP_GLOB = IPLG(IP)
#ifdef NOCGTABLE
        CALL WAVNU_LOCAL(SIG(IK),DW(IP_GLOB),WN1,CG1)
#else
        CG1    = CG(IK,IP_GLOB)
#endif
        CXY(1,IP) = CCOS * CG1/CLATS(IP_GLOB)
        CXY(2,IP) = CSIN * CG1
        IF (FLCUR) THEN
          CXY(1,IP) = CXY(1,IP) + FACX * CX(IP_GLOB)/CLATS(IP_GLOB)*IOBDP_LOC(IP)
          CXY(2,IP) = CXY(2,IP) + FACY * CY(IP_GLOB)*IOBDP_LOC(IP)
        ENDIF
#ifdef W3_MGP
        CXY(1,IP) = CXY(1,IP) - CCURX*VGX/CLATS(ISEA)
        CXY(2,IP) = CXY(2,IP) - CCURY*VGY
#endif
      ENDDO

      DO IE = 1, NE
        NI = INE(:,IE)
        CXYY(1,:) = CXY(1,NI)
        CXYY(2,:) = CXY(2,NI)
        FL11 = CXYY(1,2)*PDLIB_IEN(1,IE)+CXYY(2,2)*PDLIB_IEN(2,IE)
        FL12 = CXYY(1,3)*PDLIB_IEN(1,IE)+CXYY(2,3)*PDLIB_IEN(2,IE)
        FL21 = CXYY(1,3)*PDLIB_IEN(3,IE)+CXYY(2,3)*PDLIB_IEN(4,IE)
        FL22 = CXYY(1,1)*PDLIB_IEN(3,IE)+CXYY(2,1)*PDLIB_IEN(4,IE)
        FL31 = CXYY(1,1)*PDLIB_IEN(5,IE)+CXYY(2,1)*PDLIB_IEN(6,IE)
        FL32 = CXYY(1,2)*PDLIB_IEN(5,IE)+CXYY(2,2)*PDLIB_IEN(6,IE)
        CRFS(1) = - ONESIXTH *  (2.0d0 *FL31 + FL32 + FL21 + 2.0d0 * FL22 )
        CRFS(2) = - ONESIXTH *  (2.0d0 *FL32 + 2.0d0 * FL11 + FL12 + FL31 )
        CRFS(3) = - ONESIXTH *  (2.0d0 *FL12 + 2.0d0 * FL21 + FL22 + FL11 )
        LAMBDA(1) = ONESIXTH * SUM(CXYY(1,:))
        LAMBDA(2) = ONESIXTH * SUM(CXYY(2,:))
        K(1)  = LAMBDA(1) * PDLIB_IEN(1,IE) + LAMBDA(2) * PDLIB_IEN(2,IE)
        K(2)  = LAMBDA(1) * PDLIB_IEN(3,IE) + LAMBDA(2) * PDLIB_IEN(4,IE)
        K(3)  = LAMBDA(1) * PDLIB_IEN(5,IE) + LAMBDA(2) * PDLIB_IEN(6,IE)
        KP(1:3,IE) = MAX(ZERO,K(1:3))
        DELTAL(1:3,IE) = (CRFS(1:3) - KP(1:3,IE)) * 1.d0/MIN(-THR,SUM(MIN(ZERO,K(1:3))))
      ENDDO

      J = 0
      DO IP = 1, np
        IB1 = (1-IOBPA_LOC(IP)) * IOBPD_LOC(ITH,IP)
        IB2 = IOBPD_LOC(ITH,IP)
#ifdef W3_REF1
        IBR = (1-IOBP_LOC(IP)) * (1-IOBPD_LOC(ITH,IP)) * (1-IOBPA_LOC(IP))
#endif
        IF (IOBDP_LOC(IP) .eq. 1) THEN
          DO I = 1, PDLIB_CCON(IP)
            J     =  J + 1
            IE    =  PDLIB_IE_CELL2(I,IP)
            POS   =  PDLIB_POS_CELL2(I,IP)
#ifdef W3_DEBUGSRC
            WRITE(740+IAPROC,*) 'I1=', I1, ' PDLIB_I_DIAG=', PDLIB_I_DIAG(IP)
#endif

#ifdef W3_REF1
            IF (IBR == 1) THEN
              DTK               = KP(POS,IE) * DTG
              B_JAC(ISP,IP)     = B_JAC(ISP,IP) + PDLIB_TRIA03(IE) * VA(ISP,IP)
            ELSE
              DTK               = KP(POS,IE) * DTG * IB1
              B_JAC(ISP,IP)     = B_JAC(ISP,IP) + PDLIB_TRIA03(IE) * VA(ISP,IP) * IB2
            ENDIF
#else
            DTK               = KP(POS,IE) * DTG * IB1
            B_JAC(ISP,IP)     = B_JAC(ISP,IP) + PDLIB_TRIA03(IE) * VA(ISP,IP) * IB2
#endif

            I1  =  PDLIB_POSI(1,J)
            I2  =  PDLIB_POSI(2,J)
            I3  =  PDLIB_POSI(3,J)
            IF (FSGEOADVECT) THEN
              ASPAR_JAC(ISP,I1) = ASPAR_JAC(ISP,I1) + PDLIB_TRIA03(IE) + DTK - DTK * DELTAL(POS,IE)
              ASPAR_JAC(ISP,I2) = ASPAR_JAC(ISP,I2)                          - DTK * DELTAL(POS_TRICK(POS,1),IE)
              ASPAR_JAC(ISP,I3) = ASPAR_JAC(ISP,I3)                          - DTK * DELTAL(POS_TRICK(POS,2),IE)
            ELSE
              ASPAR_JAC(ISP,I1) = ASPAR_JAC(ISP,I1) + PDLIB_TRIA03(IE)
            ENDIF
          END DO
        ELSE
          DO I = 1, PDLIB_CCON(IP)
            J = J + 1
            I1                =  PDLIB_POSI(1,J)
            IE                =  PDLIB_IE_CELL2(I,IP)
            ASPAR_JAC(ISP,I1) = ASPAR_JAC(ISP,I1) + PDLIB_TRIA03(IE)
          END DO
          B_JAC(ISP,IP) = 0.
        ENDIF
      END DO
    END DO ! ISP

    call print_memcheck(memunit, 'memcheck_____:'//' WW3_JACOBI SECTION 1')
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'sum(VA)=', sum(VA)
    CALL PrintTotalOffContrib("Offdiag after the geo advection")
#endif
    !/
    !/ End of W3XYPFSN ----------------------------------------------------- /
    !/
  END SUBROUTINE calcARRAY_JACOBI_VEC
  !/ ------------------------------------------------------------------- /
  SUBROUTINE calcARRAY_JACOBI2(DTG,FACX,FACY,VGX,VGY)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for advection part
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
    USE W3SERVMD, only: STRACE
#endif
    !

    USE W3GDATMD, only: NK, NK2, NTH, NSPEC, FACHFA, DMIN
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3GDATMD, only: NSEAL, CLATS
    USE W3GDATMD, only: MAPSTA
    USE W3WDATMD, only: VA, VAOLD
    USE W3ADATMD, only: CG, DW, WN, CX, CY
    USE W3IDATMD, only: FLCUR, FLLEV
    USE W3GDATMD, only: ECOS, ESIN, MAPFS
    USE W3PARALL, only : ONESIXTH, ZERO, THR, IMEM
    use yowElementpool, only: ne, INE
    USE YOWNODEPOOL,    only: PDLIB_IEN, PDLIB_TRIA,                  &
         PDLIB_CCON, PDLIB_POS_CELL2, PDLIB_IE_CELL2, NP, NPA,        &
         PDLIB_IA_P, PDLIB_POSI, PDLIB_IA, PDLIB_NNZ, iplg,           &
         PDLIB_I_DIAG, PDLIB_JA
    USE W3ODATMD, only : IAPROC
#ifdef W3_DB1
    USE W3SDB1MD
    USE W3GDATMD, only: SDBSC
#endif
#ifdef W3_BT1
    USE W3SBT1MD
#endif
#ifdef W3_BT4
    USE W3SBT4MD
#endif
#ifdef W3_BT8
    USE W3SBT8MD
#endif
#ifdef W3_BT9
    USE W3SBT9MD
#endif
#ifdef W3_IC1
    USE W3SIC1MD
#endif
#ifdef W3_IC2
    USE W3SIC2MD
#endif
#ifdef W3_IC3
    USE W3SIC3MD
#endif
#ifdef W3_TR1
    USE W3STR1MD
#endif
    REAL, INTENT(in) :: DTG, FACX, FACY, VGX, VGY
    INTEGER :: IP, ISP, ISEA, IP_glob
    INTEGER :: idx, IS
    INTEGER :: I, J, ITH, IK, J2
    INTEGER :: IE, POS, JSEA
    INTEGER :: I1, I2, I3, NI(3), NI_GLOB(3), NI_ISEA(3)
    INTEGER :: counter
#ifdef W3_REF1
    INTEGER :: eIOBPDR
#endif
    INTEGER :: IP1, IP2, IPP1, IPP2
    REAL  :: DTK, TMP3
    REAL  :: LAMBDA(2)
    REAL  :: FL11, FL12
    REAL  :: FL21, FL22
    REAL  :: FL31, FL32
    REAL  :: CRFS(3), K(3)
    REAL  :: KP(3)
    REAL  :: KM(3), CXY(3,2)
    REAL  :: K1, eSI, eVS, eVD
    REAL  :: eVal1, eVal2, eVal3
    REAL  :: DELTAL(3)
    REAL  :: NM, TRIA03, SIDT
    REAL  :: IEN_LOCAL(6), CG2(NK,NTH)
    REAL  :: CCOS, CSIN
    REAL  :: SPEC(NSPEC), DEPTH

    memunit = 50000+IAPROC

    call print_memcheck(memunit, 'memcheck_____:'//' WW3_JACOBI SECTION 0')

    J = 0
    DO IP = 1, npa
      IP_glob=iplg(IP)
      ISEA=MAPFS(1,IP_glob)
      DO I = 1, PDLIB_CCON(IP)
        J = J + 1
        IE    =  PDLIB_IE_CELL2(I,IP)
        IEN_LOCAL = PDLIB_IEN(:,IE)
        POS   =  PDLIB_POS_CELL2(I,IP)
        I1    =  PDLIB_POSI(1,J)
        I2    =  PDLIB_POSI(2,J)
        I3    =  PDLIB_POSI(3,J)
        IP1   =  INE(POS_TRICK(POS,1),IE)
        IP2   =  INE(POS_TRICK(POS,2),IE)
        IPP1  =  POS_TRICK(POS,1)
        IPP2  =  POS_TRICK(POS,2)
        NI    = INE(:,IE)
        NI_GLOB = iplg(NI)
        NI_ISEA = MAPFS(1,NI_GLOB)
        DO ISP=1,NSPEC
          ITH    = 1 + MOD(ISP-1,NTH)
          IK     = 1 + (ISP-1)/NTH
          CCOS   = FACX * ECOS(ITH)
          CSIN   = FACY * ESIN(ITH)
          CXY(:,1) = CCOS * CG(IK,NI_ISEA) / CLATS(NI_ISEA)
          CXY(:,2) = CSIN * CG(IK,NI_ISEA)
          IF (FLCUR) THEN
            CXY(:,1) = CXY(:,1) + FACX * CX(NI_ISEA)/CLATS(NI_ISEA)
            CXY(:,2) = CXY(:,2) + FACY * CY(NI_ISEA)
          ENDIF
#ifdef W3_MGP
          CXY(:,1) = CXY(:,1) - CCURX*VGX/CLATS(ISEA)
          CXY(:,2) = CXY(:,2) - CCURY*VGY
#endif
          FL11 = CXY(2,1)*IEN_LOCAL(1)+CXY(2,2)*IEN_LOCAL(2)
          FL12 = CXY(3,1)*IEN_LOCAL(1)+CXY(3,2)*IEN_LOCAL(2)
          FL21 = CXY(3,1)*IEN_LOCAL(3)+CXY(3,2)*IEN_LOCAL(4)
          FL22 = CXY(1,1)*IEN_LOCAL(3)+CXY(1,2)*IEN_LOCAL(4)
          FL31 = CXY(1,1)*IEN_LOCAL(5)+CXY(1,2)*IEN_LOCAL(6)
          FL32 = CXY(2,1)*IEN_LOCAL(5)+CXY(2,2)*IEN_LOCAL(6)
          CRFS(1) = - ONESIXTH *  (2.0d0 *FL31 + FL32 + FL21 + 2.0d0 * FL22 )
          CRFS(2) = - ONESIXTH *  (2.0d0 *FL32 + 2.0d0 * FL11 + FL12 + FL31 )
          CRFS(3) = - ONESIXTH *  (2.0d0 *FL12 + 2.0d0 * FL21 + FL22 + FL11 )
          LAMBDA(1) = ONESIXTH * SUM(CXY(:,1))
          LAMBDA(2) = ONESIXTH * SUM(CXY(:,2))
          K(1)  = LAMBDA(1) * IEN_LOCAL(1) + LAMBDA(2) * IEN_LOCAL(2)
          K(2)  = LAMBDA(1) * IEN_LOCAL(3) + LAMBDA(2) * IEN_LOCAL(4)
          K(3)  = LAMBDA(1) * IEN_LOCAL(5) + LAMBDA(2) * IEN_LOCAL(6)
          KP(:) = MAX(ZERO,K(:))
          DELTAL(:) = CRFS(:) - KP(:)
          KM(:) = MIN(ZERO,K(:))
          NM = 1.d0/MIN(-THR,SUM(KM))
          K1 =  KP(POS)
#ifdef W3_REF1
          eIOBPDR=(1-IOBP_LOC(IP))*(1-IOBPD_LOC(ITH,IP))
          IF (eIOBPDR .eq. 1) THEN
            K1=ZERO
          END IF
#endif
          TRIA03 = 1./3. * PDLIB_TRIA(IE)
          DTK    =  K1 * DTG * IOBDP_LOC(IP) * IOBPD_LOC(ITH,IP) * (1-IOBPA_LOC(IP))
          TMP3   =  DTK * NM
          IF (FSGEOADVECT) THEN
            ASPAR_JAC(ISP,I1) = ASPAR_JAC(ISP,I1) + TRIA03 + DTK - TMP3*DELTAL(POS)
            ASPAR_JAC(ISP,I2) = ASPAR_JAC(ISP,I2)                - TMP3*DELTAL(IPP1)
            ASPAR_JAC(ISP,I3) = ASPAR_JAC(ISP,I3)                - TMP3*DELTAL(IPP2)
          ELSE
            ASPAR_JAC(ISP,I1) = ASPAR_JAC(ISP,I1) + TRIA03
          END IF
          B_JAC(ISP,IP) = B_JAC(ISP,IP) + TRIA03 * VA(ISP,IP) * IOBDP_LOC(IP) * IOBPD_LOC(ITH,IP)
        END DO
      END DO
    END DO
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_JACOBI SECTION 1')
    !/
    !/ End of W3XYPFSN ----------------------------------------------------- /
    !/
  END SUBROUTINE calcARRAY_JACOBI2
  !/ ------------------------------------------------------------------- /
  SUBROUTINE calcARRAY_JACOBI3(IP,J,DTG,FACX,FACY,VGX,VGY,ASPAR_DIAG_LOCAL,ASPAR_OFF_DIAG_LOCAL,B_JAC_LOCAL)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for advection part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: NK, NK2, NTH, NSPEC, FACHFA, DMIN
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3GDATMD, only: NSEAL, CLATS
    USE W3GDATMD, only: MAPSTA
    USE W3WDATMD, only: VA, VAOLD
    USE W3ADATMD, only: CG, DW, WN, CX, CY
    USE W3IDATMD, only: FLCUR, FLLEV
    USE W3GDATMD, only: ECOS, ESIN, MAPFS
    USE W3PARALL, only : ONESIXTH, ZERO, THR, ONETHIRD
    use yowElementpool, only: ne, INE
    USE YOWNODEPOOL,    only: PDLIB_IEN, PDLIB_TRIA,                  &
         PDLIB_CCON, NP, NPA, PDLIB_POS_CELL2, PDLIB_IE_CELL2,        &
         PDLIB_IA_P, PDLIB_POSI, PDLIB_IA, PDLIB_NNZ, iplg,           &
         PDLIB_I_DIAG, PDLIB_JA
    USE W3GDATMD, only: IOBP
    USE W3ODATMD, only : IAPROC
#ifdef W3_DB1
    USE W3SDB1MD
    USE W3GDATMD, only: SDBSC
#endif
#ifdef W3_BT1
    USE W3SBT1MD
#endif
#ifdef W3_BT4
    USE W3SBT4MD
#endif
#ifdef W3_BT8
    USE W3SBT8MD
#endif
#ifdef W3_BT9
    USE W3SBT9MD
#endif
#ifdef W3_IC1
    USE W3SIC1MD
#endif
#ifdef W3_IC2
    USE W3SIC2MD
#endif
#ifdef W3_IC3
    USE W3SIC3MD
#endif
#ifdef W3_TR1
    USE W3STR1MD
#endif
    INTEGER, INTENT(IN) :: IP
    INTEGER, INTENT(INOUT) :: J
    REAL, INTENT(in) :: DTG, FACX, FACY, VGX, VGY
    REAL, INTENT(out) :: ASPAR_DIAG_LOCAL(NSPEC), B_JAC_LOCAL(NSPEC), ASPAR_OFF_DIAG_LOCAL(NSPEC)
    INTEGER :: ISP, ISEA, IP_glob, IPP1, IPP2
    INTEGER :: idx, IS, IP1, IP2
    INTEGER :: I, ITH, IK, J2
    INTEGER :: IE, POS, JSEA
    INTEGER :: I1, I2, I3, NI(3), NI_GLOB(3), NI_ISEA(3)
    INTEGER :: counter
#ifdef W3_REF1
    INTEGER :: eIOBPDR
#endif
    REAL :: DTK, TMP3
    REAL :: LAMBDA(2)
    REAL :: FL11, FL12
    REAL :: FL21, FL22
    REAL :: FL31, FL32
    REAL :: CRFS(3), K(3)
    REAL :: KP(3)
    REAL :: KM(3), CXY(3,2)
    REAL :: K1, eSI, eVS, eVD
    REAL :: eVal1, eVal2, eVal3
    REAL :: ien_local(6)
    REAL :: DELTAL(3)
    REAL :: NM
    REAL :: TRIA03, SIDT, CCOS, CSIN
    REAL :: DEPTH

    ASPAR_DIAG_LOCAL     = 0.d0
    B_JAC_LOCAL          = 0.d0
    ASPAR_OFF_DIAG_LOCAL = 0.d0

    IP_glob=iplg(IP)
    DO I = 1, PDLIB_CCON(IP)
      J         = J + 1
      IE    =  PDLIB_IE_CELL2(I,IP)
      IEN_LOCAL = PDLIB_IEN(:,IE)
      POS   =  PDLIB_POS_CELL2(I,IP)
      I1    =  PDLIB_POSI(1,J)
      I2    =  PDLIB_POSI(2,J)
      I3    =  PDLIB_POSI(3,J)
      IP1   =  INE(POS_TRICK(POS,1),IE)
      IP2   =  INE(POS_TRICK(POS,2),IE)
      IPP1  =  POS_TRICK(POS,1)
      IPP2  =  POS_TRICK(POS,2)
      NI    =  INE(:,IE)
      NI_GLOB = iplg(NI)
      NI_ISEA = MAPFS(1,NI_GLOB)

      DO ISP=1,NSPEC
        ITH    = 1 + MOD(ISP-1,NTH)
        IK     = 1 + (ISP-1)/NTH
        CCOS   = FACX * ECOS(ITH)
        CSIN   = FACY * ESIN(ITH)
        CXY(:,1) = CCOS * CG(IK,NI_ISEA) / CLATS(NI_ISEA)
        CXY(:,2) = CSIN * CG(IK,NI_ISEA)
        IF (FLCUR) THEN
          CXY(:,1) = CXY(:,1) + FACX * CX(NI_ISEA)/CLATS(NI_ISEA)
          CXY(:,2) = CXY(:,2) + FACY * CY(NI_ISEA)
        ENDIF

#ifdef W3_MGP
        CXY(:,1) = CXY(:,1) - CCURX*VGX/CLATS(ISEA)
        CXY(:,2) = CXY(:,2) - CCURY*VGY
#endif
        FL11 = CXY(2,1)*IEN_LOCAL(1)+CXY(2,2)*IEN_LOCAL(2)
        FL12 = CXY(3,1)*IEN_LOCAL(1)+CXY(3,2)*IEN_LOCAL(2)
        FL21 = CXY(3,1)*IEN_LOCAL(3)+CXY(3,2)*IEN_LOCAL(4)
        FL22 = CXY(1,1)*IEN_LOCAL(3)+CXY(1,2)*IEN_LOCAL(4)
        FL31 = CXY(1,1)*IEN_LOCAL(5)+CXY(1,2)*IEN_LOCAL(6)
        FL32 = CXY(2,1)*IEN_LOCAL(5)+CXY(2,2)*IEN_LOCAL(6)
        CRFS(1) = - ONESIXTH * (2.0d0 *FL31 + FL32 + FL21 + 2.0d0 * FL22 )
        CRFS(2) = - ONESIXTH * (2.0d0 *FL32 + 2.0d0 * FL11 + FL12 + FL31 )
        CRFS(3) = - ONESIXTH * (2.0d0 *FL12 + 2.0d0 * FL21 + FL22 + FL11 )
        LAMBDA(1) = ONESIXTH * SUM(CXY(:,1))
        LAMBDA(2) = ONESIXTH * SUM(CXY(:,2))
        K(1)  = LAMBDA(1) * IEN_LOCAL(1) + LAMBDA(2) * IEN_LOCAL(2)
        K(2)  = LAMBDA(1) * IEN_LOCAL(3) + LAMBDA(2) * IEN_LOCAL(4)
        K(3)  = LAMBDA(1) * IEN_LOCAL(5) + LAMBDA(2) * IEN_LOCAL(6)
        KP(:) = MAX(ZERO,K(:))
        DELTAL(:) = CRFS(:) - KP(:)
        KM(:) = MIN(ZERO,K(:))
        NM = 1.d0/MIN(-THR,SUM(KM))
#ifdef W3_REF1
        eIOBPDR=(1-IOBP_LOC(IP))*(1-IOBPD_LOC(ITH,IP))
        IF (eIOBPDR .eq. 1) THEN
          K1=ZERO
        END IF
#endif
        TRIA03 = ONETHIRD * PDLIB_TRIA(IE)
        DTK    =  KP(POS) * DBLE(DTG) * IOBDP_LOC(IP) * IOBPD_LOC(ITH,IP) * (1-IOBPA_LOC(IP))
        TMP3   =  DTK * NM
        IF (FSGEOADVECT) THEN
          ASPAR_DIAG_LOCAL(ISP)     = ASPAR_DIAG_LOCAL(ISP)    + TRIA03 + DTK   - TMP3*DELTAL(POS)
          ASPAR_OFF_DIAG_LOCAL(ISP) = ASPAR_OFF_DIAG_LOCAL(ISP)                 - TMP3*DELTAL(IPP1)*VA(ISP,IP1)
          ASPAR_OFF_DIAG_LOCAL(ISP) = ASPAR_OFF_DIAG_LOCAL(ISP)                 - TMP3*DELTAL(IPP2)*VA(ISP,IP2)
        ELSE
          ASPAR_DIAG_LOCAL(ISP) = ASPAR_DIAG_LOCAL(ISP) + TRIA03
        END IF
        B_JAC_LOCAL(ISP) = B_JAC_LOCAL(ISP) + TRIA03 * VAOLD(ISP,IP) * IOBDP_LOC(IP) * IOBPD_LOC(ITH,IP)
      END DO
    END DO
    !/
    !/ End of W3XYPFSN --------------------------------------------------- /
    !/
  END SUBROUTINE calcARRAY_JACOBI3
  !/ ------------------------------------------------------------------- /
  SUBROUTINE calcARRAY_JACOBI4(IP,DTG,FACX,FACY,VGX,VGY,ASPAR_DIAG_LOCAL,ASPAR_OFF_DIAG_LOCAL,B_JAC_LOCAL)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for advection part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: NK, NK2, NTH, NSPEC, FACHFA, DMIN
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3GDATMD, only: NSEAL,CLATS
    USE W3GDATMD, only: MAPSTA, NK
    USE W3WDATMD, only: VA, VAOLD
    USE W3ADATMD, only: CG, DW, WN, CX, CY
    USE W3IDATMD, only: FLCUR, FLLEV
    USE W3GDATMD, only: ECOS, ESIN, MAPFS
    USE W3PARALL, only : ONESIXTH, ZERO, THR, ONETHIRD
    use yowElementpool, only: ne, INE
    USE YOWNODEPOOL,    only: PDLIB_IEN, PDLIB_TRIA,                  &
         PDLIB_IE_CELL2, PDLIB_POS_CELL2, PDLIB_CCON, NP, NPA,          &
         PDLIB_IA_P, PDLIB_POSI, PDLIB_IA, PDLIB_NNZ, iplg,           &
         PDLIB_I_DIAG, PDLIB_JA
    USE W3ODATMD, only : IAPROC
#ifdef W3_DB1
    USE W3SDB1MD
    USE W3GDATMD, only: SDBSC
#endif
#ifdef W3_BT1
    USE W3SBT1MD
#endif
#ifdef W3_BT4
    USE W3SBT4MD
#endif
#ifdef W3_BT8
    USE W3SBT8MD
#endif
#ifdef W3_BT9
    USE W3SBT9MD
#endif
#ifdef W3_IC1
    USE W3SIC1MD
#endif
#ifdef W3_IC2
    USE W3SIC2MD
#endif
#ifdef W3_IC3
    USE W3SIC3MD
#endif
#ifdef W3_TR1
    USE W3STR1MD
#endif
    INTEGER, INTENT(IN) :: IP
    REAL, INTENT(in) :: DTG, FACX, FACY, VGX, VGY
    REAL, INTENT(out) :: ASPAR_DIAG_LOCAL(NSPEC), B_JAC_LOCAL(NSPEC), ASPAR_OFF_DIAG_LOCAL(NSPEC)
    !
    INTEGER :: IP1, IP2
    INTEGER :: ITH, IK
    INTEGER :: IE, POS, JSEA
    INTEGER :: I, I1, I2, I3, NI(3), NI_GLOB(3), NI_ISEA(3)
    INTEGER :: ISP, IP_glob, IPP1, IPP2, IOBPTH1(NTH), IOBPTH2(NTH)
    INTEGER :: counter
#ifdef W3_REF1
    INTEGER :: eIOBPDR
#endif
    REAL  :: DTK, TMP3, D1, D2
    REAL  :: LAMBDA(2)
    REAL  :: CRFS(3), K(3)
    REAL  :: KP(3), UV_CUR(3,2)
    REAL  :: KM(3), CSX(3), CSY(3)
    REAL  :: K1, eSI, eVS, eVD
    REAL  :: eVal1, eVal2, eVal3
    REAL  :: ien_local(6)
    REAL  :: DELTAL(3), K_X(3,NK), K_Y(3,NK), K_U(3)
    REAL  :: CRFS_X(3,NK), CRFS_Y(3,NK), CRFS_U(3)
    REAL  :: NM, CGFAK(3,NK), CSINA(NTH), CCOSA(NTH)
    REAL  :: TRIA03, SIDT, CCOS, CSIN
    REAL  :: FL11_X, FL12_X, FL21_X, FL22_X, FL31_X, FL32_X
    REAL  :: FL11_Y, FL12_Y, FL21_Y, FL22_Y, FL31_Y, FL32_Y
    REAL  :: FL11_U, FL12_U, FL21_U, FL22_U, FL31_U, FL32_U

    IP_glob              = iplg(IP)
    ASPAR_DIAG_LOCAL     = ZERO
    B_JAC_LOCAL          = ZERO
    ASPAR_OFF_DIAG_LOCAL = ZERO

    DO ITH = 1, NTH
      CCOSA(ITH) = FACX * ECOS(ITH)
      CSINA(ITH) = FACX * ESIN(ITH)
      IOBPTH1(ITH) = IOBDP_LOC(IP) * (1-IOBPA_LOC(IP)) * IOBPD_LOC(ITH,IP)
      IOBPTH2(ITH) = IOBDP_LOC(IP) * IOBPD_LOC(ITH,IP)
    ENDDO

    DO I = 1, PDLIB_CCON(IP)

      IE        = PDLIB_IE_CELL2(I,IP)
      TRIA03    = ONETHIRD * PDLIB_TRIA(IE)
      IEN_LOCAL = PDLIB_IEN(1:6,IE)
      POS       = PDLIB_POS_CELL2(I,IP)
      IP1       = INE(POS_TRICK(POS,1),IE)
      IP2       = INE(POS_TRICK(POS,2),IE)
      IPP1      = POS_TRICK(POS,1)
      IPP2      = POS_TRICK(POS,2)
      NI        = INE(1:3,IE)
      NI_GLOB   = IPLG(NI)
      NI_ISEA   = MAPFS(1,NI_GLOB)
      CRFS_U    = ZERO

      IF (FLCUR) THEN

        UV_CUR(1:3,1) = FACX * CX(NI_ISEA) / CLATS(NI_ISEA)
        UV_CUR(1:3,2) = FACY * CY(NI_ISEA)

        LAMBDA(1) = ONESIXTH*(UV_CUR(1,1)+UV_CUR(2,1)+UV_CUR(3,1))
        LAMBDA(2) = ONESIXTH*(UV_CUR(1,2)+UV_CUR(2,2)+UV_CUR(3,2))

        K_U(1)  = LAMBDA(1) * IEN_LOCAL(1) + LAMBDA(2) * IEN_LOCAL(2)
        K_U(2)  = LAMBDA(1) * IEN_LOCAL(3) + LAMBDA(2) * IEN_LOCAL(4)
        K_U(3)  = LAMBDA(1) * IEN_LOCAL(5) + LAMBDA(2) * IEN_LOCAL(6)

        FL11_U  = UV_CUR(2,1)*IEN_LOCAL(1)+UV_CUR(2,2)*IEN_LOCAL(2)
        FL12_U  = UV_CUR(3,1)*IEN_LOCAL(1)+UV_CUR(3,2)*IEN_LOCAL(2)
        FL21_U  = UV_CUR(3,1)*IEN_LOCAL(3)+UV_CUR(3,2)*IEN_LOCAL(4)
        FL22_U  = UV_CUR(1,1)*IEN_LOCAL(3)+UV_CUR(1,2)*IEN_LOCAL(4)
        FL31_U  = UV_CUR(1,1)*IEN_LOCAL(5)+UV_CUR(1,2)*IEN_LOCAL(6)
        FL32_U  = UV_CUR(2,1)*IEN_LOCAL(5)+UV_CUR(2,2)*IEN_LOCAL(6)

        CRFS_U(1) = - ONESIXTH*(2.d0 *FL31_U + FL32_U + FL21_U + 2.d0 * FL22_U)
        CRFS_U(2) = - ONESIXTH*(2.d0 *FL32_U + 2.d0 * FL11_U + FL12_U + FL31_U)
        CRFS_U(3) = - ONESIXTH*(2.d0 *FL12_U + 2.d0 * FL21_U + FL22_U + FL11_U)

      ENDIF

      DO IK = 1, NK
        CSX = CG(IK,NI_ISEA) / CLATS(NI_ISEA)
        CSY = CG(IK,NI_ISEA)
        LAMBDA(1) = ONESIXTH * (CSX(1) + CSX(2) + CSX(3))
        LAMBDA(2) = ONESIXTH * (CSY(1) + CSY(2) + CSY(3))
        K_X(1,IK) = LAMBDA(1) * IEN_LOCAL(1)
        K_X(2,IK) = LAMBDA(1) * IEN_LOCAL(3)
        K_X(3,IK) = LAMBDA(1) * IEN_LOCAL(5)
        K_Y(1,IK) = LAMBDA(2) * IEN_LOCAL(2)
        K_Y(2,IK) = LAMBDA(2) * IEN_LOCAL(4)
        K_Y(3,IK) = LAMBDA(2) * IEN_LOCAL(6)
        FL11_X = CSX(2) * IEN_LOCAL(1)
        FL12_X = CSX(3) * IEN_LOCAL(1)
        FL21_X = CSX(3) * IEN_LOCAL(3)
        FL22_X = CSX(1) * IEN_LOCAL(3)
        FL31_X = CSX(1) * IEN_LOCAL(5)
        FL32_X = CSX(2) * IEN_LOCAL(5)
        FL11_Y = CSY(2) * IEN_LOCAL(2)
        FL12_Y = CSY(3) * IEN_LOCAL(2)
        FL21_Y = CSY(3) * IEN_LOCAL(4)
        FL22_Y = CSY(1) * IEN_LOCAL(4)
        FL31_Y = CSY(1) * IEN_LOCAL(6)
        FL32_Y = CSY(2) * IEN_LOCAL(6)
        CRFS_X(1,IK) = - ONESIXTH*(2.d0*FL31_X + FL32_X + FL21_X + 2.d0 * FL22_X)
        CRFS_X(2,IK) = - ONESIXTH*(2.d0*FL32_X + 2.d0 * FL11_X + FL12_X + FL31_X)
        CRFS_X(3,IK) = - ONESIXTH*(2.d0*FL12_X + 2.d0 * FL21_X + FL22_X + FL11_X)
        CRFS_Y(1,IK) = - ONESIXTH*(2.d0*FL31_Y + FL32_Y + FL21_Y + 2.d0 * FL22_Y)
        CRFS_Y(2,IK) = - ONESIXTH*(2.d0*FL32_Y + 2.d0 * FL11_Y + FL12_Y + FL31_Y)
        CRFS_Y(3,IK) = - ONESIXTH*(2.d0*FL12_Y + 2.d0 * FL21_Y + FL22_Y + FL11_Y)
      ENDDO

      DO ISP = 1, NSPEC
        ITH     = 1 + MOD(ISP-1,NTH)
        IK      = 1 + (ISP-1)/NTH
        K(1)     = K_X(1,IK) * CCOSA(ITH) + K_Y(1,IK) * CSINA(ITH) + K_U(1)
        K(2)     = K_X(2,IK) * CCOSA(ITH) + K_Y(2,IK) * CSINA(ITH) + K_U(2)
        K(3)     = K_X(3,IK) * CCOSA(ITH) + K_Y(3,IK) * CSINA(ITH) + K_U(3)
        CRFS(1)  = CRFS_X(1,IK) * CCOSA(ITH) + CRFS_Y(1,IK) * CSINA(ITH) + CRFS_U(1)
        CRFS(2)  = CRFS_X(2,IK) * CCOSA(ITH) + CRFS_Y(2,IK) * CSINA(ITH) + CRFS_U(2)
        CRFS(3)  = CRFS_X(3,IK) * CCOSA(ITH) + CRFS_Y(3,IK) * CSINA(ITH) + CRFS_U(3)
        !KM      = MIN(ZERO,K)
        KP(1:3)  = MAX(ZERO,K(1:3))
        DELTAL(1:3) = CRFS(1:3) - KP(1:3)
        !NM      = 1.d0/MIN(-THR,SUM(MIN(ZERO,K)))
        DTK     = KP(POS) * DTG * IOBPTH1(ITH)!IOBDP(IP_glob) * (1-IOBPA(IP_glob)) * IOBPD(ITH,IP_glob)
        TMP3    = DTK * 1.d0/MIN(-THR,SUM(MIN(ZERO,K(1:3))))
        IF (FSGEOADVECT) THEN
          ASPAR_DIAG_LOCAL(ISP)     = ASPAR_DIAG_LOCAL(ISP) + TRIA03 + DTK - TMP3*DELTAL(POS)
          D1                        = DELTAL(IPP1)*VA(ISP,IP1)
          D2                        = DELTAL(IPP2)*VA(ISP,IP2)
          ASPAR_OFF_DIAG_LOCAL(ISP) = ASPAR_OFF_DIAG_LOCAL(ISP) - ( TMP3 * ( D1 + D2 )  )
          !ASPAR_OFF_DIAG_LOCAL(ISP) = ASPAR_OFF_DIAG_LOCAL(ISP)            - D2
        ELSE
          ASPAR_DIAG_LOCAL(ISP)     = ASPAR_DIAG_LOCAL(ISP) + TRIA03
        END IF
        B_JAC_LOCAL(ISP) = B_JAC_LOCAL(ISP) + TRIA03 * VAOLD(ISP,IP) * IOBPTH2(ITH)!IOBDP(IP_glob) * IOBPD(ITH,IP_glob)
      END DO
    END DO
  END SUBROUTINE calcARRAY_JACOBI4
  !/ ------------------------------------------------------------------- /
  SUBROUTINE calcARRAY_JACOBI_SPECTRAL_1(DTG)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for spectral part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: FSREFRACTION, FSFREQSHIFT, FACHFA
    USE W3ODATMD, only : IAPROC
    USE YOWNODEPOOL, only: np, iplg, PDLIB_SI, PDLIB_I_DIAG
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3IDATMD, only: FLLEV, FLCUR
    USE W3GDATMD, only: NK, NK2, NTH, NSPEC, MAPFS, DMIN, DSIP, NSEAL
    USE W3PARALL, only : PROP_REFRACTION_PR3, PROP_REFRACTION_PR1, PROP_FREQ_SHIFT, PROP_FREQ_SHIFT_M2, ZERO, IMEM
    USE W3ADATMD, only: CG, DW

    REAL, INTENT(in) :: DTG
    INTEGER IP, IP_glob, ITH, IK
    INTEGER ISEA, ISP
    REAL ::  eSI
    REAL  :: B_SIG(NSPEC), B_THE(NSPEC)
    REAL  :: CP_SIG(NSPEC), CM_SIG(NSPEC)
    REAL  :: CP_THE(NSPEC), CM_THE(NSPEC)
    REAL  :: CAD(NSPEC), CAS(NSPEC)
    REAL  :: DMM(0:NK2), eVal
    REAL  :: DWNI_M2(NK), CWNB_M2(1-NTH:NSPEC)
    LOGICAL :: DoLimiterRefraction = .FALSE.
    LOGICAL :: DoLimiterFreqShit   = .FALSE. !AR: This one is missing ...
    INTEGER :: ITH0

    LOGICAL :: LSIG = .FALSE.
    !AR: TODO: check&report if needed ...
    LSIG = FLCUR .OR. FLLEV

    DO IP = 1, np
      IP_glob=iplg(IP)
      ISEA=MAPFS(1,IP_glob)
      eSI=PDLIB_SI(IP)
      IF (FSFREQSHIFT .AND. LSIG) THEN
        IF (FreqShiftMethod .eq. 1) THEN
          IF (IOBP_LOC(IP).eq.1.and.IOBDP_LOC(IP).eq.1.and.IOBPA_LOC(IP).eq.0) THEN
            CALL PROP_FREQ_SHIFT(IP, ISEA, CAS, DMM, DTG)
            CP_SIG = MAX(ZERO,CAS)
            CM_SIG = MIN(ZERO,CAS)
            B_SIG=0
            DO ITH=1,NTH
              DO IK=1,NK
                ISP=ITH + (IK-1)*NTH
                B_SIG(ISP)= CP_SIG(ISP)/DMM(IK-1) - CM_SIG(ISP)/DMM(IK)
              END DO
              ISP  = ITH + (NK-1)*NTH
              B_SIG(ISP)= B_SIG(ISP) + CM_SIG(ISP)/DMM(NK) * FACHFA
            END DO
            ASPAR_JAC(:,PDLIB_I_DIAG(IP))=ASPAR_JAC(:,PDLIB_I_DIAG(IP)) + B_SIG(:)*eSI
          ELSE
            CAS=0
          END IF
          CAS_SIG(:,IP) = CAS
        ELSE IF (FreqShiftMethod .eq. 2) THEN
          IF (IOBP_LOC(IP).eq.1.and.IOBDP_LOC(IP).eq.1.and.IOBPA_LOC(IP).eq.0) THEN
            CALL PROP_FREQ_SHIFT_M2(IP, ISEA, CWNB_M2, DWNI_M2, DTG)
#ifdef W3_DEBUGFREQSHIFT
            WRITE(740+IAPROC,*) 'sum(CWNB_M2)=', sum(CWNB_M2)
#endif
            DO ITH=1,NTH
              DO IK=1,NK
                ISP = ITH + (IK-1)*NTH
                eVal = DWNI_M2(IK) * ( MIN(CWNB_M2(ISP - NTH), ZERO) - MAX(CWNB_M2(ISP),ZERO) )
                ASPAR_JAC(ISP,PDLIB_I_DIAG(IP)) =  ASPAR_JAC(ISP,PDLIB_I_DIAG(IP)) - eSI * eVal
              END DO
              eVal = DWNI_M2(NK) * MIN(CWNB_M2(ITH + (NK-1)*NTH), ZERO) * FACHFA
              ITH0 = NSPEC - NTH
              ASPAR_JAC(ITH0 + ITH,PDLIB_I_DIAG(IP)) = ASPAR_JAC(ITH0 + ITH,PDLIB_I_DIAG(IP)) + eSI * eVal
            END DO
          ELSE
            CWNB_M2 = 0
          END IF
          CWNB_SIG_M2(:,IP)=CWNB_M2
        END IF
      END IF
      !
      ! The refraction
      !
      IF (FSREFRACTION) THEN
        IF (IOBP_LOC(IP) .eq. 1 .and. IOBDP_LOC(IP).eq.1.and.IOBPA_LOC(IP).eq.0) THEN
          !    CALL PROP_REFRACTION_PR1(ISEA,DTG,CAD) !AR: Check statuts ...
          !    CALL PROP_REFRACTION_PR3(ISEA,DTG,CAD, DoLimiterRefraction)
          CALL PROP_REFRACTION_PR3(IP,ISEA,DTG,CAD,DoLimiterRefraction)
        ELSE
          CAD=ZERO
        END IF
#ifdef W3_DEBUGREFRACTION
        WRITE(740+IAPROC,*) 'refraction IP=', IP, ' ISEA=', ISEA
        WRITE(740+IAPROC,*) 'sum(abs(CAD))=', sum(abs(CAD))
#endif
        CAD_THE(:,IP)=CAD
        CP_THE = DTG*MAX(ZERO,CAD)
        CM_THE = DTG*MIN(ZERO,CAD)
        B_THE(:) = CP_THE(:) - CM_THE(:)
        ASPAR_JAC(:,PDLIB_I_DIAG(IP))=ASPAR_JAC(:,PDLIB_I_DIAG(IP)) + B_THE(:)*eSI
      END IF
    END DO
  END SUBROUTINE calcARRAY_JACOBI_SPECTRAL_1
  !/ ------------------------------------------------------------------- /
  SUBROUTINE calcARRAY_JACOBI_SPECTRAL_2(DTG,ASPAR_DIAG_LOCAL)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for spectral part
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

    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3GDATMD, only: FSREFRACTION, FSFREQSHIFT, FACHFA
    USE W3ODATMD, only : IAPROC
    USE YOWNODEPOOL, only: np, iplg, PDLIB_SI, PDLIB_I_DIAG
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3IDATMD, only: FLLEV, FLCUR
    USE W3GDATMD, only: NK, NK2, NTH, NSPEC, MAPFS, DMIN, DSIP, NSEAL, MAPSTA
    USE W3PARALL, only : PROP_REFRACTION_PR3, PROP_REFRACTION_PR1, PROP_FREQ_SHIFT, PROP_FREQ_SHIFT_M2, ZERO, IMEM
    USE W3ADATMD, only: CG, DW

    REAL, INTENT(in) :: DTG
    REAL, INTENT(inout) :: ASPAR_DIAG_LOCAL(nspec,NSEAL)
    INTEGER IP, IP_glob, ITH, IK
    INTEGER ISEA, ISP
    REAL ::  eSI
    REAL  :: B_SIG(NSPEC), B_THE(NSPEC)
    REAL  :: CP_SIG(NSPEC), CM_SIG(NSPEC)
    REAL  :: CP_THE(NSPEC), CM_THE(NSPEC)
    REAL  :: CAD(NSPEC), CAS(NSPEC)
    REAL  :: DMM(0:NK2), eVal
    REAL  :: DWNI_M2(NK), CWNB_M2(1-NTH:NSPEC)
    LOGICAL :: DoLimiterRefraction = .FALSE.
    LOGICAL :: DoLimiterFreqShit   = .FALSE. !AR: This one is missing ...
    INTEGER :: ITH0

    LOGICAL :: LSIG = .FALSE.
    LSIG = FLCUR .OR. FLLEV

    DO IP = 1, np

      IP_glob=iplg(IP)
      ISEA=MAPFS(1,IP_glob)
      eSI=PDLIB_SI(IP)
      !
      ! The frequency shifting
      !
      IF (FSFREQSHIFT .AND. LSIG) THEN
        IF (FreqShiftMethod .eq. 1) THEN
          IF (IOBP_LOC(IP).eq.1.and.IOBDP_LOC(IP).eq.1.and.IOBPA_LOC(IP).eq.0) THEN
            CALL PROP_FREQ_SHIFT(IP, ISEA, CAS, DMM, DTG)
            CP_SIG = MAX(ZERO,CAS)
            CM_SIG = MIN(ZERO,CAS)
            B_SIG=0
            DO ITH=1,NTH
              DO IK=1,NK
                ISP=ITH + (IK-1)*NTH
                B_SIG(ISP)= CP_SIG(ISP)/DMM(IK-1) - CM_SIG(ISP)/DMM(IK)
              END DO
              ISP  = ITH + (NK-1)*NTH
              B_SIG(ISP)= B_SIG(ISP) + CM_SIG(ISP)/DMM(NK) * FACHFA
            END DO
            ASPAR_DIAG_LOCAL(:,IP) = ASPAR_DIAG_LOCAL(:,IP) + B_SIG * eSI
          ELSE
            CAS = 0
          END IF
          CAS_SIG(:,IP) = CAS
        END IF

        IF (FreqShiftMethod .eq. 2) THEN
          IF (IOBP_LOC(IP).eq.1) THEN
            CALL PROP_FREQ_SHIFT_M2(IP, ISEA, CWNB_M2, DWNI_M2, DTG)
#ifdef W3_DEBUGFREQSHIFT
            WRITE(740+IAPROC,*) 'sum(CWNB_M2)=', sum(CWNB_M2)
#endif
            DO ITH=1,NTH
              DO IK=1,NK
                ISP = ITH + (IK-1)*NTH
                eVal = DWNI_M2(IK) * ( MIN(CWNB_M2(ISP - NTH), ZERO) - MAX(CWNB_M2(ISP),ZERO) )
                IF (IMEM == 1) THEN
                  ASPAR_JAC(ISP,PDLIB_I_DIAG(IP)) =  ASPAR_JAC(ISP,PDLIB_I_DIAG(IP)) - eSI * eVal
                ELSE IF (IMEM == 2) THEN
                  ASPAR_DIAG_LOCAL(ISP,IP) =  ASPAR_DIAG_LOCAL(ISP,IP) - eSI * eVal
                ENDIF
              END DO
              eVal = DWNI_M2(NK) * MIN(CWNB_M2(ITH + (NK-1)*NTH), ZERO) * FACHFA
              ITH0 = NSPEC - NTH
              ASPAR_DIAG_LOCAL(ITH0 + ITH,IP) = ASPAR_DIAG_LOCAL(ITH0 + ITH,IP) + eSI * eVal
            END DO
          ELSE
            CWNB_M2=0
          END IF
          CWNB_SIG_M2(:,IP)=CWNB_M2
        END IF
      END IF
      !
      IF (FSREFRACTION) THEN
        IF (IOBP_LOC(IP) .eq. 1.and.IOBDP_LOC(IP).eq.1.and.IOBPA_LOC(IP).eq.0) THEN
          !    CALL PROP_REFRACTION_PR1(ISEA,DTG,CAD) !AR: Is this working?
          !    CALL PROP_REFRACTION_PR3(ISEA,DTG,CAD, DoLimiterRefraction)
          CALL PROP_REFRACTION_PR3(IP,ISEA,DTG,CAD,DoLimiterRefraction)
        ELSE
          CAD=ZERO
        END IF
#ifdef W3_DEBUGREFRACTION
        WRITE(740+IAPROC,*) 'refraction IP=', IP, ' ISEA=', ISEA
        WRITE(740+IAPROC,*) 'sum(abs(CAD))=', sum(abs(CAD))
#endif
        CAD_THE(:,IP)=CAD
        CP_THE = DTG*MAX(ZERO,CAD)
        CM_THE = DTG*MIN(ZERO,CAD)
        B_THE(:) = CP_THE(:) - CM_THE(:)
        ASPAR_DIAG_LOCAL(:,IP) = ASPAR_DIAG_LOCAL(:,IP) + B_THE(:)*eSI
      END IF

    END DO
  END SUBROUTINE calcARRAY_JACOBI_SPECTRAL_2
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CALCARRAY_JACOBI_SOURCE_1(DTG)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for source part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3ODATMD, only : IAPROC
    USE YOWNODEPOOL, only: iplg, PDLIB_SI, PDLIB_I_DIAG, NPA, NP
    USE W3ADATMD, only: CG, DW, WN
    USE W3WDATMD, only: UST, USTDIR
    USE W3GDATMD, only: NK, NTH, NSPEC, MAPFS, optionCall, DMIN
    USE W3GDATMD, only: MAPSTA, FACP, SIG
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3PARALL, only: IMEM
    USE W3GDATMD, only: NSEAL, CLATS
#ifdef W3_DB1
    USE W3SDB1MD
    USE W3GDATMD, only: SDBSC
#endif
#ifdef W3_DB2
    USE W3SDB2MD
#endif
    USE W3WDATMD, only: VA, VSTOT, VDTOT, SHAVETOT
    USE constants, only : TPI, TPIINV, GRAV

    REAL, INTENT(in) :: DTG
    REAL, PARAMETER :: COEF4 = 5.0E-07
    REAL, PARAMETER :: FACDAM = 1
    INTEGER JSEA, IP, IP_glob, ISEA
    INTEGER IK, ITH, ISP, IS0
    LOGICAL :: LBREAK
    REAL ::  eSI, eVS, eVD, SIDT
    REAL :: DEPTH, DAM(NSPEC), RATIO, MAXDAC, VSDB(NSPEC), VDDB(NSPEC)
    REAL :: PreVS, eDam, DVS, FREQ, EMEAN, FMEAN, WNMEAN, AMAX, CG1(NK),WN1(NK),SPEC_VA(NSPEC)
    REAL TheFactor

    DO JSEA = 1, NP

      IP      = JSEA
      IP_glob = iplg(IP)
      ISEA    = MAPFS(1,IP_glob)

      IF ((IOBP_LOC(IP).eq.1..or.IOBP_LOC(JSEA).eq. 3).and.IOBDP_LOC(IP).eq.1.and.IOBPA_LOC(IP).eq.0) THEN

        DO IK=1, NK
          DAM(1+(IK-1)*NTH) = FACP / ( SIG(IK) * WN(IK,ISEA)**3 )
        END DO
        DO IK=1, NK
          IS0    = (IK-1)*NTH
          DO ITH=2, NTH
            DAM(ITH+IS0) = DAM(1+IS0)
          END DO
        END DO

        eSI    = PDLIB_SI(IP)
        SIDT   = eSI * DTG
        DEPTH  = DW(ISEA)
#ifdef W3_DB1
        VSDB   = 0.
        VDDB   = 0.
        CG1 = CG(1:NK,ISEA)
        WN1 = WN(1:NK,ISEA)
        DO IK=1,NK
          DO ITH=1,NTH
            ISP=ITH + (IK-1)*NTH
            SPEC_VA(ISP) = VA(ISP,JSEA) * CG(IK,ISEA) / CLATS(ISEA)
          ENDDO
        ENDDO
        CALL COMPUTE_MEAN_PARAM(SPEC_VA, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
        SELECT CASE (NINT(SDBSC))
        CASE(1)
          CALL W3SDB1 ( JSEA, SPEC_VA, DEPTH, EMEAN, FMEAN, WNMEAN, CG1, LBREAK, VSDB, VDDB )
        CASE(2)
          !CALL W3SDB2 ( JSEA, SPEC_VA, DEPTH, EMEAN, FMEAN, CG1, LBREAK, VSDB, VDDB )
        END SELECT
#endif
#ifdef W3_DB2
        VSDB   = 0.
        VDDB   = 0.
        CG1 = CG(1:NK,ISEA)
        WN1 = WN(1:NK,ISEA)
        DO IK=1,NK
          DO ITH=1,NTH
            ISP=ITH + (IK-1)*NTH
            SPEC_VA(ISP) = VA(ISP,JSEA) * CG(IK,ISEA) / CLATS(ISEA)
          ENDDO
        ENDDO
        CALL COMPUTE_MEAN_PARAM(SPEC_VA, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
        CALL W3SDB2 ( JSEA, SPEC_VA, DEPTH, EMEAN, FMEAN, CG1, LBREAK, VSDB, VDDB )
#endif
        DO IK=1,NK
          DO ITH=1,NTH
            ISP=ITH + (IK-1)*NTH
            IF (SHAVETOT(JSEA)) THEN ! Limit only the source term part ...
              MAXDAC    = FACDAM * DAM(ISP)
              TheFactor = DTG / MAX ( 1. , (1.-DTG*VDTOT(ISP,JSEA)))
              DVS       = VSTOT(ISP,JSEA) * TheFactor
              DVS       = SIGN(MIN(MAXDAC,ABS(DVS)),DVS)
              PreVS     = DVS / TheFactor
            ELSE
              PreVS     = VSTOT(ISP,JSEA)
            END IF
            eVS = PreVS * CLATS(ISEA) / CG(IK,ISEA)
            eVD = DBLE(VDTOT(ISP,JSEA))
#ifdef W3_DB1
            eVS = eVS + DBLE(VSDB(ISP)) / CG(IK,ISEA) * CLATS(ISEA)
            eVD = evD + DBLE(VDDB(ISP))
#endif
#ifdef W3_DB2
            eVS = eVS + DBLE(VSDB(ISP)) / CG(IK,ISEA) * CLATS(ISEA)
            eVD = evD + DBLE(VDDB(ISP))
#endif
            B_JAC(ISP,IP)                   = B_JAC(ISP,IP) + SIDT * (eVS - eVD*VA(ISP,JSEA))
            ASPAR_JAC(ISP,PDLIB_I_DIAG(IP)) = ASPAR_JAC(ISP,PDLIB_I_DIAG(IP)) - SIDT * eVD
          END DO
        END DO
      END IF
    END DO
  END SUBROUTINE CALCARRAY_JACOBI_SOURCE_1
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CALCARRAY_JACOBI_SOURCE_2(DTG,ASPAR_DIAG_LOCAL)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Compute matrix coefficients for source part
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE W3ODATMD, only : IAPROC
    USE YOWNODEPOOL, only: iplg, PDLIB_SI, PDLIB_I_DIAG, NPA, NP
    USE W3ADATMD, only: CG, DW, WN
    USE W3WDATMD, only: UST, USTDIR
    USE W3GDATMD, only: NK, NTH, NSPEC, MAPFS, optionCall, DMIN
    USE W3GDATMD, only: IOBP, MAPSTA, FACP, SIG, IOBPD, IOBPA, IOBDP
    USE W3PARALL, only: IMEM
    USE W3GDATMD, only: NSEAL, CLATS
#ifdef W3_DB1
    USE W3SDB1MD
    USE W3GDATMD, only: SDBSC
#endif
#ifdef W3_DB2
    USE W3SDB2MD
#endif
    USE W3WDATMD, only: VA, VSTOT, VDTOT, SHAVETOT
    USE constants, only : TPI, TPIINV, GRAV

    REAL, INTENT(in) :: DTG
    REAL, INTENT(inout) :: ASPAR_DIAG_LOCAL(:,:)
    REAL, PARAMETER :: COEF4 = 5.0E-07
    REAL, PARAMETER :: FACDAM = 1
    INTEGER JSEA, IP, IP_glob, ISEA
    INTEGER IK, ITH, ISP, IS0
    LOGICAL :: LBREAK
    REAL ::  eSI, eVS, eVD, SIDT
    REAL :: DEPTH, DAM(NSPEC), RATIO, MAXDAC, VSDB(NSPEC), VDDB(NSPEC)
    REAL :: PreVS, eDam, DVS, FREQ, EMEAN, FMEAN, WNMEAN, AMAX, CG1(NK),WN1(NK),SPEC_VA(NSPEC)
    REAL TheFactor

    DO JSEA = 1, NP

      IP      = JSEA
      IP_glob = iplg(IP)
      ISEA    = MAPFS(1,IP_glob)

      IF (IOBP(IP_glob).eq.1..and.IOBDP(IP_glob).eq.1.and.IOBPA(IP_glob).eq.0) THEN
        DO IK=1, NK
          DAM(1+(IK-1)*NTH) = FACP / ( SIG(IK) * WN(IK,ISEA)**3 )
        END DO
        DO IK=1, NK
          IS0    = (IK-1)*NTH
          DO ITH=2, NTH
            DAM(ITH+IS0) = DAM(1+IS0)
          END DO
        END DO
        eSI    = PDLIB_SI(IP)
        SIDT   = eSI * DTG
        DEPTH  = DW(ISEA)
#ifdef W3_DB1
        VSDB   = 0.
        VDDB   = 0.
        CG1 = CG(1:NK,ISEA)
        WN1 = WN(1:NK,ISEA)
        DO IK=1,NK
          DO ITH=1,NTH
            ISP=ITH + (IK-1)*NTH
            SPEC_VA(ISP) = VA(ISP,JSEA) * CG(IK,ISEA) / CLATS(ISEA)
          ENDDO
        ENDDO
        CALL COMPUTE_MEAN_PARAM(SPEC_VA, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
        SELECT CASE (NINT(SDBSC))
        CASE(1)
          CALL W3SDB1 ( JSEA, SPEC_VA, DEPTH, EMEAN, FMEAN, WNMEAN, CG1, LBREAK, VSDB, VDDB )
        CASE(2)
          !CALL W3SDB2 ( JSEA, SPEC_VA, DEPTH, EMEAN, FMEAN, CG1, LBREAK, VSDB, VDDB )
        END SELECT
#endif
#ifdef W3_DB2
        VSDB   = 0.
        VDDB   = 0.
        CG1 = CG(1:NK,ISEA)
        WN1 = WN(1:NK,ISEA)
        DO IK=1,NK
          DO ITH=1,NTH
            ISP=ITH + (IK-1)*NTH
            SPEC_VA(ISP) = VA(ISP,JSEA) * CG(IK,ISEA) / CLATS(ISEA)
          ENDDO
        ENDDO
        CALL COMPUTE_MEAN_PARAM(SPEC_VA, CG1, WN1, EMEAN, FMEAN, WNMEAN, AMAX)
        CALL W3SDB2 ( JSEA, SPEC_VA, DEPTH, EMEAN, FMEAN, CG1, LBREAK, VSDB, VDDB )
#endif
        DO IK=1,NK
          DO ITH=1,NTH
            ISP=ITH + (IK-1)*NTH
            IF (SHAVETOT(JSEA)) THEN ! Limit only the source term part ...
              MAXDAC    = FACDAM * DAM(ISP)
              TheFactor = DTG / MAX ( 1. , (1.-DTG*VDTOT(ISP,JSEA)))
              DVS       = VSTOT(ISP,JSEA) * TheFactor
              DVS       = SIGN(MIN(MAXDAC,ABS(DVS)),DVS)
              PreVS     = DVS / TheFactor
            ELSE
              PreVS     = VSTOT(ISP,JSEA)
            END IF
            eVS = PreVS / CG(IK,ISEA) * CLATS(ISEA)
            eVD = DBLE(VDTOT(ISP,JSEA))
#ifdef W3_DB1
            eVS = eVS + DBLE(VSDB(ISP)) / CG(IK,ISEA) * CLATS(ISEA)
            eVD = evD + DBLE(VDDB(ISP))
#endif
#ifdef W3_DB2
            eVS = eVS + DBLE(VSDB(ISP)) / CG(IK,ISEA) * CLATS(ISEA)
            eVD = evD + DBLE(VDDB(ISP))
#endif
            B_JAC(ISP,IP)                   = B_JAC(ISP,IP) + SIDT * (eVS - eVD*VA(ISP,JSEA))
            ASPAR_DIAG_LOCAL(ISP,IP) = ASPAR_DIAG_LOCAL(ISP,IP) - SIDT * eVD
          END DO
        END DO
      END IF
    END DO
  END SUBROUTINE CALCARRAY_JACOBI_SOURCE_2
  !/ ------------------------------------------------------------------- /
  SUBROUTINE APPLY_BOUNDARY_CONDITION_VA
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Boudary conditions on VA
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
    USE W3SERVMD, only: STRACE
#endif
    USE yowRankModule, only : IPGL_npa
    USE W3GDATMD, only: NSEAL, CLATS, GTYPE, UNGTYPE
    USE W3WDATMD, only: TIME
    USE W3TIMEMD, only: DSEC21
    USE W3ADATMD, only: CG, CX, CY
    USE W3WDATMD, only: VA
    USE W3GDATMD, only: NK, NK2, NTH, ECOS, ESIN, NSPEC
    USE W3ODATMD, only: TBPI0, TBPIN, FLBPI, IAPROC, NAPROC, BBPI0, BBPIN, ISBPI, NBI
    USE W3PARALL, only : ISEA_TO_JSEA
    !/
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
    REAL    :: RD1, RD2, RD10, RD20
    REAL    :: eVA, eAC
    INTEGER :: IK, ITH, ISEA, JSEA
    INTEGER :: IBI, ISP
#ifdef W3_S
    CALL STRACE (IENT, 'APPLY_BOUNDARY_CONDITION_VA')
#endif
    IF (GTYPE .eq. UNGTYPE) THEN
      IF ( FLBPI ) THEN
        RD10    = DSEC21 ( TBPI0, TIME )
        RD20    = DSEC21 ( TBPI0, TBPIN )
      ELSE
        RD10=1.
        RD20=0.
      END IF
      IF (FLBPI .and. (IAPROC .le. NAPROC)) THEN
        RD1=RD10 ! I am not completely sure about that
        RD2=RD20
        IF ( RD2 .GT. 0.001 ) THEN
          RD2    = MIN(1.,MAX(0.,RD1/RD2))
          RD1    = 1. - RD2
        ELSE
          RD1    = 0.
          RD2    = 1.
        END IF
        DO IBI=1, NBI
          ISEA=ISBPI(IBI)
          JSEA=ISEA_TO_JSEA(ISEA)
          IF (JSEA .gt. 0) THEN
            DO ITH=1,NTH
              DO IK=1,NK
                ISP=ITH + (IK-1)*NTH
                eAC = ( RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI) )   &
                     / CG(IK,ISBPI(IBI)) * CLATS(ISBPI(IBI))
                eVA = MAX(0., CG(IK,ISEA)/CLATS(ISEA)*eAC)
                VA(ISP,JSEA) = eVA
              END DO
            END DO
          END IF
        END DO
      END IF
    END IF
  END SUBROUTINE APPLY_BOUNDARY_CONDITION_VA
  !/ ------------------------------------------------------------------- /
  SUBROUTINE APPLY_BOUNDARY_CONDITION(IMOD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Apply boundary conditions
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
    USE W3SERVMD, only: STRACE
#endif
    USE YOWNODEPOOL, only: npa, np
    USE yowRankModule, only : IPGL_npa
    USE W3GDATMD, only: NSEAL, CLATS, MAPSF
    USE W3WDATMD, only: TIME
    USE W3TIMEMD, only: DSEC21
    USE W3WDATMD, only : VA
    USE W3ADATMD, only: CG, CX, CY
    USE W3GDATMD, only: NK, NK2, NTH, NSPEC
    USE W3ODATMD, only: TBPI0, TBPIN, FLBPI, IAPROC, BBPI0, BBPIN, ISBPI, NBI
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBDP_LOC, IOBPA_LOC
#ifdef W3_DEBUGIOBC
    USE W3GDATMD, only: DDEN
#endif
    !/
    INTEGER, INTENT(IN) :: IMOD
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
#ifdef W3_DEBUGSOLVER
    REAL*8 :: sumAC(NSPEC)
    REAL :: sumBPI0(NSPEC), sumBPIN(NSPEC), sumCG, sumCLATS
#endif
#ifdef W3_DEBUGIOBC
    REAL :: ETOT, HSIG_bound, eVA, eAC, FACTOR
#endif
    REAL    :: RD1, RD2, RD10, RD20
    INTEGER :: IK, ITH, ISEA
    INTEGER :: IBI, IP_glob, ISP, JX
#ifdef W3_S
    CALL STRACE (IENT, 'APPLY_BOUNDARY_CONDITION')
#endif
#ifdef W3_DEBUGSOLVERCOH
    CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(np) before boundary", 0)
    CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(npa) before boundary", 1)
#endif
    IF ( FLBPI ) THEN
      RD10    = DSEC21 ( TBPI0, TIME )
      RD20    = DSEC21 ( TBPI0, TBPIN )
    ELSE
      RD10=1.
      RD20=0.
    END IF
    IF ( FLBPI ) THEN
      RD1=RD10 ! I am not completely sure about that
      RD2=RD20
      IF ( RD2 .GT. 0.001 ) THEN
        RD2    = MIN(1.,MAX(0.,RD1/RD2))
        RD1    = 1. - RD2
      ELSE
        RD1    = 0.
        RD2    = 1.
      END IF
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'Begin of APPLY_BOUNDARY_CONDITION'
      WRITE(740+IAPROC,*) 'NBI=', NBI
      FLUSH(740+IAPROC)
      sumAC=0
      sumBPI0=0
      sumBPIN=0
      sumCG=0
      sumCLATS=0
#endif
      DO IBI=1, NBI
        ISEA    = ISBPI(IBI)
        IP_glob = MAPSF(ISEA,1)
        JX      = IPGL_npa(IP_glob)
        IF (JX .gt. 0) THEN
          DO ITH=1,NTH
            DO IK=1,NK
              ISP=ITH + (IK-1)*NTH
              VA(ISP,JX) = (( RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI) )  &
                   / CG(IK,ISBPI(IBI)) * CLATS(ISBPI(IBI))) * IOBDP_LOC(JX)
            END DO
          END DO
#ifdef W3_DEBUGIOBC
          ETOT=0
          DO ITH=1,NTH
            DO IK=1,NK
              FACTOR = DDEN(IK)/CG(IK,ISEA)
              ISP=ITH + (IK-1)*NTH
              eAC=REAL(VA(ISP,JX))
              eVA=CG(IK,ISEA)/CLATS(ISEA)*eAC
              ETOT = ETOT + eVA*FACTOR
            END DO
          END DO
          HSIG_bound=4.*SQRT(ETOT)
          WRITE(740+IAPROC,*) 'IBI=', IBI, ' HSIG=', HSIG_bound
#endif

#ifdef W3_DEBUGSOLVER
          sumAC=sumAC + VA(:,JX)
          sumBPI0=sumBPI0 + BBPI0(:,IBI)
          sumBPIN=sumBPIN + BBPIN(:,IBI)
          sumCG=sumCG + CG(IK,ISBPI(IBI))
          sumCLATS=sumCLATS + CLATS(ISBPI(IBI))
#endif
        END IF
      ENDDO
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'RD1=', RD1, ' RD2=', RD2
#endif
#ifdef W3_DEBUGSOLVERALL
      DO ISP=1,NSPEC
        WRITE(740+IAPROC,*) 'RD1=', RD1, ' RD2=', RD2
        WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumAC=', sumAC(ISP)
        WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPI0=', sumBPI0(ISP)
        WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumBPIN=', sumBPIN(ISP)
        WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCG=', sumCG
        WRITE(740+IAPROC,*) 'ISP=', ISP, 'sumCLATS=', sumCLATS
      END DO
#endif
#ifdef W3_DEBUGSOLVER
      WRITE(740+IAPROC,*) 'Begin of APPLY_BOUNDARY_CONDITION'
      FLUSH(740+IAPROC)
#endif
#ifdef W3_DEBUGSOLVERCOH
      CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(np) after boundary", 0)
      CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(npa) after boundary", 1)
#endif
    END IF
  END SUBROUTINE APPLY_BOUNDARY_CONDITION
  !/ ------------------------------------------------------------------- /
  SUBROUTINE ACTION_LIMITER_LOCAL(IP,ACLOC,ACOLD, DTG)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Computation of the limiter function
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
    USE W3SERVMD, only: STRACE
#endif
    use YOWNODEPOOL, only: iplg
    USE CONSTANTS, only : GRAV, TPI
    USE W3ADATMD, only : WN, CG
    USE W3GDATMD, only : NTH, NK, NSPEC, MAPFS, SIG, FACP
    !/
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
    INTEGER, INTENT(in) :: IP
    REAL, INTENT(in) :: ACOLD(NSPEC)
    REAL, INTENT(inout) :: ACLOC(NSPEC)
    REAL, INTENT(in) :: DTG
    INTEGER :: MELIM = 1
    REAL :: LIMFAK = 0.1
    REAL :: CONST, SND, eWN, eWK, eWKpow
    REAL :: eFact, eSPSIG
    REAL :: NewVAL
    REAL :: OLDAC, NEWAC, NEWDAC
    REAL :: MAXDAC
    REAL :: dac, limac, eDam
    INTEGER IP_glob, ISEA
    INTEGER :: IK, ITH, ISP
    LOGICAL :: LLIMITER_WWM
#ifdef W3_S
    CALL STRACE (IENT, 'ACTION_LIMITER_LOCAL')!
#endif
    IP_glob=iplg(IP)
    ISEA=MAPFS(1,IP_glob)
    eSPSIG=SIG(NK)
    CONST = TPI**2*3.0*1.0E-7*DTG*eSPSIG
    SND   = TPI*5.6*1.0E-3

    LLIMITER_WWM = .false.

    IF (LLIMITER_WWM) THEN
      MAXDAC = 0
      DO IK=1,NK
        IF (MELIM .eq. 1) THEN
          eFact=2.*SIG(IK)
          eWN=WN(IK,ISEA)
          eWK=eWN
          eWKpow=eWK**3
          MAXDAC = DBLE(0.0081*LIMFAK/(eFact*eWKpow*CG(IK,ISEA)))
        END IF
        DO ITH=1,NTH
          ISP=ITH + (IK-1)*NTH
          NEWAC  = ACLOC(ISP)
          OLDAC  = ACOLD(ISP)
          NEWDAC = NEWAC - OLDAC
          NEWDAC = SIGN(MIN(MAXDAC,ABS(NEWDAC)), NEWDAC)
          NewVAL = MAX(0., OLDAC + NEWDAC )
          ACLOC(ISP) = NewVAL
        END DO
      END DO
    ELSE
      DO IK = 1, NK
        eDam=DBLE(FACP / (SIG(IK) * WN(IK,ISEA)**3))
        DO ITH=1,NTH
          isp = ITH + (IK-1)*NTH
          dac = ACLOC(isp) - ACOLD(isp)
          limac = SIGN (MIN(eDam,ABS(dac)),dac)
          ACLOC(isp) = MAX(0., ACLOC(isp) + limac)
        END DO
      END DO
    ENDIF
  END SUBROUTINE ACTION_LIMITER_LOCAL
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_JACOBI_GAUSS_SEIDEL_BLOCK(IMOD, FACX, FACY, DTG, VGX, VGY, LCALC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Block Gauss Seidel and Jacobi solver
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
    !
#ifdef W3_S
    USE W3SERVMD, only: STRACE
#endif
    !/
    USE CONSTANTS, only : TPI, TPIINV, GRAV
    USE W3GDATMD, only: MAPSTA
    USE W3GDATMD, only: FSREFRACTION, FSFREQSHIFT, FSSOURCE, NX, DSIP
    USE W3GDATMD, only: B_JGS_NORM_THR, B_JGS_TERMINATE_NORM, B_JGS_PMIN
    USE W3GDATMD, only: B_JGS_TERMINATE_DIFFERENCE, B_JGS_MAXITER, B_JGS_LIMITER
    USE W3GDATMD, only: B_JGS_TERMINATE_MAXITER, B_JGS_BLOCK_GAUSS_SEIDEL, B_JGS_DIFF_THR
    USE W3GDATMD, only: MAPWN
#ifdef W3_DEBUGSRC
    USE W3GDATMD, only: optionCall
    USE W3WDATMD, only: SHAVETOT
#endif
    USE YOWNODEPOOL, only: PDLIB_I_DIAG, PDLIB_IA_P, PDLIB_JA, np
    USE YOWNODEPOOL, only: PDLIB_SI, PDLIB_NNZ, PDLIB_CCON
    use yowDatapool, only: rtype
    use YOWNODEPOOL, only: npa, iplg
    use yowExchangeModule, only : PDLIB_exchange2Dreal_zero, PDLIB_exchange2Dreal
    USE MPI, only : MPI_SUM, MPI_INT
    USE W3ADATMD, only: MPI_COMM_WCMP
    USE W3GDATMD, only: NSEA, SIG, FACP, FLSOU
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBDP_LOC, IOBPA_LOC
    USE W3GDATMD, only: NK, NK2, NTH, ECOS, ESIN, NSPEC, MAPFS, NSEA, SIG
    USE W3WDATMD, only: TIME
    USE W3ODATMD, only: NBI
    USE W3TIMEMD, only: DSEC21
    USE W3GDATMD, only: NSEAL, CLATS, FACHFA
    USE W3IDATMD, only: FLCUR, FLLEV
    USE W3WDATMD, only: VA, VAOLD, VSTOT, VDTOT, UST
    USE W3ADATMD, only: CG, CX, CY, WN, DW
    USE W3ODATMD, only: TBPIN, FLBPI, IAPROC
    USE W3PARALL, only : IMEM
    USE W3PARALL, only : INIT_GET_JSEA_ISPROC, ZERO, THR8, LSLOC
    USE W3PARALL, only : ListISPprevDir, ListISPnextDir
    USE W3PARALL, only : JX_TO_JSEA
    USE W3GDATMD, only: B_JGS_NLEVEL, B_JGS_SOURCE_NONLINEAR
    USE yowfunction, only : pdlib_abort
    USE yowNodepool, only: np_global
    USE W3DISPMD, only : WAVNU_LOCAL
    USE W3ADATMD, ONLY: U10, U10D
#ifdef W3_ST4
    USE W3SRC4MD, only: W3SPR4
#endif
#ifdef W3_REF1
    USE W3GDATMD, only: REFPARS
#endif
    implicit none
    LOGICAL, INTENT(IN) :: LCALC
    INTEGER, INTENT(IN) :: IMOD
    REAL, INTENT(IN) :: FACX, FACY, DTG, VGX, VGY
    !
    INTEGER :: IP, ISP, ITH, IK, JSEA, ISEA, IP_glob, IS0
    INTEGER :: myrank
    INTEGER :: nbIter, ISPnextDir, ISPprevDir
    INTEGER :: ISPp1, ISPm1, JP, ICOUNT1, ICOUNT2
    ! for the exchange
    REAL*8  :: CCOS, CSIN, CCURX, CCURY
    REAL*8  :: eSum(NSPEC), FRLOCAL
    REAL*8  :: eA_THE, eC_THE, eA_SIG, eC_SIG, eSI
    REAL*8  :: CAD(NSPEC), CAS(NSPEC), ACLOC(NSPEC)
    REAL*8  :: CP_SIG(NSPEC), CM_SIG(NSPEC)
    REAL*8  :: eFactM1, eFactP1
    REAL*8  :: Sum_Prev, Sum_New, p_is_converged, DiffNew, prop_conv
    REAL*8  :: Sum_L2, Sum_L2_GL
    REAL  :: DMM(0:NK2), DAM(NSPEC), DAM2(NSPEC), SPEC(NSPEC)
    REAL*8  :: eDiff(NSPEC), eProd(NSPEC), eDiffB(NSPEC)
    REAL*8  :: DWNI_M2(NK), CWNB_M2(1-NTH:NSPEC)
    REAL  :: VAnew(NSPEC), VFLWN(1-NTH:NSPEC), JAC, JAC2
    REAL  :: VAAnew(1-NTH:NSPEC+NTH), VAAacloc(1-NTH:NSPEC+NTH)
    REAL  :: VAinput(NSPEC), VAacloc(NSPEC), ASPAR_DIAG(NSPEC)
    REAL  :: aspar_diag_local(nspec), aspar_off_diag_local(nspec), b_jac_local(nspec)
    REAL*8 :: eDiffSing, eSumPart
    REAL  :: EMEAN, FMEAN, FMEAN1, WNMEAN, AMAX, U10ABS, U10DIR, TAUA, TAUADIR
    REAL  :: USTAR, USTDIR, TAUWX, TAUWY, CD, Z0, CHARN, FMEANWS, DLWMEAN
    REAL*8  :: eVal1, eVal2
    REAL*8  :: eVA, eVO, CG2, NEWDAC, NEWAC, OLDAC, MAXDAC
    REAL  :: CG1(0:NK+1), WN1(0:NK+1)
    LOGICAL :: LCONVERGED(NSEAL), lexist, LLWS(NSPEC)
#ifdef WEIGHTS
    INTEGER :: ipiter(nseal), ipitergl(np_global), ipiterout(np_global)
#endif
#ifdef W3_DEBUGSRC
    REAL :: IntDiff, eVA_w3srce, eVAsolve, SumACout
    REAL :: SumVAin, SumVAout, SumVAw3srce, SumVS, SumVD, VS_w3srce
    REAL    :: VAsolve(NSPEC)
    REAL*8  :: ACsolve
    REAL    :: eB
#endif
#ifdef W3_DEBUGSOLVERCOH
    REAL :: TheARR(NSPEC, npa)
    REAL :: PRE_VA(NSPEC, npa)
    REAL :: OffDIAG(NSPEC, npa)
    REAL*8 :: eOff(NSPEC)
    REAL*8 :: eSum1(NSPEC), eSum2(NSPEC)
#endif
    CHARACTER(len=128) eFile
    INTEGER ierr, i
    INTEGER JP_glob
    INTEGER is_converged, itmp

    INTEGER :: TESTNODE = 923

    LOGICAL :: LSIG = .FALSE.

    memunit = 50000+IAPROC
    !AR: this is missing in init ... but there is a design error in ww3_grid with FLCUR and FLLEV
    LSIG = FLCUR .OR. FLLEV
#ifdef W3_DEBUGSOLVERCOH
    OffDIAG = ZERO
#endif
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION 0')


    CCURX  = FACX
    CCURY  = FACY
    CALL MPI_COMM_RANK(MPI_COMM_WCMP, myrank, ierr)
    !
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_JACOBI_GAUSS_SEIDEL_BLOCK, begin'
    WRITE(740+IAPROC,*) 'NX=', NX
    WRITE(740+IAPROC,*) 'NP=', NP
    WRITE(740+IAPROC,*) 'NPA=', NPA
    WRITE(740+IAPROC,*) 'NSEA=', NSEA
    WRITE(740+IAPROC,*) 'NSEAL=', NSEAL
    WRITE(740+IAPROC,*) 'NBI=', NBI
    WRITE(740+IAPROC,*) 'B_JGS_TERMINATE_NORM=', B_JGS_TERMINATE_NORM
    WRITE(740+IAPROC,*) 'B_JGS_TERMINATE_DIFFERENCE=', B_JGS_TERMINATE_DIFFERENCE
    WRITE(740+IAPROC,*) 'B_JGS_TERMINATE_MAXITER=', B_JGS_TERMINATE_MAXITER
    WRITE(740+IAPROC,*) 'B_JGS_MAXITER=', B_JGS_MAXITER
    WRITE(740+IAPROC,*) 'B_JGS_BLOCK_GAUSS_SEIDEL=', B_JGS_BLOCK_GAUSS_SEIDEL
    WRITE(740+IAPROC,*) 'FSREFRACTION=', FSREFRACTION
    WRITE(740+IAPROC,*) 'FSFREQSHIFT=', FSFREQSHIFT
    WRITE(740+IAPROC,*) 'B_JGS_LIMITER=', B_JGS_LIMITER
    WRITE(740+IAPROC,*) 'B_JGS_BLOCK_GAUSS_SEIDEL=', B_JGS_BLOCK_GAUSS_SEIDEL
    FLUSH(740+IAPROC)
#endif
#ifdef W3_DEBUGSRC
    WRITE(740+IAPROC,*) 'optionCall=', optionCall
    FLUSH(740+IAPROC)
#endif
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION 1')
    !
    ! 2.  Convert to Wave Action ---------------- *
    !
#ifdef W3_DEBUGSRC
    WRITE(740+IAPROC,*) 'NSEAL =', NSEAL, 'NP    =', NP, 'NPA   =', NPA
#endif
#ifdef W3_DEBUGSOLVERCOH
    CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(np) before transform", 0)
    CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(npa) before transform", 1)
#endif
    DO JSEA=1,NSEAL
      IP      = JSEA
      IP_glob = iplg(IP)
      ISEA    = MAPFS(1,IP_glob)
      DO ISP=1,NSPEC
        ITH    = 1 + MOD(ISP-1,NTH)
        IK     = 1 + (ISP-1)/NTH
#ifdef NOCGTABLE
        CALL WAVNU_LOCAL(SIG(IK),DW(ISEA),WN1(IK),CG1(IK))
#else
        CG1(IK)    = CG(IK,ISEA)
#endif
        VA(ISP,JSEA) = VA(ISP,JSEA) / CG1(IK) * CLATS(ISEA)
      END DO
    END DO
    VAOLD = VA(1:NSPEC,1:NSEAL)

#ifdef W3_DEBUGSRC
    DO JSEA=1,NSEAL
      WRITE(740+IAPROC,*) 'JSEA=', JSEA
      WRITE(740+IAPROC,*) 'min/max/sum(VA)=', minval(VA(:,JSEA)), maxval(VA(:,JSEA)), sum(VA(:,JSEA))
    END DO
#endif

#ifdef W3_DEBUGSOLVERCOH
    CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(np) just defined", 0)
    CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(npa) just defined", 1)
#endif

#ifdef W3_DEBUGSOLVER
    FLUSH(740+IAPROC)
    WRITE(740+IAPROC,*) 'JACOBI_SOLVER, step 4'
    WRITE(740+IAPROC,*) 'FSSOURCE=', FSSOURCE
    WRITE(740+IAPROC,*) 'FSREFRACTION=', FSREFRACTION
    WRITE(740+IAPROC,*) 'FSFREQSHIFT=', FSFREQSHIFT
    WRITE(740+IAPROC,*) 'FSGEOADVECT=', FSGEOADVECT
    WRITE(740+IAPROC,*) 'DTG=', DTG
#endif
    !
    !    init matrix and right hand side
    !
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION 2')
    !
    IF (.not. LSLOC) THEN
      IF (IMEM == 1) THEN
        ASPAR_JAC = ZERO
      ELSE IF (IMEM == 2) THEN
        ASPAR_DIAG_ALL = ZERO
      ENDIF
      B_JAC = ZERO
    ENDIF
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION 3')
    !
    !     source terms
    !
    IF (FSSOURCE) THEN
      IF (.not. LSLOC) THEN
        IF (IMEM == 1) THEN
          call CALCARRAY_JACOBI_SOURCE_1(DTG)
        ELSE IF (IMEM == 2) THEN
          call CALCARRAY_JACOBI_SOURCE_2(DTG,ASPAR_DIAG_ALL)
        ENDIF
      ENDIF
    END IF
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION 4')
    !
    !     geographical advection
    !
    IF (IMEM == 1) THEN
      call calcARRAY_JACOBI_VEC(DTG,FACX,FACY,VGX,VGY)
    ENDIF

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,'(A20,20E20.10)') 'SUM BJAC 1', sum(B_JAC), SUM(ASPAR_JAC)
#endif
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION 5')
    !
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,'(A20,20E20.10)') 'SUM BJAC 1', sum(B_JAC), SUM(ASPAR_JAC)
#endif
    !
    !     spectral advection
    !
    IF (FSFREQSHIFT .or. FSREFRACTION) THEN
      IF (IMEM == 1) THEN
        call calcARRAY_JACOBI_SPECTRAL_1(DTG)
      ELSE IF (IMEM == 2) THEN
        call calcARRAY_JACOBI_SPECTRAL_2(DTG,ASPAR_DIAG_ALL)
      ENDIF
    END IF
    CALL APPLY_BOUNDARY_CONDITION(IMOD)
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION 6')
    !
#ifdef W3_DEBUGSOLVERCOH
    CALL CHECK_ARRAY_INTEGRAL_NX_R8(B_JAC, "B_JAC after calcARRAY", np)
    DO IP=1,npa
      TheArr(:, IP)=REAL(ASPAR_JAC(:, PDLIB_I_DIAG(IP)))
    END DO
    CALL CHECK_ARRAY_INTEGRAL_NX_R8(TheArr, "ASPAR diag after calArr", np)
#endif
    nbIter=0
    do ip = 1, np
      Lconverged(ip) = .false.
#ifdef WEIGHTS
      ipiter(ip) = 0
#endif
    enddo
    !
    DO

      is_converged = 0

      call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION SOLVER LOOP 1')

      DO IP = 1, np

        IP_glob = iplg(IP)
        ISEA    = MAPFS(1,IP_glob)
        IF (IOBDP_LOC(IP) .eq. 0) THEN
          is_converged   = is_converged + 1
          lconverged(ip) = .true.
          CYCLE
        ENDIF

        DO IK = 0, NK + 1
#ifdef NOCGTABLE
          CALL WAVNU_LOCAL(SIG(IK),DW(ISEA),WN1(IK),CG1(IK))
#else
          CG1(IK)  = CG(IK,ISEA)
          WN1(IK)  = WN(IK,ISEA)
#endif
        ENDDO

        JSEA  = JX_TO_JSEA(IP)
        ISEA  = MAPFS(1,IP_glob)
        eSI   = PDLIB_SI(IP)
        ACLOC = VA(:,JSEA)

        IF (.NOT. LCONVERGED(IP)) THEN
#ifdef WEIGHTS
          ipiter(ip) = ipiter(ip) + 1
#endif
#ifdef W3_DEBUGFREQSHIFT
          WRITE(740+IAPROC,*) 'Begin loop'
          WRITE(740+IAPROC,*) 'IP/IP_glob/ISEA/JSEA=', IP, IP_glob, ISEA, JSEA
#endif
#ifdef W3_DEBUGSRC
          WRITE(740+IAPROC,*) 'IP=', IP, ' IP_glob=', IP_glob
          WRITE(740+IAPROC,*) 'sum(VA)in=', sum(VA(:,IP))
#endif
#ifdef W3_DEBUGFREQSHIFT
          DO ISP=1,NSPEC
            VAold(ISP) = VA(ISP,JSEA)
            IK=MAPWN(ISP)
            VAinput(ISP) = DBLE(CG(IK,ISEA)/CLATS(ISEA)) * VA(ISP, IP)
            VAacloc(ISP) = DBLE(CG(IK,ISEA)/CLATS(ISEA)) * ACLOC(ISP)
          END DO
          WRITE(740+IAPROC,*) 'sum(VAold/VAinput/VAacloc)=', sum(VAold), sum(VAinput), sum(VAacloc)
#endif

          Sum_Prev = sum(ACLOC)

          IF (IMEM == 2) THEN
            CALL calcARRAY_JACOBI4(IP,DTG,FACX,FACY,VGX,VGY,ASPAR_DIAG_LOCAL,ASPAR_OFF_DIAG_LOCAL,B_JAC_LOCAL)
            ASPAR_DIAG(1:NSPEC) = ASPAR_DIAG_LOCAL(1:NSPEC) + ASPAR_DIAG_ALL(1:NSPEC,IP)
            esum       = B_JAC_LOCAL - ASPAR_OFF_DIAG_LOCAL + B_JAC(1:NSPEC,IP)
          ELSEIF (IMEM == 1) THEN
            eSum(1:NSPEC)       = B_JAC(1:NSPEC,IP)
            ASPAR_DIAG(1:NSPEC) = ASPAR_JAC(1:NSPEC,PDLIB_I_DIAG(IP))
#ifdef W3_DEBUGFREQSHIFT
            WRITE(740+IAPROC,*) 'eSI=', eSI
            WRITE(740+IAPROC,*) 'sum(ASPAR_DIAG)=', sum(ASPAR_DIAG)
#endif
#ifdef W3_DEBUGSRC
            WRITE(740+IAPROC,*) 'Step 1: sum(eSum)=', sum(eSum)
#endif
#ifdef W3_DEBUGSOLVERCOH
            eOff=ZERO
#endif
            DO i = PDLIB_IA_P(IP)+1, PDLIB_IA_P(IP+1)
              JP = PDLIB_JA(I)
              IF (JP .ne. IP) THEN
                eProd(1:NSPEC) = ASPAR_JAC(1:NSPEC,i) * VA(1:NSPEC,JP)
                eSum(1:NSPEC)  = eSum(1:NSPEC) - eProd(1:NSPEC)
#ifdef W3_DEBUGSOLVERALL
                WRITE(740+IAPROC,'(A20,3I10,20E20.10)') 'OFF DIAGONAL', IP, i, jp, sum(B_JAC(:,IP)), sum(eSum), SUM(ASPAR_JAC(:,i)), SUM(VA(:,JP))
#endif
#ifdef W3_DEBUGSOLVERCOH
                eOff=eOff + abs(ASPAR_JAC(:,i))
#endif
              END IF
            END DO
          ENDIF ! IMEM

#ifdef W3_DEBUGSOLVERCOH
          OffDiag(:, IP)=REAL(eOff)
#endif
#ifdef W3_DEBUGSOLVERCOHALL
          WRITE(740+IAPROC,*) 'Step 2: sum(eSum)=', sum(eSum), ' eOff=', sum(eOff)
#endif
          IF (FSREFRACTION) THEN
#ifdef W3_DEBUGREFRACTION
            WRITE(740+IAPROC,*) 'Adding refraction terms to eSum'
#endif
            CAD = CAD_THE(:,IP)
            DO ISP=1,NSPEC
              ISPprevDir=ListISPprevDir(ISP)
              ISPnextDir=ListISPnextDir(ISP)
              eA_THE = - DTG*eSI*MAX(ZERO,CAD(ISPprevDir))
              eC_THE =   DTG*eSI*MIN(ZERO,CAD(ISPnextDir))
              eSum(ISP) = eSum(ISP) - eA_THE * VA(ISPprevDir,IP)
              eSum(ISP) = eSum(ISP) - eC_THE * VA(ISPnextDir,IP)
            END DO
          END IF
#ifdef W3_DEBUGSRC
          WRITE(740+IAPROC,*) 'Step 3: sum(eSum)=', sum(eSum)
#endif
          IF (FSFREQSHIFT .and. LSIG) THEN
            IF (FreqShiftMethod .eq. 1) THEN
              CAS = CAS_SIG(:,IP)
              CP_SIG = MAX(ZERO,CAS)
              CM_SIG = MIN(ZERO,CAS)
              DO IK=0, NK
                DMM(IK+1) = DBLE(WN1(IK+1) - WN1(IK))
              END DO
              DMM(NK+2) = ZERO
              DMM(0)=DMM(1)
              DO ITH=1,NTH
                DO IK=2,NK
                  ISP       = ITH + (IK   -1)*NTH
                  ISPm1     = ITH + (IK-1 -1)*NTH
                  eFactM1   = CG1(IK-1) / CG1(IK)
                  eA_SIG    = - eSI * CP_SIG(ISPm1)/DMM(IK-1) * eFactM1
                  eSum(ISP) = eSum(ISP) - eA_SIG*VA(ISPm1,IP)
                END DO
                DO IK=1,NK-1
                  ISP       = ITH + (IK   -1)*NTH
                  ISPp1     = ITH + (IK+1 -1)*NTH
                  eFactP1   = CG1(IK+1) / CG1(IK)
                  eC_SIG    = eSI * CM_SIG(ISPp1)/DMM(IK) * eFactP1
                  eSum(ISP) = eSum(ISP) - eC_SIG*VA(ISPp1,IP)
                END DO
              END DO
            ELSE IF (FreqShiftMethod .eq. 2) THEN
              CWNB_M2=CWNB_SIG_M2(:,IP)
              DO IK=1, NK
                DWNI_M2(IK) = DBLE( CG1(IK) / DSIP(IK) )
              END DO
#ifdef W3_DEBUGFREQSHIFT
              WRITE(740+IAPROC,*) 'Before FreqShift oper eSum=', sum(abs(eSum))
#endif
              DO ITH=1,NTH
                DO IK=2,NK
                  ISP       = ITH + (IK   -1)*NTH
                  ISPm1     = ITH + (IK-1 -1)*NTH
                  eFactM1   = DBLE( CG1(IK-1) / CG1(IK) )
                  eA_SIG    = - eSI * DWNI_M2(IK) * MAX(CWNB_M2(ISPm1),ZERO) *eFactM1
                  eSum(ISP) = eSum(ISP) - eA_SIG*VA(ISPm1,IP)
                END DO
                DO IK=1,NK-1
                  ISP       = ITH + (IK   -1)*NTH
                  ISPp1     = ITH + (IK+1 -1)*NTH
                  eFactP1   = DBLE( CG1(IK+1) / CG1(IK) )
                  eC_SIG    = eSI * DWNI_M2(IK) * MIN(CWNB_M2(ISP),ZERO) * eFactP1
                  eSum(ISP) = eSum(ISP) - eC_SIG*VA(ISPp1,IP)
                END DO
              END DO
#ifdef W3_DEBUGFREQSHIFT
              WRITE(740+IAPROC,*) ' after FreqShift oper eSum=', sum(abs(eSum))
#endif
            END IF
          END IF
#ifdef W3_DEBUGSRC
          WRITE(740+IAPROC,*) 'Step 4: sum(eSum)=', sum(eSum)
#endif
#ifdef W3_DEBUGSOLVERCOH
          PRE_VA(:, IP)=REAL(eSum)
#endif
          eSum(1:NSPEC)  = eSum(1:NSPEC) / ASPAR_DIAG(1:NSPEC)
#ifdef W3_DEBUGFREQSHIFT
          WRITE(740+IAPROC,*) 'JSEA=', JSEA, ' nbIter=', nbIter
          DO ISP=1,NSPEC
            IK=MAPWN(ISP)
            VAnew(ISP) = DBLE(CG(IK,ISEA)/CLATS(ISEA)) * eSum(ISP)
          END DO
          DO ISP=1,NSPEC
            VAAnew(ISP)   = VAnew(ISP)
            VAAacloc(ISP) = VAacloc(ISP)
          END DO
          DO ITH=1,NTH
            VAAnew(ITH + NSPEC) = FACHFA * VAAnew(ITH + NSPEC - NTH)
            VAAnew(ITH - NTH  ) = 0.
            VAAacloc(ITH + NSPEC) = FACHFA * VAAacloc(ITH + NSPEC - NTH)
            VAAacloc(ITH - NTH  ) = 0.
          END DO
          DO ISP=1-NTH,NSPEC
            VFLWN(ISP) = MAX(CWNB_M2(ISP),0.) * VAAnew(ISP) + MIN(CWNB_M2(ISP),0.) * VAAnew(ISP + NTH)
          END DO
          DO ISP=1,NSPEC
            eDiff(ISP)  = VAnew(ISP) - VAold(ISP) - DWNI_M2(MAPWN(ISP)) * (VFLWN(ISP-NTH) - VFLWN(ISP) )
            eVal1=MAX(CWNB_M2(ISP-NTH),0.) * VAAacloc(ISP-NTH) + MIN(CWNB_M2(ISP-NTH),0.) * VAAnew(ISP)
            eVal2=MAX(CWNB_M2(ISP),0.) * VAAnew(ISP) + MIN(CWNB_M2(ISP),0.) * VAAacloc(ISP + NTH)
            eDiffB(ISP) = VAnew(ISP) - VAold(ISP) - DWNI_M2(MAPWN(ISP)) * (eVal1 - eVal2)
          END DO
          IF (ISEA .eq. 190) THEN
            DO IK=1,NK
              DO ITH=1,NTH
                ISP = ITH + (IK-1)*NTH
                WRITE(740+IAPROC,*) 'ISP/ITH/IK=', ISP, ITH, IK
                WRITE(740+IAPROC,*) 'eDiff(A/B)=', eDiff(ISP), eDiffB(ISP)
              END DO
            END DO
          END IF
          WRITE(740+IAPROC,*) 'NK=', NK, ' NTH=', NTH
          eSumPart=0
          DO IK=1,NK
            DO ITH=1,NTH
              ISP  = ITH + (IK-1)*NTH
              eSumPart = eSumPart + abs(eDiff(ISP))
            END DO
            IF (ISEA .eq. 190) THEN
              WRITE(740+IAPROC,*) 'IK=', IK, ' eSumDiff=', eSumPart
            END IF
          END DO
          WRITE(740+IAPROC,*) 'sum(eDiff/VAnew/VAold)=', sum(abs(eDiff)), sum(abs(VAnew)), sum(abs(VAold))
#endif

          IF (B_JGS_BLOCK_GAUSS_SEIDEL) THEN
            VA(1:NSPEC,IP) = REAL(eSum) * IOBDP_LOC(IP)
#ifdef W3_REF1
            DO IK=1,NK
              DO ITH=1,NTH
                ISP  = ITH + (IK-1)*NTH
                IF (REFPARS(3) .LT. 0.5 .AND. IOBPD_LOC(ITH,IP) .EQ. 0 .AND. IOBPA_LOC(IP) .EQ. 0) THEN
                  VA(ISP,IP) = VAOLD(ISP,IP) * IOBDP_LOC(IP) ! Restores reflected action spectra ...
                ENDIF
              ENDDO
            ENDDO
#endif
          ELSE
            U_JAC(1:NSPEC,IP) = eSum
          END IF
        ELSE
          esum = VA(1:NSPEC,IP)
        ENDIF ! .NOT. LCONVERGED

        IF (B_JGS_TERMINATE_DIFFERENCE) THEN
          Sum_New = sum(eSum)
          if (Sum_new .gt. 0.d0) then
            DiffNew = abs(sum(ACLOC-eSum))/Sum_new
#ifdef W3_DEBUGFREQSHIFT
            WRITE(740+IAPROC,*) 'DiffNew=', DiffNew, ' Sum_new=', Sum_new
#endif
            p_is_converged = DiffNew
          else
            p_is_converged = zero
          endif
#ifdef W3_DEBUGFREQSHIFT
          WRITE(740+IAPROC,*) 'p_is_converged=', p_is_converged
#endif
          IF (p_is_converged .lt. B_JGS_DIFF_THR .and. nbiter .gt. 1) then
            is_converged   = is_converged + 1
            lconverged(ip) = .true.
          ELSE
            lconverged(ip) = .false.
          ENDIF
        END IF
#ifdef W3_DEBUGSRC
        WRITE(740+IAPROC,*) 'sum(VA)out=', sum(VA(:,IP))
#endif
      END DO ! IP

      call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION SOLVER LOOP 2')

#ifdef W3_DEBUGSOLVERCOH
      WRITE (eFile,40) nbIter
40    FORMAT ('PRE_VA_',i4.4,'.txt')
      CALL CHECK_ARRAY_INTEGRAL_NX_R8(OffDiag, "OffDiag(np) just check", np)
      !     CALL WRITE_VAR_TO_TEXT_FILE(PRE_VA, eFile)
      CALL CHECK_ARRAY_INTEGRAL_NX_R8(PRE_VA, "PRE_VA(np) just check", np)
      CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(np) before exchanges", 0)
#endif
      IF (B_JGS_BLOCK_GAUSS_SEIDEL) THEN
        CALL PDLIB_exchange2DREAL_zero(VA)
      ELSE
        CALL PDLIB_exchange2DREAL(U_JAC)
        VA(:,1:NPA) = U_JAC
      END IF
      call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION SOLVER LOOP 3')
      !
      ! Terminate via number of iteration
      !
      IF (B_JGS_TERMINATE_MAXITER) THEN
        IF (nbIter .gt. B_JGS_MAXITER) THEN
#ifdef W3_DEBUGSOLVER
          WRITE(740+IAPROC,*) 'Exiting by TERMINATE_MAXITER'
#endif
          EXIT
        END IF
      END IF
      call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION SOLVER LOOP 4')
      !
      ! Terminate via differences
      !
      IF (B_JGS_TERMINATE_DIFFERENCE .and. INT(MOD(NBITER,10)) == 0) THEN ! Every 10th step check conv.
        CALL MPI_ALLREDUCE(is_converged, itmp, 1, MPI_INT, MPI_SUM, MPI_COMM_WCMP, ierr)
        is_converged = itmp
        prop_conv = (DBLE(NX) - DBLE(is_converged))/DBLE(NX) * 100.
#ifdef W3_DEBUGSOLVER
        WRITE(740+IAPROC,*) 'solver', nbiter, is_converged, prop_conv, B_JGS_PMIN
        FLUSH(740+IAPROC)
#endif
        IF (myrank == 0) WRITE(*,*) 'No. of solver iterations', nbiter, is_converged, prop_conv, B_JGS_PMIN
        IF (prop_conv .le. B_JGS_PMIN + TINY(1.)) THEN
#ifdef W3_DEBUGFREQSHIFT
          WRITE(740+IAPROC,*) 'prop_conv=', prop_conv
          WRITE(740+IAPROC,*) 'NX=', NX
          WRITE(740+IAPROC,*) 'is_converged=', is_converged
          WRITE(740+IAPROC,*) 'Exiting by TERMINATE_DIFFERENCE'
#endif
          EXIT
        END IF
      END IF
      call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION SOLVER LOOP 5')
      !
      ! Terminate via norm
      !
      IF (B_JGS_TERMINATE_NORM) THEN
        Sum_L2 =0
        DO IP = 1, np
          IP_glob=iplg(IP)
          IF (IOBP_LOC(IP).eq.1) THEN
            JSEA=JX_TO_JSEA(IP)
            eSI=PDLIB_SI(IP)
            eSum=B_JAC(:,IP)
            ACLOC=VA(:,IP)
            ISEA= MAPFS(1,IP_glob)
            eSum(:) = eSum(:) - ASPAR_DIAG(:)*ACLOC
            DO I = PDLIB_IA_P(IP)+1, PDLIB_IA_P(IP+1)
              JP=PDLIB_JA(I)
              eSum(:) = eSum(:) - ASPAR_JAC(:,i)*VA(:,JP)
            END DO
            IF (FSREFRACTION) THEN
              CAD=CAD_THE(:,IP)
              DO ISP=1,NSPEC
                ISPprevDir=ListISPprevDir(ISP)
                ISPnextDir=ListISPnextDir(ISP)
                eA_THE = - DTG*eSI*MAX(ZERO,CAD(ISPprevDir))
                eC_THE =   DTG*eSI*MIN(ZERO,CAD(ISPnextDir))
                eSum(ISP) = eSum(ISP) - eA_THE*VA(ISPprevDir,IP)
                eSum(ISP) = eSum(ISP) - eC_THE*VA(ISPnextDir,IP)
              END DO
            END IF
            IF (FSFREQSHIFT) THEN
              CAS=CAS_SIG(:,IP)
              CP_SIG = MAX(ZERO,CAS)
              CM_SIG = MIN(ZERO,CAS)
              DO IK = 0, NK + 1
#ifdef NOCGTABLE
                CALL WAVNU_LOCAL(SIG(IK),DW(ISEA),WN1(IK),CG1(IK))
#else
                CG1(IK)  = CG(IK,ISEA)
                WN1(IK)  = WN(IK,ISEA)
#endif
              ENDDO
              DO ITH=1,NTH
                IF (IOBPD_LOC(ITH,IP) .NE. 0) THEN
                  DO IK=2,NK
                    ISP  =ITH + (IK  -1)*NTH
                    ISPm1=ITH + (IK-1-1)*NTH
                    eFactM1=CG(IK-1,ISEA) / CG1(IK)
                    eA_SIG= - eSI*CP_SIG(ISPm1)/DMM(IK-1) * eFactM1
                    eSum(ISP) = eSum(ISP) - eA_SIG*VA(ISPm1,IP)
                  END DO
                  DO IK=1,NK-1
                    ISP  =ITH + (IK  -1)*NTH
                    ISPp1=ITH + (IK+1-1)*NTH
                    eFactP1=CG(IK+1,ISEA) / CG1(IK)
                    eC_SIG= eSI*CM_SIG(ISPp1)/DMM(IK) * eFactP1
                    eSum(ISP) = eSum(ISP) - eC_SIG*VA(ISPp1,IP)
                  END DO
                END IF
              END DO
            END IF
            Sum_L2 = Sum_L2 + sum(eSum*eSum)
          END IF
        END DO
        CALL MPI_ALLREDUCE(Sum_L2, Sum_L2_GL, 1, rtype, MPI_SUM, MPI_COMM_WCMP, ierr)
#ifdef W3_DEBUGSOLVER
        WRITE(740+IAPROC,*) 'Sum_L2_gl=', Sum_L2_gl
        FLUSH(740+IAPROC)
#endif
        IF (Sum_L2_gl .le. B_JGS_NORM_THR) THEN
#ifdef W3_DEBUGFREQSHIFT
          WRITE(740+IAPROC,*) 'Exiting by TERMINATE_NORM'
#endif
          EXIT
        END IF
      END IF
      call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION SOLVER LOOP 6')

      nbiter = nbiter + 1

    END DO ! Open Do Loop ... End of Time Interval

#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'nbIter=', nbIter, ' B_JGS_MAXITER=', B_JGS_MAXITER
    FLUSH(740+IAPROC)
#endif
    ! Tihs is below also goes into the matrix ... like the wave boundary ...
    DO IP = 1, npa
#ifdef W3_DEBUGSRC
      WRITE(740+IAPROC,*) 'IOBPD loop, Before, sum(VA)=', sum(VA(:,IP))
#endif
      DO ISP=1,NSPEC
        ITH    = 1 + MOD(ISP-1,NTH)
        VA(ISP,IP)=MAX(ZERO, VA(ISP,IP))*IOBDP_LOC(IP)*DBLE(IOBPD_LOC(ITH,IP))
#ifdef W3_REF1
        IF (REFPARS(3).LT.0.5.AND.IOBPD_LOC(ITH,IP).EQ.0.AND.IOBPA_LOC(IP).EQ.0) THEN
          VA(ISP,IP) = VAOLD(ISP,IP) ! restores reflected boundary values
        ENDIF
#endif
      END DO
#ifdef W3_DEBUGSRC
      WRITE(740+IAPROC,*) 'IOBPD loop, After, sum(VA)=', sum(VA(:,IP))
#endif
    END DO
#ifdef W3_DEBUGSOLVERCOH
    CALL ALL_VA_INTEGRAL_PRINT(IMOD, "VA(npa) after loop", 1)
#endif
#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'FLBPI=', FLBPI
    FLUSH(740+IAPROC)
#endif

    DO JSEA=1, NSEAL

      IP      = JSEA
      IP_glob = iplg(IP)
      ISEA    = MAPFS(1,IP_glob)
      !
#ifdef W3_DEBUGSRC
      IntDiff=0
      SumVS=0
      SumVD=0
      SumVAin=0
      SumVAout=0
      SumVAw3srce=0
      SumACout=0
#endif
      !
      DO ISP=1,NSPEC

        IK     = 1 + (ISP-1)/NTH
#ifdef NOCGTABLE
        CALL WAVNU_LOCAL(SIG(IK),DW(ISEA),WN1(IK),CG1(IK))
#else
        CG1(IK)    = CG(IK,ISEA)
#endif
        eVA = MAX ( ZERO ,CG1(IK)/CLATS(ISEA)*REAL(VA(ISP,IP)) )
        eVO = MAX ( ZERO ,CG1(IK)/CLATS(ISEA)*REAL(VAOLD(ISP,JSEA)) )
#ifdef W3_DEBUGSRC
        SumACout=SumACout + REAL(VA(ISP,IP))
        VS_w3srce = VSTOT(ISP,JSEA) * DTG / MAX(1., (1. - DTG*VDTOT(ISP,JSEA)))
        eVA_w3srce = MAX(0., VA(ISP,JSEA) + VS_w3srce)
        IntDiff = IntDiff + abs(eVA - eVA_w3srce)
        ACsolve=B_JAC(ISP,IP)/ASPAR_JAC(ISP,PDLIB_I_DIAG(IP))
        eB=VA(ISP,JSEA) + DTG*(VSTOT(ISP,JSEA) - VDTOT(ISP,JSEA)*VA(ISP,JSEA))
        eVAsolve=MAX(0., CG(IK,ISEA)/CLATS(ISEA)*ACsolve)
        VAsolve(ISP)=eVAsolve
        SumVS = SumVS + abs(VSTOT(ISP,JSEA))
        SumVD = SumVD + abs(VDTOT(ISP,JSEA))
        SumVAin = SumVAin + abs(VA(ISP,JSEA))
        SumVAout = SumVAout + abs(eVA)
        SumVAw3srce = SumVAw3srce + abs(eVA_w3srce)
#endif
        VAOLD(ISP,JSEA) = eVO
        VA(ISP,JSEA) = eVA
      END DO
#ifdef W3_DEBUGSRC
      WRITE(740+IAPROC,*) 'ISEA=', ISEA, ' IntDiff=', IntDiff, ' DTG=', DTG
      IF (ISEA .eq. TESTNODE) THEN
        DO ISP=1,NSPEC
          WRITE(740+IAPROC,*) 'ISP=', ISP, 'VA/VAsolve=', VA(ISP,JSEA), VAsolve(ISP)
        END DO
      END IF
      WRITE(740+IAPROC,*) 'SHAVE=', SHAVETOT(JSEA)
      WRITE(740+IAPROC,*) 'Sum(VS/VD)=', SumVS, SumVD
      WRITE(740+IAPROC,*) 'min/max/sum(VS)=', minval(VSTOT(:,JSEA)), maxval(VSTOT(:,JSEA)), sum(VSTOT(:,JSEA))
      WRITE(740+IAPROC,*) 'min/max/sum(VD)=', minval(VDTOT(:,JSEA)), maxval(VDTOT(:,JSEA)), sum(VDTOT(:,JSEA))
      WRITE(740+IAPROC,*) 'min/max/sum(VA)=', minval(VA(:,JSEA)), maxval(VA(:,JSEA)), sum(VA(:,JSEA))
      WRITE(740+IAPROC,*) 'min/max/sum(VAsolve)=', minval(VAsolve), maxval(VAsolve), sum(VAsolve)
      WRITE(740+IAPROC,*) 'SumVA(in/out/w3srce)=', SumVAin, SumVAout, SumVAw3srce
      WRITE(740+IAPROC,*) 'SumACout=', SumACout
#endif

      IF (FLSOU) THEN
        IF (B_JGS_LIMITER) THEN

          DO ISP=1,NSPEC
            IK   = 1 + (ISP-1)/NTH
            SPEC(ISP) = VAOLD(ISP,JSEA)
          ENDDO
#ifdef W3_ST4
          CALL W3SPR4 (SPEC, CG1, WN1, EMEAN, FMEAN, FMEAN1, WNMEAN, &
               AMAX, U10(ISEA), U10D(ISEA),                           &
#ifdef W3_FLX5
               TAUA, TAUADIR, DAIR,                             &
#endif
               USTAR, USTDIR,                                  &
               TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS, DLWMEAN)
#endif

          DAM = 0.
          DO IK=1, NK
            DAM(1+(IK-1)*NTH) = 0.0081*0.1 / ( 2 * SIG(IK) * WN(IK,ISEA)**3 * CG(IK,ISEA)) * CG1(IK) / CLATS(ISEA)
          END DO
          !
          DO IK=1, NK
            IS0    = (IK-1)*NTH
            DO ITH=2, NTH
              DAM(ITH+IS0) = DAM(1+IS0)
            END DO
          END DO

          DAM2 = 0.
          DO IK=1, NK
            JAC2     = 1./TPI/SIG(IK)
            FRLOCAL  = SIG(IK)*TPIINV
            DAM2(1+(IK-1)*NTH) = 1E-06 * GRAV/FRLOCAL**4 * USTAR * MAX(FMEANWS,FMEAN) * DTG * JAC2 * CG1(IK) / CLATS(ISEA)
          END DO
          DO IK=1, NK
            IS0  = (IK-1)*NTH
            DO ITH=2, NTH
              DAM2(ITH+IS0) = DAM2(1+IS0)
            END DO
          END DO

          DO IK = 1, NK
            DO ITH = 1, NTH
              ISP = ITH + (IK-1)*NTH
              newdac     = VA(ISP,IP) - VAOLD(ISP,JSEA)
              maxdac     = max(DAM(ISP),DAM2(ISP))
              NEWDAC     = SIGN(MIN(MAXDAC,ABS(NEWDAC)), NEWDAC)
              VA(ISP,IP) = max(0., VAOLD(ISP,IP) + NEWDAC)
            ENDDO
          ENDDO
        ENDIF ! B_JGS_LIMITER
      ENDIF  ! FLSOU
    END DO ! JSEA

#ifdef WEIGHTS
    INQUIRE ( FILE='weights.ww3', EXIST = lexist )
    if (.not. lexist) then
      ipitergl = 0
      ipiterout = 0
      DO IP = 1, np
        ipitergl(iplg(IP)) = ipiter(ip)
      END DO
      call mpi_reduce(ipitergl,ipiterout,NP_GLOBAL,MPI_INT,MPI_SUM,0,MPI_COMM_WCMP,ierr)
      if (myrank == 0) tHEN
        OPEN(100001,FILE='weights.ww3',FORM='FORMATTED',STATUS='unknown')
        do ip = 1, np_global
          write(100001,*) ipiterout(ip)
        enddo
        CLOSE(100001)
      endif
    endif
#endif
    !
    call print_memcheck(memunit, 'memcheck_____:'//' WW3_PROP SECTION LOOP 7')
    !
#ifdef W3_DEBUGSRC
    DO JSEA=1,NSEAL
      WRITE(740+IAPROC,*) 'JSEA=', JSEA
      WRITE(740+IAPROC,*) 'min/max/sum(VA)=', minval(VA(:,JSEA)), maxval(VA(:,JSEA)), sum(VA(:,JSEA))
    END DO
    WRITE(740+IAPROC,*) 'min/max/sum(VAtot)=', minval(VA), maxval(VA), sum(VA)
#endif


#ifdef W3_DEBUGSOLVER
    WRITE(740+IAPROC,*) 'PDLIB_JACOBI_GAUSS_SEIDEL_BLOCK, end'
    FLUSH(740+IAPROC)
#endif
  END SUBROUTINE PDLIB_JACOBI_GAUSS_SEIDEL_BLOCK
  !/ ------------------------------------------------------------------- /
  SUBROUTINE PDLIB_EXPLICIT_BLOCK(IMOD, FACX, FACY, DTG, VGX, VGY, LCALC)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Explicit block solver
    !  2. Method : It uses the n-scheme and the idea is to reduce latency due
    !              to DD communication and increase vectorization level on the
    !              single core
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
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
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
    !
#ifdef W3_S
    USE W3SERVMD, only: STRACE
#endif
    USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, DTH, ESIN, ECOS, NSEAL, FSBCCFL, CLATS, MAPFS
    USE W3GDATMD, ONLY: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC, MAPSF, NSEA
    USE W3ODATMD, ONLY: NDSE, NDST, FLBPI, NBI, TBPI0, TBPIN, ISBPI, BBPI0, BBPIN
    USE W3ADATMD, ONLY: DW, CX, CY, MPI_COMM_WCMP
    USE W3IDATMD, ONLY: FLCUR, FLLEV
    USE W3WDATMD, ONLY: VA
    USE W3DISPMD, ONLY: WAVNU3
    USE W3ODATMD, ONLY : IAPROC
#ifdef W3_PDLIB
    USE yowElementpool, only: ne, ine
    USE yowNodepool, only: np, npa, pdlib_ien, pdlib_si, iplg
    use yowDatapool, only: rtype
    use yowExchangeModule, only: PDLIB_exchange2Dreal_zero, PDLIB_exchange2Dreal
    use yowRankModule,     only: ipgl_npa
    USE MPI, only : MPI_MIN
#endif
#ifdef W3_REF1
    USE W3GDATMD, only: REFPARS
#endif

    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: LCALC

    INTEGER, INTENT(IN) :: IMOD

    REAL, INTENT(IN)    :: FACX, FACY, DTG, VGX, VGY

    REAL              :: KTMP(3), UTILDE(NTH), ST(NTH,NPA)
    REAL              :: FL11(NTH), FL12(NTH), FL21(NTH), FL22(NTH), FL31(NTH), FL32(NTH), KKSUM(NTH,NPA)
    REAL              :: FL111(NTH), FL112(NTH), FL211(NTH), FL212(NTH), FL311(NTH), FL312(NTH)

    REAL              :: KSIG(NPA), CGSIG(NPA), CXX(NTH,NPA), CYY(NTH,NPA)
    REAL              :: LAMBDAX(NTH), LAMBDAY(NTH)
    REAL              :: DTMAX(NTH), DTMAXEXP(NTH), DTMAXOUT, DTMAXGL
    REAL              :: FIN(1), FOUT(1), REST, CFLXY, RD1, RD2, RD10, RD20
    REAL              :: UOLD(NTH,NPA), U(NTH,NPA)

    REAL, PARAMETER   :: ONESIXTH = 1.0/6.0
    REAL, PARAMETER   :: ZERO = 0.0
    REAL, PARAMETER   :: THR = 1.0E-12

    INTEGER           :: IK, ISP, ITH, IE, IP, IT, IBI, NI(3), I1, I2, I3, JX, IERR, IP_GLOB, ISEA
    !
    ! 1.b Initialize arrays
    !
    ! 2.  Calculate velocities ---------------- *
    !
    !   2a. Vectorized for all points looping over each wave number (maybe do a dirty save will be nice!)
    !

    DO IK = 1, NK

      IF (LCALC) THEN

        DO IP = 1, NPA
          CALL WAVNU3 (SIG(IK), DW(iplg(IP)), KSIG(IP), CGSIG(IP))
        ENDDO

        DO IP = 1, NPA
          DO ITH = 1, NTH
            ISEA = IPLG(IP)
            CXX(ITH,IP) = CGSIG(IP) * FACX * ECOS(ITH) / CLATS(ISEA)
            CYY(ITH,IP) = CGSIG(IP) * FACY * ESIN(ITH)
          ENDDO ! ith
          IF (FLCUR) THEN
            DO ITH = 1, NTH
              ISEA = IPLG(IP)
              IF (IOBP_LOC(IP) .GT. 0) THEN
                CXX(ITH,IP) = CXX(ITH,IP) + FACX * CX(ISEA)/CLATS(ISEA)
                CYY(ITH,IP) = CYY(ITH,IP) + FACY * CY(ISEA)
              ENDIF
            ENDDO !ith
          ENDIF
        ENDDO

        DO IE = 1, NE

          NI  = INE(:,IE)

          I1  = NI(1)
          I2  = NI(2)
          I3  = NI(3)

          DO ITH = 1, NTH
            LAMBDAX(ITH) = ONESIXTH *(CXX(ITH,I1)+CXX(ITH,I2)+CXX(ITH,I3)) ! Linearized advection speed in X and Y direction
            LAMBDAY(ITH) = ONESIXTH *(CYY(ITH,I1)+CYY(ITH,I2)+CYY(ITH,I3))
            KELEM1(ITH,IE,IK) = LAMBDAX(ITH) * PDLIB_IEN(1,IE) + LAMBDAY(ITH) * PDLIB_IEN(2,IE) ! K-Values - so called Flux Jacobians
            KELEM2(ITH,IE,IK) = LAMBDAX(ITH) * PDLIB_IEN(3,IE) + LAMBDAY(ITH) * PDLIB_IEN(4,IE)
            KELEM3(ITH,IE,IK) = LAMBDAX(ITH) * PDLIB_IEN(5,IE) + LAMBDAY(ITH) * PDLIB_IEN(6,IE)
            KTMP(1)           = KELEM1(ITH,IE,IK) ! Extract
            KTMP(2)           = KELEM2(ITH,IE,IK)
            KTMP(3)           = KELEM3(ITH,IE,IK)
            NM(ITH,IE,IK)     = - 1.D0/MIN(-THR,SUM(MIN(ZERO,KTMP))) ! N-Values
            KELEM1(ITH,IE,IK) = MAX(ZERO,KTMP(1))
            KELEM2(ITH,IE,IK) = MAX(ZERO,KTMP(2))
            KELEM3(ITH,IE,IK) = MAX(ZERO,KTMP(3))
          ENDDO

          FL11  = CXX(:,I2) * PDLIB_IEN(1,IE) + CYY(:,I2) * PDLIB_IEN(2,IE) ! Weights for Simpson Integration
          FL12  = CXX(:,I3) * PDLIB_IEN(1,IE) + CYY(:,I3) * PDLIB_IEN(2,IE)
          FL21  = CXX(:,I3) * PDLIB_IEN(3,IE) + CYY(:,I3) * PDLIB_IEN(4,IE)
          FL22  = CXX(:,I1) * PDLIB_IEN(3,IE) + CYY(:,I1) * PDLIB_IEN(4,IE)
          FL31  = CXX(:,I1) * PDLIB_IEN(5,IE) + CYY(:,I1) * PDLIB_IEN(6,IE)
          FL32  = CXX(:,I2) * PDLIB_IEN(5,IE) + CYY(:,I2) * PDLIB_IEN(6,IE)

          FL111 = 2.d0 * FL11 + FL12
          FL112 = 2.d0 * FL12 + FL11
          FL211 = 2.d0 * FL21 + FL22
          FL212 = 2.d0 * FL22 + FL21
          FL311 = 2.d0 * FL31 + FL32
          FL312 = 2.d0 * FL32 + FL31

          FLALL1(:,IE,IK) = (FL311 + FL212) * ONESIXTH + KELEM1(:,IE,IK)
          FLALL2(:,IE,IK) = (FL111 + FL312) * ONESIXTH + KELEM2(:,IE,IK)
          FLALL3(:,IE,IK) = (FL211 + FL112) * ONESIXTH + KELEM3(:,IE,IK)

        ENDDO  ! IE

        KKSUM = ZERO
        DO IE = 1, NE
          NI = INE(:,IE)
          DO ITH = 1, NTH
            KKSUM(ITH,NI(1)) = KKSUM(ITH,NI(1)) + KELEM1(ITH,IE,IK)
            KKSUM(ITH,NI(2)) = KKSUM(ITH,NI(2)) + KELEM2(ITH,IE,IK)
            KKSUM(ITH,NI(3)) = KKSUM(ITH,NI(3)) + KELEM3(ITH,IE,IK)
          ENDDO
        END DO

        DTMAXEXP = 1.E10
        DTMAX    = 1.E10
        DO IP = 1, npa
          IF (IOBP_LOC(IP) .EQ. 1 .OR. FSBCCFL) THEN
            DO ITH = 1, NTH
              DTMAXEXP(ITH) = PDLIB_SI(IP)/MAX(THR,KKSUM(ITH,IP)*IOBDP_LOC(IP))
              DTMAX(ITH)    = MIN(DTMAX(ITH),DTMAXEXP(ITH))
            ENDDO
            DTMAXOUT = MINVAL(DTMAX)
          ENDIF
        END DO

        FIN(1) = DTMAXOUT
        CALL MPI_ALLREDUCE(FIN,FOUT,1,rtype,MPI_MIN,MPI_COMM_WCMP,ierr)
        DTMAXGL = FOUT(1)

        CFLXY = DBLE(DTG)/DTMAXGL
        REST  = ABS(MOD(CFLXY,1.0d0))
        IF (REST .LT. THR) THEN
          ITER(IK) = ABS(NINT(CFLXY))
        ELSE IF (REST .GT. THR .AND. REST .LT. 0.5d0) THEN
          ITER(IK) = ABS(NINT(CFLXY)) + 1
        ELSE
          ITER(IK) = ABS(NINT(CFLXY))
        END IF

        DO IP = 1, npa
          DTSI(IP) = DBLE(DTMAXGL)/DBLE(ITER(IK))/PDLIB_SI(IP) ! Some precalculations for the time integration.
        END DO

      END IF ! LCALC

      ! Exact and convert Wave Action - should be some subroutine function or whatever
      do ip = 1,npa
        isp = 0
        do ith = 1,nth
          isp = ith + (ik-1)*nth
          u(ith,ip) = va(isp,ip) / cgsig(ip) * clats(iplg(ip))
        enddo
      enddo
      CALL PDLIB_exchange2DREAL(U)

      DO IT = 1, ITER(IK)
        ST = ZERO
        DO IE = 1, NE
          NI  = INE(:,IE)
          DO ITH = 1, NTH
            UTILDE(ITH)   = NM(ITH,IE,IK) * (FLALL1(ITH,IE,IK)*U(ITH,NI(1)) + FLALL2(ITH,IE,IK)*U(ITH,NI(2)) + FLALL3(ITH,IE,IK)*U(ITH,NI(3)))
            ST(ITH,NI(1)) = ST(ITH,NI(1)) + KELEM1(ITH,IE,IK) * (U(ITH,NI(1)) - UTILDE(ITH)) ! the 2nd term are the theta values of each node ...
            ST(ITH,NI(2)) = ST(ITH,NI(2)) + KELEM2(ITH,IE,IK) * (U(ITH,NI(2)) - UTILDE(ITH)) ! the 2nd term are the theta values of each node ...
            ST(ITH,NI(3)) = ST(ITH,NI(3)) + KELEM3(ITH,IE,IK) * (U(ITH,NI(3)) - UTILDE(ITH)) ! the 2nd term are the theta values of each node ...
          ENDDO
        END DO ! IE
        DO IP = 1, NPA
          DO ITH = 1, NTH
            U(ITH,IP) = MAX(ZERO,U(ITH,IP)-DTSI(IP)*ST(ITH,IP)*(1-IOBPA_LOC(IP)))*IOBPD_LOC(ITH,IP)*IOBDP_LOC(IP)
#ifdef W3_REF1
            IF (REFPARS(3).LT.0.5.AND.IOBPD_LOC(ITH,IP).EQ.0.AND.IOBPA_LOC(IP).EQ.0) U(ITH,IP) = UOLD(ITH,IP) ! restores reflected boundary values
#endif
          ENDDO
        ENDDO ! IP

        IF ( FLBPI ) THEN
          DO ITH = 1, NTH
            ISP = ITH + (IK-1) * NTH
            RD1 = RD10 - DTG * REAL(ITER(IK)-IT)/REAL(ITER(IK))
            RD2 = RD20
            IF ( RD2 .GT. 0.001 ) THEN
              RD2    = MIN(1.,MAX(0.,RD1/RD2))
              RD1    = 1. - RD2
            ELSE
              RD1    = 0.
              RD2    = 1.
            END IF
            DO IBI = 1, NBI
              IP_glob = MAPSF(ISBPI(IBI),1)
              JX      = IPGL_npa(IP_glob)
              IF (JX .gt. 0) THEN
                U(ITH,JX) = ( RD1*BBPI0(ISP,IBI) + RD2*BBPIN(ISP,IBI) ) / CGSIG(ISBPI(IBI)) * CLATS(ISBPI(IBI))
              END IF
            END DO
          ENDDO
        ENDIF ! FLBPI

        CALL PDLIB_exchange2DREAL(U)

      ENDDO ! IT

      ! Exact and convert Wave Action
      do ip = 1,npa
        isp = 0
        do ith = 1,nth
          isp = ith + (ik-1)*nth
          va(isp,ip) = u(ith,ip) * cgsig(ip) / clats(iplg(ip))
        end do
      end do

    ENDDO ! IK

  END SUBROUTINE PDLIB_EXPLICIT_BLOCK
  !/ ------------------------------------------------------------------- /
  SUBROUTINE BLOCK_SOLVER_EXPLICIT_INIT()
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Initialization of the block solver
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
    USE W3SERVMD, only: STRACE
#endif
    USE W3GDATMD, only:  NTH, NK
#ifdef W3_PDLIB
    USE YOWNODEPOOL, only: np, npa
    USE YOWELEMENTPOOL, only: ne
#endif
    IMPLICIT NONE
    !/

    ALLOCATE(FLALL1(NTH,NE,NK), FLALL2(NTH,NE,NK), FLALL3(NTH,NE,NK))
    ALLOCATE(KELEM1(NTH,NE,NK), KELEM2(NTH,NE,NK), KELEM3(NTH,NE,NK))
    ALLOCATE(NM(NTH,NE,NK), DTSI(NPA))
    ALLOCATE(ITER(NK))

    !/ ------------------------------------------------------------------- /
    !/
  END SUBROUTINE BLOCK_SOLVER_EXPLICIT_INIT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE BLOCK_SOLVER_INIT(IMOD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Initialization of the block solver
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
    USE W3SERVMD, only: STRACE
#endif
    !
    USE CONSTANTS, only : LPDLIB, TPI, TPIINV
    USE W3GDATMD, only: MAPSF, NSEAL, DMIN, IOBDP, MAPSTA, IOBP, MAPFS, NX
    USE W3ADATMD, only: DW
    USE W3PARALL, only: INIT_GET_ISEA
    USE YOWNODEPOOL, only: iplg, np
    USE yowfunction, only: pdlib_abort
    use YOWNODEPOOL, only: npa
    USE W3GDATMD, only: B_JGS_USE_JACOBI
    USE W3PARALL, only : ListISPprevDir, ListISPnextDir
    USE W3PARALL, only : ListISPprevFreq, ListISPnextFreq
    USE W3GDATMD, only: NSPEC, NTH, NK
    USE W3GDATMD, only: FSTOTALIMP
    USE W3ODATMD, only: IAPROC
    !/
    INTEGER, INTENT(IN) :: IMOD
    !
    !/ ------------------------------------------------------------------- /
    !/
    INTEGER ISP, ITH, IK, ISPprevFreq, ISPnextFreq
    INTEGER NewISP, JTH, istat

    POS_TRICK(1,1) = 2
    POS_TRICK(1,2) = 3
    POS_TRICK(2,1) = 3
    POS_TRICK(2,2) = 1
    POS_TRICK(3,1) = 1
    POS_TRICK(3,2) = 2

#ifdef W3_DEBUGINIT
    WRITE(740+IAPROC,*) 'BLOCK_SOLVER_INIT, step 1'
    FLUSH(740+IAPROC)
#endif
    ALLOCATE(ListISPnextDir(NSPEC), ListISPprevDir(NSPEC), ListISPnextFreq(NSPEC), ListISPprevFreq(NSPEC),stat=istat)
    IF (istat /= 0) CALL PDLIB_ABORT(8)
#ifdef W3_DEBUGINIT
    WRITE(740+IAPROC,*) 'BLOCK_SOLVER_INIT, step 2'
    FLUSH(740+IAPROC)
#endif
    DO ISP=1,NSPEC
      ITH    = 1 + MOD(ISP-1,NTH)
      IK     = 1 + (ISP-1)/NTH
      IF (IK .eq. 1) THEN
        ISPprevFreq=-1
      ELSE
        ISPprevFreq=iTH + (IK-1 -1)*NTH
      END IF
      ListISPprevFreq(ISP)=ISPprevFreq
      IF (IK .eq. NK) THEN
        ISPnextFreq=-1
      ELSE
        ISPnextFreq=iTH + (IK+1 -1)*NTH
      END IF
      ListISPnextFreq(ISP)=ISPnextFreq
      !
      IF (ITH .eq. 1) THEN
        JTH=NTH
      ELSE
        JTH=ITH-1
      ENDIF
      NewISP=JTH + (IK-1)*NTH
      ListISPprevDir(ISP)=NewISP
      IF (ITH .eq. NTH) THEN
        JTH=1
      ELSE
        JTH=ITH+1
      ENDIF
      NewISP=JTH + (IK-1)*NTH
      ListISPnextDir(ISP)=NewISP
    END DO
#ifdef W3_DEBUGINIT
    WRITE(740+IAPROC,*) 'BLOCK_SOLVER_INIT, step 3'
    FLUSH(740+IAPROC)
#endif
    IF (FSTOTALIMP .and. B_JGS_USE_JACOBI) THEN
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'BLOCK_SOLVER_INIT, step 4'
      FLUSH(740+IAPROC)
#endif
      CALL JACOBI_INIT(IMOD)
#ifdef W3_DEBUGINIT
      WRITE(740+IAPROC,*) 'BLOCK_SOLVER_INIT, step 5'
      FLUSH(740+IAPROC)
#endif
    END IF
#ifdef W3_DEBUGINIT
    WRITE(740+IAPROC,*) 'BLOCK_SOLVER_INIT, step 6'
    FLUSH(740+IAPROC)
#endif
  END SUBROUTINE BLOCK_SOLVER_INIT
  !/ ------------------------------------------------------------------ /
  SUBROUTINE SET_IOBDP_PDLIB
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set depth pointer
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
    USE W3SERVMD, only: STRACE
#endif
    USE CONSTANTS, only : LPDLIB
    USE W3GDATMD, only: MAPSF, NSEAL, DMIN, MAPSTA, NX
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3ADATMD, only: DW
    USE W3PARALL, only: INIT_GET_ISEA
    USE YOWNODEPOOL, only: iplg, np, npa
    !/
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
    INTEGER :: JSEA, ISEA, IX, IP, IP_glob
    REAL*8, PARAMETER :: DTHR = 10E-6
#ifdef W3_S
    CALL STRACE (IENT, 'SETDEPTH_PDLIB')
#endif
    DO JSEA=1,NPA
      IP = JSEA
      IP_glob = iplg(IP)
      IF (DW(IP_glob) .LT. DMIN + DTHR) THEN
        IOBDP_LOC(IP)  = 0
      ELSE
        IOBDP_LOC(IP)  = 1
      ENDIF
    END DO
    !/
    !/ End of SETDEPTH_PDLIB --------------------------------------------- /
    !/
  END SUBROUTINE SET_IOBDP_PDLIB


  SUBROUTINE SET_IOBPA_PDLIB
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set depth pointer
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
    USE W3SERVMD, only: STRACE
#endif
    USE CONSTANTS, only : LPDLIB
    USE W3GDATMD, only: MAPSF, NSEAL, DMIN, MAPSTA, NX
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3ADATMD, only: DW
    USE W3PARALL, only: INIT_GET_ISEA
    USE YOWNODEPOOL, only: iplg, np
    !/
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
    INTEGER :: JSEA, ISEA, IX, IP, IP_glob
    REAL*8, PARAMETER :: DTHR = 10E-6
#ifdef W3_S
    CALL STRACE (IENT, 'SETDEPTH_PDLIB')
#endif
    DO JSEA=1,NSEAL
      IP_glob = iplg(JSEA)
      IF (MAPSTA(1,IP_glob).EQ.2) THEN
        IOBPA_LOC(JSEA) = 1
      ELSE
        IOBPA_LOC(JSEA) = 0
      ENDIF
    END DO
    !/
    !/ End of SETDEPTH_PDLIB --------------------------------------------- /
    !/
  END SUBROUTINE SET_IOBPA_PDLIB


  SUBROUTINE SET_UG_IOBP_PDLIB_INIT()
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        Fabrice Ardhuin            |
    !/                  |        Aron Roland                |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         17-Apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    23-Aug-2011 : Origination.                        ( version 4.04 )
    !/    17-Apr-2016 : Uses optimized boundary detection   ( version 5.10 )
    !/
    !  1. Purpose :
    !
    !     Redefines the values of the boundary points and angle pointers
    !     based on the MAPSTA array
    !
    !  2. Method :
    !
    !     Adapted boundary detection from A. Roland and M. Dutour (WWM code)
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !     Local variables.
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !

    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_GRID  Prog. WW3_GRID Grid preprocessor
    !      W3ULEV    Subr. W3UPDTMD Water level update
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
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !
    !
    ! 10. Source code :
    !/ ------------------------------------------------------------------- /
    !/
    !
    USE CONSTANTS
    !
    !
    USE W3GDATMD, only: NX, NY, NSEA, MAPFS,                        &
         NK, NTH, DTH, XFR, MAPSTA, COUNTRI,         &
         ECOS, ESIN, IEN, NTRI, TRIGP,               &
         IOBP,IOBPD, IOBPA,                          &
#ifdef W3_REF1
         REFPARS, REFLC, REFLD,                      &
#endif
         ANGLE0, ANGLE, NSEAL

    USE W3ODATMD, only: TBPI0, TBPIN, FLBPI
    USE W3ADATMD, only: CG, CX, CY, ATRNX, ATRNY, ITIME, CFLXYMAX
    USE W3GDATMD, only: IOBP_LOC, IOBPD_LOC, IOBPA_LOC, IOBDP_LOC
    USE W3IDATMD, only: FLCUR
    USE W3ODATMD, only : IAPROC
    USE YOWNODEPOOL,    only: PDLIB_SI, PDLIB_IEN, PDLIB_TRIA, ipgl, iplg, npa, np
    use yowElementpool, only: NE, INE
    use yowExchangeModule, only : PDLIB_exchange1DREAL
#ifdef W3_S
    USE W3SERVMD, only: STRACE
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: ITH, IX, I, J, IP, IE, NDIRSUM
    REAL (KIND = 8)         :: COSSUM, SINSUM
    REAL (KIND = 8)         :: DIRMIN, DIRMAX, SHIFT, TEMPO, DIRCOAST
    REAL (KIND = 8)         :: X1, X2, Y1, Y2, DXP1, DXP2, DXP3
    REAL (KIND = 8)         :: DYP1, DYP2, DYP3, eDet1, eDet2, EVX, EVY
    REAL(KIND=8), PARAMETER :: THR    = TINY(1.)
    INTEGER                 :: I1, I2, I3
    INTEGER                 :: ITMP(NX), NEXTVERT(NX), PREVVERT(NX)
    INTEGER                 :: MAX_IOBPD, MIN_IOBPD
    REAL                    :: rtmp(NPA)
    CHARACTER(60) :: FNAME
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/ ------------------------------------------------------------------- /
    !
    !
    DO IE = 1, NE
      I1   =   INE(1,IE)
      I2   =   INE(2,IE)
      I3   =   INE(3,IE)
      DXP1 =   PDLIB_IEN(6,IE)
      DYP1 = - PDLIB_IEN(5,IE)
      DXP2 =   PDLIB_IEN(2,IE)
      DYP2 = - PDLIB_IEN(1,IE)
      DXP3 =   PDLIB_IEN(4,IE)
      DYP3 = - PDLIB_IEN(3,IE)
      DO ITH = 1, NTH
        EVX = ECOS(ITH)
        EVY = ESIN(ITH)
        DO I = 1, 3
          IF (I .eq. 1) THEN
            x1 =   DXP1
            y1 =   DYP1
            x2 = - DXP3
            y2 = - DYP3
            IP =   I1
          ELSE IF (I.eq.2) THEN
            x1 =   DXP2
            y1 =   DYP2
            x2 = - DXP1
            y2 = - DYP1
            IP =   I2
          ELSE IF (I.eq.3) THEN
            x1 =   DXP3
            y1 =   DYP3
            x2 = - DXP2
            y2 = - DYP2
            IP =   I3
          END IF
          IF (IOBP_LOC(IP) .eq. 0) THEN ! physical boundary
            eDet1 = THR-x1*EVY+y1*EVX
            eDet2 = THR+x2*EVY-y2*EVX
            IF ((eDet1.gt.0.).and.(eDet2.gt.0.)) THEN
              ! this is the case of waves going towards the boundary ...
              IOBPD_LOC(ITH,IP) = 1
            ENDIF
          ELSE ! water ...
            IOBPD_LOC(ITH,IP) = 1
          END IF
        END DO
      END DO
    END DO

    DO ITH = 1, NTH
      rtmp = REAL(IOBPD_LOC(ITH,1:NPA))
      CALL PDLIB_exchange1Dreal(rtmp)
      IOBPD_LOC(ITH,1:NPA) = INT(rtmp)
    ENDDO
    MAX_IOBPD = MAXVAL(IOBPD_LOC)
    MIN_IOBPD = MINVAL(IOBPD_LOC)

    IF (MAX_IOBPD .gt. 1 .OR. MIN_IOBPD .lt. 0) THEN
      WRITE(*,*) 'MAX_IOBPD - MIN_IOBPD', MAX_IOBPD, MIN_IOBPD
      STOP 'MAX_IOBPD ERRROR'
    ENDIF

#ifdef W3_DEBUGSETUGIOBP
    WRITE(740+IAPROC,*) 'Calling SETUGIOBP, step 5'
    FLUSH(740+IAPROC)
#endif
    DO IP = 1, NPA
      IF ( IOBPA_LOC(IP) .eq. 1 .OR. IOBP_LOC(IP) .eq. 3 .OR. IOBP_LOC(IP) .eq. 4) IOBPD_LOC(:,IP) = 1
    END DO
#ifdef W3_DEBUGSETUGIOBP
    WRITE(740+IAPROC,*) 'Calling SETUGIOBP, step 7'
    FLUSH(740+IAPROC)
#endif
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3. Updates the reflection direction and sharp / flat shoreline angle

#ifdef W3_REF1
    !
    ! Finds the shoreline direction from IOBPD
    !
    REFLC(1,:)= 0.
    REFLD(:,:)= 1
    DO IP=1,NX
      IF (IOBP(IP).EQ.0.AND.MAPSTA(1,IP).EQ.1) THEN
        COSSUM=0.
        SINSUM=0.
        NDIRSUM=0.
        DO ITH=1,NTH
          COSSUM=COSSUM+IOBPD(ITH,IP)*ECOS(ITH)
          SINSUM=SINSUM+IOBPD(ITH,IP)*ESIN(ITH)
          NDIRSUM=NDIRSUM+IOBPD(ITH,IP)
        END DO
        DIRCOAST=ATAN2(SINSUM, COSSUM)
        REFLD(1,MAPFS(1,IP)) = 1+MOD(NTH+NINT(DIRCOAST/DTH),NTH)
        REFLD(2,MAPFS(1,IP)) = 4-MAX(2,NINT(4.*REAL(NDIRSUM)/REAL(NTH)))
        REFLC(1,MAPFS(1,IP))= REFPARS(1)
      END IF
    END DO
#endif
#ifdef W3_DEBUGSETUGIOBP
    WRITE(740+IAPROC,*) 'Calling SETUGIOBP, step 8'
    FLUSH(740+IAPROC)
#endif
  END SUBROUTINE SET_UG_IOBP_PDLIB_INIT
  !/ ------------------------------------------------------------------- /
  !/ ------------------------------------------------------------------- /
  SUBROUTINE BLOCK_SOLVER_FINALIZE
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Finalize Solver
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
    USE W3SERVMD, only: STRACE
#endif
    USE W3GDATMD, only: B_JGS_USE_JACOBI
    !/
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
#ifdef W3_S
    CALL STRACE (IENT, 'BLOCK_SOLVER_FINALIZE')
#endif
    IF (B_JGS_USE_JACOBI) THEN
      CALL JACOBI_FINALIZE
    END IF
    !/
    !/ End of SETDEPTH_PDLIB --------------------------------------------- /
    !/
  END SUBROUTINE BLOCK_SOLVER_FINALIZE
  !/ ------------------------------------------------------------------- /
  SUBROUTINE DEALLOCATE_PDLIB_GLOBAL(IMOD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Init jacobi solver
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
    USE W3SERVMD, only: STRACE
#endif
    USE W3GDATMD, only: NSPEC, B_JGS_BLOCK_GAUSS_SEIDEL, GRIDS
    use YOWNODEPOOL, only: PDLIB_NNZ, npa, np
    USE yowfunction, only: pdlib_abort
    USE W3GDATMD, only: NTH, NK, NSEAL
    USE W3PARALL, only: IMEM
#ifdef W3_DEBUGINIT
    USE W3ODATMD, only : IAPROC
#endif
    !/
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
    INTEGER, INTENT(IN) :: IMOD

    DEALLOCATE (                 &
                                ! GRIDS(IMOD)%TRIGP,    &
         GRIDS(IMOD)%SI,         &
         GRIDS(IMOD)%TRIA,       &
         GRIDS(IMOD)%CROSSDIFF,  &
         GRIDS(IMOD)%IEN,        &
         GRIDS(IMOD)%LEN,        &
         GRIDS(IMOD)%ANGLE,      &
         GRIDS(IMOD)%ANGLE0,     &
         GRIDS(IMOD)%CCON,       &
         GRIDS(IMOD)%COUNTCON,   &
         GRIDS(IMOD)%INDEX_CELL, &
         GRIDS(IMOD)%IE_CELL,    &
         GRIDS(IMOD)%POS_CELL,   &
         GRIDS(IMOD)%IAA,        &
         GRIDS(IMOD)%JAA,        &
         GRIDS(IMOD)%POSI,       &
         GRIDS(IMOD)%I_DIAG,     &
         GRIDS(IMOD)%JA_IE,      &
                                !GRIDS(IMOD)%IOBP,      &
                                !GRIDS(IMOD)%IOBPD,     &
         GRIDS(IMOD)%IOBDP,      &
         GRIDS(IMOD)%IOBPA  )
    !/
    !/ End of DEALLOCATE_PDLIB_GLOBAL ------------------------------------------------ /
    !/
  END SUBROUTINE DEALLOCATE_PDLIB_GLOBAL

  SUBROUTINE ERGOUT(FHNDL, ERGNAME)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :      01-Januar-2023 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 7.xx )
    !/
    !  1. Purpose : write spatial out for xfn
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
    USE W3SERVMD, only: STRACE
#endif
    USE W3GDATMD, only: NSPEC, NTH, NK, NSEAL
    USE W3WDATMD, only: VA, VAOLD
    IMPLICIT NONE

    INTEGER, INTENT(IN)           :: FHNDL
    CHARACTER(LEN=*), INTENT(IN) :: ERGNAME
    REAL    :: SUMVA(NSEAL)
    INTEGER :: JSEA

    IF (LINIT_OUTPUT) THEN
      OPEN(FHNDL, FILE  = TRIM(ERGNAME), FORM = 'UNFORMATTED')
      LINIT_OUTPUT = .false.
    ENDIF

    RTIME = RTIME + 1.

    DO JSEA = 1, NSEAL
      SUMVA(JSEA) = SUM(VA(:,JSEA))
    ENDDO

    WRITE(FHNDL)  RTIME
    WRITE(FHNDL) (SUMVA(JSEA), SUMVA(JSEA), SUMVA(JSEA), JSEA = 1, NSEAL)

  END SUBROUTINE ERGOUT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE JACOBI_INIT(IMOD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Init jacobi solver
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
    USE W3SERVMD, only: STRACE
#endif
    USE W3GDATMD, only: NSPEC, B_JGS_BLOCK_GAUSS_SEIDEL, GRIDS
    use YOWNODEPOOL, only: PDLIB_NNZ, npa, np
    USE yowfunction, only: pdlib_abort
    USE W3GDATMD, only: NTH, NK, NSEAL
    USE W3PARALL, only: IMEM
#ifdef W3_DEBUGINIT
    USE W3ODATMD, only : IAPROC
#endif
    !/
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
    INTEGER, INTENT(IN) :: IMOD

    INTEGER istat
    IF (IMEM == 1) THEN
      ALLOCATE(ASPAR_JAC(NSPEC, PDLIB_NNZ), stat=istat)
      if(istat /= 0) CALL PDLIB_ABORT(9)
    ELSE IF (IMEM == 2) THEN
      ALLOCATE(ASPAR_DIAG_ALL(NSPEC, npa), stat=istat)
      if(istat /= 0) CALL PDLIB_ABORT(9)
    ENDIF
    ALLOCATE(B_JAC(NSPEC,NSEAL), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(10)
    ALLOCATE(CAD_THE(NSPEC,NSEAL), stat=istat)
    if(istat /= 0) CALL PDLIB_ABORT(11)
    IF (FreqShiftMethod .eq. 1) THEN
      ALLOCATE(CAS_SIG(NSPEC,NSEAL), stat=istat)
      if(istat /= 0) CALL PDLIB_ABORT(11)
    ELSE IF (FreqShiftMethod .eq. 2) THEN
      ALLOCATE(CWNB_SIG_M2(1-NTH:NSPEC,NSEAL), stat=istat)
      if(istat /= 0) CALL PDLIB_ABORT(11)
    END IF
    IF (.NOT. B_JGS_BLOCK_GAUSS_SEIDEL) THEN
      ALLOCATE(U_JAC(NSPEC,npa), stat=istat)
      if(istat /= 0) CALL PDLIB_ABORT(12)
    END IF
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE JACOBI_INIT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE JACOBI_FINALIZE
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Finalize jacobi solver
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
    USE W3GDATMD, only: B_JGS_BLOCK_GAUSS_SEIDEL
    USE W3PARALL, only: IMEM
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETER
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'JACOBI_FINALIZE')
#endif
    IF (IMEM == 1) THEN
      DEALLOCATE(ASPAR_JAC)
    ELSE IF (IMEM == 2) THEN
      DEALLOCATE(ASPAR_DIAG_ALL)
    ENDIF
    DEALLOCATE(B_JAC)
    DEALLOCATE(CAD_THE)
    IF (FreqShiftMethod .eq. 1) THEN
      DEALLOCATE(CAS_SIG)
    ELSE IF (FreqShiftMethod .eq. 2) THEN
      DEALLOCATE(CWNB_SIG_M2)
    END IF
    IF (.NOT. B_JGS_BLOCK_GAUSS_SEIDEL) THEN
      DEALLOCATE(U_JAC)
    END IF
    !/
    !/ End of JACOBI_FINALIZE -------------------------------------------- /
    !/
  END SUBROUTINE JACOBI_FINALIZE
  !/ ------------------------------------------------------------------- /
END MODULE PDLIB_W3PROFSMD
