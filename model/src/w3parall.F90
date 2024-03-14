!> @file
!> @brief Parallel routines for implicit solver.
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date   01-Jun-2018
!>

!/ ------------------------------------------------------------------- /
!>
!> @brief Parallel routines for implicit solver.
!>
!> @author Aron Roland
!> @author Mathieu Dutour-Sikiric
!> @date   01-Jun-2018
!>
!> @copyright Copyright 2009-2022 National Weather Service (NWS),
!>       National Oceanic and Atmospheric Administration.  All rights
!>       reserved.  WAVEWATCH III is a trademark of the NWS.
!>       No unauthorized use without permission.
!>
MODULE W3PARALL
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
  !/   01-June-2018 : Origination.                        ( version 6.04 )
  !/
  !  1. Purpose : Parallel routines for implicit solver
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
  !/
  !/ ------------------------------------------------------------------- /
  !/ Parameter list
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
#ifdef W3_S
  INTEGER, SAVE           :: IENT = 0
#endif
  !
#ifdef W3_PDLIB
  INTEGER :: PDLIB_NSEAL, PDLIB_NSEALM
  INTEGER, ALLOCATABLE :: JX_TO_JSEA(:), ISEA_TO_JSEA(:)
#endif

  INTEGER, ALLOCATABLE :: ListISPnextDir(:), ListISPprevDir(:)
  INTEGER, ALLOCATABLE :: ListISPnextFreq(:), ListISPprevFreq(:)

  LOGICAL, PARAMETER   :: LSLOC = .true.
  INTEGER, PARAMETER   :: IMEM = 1

  
  REAL*8,  PARAMETER     :: ONESIXTH  = 1.0d0/6.0d0
  REAL*8,  PARAMETER     :: ONETHIRD  = 1.0d0/3.0d0
  REAL*8,  PARAMETER     :: ZERO      = 0.0d0
  REAL*8,  PARAMETER     :: ONE       = 1.d0 

  REAL*8,  PARAMETER    :: THR8 = TINY(1.d0)
  REAL,  PARAMETER      :: THR  = TINY(1.0)

  REAL*8, PARAMETER     :: PI = 3.14159265358979323846D0
  REAL*8, PARAMETER     :: DEGRAD    = PI/180.d0
  REAL*8, PARAMETER     :: RADDEG    = 180.d0/PI
  REAL*8, PARAMETER     :: KDMAX     = 200.d0

CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief NA
  !>
  !> @param[out] eTime
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE WAV_MY_WTIME(eTime)
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose :
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
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    IMPLICIT NONE
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER mpimode
    REAL(8), intent(out) :: eTime
#ifdef W3_MPI
    REAL(8) mpi_wtime
#endif
    mpimode=0
#ifdef W3_MPI
    mpimode=1
    eTime=mpi_wtime()
#endif
#ifdef W3_S
    CALL STRACE (IENT, 'WAV_MY_WTIME')
#endif
    IF (mpimode .eq. 0) THEN
      CALL CPU_TIME(eTime)
    END IF
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE WAV_MY_WTIME
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Print timings.
  !>
  !> @param[in] string
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE PRINT_MY_TIME(string)
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Print timings
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
    USE W3ODATMD, ONLY : IAPROC
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !
    character(*), intent(in) :: string
    REAL(8) :: eTime
#ifdef W3_S
    CALL STRACE (IENT, 'PRINT_MY_TIME')
#endif
    CALL WAV_MY_WTIME(eTime)
    WRITE(740+IAPROC,*) 'TIMING time=', eTime, ' at step ', string
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE PRINT_MY_TIME
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Sync global local arrays.
  !>
  !> @param[in] IMOD
  !> @param[in] IsMulti
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY(IMOD, IsMulti)
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Sync global local arrays
    !  2. Method :
    ! 		All the process need to have IPGL_tot and IPGL_TO_PROC
    ! 		This is especially the case for the output process.
    ! 		So we need some painful exportation business
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
#ifdef W3_PDLIB
    USE yowDatapool, only: istatus
    USE yowNodepool, only: np_global
    USE W3ODATMD, ONLY: NTPROC, NAPROC, IAPROC
    USE W3GDATMD, ONLY: MAPSF, NSEA
    USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
    USE yowRankModule, only : IPGL_TO_PROC, IPGL_tot
    USE WMMDATMD, ONLY: MDATAS
#endif
    IMPLICIT NONE
#ifdef W3_PDLIB
    INCLUDE "mpif.h"
#endif
    INTEGER, intent(in) :: IMOD
    logical, intent(in) :: IsMulti
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_PDLIB
    INTEGER :: Iarr(1)
    INTEGER :: ISEA, IP_glob
    INTEGER :: IPROC, IERR_MPI, istat
#endif

#ifdef W3_S
    CALL STRACE (IENT, 'SYNCHRONIZE_IPGL_ETC_ARRAY')
#endif

#ifdef W3_PDLIB
    IF (IAPROC .le. NAPROC) THEN
      IF (IAPROC .eq. 1) THEN
        Iarr(1)=np_global
        DO IPROC=NAPROC+1,NTPROC
          CALL MPI_SEND(Iarr,1,MPI_INT, IPROC-1, 37, MPI_COMM_WAVE, IERR_MPI)
        END DO
        DO IPROC=NAPROC+1,NTPROC
          CALL MPI_SEND(ipgl_tot,np_global,MPI_INT, IPROC-1, 43, MPI_COMM_WAVE, IERR_MPI)
          CALL MPI_SEND(ipgl_to_proc,np_global,MPI_INT, IPROC-1, 91, MPI_COMM_WAVE, IERR_MPI)
        END DO
      END IF
    ELSE
      CALL MPI_RECV(Iarr,1,MPI_INT, 0, 37, MPI_COMM_WAVE, istatus, IERR_MPI)
      np_global=Iarr(1)
      allocate(IPGL_tot(np_global), IPGL_TO_PROC(np_global), stat=istat)
      CALL MPI_RECV(ipgl_tot,np_global,MPI_INT, 0, 43, MPI_COMM_WAVE, istatus, IERR_MPI)
      CALL MPI_RECV(ipgl_to_proc,np_global,MPI_INT, 0, 91, MPI_COMM_WAVE, istatus, IERR_MPI)
    END IF
    IF (IsMulti) THEN
      WRITE(*,*) ' Before allocation of MDATAS % SEA_IPGL, SEA_IPGL_TO_PROC : IMOD=', IMOD, ' NSEA=', NSEA
      ALLOCATE(MDATAS(IMOD)%SEA_IPGL(NSEA), MDATAS(IMOD)%SEA_IPGL_TO_PROC(NSEA), STAT=ISTAT)
      !CHECK_ALLOC_STATUS ( ISTAT )
      DO ISEA=1,NSEA
        IP_glob = MAPSF(ISEA, 1)
        MDATAS(IMOD)%SEA_IPGL(ISEA) = IPGL_tot(IP_glob)
        MDATAS(IMOD)%SEA_IPGL_TO_PROC(ISEA) = IPGL_TO_PROC(IP_glob)
      END DO
    END IF
#endif
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE SYNCHRONIZE_IPGL_ETC_ARRAY
  !/ ....................----------------------------------------------- /
  !>
  !> @brief Setup NSEAL, NSEALM in context of PDLIB.
  !>
  !> @param[out] NSEALout
  !> @param[out] NSEALMout
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SET_UP_NSEAL_NSEALM(NSEALout, NSEALMout)
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Setup nseal, nsealm in contect of pdlib
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
    !/
    !/ ------------------------------------------------------------------- /
#ifdef W3_PDLIB
    use yowDatapool, only: istatus
    use yowNodepool, only: npa
    use yowRankModule, only : rank
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE
#endif
#ifdef W3_MPI
    USE W3ADATMD, ONLY: MPI_COMM_WAVE, MPI_COMM_WCMP
#endif
    USE CONSTANTS, ONLY : LPDLIB
    USE W3GDATMD, ONLY: NSEA
    USE W3ODATMD, ONLY: NTPROC, NAPROC, IAPROC
    IMPLICIT NONE
    INTEGER, intent(out) :: NSEALout, NSEALMout
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'SET_UP_NSEAL_NSEALM')
#endif

#ifdef W3_SHRD
    NSEALout  = NSEA
    NSEALMout = NSEA
#endif
    !
#ifdef W3_DIST
    IF (.NOT. LPDLIB ) THEN
      IF ( IAPROC .LE. NAPROC ) THEN
        NSEALout  = 1 + (NSEA-IAPROC)/NAPROC
      ELSE
        NSEALout  = 0
      END IF
      NSEALMout = 1 + (NSEA-1)/NAPROC
    ELSE
#endif
#ifdef W3_PDLIB
      IF (GTYPE .eq. UNGTYPE) THEN
        NSEALout  = PDLIB_NSEAL
        NSEALMout = PDLIB_NSEALM
      ELSE
        IF ( IAPROC .LE. NAPROC ) THEN
          NSEALout  = 1 + (NSEA-IAPROC)/NAPROC
        ELSE
          NSEALout  = 0
        END IF
        NSEALMout = 1 + (NSEA-1)/NAPROC
      ENDIF
#endif
#ifdef W3_DIST
    ENDIF
#endif
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE SET_UP_NSEAL_NSEALM
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Set JSEA for all schemes.
  !>
  !> @param[in]  ISEA
  !> @param[out] JSEA
  !> @param[out] ISPROC
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set Jsea for all schemes
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
    !/
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE yowRankModule, only : IPGL_TO_PROC, IPGL_tot
    use yowNodepool, only: ipgl, iplg
#endif
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    INTEGER, intent(in) :: ISEA
    INTEGER, intent(out) :: JSEA, ISPROC
    INTEGER IP_glob
#ifdef W3_S
    CALL STRACE (IENT, 'INIT_GET_JSEA_ISPROC')
#endif

#ifdef W3_PDLIB
    IF (.NOT. LPDLIB) THEN
#endif
      JSEA   = 1 + (ISEA-1)/NAPROC
      ISPROC = ISEA - (JSEA-1)*NAPROC
#ifdef W3_PDLIB
    ELSE
      IP_glob = MAPSF(ISEA,1)
      IF (IAPROC .le. NAPROC) THEN
        JSEA   = ISEA_TO_JSEA(ISEA)
      ELSE
        JSEA   = -1
      END IF
      ISPROC = IPGL_TO_PROC(IP_glob)
    ENDIF
#endif
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE INIT_GET_JSEA_ISPROC
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Set belongings of JSEA in context of PDLIB.
  !>
  !> @param[in]  ISEA
  !> @param[out] JSEA
  !> @param[out] IBELONG
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE GET_JSEA_IBELONG(ISEA, JSEA, IBELONG)
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set belongings of jsea in context of pdlib
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
    !/
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE, MAPSF
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE yowRankModule, only : IPGL_TO_PROC, IPGL_tot, IPGL_npa
    use yowNodepool, only: ipgl, iplg
#endif
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
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
    INTEGER, intent(in) :: ISEA
    INTEGER, intent(out) :: JSEA, IBELONG
    INTEGER ISPROC, IX, JX
#ifdef W3_S
    CALL STRACE (IENT, 'GET_JSEA_IBELONG')
#endif
    IF (.NOT. LPDLIB) THEN
      JSEA   = 1 + (ISEA-1)/NAPROC
      ISPROC = ISEA - (JSEA-1)*NAPROC
      IF (ISPROC .eq. IAPROC) THEN
        IBELONG=1
      ELSE
        IBELONG=0
      END IF
    ELSE
#ifdef W3_PDLIB
      IF (GTYPE .ne. UNGTYPE) THEN
        JSEA   = 1 + (ISEA-1)/NAPROC
        ISPROC = ISEA - (JSEA-1)*NAPROC
        IF (ISPROC .eq. IAPROC) THEN
          IBELONG=1
        ELSE
          IBELONG=0
        END IF
      ELSE
        IF (IAPROC .le. NAPROC) THEN
          IX     = MAPSF(ISEA,1)
          JX     = IPGL_npa(IX)
          IF (JX .eq. 0) THEN
            IBELONG=0
            JSEA=-1
          ELSE
            IBELONG=1
            JSEA = JX_TO_JSEA(JX)
          END IF
        ELSE
          IBELONG=0
          JSEA=-1
        END IF
      ENDIF
#endif
    ENDIF
    !/
    !/ End of INIT_GET_ISEA ---------------------------------------------- /
    !/
  END SUBROUTINE GET_JSEA_IBELONG
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Set ISEA for all schemes.
  !>
  !> @param[out] ISEA
  !> @param[in]  JSEA
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE INIT_GET_ISEA(ISEA, JSEA)
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Set Isea for all schemes
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
    !/
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE YOWNODEPOOL, ONLY: iplg
#endif
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !/ ------------------------------------------------------------------- /
    !
    USE W3ODATMD, ONLY: OUTPTS, IAPROC, NAPROC
    USE W3GDATMD, ONLY: GTYPE, UNGTYPE
    USE CONSTANTS, ONLY : LPDLIB
#ifdef W3_PDLIB
    USE YOWNODEPOOL, ONLY: iplg
#endif
    IMPLICIT NONE
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    INTEGER, intent(in) :: JSEA
    INTEGER, intent(out) :: ISEA
#ifdef W3_S
    CALL STRACE (IENT, 'INIT_GET_ISEA')
#endif
#ifdef W3_SHRD
    ISEA         = JSEA
#endif
#ifdef W3_DIST
    IF (.NOT. LPDLIB) THEN
      ISEA         = IAPROC + (JSEA-1)*NAPROC
    ELSE
#endif
#ifdef W3_PDLIB
      IF (GTYPE .eq. UNGTYPE) THEN
        ISEA         = iplg(JSEA)
      ELSE
        ISEA         = IAPROC + (JSEA-1)*NAPROC
      ENDIF
#endif
#ifdef W3_DIST
    ENDIF
#endif
    !/
    !/ End of INIT_GET_ISEA ------------------------------------------------ /
    !/
  END SUBROUTINE INIT_GET_ISEA

  !>
  !> @brief Sync global array in context of PDLIB.
  !>
  !> @details An array of size (NSEA) is send but only the (1:NSEAL) values
  !>          are correct. The program synchonizes everything on all nodes.
  !>
  !> @param[inout] TheVar
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY(TheVar)
    !/ ------------------------------------------------------------------- /
    !**********************************************************************
    !*  An array of size (NSEA) is send but only the (1:NSEAL) values     *
    !*  are correct. The program synchonizes everything on all nodes.     *
    !**********************************************************************
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
    !/   01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : Sync global array in context of pdlib
    !  2. Method :
    !			An array of size (NSEA) is send but only the (1:NSEAL) values
    ! 			are correct. The program synchonizes everything on all nodes.
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
    USE W3GDATMD, ONLY: NSEAL, NSEA, NX
#ifdef W3_PDLIB
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    USE W3ADATMD, ONLY: MPI_COMM_WCMP
    use yowDatapool, only: rtype, istatus
    USE yowNodepool, only: npa
    use yowNodepool, only: iplg
    use yowDatapool, only: rkind
#endif
    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
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
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    INTEGER ISEA, JSEA, Status(NX), rStatus(NX)
    INTEGER IPROC, I, ierr, IP, IX, IP_glob
#ifdef W3_PDLIB
    REAL(rkind), intent(inout) :: TheVar(NX)
    REAL(rkind) ::  rVect(NX)
#else
    DOUBLE PRECISION, intent(inout) :: TheVar(NX)
    DOUBLE PRECISION                ::  rVect(NX)
#endif
    Status=0
#ifdef W3_S
    CALL STRACE (IENT, 'SYNCHRONIZE_GLOBAL_ARRAY')
#endif
#ifdef W3_PDLIB
    DO IP=1,npa
      IP_glob=iplg(IP)
      Status(IP_glob)=1
    END DO
    IF (IAPROC .eq. 1) THEN
      DO iProc=2,NAPROC
        CALL MPI_RECV(rVect,NX,rtype, iProc-1, 19, MPI_COMM_WCMP, istatus, ierr)
        CALL MPI_RECV(rStatus,NX,MPI_INTEGER, iProc-1, 23, MPI_COMM_WCMP, istatus, ierr)
        DO I=1,NX
          IF (rStatus(I) .eq. 1) THEN
            TheVar(I)=rVect(I)
            Status(I)=1
          END IF
        END DO
      END DO
      DO IPROC=2,NAPROC
        CALL MPI_SEND(TheVar,NX,rtype, iProc-1, 29, MPI_COMM_WCMP, ierr)
      END DO
    ELSE
      CALL MPI_SEND(TheVar,NX,rtype, 0, 19, MPI_COMM_WCMP, ierr)
      CALL MPI_SEND(Status,NX,MPI_INTEGER, 0, 23, MPI_COMM_WCMP, ierr)
      CALL MPI_RECV(TheVar,NX,rtype, 0, 29, MPI_COMM_WCMP, istatus, ierr)
    END IF
#endif
    !/
    !/ End of JACOBI_INIT ------------------------------------------------ /
    !/
  END SUBROUTINE SYNCHRONIZE_GLOBAL_ARRAY
  !/ ------------------------------------------------------------------- /

END MODULE W3PARALL
!/ ------------------------------------------------------------------- /
