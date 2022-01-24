MODULE PDLIB_FIELD_VEC
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    01-Jan-2010 : Origination.                        ( version 6.04 )
!/    22-Mar-2021 : Add WNMEAN, TAUOC output            ( version 7.13 )
!/
!/    Copyright 2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose : Provides parallel I/O in context of PDLIB
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3XXXX    Subr. Public   ........
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
!  6. Switches :
!
!     !/S  Enable subroutine tracing.
!
!  7. Source code :
!/
!/ ------------------------------------------------------------------- /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
!
      SUBROUTINE GET_ARRAY_SIZE(TheSize)
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
!  1. Purpose : Estimate arrays size for communication 
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
      USE W3ODATMD, ONLY: FLOGRD, FLOGR2, NOSWLL, NOEXTR,              &
                          NOGRP, NGRPP
      USE W3GDATMD, ONLY: E3DF, P2MSF, NK
      IMPLICIT NONE
      INTEGER, INTENT(OUT)    :: TheSize
      LOGICAL                 :: FLGRDALL(NOGRP,NGRPP)
      INTEGER IH, I, J, K, IK
!/
!/ ------------------------------------------------------------------- /
!/
      DO J=1, NOGRP
        DO K=1, NGRPP
          FLGRDALL (J,K) =  (FLOGRD(J,K) .OR. FLOGR2(J,K))
        END DO
      END DO
      IH = 0
      IF ( FLGRDALL( 2, 1) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 2) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 3) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 4) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 5) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 6) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 7) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 8) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 9) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 10) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 11) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 12) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 13) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 14) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 15) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 16) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 17) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 2, 19) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 3, 1) ) THEN
        DO IK=E3DF(2,1),E3DF(3,1)
          IH = IH + 1
        END DO
      END IF
      IF ( FLGRDALL( 3, 2) ) THEN
        DO IK=E3DF(2,2),E3DF(3,2)
          IH = IH + 1
        END DO
      END IF
      IF ( FLGRDALL( 3, 3) ) THEN
        DO IK=E3DF(2,3),E3DF(3,3)
          IH = IH + 1
        END DO
      END IF
      IF ( FLGRDALL( 3, 4) ) THEN
        DO IK=E3DF(2,4),E3DF(3,4)
          IH = IH + 1
        END DO
      END IF
      IF ( FLGRDALL( 3, 5) ) THEN
        DO IK=E3DF(2,5),E3DF(3,5)
          IH = IH + 1
        END DO
      END IF
      IF ( FLGRDALL( 4, 1) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 2) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 3) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 4) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 5) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 6) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 7) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 8) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4, 9) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4,10) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4,11) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4,12) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4,13) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4,14) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4,15) ) THEN
        IH = IH + NOSWLL + 1
      END IF
      IF ( FLGRDALL( 4,16) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 4,17) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 5, 1) ) THEN
        IH = IH + 1
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 2) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 3) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 4) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 5) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 6) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 7) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 8) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5, 9) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 5,10) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 1) ) THEN
        IH = IH + 1
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 2) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 3) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 4) ) THEN
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 5) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 6) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 7) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 6, 8) ) THEN
        DO IK=1,2*NK
          IH = IH + 1
        END DO
      END IF
      IF ( FLGRDALL( 6, 9) ) THEN
        DO K=P2MSF(2),P2MSF(3)
          IH = IH + 1
        END DO
      END IF
      IF ( FLGRDALL( 6, 10) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 6, 11) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 6, 13) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 7, 1) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF 
      IF ( FLGRDALL( 7, 2) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 7, 3) ) THEN
        IH = IH + 1
        IH = IH + 1
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 7, 4) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 7, 5) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 8, 1) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 8, 2) ) THEN
        IH = IH + 1
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 8, 3) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 8, 4) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 8, 5) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 9, 1) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 9, 2) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 9, 3) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 9, 4) ) THEN
        IH = IH + 1
      END IF
      IF ( FLGRDALL( 9, 5) ) THEN
        IH = IH + 1
      END IF
      DO I=1, NOEXTR
        IF ( FLGRDALL(10, I) ) THEN
          IH = IH + 1
        END IF
      END DO
      TheSize=IH
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE UNST_PDLIB_READ_FROM_FILE(NDREAD)
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
!  1. Purpose : PDLIB read from file 
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

      use yowDatapool, only: istatus
      USE W3GDATMD, only : NSEA, NSPEC
      USE W3ODATMD, only : NAPROC, NTPROC, IAPROC
      USE W3ADATMD, only : MPI_COMM_WAVE
      USE W3PARALL, only : GET_JSEA_IBELONG
      USE W3WDATMD, ONLY : VA
      USE W3GDATMD, ONLY: NSEAL
      USE W3ADATMD, ONLY: NSEALM
      USE W3SERVMD, ONLY : EXTCDE
#ifdef W3_TIMINGS
      USE W3PARALL, ONLY: PRINT_MY_TIME
#endif
      use yowNodepool, only: ListNP, ListNPA, ListIPLG
      IMPLICIT NONE
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
!
      INTEGER, intent(in) :: NDREAD
      INTEGER iBlock, iFirst, iEnd, len, i, IB, iProc
      INTEGER NREC, ISEA, JSEA, ierr
      INTEGER nbBlock, IBELONG
      INTEGER :: BlockSize
      REAL, allocatable :: ArrSend(:,:)
      REAL, allocatable :: DataRead(:,:)
      integer(KIND=8) RPOS
      integer LRECL
      INTEGER, PARAMETER      :: LRB = 4
      INTEGER NBLKRSloc, RSBLKSloc
      integer eArr(1)
      integer IERR_MPI, istat
      integer IPloc, IPglob, pos
      integer NbMatch, idx
      integer ListFirst(NAPROC)
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
      !
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'UNST_PDLIB_READ, Beginning of function'
     FLUSH(740+IAPROC)
#endif
      LRECL  = MAX ( LRB*NSPEC ,                                      &
                     LRB*(6+(25/LRB)+(9/LRB)+(29/LRB)+(3/LRB)) )
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'UNST_PDLIB_READ, LRB=', LRB, ' LRECL=', LRECL
     FLUSH(740+IAPROC)
#endif
      IF (IAPROC .gt. NAPROC) THEN
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'Leaving bc rank IAPROC > NAPROC=', NAPROC
     FLUSH(740+IAPROC)
#endif
        RETURN
      END IF
      ListFirst(1)=0
      DO iProc=2,NAPROC
        ListFirst(iProc) = ListFirst(iProc-1) + ListNPA(iProc-1)
      END DO
      NBLKRSloc = 10
      RSBLKSloc = MAX ( 5 , NSEALM/NBLKRSloc )
      IF ( NBLKRSloc*RSBLKSloc .LT. NSEALM ) RSBLKSloc = RSBLKSloc + 1
      NBLKRSloc = 1 + (NSEALM-1)/RSBLKSloc
      BLOCKSIZE = INT(REAL(NSEA)/REAL(NBLKRSloc))
      !
      nbBlock=NSEA / BlockSize
      IF (nbBlock * BlockSize .lt. NSEA) THEN
        nbBlock=nbBlock+1
      END IF
      IF (IAPROC .eq. 1) THEN
        allocate(DATAread(NSPEC,BlockSize))
        DATAread = 0.
      END IF
      DO iBlock=1,nbBlock
        iFirst = 1 + (iBlock - 1)*BlockSize
        iEnd   = MIN(iBlock * BlockSize, NSEA)
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Beginning of iBlock value treatment")
#endif

#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'R : iBlock=', iBlock, '/', nbBlock, 'iFirst=', iFirst, 'iEnd=', iEnd
     FLUSH(740+IAPROC)
#endif
!    Let's try to get the indexes right.
!    We have 1 <= IB <= len = iEnd + 1 - iFirst
!    We have iFirst - 1 = (iBlock - 1)*BlockSize
!    and so  iFirst <= IB + (iBlock - 1)*BlockSize <= iEnd
!    and thus iFirst <= ISEA <= iEnd
        len=iEnd + 1 - iFirst
        IF (IAPROC .eq. 1) THEN
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Before data reading")
#endif
          DO IB=1,len
            ISEA = (iBlock - 1)*BlockSize + IB
            NREC = ISEA + 2
            RPOS = 1_8 + LRECL*(NREC-1_8)
!!/DEBUGIO     WRITE(740+IAPROC,*) 'READ AT ISEA=', ISEA, ' RPOS=', RPOS
!!/DEBUGIO     FLUSH(740+IAPROC)
            READ (NDREAD, POS=RPOS, IOSTAT=IERR) (DATAread(I,IB), I=1,NSPEC)
          END DO
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After data reading")
#endif
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'After the block of reads'
     WRITE(740+IAPROC,*) 'iBlock=', iBlock, '/', nbBlock, ' sum(DATAread)=', sum(DATAread)
     FLUSH(740+IAPROC)
#endif
          DO iProc=2,NAPROC
            NbMatch=0
            DO IPloc=1,ListNPA(iProc)
              IPglob = ListIPLG(ListFirst(iProc) + IPloc)
              IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
                NbMatch = NbMatch+1
              END IF
            END DO
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'Sending to iProc=', iProc, ' NbMatch=', NbMatch
     FLUSH(740+IAPROC)
#endif
            IF (NbMatch .gt. 0) THEN
              allocate(ArrSend(NSPEC,NbMatch), stat=istat)
              ArrSend = 0.
              idx=0
              DO IPloc=1,ListNPA(iProc)
                IPglob = ListIPLG(ListFirst(iProc) + IPloc)
                IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
                  pos = IPglob - iFirst + 1
                  idx = idx + 1
                  ArrSend(:,idx) = DATAread(:,pos)
                END IF
              END DO
              CALL MPI_SEND(ArrSend,NSPEC*NbMatch,MPI_REAL, iProc-1, 37, MPI_COMM_WAVE, ierr)
              deallocate(ArrSend)
            END IF
          END DO
          DO IPloc=1,ListNPA(1)
            IPglob = ListIPLG(IPloc)
            IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
              pos = IPglob - iFirst + 1
              VA(:,IPloc) = DATAread(:,pos)
            END IF
          END DO
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("After the sending")
#endif
        ELSE
          NbMatch=0
          DO IPloc=1,ListNPA(IAPROC)
            IPglob = ListIPLG(ListFirst(IAPROC) + IPloc)
            IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
              NbMatch = NbMatch+1
            END IF
          END DO
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'Receiving NbMatch=', NbMatch
     FLUSH(740+IAPROC)
#endif
          IF (NbMatch .gt. 0) THEN
            allocate(ArrSend(NSPEC,NbMatch), stat=istat)
            CALL MPI_RECV(ArrSend,NSPEC*NbMatch,MPI_REAL, 0, 37, MPI_COMM_WAVE, istatus, ierr)
            idx=0
            DO IPloc=1,ListNPA(IAPROC)
              IPglob = ListIPLG(ListFirst(IAPROC) + IPloc)
              IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
                idx = idx + 1
                VA(:,IPloc) = ArrSend(:,idx)
              END IF
            END DO
            deallocate(ArrSend)
          END IF
        END IF
#ifdef W3_TIMINGS
       CALL PRINT_MY_TIME("Beginning of iBlock value treatment")
#endif
      END DO
      IF (IAPROC .eq. 1) THEN
        deallocate(DATAread)
      END IF
#ifdef W3_DEBUGIO
     IF (IAPROC .le. NAPROC) THEN
       WRITE(740+IAPROC,*) 'iBlock=', iBlock, '/', nbBlock, ' sum(VA)=', sum(VA)
       FLUSH(740+IAPROC)
     END IF
     WRITE(740+IAPROC,*) 'Exiting READ_FROM_FILE'
     FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE UNST_PDLIB_WRITE_TO_FILE(NDWRITE)
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
!  1. Purpose : PDLIB write to file 
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
      use yowDatapool, only: istatus
      USE yowNodepool, only: ListNP, ListNPA, ListIPLG
      USE W3PARALL, ONLY: INIT_GET_ISEA
      USE W3GDATMD, only : NSEA, NSPEC
      USE W3ODATMD, only : NAPROC, NTPROC, NAPRST, IAPROC
      USE W3ADATMD, only : MPI_COMM_WAVE
      USE W3PARALL, only : GET_JSEA_IBELONG
      USE W3WDATMD, ONLY : VA
      USE W3GDATMD, ONLY: NSEAL, NX, NY
      IMPLICIT NONE
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
!
      INTEGER, intent(in) :: NDWRITE
      INTEGER, PARAMETER :: BlockSize = 100000
      REAL :: DATAwrite(NSPEC,BlockSize)
      REAL, allocatable :: DATArecv(:,:)
      integer ListFirst(NAPROC)
      integer idx, idxB
      integer len, i, IS
      integer iBlock, iFirst, iEnd
      integer IPglob, IPloc, pos, ISEA, nbBlock, NPAloc
      integer ierr, istat, JSEA, NREC, iProc
      integer NbMatch
      INTEGER, PARAMETER      :: LRB = 4
      INTEGER(KIND=8) RPOS
      INTEGER LRECL
      INTEGER IERR_MPI
      REAL(KIND=LRB) WRITEBUFF(NSPEC)
      REAL, allocatable :: DATAsend(:,:)
#ifdef W3_S
      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'Beginning of UNST_PDLIB_WRITE_TO_FILE IAPROC=', IAPROC, 'NAPRST=', NAPRST
      FLUSH(740+IAPROC)
      WRITE(740+IAPROC,*) 'sum(VA)=', sum(VA)
      FLUSH(740+IAPROC)
#endif
      ListFirst(1) = 0
      DO IPROC=2,NAPROC
        ListFirst(iProc)=ListFirst(iProc-1) + ListNPA(iProc-1)
      END DO
      !
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'NX=', NX, ' NY=', NY, ' NSEA=', NSEA
      WRITE(740+IAPROC,*) 'NAPROC=', NAPROC, ' NTPROC=', NTPROC
#endif
      LRECL  = MAX ( LRB*NSPEC ,                                      &
                     LRB*(6+(25/LRB)+(9/LRB)+(29/LRB)+(3/LRB)) )
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'UNST_PDLIB_WRITE, LRB=', LRB, ' LRECL=', LRECL
      WRITE(740+IAPROC,*) 'NDWRITE=', NDWRITE, 'NAPROC=', NAPROC, 'NTPROC=', NTPROC
      FLUSH(740+IAPROC)
#endif
      nbBlock=NSEA / BlockSize + 1
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'NSEA=', NSEA, ' BlockSize=', BlockSize
#endif
      DO iBlock=1,nbBlock
        iFirst= 1 + (iBlock - 1)*BlockSize
        iEnd= MIN(iBlock * BlockSize, NSEA)
        len=iEnd + 1 - iFirst
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'W : iBlock=', iBlock, '/', nbBlock, 'iFirst=', iFirst, 'iEnd=', iEnd, ' len=', len
      FLUSH(740+IAPROC)
#endif
        IF (IAPROC .eq. NAPRST) THEN
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'The Node is a restart writing node'
      FLUSH(740+IAPROC)
#endif
          IF (IAPROC .le. NAPROC) THEN
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'It is also a running node'
      FLUSH(740+IAPROC)
#endif
            DO JSEA=1,NSEAL
              CALL INIT_GET_ISEA(ISEA, JSEA)
              IF ((iFirst .le. ISEA).and.(ISEA .le. iEnd)) THEN
                idx = ISEA - iFirst + 1
                DATAwrite(:, idx) = VA(:, JSEA)
              END IF
            END DO
          END IF
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'Now iterating over all the nodes for RECV'
      FLUSH(740+IAPROC)
#endif
          DO iProc=1,NAPROC
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'iProc=', iProc, ' / ', NAPROC
      FLUSH(740+IAPROC)
#endif
            IF (iProc .ne. IAPROC) THEN
              NPAloc=ListNPA(iProc)
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'We found NPAloc=', NPAloc
     FLUSH(740+IAPROC)
#endif
              NbMatch=0
              DO IPloc=1,NPAloc
                IPglob = ListIPLG(ListFirst(iProc) + IPloc)
                IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
                  NbMatch=NbMatch+1
                END IF
              END DO
              IF (NbMatch .gt. 0) THEN
                allocate(DATArecv(NSPEC, NbMatch), stat=istat)
#ifdef W3_DEBUGIO
       WRITE(740+IAPROC,*) 'After allocation and before reception, istat=', istat
       FLUSH(740+IAPROC)
#endif
                CALL MPI_RECV(DATArecv,NSPEC*NbMatch,MPI_REAL, iProc-1, 101, MPI_COMM_WAVE, istatus, ierr)
#ifdef W3_DEBUGIO
       WRITE(740+IAPROC,*) 'After reception, ierr=', ierr
       FLUSH(740+IAPROC)
#endif
                idx=0
                DO IPloc=1,NPAloc
                  IPglob = ListIPLG(IPloc + ListFirst(iProc))
                  ISEA = IPglob ! Great ansatz here. False in general
                  IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
                    idx=idx+1
                    pos = IPglob - iFirst + 1
                    DATAwrite(:, pos) = DATArecv(:, idx)
                  END IF
                END DO
#ifdef W3_DEBUGIO
       WRITE(740+IAPROC,*) 'After assignation'
       FLUSH(740+IAPROC)
#endif
                deallocate(DATArecv, stat=istat)
#ifdef W3_DEBUGIO
       WRITE(740+IAPROC,*) 'After assignation istat=', istat
       FLUSH(740+IAPROC)
#endif
              END IF
            END IF
          END DO
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'Before the actual write down'
     WRITE(740+IAPROC,*) 'iBlock=', iBlock, '/', nbBlock, 'Sum DATAwrite=', sum(DATAwrite)
     FLUSH(740+IAPROC)
#endif
          DO ISEA=iFirst,iEnd
            idx  = ISEA - iFirst + 1
            NREC = ISEA + 2
            RPOS = 1_8 + LRECL*(NREC-1_8)
!!/DEBUGIO     WRITE(740+IAPROC,*) 'WRITE AT ISEA=', ISEA, ' RPOS=', RPOS
!!/DEBUGIO     FLUSH(740+IAPROC)
            WRITEBUFF(:) = 0
            WRITEBUFF(1:NSPEC) = DATAwrite(1:NSPEC, idx)
            WRITE(NDWRITE, POS=RPOS) WRITEBUFF
          END DO
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'After the write down'
     FLUSH(740+IAPROC)
#endif
        ELSE
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'We are a node different from NAPRST'
     FLUSH(740+IAPROC)
#endif
          IF (IAPROC .le. NAPROC) THEN
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'We are a computing node'
     FLUSH(740+IAPROC)
#endif
            NbMatch=0
            DO IPloc=1,ListNPA(IAPROC)
              IPglob = ListIPLG(ListFirst(IAPROC) + IPloc)
              IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
                NbMatch=NbMatch+1
              END IF
            END DO
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'NbMatch=', NbMatch
     FLUSH(740+IAPROC)
#endif
            IF (NbMatch .gt. 0) THEN
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'We are actually a computing node so we have something to send'
     WRITE(740+IAPROC,*) 'Sending message of length NSEAL=', NSEAL
     FLUSH(740+IAPROC)
#endif
              allocate(DATAsend(NSPEC,NbMatch), stat=istat)
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'After allocation of DATAsend, istat=', istat
     FLUSH(740+IAPROC)
#endif
              idx=0
              DO IPloc=1,ListNPA(IAPROC)
                IPglob = ListIPLG(ListFirst(IAPROC) + IPloc)
                IF ((iFirst .le. IPglob).and.(IPglob .le. iEnd)) THEN
                  idx=idx + 1
                  DATAsend(:,idx)=VA(:,IPloc)
                END IF
              END DO
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'After assignation of DATAsend'
     FLUSH(740+IAPROC)
#endif
              CALL MPI_SEND(DATAsend,NSPEC*NbMatch,MPI_REAL, NAPRST-1, 101, MPI_COMM_WAVE, ierr)
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'After sending of DATAsend, ierr=', ierr
     FLUSH(740+IAPROC)
#endif
              deallocate(DATAsend, stat=istat)
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'After deallocation of DATAsend, istat=', istat
     FLUSH(740+IAPROC)
#endif
            END IF
          END IF
#ifdef W3_DEBUGIO
     WRITE(740+IAPROC,*) 'After the IAPROC test'
     FLUSH(740+IAPROC)
#endif
        END IF
      END DO
!!/DEBUGIO     WRITE(740+IAPROC,*) 'Before the MPI_BARRIER'
!!/DEBUGIO     FLUSH(740+IAPROC)
!      CALL MPI_BARRIER(MPI_COMM_WAVE, IERR_MPI)
#ifdef W3_DEBUGIO
      WRITE(740+IAPROC,*) 'Exiting the UNST_PDLIB_WRITE_TO_FILE'
      FLUSH(740+IAPROC)
#endif
      END SUBROUTINE
!/ ------------------------------------------------------------------- /
      SUBROUTINE DO_OUTPUT_EXCHANGES(IMOD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/    22-Mar-2021 : Add WNMEAN, TAUOC output            ( version 7.13 )
!/
!  1. Purpose : Do communication for PDLIB output
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
      USE W3ADATMD, ONLY: W3XDMA, W3SETA, W3XETA
      USE W3SERVMD, ONLY: EXTCDE
      USE W3GDATMD, ONLY: NSEA
      USE W3GDATMD, ONLY: NX, NSPEC, MAPFS, E3DF, P2MSF, US3DF
      USE W3WDATMD, ONLY: VA, UST, USTDIR, ASF, FPIS
      USE W3ADATMD, ONLY: MPI_COMM_WAVE, WW3_FIELD_VEC
      USE W3ADATMD, ONLY: HS, WLM, T02
      USE W3ADATMD, ONLY: T0M1, THM, THS, FP0, THP0, FP1, THP1,  &
                          DTDYN, FCUT, SPPNT, ABA, ABD, UBA, UBD,&
                          SXX, SYY, SXY, USERO, PHS, PTP, PLP,   &
                          PDIR, PSI, PWS, PWST, PNR, PHIAW,      &
                          PHIOC, TAUOCX, TAUOCY, WNMEAN,         &
                          TUSX, TUSY, TAUWIX, TAUWIY, TAUOX,     &
                          TAUOY, USSX, USSY, MSSX, MSSY,         &
                          MSCX, MSCY, PRMS, TPMS, CHARN,         &
                          TAUWNX, TAUWNY, BHD, CGE,              &
                          CFLXYMAX, CFLTHMAX, CFLKMAX, WHITECAP, &
                          BEDFORMS, PHIBBL, TAUBBL, T01,         &
                          P2SMS, US3D, EF,  TH1M, STH1M, TH2M,   &
                          STH2M, HSIG, TAUICE, PHICE, PTHP0, PQP,&
                          PPE, PGW, PSW, PTM1, PT1, PT2, PEP,   &
                          QP, MSSD, MSCD, STMAXE, STMAXD, HMAXE, &
                          HCMAXE, HMAXD, HCMAXD, WBT
      USE W3GDATMD, ONLY: NK, NSEAL
      USE W3ODATMD, ONLY: NDST, IAPROC, NAPROC, NTPROC, FLOUT,   &
                          NAPFLD, NAPPNT, NAPRST, NAPBPT, NAPTRK,&
                          NOGRP, NGRPP
      USE W3ODATMD, ONLY: OUTPTS, NRQGO, NRQGO2, IRQGO, IRQGO2,  &
                          FLOGRD, NRQPO, NRQPO2, IRQPO1, IRQPO2, &
                          NOPTS, IPTINT, NRQRS, IRQRS, NBLKRS,   &
                          RSBLKS, IRQRSS, VAAUX, NRQBP, NRQBP2,  &
                          IRQBP1, IRQBP2, NFBPO, NBO2, ISBPO,    &
                          ABPOS, NRQTR, IRQTR, IT0PNT, IT0TRK,   &
                          IT0PRT, NOSWLL, NOEXTR, NDSE, IOSTYP,  &
                          FLOGR2
      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      USE W3PARALL, ONLY: INIT_GET_JSEA_ISPROC
      USE W3PARALL, ONLY: INIT_GET_ISEA
      use yowDatapool, only: istatus
!/
      IMPLICIT NONE
!
      INCLUDE "mpif.h"
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IK, IFJ
      INTEGER                 :: IH, IT0, IROOT, IT, IERR, I0,   &
                                 IFROM, IX(4), IY(4), IS(4),     &
                                 IP(4), I, J, JSEA, ITARG, IB,   &
                                 JSEA0, JSEAN, NSEAB, IBOFF,     &
                                 ISEA, ISPROC, K, NRQMAX
#ifdef W3_S
      INTEGER, SAVE           :: IENT
#endif
      LOGICAL                 :: FLGRDALL(NOGRP,NGRPP)
      REAL, allocatable       :: ARRexch(:,:), ARRexch_loc(:,:)
      REAL, allocatable       :: ARRtotal(:,:)
      INTEGER, allocatable    :: ARRpos(:), ARRpos_loc(:)
      INTEGER                 :: eEnt(1), IPROC
      INTEGER                 :: TheSize, NSEAL_loc
      INTEGER, SAVE           :: indexOutput
#ifdef W3_DEBUGOUTPUT
      WRITE(740+IAPROC,*) 'Beginning of output, indexOutput=', indexOutput
      WRITE(740+IAPROC,*) 'NAPROC=', NAPROC, ' NAPFLD=', NAPFLD
      FLUSH(740+IAPROC)
#endif
!/
!/ ------------------------------------------------------------------- /
!/
      DO J=1, NOGRP
        DO K=1, NGRPP
          FLGRDALL (J,K) =  (FLOGRD(J,K) .OR. FLOGR2(J,K))
        END DO
      END DO
      NRQGO  = 0
      NRQGO2 = 0
      IT0    = NSPEC
      IROOT  = NAPFLD - 1
#ifdef W3_DEBUGOUTPUT
      WRITE(740+IAPROC,*) 'Entering DO_OUTPUT_EXCHANGES'
      FLUSH(740+IAPROC)
#endif
      IF ( FLOUT(1) .OR. FLOUT(7) ) THEN
        CALL GET_ARRAY_SIZE(TheSize)
        IF ( IAPROC .LE. NAPROC ) THEN
#ifdef W3_DEBUGOUTPUT
          WRITE(740+IAPROC,*) 'Allocating and filling'
          FLUSH(740+IAPROC)
#endif
          allocate(ARRexch(TheSize, NSEAL), ARRpos(NSEAL))
          DO JSEA=1,NSEAL
            CALL INIT_GET_ISEA(ISEA, JSEA)
            ARRpos(JSEA)=ISEA
            IH     = 0
            IF ( FLGRDALL( 2, 1) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=HS(JSEA)
            END IF
            IF ( FLGRDALL( 2, 2) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=WLM(JSEA)
            END IF
            IF ( FLGRDALL( 2, 3) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=T02(JSEA)
            END IF
            IF ( FLGRDALL( 2, 4) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=T0M1(JSEA)
            END IF
            IF ( FLGRDALL( 2, 5) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=T01(JSEA)
            END IF
            IF ( FLGRDALL( 2, 6) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=FP0(JSEA)
            END IF
            IF ( FLGRDALL( 2, 7) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=THM(JSEA)
            END IF
            IF ( FLGRDALL( 2, 8) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=THS(JSEA)
            END IF
            IF ( FLGRDALL( 2, 9) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=THP0(JSEA)
            END IF
            IF ( FLGRDALL( 2, 10) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=HSIG(JSEA)
            END IF
            IF ( FLGRDALL( 2, 11) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=STMAXE(JSEA)
            END IF
            IF ( FLGRDALL( 2, 12) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=STMAXD(JSEA)
            END IF
            IF ( FLGRDALL( 2, 13) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=HMAXE(JSEA)
            END IF
            IF ( FLGRDALL( 2, 14) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=HCMAXE(JSEA)
            END IF
            IF ( FLGRDALL( 2, 15) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=HMAXD(JSEA)
            END IF
            IF ( FLGRDALL( 2, 16) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=HCMAXD(JSEA)
            END IF
            IF ( FLGRDALL( 2, 17) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=WBT(JSEA)
            END IF
            IF ( FLGRDALL( 2, 19) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=WNMEAN(JSEA)
            END IF
            IF ( FLGRDALL( 3, 1) ) THEN 
              DO IK=E3DF(2,1),E3DF(3,1)
                IH = IH + 1
                Arrexch(IH,JSEA)=EF(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 3, 2) ) THEN 
              DO IK=E3DF(2,2),E3DF(3,2)
                IH = IH + 1
                Arrexch(IH,JSEA)=TH1M(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 3, 3) ) THEN 
              DO IK=E3DF(2,3),E3DF(3,3)
                IH = IH + 1
                Arrexch(IH,JSEA)=STH1M(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 3, 4) ) THEN 
              DO IK=E3DF(2,4),E3DF(3,4)
                IH = IH + 1
                Arrexch(IH,JSEA)=TH2M(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 3, 5) ) THEN 
              DO IK=E3DF(2,5),E3DF(3,5)
                IH = IH + 1
                Arrexch(IH,JSEA)=STH2M(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 1) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PHS(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 2) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PTP(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 3) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PLP(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 4) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PDIR(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 5) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PSI(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 6) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PWS(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 7) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PTHP0(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 8) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PQP(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4, 9) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PPE(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4,10) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PGW(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4,11) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PSW(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4,12) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PTM1(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4,13) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PT1(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4,14) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PT2(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4,15) ) THEN
              DO IK=0, NOSWLL
                IH = IH + 1
                Arrexch(IH,JSEA)=PEP(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 4,16) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=PWST(JSEA)
            END IF
            IF ( FLGRDALL( 4,17) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=PNR(JSEA)
            END IF
            IF ( FLGRDALL( 5, 1) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=UST(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=USTDIR(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=ASF(JSEA)
            END IF 
            IF ( FLGRDALL( 5, 2) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=CHARN(JSEA)
            END IF
            IF ( FLGRDALL( 5, 3) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=CGE(JSEA)
            END IF
            IF ( FLGRDALL( 5, 4) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=PHIAW(JSEA)
            END IF
            IF ( FLGRDALL( 5, 5) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUWIX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUWIY(JSEA)
            END IF
            IF ( FLGRDALL( 5, 6) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUWNX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUWNY(JSEA)
            END IF
            IF ( FLGRDALL( 5, 7) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=WHITECAP(JSEA,1)
            END IF
            IF ( FLGRDALL( 5, 8) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=WHITECAP(JSEA,2)
            END IF
            IF ( FLGRDALL( 5, 9) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=WHITECAP(JSEA,3)
            END IF
            IF ( FLGRDALL( 5,10) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=WHITECAP(JSEA,4)
            END IF
            IF ( FLGRDALL( 6, 1) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=SXX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=SYY(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=SXY(JSEA)
            END IF
            IF ( FLGRDALL( 6, 2) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUOX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUOY(JSEA)
            END IF
            IF ( FLGRDALL( 6, 3) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=BHD(JSEA)
            END IF
            IF ( FLGRDALL( 6, 4) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=PHIOC(JSEA)
            END IF
            IF ( FLGRDALL( 6, 5) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=TUSX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=TUSY(JSEA)
            END IF
            IF ( FLGRDALL( 6, 6) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=USSX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=USSY(JSEA)
            END IF
            IF ( FLGRDALL( 6, 7) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=PRMS(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=TPMS(JSEA)
            END IF
            IF ( FLGRDALL( 6, 8) ) THEN
              DO IK=1,2*NK
                IH = IH + 1
                Arrexch(IH,JSEA)=US3D(JSEA,IK)
              END DO
            END IF
            IF ( FLGRDALL( 6, 9) ) THEN
              DO K=P2MSF(2),P2MSF(3)
                IH = IH + 1
                Arrexch(IH,JSEA)=P2SMS(JSEA,K)
              END DO
            END IF
            IF ( FLGRDALL( 6, 10) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUICE(JSEA,1)
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUICE(JSEA,2)
            END IF
            IF ( FLGRDALL( 6, 11) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=PHICE(JSEA)
            END IF
            IF ( FLGRDALL( 6, 13) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUOCX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUOCY(JSEA)
            END IF
            IF ( FLGRDALL( 7, 1) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=ABA(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=ABD(JSEA)
            END IF
            IF ( FLGRDALL( 7, 2) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=UBA(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=UBD(JSEA)
            END IF
            IF ( FLGRDALL( 7, 3) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=BEDFORMS(JSEA,1)
              IH = IH + 1
              Arrexch(IH,JSEA)=BEDFORMS(JSEA,2)
              IH = IH + 1
              Arrexch(IH,JSEA)=BEDFORMS(JSEA,3)
            END IF
            IF ( FLGRDALL( 7, 4) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=PHIBBL(JSEA)
            END IF
            IF ( FLGRDALL( 7, 5) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUBBL(JSEA,1)
              IH = IH + 1
              Arrexch(IH,JSEA)=TAUBBL(JSEA,2)
            END IF
            IF ( FLGRDALL( 8, 1) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=MSSX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=MSSY(JSEA)
            END IF
            IF ( FLGRDALL( 8, 2) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=MSCX(JSEA)
              IH = IH + 1
              Arrexch(IH,JSEA)=MSCY(JSEA)
            END IF
            IF ( FLGRDALL( 8, 3) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=MSSD(JSEA)
            END IF
            IF ( FLGRDALL( 8, 4) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=MSCD(JSEA)
            END IF
            IF ( FLGRDALL( 8, 5) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=QP(JSEA)
            END IF
            IF ( FLGRDALL( 9, 1) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=DTDYN(JSEA)
            END IF
            IF ( FLGRDALL( 9, 2) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=FCUT(JSEA)
            END IF
            IF ( FLGRDALL( 9, 3) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=CFLXYMAX(JSEA)
            END IF
            IF ( FLGRDALL( 9, 4) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=CFLTHMAX(JSEA)
            END IF
            IF ( FLGRDALL( 9, 5) ) THEN
              IH = IH + 1
              Arrexch(IH,JSEA)=CFLKMAX(JSEA)
            END IF
            DO I=1, NOEXTR
              IF ( FLGRDALL(10, I) ) THEN
                IH = IH + 1
                Arrexch(IH,JSEA)=USERO(JSEA,I)
              END IF
            END DO
          END DO
        END IF
#ifdef W3_DEBUGOUTPUT
        WRITE(740+IAPROC,*) 'Before assigning field values'
        FLUSH(740+IAPROC)
#endif
!
!  Now synchronizing the data
!  It must be possible to ensure that the output
!  node is also a computational node.
!
        IF (IAPROC .eq. NAPFLD) THEN
          allocate(ARRtotal(TheSize, NSEA))
          IF (IAPROC .le. NAPROC) THEN
            DO I=1,NSEAL
              ARRtotal(:,ARRpos(I)) = ARRexch(:,I)
            END DO
          END IF
        END IF
#ifdef W3_DEBUGOUTPUT
        WRITE(740+IAPROC,*) 'Before ARRexch operations'
        FLUSH(740+IAPROC)
#endif
        IF ((IAPROC .le. NAPROC).and.(IAPROC.ne.NAPFLD)) THEN
#ifdef W3_DEBUGOUTPUT
            WRITE(740+IAPROC,*) 'Case 1'
            WRITE(740+IAPROC,*) 'NSEAL=', NSEAL
            WRITE(740+IAPROC,*) 'IAPROC=', IAPROC, ' NAPFLD=', NAPFLD
            FLUSH(740+IAPROC)
#endif
          eEnt(1)=NSEAL
          CALL MPI_SEND(eEnt,1,MPI_INTEGER, NAPFLD-1, 23, MPI_COMM_WAVE, ierr)
#ifdef W3_DEBUGOUTPUT
            WRITE(740+IAPROC,*) 'After MPI_SEND 1'
            FLUSH(740+IAPROC)
#endif
          CALL MPI_SEND(ARRpos,NSEAL,MPI_INTEGER, NAPFLD-1, 29, MPI_COMM_WAVE, ierr)
#ifdef W3_DEBUGOUTPUT
            WRITE(740+IAPROC,*) 'After MPI_SEND 2'
            FLUSH(740+IAPROC)
#endif
          CALL MPI_SEND(ARRexch,NSEAL*TheSize,MPI_REAL, NAPFLD-1, 37, MPI_COMM_WAVE, ierr)
#ifdef W3_DEBUGOUTPUT
            WRITE(740+IAPROC,*) 'After MPI_SEND 3'
            FLUSH(740+IAPROC)
#endif
          deallocate(ARRpos, ARRexch)
        END IF
#ifdef W3_DEBUGOUTPUT
            WRITE(740+IAPROC,*) 'Case 2'
            FLUSH(740+IAPROC)
#endif
        IF (IAPROC .eq. NAPFLD) THEN
#ifdef W3_DEBUGOUTPUT
              WRITE(740+IAPROC,*) 'Case 2a'
              FLUSH(740+IAPROC)
#endif
            DO IPROC=1,NAPROC
              IF (IPROC .ne. IAPROC) THEN
#ifdef W3_DEBUGOUTPUT
                WRITE(740+IAPROC,*) 'IPROC=', IPROC
                FLUSH(740+IAPROC)
#endif
                CALL MPI_RECV(eEnt,1,MPI_INTEGER, IPROC-1, 23, MPI_COMM_WAVE, istatus, ierr)
#ifdef W3_DEBUGOUTPUT
                WRITE(740+IAPROC,*) 'After MPI_RECV 1'
                FLUSH(740+IAPROC)
#endif
                NSEAL_loc=eEnt(1)
#ifdef W3_DEBUGOUTPUT
                WRITE(740+IAPROC,*) 'NSEAL_loc=', NSEAL_loc
                FLUSH(740+IAPROC)
#endif
                allocate(ARRpos_loc(NSEAL_loc), ARRexch_loc(TheSize, NSEAL_loc))
                CALL MPI_RECV(ARRpos_loc,NSEAL_loc,MPI_INTEGER, IPROC-1, 29, MPI_COMM_WAVE, istatus, ierr)
#ifdef W3_DEBUGOUTPUT
                WRITE(740+IAPROC,*) 'After MPI_RECV 2'
                FLUSH(740+IAPROC)
#endif
                CALL MPI_RECV(ARRexch_loc,NSEAL_loc*TheSize,MPI_INTEGER, IPROC-1, 37, MPI_COMM_WAVE, istatus, ierr)
#ifdef W3_DEBUGOUTPUT
                WRITE(740+IAPROC,*) 'After MPI_RECV 3'
                FLUSH(740+IAPROC)
#endif
                DO I=1,NSEAL_loc
                  ARRtotal(:,ARRpos_loc(I)) = ARRexch_loc(:,I)
                END DO
                deallocate(ARRexch_loc, ARRpos_loc)
              END IF
            END DO
        END IF
#ifdef W3_DEBUGOUTPUT
        WRITE(740+IAPROC,*) 'After ARRexch operations'
        FLUSH(740+IAPROC)
        WRITE(740+IAPROC,*) 'NAPFLD=', NAPFLD
        FLUSH(740+IAPROC)
#endif
        IF ( IAPROC .EQ. NAPFLD ) THEN
!              CALL W3XDMA ( IMOD, NDSE, NDST, FLGRDALL )
#ifdef W3_DEBUGOUTPUT
        WRITE(740+IAPROC,*) 'Call W3XETA from DO_OUTPUT_EXCHANGES'
        FLUSH(740+IAPROC)
#endif
              CALL W3XETA ( IMOD, NDSE, NDST )
              IH     = 0
              IF ( FLGRDALL( 2, 1) ) THEN
                IH = IH + 1
                HS(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 2) ) THEN
                IH = IH + 1
                WLM(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 3) ) THEN
                IH = IH + 1
                T02(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 4) ) THEN
                IH = IH + 1
                T0M1(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 5) ) THEN
                IH = IH + 1
                T01(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 6) ) THEN
                IH = IH + 1
                FP0(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 7) ) THEN
                IH = IH + 1
                THM(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 8) ) THEN
                IH = IH + 1
                THS(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 9) ) THEN
                IH = IH + 1
                THP0(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 10) ) THEN
                IH = IH + 1
                HSIG(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 11) ) THEN
                IH = IH + 1
                STMAXE(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 12) ) THEN
                IH = IH + 1
                STMAXD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 13) ) THEN
                IH = IH + 1
                HMAXE(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 14) ) THEN
                IH = IH + 1
                HCMAXE(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 15) ) THEN
                IH = IH + 1
                HMAXD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 16) ) THEN
                IH = IH + 1
                HCMAXD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 17) ) THEN
                IH = IH + 1
                WBT(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 2, 19) ) THEN
                IH = IH + 1
                WNMEAN(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 3, 1) ) THEN 
                DO IK=E3DF(2,1),E3DF(3,1)
                  IH = IH + 1
                  EF(1:NSEA,IK) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 3, 2) ) THEN 
                DO IK=E3DF(2,2),E3DF(3,2)
                  IH = IH + 1
                  TH1M(1:NSEA,IK) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 3, 3) ) THEN
                DO IK=E3DF(2,3),E3DF(3,3)
                  IH = IH + 1
                  STH1M(1:NSEA,IK) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 3, 4) ) THEN
                DO IK=E3DF(2,4),E3DF(3,4)
                  IH = IH + 1
                  TH2M(1:NSEA,IK) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 3, 5) ) THEN
                DO IK=E3DF(2,5),E3DF(3,5)
                  IH = IH + 1
                  STH2M(1:NSEA,IK) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 1) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PHS(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 2) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PTP(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 3) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PLP(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 4) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PDIR(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 5) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PSI(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 6) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PWS(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 7) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PTHP0(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 8) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PQP(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4, 9) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PPE(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4,10) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PGW(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4,11) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PSW(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4,12) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PTM1(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4,13) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PT1(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4,14) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PT2(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4,15) ) THEN
                DO K=0, NOSWLL
                  IH = IH + 1
                  PEP(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF ( FLGRDALL( 4,16) ) THEN
                IH = IH + 1
                PWST(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 4,17) ) THEN
                IH = IH + 1
                PNR(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 1) ) THEN
                IH = IH + 1
                UST(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                USTDIR(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                ASF(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 2) ) THEN
                IH = IH + 1
                CHARN(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 3) ) THEN
                IH = IH + 1
                CGE(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 4) ) THEN
                IH = IH + 1
                PHIAW(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 5) ) THEN
                IH = IH + 1
                TAUWIX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                TAUWIY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 6) ) THEN
                IH = IH + 1
                TAUWNX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                TAUWNY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 7) ) THEN
                IH = IH + 1
                WHITECAP(1:NSEA,1) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 8) ) THEN
                IH = IH + 1
                WHITECAP(1:NSEA,2) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5, 9) ) THEN
                IH = IH + 1
                WHITECAP(1:NSEA,3) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 5,10) ) THEN
                IH = IH + 1
                WHITECAP(1:NSEA,4) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 1) ) THEN
                IH = IH + 1
                SXX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                SYY(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                SXY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 2) ) THEN
                IH = IH + 1
                TAUOX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                TAUOY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 3) ) THEN
                IH = IH + 1
                BHD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 4) ) THEN
                IH = IH + 1
                PHIOC(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 5) ) THEN
                IH = IH + 1
                TUSX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                TUSY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 6) ) THEN
                IH = IH + 1
                USSX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                USSY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 7) ) THEN
                IH = IH + 1
                PRMS(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                TPMS(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 8) ) THEN
                DO IK=1,2*NK
                  IH = IH + 1
                  US3D(1:NSEA,IK) = ARRtotal(IH,:)
                END DO
              END IF
              IF (  FLGRDALL( 6, 9) ) THEN
                DO K=P2MSF(2),P2MSF(3)
                  IH = IH + 1
                  P2SMS(1:NSEA,K) = ARRtotal(IH,:)
                END DO
              END IF
              IF (  FLGRDALL( 6, 10) ) THEN
                IH = IH + 1
                TAUICE(1:NSEA,1) = ARRtotal(IH,:)
                IH = IH + 1
                TAUICE(1:NSEA,2) = ARRtotal(IH,:)
              END IF
              IF (  FLGRDALL( 6, 11) ) THEN
                IH = IH + 1
                PHICE(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 6, 13) ) THEN
                IH = IH + 1
                TAUOCX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                TAUOCY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 7, 1) ) THEN
                IH = IH + 1
                ABA(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                ABD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 7, 2) ) THEN
                IH = IH + 1
                UBA(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                UBD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 7, 3) ) THEN
                IH = IH + 1
                BEDFORMS(1:NSEA,1) = ARRtotal(IH,:)
                IH = IH + 1
                BEDFORMS(1:NSEA,2) = ARRtotal(IH,:)
                IH = IH + 1
                BEDFORMS(1:NSEA,3) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 7, 4) ) THEN
                IH = IH + 1
                PHIBBL(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 7, 5) ) THEN
                IH = IH + 1
                TAUBBL(1:NSEA,1) = ARRtotal(IH,:)
                IH = IH + 1
                TAUBBL(1:NSEA,2) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 8, 1) ) THEN
                IH = IH + 1
                MSSX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                MSSY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 8, 2) ) THEN
                IH = IH + 1
                MSCX(1:NSEA) = ARRtotal(IH,:)
                IH = IH + 1
                MSCY(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 8, 3) ) THEN
                IH = IH + 1
                MSSD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 8, 4) ) THEN
                IH = IH + 1
                MSCD(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 8, 5) ) THEN
                IH = IH + 1
                QP(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 9, 1) ) THEN
                IH = IH + 1
                DTDYN(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 9, 2) ) THEN
                IH = IH + 1
                FCUT(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 9, 3) ) THEN
                IH = IH + 1
                CFLXYMAX(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 9, 4) ) THEN
                IH = IH + 1
                CFLTHMAX(1:NSEA) = ARRtotal(IH,:)
              END IF
              IF ( FLGRDALL( 9, 5) ) THEN
                IH = IH + 1
                CFLKMAX(1:NSEA) = ARRtotal(IH,:)
              END IF
              DO I=1, NOEXTR
                IF ( FLGRDALL(10, I) ) THEN
                  IH = IH + 1
                  USERO(1:NSEA,I) = ARRtotal(IH,:)
                END IF
              END DO
              CALL W3SETA ( IMOD, NDSE, NDST )
        END IF
#ifdef W3_DEBUGOUTPUT
        WRITE(740+IAPROC,*) 'After IAPROC = NAPFLD test'
        FLUSH(740+IAPROC)
#endif
      END IF
#ifdef W3_DEBUGOUTPUT
        WRITE(740+IAPROC,*) 'Ending of output, indexOutput=', indexOutput
        FLUSH(740+IAPROC)
#endif
      indexOutput=indexOutput+1
      END SUBROUTINE DO_OUTPUT_EXCHANGES
!/ ------------------------------------------------------------------- /
END MODULE PDLIB_FIELD_VEC
!/ ------------------------------------------------------------------- /

