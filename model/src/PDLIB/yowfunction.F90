!PDLIB Software License
!
!Software, as understood herein, shall be broadly interpreted as being inclusive of algorithms,
!source code, object code, data bases and related documentation, all of which shall be furnished
!free of charge to the Licensee. Corrections, upgrades or enhancements may be furnished and, if
!furnished, shall also be furnished to the Licensee without charge. NOAA, however, is not
!required to develop or furnish such corrections, upgrades or enhancements.
!Roland & Partner software, whether that initially furnished or corrections or upgrades,
!are furnished "as is". Roland & Partner furnishes its software without any warranty
!whatsoever and is not responsible for any direct, indirect or consequential damages
!that may be incurred by the Licensee. Warranties of merchantability, fitness for any
!particular purpose, title, and non-infringement, are specifically negated.
!The Licensee is not required to develop any software related to the licensed software.
!However, in the event that the Licensee does so, the Licensee is required to offer same
!to Roland & Partner for inclusion under the instant licensing terms with Roland & Partner
!licensed software along with documentation regarding its principles, use and its advantages.
!This includes changes to the wave model proper including numerical and physical approaches
!to wave modeling, and boundary layer parameterizations embedded in the wave model
!A Licensee may reproduce sufficient software to satisfy its needs.
!All copies shall bear the name of the software with any version number
!as well as replicas of any applied copyright notice, trademark notice,
!other notices and credit lines. Additionally, if the copies have been modified,
!e.g. with deletions or additions, this shall be so stated and identified.
!All of Licensee's employees who have a need to use the software may have access
!to the software but only after reading the instant license and stating, in writing,
!that they have read and understood the license and have agreed to its terms.
!Licensee is responsible for employing reasonable efforts to assure
!that only those of its employees that should have access to the software, in fact, have access.
!The Licensee may use the software for any purpose relating to sea state prediction.
!No disclosure of any portion of the software, whether by means of a media or verbally,
!may be made to any third party by the Licensee or the Licensee's employees
!The Licensee is responsible for compliance with any applicable export or
!import control laws of the United States, the European Union and Germany.
!
!© 2009 Roland&Partner, Georgenstr.32, 64297 Germany. All rights reserved.
!PDLIB is a trademark of Roland & Partner. No unauthorized use without permission.
!
!> \file yowpdlibmain.F90
!> \brief initialization
!> \author Thomas Huxhorn
!> \date 2011-2012
module yowfunction
  CONTAINS
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE PDLIB_ABORT(istat)
      IMPLICIT NONE
      integer, intent(in) :: istat
      Print *, 'Error with istat=', istat
      CALL ABORT
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
  SUBROUTINE ComputeListNP_ListNPA_ListIPLG_Kernel
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    USE W3ADATMD, ONLY: MPI_COMM_WCMP
    USE yowDatapool, only: rtype, istatus
    USE yowNodepool, only: npa, np, iplg
    USE yowNodepool, only: ListNP, ListNPA, ListIPLG
    IMPLICIT NONE
    INCLUDE "mpif.h"
    integer IPROC, idx, IP, len, istat, sumNP, ierr
    integer, allocatable :: iVect(:)
    !
    ! Computing ListNP and ListNPA
    !
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 1'
     FLUSH(740+IAPROC)
#endif
    allocate(ListNP(NAPROC), ListNPA(NAPROC), iVect(2), stat=istat)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 2'
     FLUSH(740+IAPROC)
#endif
    IF (istat /= 0) CALL PDLIB_ABORT(1)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 3'
     FLUSH(740+IAPROC)
#endif
    IF (IAPROC .eq. 1) THEN
      ListNP(1)=np
      ListNPA(1)=npa
      DO IPROC=2,NAPROC
        CALL MPI_RECV(iVect,2,MPI_INTEGER, iProc-1, 19, MPI_COMM_WCMP, istatus, ierr)
        ListNP(IPROC)=iVect(1)
        ListNPA(IPROC)=iVect(2)
      END DO
      DO IPROC=2,NAPROC
        CALL MPI_SEND(ListNP, NAPROC,MPI_INTEGER, iProc-1, 20, MPI_COMM_WCMP, ierr)
        CALL MPI_SEND(ListNPA,NAPROC,MPI_INTEGER, iProc-1, 21, MPI_COMM_WCMP, ierr)
      END DO
    ELSE
      iVect(1)=np
      iVect(2)=npa
      CALL MPI_SEND(iVect,2,MPI_INTEGER, 0, 19, MPI_COMM_WCMP, ierr)
      CALL MPI_RECV(ListNP ,NAPROC,MPI_INTEGER, 0, 20, MPI_COMM_WCMP, istatus, ierr)
      CALL MPI_RECV(ListNPA,NAPROC,MPI_INTEGER, 0, 21, MPI_COMM_WCMP, istatus, ierr)
    END IF
    deallocate(iVect)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 4'
     FLUSH(740+IAPROC)
#endif
    !
    ! ListIPLG
    !
    sumNP=sum(ListNPA)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 5, sumNP=', sumNP
     FLUSH(740+IAPROC)
#endif
    allocate(ListIPLG(sumNP), stat=istat)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 6'
     FLUSH(740+IAPROC)
#endif
    IF (istat /= 0) CALL PDLIB_ABORT(2)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 7'
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, NAPROC=', NAPROC, ' NTPROC=', NTPROC
     FLUSH(740+IAPROC)
#endif
    IF (IAPROC .eq. 1) THEN
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Main node 1'
     FLUSH(740+IAPROC)
#endif
      idx=0
      DO IP=1,NPA
        idx=idx+1
        ListIPLG(IP)=iplg(IP)
      END DO
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Main node 2'
     FLUSH(740+IAPROC)
#endif
      DO IPROC=2,NAPROC
        len=ListNPA(IPROC)
        allocate(iVect(len), stat=istat)
        IF (istat /= 0) CALL PDLIB_ABORT(3)
        CALL MPI_RECV(iVect,len,MPI_INTEGER, iProc-1, 269, MPI_COMM_WCMP, istatus, ierr)
        DO IP=1,len
          idx=idx+1
          ListIPLG(idx)=iVect(IP)
        END DO
        deallocate(iVect)
      END DO
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Main node 3'
     FLUSH(740+IAPROC)
#endif
      DO IPROC=2,NAPROC
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before mpi_send IPROC=', IPROC
     FLUSH(740+IAPROC)
#endif
        CALL MPI_SEND(ListIPLG, sumNP,MPI_INTEGER, iProc-1, 271, MPI_COMM_WCMP, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After mpi_send IPROC=', IPROC
     FLUSH(740+IAPROC)
#endif
      END DO
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Main node 4'
     FLUSH(740+IAPROC)
#endif
    ELSE
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Peripheral node 1'
     FLUSH(740+IAPROC)
#endif
      CALL MPI_SEND(iplg, npa,MPI_INTEGER, 0, 269, MPI_COMM_WCMP, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Peripheral node 2'
     FLUSH(740+IAPROC)
#endif
      CALL MPI_RECV(ListIPLG,sumNP,MPI_INTEGER, 0, 271, MPI_COMM_WCMP, istatus, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Peripheral node 3'
     FLUSH(740+IAPROC)
#endif
    END IF
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'ComputeListNP_ListNPA_Kernel, step 8'
     FLUSH(740+IAPROC)
#endif
  END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
    SUBROUTINE ComputeListNP_ListNPA_ListIPLG
    USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
    USE W3ADATMD, ONLY: MPI_COMM_WAVE
    USE yowDatapool, only: rtype, istatus
    USE yowNodepool, only: npa, np, iplg
    USE yowNodepool, only: ListNP, ListNPA, ListIPLG
    IMPLICIT NONE
    INCLUDE "mpif.h"
    INTEGER sumNP, iProc, ierr, istat
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before ComputeListNP_ListNPA_Kernel'
     FLUSH(740+IAPROC)
#endif
    IF (IAPROC .le. NAPROC) THEN
      CALL ComputeListNP_ListNPA_ListIPLG_Kernel    
    END IF
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) ' After ComputeListNP_ListNPA_Kernel'
     FLUSH(740+IAPROC)
#endif
    IF (IAPROC .eq. 1) THEN
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Doing the send'
     FLUSH(740+IAPROC)
#endif
      sumNP=sum(ListNPA)
      DO iProc=NAPROC+1,NTPROC
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Loop state 1, iProc=', iProc
     FLUSH(740+IAPROC)
#endif
        CALL MPI_SEND(ListNP, NAPROC,MPI_INTEGER, iProc-1, 20, MPI_COMM_WAVE, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Loop state 2, iProc=', iProc
     FLUSH(740+IAPROC)
#endif
        CALL MPI_SEND(ListNPA,NAPROC,MPI_INTEGER, iProc-1, 21, MPI_COMM_WAVE, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Loop state 3, iProc=', iProc
     FLUSH(740+IAPROC)
#endif
        CALL MPI_SEND(ListIPLG, sumNP,MPI_INTEGER, iProc-1, 271, MPI_COMM_WAVE, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Loop state 4, iProc=', iProc
     FLUSH(740+IAPROC)
#endif
      END DO
    END IF
    IF (IAPROC .gt. NAPROC) THEN
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before allocation'
     FLUSH(740+IAPROC)
#endif
      allocate(ListNP(NAPROC), ListNPA(NAPROC), stat=istat)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before receiving of data 1'
     FLUSH(740+IAPROC)
#endif
      CALL MPI_RECV(ListNP ,NAPROC,MPI_INTEGER, 0, 20, MPI_COMM_WAVE, istatus, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before receiving of data 2'
     FLUSH(740+IAPROC)
#endif
      CALL MPI_RECV(ListNPA,NAPROC,MPI_INTEGER, 0, 21, MPI_COMM_WAVE, istatus, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before computing sumNP'
     FLUSH(740+IAPROC)
#endif
      sumNP=sum(ListNPA)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before allocating ListIPLG'
     FLUSH(740+IAPROC)
#endif
      allocate(ListIPLG(sumNP), stat=istat)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'Before receiving ListIPLG'
     FLUSH(740+IAPROC)
#endif
      CALL MPI_RECV(ListIPLG,sumNP,MPI_INTEGER, 0, 271, MPI_COMM_WAVE, istatus, ierr)
#ifdef W3_DEBUGINIT
     WRITE(740+IAPROC,*) 'After receiving ListIPLG'
     FLUSH(740+IAPROC)
#endif
    END IF
    END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
  SUBROUTINE ComputeBoundaryInformation
    use yowNodepool, only: ListNP, ListNPA, ListIPLG
    USE W3GDATMD, ONLY: IOBP
    USE W3ODATMD, only : IAPROC, NAPROC
    IMPLICIT NONE
    integer ListFirst(NAPROC), NbSend(NAPROC)
    integer IPROC, eSend, IP, IP_glob, NPAloc
    ListFirst=0
    DO IPROC=2,NAPROC
      ListFirst(iProc)=ListFirst(iProc-1) + ListNPA(iProc-1)
    END DO
    DO IPROC=1,NAPROC
      NPAloc=ListNPA(IPROC)
      eSend=0
      DO IP=1,NPAloc
        IP_glob=ListIPLG(IP + ListFirst(IPROC))
        IF (IOBP(IP_glob) .eq. 1) THEN
          eSend=eSend + 1
        END IF
      END DO
      NbSend(IPROC)=eSend
    END DO
  END SUBROUTINE
end module yowfunction
