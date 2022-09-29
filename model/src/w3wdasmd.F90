!> @file
!> @brief Contains module W3WDASMD.
!>
!> @author H. L. Tolman @date 06-Dec-2010
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Intended as the interface for externally supplied
!>  data assimilation software.
!>
!> @details This module is intended as the interface for externally
!>  supplied data assimilation software to be used with WAVEWATCH III.
!>  The main subroutine W3WDAS is incorporated in the generic WAVEWATCH
!>  III shell ww3_shel, and thus provides integrated time management
!>  and running of the wave model and data assimilation side by side.
!>
!>  Present wave conditions (including dynamically changing wave
!>  grids), as well as wave data are passed to the routine through
!>  the dynamic data structrure, as introduced in model version 3.06.
!>
!>  A three tier data structure is used with three separate data
!>  sets. Tentatively, they are intended for mean wave parameters,
!>  1-D and 2-D spectral data. This separation is made only for
!>  economy in file and memory usage. All three data sets are defined
!>  here only by a record length and a number of records. All data are
!>  treated as real numbers, but the meaning of all record components
!>  is completely at the discretion of the author of the data
!>  assimilation scheme.
!>
!>  To promote portability, it is suggested to use this module only
!>  as an interface to your own assimilation routine(s).
!>
!> @author H. L. Tolman @date 06-Dec-2010
!>
MODULE W3WDASMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         06-Dec-2010 |
  !/                  +-----------------------------------+
  !/
  !/    25-Jan-2002 : Origination.                        ( version 2.17 )
  !/    27-Dec-2004 : Multiple grid version.              ( version 3.06 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
  !/                  specify index closure for a grid.   ( version 3.14 )
  !/                  (T. J. Campbell, NRL)
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     This module is intended as the interface for externally supplied
  !     data assimlation software to be used with WAVEWATCH III. The
  !     main subroutine W3WDAS is incorporated in the generic WAVEWATCH
  !     III shell ww3_shel, and thus provides integrated time management
  !     and running of the wave model and data assimilation side by side.
  !
  !     Present wave conditions (including dynamically changing wave
  !     grids), as well as wave data are passed to the routine through
  !     the dynamic data structrure, as introduced in model version 3.06
  !
  !     A three tier data structure is used with three separate data
  !     sets. Tentatively, they are intended for mean wave parameters,
  !     1-D and 2-D spectral data. This separation is made only for
  !     economy in file and menory usage. All three data sets are defined
  !     here onlt by a record length and a number of records. All data are
  !     treated as real numbers, but the meaing of all record components
  !     is completely at the discretion of the author of the data
  !     assimilation scheme.
  !
  !     To promote portability, it is suggested to use this module only
  !     as an interface to your own assimilation routine(s).
  !
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
  !      W3WDAS    Subr. Public   Actual wave model.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      ....      Subr. W3SERVMD Service routines.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !     - This module still requires an OpenMP or  MPI setup to be made
  !       compatible with WAVEWATCH III inside the user supplied
  !       routines.
  !
  !  6. Switches :
  !
  !       !/S     Enable subroutine tracing.
  !       !/T     Test output.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  PUBLIC
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief WAVEWATCH III data assimilation interface routine.
  !>
  !> @param[in] DASFLAG FLags for three data sets.
  !> @param[in] RECL Record lengths for three data sets.
  !> @param[in] NDAT Number of data for three data sets.
  !> @param[in] DATA0 Observations.
  !> @param[in] DATA1 Observations.
  !> @param[in] DATA2 Observations.
  !>
  !> @author H. L. Tolman  @date 06-Dec-2010
  !>
  SUBROUTINE W3WDAS ( DASFLAG, RECL, NDAT, DATA0, DATA1, DATA2 )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         06-Dec-2010 |
    !/                  +-----------------------------------+
    !/
    !/    25-Jan-2002 : Origination.                        ( version 2.17 )
    !/    27-Dec-2004 : Multiple grid version.              ( version 3.06 )
    !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (T. J. Campbell, NRL)
    !/
    !  1. Purpose :
    !
    !     WAVEWATCH III data assimilation interface routine.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       DASFLAG   L.A.   I   FLags for three data sets.
    !       RECLD   I.A.   I   Record lengths for three data sets.
    !       ND      I.A.   I   Number of data for three data sets.
    !       DATAn   R.A.   I   Observations.
    !     ----------------------------------------------------------------
    !
    !     Local parameters :
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !      EXTCDE    Subr. W3SERVMD Program abort.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !     Any program shell or integrated model after initialization of
    !     WAVEWATCH III (to assure availability of data in used modules).
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !       !/T     Enable test output.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD
    USE W3WDATMD
    USE W3ADATMD
    USE W3ODATMD, ONLY: NDSO, NDSE, NDST, SCREEN, NAPROC, IAPROC,   &
         NAPLOG, NAPOUT, NAPERR
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    IMPLICIT NONE
    !
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: RECL(3), NDAT(3)
    REAL, INTENT(IN)        :: DATA0(RECL(1),NDAT(1))
    REAL, INTENT(IN)        :: DATA1(RECL(2),NDAT(2))
    REAL, INTENT(IN)        :: DATA2(RECL(3),NDAT(3))
    LOGICAL, INTENT(IN)     :: DASFLAG(3)
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters :
    !/
    INTEGER                 :: J
#ifdef W3_T
    INTEGER                 :: MREC, MDAT, IREC, IDAT
#endif
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T
    REAL, ALLOCATABLE       :: TDATA(:,:)
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! 1.  Initializations and test output
    ! 1.a Subroutine tracing
    !
#ifdef W3_S
    CALL STRACE (IENT, 'W3WDAS')
#endif
    !
    ! 1.b Echo part of parameter list (test output only).
    !
#ifdef W3_T
    WRITE (NDST,9000) NDSO, NDSE, NDST, SCREEN, NAPROC, IAPROC,  &
         NAPOUT, NAPERR, TIME
    DO J=1, 3
      IF ( DASFLAG(J) ) THEN
        WRITE (NDST,9001) J, DASFLAG(J), RECL(J), NDAT(J)
        MREC    = MIN(RECL(J),6)
        MDAT    = MIN(NDAT(J),10)
        IF ( ALLOCATED(TDATA) ) DEALLOCATE (TDATA)
        ALLOCATE ( TDATA(RECL(J),MDAT) )
        IF ( J .EQ. 1 ) TDATA = DATA0(:,1:MDAT)
        IF ( J .EQ. 2 ) TDATA = DATA1(:,1:MDAT)
        IF ( J .EQ. 3 ) TDATA = DATA2(:,1:MDAT)
        DO IDAT=1, MDAT
          WRITE (NDST,9002) IDAT, TDATA(1:MREC,IDAT)
          IF ( MREC .LT. RECL(J) ) WRITE (NDST,9003)         &
               TDATA(MREC+1:RECL(J),IDAT)
        END DO
      ELSE
        WRITE (NDST,9001) J, DASFLAG(J)
      END IF
    END DO
    IF ( ALLOCATED(TDATA) ) DEALLOCATE (TDATA)
#endif
    !
    ! 1.c Test grid info from W3GDATMD
    !
#ifdef W3_T
    WRITE (NDST,9010) NX, NY, NSEA, NSEAL, NK, NTH,              &
         ICLOSE, FLAGLL, SX, SY, X0, Y0
#endif
    !
    ! 2.  Actual data assimilation routine ------------------------------- /
    !
    !     User-defined data assimilation routines to be plugged in here.
    !     All that could be needed is avainalble in this subroutine,
    !     including the grid definition from W3GDATMD. All
    !     can thus be included in the parameter list, and no explcit links
    !     to other WAVEWATCH III routines will be needed within the
    !     data assimilation routines ( with the possible exception of the
    !     CONSTANTS module ), If there is a reason to terminate the code,
    !     pass an error code out of the routine and use EXTCDE to stop
    !     the WAVEWATCH III run altogether. Check the system documentation
    !     on how to ad your routines to the compile and link system.
    !
    !     CALL .....
    !
    !     IF ( ..... ) CALL EXTCDE ( 99 )
    !
    RETURN
    !
    ! Formats
    !
    !1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3WDAS :'/                &
    !              '     ILLIGAL GRID SIZES INPUT : ',4I8/                &
    !              '                         GRID : ',4I8/)
#ifdef W3_T
9000 FORMAT ( ' TEST W3WDAS : UNIT NUMBERS : ',4I4/               &
         '               MPI SETTINGS : ',4I4/               &
         '               TIME         : ',I8.8,I7.6)
9001 FORMAT ( '               DATASET INFO : ',I1,L2,2I8)
9002 FORMAT (17X,I2,6E10.3)
9003 FORMAT (19X,   6E10.3)
#endif
    !
#ifdef W3_T
9010 FORMAT ( ' TEST W3WDAS : ARRAY DIMS.  : ',6I8/               &
         '               GRID         : ',1I2,1L2,4E11.4)
#endif
    !/
    !/ End of W3WDAS ----------------------------------------------------- /
    !/
  END SUBROUTINE W3WDAS
  !/
  !/ End of module W3WDASMD -------------------------------------------- /
  !/
END MODULE W3WDASMD
