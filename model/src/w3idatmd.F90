#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3IDATMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    02-Apr-2004 : Origination.                        ( version 3.06 )
!/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
!/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
!/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
!/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
!/    24-Apr-2015 : Adding OASIS coupling calls         ( version 5.07 )
!/                  (M. Accensi & F. Ardhuin, IFREMER)
!/    21-Jun-2018 : Add FSWND input for SMC grid. JGLi  ( version 6.04 )
!/    22-Mar-2021 : Momentum and air density support    ( version 7.13 )
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Define data structures to set up wave model input data for
!     several models simultaneously.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      NIDATA    Int.  Public   Number of models in array dim.
!      IIDATA    Int.  Public   Selected model for output, init. at -1.
!      INPUT     TYPE  Public   Basic data structure.
!      INPUTS    INPUT Public   Array of data structures.
!     ----------------------------------------------------------------
!
!     All elements of INPUT are aliased to pointers with the same
!     name. Some aditional pointer provide previous equivalenced
!     parameters. These pointers are defined as :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      TLN       I.A.  Public   Time for water level field.
!      TC0/N     I.A.  Public   Times for current fields.
!      TW0/N     I.A.  Public   Times for wind fields.
!      TDN       I.A.  Public   Time for mud density field.
!      TTN       I.A.  Public   Time for mud thickness field.
!      TVN       I.A.  Public   Time for mud viscosity field.
!      TIN       I.A.  Public   Time for ice field. (concentration)
!      TU0/N     I.A.  Public   Times for momentum fields.
!      TR0/N     I.A.  Public   Times for air density fields.
!      TI1N      I.A.  Public   Time for ice field. (parameter 1)
!      TI2N      I.A.  Public   Time for ice field. (parameter 2)
!      TI3N      I.A.  Public   Time for ice field. (parameter 3)
!      TI4N      I.A.  Public   Time for ice field. (parameter 4)
!      TI5N      I.A.  Public   Time for ice field. (parameter 5)
!      TnN       I.A.  Public   Time for data types 1-3.
!      TDN       I.A.  Public   Time for next data.
!      TG0/N     I.A.  Public   Times for grid motion data.
!      TFN       I.A.  Public   Array consolidating most above times.
!      GA0/N     Real  Public   Norm of grid speed vector.
!      GD0/N     Real  Public   Direction of grid speed vector.
!      WX0/N     R.A.  Public   Cartesian X and Y wind components
!      WY0/N     R.A.  Public      for both times.
!      DT0/N     R.A.  Public   Corr. air-sea temperature differences.
!      CX0/N     R.A.  Public   Cartesian X and Y current components
!      CY0/N     R.A.  Public      for both times.
!      WLEV      R.A.  Public   Next water level field.
!      ICEI      R.A.  Public   Ice concentrations.
!      UX0/N     R.A.  Public   Cartesian X and Y momentum components
!      UY0/N     R.A.  Public      for both times.
!      RH0/N     R.A.  Public   Air density for both times
!      BERGI     R.A.  Public   Iceberg damping coefficient
!      IINIT     Log.  Public   Flag for array initialization.
!      FLLEV     Log.  Public   Flag for water level input.
!      FLCUR     Log.  Public   Flag for current input.
!      FLWIND    Log.  Public   Flag for wind input.
!      FLICE     Log.  Public   Flag for ice input.
!      FLTAUA    Log.  Public   Flag for atmospheric momentum input
!      FLRHOA    Log.  Public   Flag for air density input
!      INFLAGS1  L.A.  Public   Array consolidating the above six 
!                               flags, as well as four additional
!                               data flags.
!      INFLAGS2  L.A.  Public   Like INFLAGS1 but does *not* get changed
!                               when model reads last record of ice.ww3
!      FLAGSC    L.A.  Public   Coupling or not for input variables
!      JFIRST    Int   Public   First index of arrays related to
!                               input fields.  At present this is
!                               hardwired below. Field-related arrays
!                               (e.g., INFLAGS1) will be allocated from
!                               JFIRST:9 (e.g., ALLOCATE(INFLAGS1(JFIRST:9))).
!       CXTIDE    R.A.  Public   Tidal constituents of X current component
!       CYTIDE    R.A.  Public   Tidal constituents of Y current component
!       WLTIDE    R.A.  Public   Tidal constituents of water level
!       FLLEVTIDE Log.  Public   Flag for use of tidal const. in water level input.
!       FLCURTIDE Log.  Public   Flag for use of tidal const. in current input.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3NINP    Subr. Public   Set number of grids/models.
!      W3DIMI    Subr. Public   Set dimensions of arrays.
!      W3SETI    Subr. Public   Point to selected grid / model.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SETG    Subr. W3GDATMD Point to proper model grid.
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr. W3SERVMD Abort program with exit code.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!     - The number of grids is taken from W3GDATMD, and needs to be
!       set first with W3DIMG.
!
!     - INFLAGS1 dimensioning is hardwired as INFLAGS1(-7:14) where lowest possible 
!       value of JFIRST is JFIRST=-7
!
!  6. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!     !/TIDE Use of tidal constituents
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
      PUBLIC
!/
!/ Module private variable for checking error returns
!/
      INTEGER, PRIVATE        :: ISTAT
!/
!/ Conventional declarations
!/
      INTEGER                 :: NIDATA = -1, IIDATA = -1

      INTEGER                 :: JFIRST = 1

#ifdef W3_TIDE
      INTEGER                 :: NTIDE   ! number of tidal constituents
      REAL, ALLOCATABLE       :: TIDEFREQ(:)
#endif
!/
!/ Data structure INPUT
!/
      TYPE INPUT
        INTEGER               :: TFN(2,-7:10), TC0(2), TW0(2),        &
                                 TU0(2), TR0(2), TDN(2), TG0(2)
        REAL                  :: GA0, GD0, GAN, GDN
#ifdef W3_WRST
        REAL, POINTER         :: WXNwrst(:,:),WYNwrst(:,:)
#endif
        REAL, POINTER         :: WX0(:,:), WY0(:,:), DT0(:,:),        &
                                 WXN(:,:), WYN(:,:), DTN(:,:),        &
                                 CX0(:,:), CY0(:,:), CXN(:,:),        &
                                 CYN(:,:), WLEV(:,:), ICEI(:,:),      &
                                 UX0(:,:), UY0(:,:), UXN(:,:),        &
                                 UYN(:,:), RH0(:,:), RHN(:,:),        &
                                 BERGI(:,:), MUDT(:,:), MUDV(:,:),    &
                                 MUDD(:,:), ICEP1(:,:), ICEP2(:,:),   &
                                 ICEP3(:,:), ICEP4(:,:), ICEP5(:,:)
#ifdef W3_TIDE
 REAL, POINTER         ::  CXTIDE(:,:,:,:), CYTIDE(:,:,:,:),    &
                           WLTIDE(:,:,:,:)
#endif
        LOGICAL               :: IINIT
#ifdef W3_WRST
        LOGICAL               :: WRSTIINIT=.FALSE.
#endif
! note that if size of INFLAGS1 is changed, then TFLAGS in wminitmd.ftn
!    also must be resized.
        LOGICAL               :: INFLAGS1(-7:14), FLAGSC(-7:14),      &
                                 INFLAGS2(-7:14)
      END TYPE INPUT
!/
!/ Data storage
!/
      TYPE(INPUT), TARGET, ALLOCATABLE :: INPUTS(:)
!/
!/ Data aliasses for structure INPUT(S)
!/
      INTEGER, POINTER        :: TFN(:,:), TLN(:), TC0(:), TCN(:),    &
                                 TW0(:), TWN(:), TU0(:), TUN(:),      &
                                 TIN(:), TR0(:), TRN(:), T0N(:),      &
                                 T1N(:), T2N(:), TDN(:), TG0(:),      &
                                 TGN(:), TTN(:), TVN(:), TZN(:),      &
                                 TI1(:), TI2(:), TI3(:), TI4(:), TI5(:) 
      REAL, POINTER           :: GA0, GD0, GAN, GDN
      REAL, POINTER           :: WX0(:,:), WY0(:,:), DT0(:,:),        &
                                 WXN(:,:), WYN(:,:), DTN(:,:),        &
#ifdef W3_WRST
                                 WXNwrst(:,:),WYNwrst(:,:),           &
#endif
                                 CX0(:,:), CY0(:,:), CXN(:,:),        &
                                 CYN(:,:), WLEV(:,:), ICEI(:,:),      &
                                 UX0(:,:), UY0(:,:), UXN(:,:),        &
                                 UYN(:,:), RH0(:,:), RHN(:,:),        &
                                 BERGI(:,:), MUDT(:,:), MUDV(:,:),    &
                                 MUDD(:,:), ICEP1(:,:), ICEP2(:,:),   &
                                 ICEP3(:,:), ICEP4(:,:), ICEP5(:,:)
#ifdef W3_TIDE
       REAL, POINTER           :: CXTIDE(:,:,:,:),         &
                                  CYTIDE(:,:,:,:), WLTIDE(:,:,:,:)
#endif
      LOGICAL, POINTER        :: IINIT
      LOGICAL, POINTER        :: INFLAGS1(:), INFLAGS2(:), FLAGSC(:)
      LOGICAL, POINTER        :: FLLEV, FLCUR, FLWIND, FLICE, FLTAUA, &
                                 FLRHOA
      LOGICAL, POINTER        :: FLMTH, FLMVS, FLMDN
      LOGICAL, POINTER        :: FLIC1, FLIC2, FLIC3, FLIC4, FLIC5 
#ifdef W3_TIDE
      LOGICAL, POINTER        ::  FLLEVTIDE, FLCURTIDE,  &
                                  FLLEVRESI, FLCURRESI
#endif
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3NINP ( NDSE, NDST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 !
!/                  +-----------------------------------+
!/
!/    02-Apr-2004 : Origination.                        ( version 3.06 )
!/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
!/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
!/    22-Mar-2021 : Momentum and air density support    ( version 7.13 )
!/
!  1. Purpose :
!
!     Set up the number of grids to be used.
!
!  2. Method :
!
!     Use data stored in NGRIDS in W3GDATMD.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NDSE    Int.   I   Error output unit number.
!       NDST    Int.   I   Test output unit number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!     Any program that uses this grid structure.
!
!  6. Error messages :
!
!     - Error checks on previous setting of variable NGRIDS.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NGRIDS, NAUXGR
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NDSE, NDST
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: I
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3NINP')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .EQ. -1 ) THEN
          WRITE (NDSE,1001) NGRIDS
          CALL EXTCDE (1)
        END IF
!
! -------------------------------------------------------------------- /
! 2.  Set variable and allocate arrays
!
      ALLOCATE ( INPUTS(-NAUXGR:NGRIDS), STAT=ISTAT )
      CHECK_ALLOC_STATUS ( ISTAT )
      NIDATA = NGRIDS
!
! -------------------------------------------------------------------- /
! 3.  Initialize parameters
!
      DO I=-NAUXGR, NGRIDS
        INPUTS(I)%TFN(1,:) = -1
        INPUTS(I)%TFN(2,:) =  0
        INPUTS(I)%TC0(1)   = -1
        INPUTS(I)%TC0(2)   =  0
        INPUTS(I)%TW0(1)   = -1
        INPUTS(I)%TW0(2)   =  0
        INPUTS(I)%TU0(1)   = -1
        INPUTS(I)%TU0(2)   =  0
        INPUTS(I)%TR0(1)   = -1
        INPUTS(I)%TR0(2)   =  0
        INPUTS(I)%TDN(1)   = -1
        INPUTS(I)%TDN(2)   =  0
        INPUTS(I)%TG0(1)   = -1
        INPUTS(I)%TG0(2)   =  0
        INPUTS(I)%GA0      = 0.
        INPUTS(I)%GD0      = 0.
        INPUTS(I)%GAN      = 0.
        INPUTS(I)%GDN      = 0.
        INPUTS(I)%IINIT    = .FALSE.
        INPUTS(I)%INFLAGS1 = .FALSE.
        INPUTS(I)%INFLAGS2 = .FALSE.
        INPUTS(I)%FLAGSC   = .FALSE.
        END DO
!
#ifdef W3_T
      WRITE (NDST,9000) -NAUXGR, NGRIDS
#endif
!
      RETURN
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3NINP : NGRIDS NOT YET SET *** '/         &
               '                    NGRIDS = ',I10/                   &
               '                    RUN W3NMOD FIRST'/)
!
#ifdef W3_T
 9000 FORMAT (' TEST W3NINP : SETTING UP FOR ',I2,' -',I3,' GRIDS')
#endif
!/
!/ End of W3NINP ----------------------------------------------------- /
!/
      END SUBROUTINE W3NINP
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3DIMI  ( IMOD, NDSE, NDST, FLAGSTIDEIN )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 !
!/                  +-----------------------------------+
!/
!/    02-Apr-2004 : Origination.                        ( version 3.06 )
!/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
!/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
!/    10-Dec-2014 : Add checks for allocate status      ( version 5.04 )
!/    21-Jun-2018 : Add FSWND input for SMC grid. JGLi  ( version 6.04 )
!/    22-Mar-2021 : Momentum and air density support    ( version 7.13 )
!/
!  1. Purpose :
!
!     Initialize an individual data grid at the proper dimensions.
!
!  2. Method :
!
!     Allocate directly into the structure array. Note that
!     this cannot be done through the pointer alias!
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number to point to.
!       NDSE    Int.   I   Error output unit number.
!       NDST    Int.   I   Test output unit number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!       
!     Main wave model drivers.
!
!  6. Error messages :
!
!     - Check on input parameters.
!     - Check on previous allocation.
!
!  7. Remarks :
!
!     - W3SETI needs to be called after allocation to point to 
!       proper allocated arrays.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD,  ONLY: NGRIDS, NAUXGR, IGRID, W3SETG, NX, NY
#ifdef W3_SMC
      USE W3GDATMD,  ONLY: FSWND, NSEA
#endif
      USE W3SERVMD,  ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD,  ONLY: STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
      LOGICAL, INTENT(IN), OPTIONAL     :: FLAGSTIDEIN(4)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: JGRID
      LOGICAL                 :: FLAGSTIDE(4)=.FALSE.
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3DIMI')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NGRIDS .EQ. -1 ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF
!
      IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NIDATA ) THEN
          WRITE (NDSE,1002) IMOD, -NAUXGR, NIDATA
          CALL EXTCDE (2)
        END IF
!
      IF ( INPUTS(IMOD)%IINIT ) THEN
          WRITE (NDSE,1003)
          CALL EXTCDE (3)
        END IF
!
#ifdef W3_T
      WRITE (NDST,9000) IMOD
#endif
!
      JGRID  = IGRID
      IF ( JGRID .NE. IMOD ) CALL W3SETG ( IMOD, NDSE, NDST )
!
! -------------------------------------------------------------------- /
! 2.  Allocate arrays
!
#ifdef W3_TIDE
      IF ( PRESENT(FLAGSTIDEIN) ) THEN
          FLAGSTIDE(:) = FLAGSTIDEIN(:)
        END IF
#endif

      FLIC1  => INPUTS(IMOD)%INFLAGS1(-7)
      FLIC2  => INPUTS(IMOD)%INFLAGS1(-6)
      FLIC3  => INPUTS(IMOD)%INFLAGS1(-5)
      FLIC4  => INPUTS(IMOD)%INFLAGS1(-4)
      FLIC5  => INPUTS(IMOD)%INFLAGS1(-3)
!
      FLMDN  => INPUTS(IMOD)%INFLAGS1(-2)
      FLMTH  => INPUTS(IMOD)%INFLAGS1(-1)
      FLMVS  => INPUTS(IMOD)%INFLAGS1(0)
!
      FLLEV  => INPUTS(IMOD)%INFLAGS1(1)
      FLCUR  => INPUTS(IMOD)%INFLAGS1(2)
#ifdef W3_TIDE
      FLLEVTIDE  => INPUTS(IMOD)%INFLAGS1(11)
      FLCURTIDE  => INPUTS(IMOD)%INFLAGS1(12)
      FLLEVRESI  => INPUTS(IMOD)%INFLAGS1(13)
      FLCURRESI  => INPUTS(IMOD)%INFLAGS1(14)
!
      FLLEVTIDE = FLAGSTIDE(1)
      FLCURTIDE = FLAGSTIDE(2)
      FLLEVRESI = FLAGSTIDE(3)
      FLCURRESI = FLAGSTIDE(4)     
#endif
 
      FLWIND => INPUTS(IMOD)%INFLAGS1(3)
      FLICE  => INPUTS(IMOD)%INFLAGS1(4)
      FLTAUA => INPUTS(IMOD)%INFLAGS1(5)
      FLRHOA => INPUTS(IMOD)%INFLAGS1(6)
!
! notes: future improvement: flags for ICEPx should be 
!     "all or nothing" rather than 5 individual flags

      IF ( FLIC1  ) THEN
          ALLOCATE ( INPUTS(IMOD)%ICEP1(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
      IF ( FLIC2  ) THEN
          ALLOCATE ( INPUTS(IMOD)%ICEP2(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
      IF ( FLIC3  ) THEN
          ALLOCATE ( INPUTS(IMOD)%ICEP3(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
      IF ( FLIC4  ) THEN
          ALLOCATE ( INPUTS(IMOD)%ICEP4(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
      IF ( FLIC5  ) THEN
          ALLOCATE ( INPUTS(IMOD)%ICEP5(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
!
      IF ( FLMDN  ) THEN
          ALLOCATE ( INPUTS(IMOD)%MUDD(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
      IF ( FLMTH  ) THEN
          ALLOCATE ( INPUTS(IMOD)%MUDT(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
      IF ( FLMVS  ) THEN
          ALLOCATE ( INPUTS(IMOD)%MUDV(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
!
      IF ( FLLEV  ) THEN
          ALLOCATE ( INPUTS(IMOD)%WLEV(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
!
      IF ( FLCUR  ) THEN
#ifdef W3_SMC
       IF( FSWND ) THEN
          ALLOCATE ( INPUTS(IMOD)%CX0(NSEA,1) ,              &
                     INPUTS(IMOD)%CY0(NSEA,1) ,              &
                     INPUTS(IMOD)%CXN(NSEA,1) ,              &
                     INPUTS(IMOD)%CYN(NSEA,1) , STAT=ISTAT )
       ELSE
#endif
          ALLOCATE ( INPUTS(IMOD)%CX0(NX,NY) ,              &
                     INPUTS(IMOD)%CY0(NX,NY) ,              &
                     INPUTS(IMOD)%CXN(NX,NY) ,              &
                     INPUTS(IMOD)%CYN(NX,NY) , STAT=ISTAT )
#ifdef W3_SMC
       ENDIF
#endif
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
!
#ifdef W3_TIDE
      IF ( FLLEVTIDE  ) THEN
          ALLOCATE ( INPUTS(IMOD)%WLTIDE(NX,NY,NTIDE,2), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
!
      IF ( FLCURTIDE  ) THEN
          ALLOCATE ( INPUTS(IMOD)%CXTIDE(NX,NY,NTIDE,2),  &
                     INPUTS(IMOD)%CYTIDE(NX,NY,NTIDE,2), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
#endif
!

#ifdef W3_WRST
      IF(.NOT.(INPUTS(IMOD)%WRSTIINIT)) THEN 
        ALLOCATE (   INPUTS(IMOD)%WXNwrst(NX,NY) ,              &
                     INPUTS(IMOD)%WYNwrst(NX,NY) , STAT=ISTAT )
        INPUTS(IMOD)%WRSTIINIT=.TRUE.
      ENDIF
#endif

      IF ( FLWIND ) THEN
#ifdef W3_SMC
       IF( FSWND ) THEN
          ALLOCATE ( INPUTS(IMOD)%WX0(NSEA,1) ,              &
                     INPUTS(IMOD)%WY0(NSEA,1) ,              &
                     INPUTS(IMOD)%DT0(NSEA,1) ,              &
                     INPUTS(IMOD)%WXN(NSEA,1) ,              &
                     INPUTS(IMOD)%WYN(NSEA,1) ,              &
                     INPUTS(IMOD)%DTN(NSEA,1) , STAT=ISTAT )
       ELSE
#endif
          ALLOCATE ( INPUTS(IMOD)%WX0(NX,NY) ,              &
                     INPUTS(IMOD)%WY0(NX,NY) ,              &
                     INPUTS(IMOD)%DT0(NX,NY) ,              &
                     INPUTS(IMOD)%WXN(NX,NY) ,              &
                     INPUTS(IMOD)%WYN(NX,NY) ,              &
                     INPUTS(IMOD)%DTN(NX,NY) , STAT=ISTAT )
#ifdef W3_SMC
       ENDIF
#endif
          CHECK_ALLOC_STATUS ( ISTAT )
          INPUTS(IMOD)%DT0 = 0.
          INPUTS(IMOD)%DTN = 0.
        END IF
!
      IF ( FLICE  ) THEN
          ALLOCATE ( INPUTS(IMOD)%ICEI(NX,NY),              &
                     INPUTS(IMOD)%BERGI(NX,NY), STAT=ISTAT )
          CHECK_ALLOC_STATUS ( ISTAT )
          INPUTS(IMOD)%BERGI = 0.
        END IF
!
      IF ( FLTAUA  ) THEN
#ifdef W3_SMC
       IF( FSWND ) THEN
          ALLOCATE ( INPUTS(IMOD)%UX0(NSEA,1) ,              &
                     INPUTS(IMOD)%UY0(NSEA,1) ,              &
                     INPUTS(IMOD)%UXN(NSEA,1) ,              &
                     INPUTS(IMOD)%UYN(NSEA,1) , STAT=ISTAT )
       ELSE
#endif
          ALLOCATE ( INPUTS(IMOD)%UX0(NX,NY) ,              &
                     INPUTS(IMOD)%UY0(NX,NY) ,              &
                     INPUTS(IMOD)%UXN(NX,NY) ,              &
                     INPUTS(IMOD)%UYN(NX,NY) , STAT=ISTAT )
#ifdef W3_SMC
       ENDIF
#endif
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
!
      IF ( FLRHOA  ) THEN
#ifdef W3_SMC
       IF( FSWND ) THEN
          ALLOCATE ( INPUTS(IMOD)%RH0(NSEA,1) ,             &
                     INPUTS(IMOD)%RHN(NSEA,1) , STAT=ISTAT )
       ELSE
#endif
          ALLOCATE ( INPUTS(IMOD)%RH0(NX,NY) ,              &
                     INPUTS(IMOD)%RHN(NX,NY) , STAT=ISTAT )
#ifdef W3_SMC
       ENDIF
#endif
          CHECK_ALLOC_STATUS ( ISTAT )
        END IF
!
      INPUTS(IMOD)%IINIT  = .TRUE.
!
#ifdef W3_T
      WRITE (NDST,9001)
#endif
!
! -------------------------------------------------------------------- /
! 3.  Point to allocated arrays
!
      CALL W3SETI ( IMOD, NDSE, NDST )
!
#ifdef W3_T
      WRITE (NDST,9002)
#endif
!
! -------------------------------------------------------------------- /
! 4.  Update counters in grid
!
#ifdef W3_T
      WRITE (NDST,9003)
#endif
!
! -------------------------------------------------------------------- /
! 5.  Restore previous grid setting if necessary
!
      IF ( JGRID .NE. IMOD ) CALL W3SETG ( JGRID, NDSE, NDST )
!
      RETURN
!
! Check inputs for stresses
      IF(FLTAUA) THEN
#ifdef W3_FLX0
        WRITE (NDSE,*) " *** WARNING W3DIMI : TAUA NOT USED *** "
#endif
#ifdef W3_FLX1
        WRITE (NDSE,*) " *** WARNING W3DIMI : TAUA NOT USED *** "
#endif
#ifdef W3_FLX2
        WRITE (NDSE,*) " *** WARNING W3DIMI : TAUA NOT USED *** "
#endif
#ifdef W3_FLX3
        WRITE (NDSE,*) " *** WARNING W3DIMI : TAUA NOT USED *** "
#endif
#ifdef W3_FLX4
        WRITE (NDSE,*) " *** WARNING W3DIMI : TAUA NOT USED *** "
#endif
      END IF
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3DIMI : GRIDS NOT INITIALIZED *** '/      &
               '                    RUN W3NMOD FIRST '/)
 1002 FORMAT (/' *** ERROR W3DIMI : ILLEGAL MODEL NUMBER *** '/       &
               '                    IMOD   = ',I10/                   &
               '                    NAUXGR = ',I10/                   &
               '                    NIDATA = ',I10/)
 1003 FORMAT (/' *** ERROR W3DIMI : ARRAY(S) ALREADY ALLOCATED *** ')
!
#ifdef W3_T
 9000 FORMAT (' TEST W3DIMI : MODEL ',I4,' DIM. AT ',2I5,I7)
 9001 FORMAT (' TEST W3DIMI : ARRAYS ALLOCATED')
 9002 FORMAT (' TEST W3DIMI : POINTERS RESET')
 9003 FORMAT (' TEST W3DIMI : DIMENSIONS STORED')
#endif
!/
!/ End of W3DIMI ----------------------------------------------------- /
!/
      END SUBROUTINE W3DIMI
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3SETI ( IMOD, NDSE, NDST )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 !
!/                  +-----------------------------------+
!/
!/    02-Apr-2004 : Origination.                        ( version 3.06 )
!/    19-Jul-2006 : Adding auxiliary grids.             ( version 3.10 )
!/    04-Oct-2006 : Add filter to array pointers.       ( version 3.10 )
!/    22-Mar-2021 : Momentum and air density support    ( version 7.13 )
!/
!  1. Purpose :
!
!     Select one of the WAVEWATCH III grids / models.
!
!  2. Method :
!
!     Point pointers to the proper variables in the proper element of
!     the GRIDS array.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       IMOD    Int.   I   Model number to point to.
!       NDSE    Int.   I   Error output unit number.
!       NDST    Int.   I   Test output unit number.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     See module documentation.
!
!  5. Called by :
!
!     Any subroutine.
!
!  6. Error messages :
!
!     Many subroutines in the WAVEWATCH system.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/S    Enable subroutine tracing.
!     !/T    Enable test output
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3GDATMD, ONLY: NAUXGR
!
      USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
      USE W3SERVMD, ONLY: STRACE
#endif
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: IMOD, NDSE, NDST
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!/
#ifdef W3_S
      CALL STRACE (IENT, 'W3SETI')
#endif
!
! -------------------------------------------------------------------- /
! 1.  Test input and module status
!
      IF ( NIDATA .EQ. -1 ) THEN
          WRITE (NDSE,1001)
          CALL EXTCDE (1)
        END IF   
!
      IF ( IMOD.LT.-NAUXGR .OR. IMOD.GT.NIDATA ) THEN
          WRITE (NDSE,1002) IMOD, -NAUXGR, NIDATA
          CALL EXTCDE (2)
        END IF
!
#ifdef W3_T
      WRITE (NDST,9000) IMOD
#endif
!
! -------------------------------------------------------------------- /
! 2.  Set model numbers
!
      IIDATA = IMOD
!
! -------------------------------------------------------------------- /
! 3.  Set pointers
!
      TFN    => INPUTS(IMOD)%TFN
      TC0    => INPUTS(IMOD)%TC0
      TW0    => INPUTS(IMOD)%TW0
      TU0    => INPUTS(IMOD)%TU0
      TR0    => INPUTS(IMOD)%TR0
      TG0    => INPUTS(IMOD)%TG0
      TDN    => INPUTS(IMOD)%TDN
!
      TI1    => INPUTS(IMOD)%TFN(:,-7)
      TI2    => INPUTS(IMOD)%TFN(:,-6)
      TI3    => INPUTS(IMOD)%TFN(:,-5)
      TI4    => INPUTS(IMOD)%TFN(:,-4)
      TI5    => INPUTS(IMOD)%TFN(:,-3)
!
      TZN    => INPUTS(IMOD)%TFN(:,-2)
      TTN    => INPUTS(IMOD)%TFN(:,-1)
      TVN    => INPUTS(IMOD)%TFN(:,0)
!
      TLN    => INPUTS(IMOD)%TFN(:,1)
      TCN    => INPUTS(IMOD)%TFN(:,2)
      TWN    => INPUTS(IMOD)%TFN(:,3)
      TIN    => INPUTS(IMOD)%TFN(:,4)
      TUN    => INPUTS(IMOD)%TFN(:,5)
      TRN    => INPUTS(IMOD)%TFN(:,6)
      T0N    => INPUTS(IMOD)%TFN(:,7)
      T1N    => INPUTS(IMOD)%TFN(:,8)
      T2N    => INPUTS(IMOD)%TFN(:,9)
      TGN    => INPUTS(IMOD)%TFN(:,10)
!
      GA0    => INPUTS(IMOD)%GA0
      GD0    => INPUTS(IMOD)%GD0
      GAN    => INPUTS(IMOD)%GAN
      GDN    => INPUTS(IMOD)%GDN
!
      IINIT  => INPUTS(IMOD)%IINIT
      INFLAGS1  => INPUTS(IMOD)%INFLAGS1
      INFLAGS2  => INPUTS(IMOD)%INFLAGS2
      FLAGSC => INPUTS(IMOD)%FLAGSC
!
      FLIC1  => INPUTS(IMOD)%INFLAGS1(-7)
      FLIC2  => INPUTS(IMOD)%INFLAGS1(-6)
      FLIC3  => INPUTS(IMOD)%INFLAGS1(-5)
      FLIC4  => INPUTS(IMOD)%INFLAGS1(-4)
      FLIC5  => INPUTS(IMOD)%INFLAGS1(-3)
!
      FLMDN  => INPUTS(IMOD)%INFLAGS1(-2)
      FLMTH  => INPUTS(IMOD)%INFLAGS1(-1)
      FLMVS  => INPUTS(IMOD)%INFLAGS1(0)
!
      FLLEV  => INPUTS(IMOD)%INFLAGS1(1)
      FLCUR  => INPUTS(IMOD)%INFLAGS1(2)
#ifdef W3_TIDE
      FLLEVTIDE  => INPUTS(IMOD)%INFLAGS1(11)
      FLCURTIDE  => INPUTS(IMOD)%INFLAGS1(12)
      FLLEVRESI  => INPUTS(IMOD)%INFLAGS1(13)
      FLCURRESI  => INPUTS(IMOD)%INFLAGS1(14)
#endif

      FLWIND => INPUTS(IMOD)%INFLAGS1(3)
      FLICE  => INPUTS(IMOD)%INFLAGS1(4)
      FLTAUA => INPUTS(IMOD)%INFLAGS1(5)
      FLRHOA => INPUTS(IMOD)%INFLAGS1(6)
!
      IF ( IINIT ) THEN
!
          IF ( FLIC1  ) THEN
              ICEP1  => INPUTS(IMOD)%ICEP1
          END IF
          IF ( FLIC2  ) THEN
              ICEP2  => INPUTS(IMOD)%ICEP2
          END IF
          IF ( FLIC3  ) THEN
              ICEP3  => INPUTS(IMOD)%ICEP3
          END IF
          IF ( FLIC4  ) THEN
              ICEP4  => INPUTS(IMOD)%ICEP4
          END IF
          IF ( FLIC5  ) THEN
              ICEP5  => INPUTS(IMOD)%ICEP5
          END IF
!
          IF ( FLMDN  ) THEN
              MUDD   => INPUTS(IMOD)%MUDD
          END IF
          IF ( FLMTH  ) THEN
              MUDT   => INPUTS(IMOD)%MUDT
          END IF
          IF ( FLMVS  ) THEN
              MUDV   => INPUTS(IMOD)%MUDV
          END IF
!
          IF ( FLLEV  ) THEN
              WLEV   => INPUTS(IMOD)%WLEV
            END IF
!
          IF ( FLCUR  ) THEN
              CX0    => INPUTS(IMOD)%CX0
              CY0    => INPUTS(IMOD)%CY0
              CXN    => INPUTS(IMOD)%CXN
              CYN    => INPUTS(IMOD)%CYN
            END IF
#ifdef W3_TIDE
          IF ( FLLEVTIDE ) THEN 
              WLTIDE => INPUTS(IMOD)%WLTIDE
            END IF
          IF ( FLCURTIDE ) THEN 
              CXTIDE => INPUTS(IMOD)%CXTIDE
              CYTIDE => INPUTS(IMOD)%CYTIDE
            END IF
#endif
!
#ifdef W3_WRST
          WXNwrst    => INPUTS(IMOD)%WXNwrst
          WYNwrst    => INPUTS(IMOD)%WYNwrst
#endif

          IF ( FLWIND  ) THEN
              WX0    => INPUTS(IMOD)%WX0
              WY0    => INPUTS(IMOD)%WY0
              DT0    => INPUTS(IMOD)%DT0
              WXN    => INPUTS(IMOD)%WXN
              WYN    => INPUTS(IMOD)%WYN
              DTN    => INPUTS(IMOD)%DTN
            END IF
!
          IF ( FLICE  ) THEN
              ICEI   => INPUTS(IMOD)%ICEI
              BERGI  => INPUTS(IMOD)%BERGI
            END IF
!
          IF ( FLTAUA  ) THEN
              UX0    => INPUTS(IMOD)%UX0
              UY0    => INPUTS(IMOD)%UY0
              UXN    => INPUTS(IMOD)%UXN
              UYN    => INPUTS(IMOD)%UYN
            END IF
!
          IF ( FLRHOA  ) THEN
              RH0    => INPUTS(IMOD)%RH0
              RHN    => INPUTS(IMOD)%RHN
            END IF
!
        END IF
!
      RETURN
!
! Formats
!
 1001 FORMAT (/' *** ERROR W3SETI : GRIDS NOT INITIALIZED *** '/      &
               '                    RUN W3NMOD FIRST '/)
 1002 FORMAT (/' *** ERROR W3SETI : ILLEGAL MODEL NUMBER *** '/       &
               '                    IMOD   = ',I10/                   &
               '                    NAUXGR = ',I10/                   &
               '                    NIDATA = ',I10/)
!
#ifdef W3_T
 9000 FORMAT (' TEST W3SETI : MODEL ',I4,' SELECTED')
#endif
!/
!/ End of W3SETI ----------------------------------------------------- /
!/
      END SUBROUTINE W3SETI
!/
!/ End of module W3IDATMD -------------------------------------------- /
!/
      END MODULE W3IDATMD
