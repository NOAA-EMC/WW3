!> @file
!> @brief Reading/writing of model definition file.
!>
!> @author H. L. Tolman
!> @author F. Ardhuin
!> @date   15-Apr-2020
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Reading/writing of model definition file.
!>
!> @details Arrays allocated here on read or ing ww3_grid on write.
!>
!> @author H. L. Tolman
!> @author F. Ardhuin
!> @date   15-Apr-2020
!>
MODULE W3IOGRMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  !            F. Ardhuin             !
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         15-Apr-2020 |
  !/                  +-----------------------------------+
  !/
  !/    For updates see W3IOGR documentation.
  !/
  !  1. Purpose :
  !
  !     Reading/writing of model definition file .
  !
  !  2. Variables and types :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      VERGRD    C*10  Private  Model definition file version number.
  !      IDSTR     C*35  Private  Model definition file ID string.
  !     ----------------------------------------------------------------
  !
  !  3. Subroutines and functions :
  !
  !      Name      Type  Scope    Description
  !     ----------------------------------------------------------------
  !      W3IOGR    Subr. Public   Read/write model definition file.
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3SETG    Subr. W3GDATMD Point to data structure for spatial gr.
  !      W3DIMX    Subr.    Id.   Set up arrays for spatial grid.
  !      W3DIMS    Subr.    Id.   Set array dimensions for a spec. grid.
  !      W3SETO    Subr. W3ODATMD Point to data structure for spatial gr.
  !      W3DMO5    Subr.    Id.   Set array dimensions.
  !      INPTAB    Subr. W3SRC2MD Fill interpolation tables for
  !                               dispersion relation.
  !      DISTAB    Subr. W3DISPMD Input coefficient lookup table.
  !      INSNL1    Subr. W3SNL1MD Initialization of the DIA.
  !      INSNL2    Subr. W3SNL2MD Initialization of WRT.
  !      INSNL3    Subr. W3SNL3MD Initialization of GMD.
  !      INSNL5    Subr. W3SNL5MD Initialization of GKE.
  !      INSNLS    Subr. W3SNLSMD Initialization of nonlinear `smoother'.
  !      STRACE    Subr. W3SERVMD Subroutine tracing.
  !      EXTCDE    Subr. W3SERVMD Abort program with exit code.
  !     ----------------------------------------------------------------
  !
  !  5. Remarks :
  !
  !     - Arrays allocated here on read or ing ww3_grid on write.
  !
  !  6. Switches :
  !
  !     See subroutine.
  !
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  ! module default
  implicit none

  PUBLIC
  !/
  !/ Private parameter statements (ID strings)
  !/
  CHARACTER(LEN=10), PARAMETER, PRIVATE :: VERGRD = '2021-08-06'
  CHARACTER(LEN=35), PARAMETER, PRIVATE ::                        &
       IDSTR = 'WAVEWATCH III MODEL DEFINITION FILE'
  !/
  !/ Public variables
  !/
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Reading and writing of the model definition file.
  !>
  !> @details The file is opened within the routine, the name is pre-defined
  !>  and the unit number is given in the parameter list. The model
  !>  definition file is written using UNFORMATTED write statements.
  !>
  !> @param[in] INXOUT  Test string for read/write.
  !> @param[in] NDSM    File unit number.
  !> @param[in] IMOD    Model number for W3GDAT etc.
  !> @param[in] FEXT    File extension to be used.
  !>
  !> @author H. L. Tolman
  !> @author F. Ardhuin
  !> @date   19-Oct-2020

  SUBROUTINE W3IOGR ( INXOUT, NDSM, IMOD, FEXT &
#ifdef W3_ASCII
                      ,NDSA                    &
#endif          
          )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  !            F. Ardhuin             !
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         19-Oct-2020 |
    !/                  +-----------------------------------+
    !/
    !/    14-Jan-1999 : Distributed FORTRAN 77 version.     ( version 1.18 )
    !/    04-Feb-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Major changes to logistics.
    !/    14-Feb-2000 : Exact-NL added.                     ( version 2.01 )
    !/    09-Jan-2001 : Flat grid option.                   ( version 2.06 )
    !/    02-Feb-2001 : Exact-NL version 3.0                ( version 2.07 )
    !/    27-Feb-2001 : Third propagation scheme added.     ( version 2.08 )
    !/    16-Mar-2001 : Fourth propagation scheme added.    ( version 2.09 )
    !/    29-Mar-2001 : Sub-grid islands added.             ( version 2.10 )
    !/    11-Jan-2002 : Sub-grid ice added.                 ( version 2.15 )
    !/    09-May-2002 : Switch clean up.                    ( version 2.21 )
    !/    27-Aug-2002 : Exact-NL version 4.0                ( version 2.22 )
    !/    26-Nov-2002 : Adding first VDIA and MDIA.         ( version 3.01 )
    !/    01-Aug-2003 : Adding moving grid GSE correction.  ( version 3.03 )
    !/    08-Mar-2004 : Multiple grid version.              ( version 3.06 )
    !/    04-May-2005 : Change to MPI_COMM_WAVE.            ( version 3.07 )
    !/    24-Jun-2005 : Add MAPST2 processing.              ( version 3.07 )
    !/    09-Nov-2005 : Remove soft boundary options.       ( version 3.08 )
    !/    23-Jun-2006 : Add W3SLN1 parameters.              ( version 3.09 )
    !/    27-Jun-2006 : Adding file name preamble.          ( version 3.09 )
    !/    25-Jul-2006 : Reorder for 'GRID' option to read   ( version 3.10 )
    !/                  spectral data also.
    !/    28-Oct-2006 : Add partitioning pars.              ( version 3.10 )
    !/    26-Mar-2007 : Add partitioning pars.              ( version 3.11 )
    !/    16-Apr-2006 : Add Miche limiter pars.             ( version 3.11 )
    !/    25-Apr-2007 : Adding Battjes-Janssen Sdb.         ( version 3.11 )
    !/    09-Oct-2007 : Adding WAM cycle 4+ Sin and Sds.    ( version 3.13 )
    !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
    !/    30-Oct-2009 : Fix ndst arg in call to w3dmo5.     ( version 3.14 )
    !/                  (T. J. Campbell, NRL)
    !/    30-Oct-2009 : Implement curvilinear grid type.    ( version 3.14 )
    !/                  (W. E. Rogers & T. J. Campbell, NRL)
    !/    23-Dec-2009 : Addition of COU namelists           ( version 3.14 )
    !/    31-Oct-2010 : Implement unstructured grids        ( version 3.14 )
    !/                  (A. Roland and F. Ardhuin)
    !/    06-Dec-2010 : Change from GLOBAL (logical) to ICLOSE (integer) to
    !/                  specify index closure for a grid.   ( version 3.14 )
    !/                  (T. J. Campbell, NRL)
    !/    12-Jun-2012 : Add /RTD option or rotated grid option.
    !/                  (Jian-Guo Li)                       ( version 4.06 )
    !/    13-Jul-2012 : Move GMD (SNL3) and nonlinear filter (SNLS)
    !/                  from 3.15 (HLT).                    ( version 4.08 )
    !/    12-Dec-2012 : Adding SMC grid.  JG_Li             ( version 4.08 )
    !/    19-Dec-2012 : Add NOSWLL to file.                 ( version 4.11 )
    !/    01-Jul-2013 : Document UQ / UNO switches in file  ( version 4.12 )
    !/    10-Sep-2013 : Add IG1 parameters                  ( version 4.12 )
    !/    16-Sep-2013 : Add Arctic part in SMC grid.        ( version 4.12 )
    !/    11-Nov-2013 : Make SMC and RTD grids compatible.  ( version 4.13 )
    !/    06-Mar-2014 : Writes out a help message on error  ( version 4.18 )
    !/    10-Mar-2014 : Add IC2 parameters                  ( version 5.01 )
    !/    29-May-2014 : Add IC3 parameters                  ( version 5.01 )
    !/    20-Aug-2016 : Add IOBPA                           ( version 5.12 )
    !/    08-Mar-2018 : Add FSWND for SMC grid.             ( version 6.02 )
    !/    05-Jun-2018 : Add PDLIB/DEBUGINIT and implcit scheme parameters
    !/                  for unstructured grids              ( version 6.04 )
    !/    27-Jul-2018 : Added PTMETH and PTFCUT parameters  ( version 6.05 )
    !/                  (C. Bunney, UKMO)
    !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
    !/    26-Aug-2018 : UOST (Mentaschi et al. 2015, 2018)  ( version 6.06 )
    !/    15-Apr-2020 : Adds optional opt-out for CFL on BC ( version 7.08 )
    !/    18-Jun-2020 : Adds 360-day calendar option        ( version 7.08 )
    !/    19-Oct-2020 : Add AIRCMIN, AIRGB parameters       ( version 7.08 )
    !/    07-07-2021  : S_{nl} GKE NL5 (Q. Liu)             ( version 7.12 )
    !/    19-Jul-2021 : Momentum and air density support    ( version 7.14 )
    !/
    !/    Copyright 2009-2013 National Weather Service (NWS),
    !/       National Oceanic and Atmospheric Administration.  All rights
    !/       reserved.  WAVEWATCH III is a trademark of the NWS.
    !/       No unauthorized use without permission.
    !/
    !  1. Purpose :
    !
    !     Reading and writing of the model definition file.
    !
    !  2. Method :
    !
    !     The file is opened within the routine, the name is pre-defined
    !     and the unit number is given in the parameter list. The model
    !     definition file is written using UNFORMATTED write statements.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       INXOUT  C*(*)  I   Test string for read/write, valid are:
    !                         'READ',  'WRITE' and 'GRID'.
    !       NDSM    Int.   I   File unit number.
    !       NDSA    Int.   I   File unit number. ascii
    !       IMOD    Int.   I   Model number for W3GDAT etc.
    !       FEXT    C*(*)  I   File extension to be used.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     See above.
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3INIT    Subr. W3INITMD Wave model initialization routine.
    !      ......    Prog.   N/A    All WAVEWATCH III aux programs and
    !                               drivers.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       Tests on INXOUT, file status and on array dimensions.
    !
    !  7. Remarks :
    !
    !     - The model definition file has the pre-defined name
    !       'mod_def.FILEXT'.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/MPI  MPI calls
    !
    !     !/LNn  Select source terms
    !     !/STn
    !     !/NLn
    !     !/BTn
    !     !/DBn
    !     !/TRn
    !     !/BSn
    !     !/XXn
    !
    !     !/S    Enable subroutine tracing.
    !     !/T    Enable test output
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    use w3servmd, only : print_memcheck

    USE CONSTANTS
    USE W3GDATMD
#ifdef W3_MPI
    USE W3ADATMD, ONLY: MPI_COMM_WAVE
#endif
    USE W3ODATMD
#ifdef W3_ST2
    USE W3SRC2MD, ONLY: INPTAB
#endif
#ifdef W3_ST3
    USE W3SRC3MD, ONLY: INSIN3
#endif
#ifdef W3_ST4
    USE W3SRC4MD, ONLY: INSIN4, TAUT, TAUHFT, TAUHFT2, &
         DELU, DELTAUW, DELUST, &
         DELALP, DELTAIL, &
         DIKCUMUL
#endif
#ifdef W3_NL1
    USE W3SNL1MD, ONLY: INSNL1, INSNLGQM
#endif
#ifdef W3_NL2
    USE W3SNL2MD, ONLY: INSNL2
#endif
#ifdef W3_NL3
    USE W3SNL3MD, ONLY: INSNL3
#endif
#ifdef W3_NL5
    USE W3SNL5MD, ONLY: INSNL5
#endif
#ifdef W3_NLS
    USE W3SNLSMD, ONLY: INSNLS
#endif
#ifdef W3_IS2
    USE W3SIS2MD, ONLY: INSIS2
#endif
    USE W3TIMEMD, ONLY: CALTYPE
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3DISPMD
#ifdef W3_UOST
    USE W3UOSTMD, ONLY: UOST_INITGRID
#endif
    !
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)             :: NDSM
    INTEGER, INTENT(IN), OPTIONAL   :: IMOD
    CHARACTER, INTENT(IN)           :: INXOUT*(*)
    CHARACTER, INTENT(IN), OPTIONAL :: FEXT*(*)
#ifdef W3_ASCII
    INTEGER, INTENT(IN), OPTIONAL   :: NDSA
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IGRD, IERR, I, J, MTH, MK, ISEA, IX, IY
    INTEGER                 :: IEXT, IPRE
#ifdef W3_ST4
    INTEGER                 :: IK, ITH, IK2, ITH2
#endif
    INTEGER, ALLOCATABLE    :: MAPTMP(:,:)
#ifdef W3_MPI
    INTEGER                 :: IERR_MPI, IP
#endif
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
#ifdef W3_T
    INTEGER                 :: K
#endif
    LOGICAL                 :: WRITE, FLTEST = .FALSE., TESTLL,     &
         FLSNL2 = .FALSE.
    LOGICAL, SAVE           :: FLINP = .FALSE. , FLDISP = .FALSE.,  &
         FLIS  = .FALSE.
    CHARACTER(LEN=10)       :: VERTST
    CHARACTER(LEN=13)       :: TEMPXT
    CHARACTER(LEN=30)       :: TNAME0, TNAME1, TNAME2, TNAME3,      &
         TNAME4, TNAME5, TNAME6,              &
         TNAMEP, TNAMEG, TNAMEF, TNAMEI
    CHARACTER(LEN=30)       :: FNAME0, FNAME1, FNAME2, FNAME3,      &
         FNAME4, FNAME5, FNAME6,              &
         FNAMEP, FNAMEG, FNAMEF, FNAMEI
    CHARACTER(LEN=35)       :: IDTST
    CHARACTER(LEN=60)       :: MESSAGE(5)
    LOGICAL                 :: GLOBAL

    REAL, ALLOCATABLE       :: XGRD4(:,:), YGRD4(:,:)

    integer                 :: memunit
    !/
    !/ ------------------------------------------------------------------- /
    !/
    memunit = 740+IAPROC
#ifdef W3_S
    CALL STRACE (IENT, 'W3IOGR')
#endif
    !
    call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 1')

    MESSAGE =(/ '     MOD DEF FILE WAS GENERATED WITH A DIFFERENT    ', &
         '     WW3 VERSION OR USING A DIFFERENT SWITCH FILE.  ',        &
         '     MAKE SURE WW3_GRID IS COMPILED WITH SAME SWITCH',        &
         '     AS WW3_SHEL OR WW3_MULTI, RUN WW3_GRID AGAIN   ',        &
         '     AND THEN TRY AGAIN THE PROGRAM YOU JUST USED.  '/)
    !
    TNAMEF = '------------------------------'
    TNAME0 = '------------------------------'
    TNAME1 = '------------------------------'
    TNAME2 = '------------------------------'
    TNAME3 = '------------------------------'
    TNAME4 = '------------------------------'
    TNAME5 = '------------------------------'
    TNAME6 = '------------------------------'
    TNAMEP = '------------------------------'
    TNAMEG = '------------------------------'
    TNAMEI = '------------------------------'
    !
#ifdef W3_FLX1
    TNAMEF = 'Wu (1980)                     '
#endif
#ifdef W3_FLX2
    TNAMEF = 'Tolman and Chalikov (1996)    '
#endif
#ifdef W3_FLX3
    TNAMEF = 'T and C(1996) with cap on Cd  '
#endif
#ifdef W3_FLX4
    TNAMEF = 'Hwang (2011) with cap on Cd   '
#endif
#ifdef W3_FLX5
    TNAMEF = 'Direct use of stress          '
#endif
#ifdef W3_LN0
    TNAME0 = 'Not defined                   '
#endif
#ifdef W3_LN1
    TNAME0 = 'Cavaleri and M.-R. (1982)     '
#endif
#ifdef W3_ST0
    TNAME1 = 'Not defined                   '
#endif
#ifdef W3_ST1
    TNAME1 = 'WAM cycles 1 through 3        '
#endif
#ifdef W3_ST2
    TNAME1 = 'Tolman and Chalikov (1996)    '
#endif
#ifdef W3_ST3
    TNAME1 = 'WAM cycle 4+                  '
#endif
#ifdef W3_ST4
    TNAME1 = 'Ardhuin et al. (2009+)        '
#endif
#ifdef W3_ST6
    TNAME1 = 'BYDB input and dissipation    '
#endif
#ifdef W3_NL0
    TNAME2 = 'Not defined                   '
#endif
#ifdef W3_NL1
    TNAME2 = 'Discrete Interaction Approx.  '
#endif
#ifdef W3_NL2
    TNAME2 = 'Exact nonlinear interactions  '
#endif
#ifdef W3_NL3
    TNAME2 = 'Generalized Multiple DIA      '
#endif
#ifdef W3_NL4
    TNAME2 = 'Two Scaled Approximation      '
#endif
#ifdef W3_NL5
    TNAME2 = 'Generalized Kinetic Equation  '
#endif
#ifdef W3_BT0
    TNAME3 = 'Not defined                   '
#endif
#ifdef W3_BT1
    TNAME3 = 'JONSWAP                       '
#endif
#ifdef W3_BT4
    TNAME3 = 'SHOWEX                        '
#endif
#ifdef W3_BT8
    TNAME3 = 'Muddy Bed (D & L)             '
#endif
#ifdef W3_IC1
    TNAMEI = 'Ice sink term (uniform k_i)   '
#endif
#ifdef W3_IC2
    TNAMEI = 'Ice sink term (Lui et al)     '
#endif
#ifdef W3_IC3
    TNAMEI = 'Ice sink term (Wang and Shen) '
#endif
#ifdef W3_IC4
    TNAMEI = 'Ice sink term (empirical)     '
#endif
#ifdef W3_IC5
    TNAMEI = 'Ice sink term (eff. medium)   '
#endif
#ifdef W3_DB0
    TNAME4 = 'Not defined                   '
#endif
#ifdef W3_DB1
    TNAME4 = 'Battjes and Janssen (1978)    '
#endif
#ifdef W3_TR0
    TNAME5 = 'Not defined                   '
#endif
#ifdef W3_BS0
    TNAME6 = 'Not defined                   '
#endif
#ifdef W3_PR0
    TNAMEP = 'No propagation                '
#endif
#ifdef W3_PR1
    TNAMEP = 'First order upstream          '
#endif
#ifdef W3_UQ
    TNAMEP = '3rd order UQ scheme           '
#endif
#ifdef W3_UNO
    TNAMEP = '2nd order UNO scheme          '
#endif
#ifdef W3_PR0
    TNAMEG = 'No GSE aleviation             '
#endif
#ifdef W3_PR1
    TNAMEG = 'No GSE aleviation (1up prop)  '
#endif
#ifdef W3_PR2
    TNAMEG = 'Diffusion operator            '
#endif
#ifdef W3_PR3
    TNAMEG = 'Averaging operator            '
#endif
    !
    FNAMEF = TNAMEF
    FNAME0 = TNAME0
    FNAME1 = TNAME1
    FNAME2 = TNAME2
    FNAME3 = TNAME3
    FNAME4 = TNAME4
    FNAME5 = TNAME5
    FNAME6 = TNAME6
    FNAMEP = TNAMEP
    FNAMEG = TNAMEG
    FNAMEI = TNAMEI
    !
#ifdef W3_T
    FLTEST = .TRUE.
#endif
#ifdef W3_NL2
    FLSNL2 = .TRUE.
#endif
    !
    ! test input parameters ---------------------------------------------- *
    !
    IF ( PRESENT(IMOD) ) THEN
      IGRD   = IMOD
    ELSE
      IGRD   = 1
    END IF
    !
    IF ( PRESENT(FEXT) ) THEN
      TEMPXT = FEXT
    ELSE
      TEMPXT = 'ww3'
    END IF
    !
    IF (INXOUT.NE.'READ' .AND. INXOUT.NE.'WRITE'                    &
         .AND. INXOUT.NE.'GRID') THEN
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,900) INXOUT
      CALL EXTCDE ( 1 )
    END IF
    !
    WRITE  = INXOUT .EQ. 'WRITE'
    !
#ifdef W3_T
    WRITE (NDST,9000) INXOUT, WRITE, NDSM, IGRD, TEMPXT
#endif
    !
    CALL W3SETO ( IGRD, NDSE, NDST )
    CALL W3SETG ( IGRD, NDSE, NDST )
    FILEXT = TEMPXT
    call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 2')
    !
    ! open file ---------------------------------------------------------- *
    !
    IEXT   = LEN_TRIM(FILEXT)
    IPRE   = LEN_TRIM(FNMPRE)
    !
    !AR: ADD DEBUGFLAG      WRITE(*,*) 'FILE=', FNMPRE(:IPRE)//'mod_def.'//FILEXT(:IEXT)
    IF ( WRITE ) THEN
      OPEN (NDSM,FILE=FNMPRE(:IPRE)//'mod_def.'//FILEXT(:IEXT),   &
           form='UNFORMATTED', convert=file_endian,ERR=800,IOSTAT=IERR)
#ifdef W3_ASCII
      OPEN (NDSA,FILE=FNMPRE(:IPRE)//'mod_def.'//FILEXT(:IEXT)//'.txt',   &
           form='FORMATTED',ERR=800,IOSTAT=IERR)
#endif
    ELSE
      OPEN (NDSM,FILE=FNMPRE(:IPRE)//'mod_def.'//FILEXT(:IEXT),   &
           form='UNFORMATTED', convert=file_endian,STATUS='OLD',ERR=800,IOSTAT=IERR)
    ENDIF
    !
    REWIND ( NDSM )
    !
    ! Dimensions and test information --------------------------------------
    !
    IF ( WRITE ) THEN
      WRITE (NDSM)                                               &
           IDSTR, VERGRD, NX, NY, NSEA, NTH, NK,                 &
           NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
           FNAME4, FNAME5, FNAME6, FNAMEP, FNAMEG,               &
           FNAMEF, FNAMEI
#ifdef W3_ASCII
      WRITE (NDSA,*)                                             &
           'IDSTR, VERGRD, NX, NY, NSEA, NTH, NK,                &
           NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
           FNAME4, FNAME5, FNAME6, FNAMEP, FNAMEG,               &
           FNAMEF, FNAMEI:',                                     &
           IDSTR, VERGRD, NX, NY, NSEA, NTH, NK,                 &
           NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
           FNAME4, FNAME5, FNAME6, FNAMEP, FNAMEG,               &
           FNAMEF, FNAMEI
#endif
      !
#ifdef W3_SMC
      WRITE (NDSM) NCel, NUFc, NVFc, NRLv, MRFct
#ifdef W3_ASCII
      WRITE (NDSA,*) 'NCel, NUFc, NVFc, NRLv, MRFct:',           &
                     NCel, NUFc, NVFc, NRLv, MRFct
#endif
      WRITE (NDSM) NGLO, NARC, NBGL, NBAC, NBSMC
#ifdef W3_ASCII
      WRITE (NDSA,*) 'NGLO, NARC, NBGL, NBAC, NBSMC:',             &
                   NGLO, NARC, NBGL, NBAC, NBSMC
#endif
#endif
      !
      WRITE (NDSM)                                               &
           (NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO)
#ifdef W3_ASCII
      WRITE (NDSA,*)                                             &
           '(NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO):',           &
           (NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO)
#endif
#ifdef W3_T
      WRITE (NDST,9001) IDSTR, VERGRD, NX, NY, NSEA, NTH, NK,    &
           NBI, NFBPO, 9, GNAME, FNAME0, FNAME1, FNAME2, FNAME3, &
           FNAME4, FNAME5, FNAME6, FNAMEP, FNAMEG,               &
           FNAMEF, FNAMEI
      WRITE (NDST,9002) (NBO(I),I=0,NFBPO)
      WRITE (NDST,9003) (NBO2(I),I=0,NFBPO)
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                    &
           IDTST, VERTST, NX, NY, NSEA, MTH, MK,                 &
           NBI, NFBPO, GNAME, FNAME0, FNAME1, FNAME2, FNAME3,    &
           FNAME4, FNAME5, FNAME6, FNAMEP, FNAMEG,               &
           FNAMEF, FNAMEI
      !
#ifdef W3_SMC
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                    &
           NCel, NUFc, NVFc, NRLv, MRFct
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                    &
           NGLO, NARC, NBGL, NBAC, NBSMC
#endif
      !
      NK     = MK
      NTH    = MTH
      NK2    = NK + 2
      NSPEC  = NK * NTH
#ifdef W3_T
      WRITE (NDST,9001) IDSTR, VERGRD, NX, NY, NSEA, NTH, NK,    &
           NBI, NFBPO, 9, GNAME, FNAME0, FNAME1, FNAME2, FNAME3, &
           FNAME4, FNAME5, FNAME6, FNAMEP, FNAMEG,               &
           FNAMEF, FNAMEI
#endif
      !
      IF ( IDTST .NE. IDSTR ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,901) IDTST, IDSTR
        CALL EXTCDE ( 10 )
      END IF
      IF ( VERTST .NE. VERGRD ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,902) VERTST, VERGRD
        CALL EXTCDE ( 11 )
      END IF
      IF ( NFBPO .GT. 9 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,904) NFBPO, 9
        CALL EXTCDE ( 13 )
      END IF
      IF ( FNAME0 .NE. TNAME0 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 0, FILEXT(:IEXT), FNAME0, TNAME0, &
             MESSAGE
        CALL EXTCDE ( 14 )
      END IF
      IF ( FNAME1 .NE. TNAME1 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 1, FILEXT(:IEXT), FNAME1, TNAME1, &
             MESSAGE
        CALL EXTCDE ( 15 )
      END IF
      IF ( FNAME2 .NE. TNAME2 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 2, FILEXT(:IEXT), FNAME2, TNAME2, &
             MESSAGE
        CALL EXTCDE ( 16 )
      END IF
      IF ( FNAME3 .NE. TNAME3 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 3, FILEXT(:IEXT), FNAME3, TNAME3, &
             MESSAGE
        CALL EXTCDE ( 17 )
      END IF
      IF ( FNAMEI .NE. TNAMEI ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 3, FILEXT(:IEXT), FNAMEI, TNAMEI, &
             MESSAGE
        CALL EXTCDE ( 17 )
      END IF
      IF ( FNAME4 .NE. TNAME4 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 4, FILEXT(:IEXT), FNAME4, TNAME4, &
             MESSAGE
        CALL EXTCDE ( 18 )
      END IF
      IF ( FNAME5 .NE. TNAME5 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 5, FILEXT(:IEXT), FNAME5, TNAME5, &
             MESSAGE
        CALL EXTCDE ( 19 )
      END IF
      IF ( FNAME6 .NE. TNAME6 ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,905) 6, FILEXT(:IEXT), FNAME6, TNAME6, &
             MESSAGE
        CALL EXTCDE ( 20 )
      END IF
      IF ( FNAMEP .NE. TNAMEP ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,906) FNAMEP, TNAMEP
        CALL EXTCDE ( 22 )
      END IF
      IF ( FNAMEG .NE. TNAMEG ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,907) FNAMEG, TNAMEG, MESSAGE
        CALL EXTCDE ( 22 )
      END IF
      IF ( FNAMEF .NE. TNAMEF ) THEN
        IF ( IAPROC .EQ. NAPERR )                               &
             WRITE (NDSE,908) FILEXT(:IEXT), FNAMEF, TNAMEF, MESSAGE
        CALL EXTCDE ( 24 )
      END IF
      !
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                   &
           (NBO(I),I=0,NFBPO), (NBO2(I),I=0,NFBPO)
#ifdef W3_T
      WRITE (NDST,9002) (NBO(I),I=0,NFBPO)
      WRITE (NDST,9003) (NBO2(I),I=0,NFBPO)
#endif
      !
    ENDIF
    call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 3')
    !
    ! Parameters in modules  --------------------------------------------- *
    !                                                   Module W3GDAT GRID
    !
    ALLOCATE ( MAPTMP(NY,NX) )
    !
    IF ( WRITE ) THEN
      MAPTMP = MAPSTA + 8*MAPST2
      WRITE (NDSM)                                            &
           GTYPE, FLAGLL, ICLOSE
#ifdef W3_ASCII
      WRITE (NDSA,*)                                          &
           'GTYPE, FLAGLL, ICLOSE:',                          &
           GTYPE, FLAGLL, ICLOSE
#endif
      !
      ! Writes different kind of information depending on grid type
      !
      SELECT CASE ( GTYPE )
        !!Li  SMCTYPE shares info with RLGTYPE.   JGLi12Oct2020
      CASE ( RLGTYPE, SMCTYPE )
        WRITE (NDSM)                                          &
             SX, SY, X0, Y0
#ifdef W3_ASCII
        WRITE (NDSA,*)                                        &
             'SX, SY, X0, Y0:',                               &
             SX, SY, X0, Y0
#endif
      CASE ( CLGTYPE )
        WRITE (NDSM)                                          &
             REAL(XGRD), REAL(YGRD)
#ifdef W3_ASCII
        WRITE (NDSA,*)                                        &
             'REAL(XGRD), REAL(YGRD):',                       &
             REAL(XGRD), REAL(YGRD)
#endif
      CASE (UNGTYPE)
        WRITE (NDSM)                                          &
             FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,   &
             FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,    &
             DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,      &
             NTRI,COUNTOT, COUNTRI, NNZ,                      &
             B_JGS_TERMINATE_MAXITER,                         &
             B_JGS_TERMINATE_DIFFERENCE,                      &
             B_JGS_TERMINATE_NORM,                            &
             B_JGS_LIMITER,                                   &
             B_JGS_BLOCK_GAUSS_SEIDEL,                        &
             B_JGS_USE_JACOBI,                                &
             B_JGS_MAXITER,                                   &
             B_JGS_PMIN,                                      &
             B_JGS_DIFF_THR,                                  &
             B_JGS_NORM_THR,                                  &
             B_JGS_NLEVEL,                                    &
             B_JGS_SOURCE_NONLINEAR
#ifdef W3_ASCII
        WRITE (NDSA,*)                                        &
             'FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,   &
             FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,    &
             DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,      &
             NTRI,COUNTOT, COUNTRI, NNZ,                      &
             B_JGS_TERMINATE_MAXITER,                         &
             B_JGS_TERMINATE_DIFFERENCE,                      &
             B_JGS_TERMINATE_NORM,                            &
             B_JGS_LIMITER,                                   &
             B_JGS_BLOCK_GAUSS_SEIDEL,                        &
             B_JGS_USE_JACOBI,                                &
             B_JGS_MAXITER,                                   &
             B_JGS_PMIN,                                      &
             B_JGS_DIFF_THR,                                  &
             B_JGS_NORM_THR,                                  &
             B_JGS_NLEVEL,                                    &
             B_JGS_SOURCE_NONLINEAR:',                        &
             FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,   &
             FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,    &
             DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,      &
             NTRI,COUNTOT, COUNTRI, NNZ,                      &
             B_JGS_TERMINATE_MAXITER,                         &
             B_JGS_TERMINATE_DIFFERENCE,                      &
             B_JGS_TERMINATE_NORM,                            &
             B_JGS_LIMITER,                                   &
             B_JGS_BLOCK_GAUSS_SEIDEL,                        &
             B_JGS_USE_JACOBI,                                &
             B_JGS_MAXITER,                                   &
             B_JGS_PMIN,                                      &
             B_JGS_DIFF_THR,                                  &
             B_JGS_NORM_THR,                                  &
             B_JGS_NLEVEL,                                    &
             B_JGS_SOURCE_NONLINEAR
#endif
        !Init COUNTCON and IOBDP to zero, it needs to be set somewhere or
        !removed
        COUNTCON=0
        IOBDP=0  
        WRITE (NDSM)                                          &
             X0, Y0, SX, SY, DXYMAX, XGRD, YGRD, TRIGP, TRIA, &
             LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,         &
             DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,     &
             POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI
#ifdef W3_ASCII
        WRITE (NDSA,*)                                          &
             'X0, Y0, SX, SY, DXYMAX, XGRD, YGRD, TRIGP, TRIA, &
             LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,         &
             DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,     &
             POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI:', &
             X0, Y0, SX, SY, DXYMAX, XGRD, YGRD, TRIGP, TRIA, &
             LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,         &
             DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,     &
             POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI
#endif
      END SELECT !GTYPE
      !
      WRITE (NDSM)                                            &
           ZB, MAPTMP, MAPFS, MAPSF, TRFLAG
#ifdef W3_ASCII
      WRITE (NDSA,*)                                          &
           'ZB, MAPTMP, MAPFS, MAPSF, TRFLAG:',               &
           ZB, MAPTMP, MAPFS, MAPSF, TRFLAG
#endif
      !
#ifdef W3_SMC
      IF( GTYPE .EQ. SMCTYPE ) THEN
        WRITE (NDSM)  NLvCel, NLvUFc, NLvVFc
        WRITE (NDSM)  IJKCel, IJKUFc, IJKVFc, ISMCBP
        WRITE (NDSM)  ICLBAC
        WRITE (NDSM)  ANGARC
        WRITE (NDSM)  CTRNX,  CTRNY,  CLATF
#ifdef W3_ASCII
        WRITE (NDSA,*)  'NLvCel, NLvUFc, NLvVFc:',            &
                        NLvCel, NLvUFc, NLvVFc
        WRITE (NDSA,*)  'IJKCel, IJKUFc, IJKVFc, ISMCBP:',    &
                        IJKCel, IJKUFc, IJKVFc, ISMCBP
        WRITE (NDSA,*)  'ICLBAC:',                            &
                        ICLBAC
        WRITE (NDSA,*)  'ANGARC:',                            &
                        ANGARC
        WRITE (NDSA,*)  'CTRNX,  CTRNY,  CLATF:',             &
                        CTRNX,  CTRNY,  CLATF
#endif
        IF ( FLTEST ) THEN
          WRITE (NDSE,"('  NRLv, MRFct and NBSMC values are',3I9)") NRLv, MRFct, NBSMC
          WRITE (NDSE,"('  IJKCel, IJKUFc, IJKVFc Write for',3I9)") NCel, NUFc, NVFc
          WRITE (NDSE,"('  CTRNXY transparency write for 2x', I9)") NCel
        ENDIF
      ENDIF
#endif
      !
      IF ( TRFLAG .NE. 0 ) WRITE (NDSM) TRNX, TRNY
#ifdef W3_ASCII
      IF ( TRFLAG .NE. 0 ) WRITE (NDSA,*) 'TRNX, TRNY:', TRNX, TRNY
#endif
      WRITE (NDSM)                     &
           DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,              &
           FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY, FLCTH, &
           FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS, CTHG0S,      &
           STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT, IICEDISP,    &
           ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, IICEHDISP,&
           IICEDDISP, IICEFDISP, BTBETA,                          &
           AAIRCMIN, AAIRGB
#ifdef W3_ASCII
      WRITE (NDSA,*)                   &
           'DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,             &
           FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY, FLCTH, &
           FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS, CTHG0S,      &
           STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT, IICEDISP,    &
           ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, IICEHDISP,&
           IICEDDISP, IICEFDISP, BTBETA,                          &
           AAIRCMIN, AAIRGB:',                                    &
           DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,              &
           FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY, FLCTH, &
           FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS, CTHG0S,      &
           STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT, IICEDISP,    &
           ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, IICEHDISP,&
           IICEDDISP, IICEFDISP, BTBETA,                          &
           AAIRCMIN, AAIRGB
#endif

      WRITE(NDSM)GRIDSHIFT
#ifdef W3_ASCII
      WRITE(NDSA,*)'GRIDSHIFT:',                                  &
                 GRIDSHIFT
#endif
#ifdef W3_SEC1
      WRITE (NDSM) NITERSEC1
#ifdef W3_ASCII
      WRITE (NDSA,*) 'NITERSEC1:',                                &
                   NITERSEC1
#endif
#endif
#ifdef W3_RTD
      !!  Add rotated Polat/lon and AnglD to mod_def   JGLi12Jun2012
      WRITE (NDSM) PoLat, PoLon, AnglD, FLAGUNR
#ifdef W3_ASCII
      WRITE (NDSA,*) 'PoLat, PoLon, AnglD, FLAGUNR:',             &
                   PoLat, PoLon, AnglD, FLAGUNR
#endif

#endif
      !!        WRITE(NDSM)                                                 &
      !!             COUG_2D, COUG_RAD3D, COUG_US3D
    ELSE
      call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 4')

      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                      &
           GTYPE, FLAGLL, ICLOSE
      !!Li      IF (.NOT.GINIT) CALL W3DIMX ( IGRD, NX, NY, NSEA, NDSE, NDST )
      IF (.NOT.GINIT) CALL W3DIMX ( IGRD, NX, NY, NSEA, NDSE, NDST &
#ifdef W3_SMC
           , NCel, NUFc, NVFc, NRLv, NBSMC                         &
           , NARC, NBAC, NSPEC                                     &
#endif
           )
      !
      ! Reads different kind of information depending on grid type
      !
      SELECT CASE ( GTYPE )
        !!Li  SMCTYPE shares info with RLGTYPE.   JGLi12Oct2020
      CASE ( RLGTYPE, SMCTYPE )
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
             SX, SY, X0, Y0
        DO IX=1,NX
          XGRD(:,IX) = REAL(X0 + REAL(IX-1)*SX)
        END DO
        DO IY=1,NY
          YGRD(IY,:) = REAL(Y0 + REAL(IY-1)*SY)
        END DO
      CASE ( CLGTYPE )
        ALLOCATE(XGRD4(NY,NX),YGRD4(NY,NX)); XGRD4 = 0.; YGRD4 = 0.
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR)               &
             XGRD4, YGRD4
        XGRD = XGRD4
        YGRD = YGRD4
        DEALLOCATE(XGRD4, YGRD4)
        !Set SX, SY, X0, Y0 to large values if curvilinear grid
        X0 = HUGE(X0); Y0 = HUGE(Y0)
        SX = HUGE(SX); SY = HUGE(SY)
      CASE (UNGTYPE)
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR)               &
             FSN, FSPSI,FSFCT,FSNIMP,FSTOTALIMP,FSTOTALEXP,   &
             FSBCCFL, FSREFRACTION, FSFREQSHIFT, FSSOURCE,    &
             DO_CHANGE_WLV, SOLVERTHR_STP, CRIT_DEP_STP,      &
             NTRI,COUNTOT, COUNTRI, NNZ,                      &
             B_JGS_TERMINATE_MAXITER,                         &
             B_JGS_TERMINATE_DIFFERENCE,                      &
             B_JGS_TERMINATE_NORM,                            &
             B_JGS_LIMITER,                                   &
             B_JGS_BLOCK_GAUSS_SEIDEL,                        &
             B_JGS_USE_JACOBI,                                &
             B_JGS_MAXITER,                                   &
             B_JGS_PMIN,                                      &
             B_JGS_DIFF_THR,                                  &
             B_JGS_NORM_THR,                                  &
             B_JGS_NLEVEL,                                    &
             B_JGS_SOURCE_NONLINEAR
        IF (.NOT. GUGINIT) THEN
          CALL W3DIMUG ( IGRD, NTRI, NX, COUNTOT, NNZ, NDSE, NDST )
        END IF
        call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 5')

        READ (NDSM,END=801,ERR=802,IOSTAT=IERR)               &
             X0, Y0, SX, SY, DXYMAX, XGRD, YGRD, TRIGP, TRIA, &
             LEN, IEN, ANGLE0, ANGLE, SI, MAXX, MAXY,         &
             DXYMAX, INDEX_CELL, CCON, COUNTCON, IE_CELL,     &
             POS_CELL, IOBP, IOBPA, IOBDP, IOBPD, IAA, JAA, POSI
        call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 6')

      END SELECT !GTYPE
      !
      IF (GTYPE.NE.UNGTYPE) CALL W3GNTX ( IGRD, NDSE, NDST )
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)   &
           ZB, MAPTMP, MAPFS, MAPSF, TRFLAG
      !
#ifdef W3_SMC
      IF( GTYPE .EQ. SMCTYPE ) THEN
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR) &
             NLvCel, NLvUFc, NLvVFc
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR) &
             IJKCel, IJKUFc, IJKVFc, ISMCBP
        DO J=lbound(IJKCel,2), ubound(IJKCel,2)
          IJKCel3(J) = IJKCel(3,J)
          IJKCel4(J) = IJKCel(4,J)
        END DO
        DO J=lbound(IJKVFc,2), ubound(IJKVFc,2)
          IJKVFc5(J) = IJKVFc(5,J)
          IJKVFc6(J) = IJKVFc(6,J)
        END DO
        DO J=lbound(IJKUFc,2), ubound(IJKUFc,2)
          IJKUFc5(J) = IJKUFc(5,J)
          IJKUFc6(J) = IJKUFc(6,J)
        END DO
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR) &
             ICLBAC
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR) &
             ANGARC
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR) &
             CTRNX,  CTRNY,  CLATF
      ENDIF
#endif
      !
      MAPSTA = MOD(MAPTMP+2,8) - 2
      MAPST2 = (MAPTMP-MAPSTA) / 8
      MAPSF(:,3) = MAPSF(:,2) + (MAPSF(:,1)-1)*NY
      IF ( TRFLAG .NE. 0 ) THEN
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR) TRNX, TRNY
      END IF
#ifdef W3_UOST
      ! UOST (Unresolved Obstacles Source Term) is enabled.
      ! setting TRNX, TRNY to null values
      TRNX = 1
      TRNY = 1
#endif

      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
           DTCFL, DTCFLI, DTMAX, DTMIN, DMIN, CTMAX,              &
           FICE0, FICEN, FICEL, PFMOVE, FLDRY, FLCX, FLCY,        &
           FLCTH, FLCK, FLSOU, FLBPI, FLBPO, CLATS, CLATIS,       &
           CTHG0S, STEXU, STEYU, STEDU, IICEHMIN, IICEHINIT,      &
           IICEDISP, ICESCALES(1:4), CALTYPE, CMPRTRCK, IICEHFAC, &
           IICEDDISP, IICEHDISP, IICEFDISP, BTBETA,               &
           AAIRCMIN, AAIRGB

      READ(NDSM,END=801,ERR=802,IOSTAT=IERR)GRIDSHIFT
#ifdef W3_SEC1
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) NITERSEC1
#endif
      !
#ifdef W3_RTD
      !!  Read rotated Polat/lon and AnglD from mod_def   JGLi12Jun2012
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) PoLat, PoLon, AnglD, FLAGUNR

#endif
      !
    END IF
    call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 7')
    !
#ifdef W3_T
    WRITE (NDST,9010) GTYPE, FLAGLL, ICLOSE, SX, SY, X0, Y0, TRFLAG
    WRITE (NDST,9011) 'MAPSTA'
    DO IY=MIN(NY,20), 1, -1
      WRITE (NDST,9012) (MAPSTA(IY,IX),IX=1,MIN(NX,30))
    END DO
    WRITE (NDST,9011) 'MAPST2'
    DO IY=MIN(NY,20), 1, -1
      WRITE (NDST,9012) (MAPST2(IY,IX),IX=1,MIN(NX,30))
    END DO
    WRITE (NDST,9011) 'MAPFS'
    DO IY=MIN(NY,20), 1, -1
      WRITE (NDST,9013) (MAPFS(IY,IX),IX=1,MIN(NX,12))
    END DO
    IF ( TRFLAG .NE. 0 ) THEN
      WRITE (NDST,9011) 'TRNX'
      DO IY=MIN(NY,20), 1, -1
        WRITE (NDST,9014) (TRNX(IY,IX),IX=1,MIN(NX,12))
      END DO
      WRITE (NDST,9011) 'TRNY'
      DO IY=MIN(NY,20), 1, -1
        WRITE (NDST,9014) (TRNY(IY,IX),IX=1,MIN(NX,12))
      END DO
    END IF
#endif
    !
    DEALLOCATE ( MAPTMP )
    !
#ifdef W3_T
    WRITE (NDST,9015) DTCFL, DTCFLI, DTMAX, DTMIN,               &
         DMIN, CTMAX, FICE0, FICEN, FICEL, PFMOVE,  &
         STEXU, STEYU, STEDU
    WRITE (NDST,9016) FLDRY, FLCX, FLCY, FLCTH, FLCK,            &
         FLSOU, FLBPI, FLBPO
    WRITE (NDST,9017) (CLATS(ISEA),ISEA=1,1),                    &
         (CLATIS(ISEA),ISEA=1,1), (CTHG0S(IY),ISEA=1,1)
#endif
    !
    ! Spectral parameters ------------------------------------------------ *
    !                                                 Module W3GDATMD SGRD
    !
    IF ( WRITE ) THEN
      WRITE (NDSM)                                                  &
           MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,        &
           XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,       &
           FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE
#ifdef W3_ASCII
      WRITE (NDSA,*)                                                &
           'MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,       &
           XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,       &
           FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE:', &
           MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,        &
           XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,       &
           FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE
#endif
    ELSE
      IF (.NOT.SINIT) CALL W3DIMS ( IGRD, NK, NTH, NDSE, NDST )
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                       &
           MAPWN, MAPTH, DTH, TH, ESIN, ECOS, ES2, ESC, EC2,        &
           XFR, FR1, SIG, SIG2, DSIP, DSII, DDEN, DDEN2, FTE,       &
           FTF, FTWN, FTTR, FTWL, FACTI1, FACTI2, FACHFA, FACHFE
    END IF

    !
#ifdef W3_T
    WRITE (NDST,9030) (MAPWN(I),I=1,8), (MAPTH(I),I=1,8), DTH*RADE, &
         (TH(I)*RADE,I=1,4), (ESIN(I),I=1,4), (ECOS(I),I=1,4),      &
         XFR, SIG(1)*TPIINV, SIG(NK)*TPIINV, FTE, FTF, FTWN, FTTR,  &
         FTWL, FACTI1, FACTI2, FACHFA, FACHFE
#endif
    !
    !
    ! Output flags for 3D parameters ------------------------------------- *
    !                                                 Module W3GDATMD
    IF ( WRITE ) THEN
      WRITE (NDSM)                                                &
           E3DF, P2MSF, US3DF,USSPF, USSP_WN
#ifdef W3_ASCII
      WRITE (NDSA,*)                                              &
           'E3DF, P2MSF, US3DF,USSPF, USSP_WN:',                  &
           E3DF, P2MSF, US3DF,USSPF, USSP_WN
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                     &
           E3DF, P2MSF, US3DF,USSPF, USSP_WN
    END IF

    IF ( INXOUT .EQ. 'GRID' ) THEN
      CLOSE (NDSM)
      RETURN
    END IF
    !
    ! Parameters for output boundary points ------------------------------ *
    !                                                 Module W3ODATMD OUT5
    !
    IF ( WRITE ) THEN
      WRITE (NDSM)                                               &
           XBPO, YBPO, RDBPO, IPBPO, ISBPO
#ifdef W3_ASCII
      WRITE (NDSA,*)                                              &
           'XBPO, YBPO, RDBPO, IPBPO, ISBPO:',                   &
           XBPO, YBPO, RDBPO, IPBPO, ISBPO
#endif
    ELSE
      CALL W3DMO5 ( IGRD, NDSE, NDST, 2 )
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                    &
           XBPO, YBPO, RDBPO, IPBPO, ISBPO
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9020)
    DO I=1, NFBPO
      WRITE (NDST,9021) I
      DO J=NBO(I-1)+1,NBO(I)
        WRITE (NDST,9022) J-NBO(I-1), (IPBPO(J,K),K=1,4),        &
             (RDBPO(J,K),K=1,4)
      END DO
      WRITE (NDST,9023) (ISBPO(J),J=NBO2(I-1)+1,NBO2(I))
    END DO
#endif
    !
    ! Parameters for spectral partitioning  ------------------------------ *
    !                                                 Module W3ODATMD OUT6
    !
    IF ( WRITE ) THEN
      WRITE (NDSM)                                               &
           IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,         &
           PTMETH, PTFCUT
#ifdef W3_ASCII
      WRITE (NDSA,*)                                             &
           'IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,        &
           PTMETH, PTFCUT:',                                     &
           IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,         &
           PTMETH, PTFCUT
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                    &
           IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL,         &
           PTMETH, PTFCUT
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9025) IHMAX, HSPMIN, WSMULT, WSCUT, FLCOMB, NOSWLL
#endif
    !
    ! Numerical parameters ----------------------------------------------- *
    !                                                 Module W3GDATMD NPAR
    !
    IF ( WRITE ) THEN
      WRITE (NDSM)                                               &
           FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
           FFACBERG, DELAB, FWTABLE
#ifdef W3_ASCII
      WRITE (NDSA,*)                                              &
           'FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
           FFACBERG, DELAB, FWTABLE:',                           &
           FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
           FFACBERG, DELAB, FWTABLE
#endif
#ifdef W3_RWND
      WRITE (NDSM)                                               &
           RWINDC
#ifdef W3_ASCII
      WRITE (NDSA,*)                                             &
           'RWINDC:',                                            &
           RWINDC
#endif
#endif
#ifdef W3_WCOR
      WRITE (NDSM)                                               &
           WWCOR
#ifdef W3_ASCII
      WRITE (NDSA,*)                                             &
           'WWCOR:',                                             &  
           WWCOR
#endif
#endif
#ifdef W3_REF1
      WRITE (NDSM)                                               &
           RREF, REFPARS, REFLC, REFLD
#ifdef W3_ASCII
      WRITE (NDSA,*)                                             &
           'RREF, REFPARS, REFLC, REFLD:',                       &
           RREF, REFPARS, REFLC, REFLD
#endif
#endif
#ifdef W3_IG1
      WRITE   (NDSM)                                             &
           IGPARS(1:12)
#ifdef W3_ASCII
      WRITE   (NDSA,*)                                           &
           'IGPARS(1:12):',                                      &
           IGPARS(1:12)
#endif
#endif
#ifdef W3_IC2
      WRITE   (NDSM)                                             &
           IC2PARS(1:8)
#ifdef W3_ASCII
      WRITE   (NDSA,*)                                           &
           'IC2PARS(1:8):',                                      &
           IC2PARS(1:8)
#endif
#endif
#ifdef W3_IC3
      WRITE   (NDSM)                                             &
           IC3PARS
#ifdef W3_ASCII
      WRITE   (NDSA,*)                                           &
           'IC3PARS:',                                           &
           IC3PARS
#endif
#endif
#ifdef W3_IC4
      WRITE   (NDSM)                                             &
           IC4PARS,IC4_KI,IC4_FC,IC4_CN,IC4_FMIN,IC4_KIBK
#ifdef W3_ASCII
      WRITE   (NDSA,*)                                           &
           'IC4PARS,IC4_KI,IC4_FC,IC4_CN,IC4_FMIN,IC4_KIBK:',    &
           IC4PARS,IC4_KI,IC4_FC,IC4_CN,IC4_FMIN,IC4_KIBK
#endif
#endif
#ifdef W3_IC5
      WRITE   (NDSM)                                             &
           IC5PARS
#ifdef W3_ASCII
      WRITE   (NDSA,*)                                           &
           'IC5PARS:',                                           &
           IC5PARS
#endif
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                    &
           FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC, FACSD, FHMAX, &
           FFACBERG, DELAB, FWTABLE
#ifdef W3_RWND
      READ  (NDSM,END=801,ERR=802,IOSTAT=IERR)                                               &
           RWINDC
#endif
#ifdef W3_WCOR
      READ  (NDSM,END=801,ERR=802,IOSTAT=IERR)                                               &
           WWCOR
#endif
#ifdef W3_REF1
      READ  (NDSM,END=801,ERR=802,IOSTAT=IERR)                                               &
           RREF, REFPARS, REFLC, REFLD
#endif
#ifdef W3_IG1
      READ   (NDSM,END=801,ERR=802,IOSTAT=IERR)                                              &
           IGPARS(1:12)
#endif
#ifdef W3_IC2
      READ   (NDSM,END=801,ERR=802,IOSTAT=IERR)                                              &
           IC2PARS(1:8)
#endif
#ifdef W3_IC3
      READ   (NDSM,END=801,ERR=802,IOSTAT=IERR)                                              &
           IC3PARS
#endif
#ifdef W3_IC4
      READ   (NDSM,END=801,ERR=802,IOSTAT=IERR)                                              &
           IC4PARS,IC4_KI,IC4_FC,IC4_CN,IC4_FMIN,IC4_KIBK
#endif
#ifdef W3_IC5
      READ   (NDSM,END=801,ERR=802,IOSTAT=IERR)                                              &
           IC5PARS
#endif
    END IF
    !
#ifdef W3_T
    WRITE (NDST,9040) FACP, XREL, XFLT, FXFM, FXPM, XFT, XFC,    &
         FACSD, FHMAX
#endif
    !
    ! Source term parameters --------------------------------------------- *
    !                                                 Module W3GDATMD SFLP
    !                                                 Module W3GDATMD SLNP
    !                                                 Module W3GDATMD SRCP
    !                                                 Module W3GDATMD SNLP
    !                                                 Module W3GDATMD SBTP
    !
#ifdef W3_FLX2
    IF ( WRITE ) THEN
      WRITE (NDSM)                            NITTIN, CINXSI
#ifdef W3_ASCII
      WRITE (NDSA,*)'                            NITTIN, CINXSI:', &
                                              NITTIN, CINXSI
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) NITTIN, CINXSI
    END IF
    IF ( FLTEST ) WRITE (NDST,9048) NITTIN, CINXSI
#endif
    !
#ifdef W3_FLX3
    IF ( WRITE ) THEN
      WRITE (NDSM)                                          &
           NITTIN, CINXSI, CD_MAX, CAP_ID
#ifdef W3_ASCII
      WRITE (NDSA,*)                                          &
           'NITTIN, CINXSI, CD_MAX, CAP_ID:',                &
           NITTIN, CINXSI, CD_MAX, CAP_ID
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)               &
           NITTIN, CINXSI, CD_MAX, CAP_ID
    END IF
    IF ( FLTEST ) WRITE (NDST,9048) NITTIN, CAP_ID, CINXSI, CD_MAX
#endif
    !
#ifdef W3_FLX4
    IF ( WRITE ) THEN
      WRITE (NDSM)                            FLX4A0
#ifdef W3_ASCII
      WRITE (NDSA,*)'                            FLX4A0:',   &
                                              FLX4A0
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) FLX4A0
    END IF
#endif
    !
    !
#ifdef W3_LN1
    IF ( WRITE ) THEN
      WRITE (NDSM)                            SLNC1, FSPM, FSHF
#ifdef W3_ASCII
      WRITE (NDSA,*)'                            SLNC1, FSPM, FSHF:', &
                                              SLNC1, FSPM, FSHF
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) SLNC1, FSPM, FSHF
    END IF
    IF ( FLTEST ) WRITE (NDST,9049) SLNC1, FSPM, FSHF
#endif
    !
#ifdef W3_ST1
    IF ( WRITE ) THEN
      WRITE (NDSM)                            SINC1, SDSC1
#ifdef W3_ASCII
      WRITE (NDSA,*)'                            SINC1, SDSC1:', &
                                              SINC1, SDSC1
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) SINC1, SDSC1
    END IF
    IF ( FLTEST ) WRITE (NDST,9050) SINC1, SDSC1
#endif
    !
#ifdef W3_ST2
    IF ( WRITE ) THEN
      WRITE (NDSM)                                             &
           ZWIND, FSWELL,                                      &
           SHSTAB, OFSTAB, CCNG, CCPS, FFNG, FFPS,             &
           CDSA0, CDSA1, CDSA2, SDSALN,                        &
           CDSB0, CDSB1, CDSB2, CDSB3, FPIMIN, XFH, XF1, XF2
#ifdef W3_ASCII
      WRITE (NDSA,*)                                           &
           'ZWIND, FSWELL,                                    &
           SHSTAB, OFSTAB, CCNG, CCPS, FFNG, FFPS,             &
           CDSA0, CDSA1, CDSA2, SDSALN,                        &
           CDSB0, CDSB1, CDSB2, CDSB3, FPIMIN, XFH, XF1, XF2:',&
           ZWIND, FSWELL,                                      &
           SHSTAB, OFSTAB, CCNG, CCPS, FFNG, FFPS,             &
           CDSA0, CDSA1, CDSA2, SDSALN,                        &
           CDSB0, CDSB1, CDSB2, CDSB3, FPIMIN, XFH, XF1, XF2
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                  &
           ZWIND, FSWELL,                                      &
           SHSTAB, OFSTAB, CCNG, CCPS, FFNG, FFPS,             &
           CDSA0, CDSA1, CDSA2, SDSALN,                        &
           CDSB0, CDSB1, CDSB2, CDSB3, FPIMIN, XFH, XF1, XF2
      IF ( .NOT. FLINP ) CALL INPTAB
      FLINP  = .TRUE.
    END IF
    IF ( FLTEST ) WRITE (NDST,9050)                            &
         ZWIND, FSWELL, CDSA0, CDSA1, CDSA2,                   &
         SDSALN, CDSB0, CDSB1, CDSB2, CDSB3, FPIMIN, XFH, XF1, &
         XF2, SHSTAB, OFSTAB, CCNG, CCPS, FFNG, FFPS
#endif
    !
#ifdef W3_ST3
    IF ( WRITE ) THEN
      WRITE (NDSM)                                             &
           ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,       &
           SSWELLF, SSDSC1, WWNMEANP, WWNMEANPTAIL, SSTXFTF,   &
           SSTXFTFTAIL, SSTXFTWN,                              &
           DDELTA1, DDELTA2, SSTXFTF, SSTXFTWN,                &
           FFXPM, FFXFM
#ifdef W3_ASCII
      WRITE (NDSA,*)                                           &
           'ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,      &
           SSWELLF, SSDSC1, WWNMEANP, WWNMEANPTAIL, SSTXFTF,   &
           SSTXFTFTAIL, SSTXFTWN,                              &
           DDELTA1, DDELTA2, SSTXFTF, SSTXFTWN,                &
           FFXPM, FFXFM:',                                     &
           ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,       &
           SSWELLF, SSDSC1, WWNMEANP, WWNMEANPTAIL, SSTXFTF,   &
           SSTXFTFTAIL, SSTXFTWN,                              &
           DDELTA1, DDELTA2, SSTXFTF, SSTXFTWN,                &
           FFXPM, FFXFM
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                  &
           ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,       &
           SSWELLF, SSDSC1, WWNMEANP, WWNMEANPTAIL, SSTXFTF,   &
           SSTXFTFTAIL, SSTXFTWN,                              &
           DDELTA1, DDELTA2, SSTXFTF, SSTXFTWN,                &
           FFXPM, FFXFM
      IF ( .NOT. FLINP ) THEN
        CALL INSIN3
        FLINP  = .TRUE.
      END IF
    END IF
#endif
    !
#ifdef W3_ST4
    IF ( WRITE ) THEN
      CALL INSIN4(.TRUE.)
      WRITE (NDSM)                                           &
           ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,     &
           TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,        &
           ZZ0RAT, SSDSC,                                    &
           SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,           &
           SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF, &
           SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,         &
           SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,   &
           SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,           &
           SSDSHCK,                                          &
           IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,         &
           DIKCUMUL, CUMULW, SINTAILPAR, CAPCHNK
#ifdef W3_ASCII
      WRITE (NDSA,*)                                         &
           'ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,    &
           TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,        &
           ZZ0RAT, SSDSC,                                    &
           SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,           &
           SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF, &
           SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,         &
           SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,   &
           SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,           &
           SSDSHCK,                                          &
           IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,         &
           DIKCUMUL, CUMULW, SINTAILPAR, CAPCHNK:',          &
           ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,     &
           TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,        &
           ZZ0RAT, SSDSC,                                    &
           SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,           &
           SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF, &
           SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,         &
           SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,   &
           SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,           &
           SSDSHCK,                                          &
           IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,         &
           DIKCUMUL, CUMULW, SINTAILPAR, CAPCHNK
#endif
      IF (SINTAILPAR(1).GT.0.5) THEN
        WRITE (NDSM) DELUST, DELTAIL, DELTAUW, DELU, DELALP, &
                     TAUT, TAUHFT
        IF (TTAUWSHELTER.GT.0) WRITE (NDSM) TAUHFT2
#ifdef W3_ASCII
        WRITE (NDSA,*) 'DELUST, DELTAIL, DELTAUW, DELU, DELALP,&
                     TAUT, TAUHFT:',                         &
                     DELUST, DELTAIL, DELTAUW, DELU, DELALP, &
                     TAUT, TAUHFT
        IF (TTAUWSHELTER.GT.0) WRITE (NDSA,*) 'TAUHFT2:', TAUHFT2 
#endif
      END IF
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
           ZZWND, AALPHA, ZZ0MAX, BBETA, SSINTHP, ZZALP,     &
           TTAUWSHELTER, SSWELLFPAR, SSWELLF, SSINBR,        &
           ZZ0RAT, SSDSC,                                    &
           SSDSISO, SSDSBR, SSDSBT, SSDSBM, SSDSP,           &
           SSDSCOS, SSDSDTH, WWNMEANP, WWNMEANPTAIL,SSTXFTF, &
           SSTXFTFTAIL, SSTXFTWN, SSTXFTF, SSTXFTWN,         &
           SSDSBRF1, SSDSBRF2, SSDSBRFDF,SSDSBCK, SSDSABK,   &
           SSDSPBK, SSDSBINT, FFXPM, FFXFM, FFXFA,           &
           SSDSHCK,                                          &
           IKTAB, DCKI, QBI, SATINDICES, SATWEIGHTS,         &
           DIKCUMUL, CUMULW, SINTAILPAR, CAPCHNK
      IF (SINTAILPAR(1).GT.0.5) THEN
        CALL INSIN4(.FALSE.)
        READ (NDSM,END=801,ERR=802,IOSTAT=IERR)              &
             DELUST, DELTAIL, DELTAUW, DELU, DELALP,  &
             TAUT, TAUHFT
        IF (TTAUWSHELTER.GT.0) READ(NDSM,END=801,ERR=802,IOSTAT=IERR) TAUHFT2
      END IF
    END IF
#endif
    !
#ifdef W3_ST6
    IF ( WRITE ) THEN
      WRITE (NDSM) SIN6A0, SDS6ET, SDS6A1, SDS6A2,           &
           SDS6P1, SDS6P2, SWL6S6, SWL6B1, SWL6CSTB1,        &
           SIN6WS, SIN6FC
#ifdef W3_ASCII
      WRITE (NDSA,*) 'SIN6A0, SDS6ET, SDS6A1, SDS6A2,        &
           SDS6P1, SDS6P2, SWL6S6, SWL6B1, SWL6CSTB1,        &
           SIN6WS, SIN6FC:',                                 &
                   SIN6A0, SDS6ET, SDS6A1, SDS6A2,           &
           SDS6P1, SDS6P2, SWL6S6, SWL6B1, SWL6CSTB1,        &
           SIN6WS, SIN6FC
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
           SIN6A0, SDS6ET, SDS6A1, SDS6A2,                   &
           SDS6P1, SDS6P2, SWL6S6, SWL6B1, SWL6CSTB1,        &
           SIN6WS, SIN6FC
    END IF
#endif
    !
    ! ... Nonlinear interactions
    !
#ifdef W3_NL1
    IF ( WRITE ) THEN
      WRITE (NDSM)                                         &
           SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3,   &
           IQTPE, NLTAIL, GQNF1, GQNT1,                    &
           GQNQ_OM2, GQTHRSAT, GQTHRCOU, GQAMP
#ifdef W3_ASCII
      WRITE (NDSA,*)                                       &
           'SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3,  &
           IQTPE, NLTAIL, GQNF1, GQNT1,                    &
           GQNQ_OM2, GQTHRSAT, GQTHRCOU, GQAMP:',          &
           SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3,   &
           IQTPE, NLTAIL, GQNF1, GQNT1,                    &
           GQNQ_OM2, GQTHRSAT, GQTHRCOU, GQAMP
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)              &
           SNLC1, LAM, KDCON, KDMN, SNLS1, SNLS2, SNLS3,   &
           IQTPE, NLTAIL, GQNF1, GQNT1,                    &
           GQNQ_OM2, GQTHRSAT, GQTHRCOU, GQAMP
    END IF
    IF ( FLTEST ) WRITE (NDST,9051) SNLC1, LAM,            &
         KDCON, KDMN, SNLS1, SNLS2, SNLS3,                 &
         IQTPE, NLTAIL, GQNF1, GQNT1, GQNQ_OM2,            &
         GQTHRSAT, GQTHRCOU, GQAMP
#endif
    !
#ifdef W3_NL2
    IF ( WRITE ) THEN
      WRITE (NDSM) IQTPE, NLTAIL, NDPTHS
      WRITE (NDSM) DPTHNL
#ifdef W3_ASCII
      WRITE (NDSA,*) 'IQTPE, NLTAIL, NDPTHS:',             &
                   IQTPE, NLTAIL, NDPTHS
      WRITE (NDSA,*) 'DPTHNL:',                            &
                   DPTHNL
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)              &
           IQTPE, NLTAIL, NDPTHS
      ALLOCATE ( MPARS(IGRD)%SNLPS%DPTHNL(NDPTHS) )
      DPTHNL => MPARS(IGRD)%SNLPS%DPTHNL
      PINIT  = .TRUE.
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) DPTHNL
    END IF
    IF ( FLTEST ) WRITE (NDST,9051) IQTPE, NLTAIL, NDPTHS
    IF ( FLTEST ) WRITE (NDST,9151) DPTHNL
#endif
    !
#ifdef W3_NL3
    IF ( WRITE ) THEN
      WRITE (NDSM) SNLNQ, SNLMSC, SNLNSC, SNLSFD, SNLSFS
      WRITE (NDSM) SNLL(1:SNLNQ), SNLM(1:SNLNQ),           &
           SNLT(1:SNLNQ), SNLCD(1:SNLNQ),                  &
           SNLCS(1:SNLNQ)
#ifdef W3_ASCII
      WRITE (NDSA,*) 'SNLNQ, SNLMSC, SNLNSC, SNLSFD, SNLSFS:',&
                   SNLNQ, SNLMSC, SNLNSC, SNLSFD, SNLSFS
      WRITE (NDSA,*) 'SNLL(1:SNLNQ), SNLM(1:SNLNQ),           &
           SNLT(1:SNLNQ), SNLCD(1:SNLNQ),                  &
           SNLCS(1:SNLNQ):',                               &
                   SNLL(1:SNLNQ), SNLM(1:SNLNQ),           &
           SNLT(1:SNLNQ), SNLCD(1:SNLNQ),                  &
           SNLCS(1:SNLNQ)
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)              &
           SNLNQ, SNLMSC, SNLNSC, SNLSFD, SNLSFS
      ALLOCATE ( MPARS(IGRD)%SNLPS%SNLL(SNLNQ),            &
           MPARS(IGRD)%SNLPS%SNLM(SNLNQ),                  &
           MPARS(IGRD)%SNLPS%SNLT(SNLNQ),                  &
           MPARS(IGRD)%SNLPS%SNLCD(SNLNQ),                 &
           MPARS(IGRD)%SNLPS%SNLCS(SNLNQ) )
      SNLL   => MPARS(IGRD)%SNLPS%SNLL
      SNLM   => MPARS(IGRD)%SNLPS%SNLM
      SNLT   => MPARS(IGRD)%SNLPS%SNLT
      SNLCD  => MPARS(IGRD)%SNLPS%SNLCD
      SNLCS  => MPARS(IGRD)%SNLPS%SNLCS
      PINIT  = .TRUE.
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)              &
           SNLL, SNLM, SNLT, SNLCD, SNLCS
    END IF
    IF ( FLTEST ) WRITE (NDST,9051) SNLNQ, SNLMSC, SNLNSC, &
         SNLSFD, SNLSFS
    IF ( FLTEST ) THEN
      DO I=1, SNLNQ
        WRITE (NDST,9151) SNLL(I), SNLM(I), SNLT(I),       &
             SNLCD(I), SNLCS(I)
      END DO
    END IF
#endif
    !
#ifdef W3_NL4
    IF ( WRITE ) THEN
      WRITE (NDSM) ITSA, IALT
#ifdef W3_ASCII
      WRITE (NDSA,*) 'ITSA, IALT:',                        &
                   ITSA, IALT
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)              &
           ITSA, IALT
    END IF
    IF ( FLTEST ) WRITE (NDST,9051) ITSA, IALT
#endif
    !
    ! (QL: INXOUT = Grid option ?)
#ifdef W3_NL5
    IF (WRITE) THEN
      CALL INSNL5
      WRITE (NDSM) QR5DPT, QR5OML, QI5DIS, QI5KEV,          &
           QI5NNZ, QI5IPL, QI5PMX
#ifdef W3_ASCII
      WRITE (NDSA,*) 'QR5DPT, QR5OML, QI5DIS, QI5KEV,       &
           QI5NNZ, QI5IPL, QI5PMX:',                        &
                   QR5DPT, QR5OML, QI5DIS, QI5KEV,          &
           QI5NNZ, QI5IPL, QI5PMX
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)               &
           QR5DPT, QR5OML, QI5DIS, QI5KEV,                  &
           QI5NNZ, QI5IPL, QI5PMX
    END IF
    IF ( FLTEST ) WRITE (NDST,9051) QR5DPT, QR5OML, QI5DIS, &
         QI5KEV, QI5NNZ, QI5IPL,                            &
         QI5PMX
#endif
    !
#ifdef W3_NLS
    IF ( WRITE ) THEN
      WRITE (NDSM)                                          &
           CNLSA, CNLSC, CNLSFM, CNLSC1, CNLSC2, CNLSC3
#ifdef W3_ASCII
      WRITE (NDSA,*)                                        &
           'CNLSA, CNLSC, CNLSFM, CNLSC1, CNLSC2, CNLSC3:', &
           CNLSA, CNLSC, CNLSFM, CNLSC1, CNLSC2, CNLSC3
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)               &
           CNLSA, CNLSC, CNLSFM, CNLSC1, CNLSC2, CNLSC3
    END IF
    IF ( FLTEST ) WRITE (NDST,9251)                         &
         CNLSA, CNLSC, CNLSFM, CNLSC1, CNLSC2, CNLSC3
#endif
    !
#ifdef W3_NL1
    IF ( .NOT. WRITE ) THEN
      IF (IQTPE.GT.0) THEN
        CALL INSNL1 ( IGRD )
      ELSE
        CALL INSNLGQM
      END IF
    END IF
#endif
#ifdef W3_NL3
    IF ( .NOT. WRITE ) CALL INSNL3
#endif
#ifdef W3_NLS
    IF ( .NOT. WRITE ) CALL INSNLS
#endif
    !
    ! Layered barriers needed for file management in xnl_init
    !
#ifdef W3_MPI
    IF ( FLSNL2 .AND. .NOT.WRITE ) THEN
      DO IP=1, IAPROC-1
        CALL MPI_BARRIER (  MPI_COMM_WAVE, IERR_MPI )
      END DO
    END IF
#endif
#ifdef W3_NL2
    IF ( .NOT. WRITE ) CALL INSNL2
#endif
#ifdef W3_MPI
    IF ( FLSNL2 .AND. .NOT.WRITE ) THEN
      DO IP=IAPROC, NAPROC-1
        CALL MPI_BARRIER (  MPI_COMM_WAVE, IERR_MPI )
      END DO
    END IF
#endif
    !
    ! ... Bottom friction ...
    !
#ifdef W3_BT1
    IF ( WRITE ) THEN
      WRITE (NDSM)                            SBTC1
#ifdef W3_ASCII
      WRITE (NDSA,*) 'SBTC1:',                SBTC1
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) SBTC1
    END IF
    IF ( FLTEST ) WRITE (NDST,9052) SBTC1
#endif
    !
    !
#ifdef W3_BT4
    IF ( WRITE ) THEN
      WRITE (NDSM)                                          &
           SBTCX, SED_D50, SED_PSIC
#ifdef W3_ASCII
      WRITE (NDSA,*)                                        &
           'SBTCX, SED_D50, SED_PSIC:',                     &
           SBTCX, SED_D50, SED_PSIC
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)               &
           SBTCX, SED_D50, SED_PSIC
    END IF
#endif
    !
    ! ... Depth induced breaking ...
    !
    call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 8')
#ifdef W3_DB1
    IF ( WRITE ) THEN
      WRITE (NDSM)                                            &
           SDBC1, SDBC2, FDONLY
#ifdef W3_ASCII
      WRITE (NDSA,*)                                          &
           'SDBC1, SDBC2, FDONLY:',                           &
           SDBC1, SDBC2, FDONLY
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
           SDBC1, SDBC2, FDONLY
    END IF
    !
    IF ( FLTEST ) WRITE (NDST,9053) SDBC1, SDBC2, FDONLY
#endif

#ifdef W3_UOST
    IF ( WRITE ) THEN
      WRITE (NDSM) UOSTFILELOCAL, UOSTFILESHADOW,             &
           UOSTFACTORLOCAL, UOSTFACTORSHADOW
#ifdef W3_ASCII
      WRITE (NDSA,*) 'UOSTFILELOCAL, UOSTFILESHADOW,          &
           UOSTFACTORLOCAL, UOSTFACTORSHADOW:',               &
                     UOSTFILELOCAL, UOSTFILESHADOW,           &
           UOSTFACTORLOCAL, UOSTFACTORSHADOW
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                 &
           UOSTFILELOCAL, UOSTFILESHADOW,                     &
           UOSTFACTORLOCAL, UOSTFACTORSHADOW
      CALL UOST_INITGRID(IGRD, UOSTFILELOCAL, UOSTFILESHADOW, &
           UOSTFACTORLOCAL, UOSTFACTORSHADOW)
#endif

#ifdef W3_UOST
    END IF
#endif

    !
#ifdef W3_IS1
    IF ( WRITE ) THEN
      WRITE (NDSM)                            IS1C1, IS1C2
#ifdef W3_ASCII
      WRITE (NDSA,*) 'IS1C1, IS1C2:',         IS1C1, IS1C2
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) IS1C1, IS1C2
    END IF
#endif
    !
#ifdef W3_IS2
    IF ( WRITE ) THEN
      WRITE (NDSM)                            IS2PARS
#ifdef W3_ASCII
      WRITE (NDSA,*) 'IS3PARS:',              IS2PARS
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) IS2PARS
      IF ( .NOT. FLIS ) THEN
        CALL INSIS2
        FLIS  = .TRUE.
      END IF
    END IF
#endif
    !
    ! Propagation scheme ------------------------------------------------- *
    !                                                 Module W3GDATMD PROP
    !
#ifdef W3_PR2
    IF ( WRITE ) THEN
      WRITE (NDSM) DTME, CLATMN
#ifdef W3_ASCII
      WRITE (NDSA,*) 'DTME, CLATMN:', DTME, CLATMN
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
           DTME, CLATMN
    END IF
    !
    IF ( FLTEST ) WRITE (NDST,9060) DTME, CLATMN
#endif
    !
#ifdef W3_PR3
    IF ( WRITE ) THEN
      WRITE (NDSM) WDCG, WDTH
#ifdef W3_ASCII
      WRITE (NDSA,*) 'WDCG, WDTH:', WDCG, WDTH
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
           WDCG, WDTH
    END IF
    !
    IF ( FLTEST ) WRITE (NDST,9060) WDCG, WDTH
#endif
    !
#ifdef W3_SMC
    IF ( WRITE ) THEN
      WRITE(NDSM) DTMS, Refran, FUNO3, FVERG, FSWND, ARCTC
#ifdef W3_ASCII
      WRITE(NDSA,*) 'DTMS, Refran, FUNO3, FVERG, FSWND, ARCTC:', &
                  DTMS, Refran, FUNO3, FVERG, FSWND, ARCTC
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR)                &
           DTMS, Refran, FUNO3, FVERG, FSWND, ARCTC
    END IF
    !
    IF ( FLTEST ) WRITE (NDST,9260) DTMS, Refran
#endif
    !
#ifdef W3_FLD1
    IF ( WRITE ) THEN
      WRITE (NDSM)  TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#ifdef W3_ASCII
      WRITE (NDSA,*)  'TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2:', &
                    TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) &
           TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
    END IF
#endif
#ifdef W3_FLD2
    IF ( WRITE ) THEN
      WRITE (NDSM) TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#ifdef W3_ASCII
      WRITE (NDSA,*) 'TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2:', &
                   TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
#endif
    ELSE
      READ (NDSM,END=801,ERR=802,IOSTAT=IERR) &
           TAIL_ID, TAIL_LEV, TAIL_TRAN1, TAIL_TRAN2
    END IF
#endif
    !
    ! Interpolation tables ( fill locally ) ----------------------------- *
    !                                                      Module W3DISPMD
    !
    IF ( .NOT.WRITE .AND. .NOT.FLDISP ) THEN
#ifdef W3_T
      WRITE (NDST,9070)
#endif
      CALL DISTAB
      FLDISP = .TRUE.
    END IF
    !
    CLOSE ( NDSM )
#ifdef W3_ASCII
    IF ( WRITE ) THEN
      CLOSE ( NDSA )
    END IF
#endif
    call print_memcheck(memunit, 'memcheck_____:'//' WIOGR SECTION 9')
    !
    RETURN
    !
    ! Escape locations read errors --------------------------------------- *
    !
800 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1000) FILEXT(:IEXT), IERR
    CALL EXTCDE ( 50 )
    !
801 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001) FILEXT(:IEXT)
    CALL EXTCDE ( 51 )
    !
802 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) FILEXT(:IEXT), IERR, &
         MESSAGE
    CALL EXTCDE ( 52 )
    !
    ! Formats
    !
900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     ILEGAL INXOUT VALUE: ',A/)
901 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     ILEGAL IDSTR, READ : ',A/                     &
         '                  CHECK : ',A/)
902 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     ILEGAL VERGRD, READ : ',A/                    &
         '                   CHECK : ',A/)
904 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     ILEGAL NFBPO READ : ',I8/                     &
         '                 CHECK : ',I8/)
905 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     UNEXPECTED SOURCE TERM IDENTIFIER',I2/        &
         '          IN mod_def.',A,' FILE : ',A/             &
         '    EXPECTED FROM switch FILE : ',A,/              &
         5(A,/) /)
    !               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
906 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     UNEXPECTED PROPAGATION SCHEME IDENTIFIER'/    &
         '                IN FILE :',A/                      &
         '               EXPECTED :',A/                      &
         '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
907 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     UNEXPECTED GSE ALEVIATION IDENTIFIER'/        &
         '                IN FILE :',A/                      &
         '               EXPECTED :',A/                      &
         , 5(A,/) /)
    !               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
908 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR :'/         &
         '     UNEXPECTED FLUX PARAMETERIZATION IDENTIFIER'/ &
         '         IN mod_def.',A,' :',A/                    &
         '               EXPECTED :',A/                      &
         , 5(A,/) /)
    !               '     CHECK CONSISTENCY OF SWITCHES IN PROGRAMS'/)
    !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/       &
         '     ERROR IN OPENING mod_def.',A,' FILE'/         &
         '     IOSTAT =',I5/)
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/       &
         '     PREMATURE END OF mod_def.',A,' FILE'/)
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOGR : '/,      &
         '     ERROR IN READING FROM mod_def.',A,' FILE'/    &
         '     IOSTAT =',I5,                                 &
         5(A,/) /)
    !
#ifdef W3_T
9000 FORMAT (' TEST W3IOGR : INXOUT = ',A,', WRITE = ',L1,   &
         ', UNIT =',I3,', IGRD =',I3,', FEXT = ',A)
9001 FORMAT (' TEST W3IOGR : TEST PARAMETERS :'/             &
         '       IDSTR : ',A/                                &
         '      VERGRD : ',A/                                &
         '    NX/Y/SEA : ',3I10/                             &
         '      NTH,NK : ',2I10/                             &
         '         NBI : ',I10/                              &
         '       NFBPO : ',2I10/                             &
         '      GNAME  : ',A/                                &
         '      FNAME0 : ',A/                                &
         '      FNAME1 : ',A/                                &
         '      FNAME2 : ',A/                                &
         '      FNAME3 : ',A/                                &
         '      FNAME4 : ',A/                                &
         '      FNAME5 : ',A/                                &
         '      FNAME6 : ',A/                                &
         '      FNAMEP : ',A/                                &
         '      FNAMEG : ',A/                                &
         '      FNAMEF : ',A/                                &
         '      FNAMEI : ',A)
9002 FORMAT ('      NBO    : ',10I5)
9003 FORMAT ('      NBO2   : ',10I5)
    !
9010 FORMAT (' TEST W3IOGR : MODULE W3GDATMD GRID'/          &
         '      GTYPE  : ',I9/                               &
         '      FLAGLL : ',L9/                               &
         '      ICLOSE : ',I9/                               &
         '      SX, SY : ',2E10.3/                           &
         '      X0, Y0 : ',2E10.3/                           &
         '      TRFLAG : ',I9)
9011 FORMAT ('      LOWER LEFT PART OF ',A)
9012 FORMAT ('     ',4X,30I2)
9013 FORMAT ('     ',12I6)
9014 FORMAT ('     ',12F6.2)
9015 FORMAT ('      STEPS  : ',4F8.1/                        &
         '      DEPTH  : ',F8.1,F10.3/                       &
         '      FICE0/N: ',F9.2,F8.2/                        &
         '      FICEL  : ',F9.1 /                            &
         '      PFMOVE : ',F9.2 /                            &
         '      STEXU  : ',F9.2 /                            &
         '      STEYU  : ',F9.2 /                            &
         '      STEDU  : ',F9.2)
    !
9016 FORMAT ('      FLAGS  : ',8L2)
9017 FORMAT ('      CLATS   : ',3F8.3,' ...'/                &
         '      CLATIS  : ',3F8.3,' ...'/                    &
         '      CTHG0S  : ',3E11.3,' ...')
    !
9020 FORMAT (' TEST W3IOGR : MODULE W3ODATMD OUT5')
9021 FORMAT ('      INTERPOLATION DATA : FILE ',I1)
9022 FORMAT ('          ',I5,2X,4I4,2X,4F5.2)
9023 FORMAT ('          ',10I7)
9025 FORMAT (' TEST W3IOGR : MODULE W3ODATMD OUT6'/          &
         '      PARTITIONING DATA :',I5,3E10.3,L4,2X,I4)
    !
9030 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SGRD'/          &
         '      MAPWN  : ',8I4,' ...'/                       &
         '      MAPTH  : ',8I4,' ...'/                       &
         '      DTH    : ',F6.1/                             &
         '      TH     : ',4F6.1,' ...'/                     &
         '      ESIN   : ',4F6.3,' ...'/                     &
         '      ECOS   : ',4F6.3,' ...'/                     &
         '      XFR    : ',F6.3/                             &
         '      FR     : ',F6.3,' ...',F6.3/                 &
         '      FACs   : ',6E10.3/                           &
         '               ',3E10.3)
    !
9040 FORMAT (' TEST W3IOGR : MODULE W3GDATMD NPAR'/          &
         '      FACs   : ',5E10.3/                           &
         '               ',4E10.3)
#endif
    !
#ifdef W3_FLX2
9048 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SFLP'/          &
         '      FLUXES : ',I5,3X,E10.3)
#endif
#ifdef W3_FLX3
9048 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SFLP'/          &
         '      FLUXES : ',2I5,3X,2E10.3)
#endif
    !
#ifdef W3_LN1
9049 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SLNP'/          &
         '      INPUT  : ',3E10.3)
#endif
    !
#ifdef W3_ST1
9050 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SRCP'/          &
         '      INPUT  : ',E10.3/                            &
         '      DISSIP : ',E10.3)
#endif
#ifdef W3_ST2
9050 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SRCP'/          &
         '      INPUT  : ',2E10.3/                           &
         '      DISSIP : ',4E10.3/                           &
         '               ',5E10.3/                           &
         '               ',3E10.3/                           &
         '      STAB2  : ',6E10.3)
#endif
    !
#ifdef W3_NL1
9051 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP'/          &
         '      DATA   : ',2E10.3/                           &
         '               ',5E10.3)
#endif
    !
#ifdef W3_NL2
9051 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP'/          &
         '      DATA   : ',I4,F5.1,I4)
9151 FORMAT ('               ',5F7.1)
#endif
    !
#ifdef W3_NL3
9051 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP'/          &
         '      DATA   : ',I4,4F8.3)
9151 FORMAT ('               ',2F8.3,F6.1,2E12.4)
#endif
    !
#ifdef W3_NL4
9051 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP'/          &
         '      DATA   : ',I4,I4)
#endif
    !
#ifdef W3_NL5
9051 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP'/          &
         '      DATA   : ', F7.1, F8.2, 2I2.1, I12, 2I2.1)
#endif
    !
#ifdef W3_NLS
9251 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SNLP (NLS)'/    &
         '      DATA   : ',F8.3,E12.4,4F8.3)
#endif
    !
#ifdef W3_BT1
9052 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SBTP'/          &
         '      DATA   : ',E10.3)
#endif
    !
#ifdef W3_DB1
9053 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SDBP'/          &
         '      DATA   : ',2E10.3,L4)
#endif
    !
#ifdef W3_PR2
9060 FORMAT (' TEST W3IOGR : MODULE W3GDATMD PROP'/          &
         '      DATA   : ',2E10.3)
#endif
    !
#ifdef W3_PR3
9060 FORMAT (' TEST W3IOGR : MODULE W3GDATMD PROP'/          &
         '      DATA   : ',2F6.2)
#endif
    !
#ifdef W3_SMC
9260 FORMAT (' TEST W3IOGR : MODULE W3GDATMD SMCG'/          &
         '      DATA   : ',3E10.3)
#endif
    !
#ifdef W3_T
9070 FORMAT (' TEST W3IOGR : DISPERSION INTEPOLATION TABLES')
#endif
    !/
    !/ End of W3IOGR ----------------------------------------------------- /
    !/
  END SUBROUTINE W3IOGR
  !/
  !/ End of module W3IOGRMD -------------------------------------------- /
  !/
END MODULE W3IOGRMD
