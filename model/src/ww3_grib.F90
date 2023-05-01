!> @file
!> @brief Contains program W3GRIB for post-processing grid output.
!>
!> @author H. L. Tolman
!> @author A. Chawla
!> @author J.-H. Alves
!> @date 22-Mar-2021
!
#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!> @brief Post-processing of grid output.
!>
!> @details Data is read from the grid output file out_grd.ww3 (raw data)
!>  and from the file ww3_grib.inp ( NDSI, output requests ).
!>  Model definition and raw data files are read using WAVEWATCH III
!>  subroutines.
!>  GRIB packing is performed using NCEP's W3 library (not supplied).
!>
!>  When adding new parameters to GRIB packing, keep in mind that
!>  packing is done differently for scalar and vector quantities
!>
!> @author H. L. Tolman
!> @author A. Chawla
!> @author J.-H. Alves
!> @date 22-Mar-2021
!
PROGRAM W3GRIB
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |            A. Chawla              |
  !/                  |           J.-H. Alves             |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         22-Mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/    01-Nov-1999 : Final FORTRAN 77        ( version 1.18 + error fix )
  !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  !/    25-Jan-2001 : Flat grid error exit added          ( version 2.06 )
  !/    29-Apr-2002 : Adding output fields 17-18.         ( version 2.20 )
  !/    08-May-2002 : Replace XLF switch with NCEP1.      ( version 2.21 )
  !/    13-Nov-2002 : Add stress vector.                  ( version 3.00 )
  !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  !/    20-Jul-2005 : Additional output parameters.       ( version 3.07 )
  !/    11-Apr-2007 : Additional output parameters.       ( version 3.11 )
  !/    18-May-2007 : Update GRIB1 for partitioning.      ( version 3.11 )
  !/    16-Jul-2007 : Adding GRIB2 capability.            ( version 3.11 )
  !/                  (A. Chawla)
  !/    01-Aug-2007 : Update FLGRIB for GRIB2.            ( version 3.11 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  !/                  (W. E. Rogers & T. J. Campbell, NRL)
  !/    05-Oct-2011 : Updating to the 53 output parameter ( version 4.05 )
  !/                  (Arun Chawla)
  !/    01-Mar-2013 : Adding double-index output fields   ( version 4.11 )
  !/                  (J-Henrique Alves)
  !/    01-Dec-2016 : Adding lambert conformal grid       ( version 6.01 )
  !/                  (J.H. Alves)
  !/    26-Jul-2018 : Adding polar stereographic grid     ( version 6.05 )
  !/                  (J.H. Alves)
  !/    22-Mar-2021 : New coupling fields output          ( version 7.13 )
  !/    09-Jun-2021 : remove grib1 support (NCEP1)        ( version 7.14 )
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Post-processing of grid output.
  !
  !  2. Method :
  !
  !     Data is read from the grid output file out_grd.ww3 (raw data)
  !     and from the file ww3_grib.inp ( NDSI, output requests ).
  !     Model definition and raw data files are read using WAVEWATCH III
  !     subroutines.
  !     GRIB packing is performed using NCEP's W3 library (not supplied).
  !
  !     When adding new parameters to GRIB packing, keep in mind that
  !     packing is done differently for scalar and vector quantities
  !
  !  3. Parameters :
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3NMOD    Subr. W3GDATMD Set number of model.
  !      W3SETG    Subr.   Id.    Point to selected model.
  !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
  !      W3SETW    Subr.   Id.    Point to selected model for wave data.
  !      W3NAUX    Subr. W3ADATMD Set number of model for aux data.
  !      W3SETA    Subr.   Id.    Point to selected model for aux data.
  !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  !      STRACE    Subr.   Id.    Subroutine tracing.
  !      NEXTLN    Subr.   Id.    Get next line from input filw
  !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  !      STME21    Subr. W3TIMEMD Convert time to string.
  !      TICK21    Subr.   Id.    Advance time.
  !      DSEC21    Func.   Id.    Difference between times.
  !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  !      W3IOGO    Subr. W3IOGOMD Reading/writing raw gridded data file.
  !      W3READFLGRD Subr. W3IOGOMD Reading output fields flags.
  !      W3EXGB    Subr. Internal Execute grib output.
  !      BAOPEN    Subr.          NCEP library routine.
  !      BAOPENW    Subr.          NCEP library routine.
  !     ----------------------------------------------------------------
  !
  !  5. Called by :
  !
  !     None, stand-alone program.
  !
  !  6. Error messages :
  !
  !     Checks on input, checks in W3IOxx.
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
  !
  !       !/NCO   NCEP NCO modifications for operational implementation.
  !
  !       !/NOGRB No GRIB package included.
  !       !/NCEP2 NCEP IBM links to GRIB2 packing routines.
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  USE CONSTANTS
  !
  !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  USE W3WDATMD, ONLY: W3NDAT, W3SETW
  !      USE W3ADATMD, ONLY: W3NAUX, W3SETA
  USE W3ODATMD, ONLY: W3NOUT, W3SETO
  USE W3IOGRMD, ONLY: W3IOGR
  USE W3IOGOMD, ONLY: W3READFLGRD, W3IOGO
  USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
#ifdef W3_S
  USE W3SERVMD, ONLY : STRACE
#endif
  USE W3TIMEMD, ONLY: STME21, TICK21, DSEC21
  !
  USE W3GDATMD
  USE W3WDATMD, ONLY: TIME, WLV, ICE, UST, USTDIR, RHOAIR
  USE W3ADATMD
  USE W3ODATMD, ONLY: NDSE, NDST, NDSO, NOGRP, NGRPP, IDOUT, UNDEF,&
       FLOGRD, FNMPRE, NOSWLL, NOGE, FLOGD
  !
  IMPLICIT NONE
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local variables
  !/
  INTEGER                 :: NDSI, NDSM, NDSOG, NDSDAT, NDSTRC,   &
       NTRACE, IERR, IOTEST, I,J,K, IFI,IFJ,&
       ISEA, IX, IY, TOUT(2), NOUT, TDUM(2),&
       FTIME(2), CID, PID, GID, GDS, IOUT,  &
       GDTN
  INTEGER, ALLOCATABLE    :: IFIA(:),IFJA(:)
#ifdef W3_NOGRB
  INTEGER                 :: KPDS(1), KGDS(1)
#endif
  ! GRIB2 specific variables
#ifdef W3_NCEP2
  INTEGER                 :: KPDS(200), KGDS(200), IDRS(200)
  INTEGER                 :: LISTSEC0(3), LISTSEC1(13),IGDS(5)
  INTEGER                 :: IDEFLIST, IDEFNUM, KPDSNUM, NUMCOORD
  INTEGER                 :: IBMP, LCGRIB, LENGRIB, IDRSNUM
  REAL                    :: COORDLIST, XN
  CHARACTER(LEN=1), ALLOCATABLE  :: CGRIB(:)
  INTEGER                 :: LATAN1, LONV, SCNMOD, LATIN1, &
       LATIN2, LATSP, LONSP
  REAL                    :: DSX, DSY
  REAL                    :: YN, X0N, Y0N
#endif
#ifdef W3_S
  INTEGER, SAVE           :: IENT = 0
#endif
  REAL                    :: DTREQ, DTEST, RFTIME
  LOGICAL                 :: FLREQ(NOGRP,NGRPP), FLGRIB(NOGRP,NGRPP)
  CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11
  CHARACTER(LEN=80)       :: LINEIN
  CHARACTER(LEN=8)        :: WORDS(5)
  INTEGER                 :: GEN_PRO

  !/
  !/ ------------------------------------------------------------------- /
  !/
#ifdef W3_NCO
  !     CALL W3TAGB('WAVEGRIB',1998,0007,0050,'NP21   ')
#endif
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 1.  IO set-up.
  !
  CALL W3NMOD ( 1, 6, 6 )
  CALL W3SETG ( 1, 6, 6 )
  CALL W3NDAT (    6, 6 )
  CALL W3SETW ( 1, 6, 6 )
  CALL W3NAUX (    6, 6 )
  CALL W3SETA ( 1, 6, 6 )
  CALL W3NOUT (    6, 6 )
  CALL W3SETO ( 1, 6, 6 )
  !
  NDSI   = 10
  NDSM   = 20
  NDSOG  = 20
  NDSDAT = 50
  !
  NDSTRC =  6
  NTRACE = 10
  !
#ifdef W3_NCO
  !
  ! Redo according to NCO
  !
  NDSI   = 11
  NDSO   =  6
  NDSE   = NDSO
  NDST   = NDSO
  NDSM   = 12
  NDSOG  = 13
  NDSDAT = 51
  NDSTRC = NDSO
#endif
  !
  WRITE (NDSO,900)
  !
  CALL ITRACE ( NDSTRC, NTRACE )
#ifdef W3_S
  CALL STRACE (IENT, 'W3GRIB')
#endif
  !
  OPEN (NDSI,FILE='ww3_grib.inp',STATUS='OLD',ERR=800,IOSTAT=IERR)
  READ (NDSI,'(A)',END=801,ERR=802) COMSTR
  IF (COMSTR.EQ.' ') COMSTR = '$'
  WRITE (NDSO,901) COMSTR
  !
#ifdef W3_NOGRB
  WRITE (NDSE,902)
#endif
#ifdef W3_NCEP2
  CALL BAOPENW (NDSDAT,'gribfile',IERR)
#endif
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 2.  Read model definition file.
  !
  CALL W3IOGR ( 'READ', NDSM )
  WRITE (NDSO,920) GNAME
  !
  IF ( .NOT. FLAGLL ) GOTO 810
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 3.  Read requests from input file.
  !     Output times
  !
  CALL NEXTLN ( COMSTR , NDSI , NDSE )
  READ (NDSI,'(A)') LINEIN
  WRITE(NDSO,*)' LINEIN:  ',LINEIN
  READ(LINEIN,*,iostat=ierr) WORDS
  WRITE (NDSO,*) WORDS
  READ(WORDS( 1 ), * ) TOUT(1)
  READ(WORDS( 2 ), * ) TOUT(2)
  READ(WORDS( 3 ), * ) DTREQ
  READ(WORDS( 4 ), * ) NOUT
  IF (WORDS(5) .NE. '0' .AND. WORDS(5) .NE. '1') THEN
    GEN_PRO=-99999
  ELSE
    READ(WORDS( 5 ), * ) GEN_PRO
  ENDIF
  WRITE(NDSO,*) 'GEN_PRO  ',GEN_PRO
  DTREQ  = MAX ( 0. , DTREQ )
  IF ( DTREQ.EQ.0 ) NOUT = 1
  NOUT   = MAX ( 1 , NOUT )
  !
  CALL STME21 ( TOUT , IDTIME )
  WRITE (NDSO,940) IDTIME
  !
  TDUM(1) = 0
  TDUM(2) = 0
  CALL TICK21 ( TDUM , DTREQ )
  CALL STME21 ( TDUM , IDTIME )
  IF ( DTREQ .GE. 86400. ) THEN
    WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  ELSE
    IDDDAY = '           '
  END IF
  IDTIME(1:11) = IDDDAY
  IDTIME(21:23) = '   '
  WRITE (NDSO,941) IDTIME, NOUT
  !
  ! ... Initialize FLGRD array
  !
  FLREQ(:,:)=.FALSE.
  !
  ! ... Call to interface for reading flags or namelists
  !
  CALL W3READFLGRD ( NDSI, NDSO, 9, NDSE, COMSTR, FLOGD, FLREQ,   &
       1, 1, IERR )
  !
  ! Inform user of parameters that were requested but failed to make the
  ! grade, as they are not available for grib encoding, or are not
  ! included presently
  !
  WRITE (NDSO,944)
  ! Reset flags for variables not yet implemented in grib output
  ! interface
  !
  !
  IFI = 3 ! Entire group Frequency-dependent parameters
  DO IFJ = 1,NOGE(IFI)
    IF ( FLREQ(IFI,IFJ) ) THEN
      WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
           '*** NOT YET CODED INTO WW3_GRIB ***'
      FLREQ(IFI,IFJ) = .FALSE.
    END IF
  END DO
  !
  IFI = 5 ! Atm-waves layer, all except for friction velocity
  DO IFJ = 2,10
    IF ( FLREQ(IFI,IFJ) ) THEN
      WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
           '*** NOT YET CODED INTO WW3_GRIB ***'
      FLREQ(IFI,IFJ) = .FALSE.
    END IF
  END DO
  DO IFI = 6,8 ! Entire groups wave-ocean interaction, wave-bottom
    ! layer and spectrum parameters
    DO IFJ = 1,NOGE(IFI)
      IF ( FLREQ(IFI,IFJ) ) THEN
        WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
             '*** NOT YET CODED INTO WW3_GRIB ***'
        FLREQ(IFI,IFJ) = .FALSE.
      END IF
    END DO
  END DO
  IF ( FLREQ(9,5) ) THEN ! CFL number for K advection
    WRITE (NDSO,946) IDOUT(9,5),'*** NOT YET CODED INTO WW3_GRIB ***'
    FLREQ(9,5) = .FALSE.
  END IF
  IFI = 10 ! User defined parameters
  DO IFJ = 1,NOGE(IFI)
    IF ( FLREQ(IFI,IFJ) ) THEN
      WRITE (NDSO,946) IDOUT(IFI,IFJ),                           &
           '*** NOT YET CODED INTO WW3_GRIB ***'
      FLREQ(IFI,IFJ) = .FALSE.
    END IF
  END DO
  !
  ! Compatibility with NCEP operational codes, same effect as old FLGRIB
  !  lists variables that have no code for variable names (not 100%
  !  correct in old codes... )
  !
  ! Chage this as parameters become available in grib2 tables
  !
  ALLOCATE ( IFIA (13), IFJA(13) )

  IFIA = (/ 1, 2, 2, 4, 4, 4, 4, 4, 5, 9, 9, 9, 9 /)
  IFJA = (/ 4, 2, 8, 3, 5, 6, 7, 8, 1, 1, 2, 3, 4 /)
  DO I = 1, 13
    IF ( FLREQ(IFIA(I),IFJA(I)) ) THEN
      FLREQ(IFIA(I),IFJA(I)) = .FALSE.
      WRITE(NDSO,946) IDOUT(IFIA(I),IFJA(I)),                      &
           '*** EXCLUDED FROM GRIB OUTPUT ***'
    END IF
  END DO
  !
  ! Write to stdout parameters that have successfully been requested
  !
  WRITE (NDSO,945)
  DO I=1, NOGRP
    DO J=1, NGRPP
      IF ( FLREQ(I,J) ) WRITE (NDSO,931) IDOUT(I,J)
    END DO
  END DO
  !
  !
  !
  ! ... GRIB specific parameters
  !
  CALL NEXTLN ( COMSTR , NDSI , NDSE )
  READ (NDSI,*,END=801,ERR=802) FTIME, CID, PID, GID, GDS, GDTN
  !
  ! Check if grid type is curvilinear, and only go on if Lambert conformal
  ! or PolarStereo
  !
  IF ( GTYPE .EQ. CLGTYPE ) THEN
#ifdef W3_NCEP2
    ! Allowing code to work with Lambert conformal grids
    IF ( GDTN .NE. 30 .AND. GDTN .NE. 20 ) THEN
#endif
      WRITE(NDSE,*)'PROGRAM W3GRIB: CURVILINEAR GRID SUPPORT '// &
           'FOR GRIB OUTPUT IS NOT YET IMPLEMENTED. NOW STOPPING'
      CALL EXTCDE ( 1 )
#ifdef W3_NCEP2
    ENDIF
#endif
  END IF
  !
  !
  ! Coded up to now only for Lamber conformal grids (GDTN=30) or
  ! PolarStereo (GDTN=20). For regular grids use GDTN=0
  !
#ifdef W3_NCEP2
  IF ( GDTN .EQ. 30 ) THEN
    ! This is a Lambert conformal grid, read projection parameters
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) LATAN1, LONV, DSX, DSY,          &
         SCNMOD, LATIN1, LATIN2, LATSP, LONSP
  ELSEIF ( GDTN .EQ. 20 ) THEN
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) LATAN1, LONV, DSX, DSY,   &
         SCNMOD
#endif

#ifdef W3_NCEP2
  ENDIF
#endif
  !
  CALL STME21 ( FTIME , IDTIME )
  WRITE (NDSO,948) IDTIME, CID, PID, GID, GDS
  !
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 4.  Read general data and first fields from file
  ! 4.a Read file.
  !
  CALL W3IOGO ( 'READ', NDSOG, IOTEST )
  !
  ! 4.b Output fields in file
  !
  !
  WRITE (NDSO,930)
  DO I=1, NOGRP
    DO J=1, NGRPP
      IF ( FLOGRD(I,J) ) WRITE (NDSO,931) IDOUT(I,J)
    END DO
  END DO
  !
#ifdef W3_NCEP2
  !
  IF ( GDTN .EQ. 0 ) THEN
    !
#endif
    ! 4.c Flip MAPSF for REGULAR/RECTILINEAR grids
    !
    DO ISEA=1, NSEA
      IX            = MAPSF(ISEA,1)
      IY            = MAPSF(ISEA,2)
      MAPSF(ISEA,2) = NY + 1 - IY
      MAPSF(ISEA,3) = IY +( IX-1)*NY
    END DO
#ifdef W3_NCEP2
    !
  ENDIF
#endif
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 5. Set grib encoding parameter Sections
  !
  ! ... Initialize KPDS and KGDS (for NCEP2)
  !
  KPDS     =   0
  KGDS     =   0
  !
  ! ... Set GRIB2 packing arrays
  !
#ifdef W3_NCEP2
  LCGRIB = 4*NX*NY
  ALLOCATE(CGRIB(LCGRIB))
#endif
  !
  ! ... Set GRIB2 Indicator Section
  !      ( 1)  Discipline-GRIB Master Table Number (see Code Table 0.0)
  !            0 = Metereological; 10 = Oceanographic
  !      ( 2)  GRIB Edition Number
  !      ( 3)
#ifdef W3_NCEP2
  LISTSEC0      =   0
  LISTSEC0(1)   =   10
  LISTSEC0(2)   =   2
#endif
  !
  ! ... Set GRIB2 Identification Section
  !       ( 1) ID OF CENTER
  !       ( 2) ID OF SUB-CENTER
  !       ( 3) GRIB Master Tables Version Number (Code Table 1.0)
  !       ( 4) GRIB Local Tables Version Number (Code Table 1.0)
  !       ( 5) Significance of Reference Time (Code Table 1.2)
  !     * ( 6) YEAR (4 digits)
  !     * ( 7) MONTH OF YEAR
  !     * ( 8) DAY OF MONTH
  !     * ( 9) HOUR OF DAY
  !       (10) MINUTE OF HOUR
  !       (11) SECOND OF MINUTE
  !       (12) Production status of data (Code Table 1.3)
  !       (13) Type of processed data (Code Table 1.4)
  !
#ifdef W3_NCEP2
  LISTSEC1     =  0
  LISTSEC1(1)  = CID
  LISTSEC1(3)  = 2
  LISTSEC1(4)  = 1
  LISTSEC1(5)  = 1
  LISTSEC1(13) = 1
#endif
  !
  ! ... Set GRIB2 IGDS elements
  !       ( 1) Source of grid definition (Code Table 3.0)
  !       ( 2) Number of grid points
  !       ( 3) Number of octets needed for each additional grid points definition
  !       ( 4) Interpretation of list for optional points definition (Code Table 3.11)
  !       ( 5) Grid definition template number (Code Table 3.1)
  !
#ifdef W3_NCEP2
  IGDS     = 0 ! Defined in code
  IGDS(2)  = NX*NY
  IDEFNUM  = 0
  IDEFLIST = 0
  IGDS(5)=GDTN
  IF ( GDTN .EQ. 30 .AND. GTYPE .EQ. CLGTYPE ) THEN
    IDEFNUM  = 1
    WRITE (NDSO,1011) 'LAMBERTCONF'
  ELSEIF ( GDTN .EQ. 20 .AND. GTYPE .EQ. CLGTYPE ) THEN
    WRITE (NDSO,1011) 'POLARSTEREO'
  ELSEIF ( GDTN .EQ. 0 ) THEN
    WRITE (NDSO,1011) 'LLRECTILINEAR'
  ELSE
    WRITE(NDSE,*)'PROGRAM WAVEGRIB2: SUPPORT FOR CHOSEN '// &
         'GRIB2 GRID DEFINITION TEMPLATE NOT YET IMPLEMENTED'
    CALL EXTCDE ( 2 )
  ENDIF
#endif
  !
  ! ... Set GRIB2 KGDS elements
  !
  ! General parameters for all grids
  !       ( 1) Coordinate system (6 = spherical coordinate system with radius of 6,371,229 m)
  !       ( 2)
  !       ( 3)
  !       ( 4)
  !       ( 5)
  !       ( 6)
  !       ( 7)
  !       ( 8) Number of points along parallel
  !       ( 9) Number of points along meridian
#ifdef W3_NCEP2
  KGDS( 1) = 6
  KGDS( 8) = NX
  KGDS( 9) = NY
#endif
  !
#ifdef W3_NCEP2
  IF ( GDTN .EQ. 30 ) THEN
#endif
    !
    ! Lambert Conformal grid
    !       (10) Latitude of first grid point
    !       (11) Longitude of first grid point
    !       (12) Resolution and component flags
    !       (13) Latitude where DX and DY are specified
    !       (14) Longitude of orientation
    !       (15) Increment of longitude
    !       (16) Increment of latitude
    !       (17) Projection center flag
    !       (18) Scanning mode
    !       (19) First latitude of secant cone
    !       (20) Second latitude of secant cone
    !       (21) Latitude of southern pole
    !       (22) Longitude of southern pole
    !
#ifdef W3_NCEP2
    X0 = MOD(XGRD(1,1) + 360.,360.)
    XN = MOD(XGRD(NY,NX) + 360., 360.)
    X0N = MOD(XGRD(NY,1) + 360., 360.)
    KGDS(11)=NINT(1000000.*X0)
    Y0 = YGRD(1,1)
    YN = YGRD(NY,NX)
    Y0N = YGRD(NY,1)
    KGDS(10)=NINT(1000000.*Y0)
    KGDS(12)=0
    KGDS(13)=DBLE(1000000.*LATAN1)
    KGDS(14)=DBLE(1000000.*LONV)
    KGDS(15)=NINT(1000000*DSX)
    KGDS(16)=NINT(1000000*DSY)
    KGDS(17)=0
    KGDS(18)=SCNMOD
    KGDS(19)=DBLE(1000000.*LATIN1)
    KGDS(20)=DBLE(1000000.*LATIN2)
    KGDS(21)=DBLE(1000000.*LATSP)
    KGDS(22)=DBLE(1000000.*LONSP)
#endif
    !
#ifdef W3_NCEP2
  ELSEIF (GDTN .EQ. 20 ) THEN
#endif
    !
    ! PolarStereo grid
    !       (10) Latitude of first grid point
    !       (11) Longitude of first grid point
    !       (12) Res and component flags
    !       (13) Latitude where DX and DY are specified
    !       (14) Longitude of orientation
    !       (15) Increment of longitude
    !       (16) Increment of latitude
    !       (17) Projection center flag
    !       (18) Scanning mode
    !
    !  Projection for PolarStereo grid was changed from
    !  KGDS( 1) = 6 to KGDS( 1) = 5 (Earth assumed represented by WGS84 -
    !  Octet No 15 Table 3.2)
#ifdef W3_NCEP2
    KGDS( 1) = 5
    X0 = MOD(XGRD(1,1) + 360.,360.)
    XN = MOD(XGRD(NY,NX) + 360., 360.)
    X0N = MOD(XGRD(NY,1) + 360., 360.)
    KGDS(11)=NINT(1000000.*X0)
    Y0 = YGRD(1,1)
    YN = YGRD(NY,NX)
    Y0N = YGRD(NY,1)
    KGDS(10)=NINT(1000000.*Y0)
    KGDS(12)=0
    KGDS(13)=DBLE(1000000.*LATAN1)
    KGDS(14)=DBLE(1000000.*LONV)
    KGDS(15)=NINT(1000000*DSX)
    KGDS(16)=NINT(1000000*DSY)
    KGDS(17)=0
    KGDS(18)=SCNMOD
#endif
    !
#ifdef W3_NCEP2
  ELSEIF (GDTN .EQ. 0 ) THEN
#endif
    !
    ! Lat Lon rectilinear grid
    !       (10)
    !       (11)
    !       (12) Latitude of first grid point
    !       (13) Longitude of first grid point
    !       (14) Res and component flags
    !       (15) Latitude of last grid point
    !       (16) Longitude of last grid point
    !       (17) Increment of longitude
    !       (18) Increment of latitude
    !       (19) Scanning mode
    !
#ifdef W3_NCEP2
    KGDS(12) = NINT(1000000.*(Y0+(REAL(NY-1)*SY)))
    X0 = MOD(X0 + 360.,360.)
    KGDS(13) = NINT(1000000.*X0)
    KGDS(14) = 48
    KGDS(15) = NINT(1000000.*Y0)
    XN = MOD(X0+REAL(NX-1)*SX + 360., 360.)
    KGDS(16) = NINT(1000000.*XN)
    KGDS(17) = NINT(1000000.*SX)
    KGDS(18) = NINT(1000000.*SY)
  ENDIF
#endif
  !
  ! ... Set GRIB2 PDS elements
  !     KPDSNUM   (0 indicates forecast at a horizontal level)
  !     ( 1) Parameter category (Code Table 4.1)
  !            For oceanographic products -- 0 = waves; 1 = currents; 2 = ice
  !            For atmospheric products   -- 2 = momentum
  !     ( 2) Parameter number (Code Table 4.2)
  !     ( 3) Generating process (Code Table 4.3)
  !     ( 4) Background generating process identifier (center specific)
  !     ( 5) Process or model number
  !     ( 6) Hours of observational data cutoff after reference time
  !     ( 7) Minutes of observational data cutoff after reference time
  !     ( 8) Indicator of forecast time unit (Code Table 4.4)
  !     ( 9) Time range
  !     (10) Type of level (Code Table 4.5) 1st level
  !     (11) Scaled factor of (10)
  !     (12) Scaled value of (10)
  !     (13) Type of level (Code Table 4.5) 2nd level
  !     (14) Scaled factor of (13)
  !     (15) Scaled value of (13)
  !
  !
  !      KPDS(3)=4 ensemble forecast:ww3_grib.inp has gen_pro set to 1
  !             =2 deterministic forecast: ww3_grib.inp gen_pro set to 0
  !             =2 legacy :with no gen_pro set in ww3_grib.inp
  !              (in the case of legacy the params revert back to old names)
#ifdef W3_NCEP2
  KPDSNUM = 0
  if ( gen_pro.eq.1 ) then
    KPDS( 3) = 4
  else
    KPDS(3)=2
  endif
  KPDS( 4) = 0
  KPDS( 5) = PID
  KPDS( 8) = 1
  KPDS(10) = 1
  KPDS(12) = 1
  KPDS(13) = 255
#endif
  !
  ! ... Set GRIB2 vertical layer information
  !
#ifdef W3_NCEP2
  NUMCOORD  = 0
  COORDLIST = 0.0
#endif
  !
  ! ... Set GRIB2 bitmap information
  !      0 Bitmap is provided
  !
#ifdef W3_NCEP2
  IBMP = GDS
#endif
  !
  ! ... Set GRIB2 Data Representation Template Number (Code Table 5.0)
  !
#ifdef W3_NCEP2
  IDRSNUM = 40 !jpeg2000 *** SEGFAULTS in some linux
#endif
  !                            clusters with Intel compiler ***
#ifdef W3_NCEP2
  !IDRSNUM = 0 !simple packing
  !IDRSNUM = 41 !png packing
  !IDRSNUM = 2 !Complex Packing (Grid Point Data)
#endif
  !
  ! ... Set GRIB2 IDRS elements
  !     ( 1) Reference value (R) (IEEE 32-bit floating-point value)
  !     ( 2) Binary Scale Factor (E)
  !     ( 3) Decimal Scale Factor (D)
  !     ( 4) Number of bits used for each packed value
  !     ( 5) Type of original field values (Code Table 5.1)
  !
#ifdef W3_NCEP2
  IDRS    = 0
  IDRS(3) = 2
#endif
  !
#ifdef W3_T
  WRITE (NDST,9050) KPDS
  WRITE (NDST,9051) KGDS
#endif
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 6.  Time management.
  !
  IOUT   = 0
  WRITE (NDSO,970)
  !
  DO
    DTEST  = DSEC21 ( TIME , TOUT )
    IF ( DTEST .GT. 0. ) THEN
      CALL W3IOGO ( 'READ', NDSOG, IOTEST )
      IF ( IOTEST .EQ. -1 ) THEN
        WRITE (NDSO,942)
        GOTO 888
      END IF
      CYCLE
    END IF
    IF ( DTEST .LT. 0. ) THEN
      CALL TICK21 ( TOUT , DTREQ )
      CYCLE
    END IF
    !
    IOUT   = IOUT + 1
    CALL STME21 ( TOUT , IDTIME )
    !
    RFTIME = DSEC21 ( FTIME , TIME ) / 3600.
    IF ( RFTIME .LT. 0. ) THEN
#ifdef W3_NCEP2
      LISTSEC1( 6) = TIME(1)/10000
      LISTSEC1( 7) = MOD(TIME(1),10000) / 100
      LISTSEC1( 8) = MOD(TIME(1),100)
      LISTSEC1( 9) = TIME(2) / 10000
      KPDS( 9)     = 0
#endif
      WRITE (NDSO,972) IDTIME
    ELSE
#ifdef W3_NCEP2
      LISTSEC1( 6) = FTIME(1)/10000
      LISTSEC1( 7) = MOD(FTIME(1),10000) / 100
      LISTSEC1( 8) = MOD(FTIME(1),100)
      LISTSEC1( 9) = FTIME(2) / 10000
      KPDS( 9)     = NINT(RFTIME)
#endif
      WRITE (NDSO,971) IDTIME, NINT(RFTIME)
    END IF
    !
    CALL W3EXGB ( NX, NY, NSEA )
    CALL TICK21 ( TOUT , DTREQ )
    IF ( IOUT .GE. NOUT ) EXIT
  END DO
  !
  GOTO 888
  !
  ! Escape locations read errors :
  !
800 CONTINUE
  WRITE (NDSE,1000) IERR
  CALL EXTCDE ( 3 )
  !
801 CONTINUE
  WRITE (NDSE,1001)
  CALL EXTCDE ( 4 )
  !
802 CONTINUE
  WRITE (NDSE,1002) IERR
  CALL EXTCDE ( 5 )
  !
810 CONTINUE
  IF ( .NOT. FLAGLL ) THEN
    WRITE (NDSE,1010)
    CALL EXTCDE ( 10 )
  END IF
  !
888 CONTINUE
  WRITE (NDSO,999)
  !
#ifdef W3_NCO
  !     CALL W3TAGE('WAVEGRIB')
#endif
  !
  ! Formats
  !
900 FORMAT (/15X,'   *** WAVEWATCH III GRIB output postp. ***   '/ &
       15X,'=============================================='/)
901 FORMAT ( '  Comment character is ''',A,''''/)
902 FORMAT (/'  *** WARNING : NO GRIB PACKAGE LINKED ***'/)
  !
920 FORMAT ( '  Grid name : ',A/)
  !
930 FORMAT ( '  Fields in file : '/                                 &
       ' --------------------------')
931 FORMAT ( '      ',A)
  !
940 FORMAT (/'  Output time data : '/                               &
       ' -----------------------------------------------------'/ &
       '      First time         : ',A)
941 FORMAT ( '      Interval           : ',A/                       &
       '      Number of requests : ',I4)
942 FORMAT (/'      End of file reached '/)
  !
944 FORMAT (/'  Requested output fields not yet available: '/        &
       ' -----------------------------------------------------')
  !
945 FORMAT (/'  Successfully requested output fields : '/            &
       ' -----------------------------------------------------')
946 FORMAT ( '      ',A,1X,A)
  !
948 FORMAT (/'  Additional GRIB parameters : '/                     &
       ' -----------------------------------------------------'/ &
       '      Run time           : ',A/                       &
       '      GRIB center ID     : ',I4/                      &
       '      GRIB gen. proc. ID : ',I4/                      &
       '      GRIB grid ID       : ',I4/                      &
       '      GRIB GDS parameter : ',I4)
  !
970 FORMAT (//'  Generating file '/                                 &
       ' -----------------------------------------------------')
971 FORMAT ( '      Data for ',A,'  ',I3,'H forecast.')
972 FORMAT ( '      Data for ',A,'       hindcast.')
  !
999 FORMAT (/'  End of program '/                                   &
       ' ========================================='/          &
       '         WAVEWATCH III GRIB output '/)
  !
#ifdef W3_T
9050 FORMAT ( ' TEST W3GRIB : KPDS : ',13I4/                      &
       '                      ',12I4)
9051 FORMAT ( ' TEST W3GRIB : KGDS : ',8I6/                       &
       '                      ',8I6/                       &
       '                      ',6I6)
#endif
  !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRIB : '/               &
       '     ERROR IN OPENING INPUT FILE'/                    &
       '     IOSTAT =',I5/)
  !
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRIB : '/               &
       '     PREMATURE END OF INPUT FILE'/)
  !
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3GRIB : '/               &
       '     ERROR IN READING FROM INPUT FILE'/               &
       '     IOSTAT =',I5/)
  !
1010 FORMAT (/' *** WAVEWATCH-III ERROR IN W3GRIB : '/          &
       '     GRIB REQUIRES SPHERICAL GRID'/)
#ifdef W3_NCEP2
1011 FORMAT (/' CHOSEN GRID TYPE: : ',A/)
#endif
  !/
  !/ Internal subroutine W3EXGB ---------------------------------------- /
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !> @brief Perform actual GRIB output.
  !>
  !> @param[in] NX X array dimension
  !> @param[in] NY Y array dimension
  !> @param[in] NSEA Seapoint array dimension
  !>
  !> @author H. L. Tolman
  !> @author A. Chawla
  !> @date 22-Mar-2021
  SUBROUTINE W3EXGB ( NX, NY, NSEA )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           H. L. Tolman            |
    !/                  |             A. Chawla             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    10-Jun-1999 : Final FORTRAN 77                    ( version 1.18 )
    !/    24-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
    !/                  Massive changes to logistics.
    !/    29-Apr-2002 : Adding output fields 17-18.         ( version 2.20 )
    !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
    !/    18-May-2007 : Update GRIB1 for partitioning.      ( version 3.11 )
    !/    16-Jul-2007 : Adding GRIB2 capability             ( version 3.11 )
    !/                  (A. Chawla)
    !/    22-Mar-2021 : New coupling fields output          ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Perform actual GRIB output.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NX, NY, NSEA
    !               Int.  I  Array dimensions.
    !     ----------------------------------------------------------------
    !
    !     Internal parameters
    !     ----------------------------------------------------------------
    !       X1, X2, XX, XY
    !               R.A.  Output fields
    !       BITMAP  L.A.  Data / no data bitmap
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE     Subr. W3SERVMD Subroutine tracing.
    !      EXTCDE     Subr.   Id.    Abort program as graceful as possible.
    !      W3S2XY     Subr.   Id.    Convert from storage to spatial grid.
    !      PUTGB      Subr.          NCEP GRIB1 library routine.
    !      GRIBCREATE Subr.          NCEP GRIB2 library routine.
    !      ADDGRID    Subr.          NCEP GRIB2 library routine.
    !      ADDFIELD   Subr.          NCEP GRIB2 library routine.
    !      GRIBEND    Subr.          NCEP GRIB2 library routine.
    !      WRYTE      Subr.          NCEP GRIB2 library routine.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !     Program in which it is contained.
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !     - Note that arrays CX and CY of the main program now contain
    !       the absolute current speed and direction respectively.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !     !/S      Enable subroutine tracing.
    !     !/T      Enable test output.
    !     !/NCEP2  NCEP IBM calls to GRIB2 packer (follows updated grib2
    !     tables under verification as of 02/10/2012).
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3SERVMD, ONLY : W3S2XY
#ifdef W3_RTD
    USE W3SERVMD, ONLY : W3THRTN, W3XYRTN
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)     :: NX, NY, NSEA
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: J, IXY, NDATA
    INTEGER                 :: IO
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL                    ::  X1(NX*NY), X2(NX*NY), XX(NX*NY),    &
         XY(NX*NY), CABS, UABS,              &
         YY(NX*NY,0:NOSWLL), KPDS5A, KPDS5B, &
         KPDS5A1(3)
    LOGICAL*1               ::  BITMAP(NX*NY)
    LOGICAL                 :: FLONE, FLTWO, FLDIR, FLTRI, FLPRT
    !/
    !/ ------------------------------------------------------------------- /
    !/
#ifdef W3_S
    CALL STRACE (IENT, 'W3EXGB')
#endif
    !
#ifdef W3_T
    WRITE (NDST,9000) ((FLREQ(IFI,IFJ),IFJ=1,NGRPP), IFI=1,NOGRP)
    WRITE (NDST,9001) NDSDAT, KPDS, KGDS
#endif
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 1.  Preparations
    !
    X1     = UNDEF
    X2     = UNDEF
    XX     = UNDEF
    XY     = UNDEF
    YY     = UNDEF
    !
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 2.  Loop over output fields.
    !
    DO IFI=1, NOGRP
      DO IFJ=1, NGRPP
        IF ( FLREQ(IFI,IFJ) ) THEN


          !
          ! Initialize array dimension flags
          !
          FLONE = .FALSE.
          FLTWO = .FALSE.
          FLDIR = .FALSE.
          FLTRI = .FALSE.
          FLPRT = .FALSE.
          !
#ifdef W3_T
          WRITE (NDST,9020) IDOUT(IFI,IFJ)
#endif
          !
          ! 2.a Set output arrays and parameters
          !
          !     Water depth
          !
          IF ( IFI .EQ. 1 .AND. IFJ .EQ.  1 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 14
            KPDS(1) = 4
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, DW(1:NSEA)          &
                 , MAPSF, X1 )
            !
            !     Current
            !
          ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ.  2 ) THEN
            FLTWO = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 1
            KPDS(1) = 1
#endif
#ifdef W3_RTD
            ! Rotate x,y vector back to standard pole
            IF ( FLAGUNR ) CALL W3XYRTN(NSEA, CX, CY, AnglD)
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, CX(1:NSEA)          &
                 , MAPSF, XX )
            CALL W3S2XY ( NSEA, NSEA, NX, NY, CY(1:NSEA)          &
                 , MAPSF, XY )
            DO ISEA=1, NSEA
              IF (CX(ISEA) .NE. UNDEF) THEN
                CABS   = SQRT(CX(ISEA)**2+CY(ISEA)**2)
                IF ( CABS .GT. 0.001 ) THEN
                  CY(ISEA) = MOD ( 630. -                         &
                       RADE*ATAN2(CY(ISEA),CX(ISEA)) , 360. )
                ELSE
                  CY(ISEA) = 0.
                END IF
              ELSE
                CABS = UNDEF
                CY(ISEA) = UNDEF
              END IF
              CX(ISEA) = CABS
            END DO
            CALL W3S2XY ( NSEA, NSEA, NX, NY, CX(1:NSEA)          &
                 , MAPSF, X1 )
            CALL W3S2XY ( NSEA, NSEA, NX, NY, CY(1:NSEA)          &
                 , MAPSF, X2 )
            !
            !     Wind speed
            !
          ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ.  3 ) THEN
            FLTWO = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 1
            KPDS(1) = 2
            LISTSEC0(1) = 0
#endif
#ifdef W3_RTD
            ! Rotate x,y vector back to standard pole
            IF ( FLAGUNR ) CALL W3XYRTN(NSEA, UA, UD, AnglD)
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, UA(1:NSEA)          &
                 , MAPSF, XX )
            CALL W3S2XY ( NSEA, NSEA, NX, NY, UD(1:NSEA)          &
                 , MAPSF, XY )
            DO ISEA=1, NSEA
              IF (UA(ISEA) .NE. UNDEF) THEN
                UABS   = SQRT(UA(ISEA)**2+UD(ISEA)**2)
                IF ( UABS .GT. 0.001 ) THEN
                  UD(ISEA) = MOD ( 630. -                         &
                       RADE*ATAN2(UD(ISEA),UA(ISEA)) , 360. )
                ELSE
                  UD(ISEA) = 0.
                END IF
              ELSE
                UABS = UNDEF
                UD(ISEA) = UNDEF
              END IF
              UA(ISEA) = UABS
            END DO
            CALL W3S2XY ( NSEA, NSEA, NX, NY, UA(1:NSEA)          &
                 , MAPSF, X1 )
            CALL W3S2XY ( NSEA, NSEA, NX, NY, UD(1:NSEA)          &
                 , MAPSF, X2 )
            !
            !     Air-sea temp. dif.
            !
          ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ.  4 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
            KPDS(1) = 3
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, AS(1:NSEA)          &
                 , MAPSF, X1 )
            !
            !     Water level
            !
          ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 1
            KPDS(1) = 3
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, WLV   , MAPSF, X1 )
            !
            !     Ice concentration
            !
          ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 0
            KPDS(1) = 2
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, ICE   , MAPSF, X1 )
            !
            !     Atmospheric momentum
            !
          ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ.  8 ) THEN
            FLTWO = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 1
            KPDS(1) = 2
            LISTSEC0(1) = 0
#endif
#ifdef W3_RTD
            ! Rotate x,y vector back to standard pole
            IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUA, TAUADIR, AnglD)
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUA(1:NSEA)        &
                 , MAPSF, XX )
            CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUADIR(1:NSEA)     &
                 , MAPSF, XY )
            DO ISEA=1, NSEA
              IF (TAUA(ISEA) .NE. UNDEF) THEN
                UABS   = SQRT(TAUA(ISEA)**2+TAUADIR(ISEA)**2)
                IF ( UABS .GT. 0.001 ) THEN
                  TAUADIR(ISEA) = MOD ( 630. -                 &
                       RADE*ATAN2(TAUADIR(ISEA),TAUA(ISEA)) , 360. )
                ELSE
                  TAUADIR(ISEA) = 0.
                END IF
              ELSE
                UABS = UNDEF
                TAUADIR(ISEA) = UNDEF
              END IF
              TAUA(ISEA) = UABS
            END DO
            CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUA(1:NSEA)        &
                 , MAPSF, X1 )
            CALL W3S2XY ( NSEA, NSEA, NX, NY, TAUADIR(1:NSEA)     &
                 , MAPSF, X2 )
            !
            !     Air density
            !
          ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 9 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 0
            KPDS(1) = 2
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, RHOAIR, MAPSF, X1 )
            !
            !     Significant wave height
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  1 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 3
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, HS    , MAPSF, X1 )
            !
            !     Mean wave length
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  2 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 193
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, WLM   , MAPSF, X1 )
            !
            !     Mean wave period (based on second moment)
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  3 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            if ((gen_pro.eq.1) .or. (gen_pro.eq.0)) then
              KPDS(2) = 28
            else
              KPDS(2) = 25
            endif
#endif

            CALL W3S2XY ( NSEA, NSEA, NX, NY, T02   , MAPSF, X1 )
            !
            !     Mean wave period (based on first moment)
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  4 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 15
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, T0M1   , MAPSF, X1 )
            !
            !     Mean wave period (based on first inverse moment)
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  5 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            if ((gen_pro.eq.1) .or. (gen_pro.eq.0)) then
              KPDS(2) = 34
            else
              KPDS(2) = 15
            endif
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, T01   , MAPSF, X1 )
            !
            !     Peak frequency
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 6 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 11
#endif
            DO ISEA=1, NSEA
              IF ( FP0(ISEA) .NE. UNDEF .AND. FP0(ISEA) .NE. 0 ) THEN
                FP0(ISEA) = 1. / MAX(FR1,FP0(ISEA)) ! Limit FP to lowest discrete frequency
              END IF
            END DO
            CALL W3S2XY ( NSEA, NSEA, NX, NY, FP0   , MAPSF, X1 )
            !
            !
            !     Mean wave direction
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ.  7 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 14
#endif
#ifdef W3_RTD
            ! Rotate direction back to standard pole
            IF ( FLAGUNR ) CALL W3THRTN(NSEA, THM, AnglD, .FALSE.)
#endif
            DO ISEA=1, NSEA
              IF ( THM(ISEA) .NE. UNDEF )                         &
                   THM(ISEA) = MOD ( 630. - RADE*THM(ISEA) , 360. )
            END DO
            CALL W3S2XY ( NSEA, NSEA, NX, NY, THM   , MAPSF, X1 )
            !
            !     Directional spread
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 31
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, THS   , MAPSF, X1 )
            !
            !     Peak direction
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            if ((gen_pro.eq.1) .or. (gen_pro.eq.0)) then
              KPDS(2) = 46
            else
              KPDS(2) = 10
            endif
#endif
#ifdef W3_RTD
            ! Rotate direction back to standard pole
            IF ( FLAGUNR ) CALL W3THRTN(NSEA, THP0, AnglD, .FALSE.)
#endif
            DO ISEA=1, NSEA
              IF ( THP0(ISEA) .NE. UNDEF ) THEN
                THP0(ISEA) = MOD ( 630-RADE*THP0(ISEA) , 360. )
              END IF
            END DO
            CALL W3S2XY ( NSEA, NSEA, NX, NY, THP0  , MAPSF, X1 )
            !
            !     Mean wave number
            !
          ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 19 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, WNMEAN, MAPSF, X1 )
            !
            !     Partitioned wave height
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
            FLPRT = .TRUE.
#ifdef W3_NCEP2
            KPDS5A  = 5
            KPDS5B  = 8
            if ((gen_pro.eq.1) .or. (gen_pro.eq.0)) then
              KPDS5A1(1)  =47
              KPDS5A1(2)  =48
              KPDS5A1(3)  =49
            else
              KPDS5B  = 8
            endif
#endif
            CALL W3S2XY                                           &
                 ( NSEA, NSEA, NX, NY, PHS(:,0), MAPSF, YY(:,0) )
            DO I=1, NOSWLL
              CALL W3S2XY                                         &
                   ( NSEA, NSEA, NX, NY, PHS(:,I), MAPSF, YY(:,I) )
            END DO
            !
            !     Partitioned peak period
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
            FLPRT = .TRUE.
#ifdef W3_NCEP2
            KPDS5A  = 6
            KPDS5B = 9
            if ((gen_pro.eq.1) .or. (gen_pro.eq.0)) then
              KPDS5A1(1)  = 50
              KPDS5A1(2)  = 51
              KPDS5A1(3)  = 52
            else
              KPDS5B = 9
            endif
#endif
            CALL W3S2XY                                           &
                 ( NSEA, NSEA, NX, NY, PTP(:,0), MAPSF, YY(:,0) )
            DO I=1, NOSWLL
              CALL W3S2XY                                         &
                   ( NSEA, NSEA, NX, NY, PTP(:,I), MAPSF, YY(:,I) )
            END DO
            !
            !     Partitioned peak wave length
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
            FLPRT = .TRUE.
#ifdef W3_NCEP2
            KPDS5A  =  193
            KPDS5B  =  193
#endif
            CALL W3S2XY                                           &
                 ( NSEA, NSEA, NX, NY, PLP(:,0), MAPSF, YY(:,0) )
            DO I=1, NOSWLL
              CALL W3S2XY                                         &
                   ( NSEA, NSEA, NX, NY, PLP(:,I), MAPSF, YY(:,I) )
            END DO
            !
            !     Partitioned mean direction
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
            FLPRT = .TRUE.
#ifdef W3_NCEP2
            KPDS5A  = 4
            KPDS5B = 7
            if ((gen_pro.eq.1) .or. (gen_pro.eq.0)) then
              KPDS5A1(1)  = 53
              KPDS5A1(2)  = 54
              KPDS5A1(3)  = 55
            else
              KPDS5B = 7
            endif
#endif
#ifdef W3_RTD
            DO I = 0,NOSWLL
              ! Rotate direction back to standard pole
              IF ( FLAGUNR ) CALL W3THRTN(NSEA, PDIR(:,I), AnglD, .FALSE.)
            END DO
#endif
            DO ISEA = 1,NSEA
              DO I = 0,NOSWLL
                IF ( PDIR(ISEA,I) .NE. UNDEF ) THEN
                  PDIR(ISEA,I) = MOD ( 630 - RADE*PDIR(ISEA,I) , 360. )
                END IF
              END DO
            END DO
            CALL W3S2XY                                           &
                 ( NSEA, NSEA, NX, NY, PDIR(:,0), MAPSF, YY(:,0) )
            DO I=1, NOSWLL
              CALL W3S2XY                                         &
                   ( NSEA, NSEA, NX, NY, PDIR(:,I), MAPSF, YY(:,I) )
            END DO
            !
            !     Partitioned Directional spread
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
            FLPRT = .TRUE.
#ifdef W3_NCEP2
            KPDS5A  =  32
            KPDS5B  =  33
#endif
            CALL W3S2XY                                           &
                 ( NSEA, NSEA, NX, NY, PSI(:,0), MAPSF, YY(:,0) )
            DO I=1, NOSWLL
              CALL W3S2XY                                         &
                   ( NSEA, NSEA, NX, NY, PSI(:,I), MAPSF, YY(:,I) )
            END DO
            !
            !     Partitioned wind sea fraction
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
            FLPRT = .TRUE.
#ifdef W3_NCEP2
            KPDS5A  =  255
            KPDS5B  =  255
#endif
            CALL W3S2XY                                           &
                 ( NSEA, NSEA, NX, NY, PWS(:,0), MAPSF, YY(:,0) )
            DO I=1, NOSWLL
              CALL W3S2XY                                         &
                   ( NSEA, NSEA, NX, NY, PWS(:,I), MAPSF, YY(:,I) )
            END DO
            !
            !     Total wind sea fraction
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, PWST  , MAPSF, X1 )
            !
            !     Number of fields in partition
            !
          ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, PNR   , MAPSF, X1 )
            !
            !     Friction velocity
            !
          ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ.  1 ) THEN
            FLTWO = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 17
            KPDS(1) = 1
#endif
#ifdef W3_RTD
            ! Rotate x,y vector back to standard pole
            IF ( FLAGUNR ) CALL W3XYRTN(NSEA, UST, USTDIR, AnglD)
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, UST(1:NSEA)         &
                 , MAPSF, X1 )
            CALL W3S2XY ( NSEA, NSEA, NX, NY, USTDIR(1:NSEA)      &
                 , MAPSF, X2 )
            !
            !     Average source term time step
            !
          ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
#endif
            DO ISEA=1, NSEA
              IF ( DTDYN(ISEA) .NE. UNDEF )                       &
                   DTDYN(ISEA) = DTDYN(ISEA) / 60.
            END DO
            CALL W3S2XY ( NSEA, NSEA, NX, NY, DTDYN , MAPSF, X1 )
            !
            !     Cut-off frequency
            !
          ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, FCUT  , MAPSF, X1 )
            !
            !     CFL Maximum (in spatial space)
            !
          ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, CFLXYMAX  , MAPSF, X1 )
            !
            !     CFL Maximum (in spectral space)
            !
          ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
            FLONE = .TRUE.
#ifdef W3_NCEP2
            KPDS(2) = 255
#endif
            CALL W3S2XY ( NSEA, NSEA, NX, NY, CFLTHMAX  , MAPSF, X1 )
            !
          ELSE
            WRITE (NDSE,999)
            CALL EXTCDE ( 1 )
            !
          END IF
          !
          ! 3   Perform output
          !
          NDATA  = NX*NY
          !
          ! 3.a Partitioned data
          !
          IF ( FLPRT ) THEN
            !
#ifdef W3_NCEP2
            KPDS(2) = KPDS5A
#endif
            DO IXY=1, NX*NY
              BITMAP(IXY) = YY(IXY,0) .NE. UNDEF
            END DO
#ifdef W3_NCEP2
            CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
            IF (IO .NE. 0) GOTO 810
            CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST,   &
                 IDEFNUM, IO)
            IF (IO .NE. 0) GOTO 820
            CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200,        &
                 COORDLIST, NUMCOORD, IDRSNUM, IDRS,   &
                 200,YY(:,0), NDATA, IBMP, BITMAP, IO)
            IF (IO .NE. 0) GOTO 820
            CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
            IF (IO .NE. 0) GOTO 830
            CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
#endif
            !
#ifdef W3_NCEP2
            if ((gen_pro.eq.0) .or. (gen_pro.eq.1)) then
              KPDS(10) = 241
#endif
              DO I=1, NOSWLL
#ifdef W3_NCEP2
                KPDS(2) = KPDS5A1(I)
                KPDS(12) = I
#endif
                DO IXY=1, NX*NY
                  BITMAP(IXY) = YY(IXY,I) .NE. UNDEF
                END DO
#ifdef W3_NCEP2
                CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
                IF (IO .NE. 0) GOTO 810
                CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST,   &
                     IDEFNUM, IO)
                IF (IO .NE. 0) GOTO 820
                CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200,        &
                     COORDLIST, NUMCOORD, IDRSNUM, IDRS,   &
                     200,YY(:,I), NDATA, IBMP, BITMAP, IO)
                IF (IO .NE. 0) GOTO 820
                CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
                IF (IO .NE. 0) GOTO 830
                CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
#endif
              END DO
#ifdef W3_NCEP2
            ELSE
              KPDS(2) = KPDS5B
              KPDS(10) = 241
#endif
              DO I=1, NOSWLL
#ifdef W3_NCEP2
                KPDS(12) = I
#endif
                DO IXY=1, NX*NY
                  BITMAP(IXY) = YY(IXY,I) .NE. UNDEF
                END DO
#ifdef W3_NCEP2
                CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
                IF (IO .NE. 0) GOTO 810
                CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST,   &
                     IDEFNUM, IO)
                IF (IO .NE. 0) GOTO 820
                CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200,        &
                     COORDLIST, NUMCOORD, IDRSNUM, IDRS,   &
                     200,YY(:,I), NDATA, IBMP, BITMAP, IO)
                IF (IO .NE. 0) GOTO 820
                CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
                IF (IO .NE. 0) GOTO 830
                CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
#endif
              END DO
#ifdef W3_NCEP2
            ENDIF
            KPDS(10) = 1
            KPDS(12) = 1
#endif
            !
            ! 3.b Other data
            !
          ELSE IF (FLONE) THEN
            !
            DO IXY=1, NX*NY
              BITMAP(IXY) = X1(IXY) .NE. UNDEF
            END DO
            !
#ifdef W3_NCEP2
            CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
            IF (IO .NE. 0) GOTO 810
            CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST,   &
                 IDEFNUM, IO)
            IF (IO .NE. 0) GOTO 820
            CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200,        &
                 COORDLIST, NUMCOORD, IDRSNUM, IDRS,   &
                 200,X1, NDATA, IBMP, BITMAP, IO)
            IF (IO .NE. 0) GOTO 820
            CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
            IF (IO .NE. 0) GOTO 830
            CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
#endif
            !
          ELSE IF ( FLTWO ) THEN
            !
            DO IXY=1, NX*NY
              BITMAP(IXY) = X1(IXY) .NE. UNDEF
            END DO
#ifdef W3_NCEP2
            CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
            IF (IO .NE. 0) GOTO 810
            CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST, &
                 IDEFNUM, IO)
            IF (IO .NE. 0) GOTO 820
            CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200, &
                 COORDLIST, NUMCOORD, IDRSNUM, IDRS, &
                 200,X1, NDATA, IBMP, BITMAP, IO)
            IF (IO .NE. 0) GOTO 820
            CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
            IF (IO .NE. 0) GOTO 830
            CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
#endif

#ifdef W3_NCEP2
            KPDS(2) = 0
            CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
            IF (IO .NE. 0) GOTO 810
            CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST, &
                 IDEFNUM, IO)
            IF (IO .NE. 0) GOTO 820
            CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200,      &
                 COORDLIST, NUMCOORD, IDRSNUM, IDRS, &
                 200,X2, NDATA, IBMP, BITMAP, IO)
            IF (IO .NE. 0) GOTO 820
            CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
            IF (IO .NE. 0) GOTO 830
            CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
            KPDS(2) = 2
            CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
            IF (IO .NE. 0) GOTO 810
            CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST, &
                 IDEFNUM, IO)
            IF (IO .NE. 0) GOTO 820
            CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200,      &
                 COORDLIST, NUMCOORD, IDRSNUM, IDRS, &
                 200,XX, NDATA, IBMP, BITMAP, IO)
            IF (IO .NE. 0) GOTO 820
            CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
            IF (IO .NE. 0) GOTO 830
            CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
            KPDS(2) = 3
            CALL GRIBCREATE (CGRIB,LCGRIB,LISTSEC0,LISTSEC1,IO)
            IF (IO .NE. 0) GOTO 810
            CALL ADDGRID (CGRIB,LCGRIB,IGDS,KGDS,200,IDEFLIST, &
                 IDEFNUM, IO)
            IF (IO .NE. 0) GOTO 820
            CALL ADDFIELD (CGRIB,LCGRIB,KPDSNUM,KPDS,200,      &
                 COORDLIST, NUMCOORD, IDRSNUM, IDRS, &
                 200,XY, NDATA, IBMP, BITMAP, IO)
            IF (IO .NE. 0) GOTO 820
            CALL GRIBEND (CGRIB, LCGRIB, LENGRIB, IO)
            IF (IO .NE. 0) GOTO 830
            CALL WRYTE (NDSDAT, LENGRIB, CGRIB)
#endif
            !
          END IF
#ifdef W3_NCEP2
          LISTSEC0(1) = 10
          KPDS(1)     = 0
#endif
          !
          ! ... End of fields loop
          !
        END IF
      END DO
    END DO
    !
    RETURN
    !
    ! Error escape locations
    !
#ifdef W3_NCEP2
810 CONTINUE
    WRITE (NDSE,1010) IO
    CALL EXTCDE ( 20 )
820 CONTINUE
    WRITE (NDSE,1020) IO
    CALL EXTCDE ( 30 )
830 CONTINUE
    WRITE (NDSE,1030) IO
    CALL EXTCDE ( 40 )
#endif
    !
    ! Formats
    !
999 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGB :'/                &
         '     PLEASE UPDATE FIELDS !!! '/)
    !
#ifdef W3_NCEP2
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGB : '/               &
         '     ERROR IN OPENING OUTPUT FILE'/                   &
         '     IOSTAT =',I5/)
#endif
    !
#ifdef W3_NCEP2
1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGB : '/               &
         '     ERROR CREATING NEW GRIB2 FIELD'/                 &
         '     IOSTAT =',I5/)
#endif
    !
#ifdef W3_NCEP2
1020 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGB : '/               &
         '     ERROR ADDING GRIB2 FIELD'/                       &
         '     IOSTAT =',I5/)
#endif
    !
#ifdef W3_NCEP2
1030 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXGB : '/               &
         '     ERROR ENDING GRIB2 MESSAGE'/                     &
         '     IOSTAT =',I5/)
#endif
    !
#ifdef W3_T
9000 FORMAT (' TEST W3EXGB : FLAGS  :',40L2)
9001 FORMAT (' TEST W3EXGB : NDSDAT :',I4/                        &
         '               KPDS   :',13I4/                      &
         '                       ',12I4/                      &
         '               KGDS   :',8I6/                       &
         '                       ',8I6/                       &
         '                       ',6I6)
#endif
    !
#ifdef W3_T
9012 FORMAT (' TEST W3EXGB : BLOK PARS    : ',3I4)
9014 FORMAT ('           BASE NAME : ',A)
#endif
    !
#ifdef W3_T
9020 FORMAT (' TEST W3EXGB : OUTPUT FIELD : ',A)
#endif
    !/
    !/ End of W3EXGB ----------------------------------------------------- /
    !/
  END SUBROUTINE W3EXGB
  !/
  !/ End of W3GRIB ----------------------------------------------------- /
  !/
END PROGRAM W3GRIB
