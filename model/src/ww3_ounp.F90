!> @file
!> @brief Contains program W3OUNP.
!>
!> @author F Ardhuin
!> @author M Accensi
!> @date 06-Sep-2021

#include "w3macros.h"
!/ ------------------------------------------------------------------- /

!> @brief Post-processing of point output.
!>
!> @details Data is read from the grid output file out_pnt.ww3 (raw data)
!>  and from the file ww3_ounp.nml or ww3_ounp.inp ( NDSI).
!>  Model definition and raw data files are read using WAVEWATCH III
!>  subroutines.
!>
!> @author F. Ardhuin
!> @author M. Accensi
!> @date 06-Sep-2021
!>
PROGRAM W3OUNP
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           F. Ardhuin              |
  !/                  |           M. Accensi              |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         06-Sep-2021 |
  !/                  +-----------------------------------+
  !/
  !/    14-Jan-1999 : Final FORTRAN 77                    ( version 1.18 )
  !/    21-Jan-2000 : Upgrade to FORTRAN 90               ( version 2.00 )
  !/    14-Feb-2000 : Exact nonlinear interactions        ( version 2.01 )
  !/    09-Jan-2001 : U* bug fix in tabular output        ( version 2.05 )
  !/    25-Jan-2001 : Flat grid version                   ( version 2.06 )
  !/    02-Feb-2001 : Xnl version 3.0                     ( version 2.07 )
  !/    11-Jun-2001 : Clean up                            ( version 2.11 )
  !/    11-Oct-2001 : Clean up, X*, Y* in tables          ( version 2.14 )
  !/    13-Nov-2002 : Add stress vector                   ( version 3.00 )
  !/    27-Nov-2002 : First version of VDIA and MDIA      ( version 3.01 )
  !/    24-Dec-2004 : Multiple grid version.              ( version 3.06 )
  !/    17-Apr-2006 : Filter for directional spread.      ( version 3.09 )
  !/    23-Jun-2006 : Linear input added.                 ( version 3.09 )
  !/    28-Jun-2006 : Adding file name preamble.          ( version 3.09 )
  !/    03-Jul-2006 : Separate flux modules.              ( version 3.09 )
  !/    28-Oct-2006 : Add partitioning option.            ( version 3.10 )
  !/    24-Mar-2007 : Add pars for entire spectrum.       ( version 3.11 )
  !/    25-Apr-2007 : Battjes-Janssen Sdb added.          ( version 3.11 )
  !/                  (J. H. Alves)
  !/    08-Aug-2007 : Creation of buoy log file added     ( version 3.12 )
  !/                  (switch O14 -- A. Chawla)
  !/    09-Oct-2007 : WAM 4+ Sin and Sds added.           ( version 3.13 )
  !/                  (F. Ardhuin)
  !/    09-Oct-2007 : Experimental Sbs (BS1) added.       ( version 3.13 )
  !/                  (F. Ardhuin)
  !/    09-Apr-2008 : Adding an additional output for     ( version 3.12 )
  !/                  WMO standard (A. Chawla)
  !/    29-Apr-2008 : Adjust format partition output.     ( version 3.14 )
  !/    29-May-2009 : Preparing distribution version.     ( version 3.14 )
  !/    30-Oct-2009 : Implement run-time grid selection.  ( version 3.14 )
  !/                  (W. E. Rogers & T. J. Campbell, NRL)
  !/    24-Mar-2011 : Adaptation to NetCDF (M. Accensi)   ( version 4.04 )
  !/    16-Jul-2011 : NC3 / NC4 switch     (M. Accensi)   ( version 4.05 )
  !/    14-Mar-2013 : Writing optimization (M. Accensi)   ( version 4.09 )
  !/    04-Jun-2014 : Correct bug TOGETHER (M. Accensi)   ( version 5.00 )
  !/    04-Jun-2014 : Update use of date   (M. Accensi)   ( version 5.00 )
  !/    13-Jun-2014 : Dimension order opt. (M. Accensi)   ( version 5.00 )
  !/    18-Jun-2014 : add mpi implementat. (M. Accensi)   ( version 5.00 )
  !/    27-Aug-2015 : Sice add as additional output       ( version 5.10 )
  !/                  (in source terms)
  !/    15-May-2018 : Add namelist feature                ( version 6.05 )
  !/    18-Aug-2018 : S_{ice} IC5 (Q. Liu)                ( version 6.06 )
  !/    18-Jun-2020 : Support for 360-day calendar.       ( version 7.08 )
  !/    19-Jul-2021 : Momentum and air density support    ( version 7.14 )
  !/    06-Sep-2021 : scale factor on spectra output      ( version 7.12 )
  !/    05-Jan-2022 : Added TIMESPLIT=0 (nodate) support  ( version 7.14 )
  !/    21-Jul-2022 : Correct FP0 calc for peak energy in ( version 7.14 )
  !/                  min/max freq band (B. Pouliot, CMC)
  !/
  !/    Copyright 2009 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Post-processing of point output.
  !
  !  2. Method :
  !
  !     Data is read from the grid output file out_pnt.ww3 (raw data)
  !     and from the file ww3_ounp.nml or ww3_ounp.inp ( NDSI).
  !     Model definition and raw data files are read using WAVEWATCH III
  !     subroutines.
  !
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
  !      W3NOUT    Subr. W3ODATMD Set number of model for output.
  !      W3SETO    Subr.   Id.    Point to selected model for output.
  !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  !      STRACE    Subr.   Id.    Subroutine tracing.
  !      NEXTLN    Subr.   Id.    Get next line from input filw
  !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  !      STME21    Subr. W3TIMEMD Convert time to string.
  !      TICK21    Subr.   Id.    Advance time.
  !      DSEC21    Func.   Id.    Difference between times.
  !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  !      W3IOPO    Subr. W3IOPOMD Reading/writing raw point output file.
  !      W3EXNC    Subr. Internal Execute point output.
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
  !     - Tables written to file 'tabNN.ww3', where NN is the
  !       unit umber (NDSTAB).
  !     - Transfer file written to ww3.yymmddhh.spc with multiple
  !       spectra and times in file. yymmddhh relates to first
  !       output (NDSTAB).
  !     - !/IC1 !/IC2 !/IC3 !/IC5 are not included in dissipation term
  !       FIXME: ICE is a dummy variable at the moment
  !              Include ice parameters in point output file out_pnt.ww3
  !              Ice coupling to SIN, SDS and SIC similar to w3srcemd.ftn
  !
  !  8. Structure :
  !
  !     See source code.
  !
  !     TOUT is the time defined in the input file
  !     TIME is the time read from the out_pnt.ww3 file
  !     DTREQ is the stride used for the time steps
  !     at the beginning, if TOUT is after TIME, the program will read
  !     out_pnt.ww3 DTREQ by DTREQ until TIME is equal to TOUT
  !     /!\ if DTREQ is too big, it's possible to never have TIME=TOUT /!\
  !
  !     PASTDATE is the date of the last time step
  !     DATE is the date of the current time step
  !     IOUT is the counter of time iteration of a same file
  !
  !     MFL is the number of stations processed in the 'time' loop
  !     NOPTS is the total number of stations defined in out_pnt.ww3
  !     NFL is the number of bunch of MFL stations to loop on to
  !     process all the NOPTS stations
  !     NREQ is the number of valid stations to process, unvalid stations
  !     are duplicata or stations not specified in the input file
  !
  !  9. Switches :
  !
  !       !/S    Enable subroutine tracing.
  !
  !       !/NCO  NCEP NCO modifications for operational implementation.
  !
  !       !/O14  Buoy log file generation.
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  USE CONSTANTS
  !/
  !     USE W3GDATMD, ONLY: W3NMOD, W3SETG
  USE W3WDATMD, ONLY: W3SETW, W3NDAT
#ifdef W3_NL1
  USE W3ADATMD, ONLY: W3SETA, W3NAUX
#endif
  USE W3ODATMD, ONLY: W3SETO, W3NOUT
  USE W3ODATMD, ONLY: IAPROC, NAPROC, NAPERR, NAPOUT, DIMP
  USE W3IOGRMD, ONLY: W3IOGR
  USE W3IOPOMD
  USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE, STRSPLIT
#ifdef W3_S
  USE W3SERVMD, ONLY : STRACE
#endif
  USE W3TIMEMD, ONLY: CALTYPE, STME21, TICK21, DSEC21, T2D, TSUB, U2D
  !/
  USE W3GDATMD
  USE W3WDATMD, ONLY: TIME
  USE W3ODATMD, ONLY: NDSE, NDSO, NOPTS, PTLOC, PTNME,     &
       DPO, WAO, WDO, ASO, CAO, CDO, SPCO, FNMPRE,&
       IPASS => IPASS2, ICEFO, ICEO, ICEHO
#ifdef W3_FLX5
  USE W3ODATMD, ONLY: TAUAO, TAUDO, DAIRO
#endif
#ifdef W3_T
  USE W3ODATMD, ONLY: NDST
#endif
#ifdef W3_SETUP
  USE W3ODATMD, ONLY: ZET_SETO
#endif
  !
#ifdef W3_O14
  USE W3ODATMD, ONLY: GRDID
#endif
  !
  USE W3NMLOUNPMD
  USE NETCDF
  !
  IMPLICIT NONE
  !
#ifdef W3_MPI
  INCLUDE "mpif.h"
#endif
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
  TYPE(NML_POINT_T)       :: NML_POINT
  TYPE(NML_FILE_T)        :: NML_FILE
  TYPE(NML_SPECTRA_T)     :: NML_SPECTRA
  TYPE(NML_PARAM_T)       :: NML_PARAM
  TYPE(NML_SOURCE_T)      :: NML_SOURCE
  !
  INTEGER                 :: NDSI, NDSM, NDSOP,  NDSTRC, NTRACE,  &
       IERR, I, NOUT, NREQ, ITYPE, OTYPE,   &
       IPOINT, IOTEST, ITH, IOUT, J, DIMXP, &
       ICODE, STRL, STRL2, FLWW3, NBFILEOUT,&
       S5, S3, NBSTATION, NCTYPE,           &
       NCFLUSH, NFL, MFL, IFL, NREQL, NOUTL,&
       NDSEN, ONE, TWO, IRET, IP, NCVARTYPE
  INTEGER                 :: ISCALE = 0
  INTEGER                 :: DIMID(7), DIMLN(5), VARID(28),       &
       STARTDATE(8), STOPDATE(8),           &
       TOUT(2), TDUM(2), TOUTL(2)
#ifdef W3_MPI
  INTEGER                 :: IERR_MPI
#endif
#ifdef W3_O14
  INTEGER                 :: NDBO
#endif
#ifdef W3_S
  INTEGER, SAVE           :: IENT   = 0
#endif
#ifdef W3_NCO
  INTEGER                 :: NDSTAB
#endif
#if defined W3_NCO && !defined W3_T
  INTEGER                 :: NDST
#endif
  !
  INTEGER, ALLOCATABLE    :: INDREQ(:), INDREQTMP(:)
  INTEGER,ALLOCATABLE     :: NCID(:)
  !
  REAL                    :: DTREQ, SCALE1, SCALE2, DTEST
  REAL                    :: M2KM
  REAL                    :: DTHD,RTH0
  !
  REAL,ALLOCATABLE        :: THD(:)
  REAL, ALLOCATABLE       :: XPART(:,:)
  !
  CHARACTER(LEN=16)            :: DATE, PASTDATE
  CHARACTER(LEN=30)            :: FILEPREFIX, STRSTARTDATE, STRSTOPDATE
  CHARACTER                    :: COMSTR*1, IDTIME*23, IDDDAY*11,      &
       FILETIME*16, GLOBALATT*120,          &
       ATTNAME*120, ATTVAL*120
  CHARACTER(LEN=20)            :: FORMAT1
  CHARACTER(LEN=8)             :: EXT
  CHARACTER(LEN=128)           :: NCNAME
  CHARACTER(LEN=25)            :: IDSRCE(7)
  CHARACTER                    :: SEP
  !
  CHARACTER(LEN=100),ALLOCATABLE      :: POINTLIST(:)
  CHARACTER(LEN=128),ALLOCATABLE      :: NCFILE(:)
  !
  LOGICAL                 :: FLSRCE(7)
  LOGICAL                 :: TOGETHER, ORDER, FLGNML
  LOGICAL, ALLOCATABLE    :: FLREQ(:)
  !
  !/
  !/ ------------------------------------------------------------------- /
  !/
  DATA IDSRCE / 'Spectrum                 ' ,                     &
       'Wind-wave interactions   ' ,                     &
       'Nonlinear interactions   ' ,                     &
       'Dissipation              ' ,                     &
       'Wave-bottom interactions ' ,                     &
       'Wave-ice interactions    ' ,                     &
       'Sum of selected sources  ' /
  FLSRCE = .FALSE.
  !
#ifdef W3_NCO
  !     CALL W3TAGB('WAVESPEC',1998,0007,0050,'NP21   ')
#endif
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 1.a  IO set-up.
  !
  CALL W3NMOD ( 1, 6, 6 )
  CALL W3SETG ( 1, 6, 6 )
  CALL W3NDAT (    6, 6 )
  CALL W3SETW ( 1, 6, 6 )
#ifdef W3_NL1
  CALL W3NAUX (    6, 6 )
  CALL W3SETA ( 1, 6, 6 )
#endif
  CALL W3NOUT (    6, 6 )
  CALL W3SETO ( 1, 6, 6 )
  !
  NDSI   = 10
  NDSM   = 20
  NDSOP  = 20
  !
  NDSTRC =  6
  NTRACE = 10
  CALL ITRACE ( NDSTRC, NTRACE )
  !
#ifdef W3_S
  CALL STRACE (IENT, 'W3OUNP')
#endif
  !
#ifdef W3_NCO
  !
  ! Redo according to NCO
  !
  NDSI   = 11
  NDSO   =  6
  NDSE   = NDSO
# ifndef W3_T
  NDST   = NDSO
# endif
  NDSM   = 12
  NDSOP  = 13
#endif
#ifdef W3_O14
  NDBO   = 14
#endif
#ifdef W3_NCO
  NDSTRC = NDSO
#endif
  !
  !
  ! 1.b MPP initializations
  !
#ifdef W3_SHRD
  NAPROC = 1
  IAPROC = 1
#endif
  !
#ifdef W3_MPI
  CALL MPI_INIT      ( IERR_MPI )
  CALL MPI_COMM_SIZE ( MPI_COMM_WORLD, NAPROC, IERR_MPI )
  CALL MPI_COMM_RANK ( MPI_COMM_WORLD, IAPROC, IERR_MPI )
  IAPROC = IAPROC + 1  ! this is to have IAPROC between 1 and NAPROC
#endif
  !
  IF ( IAPROC .EQ. NAPERR ) THEN
    NDSEN  = NDSE
  ELSE
    NDSEN  = -1
  END IF
  !
  IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,900)
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 2.  Read model definition file.
  !
  CALL W3IOGR ( 'READ', NDSM )
  IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,920) GNAME
  !
  IF ( FLAGLL ) THEN
    M2KM = 1.
  ELSE
    M2KM = 1.E-3
  END IF
  !
  DIMXP  = ((NK+1)/2) * ((NTH-1)/2)
  ALLOCATE ( XPART(DIMP,0:DIMXP) )
  XPART = UNDEF
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 3.  Read general data and first fields from file
  !
#if W3_BIN2NC
  CALL W3IOPON ( 'READ', NDSOP, IOTEST )
#else
  CALL W3IOPO ( 'READ', NDSOP, IOTEST )
#endif
  !
  IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930)
  DO I=1, NOPTS
    IF ( FLAGLL ) THEN
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,931) PTNME(I), M2KM*PTLOC(1,I), M2KM*PTLOC(2,I)
    ELSE
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) PTNME(I), M2KM*PTLOC(1,I), M2KM*PTLOC(2,I)
    END IF
  END DO
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 4.  Read requests from input file.
  !

  !
  ! process ww3_ounp namelist
  !
  INQUIRE(FILE=TRIM(FNMPRE)//"ww3_ounp.nml", EXIST=FLGNML)
  IF (FLGNML) THEN
    ! Read namelist
    CALL W3NMLOUNP (NDSI, TRIM(FNMPRE)//'ww3_ounp.nml', NML_POINT, NML_FILE, &
         NML_SPECTRA, NML_PARAM, NML_SOURCE, IERR)

    ! 4.1 Time setup IDTIME, DTREQ, NOUT
    READ(NML_POINT%TIMESTRIDE, *)  DTREQ
    READ(NML_POINT%TIMECOUNT, *)   NOUT
    READ(NML_POINT%TIMESTART, *)   TOUT(1), TOUT(2)

    ! 4.2 Output points NOPTS
    ALLOCATE(POINTLIST(NOPTS+1))
    POINTLIST(:)=''
    CALL STRSPLIT(NML_POINT%LIST,POINTLIST)
    !
    ALLOCATE ( FLREQ(NOPTS) )
    ALLOCATE ( INDREQTMP(NOPTS) )
    FLREQ = .FALSE.
    NREQ   = 0
    ALLOCATE (NCFILE(NOPTS))
    ALLOCATE (NCID(NOPTS))
    NBSTATION = 1
    ! full list of point indexes
    IF (TRIM(POINTLIST(1)).EQ.'all') THEN
      FLREQ = .TRUE.
      NREQ = NOPTS
      INDREQTMP=(/(J,J=1,NREQ)/)
      ! user defined list of point indexes
    ELSE
      IP=0
      DO WHILE (LEN_TRIM(POINTLIST(IP+1)).NE.0)
        IP=IP+1
        READ(POINTLIST(IP),*) IPOINT
        ! existing index in out_pnt.ww3
        IF ((IPOINT .LE. NOPTS) .AND. (NREQ .LT. NOPTS)) THEN
          IF ( .NOT. FLREQ(IPOINT) ) THEN
            NREQ = NREQ + 1
            INDREQTMP(NREQ)=IPOINT
          END IF
          FLREQ(IPOINT) = .TRUE.
        END IF
      END DO
    END IF

    ! 4.3 Output type
    FLWW3 = 0
    FILEPREFIX = NML_FILE%PREFIX
    NCTYPE = NML_FILE%NETCDF
    S3 = NML_POINT%TIMESPLIT
    TOGETHER = NML_POINT%SAMEFILE
    MFL = NML_POINT%BUFFER
    ITYPE = NML_POINT%TYPE
    ORDER = NML_POINT%DIMORDER
    !
    IF (ITYPE .EQ. 1) THEN
      OTYPE = NML_SPECTRA%OUTPUT
      SCALE1 = NML_SPECTRA%SCALE_FAC
      SCALE2 = NML_SPECTRA%OUTPUT_FAC
      NCVARTYPE = NML_SPECTRA%TYPE
    ELSE IF (ITYPE .EQ. 2) THEN
      OTYPE = NML_PARAM%OUTPUT
    ELSE IF (ITYPE .EQ. 3) THEN
      OTYPE = NML_SOURCE%OUTPUT
      SCALE1 = NML_SOURCE%SCALE_FAC
      SCALE2 = NML_SOURCE%OUTPUT_FAC
      FLSRCE(1) = NML_SOURCE%SPECTRUM
      FLSRCE(2) = NML_SOURCE%INPUT
      FLSRCE(3) = NML_SOURCE%INTERACTIONS
      FLSRCE(4) = NML_SOURCE%DISSIPATION
      FLSRCE(5) = NML_SOURCE%BOTTOM
      FLSRCE(6) = NML_SOURCE%ICE
      FLSRCE(7) = NML_SOURCE%TOTAL
      ISCALE = NML_SOURCE%TABLE_FAC
    END IF


  END IF ! FLGNML

  !
  ! process old ww3_ounp.inp format
  !
  IF (.NOT. FLGNML) THEN
    OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_ounp.inp',STATUS='OLD',ERR=800,IOSTAT=IERR)
    REWIND (NDSI)

    READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
    IF (COMSTR.EQ.' ') COMSTR = '$'
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR
    CALL NEXTLN ( COMSTR , NDSI , NDSE )

    ! 4.1 Time setup IDTIME, DTREQ, NOUT
    READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT

    ! 4.2 Output points NOPTS
    ALLOCATE ( FLREQ(NOPTS) )
    ALLOCATE ( INDREQTMP(NOPTS) )
    FLREQ = .FALSE.
    NREQ   = 0
    ALLOCATE (NCFILE(NOPTS))
    ALLOCATE (NCID(NOPTS))
    NBSTATION = 1
    !
    DO I=1, NOPTS
      ! reads point index
      CALL NEXTLN ( COMSTR , NDSI , NDSE )
      READ (NDSI,*,END=801,ERR=802) IPOINT
      ! last index
      IF (IPOINT .LT. 0) THEN
        IF (I.EQ.1) THEN
          FLREQ = .TRUE.
          NREQ = NOPTS
          INDREQTMP=(/(J,J=1,NREQ)/)
        END IF
        EXIT
      END IF
      ! existing index in out_pnt.ww3
      IF ( (IPOINT .GT. 0) .AND. (IPOINT .LE. NOPTS) ) THEN
        IF ( .NOT. FLREQ(IPOINT) ) THEN
          NREQ = NREQ + 1
          INDREQTMP(NREQ)=IPOINT
        END IF
        FLREQ(IPOINT) = .TRUE.
      END IF
      ! read the 'end of list' if nopts reached before it
      IF ( (IPOINT .GT. 0) .AND. (NREQ .EQ. NOPTS) ) THEN
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=801,ERR=802) IPOINT
      END IF
    END DO
    ! check if last point index is -1
    IF (IPOINT .NE. -1) THEN
      WRITE (NDSE,1007)
      CALL EXTCDE ( 47 )
    END IF

    ! 4.3 Output type
    FILEPREFIX= 'ww3.'
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) FILEPREFIX
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) S3
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) NCTYPE
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) TOGETHER, MFL
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) ITYPE
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) FLWW3
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=801,ERR=802) ORDER
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    !
    IF (ITYPE .EQ. 1) READ (NDSI,*,END=801,ERR=802) OTYPE, SCALE1, SCALE2, NCVARTYPE
    IF (ITYPE .EQ. 2) READ (NDSI,*,END=801,ERR=802) OTYPE
    IF (ITYPE .EQ. 3) READ (NDSI,*,END=801,ERR=802) OTYPE, SCALE1, SCALE2, FLSRCE, ISCALE

    CLOSE(NDSI,ERR=800,IOSTAT=IERR)

  END IF ! .NOT. FLGNML

  !

  ! 4.1 Time setup IDTIME, DTREQ, NOUT
  DTREQ  = MAX ( 0. , DTREQ )
  IF ( DTREQ.EQ.0 ) NOUT = 1
  NOUT   = MAX ( 1 , NOUT )
  NOUTL = NOUT
  TOUTL = TOUT
  CALL STME21 ( TOUT , IDTIME )
  IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,940) IDTIME
  TDUM   = 0
  CALL TICK21 ( TDUM , DTREQ )
  CALL STME21 ( TDUM , IDTIME )
  IF ( DTREQ .GE. 86400. ) THEN
    WRITE (IDDDAY,'(I10,1X)') INT(DTREQ/86400.)
  ELSE
    IDDDAY = '           '
  END IF
  IDTIME(1:11) = IDDDAY
  IDTIME(21:23) = '   '
  IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) IDTIME, NOUT


  ! 4.1.2 Selects first time FILETIME between out_pnt.ww3 and ww3_ounp.nml
  IF (TOUT(1).GT.TIME(1) .OR. (TOUT(1).EQ.TIME(1) .AND. TOUT(2).GT.TIME(2))) THEN
    WRITE(DATE,'(I8.8,I6.6)') TOUT(1), TOUT(2)
  ELSE
    WRITE(DATE,'(I8.8,I6.6)') TIME(1), TIME(2)
  END IF
  WRITE(FILETIME,'(8A)') DATE(1:4), DATE(5:6), DATE(7:8), 'T', DATE(9:10), 'Z'


  ! 4.1.3 Loops on TIME from out_pnt file to reach the first time PASTDATE
  DTEST  = DSEC21 ( TIME , TOUT )
  DO WHILE (DTEST.NE.0)
    DTEST  = DSEC21 ( TIME , TOUT )
    IF ( DTEST .GT. 0. ) THEN
#ifdef W3_BIN2NC
      CALL W3IOPON ( 'READ', NDSOP, IOTEST )
#else 
      CALL W3IOPO ( 'READ', NDSOP, IOTEST )
#endif
      IF ( IOTEST .EQ. -1 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,949)
        GOTO 888
      END IF
      CYCLE
    END IF
    IF ( DTEST .LT. 0. ) THEN
      CALL TICK21 ( TOUT , DTREQ )
      CYCLE
    END IF
  END DO
  WRITE(PASTDATE,'(I8.8,I6.6)') TIME(1), TIME(2)


  ! 4.2 Output points NOPTS
  ALLOCATE ( INDREQ(NREQ) )
  INDREQ(:)=INDREQTMP(1:NREQ)
  DEALLOCATE(INDREQTMP)


  ! 4.3 Output type
  !
  ! S3 defines the number of characters in the date for the filename
  ! S3=0 -> empty, S3=4 -> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDHH
  !
  ! Setups min and max date format
  IF (S3.GT.0 .AND. S3.LT.4) S3=4
  IF (S3.GT.10) S3=10
  !
  ! Defines the format of FILETIME as ISO8601 convention
  S5=S3-8
  ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHMMSSZ'
  IF (S3.EQ.0) THEN
    FILETIME = ''
  ELSE IF (S3.EQ.10) THEN
    WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I8.8,A1,I',S5,'.',S5,',A1)'
    WRITE (FILETIME,FORMAT1) TIME(1), 'T', &
         FLOOR(REAL(TIME(2))/NINT(10.**(6-S5))), 'Z'
    ! if S3=>YYYYMMDD then filetime='YYYYMMDD'
  ELSE IF (S3.EQ.8) THEN
    WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
    WRITE (FILETIME,FORMAT1) TIME(1)
    ! if S3=>YYYYMM then filetime='YYYYMM'
    ! or S3=>YYYY then filetime='YYYY'
  ELSE
    WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
    WRITE (FILETIME,FORMAT1) FLOOR(REAL(TIME(1))/NINT(10.**(8-S3)))
  END IF
  !
  ! order time,station
  IF (ORDER) THEN
    ONE=1
    TWO=2
    ! order station,time
  ELSE
    ONE=2
    TWO=1
  END IF
  !
  IF ((NCTYPE.EQ.3) .AND. (.NOT.ORDER)) GOTO 803
  IF ((NCTYPE.EQ.4) .AND. INDEX(NF90_INQ_LIBVERS(),'"3.').NE.0) GOTO 804


  ! 4.4 Converts direction unit in degree
  ALLOCATE(THD(NTH))
  DTHD=360./NTH
  RTH0=TH(1)/DTH
  DO ITH=1, NTH
    THD(ITH)=DTHD*(RTH0+REAL(ITH-1))
  END DO
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 5.  Now creates files
  !     If too many (memory problem) then makes several reads
  !


  ! 5.1 Defines number of files/stations per file NFL
  IF (TOGETHER) THEN
    NFL=1
  ELSE
    NFL=1+NOPTS/MFL
  END IF


  ! 5.2 Creates filename listing
  SEP = '_'
  IF(S3 .EQ. 0) SEP = '' ! No "_" separator if no datetime string.
  WRITE(EXT,'(A)') ''
  IF ((ITYPE .EQ. 1) .AND. (OTYPE.EQ.2)) WRITE(EXT,'(A,A)') TRIM(SEP), 'tab.nc'
  IF ((ITYPE .EQ. 1) .AND. (OTYPE.EQ.3)) WRITE(EXT,'(A,A)') TRIM(SEP), 'spec.nc'
  IF ((ITYPE .EQ. 1) .AND. (OTYPE.EQ.4)) WRITE(EXT,'(A,A)') TRIM(SEP), 'tab.nc'
  IF  (ITYPE .EQ. 2)                     WRITE(EXT,'(A,A)') TRIM(SEP), 'tab.nc'
  IF ((ITYPE .EQ. 3) .AND. (OTYPE.EQ.2)) WRITE(EXT,'(A,A)') TRIM(SEP), 'tab.nc'
  IF ((ITYPE .EQ. 3) .AND. (OTYPE.EQ.3)) WRITE(EXT,'(A,A)') TRIM(SEP), 'tab.nc'
  IF ((ITYPE .EQ. 3) .AND. (OTYPE.EQ.4)) WRITE(EXT,'(A,A)') TRIM(SEP), 'src.nc'
  ! checks if extension exists
  IF (LEN_TRIM(EXT).EQ.0) THEN
    WRITE (NDSE,1006)
    CALL EXTCDE ( 46 )
  END IF

  ! 5.3 Redefines netCDF type
  IF((NCTYPE.EQ.4).AND.(.NOT.TOGETHER).AND.(NFL.GT.300).AND.(NREQ.GT.9000))  THEN
    WRITE(NDSO,'(A)') ' WARNING : Files will be generated in netCDF3 with NF90_share mode'
    WRITE(NDSO,'(A)') ' WARNING : this is due to NF90_sync memory problem with netCDF4 library'
    WRITE(NDSO,'(A)') ' WARNING : to convert in netCDF4, use ncks -h -a -4 -L 9 file.nc3 file.nc4'
    WRITE(NDSO,'(A)') ' WARNING : or use option "Points in same file" with value TRUE in .inp file'
    WRITE(NDSO,'(A)') ' WARNING : or limit the output points list to less than 300'
    NCTYPE=3
  END IF



  ! 5.4 Defines periodic flushing of buffer (only available for netCDF3)
  NCFLUSH=FLOOR(15E7/(FLOAT(NK)*FLOAT(NTH)*FLOAT(NREQ)/NFL))
  IF (NCTYPE.EQ.3.AND.NREQ.GT.10.AND.(.NOT.TOGETHER)) WRITE(NDSO,5940) NCFLUSH


  ! 5.5 Removes the duplicata if "ONE file per station" mode
  IF (.NOT.TOGETHER) THEN
    ! defines a file name per station (NOT TOGETHER)
    DO I=1,NOPTS
      IF (FLREQ(I)) THEN
        J = LEN_TRIM(FNMPRE)
        WRITE (NCNAME, '(5A)') TRIM(FILEPREFIX), TRIM(PTNME(I)),'_', TRIM(FILETIME), TRIM(EXT)
        WRITE(NCFILE(I),'(2A)') TRIM(FNMPRE(:J)), TRIM(NCNAME)  ! filename
        IF( SUM(index(NCFILE(:),NCFILE(I))).GT.1 ) THEN
          FLREQ(I)=.FALSE.
          WRITE(NDSO,5950) TRIM(PTNME(I))
          CYCLE
        END IF
      END IF  ! FLREQ(I)
    END DO  ! I=1,NOPTS
  END IF  ! .NOT.TOGETHER


  ! 5.6 Loops on bunch of stations NFL
  DO IFL=IAPROC,NFL,NAPROC
    !
    ! new file, so the time counter is initialized
560 CONTINUE
    IOUT=0


    ! 5.6.1 Redefines the filetime when it's a new date defined by the date division S3
    ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHMMSSZ'
    IF (S3.EQ.0) THEN
      FILETIME = ''
    ELSE IF (S3.EQ.10) THEN
      WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I8.8,A1,I',S5,'.',S5,',A1)'
      WRITE (FILETIME,FORMAT1) TIME(1), 'T', &
           NINT(REAL(TIME(2))/NINT(10.**(6-S5))), 'Z'
      ! if S3=>YYYYMMDD then filetime='YYYYMMDD'
    ELSE IF (S3.EQ.8) THEN
      WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
      WRITE (FILETIME,FORMAT1) TIME(1)
      ! if S3=>YYYYMM then filetime='YYYYMM'
      ! or S3=>YYYY then filetime='YYYY'
    ELSE
      WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
      WRITE (FILETIME,FORMAT1) NINT(REAL(TIME(1))/NINT(10.**(8-S3)))
    END IF


    ! 5.6.2 Defines the file names
    ! defines unique file name (TOGETHER)
    IF (TOGETHER) THEN
      WRITE (NCNAME, '(3A)') TRIM(FILEPREFIX), TRIM(FILETIME), TRIM(EXT)
      !IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1947) TRIM(NCNAME)
      J = LEN_TRIM(FNMPRE)
      WRITE(NCFILE(1),'(2A)') TRIM(FNMPRE(:J)), TRIM(NCNAME)  ! filename
    ELSE
      ! defines a file name per station (NOT TOGETHER)
      DO I=1,NOPTS
        IF (FLREQ(I)) THEN
          WRITE (NCNAME, '(5A)') TRIM(FILEPREFIX), TRIM(PTNME(I)),'_', TRIM(FILETIME), TRIM(EXT)
          !IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1947) TRIM(NCNAME)
          J = LEN_TRIM(FNMPRE)
          WRITE(NCFILE(I),'(2A)') TRIM(FNMPRE(:J)), TRIM(NCNAME)  ! filename
        END IF  ! FLREQ(I)
      END DO  ! I=1,NOPTS
    END IF  ! TOGETHER


    ! 5.6.3 Defines number of stations and files to CREATE
    ! together
    IF (TOGETHER) THEN
      NBFILEOUT = 1
      NBSTATION = NREQ
      NREQL=NBFILEOUT
      ! not together
    ELSE
      NBFILEOUT=MIN(MFL,NOPTS-(IFL-1)*MFL)
      NBSTATION = 1
      NREQL=0
      DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
        IF ( FLREQ(I) ) THEN
          NREQL = NREQL + 1
        END IF
      END DO
    END IF
    ! cycle if no file to CREATE
    IF (NREQL.EQ.0) CYCLE


    ! 5.6.4 Creates netcdf file

    ! ... ITYPE = 1
    IF (ITYPE .EQ. 1) THEN
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) ITYPE, '1-D and/or 2-D spectra, pass #',IFL

      ! ... OTYPE = 1
      IF (OTYPE .EQ. 1) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'print plots'
        IF ( SCALE1 .LT. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '1-D'
        ELSE IF ( SCALE1 .EQ. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '1-D'
        ELSE
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '1-D', SCALE1
        END IF
        IF ( SCALE2 .LT. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '2-D'
        ELSE IF ( SCALE2 .EQ. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '2-D'
        ELSE
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '2-D', SCALE2
        END IF

        ! ... OTYPE = 2
      ELSE IF ( OTYPE .EQ. 2 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Table of 1-D spectral data'
        DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
          IF (FLREQ(I) .OR. TOGETHER) THEN
            ! Create the netCDF file
            DIMLN(1)=NF90_UNLIMITED    ! time
            DIMLN(2)=NBSTATION         ! station
            DIMLN(3)=40                ! string station name length
            DIMLN(4)=NK                ! FREQ
            CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DIMLN,VARID,ONE,TWO)
          END IF
        END DO

        ! ... OTYPE = 3
      ELSE IF ( OTYPE .EQ. 3 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Transfer file'
        DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
          IF (FLREQ(I) .OR. TOGETHER) THEN
            ! Create the netCDF file
            DIMLN(1)=NF90_UNLIMITED  !time
            DIMLN(2)=NBSTATION ! station
            DIMLN(3)=40 ! string station name length
            DIMLN(4)=NK ! FREQ
            DIMLN(5)=NTH ! DIR
            CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DIMLN,VARID,ONE,TWO,NCVARTYPE=NCVARTYPE)
          END IF
        END DO

        ! ... OTYPE = 4
      ELSE IF ( OTYPE .EQ. 4 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Partitioning of spectra'
        DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
          IF (FLREQ(I) .OR. TOGETHER) THEN
            ! Create the netCDF file
            DIMLN(1)=NF90_UNLIMITED  !time
            DIMLN(2)=NBSTATION ! station
            DIMLN(3)=40    ! string station name length
            DIMLN(4)=DIMXP ! npart
            CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DIMLN,VARID,ONE,TWO)
          END IF
        END DO
      ELSE
        WRITE (NDSE,1011) OTYPE
        CALL EXTCDE ( 10 )
      END IF



      ! ... ITYPE = 2
    ELSE IF (ITYPE .EQ. 2) THEN
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) ITYPE, 'Table of mean wave parameters'
      DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
        IF (FLREQ(I) .OR. TOGETHER) THEN
          ! Create the netCDF file
          DIMLN(1)=NF90_UNLIMITED  !time
          DIMLN(2)=NBSTATION ! station
          DIMLN(3)=40    ! string station name length
          CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DIMLN,VARID,ONE,TWO)
        END IF
      END DO

      ! ... OTYPE = 1
      IF ( OTYPE .EQ. 1 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'depth, current and wind', NCNAME

        ! ... OTYPE = 2
      ELSE IF ( OTYPE .EQ. 2 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Mean wave parameters', NCNAME

        ! ... OTYPE = 3
      ELSE IF ( OTYPE .EQ. 3 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Nondimensional parameters (U*)', NCNAME

        ! ... OTYPE = 4
      ELSE IF ( OTYPE .EQ. 4 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Nondimensional parameters (U10)', NCNAME

        ! ... OTYPE = 5
      ELSE IF ( OTYPE .EQ. 5 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'Validation parameters', NCNAME

        ! ... OTYPE = 6
      ELSE IF ( OTYPE .EQ. 6 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,2940) 'WMO standard mean parameters', NCNAME
        ! ... OTYPE = ILLEGAL
      ELSE
        WRITE (NDSE,1011) OTYPE
        CALL EXTCDE ( 30 )
      END IF
      !
      DO I=1,6
        IF ( FLSRCE(I) .AND. IAPROC .EQ. NAPOUT ) WRITE (NDSO,3940) IDSRCE(I)
      END DO
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,*) ' '


      ! ... ITYPE = 3
    ELSE IF (ITYPE .EQ. 3) THEN
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) ITYPE, 'Source terms'
#ifdef W3_NCO
      NDSTAB = 51
#endif
      ISCALE = MAX ( 0 , MIN ( 5 , ISCALE ) )

      ! ... OTYPE = 1
      IF ( OTYPE .EQ. 1 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Print plots'
        IF ( SCALE1 .LT. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '1-D'
        ELSE IF ( SCALE1 .EQ. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '1-D'
        ELSE
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '1-D', SCALE1
        END IF
        IF ( SCALE2 .LT. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1940) '2-D'
        ELSE IF ( SCALE2 .EQ. 0.  ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1941) '2-D'
        ELSE
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,1942) '2-D', SCALE2
        END IF

        ! ... OTYPE = 2
        ! or  OTYPE = 3
      ELSE IF (( OTYPE .EQ. 2 ) .OR. ( OTYPE .EQ. 3 )) THEN
        IF ( ISCALE .LE. 2) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Tables as a function of freq.'
        ELSE
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Tables as a function of f/fp.'
        END IF
        IF ( MOD(ISCALE,3) .EQ. 1 ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,944) '(nondimensional based on U10)'
        ELSE IF ( MOD(ISCALE,3) .EQ. 2) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,944) '(nondimensional based on U*)'
        END IF

        DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
          IF (FLREQ(I) .OR. TOGETHER) THEN
            ! Create the netCDF file
            DIMLN(1)=NF90_UNLIMITED  !time
            DIMLN(2)=NBSTATION ! station
            DIMLN(3)=40    ! string station name length
            DIMLN(4)=NK ! freq
            CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DIMLN,VARID,ONE,TWO)
          END IF
        END DO

        ! ... OTYPE = 4
      ELSE IF ( OTYPE .EQ. 4 ) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) 'Transfer file'
        DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
          IF (FLREQ(I) .OR. TOGETHER) THEN
            ! Create the netCDF file
            DIMLN(1)=NF90_UNLIMITED  !time
            DIMLN(2)=NBSTATION ! station
            DIMLN(3)=40    ! string station name length
            DIMLN(4)=NK ! freq
            DIMLN(5)=NTH ! dir
            CALL W3CRNC(ITYPE,OTYPE,NCTYPE,NCFILE(I),NCID(I),DIMID,DIMLN,VARID,ONE,TWO,FLSRCE=FLSRCE)
          END IF
        END DO

        ! ... OTYPE = ILLEGAL
      ELSE
        WRITE (NDSE,1011) OTYPE
        CALL EXTCDE ( 20 )
      END IF


      ! ... ITYPE = ILLEGAL
    ELSE
      WRITE (NDSE,1010) ITYPE
      CALL EXTCDE ( 1 )
    END IF


    ! 5.6.5 Output of output points
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,950) NREQ
    ! together
    IF (TOGETHER) THEN
      DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBSTATION
        IF (FLREQ(I)) THEN
          IF ( FLAGLL ) THEN
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) PTNME(I), M2KM*PTLOC(1,I),   &
                 M2KM*PTLOC(2,I)
          ELSE
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,953) PTNME(I), M2KM*PTLOC(1,I),   &
                 M2KM*PTLOC(2,I)
          END IF
        END IF
      END DO
      ! not together
    ELSE
      DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
        IF (FLREQ(I)) THEN
          IF ( FLAGLL ) THEN
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) PTNME(I), M2KM*PTLOC(1,I),   &
                 M2KM*PTLOC(2,I)
          ELSE
            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,953) PTNME(I), M2KM*PTLOC(1,I),   &
                 M2KM*PTLOC(2,I)
          END IF
        END IF
      END DO
    END IF
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 6.  Time management.
    !
#ifdef W3_IC1
    WRITE(NDSO,3960)
#endif
#ifdef W3_IC2
    WRITE(NDSO,3960)
#endif
#ifdef W3_IC3
    WRITE(NDSO,3960)
#endif
#ifdef W3_IC5
    WRITE(NDSO,3960)
#endif
#ifdef W3_NL5
    WRITE(NDSO,3961)
#endif
    !
    CALL T2D(TIME,STARTDATE,IERR)
    WRITE(STRSTARTDATE,'(I4.4,A,4(I2.2,A),I2.2)') STARTDATE(1),'-',STARTDATE(2), &
         '-',STARTDATE(3),' ',STARTDATE(5),':',STARTDATE(6),':',STARTDATE(7)

    ! loops on TIME from out_pnt.ww3 till not reach TOUT from inp file
    DO
      DTEST = DSEC21 ( TIME , TOUT )
      IF ( DTEST .GT. 0. ) THEN
        ! reads TIME from out_pnt.ww3
#ifdef W3_BIN2NC
        CALL W3IOPON ( 'READ', NDSOP, IOTEST )
#else
        CALL W3IOPO ( 'READ', NDSOP, IOTEST )
#endif
        IF ( IOTEST .EQ. -1 ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,949)
          GOTO 700
        END IF
        CYCLE
      END IF
      IF ( DTEST .LT. 0. ) THEN
        CALL TICK21 ( TOUT , DTREQ )
        CYCLE
      END IF
      ! increment the time counter IOUT
      IOUT = IOUT + 1
      CALL STME21 ( TOUT , IDTIME )
      WRITE(DATE,'(I8.8,I6.6)') TOUT(1), TOUT(2)


      ! 6.1 Creates a new file if it is a new date defined by the date division S3
      IF ( (IOUT.GT.1) .AND. (INDEX(PASTDATE(1:S3),DATE(1:S3)).EQ.0) ) THEN
        WRITE(NDSO,954) TRIM(DATE(1:S3))
        ! decrements timesteps already processed
        NOUT=NOUT-(IOUT-1)
        GOTO 700
      END IF


      ! 6.2 Writes out a progress message
      IF (NREQ.GT.10.OR.NBFILEOUT.GT.10) WRITE(NDSO,955) TIME,    &
           NBFILEOUT, IOUT, NOUT, IFL
      J=0

      ! 6.3 Calls subroutine w3exnc for each file
      DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
        IF (FLREQ(I) .OR. TOGETHER) THEN
          ! together
          IF ( TOGETHER ) THEN
            CALL W3EXNC(I,NCID(I),NREQ,INDREQ,ORDER)
            ! not together
          ELSE
            J=J+1
            CALL W3EXNC(I,NCID(I),1,(/ I /),ORDER)
            ! flush buffer (only available in netcdf3)
            IF (MOD(IOUT,NCFLUSH).EQ.0) THEN
              IRET=NF90_SYNC(NCID(I))
            END IF
          END IF ! TOGETHER
        END IF ! (FLREQ(I) .OR. TOGETHER)
      END DO ! I=1+ ...
      !
      WRITE(PASTDATE,'(I8.8,I6.6)') TOUT(1), TOUT(2)
      CALL TICK21 ( TOUT , DTREQ )
      IF ( IOUT .GE. NOUT ) GOTO 700
      !
    END DO
    !
    GOTO 888


    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 7. Finalize file
    !
700 CONTINUE
    !
    CALL T2D(TIME,STOPDATE,IERR)
    WRITE(STRSTOPDATE,'(I4.4,A,4(I2.2,A),I2.2)') STOPDATE(1),'-',STOPDATE(2), &
         '-',STOPDATE(3),' ',STOPDATE(5),':',STOPDATE(6),':',STOPDATE(7)


    ! 7.1 Writes the global attributes to netCDF file
    DO I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT
      IF ( FLREQ(I) .OR. TOGETHER ) THEN
        IRET=NF90_REDEF(NCID(I))
        CALL CHECK_ERR(IRET,0)
        IF (FLWW3.EQ.0)                                      &
             OPEN(unit=994,file='NC_globatt.inp',status='old',iostat=ICODE)
        REWIND(994)
        IF (ICODE.EQ.0) THEN
          DO WHILE (ICODE.EQ.0)
            READ(994,'(a)',iostat=ICODE) ATTNAME
            READ(994,'(a)',iostat=ICODE) ATTVAL
            IF (ICODE.EQ.0) THEN
              STRL=LEN_TRIM(ATTNAME)
              STRL2=LEN_TRIM(ATTVAL)
              IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,ATTNAME(1:STRL),ATTVAL(1:STRL2))
              CALL CHECK_ERR(IRET,1)
            END IF
          END DO
        END IF
        CLOSE(994)
        !
        WRITE(GLOBALATT,'(A)') TRIM(NCFILE(I))
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'product_name' ,GLOBALATT(3:))
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'area',TRIM(GNAME))
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'data_type','OCO spectra 2D')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'format_version','1.1')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'southernmost_latitude','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'northernmost_latitude','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'latitude_resolution','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'westernmost_longitude','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'easternmost_longitude','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'longitude_resolution','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'minimum_altitude','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'maximum_altitude','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'altitude_resolution','n/a')
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'start_date',STRSTARTDATE)
        IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'stop_date',STRSTOPDATE)
        IF (DTREQ.EQ.3600)  THEN
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','hourly')
        ELSE IF (DTREQ.EQ.7200)  THEN
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','2-hourly')
        ELSE IF (DTREQ.EQ.10800)  THEN
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','3-hourly')
        ELSE IF (DTREQ.EQ.21600)  THEN
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','6-hourly')
        ELSE IF (DTREQ.EQ.32400)  THEN
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','9-hourly')
        ELSE IF (DTREQ.EQ.43200)  THEN
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','12-hourly')
        ELSE IF (DTREQ.EQ.86400)  THEN
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','daily')
        ELSE
          IRET=NF90_PUT_ATT(NCID(I),NF90_GLOBAL,'field_type','n/a')
        END IF
        !
        ! Close netCDF file
        IRET=NF90_ENDDEF(NCID(I))
        CALL CHECK_ERR(IRET,2)
        IRET=NF90_CLOSE(NCID(I))
        CALL CHECK_ERR(IRET,3)
        !
      END IF ! FLREQ(I) .OR. TOGETHER
    END DO ! I=1+(IFL-1)*MFL,(IFL-1)*MFL+NBFILEOUT


    ! 7.2 Goes back to the start of the loop with the same points
    ! but with a new date defined by the date division S3
    IF ( (IOUT.GT.1) .AND. (INDEX(PASTDATE(1:S3),DATE(1:S3)).EQ.0) ) THEN
      GOTO 560
    END IF


    ! 7.3 Reinitiazes TIME (close open out_pnt.ww3) and TOUT to process a new bunch of stations
    CLOSE(NDSOP) ! closes binary file out_pnt*
    IPASS = 0   ! resets time counter for binary file out_pnt*
#ifdef W3_BIN2NC
    CALL W3IOPON ( 'READ', NDSOP, IOTEST )
#else
    CALL W3IOPO ( 'READ', NDSOP, IOTEST )
#endif
#ifdef W3_T
    WRITE(NDSE,*) 'out_pnt* closed and reopened'
#endif
    TOUT=TOUTL
    NOUT=NOUTL


    ! 7.4 Loops on TIME till it is equal to TOUT
    DTEST  = DSEC21 ( TIME , TOUT )
    DO WHILE (DTEST.NE.0)
      DTEST  = DSEC21 ( TIME , TOUT )
      IF ( DTEST .GT. 0. ) THEN
#ifdef W3_BIN2NC
        CALL W3IOPON ( 'READ', NDSOP, IOTEST )
#else
        CALL W3IOPO ( 'READ', NDSOP, IOTEST )
#endif
        IF ( IOTEST .EQ. -1 ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,949)
          GOTO 700
        END IF
        CYCLE
      END IF
      IF ( DTEST .LT. 0. ) THEN
        CALL TICK21 ( TOUT , DTREQ )
        CYCLE
      END IF
    END DO
    !
  END DO ! IFL=1,NFL
  !
  GOTO 888
  !
  ! Escape locations read errors :
  !
800 CONTINUE
  WRITE (NDSE,1000) IERR
  CALL EXTCDE ( 40 )
  !
801 CONTINUE
  WRITE (NDSE,1001)
  CALL EXTCDE ( 41 )
  !
802 CONTINUE
  WRITE (NDSE,1002) IERR
  CALL EXTCDE ( 42 )
  !
803 CONTINUE
  WRITE (NDSE,1003)
  CALL EXTCDE ( 43 )
  !
804 CONTINUE
  WRITE (NDSE,1004) NF90_INQ_LIBVERS()
  CALL EXTCDE ( 44 )
  !
#ifdef W3_O14
805 CONTINUE
  WRITE (NDSE,1005) IERR
  CALL EXTCDE ( 45 )
#endif
  !
  !
888 CONTINUE
  !
  IF(ALLOCATED(THD)) DEALLOCATE(THD)
  IF(ALLOCATED(NCID)) DEALLOCATE(NCID)
  IF(ALLOCATED(NCFILE)) DEALLOCATE(NCFILE)
  IF(ALLOCATED(INDREQ)) DEALLOCATE(INDREQ)
  !
  IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,999)
#ifdef W3_MPI
  CALL MPI_FINALIZE  ( IERR_MPI )
#endif
  !
#ifdef W3_NCO
  !     CALL W3TAGE('WAVESPEC')
#endif
  !
  ! Formats
  !
900 FORMAT (/15X,'    *** WAVEWATCH III Point output post.***    '/ &
       15X,'==============================================='/)
901 FORMAT ( '  Comment character is ''',A,''''/)
  !
920 FORMAT ( '  Grid name : ',A/)
  !
930 FORMAT ( '  Points in file : '/                                 &
       ' ------------------------------------')
931 FORMAT ( '      ',A,2F10.2)
932 FORMAT ( '      ',A,2(F8.1,'E3'))
  !
940 FORMAT (/'  Output time data : '/                               &
       ' --------------------------------------------------'/ &
       '      First time         : ',A)
941 FORMAT ( '      Interval           : ',A/                       &
       '      Number of requests : ',I8)
942 FORMAT (/'  Output type ',I2,' :'/                              &
       ' --------------------------------------------------'/ &
       '      ',A, I3 /)
943 FORMAT ( '      Subtype   : ',A)
944 FORMAT ( '                  ',A)
#ifdef W3_O14
945 FORMAT ( '      ',I5,3X,A,2F10.2,3X,A)
#endif
949 FORMAT (/'      End of file reached '/)
  !
950 FORMAT (/'  Requested output for ',I6,' points : '/             &
       ' --------------------------------------------------')
951 FORMAT ( '      ',A,2F10.2)
953 FORMAT ( '      ',A,2(F8.1,'E3'))
954 FORMAT (/'      New time step      : ',A)
955 FORMAT ( '  Processing time : ', 2I8, ' in ', I8, 'files. Step '&
       I10, 'out of ', I10, ' pass ', I4)
  !
1940 FORMAT ( '      ',A,' print plots not requested.')
1941 FORMAT ( '      ',A,' print plots normalized.')
1942 FORMAT ( '      Scale factor ',A,' spectrum : ',E10.3)
  ! 1947 FORMAT ( '      File name : ',A)
  !
2940 FORMAT ( '      Table output : ',A/                             &
       '      File name    : ',A)
  !
3940 FORMAT ( '                        ',A)
5940 FORMAT ( '      Buffer will be flushed every ',I6,' steps.')
5950 FORMAT ( '      Point already exists, it will be skipped : ', A)
  !
999 FORMAT (/'  End of program '/                                   &
       ' ========================================='/          &
       '         WAVEWATCH III Point output '/)
  !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
       '     ERROR IN OPENING INPUT FILE'/                    &
       '     IOSTAT =',I5/)
  !
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
       '     PREMATURE END OF INPUT FILE'/)
  !
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
       '     ERROR IN READING FROM INPUT FILE'/               &
       '     IOSTAT =',I5/)
  !
1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
       '     NCTYPE=3 IS INCOMPATIBLE WITH'/                  &
       '     THE OPTIMIZED DIMENSION ORDER'/)
  !
1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
       '     NCTYPE=4 IS INCOMPATIBLE WITH'/                  &
       '     NETCDF LIBRARY USED :',A/)
  !
#ifdef W3_O14
1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/          &
       '     ERROR IN OPENING BUOY LOG FILE'/            &
       '     IOSTAT =',I5/)
#endif
  !
1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/              &
       '     ITYPE AND OTYPE COMBINATION NOT RECOGNIZED'/)
  !
1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/              &
       '     ERROR IN READING FROM INPUT FILE'/              &
       '     LAST POINT INDEX IS NOT -1'/                    &
       '     OR TOO MANY POINT INDEXES DEFINED'/)
  !
1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
       '     ILLEGAL TYPE, ITYPE =',I4/)
  !
1011 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNP : '/               &
       '     ILLEGAL TYPE, OTYPE =',I4/)
  !
#ifdef W3_IC1
3960 FORMAT (/' *** WAVEWATCH-III WARNING IN W3OUNP :'/         &
       '     Ice source terms !/IC1 skipped'/            &
       '     in dissipation term.'/)
#endif
#ifdef W3_IC2
3960 FORMAT (/' *** WAVEWATCH-III WARNING IN W3OUNP :'/         &
       '     Ice source terms !/IC2 skipped'/            &
       '     in dissipation term.'/)
#endif
#ifdef W3_IC3
3960 FORMAT (/' *** WAVEWATCH-III WARNING IN W3OUNP :'/         &
       '     Ice source terms !/IC3 skipped'/            &
       '     in dissipation term.'/)
#endif
#ifdef W3_IC5
3960 FORMAT (/' *** WAVEWATCH-III WARNING IN W3OUNP :'/         &
       '     Ice source terms !/IC5 skipped'/            &
       '     in dissipation term.'/)
#endif
#ifdef W3_NL5
3961 FORMAT (/' *** WAVEWATCH-III WARNING IN W3OUNP :'/         &
       '     Snl source terms !/NL5 skipped'/            &
       '     in interaction term.'/)
#endif
  !/
  !/ Internal subroutine W3EXNC ---------------------------------------- /
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Perform actual point output.
  !>
  !> @details Spectra are relative frequency energy spectra.
  !>  Note that arrays CX and CY of the main program now contain
  !>  the absolute sea water speed and direction respectively.
  !>
  !> @param[in] I
  !> @param[in] NCID
  !> @param[in] NREQ
  !> @param[in] INDREQ
  !> @param[in] ORDER
  !>
  !> @author F. Ardhuin
  !> @author M. Accensi
  !> @date 14-Mar-2013
  !>
  SUBROUTINE W3EXNC(I,NCID,NREQ,INDREQ,ORDER)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |            F. Ardhuin             |
    !/                  |            M. Accensi             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         14-Mar-2013 |
    !/                  +-----------------------------------+
    !/
    !/    01-Apr-2011 : Creation                            ( version 3.14 )
    !/    14-Mar-2013 : Optimization and cleanup            ( version 4.10 )
    !/
    !  1. Purpose :
    !
    !     Perform actual point output.
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3SPRn    Subr. W3SRCnMD Mean wave parameters for use in
    !                               source terms.
    !      W3FLXn    Subr. W3FLXnMD Flux/stress computation.
    !      W3SLNn    Subr. W3SLNnMD Linear input.
    !      W3SINn    Subr. W3SRCnMD Input source term.
    !      W3SDSn    Subr. W3SRCnMD Whitecapping source term
    !      W3SNLn    Subr. W3SNLnMD Nonlinear interactions.
    !      W3SBTn    Subr. W3SBTnMD Bottom friction source term.
    !      W3SDBn    Subr. W3SBTnMD Depth induced breaking source term.
    !      W3STRn    Subr. W3STRnMD Triad interaction source term.
    !      W3SBSn    Subr. W3SBSnMD Bottom scattering source term.
    !      W3SXXn    Subr. W3SXXnMD Unclassified source term.
    !      W3PART    Sunr. W3PARTMD Spectral partitioning routine.
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !      STME21    Subr. W3TIMEMD Convert time to string.
    !      PRT1DS    Subr. W3ARRYMD Print plot of 1-D spectrum.
    !      PRT1DM    Subr.   Id.    Print plot of several 1-D spectra.
    !      PRT2DS    Subr.   Id.    Print plot of 2-D spectrum.
    !      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !     Main program in which it is contained,
    !
    !  6. Error messages :
    !
    !     None.
    !
    !  7. Remarks :
    !
    !     - Spectra are relative frequency energy spectra.
    !     - Note that arrays CX and CY of the main program now contain
    !       the absolute sea water speed and direction respectively.
    !
    !  8. Structure :
    !
    !     See source code.
    !
    !  9. Switches :
    !
    !       !/S      Enable subroutine tracing.
    !       !/T      Enable test output.
    !
    !       !/FLXx   Flux/stress computation.
    !       !/LNx    Linear input package
    !       !/STx    Source term package
    !       !/NLx    Nonlinear interaction package
    !       !/BTx    Bottom friction package
    !       !/ICx    S_ice source term package
    !       !/DBx    Depth-induced breaking package
    !       !/TRx    Triad interaction package
    !       !/BSx    Bottom scattering package
    !
    !       !/STAB2  Stability correction for !/ST2
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_FLX1
    USE W3FLX1MD
#endif
#ifdef W3_FLX2
    USE W3FLX2MD
#endif
#ifdef W3_FLX3
    USE W3FLX3MD
#endif
#ifdef W3_FLX4
    USE W3FLX4MD
#endif
#ifdef W3_FLX5
    USE W3FLX5MD
#endif
#ifdef W3_LN1
    USE W3SLN1MD
#endif
#ifdef W3_ST1
    USE W3SRC1MD
#endif
#ifdef W3_ST2
    USE W3SRC2MD
#endif
#ifdef W3_ST3
    USE W3SRC3MD
#endif
#ifdef W3_ST4
    USE W3SRC4MD, ONLY : W3SPR4, W3SIN4, W3SDS4
#endif
#ifdef W3_ST6
    USE W3SRC6MD
    USE W3SWLDMD, ONLY : W3SWL6
    USE W3GDATMD, ONLY : SWL6S6
#endif
#ifdef W3_NL1
    USE W3SNL1MD
    USE W3GDATMD, ONLY: IQTPE
#endif
#ifdef W3_NL2
    USE W3SNL2MD
#endif
#ifdef W3_NL3
    USE W3SNL3MD
#endif
#ifdef W3_NL4
    USE W3SNL4MD
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
#ifdef W3_DB1
    USE W3SDB1MD
#endif
#ifdef W3_BS1
    USE W3SBS1MD
#endif
#ifdef W3_IS2
    USE W3SIS2MD
#endif
    USE W3PARTMD, ONLY: W3PART
    USE W3DISPMD, ONLY: WAVNU1, LIU_FORWARD_DISPERSION
    USE W3GDATMD, ONLY: IICEDISP
    !/
    USE W3ARRYMD, ONLY: PRT1DS, PRT2DS, PRT1DM
    USE W3DISPMD, ONLY: NAR1D, DFAC, N1MAX, ECG1, EWN1, DSIE
    USE NETCDF
#ifdef W3_IG1
    USE W3GIG1MD, ONLY: W3ADDIG
    USE W3CANOMD, ONLY: W3ADD2NDORDER
#endif

    IMPLICIT NONE

    !/
    !/ ------------------------------------------------------------------- /

    INTEGER, INTENT(IN)     :: I, NCID, NREQ, INDREQ(NREQ)
    LOGICAL, INTENT(IN)     :: ORDER



    !/ Local parameters
    !/
    INTEGER                 :: J, J1, I1, I2, ISP, IKM,              &
         ITH, IK, ITT, NPART, IX, IY, ISEA
    INTEGER                 :: CURDATE(8), REFDATE(8)
#ifdef W3_S
    INTEGER, SAVE           :: IENT   = 0
#endif
    !
    REAL                    :: DEPTH, SQRTH, CDIR, SIX, R1, R2,      &
         UDIR, UDIRR, UABS, XL, XH, XL2, XH2,  &
         ET, EWN, ETR, ETX, ETY, EBND, EBX,    &
         EBY, HSIG, WLEN, TMEAN, THMEAN,       &
         THSPRD, EMAX, EL, EH, DENOM, FP, THP, &
         SPP, CD, USTAR, FACTOR, UNORM, ESTAR, &
         FPSTAR, FACF, FACE, FACS, HMAT, WNA,  &
         XYZ, AGE1, AFR, AGE2, FACT, XSTAR,    &
         YSTAR, FHIGH, ZWND, Z0, USTD, EMEAN,  &
         FMEAN, WNMEAN, UDIRCA, CHARN, M2KM,   &
         ICETHICK, ICECON
#ifdef W3_FLX5
    REAL                    :: TAUA, TAUADIR, RHOAIR
#endif
    REAL                    :: WN_R(NK),CG_ICE(NK), ALPHA_LIU(NK),   &
         R(NK), WN(NK), CG(NK), APM(NK),       &
         E3(NTH,NK,NREQ), E(NK,NTH), E1(NK),   &
         THBND(NK), SPBND(NK), A(NTH,NK),      &
         WN2(NTH,NK),                          &
         STT(NK,NTH), SWN(NK,NTH), SNL(NK,NTH),&
         SDS(NK,NTH), SBT(NK,NTH), SIS(NK,NTH),&
         XIN(NTH,NK), XNL(NTH,NK), XTR(NTH,NK),&
         XDS(NTH,NK), XDB(NTH,NK), XBT(NTH,NK),&
         XBS(NTH,NK), XXX(NTH,NK), DIA(NTH,NK),&
         XLN(NTH,NK), XWL(NTH,NK), XIS(NTH,NK),&
         SIN1(NK), SNL1(NK), SDS1(NK),         &
         SBT1(NK), SIS1(NK), STT1(NK),         &
         E1ALL(NK,6), UDIR1(NREQ), CDIR1(NREQ)
#ifdef W3_FLX5
    REAL                    :: TAUDIR1(NREQ)
#endif
    REAL, SAVE              :: HSMIN  = 0.05
#ifdef W3_IS2
    REAL                     :: ICEF, ICEDMAX, DIA2(NTH,NK)
#endif
#ifdef W3_ST1
    REAL                    :: AMAX, FH1, FH2
#endif
#ifdef W3_ST2
    REAL                    :: AMAX, ALPHA(NK), FPI
#endif
#ifdef W3_ST3
    REAL                    :: AMAX, FMEANS, FMEANWS, TAUWX, TAUWY, &
         TAUWNX, TAUWNY
#endif
#ifdef W3_ST4
    REAL                    :: AMAX, FMEANS, FMEANWS, TAUWX, TAUWY, &
         TAUWNX, TAUWNY, FMEAN1, WHITECAP(1:4)
    REAL                    :: LAMBDA(NSPEC), DLWMEAN
#endif
#ifdef W3_ST6
    REAL                    :: AMAX, TAUWX, TAUWY, TAUWNX, TAUWNY
#endif
#ifdef W3_BS1
    REAL                    :: TAUSCX, TAUSCY
#endif
#ifdef W3_BT4
    REAL                    :: D50, PSIC, BEDFORM(3), TAUBBL(2)
#endif
    REAL                    :: ICE
#ifdef W3_STAB2
    REAL                    :: STAB0, STAB,  COR1, COR2, ASFAC,     &
         THARG1, THARG2
#endif
    !
    DOUBLE PRECISION        :: OUTJULDAY
    !
    CHARACTER*4             :: VAR1(6)
    !
    LOGICAL                 :: LASTSTATION=.FALSE.
    LOGICAL                 :: SHORT=.TRUE.
    LOGICAL                   :: LBREAK
#ifdef W3_ST3
    LOGICAL                 :: LLWS(NSPEC)
#endif
#ifdef W3_ST4
    LOGICAL                 :: LLWS(NSPEC)
#endif
    !
    DATA VAR1   / 'Sin ' , 'Snl ', 'Sds ' , 'Sbt ' , 'Sice', 'Stot' /



    !/
    !/ ------------------------------------------------------------------- /
    !/
    ! 1. Initialisations
    !
#ifdef W3_S
    CALL STRACE (IENT, 'W3EXNC')
#endif
    !
    IF ( FLAGLL ) THEN
      M2KM   = 1.
    ELSE
      M2KM   = 1.E-3
    END IF
    !
    XL     = 1./XFR - 1.
    XH     =  XFR - 1.
    XL2    = XL**2
    XH2    = XH**2
    !
    IF ( ITYPE .EQ. 3 ) THEN
      XLN = 0.
      XIN = 0.
      XNL = 0.
      XTR = 0.
      XDS = 0.
      XDB = 0.
      XBT = 0.
      XBS = 0.
      XWL = 0.
      XIS = 0.
      XXX = 0.
    END IF
    !
    CALL U2D('days since 1990-01-01 00:00:00',REFDATE,IERR)
    !
#ifdef W3_T
    WRITE (NDST,9000) (FLREQ(J),J=1,NOPTS)
    WRITE (NDST,9001) ITYPE, OTYPE, NREQ, SCALE1, SCALE2, FLSRCE
#endif

    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    !     Loop over output points.
    !



    !
    ! Selects first station index
    !
    IF (TOGETHER) THEN
      J=1
    ELSE
      J=I
    END IF
    !
    ! Short version of the ww3_ounp code for ITYPE = 1
    !                                    and OTYPE = 3
    !
    IF (SHORT.AND.ITYPE.EQ.1.AND.OTYPE.EQ.3) THEN

      DEPTH  = MAX ( DMIN, DPO(J) )
      SQRTH  = SQRT ( DEPTH )
      DO IK=1, NK
        SIX    = SIG(IK) * SQRTH
        I1     = INT(SIX/DSIE)
        IF (I1.LE.N1MAX) THEN
          I2 = I1 + 1
          R1 = SIX/DSIE - REAL(I1)
          R2 = 1. - R1
          WN(IK) = ( R2*EWN1(I1) + R1*EWN1(I2) ) / DEPTH
          CG(IK) = ( R2*ECG1(I1) + R1*ECG1(I2) ) * SQRTH
        ELSE
          WN(IK) = SIG(IK)*SIG(IK)/GRAV
          CG(IK) = 0.5 * GRAV / SIG(IK)
        END IF
#ifdef W3_T
        WRITE (NDST,9011) IK, TPI/SIG(IK), WN(IK), CG(IK)
#endif
        !
      END DO

      !
      ! Computes 2nd order spectrum
      !
#ifdef W3_IG1
      IF (IGPARS(2).EQ.1) THEN
        IF(IGPARS(1).EQ.1) THEN
          CALL W3ADDIG(SPCO(:,J),DPO(J),WN,CG,0)
        ELSE
          CALL W3ADD2NDORDER(SPCO(:,J),DPO(J),WN,CG,0)
        END IF
      END IF
#endif
      !

      DO J1=1, NREQ
        DO IK=1, NK
          DO ITH=1, NTH
            ISP    = ITH + (IK-1)*NTH
            E3(ITH,IK,J1) = SPCO(ISP,INDREQ(J1))
          END DO
        END DO
      END DO

      CALL T2D(TIME,CURDATE,IERR)
      OUTJULDAY=TSUB(REFDATE,CURDATE)
      IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))
      CALL CHECK_ERR(IRET,4)
      !
      IF (IOUT.EQ.1) THEN
        DO J1=1, NREQ
          IRET=NF90_PUT_VAR(NCID,VARID(27),INDREQ(J1),(/J1/))
          CALL CHECK_ERR(IRET,5)
          IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(INDREQ(J1)),         &
               start=(/1,J1/),count=(/LEN_TRIM(PTNME(INDREQ(J1))) ,1/))
          CALL CHECK_ERR(IRET,6)
        END DO
      END IF
      !
      DO J1=1, NREQ
        IF ((FLWW3.NE.0).AND.(ORDER)) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
        IF ((FLWW3.NE.0).AND.(.NOT.ORDER)) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/IOUT,J1/))
        CALL CHECK_ERR(IRET,7)
      END DO
      !
      IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,INDREQ(1:NREQ)),(/1,IOUT/))
      IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,INDREQ(1:NREQ)),(/IOUT,1/))
      CALL CHECK_ERR(IRET,8)
      IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,INDREQ(1:NREQ)),(/1,IOUT/))
      IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,INDREQ(1:NREQ)),(/IOUT,1/))
      CALL CHECK_ERR(IRET,9)
      !
      DO J1=1,NREQ
        UDIR1(J1)  = MOD ( 270. - WDO(INDREQ(J1))*RADE , 360. )
        CDIR1(J1)   = MOD ( 270. - CDO(INDREQ(J1))*RADE , 360. )
#ifdef W3_FLX5
        TAUDIR1(J1) =  MOD ( 270. - TAUDO(INDREQ(J1))*RADE , 360. )
#endif
      END DO
      !
      IF (NCVARTYPE.LE.3) THEN
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(11),NINT(DPO(INDREQ(1:NREQ))/0.5),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(11),NINT(DPO(INDREQ(1:NREQ))/0.5),(/IOUT,1/))
        CALL CHECK_ERR(IRET,10)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(12),NINT(WAO(INDREQ(1:NREQ))/0.1),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(12),NINT(WAO(INDREQ(1:NREQ))/0.1),(/IOUT,1/))
        CALL CHECK_ERR(IRET,11)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(13),NINT(UDIR1/0.1),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(13),NINT(UDIR1/0.1),(/IOUT,1/))
        CALL CHECK_ERR(IRET,12)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(14),NINT(CAO(INDREQ(1:NREQ))/0.1),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(14),NINT(CAO(INDREQ(1:NREQ))/0.1),(/IOUT,1/))
        CALL CHECK_ERR(IRET,13)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(15),NINT(CDIR1/0.1),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(15),NINT(CDIR1/0.1),(/IOUT,1/))
        CALL CHECK_ERR(IRET,14)
      ELSE
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(11),DPO(INDREQ(1:NREQ)),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(11),DPO(INDREQ(1:NREQ)),(/IOUT,1/))
        CALL CHECK_ERR(IRET,10)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(12),WAO(INDREQ(1:NREQ)),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(12),WAO(INDREQ(1:NREQ)),(/IOUT,1/))
        CALL CHECK_ERR(IRET,11)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(13),UDIR1,(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(13),UDIR1,(/IOUT,1/))
        CALL CHECK_ERR(IRET,12)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(14),CAO(INDREQ(1:NREQ)),(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(14),CAO(INDREQ(1:NREQ)),(/IOUT,1/))
        CALL CHECK_ERR(IRET,13)
        IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(15),CDIR1,(/1,IOUT/))
        IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(15),CDIR1,(/IOUT,1/))
        CALL CHECK_ERR(IRET,14)
      END IF
      !
      IF (NCVARTYPE.LE.3) THEN
        WHERE(E3(:,:,:).GE.0) E3(:,:,:)=NINT(ALOG10(E3(:,:,:)+1E-12)/0.0004)
      END IF
      IF(ORDER) IRET=NF90_PUT_VAR(NCID,VARID(10),E3(1:NTH,1:NK,:), &
           start=(/1,1,1,IOUT/),count=(/NTH,NK,NREQ,1/))
      IF(.NOT.ORDER) IRET=NF90_PUT_VAR(NCID,VARID(10),E3(1:NTH,1:NK,1:NREQ), &
           start=(/1,1,IOUT,1/),count=(/NTH,NK,1,NREQ/))
      CALL CHECK_ERR(IRET,15)
      !
      ! End of short version
      !
    ELSE
      !
      ! And here is the full thing with all options ITYPE and OTYPE ...
      !
      J1=1
      LASTSTATION=.FALSE.
      !
      DO WHILE (.NOT.LASTSTATION)
        !
        IF ( FLREQ(J) ) THEN
          !
          !  Open netCDF file
          !
#ifdef W3_T
          WRITE (NDST,9002) PTNME(J)
#endif
          !
          ! 2. Calculate grid parameters using and inlined version of WAVNU1.
          !
          DEPTH    = MAX ( DMIN, DPO(J) )
          SQRTH    = SQRT ( DEPTH )
          UDIR     = MOD ( 270. - WDO(J)*RADE , 360. )
          UDIRCA   = WDO(J)*RADE
          UDIRR    = WDO(J)
          UABS     = MAX ( 0.001 , WAO(J) )
          CDIR     = MOD ( 270. - CDO(J)*RADE , 360. )
#ifdef W3_FLX5
          TAUA     = MAX ( 0.001 , TAUAO(J))
          TAUADIR  = MOD ( 270. - TAUDO(J)*RADE , 360. )
          RHOAIR   = MAX ( 0. , DAIRO(J))
#endif
#ifdef W3_IS2
          ICEDMAX  = MAX ( 0., ICEFO(J))
          ICEF     = ICEDMAX
#endif
          ICETHICK = MAX (0., ICEHO(J))
          ICECON   = MAX (0., ICEO(J))
          !
#ifdef W3_STAB2
          STAB0  = ZWIND * GRAV / 273.
          STAB   = STAB0 * ASO(J) / MAX(5.,WAO(J))**2
          STAB   = MAX ( -1. , MIN ( 1. , STAB ) )
          THARG1 = MAX ( 0. , FFNG*(STAB-OFSTAB))
          THARG2 = MAX ( 0. , FFPS*(STAB-OFSTAB))
          COR1   = CCNG * TANH(THARG1)
          COR2   = CCPS * TANH(THARG2)
          ASFAC  = SQRT ( (1.+COR1+COR2)/SHSTAB )
#endif
          !
#ifdef W3_T
          WRITE (NDST,9010) DEPTH
#endif
          DO IK=1, NK
            SIX    = SIG(IK) * SQRTH
            I1     = INT(SIX/DSIE)
            IF (I1.LE.N1MAX) THEN
              I2 = I1 + 1
              R1 = SIX/DSIE - REAL(I1)
              R2 = 1. - R1
              WN(IK) = ( R2*EWN1(I1) + R1*EWN1(I2) ) / DEPTH
              CG(IK) = ( R2*ECG1(I1) + R1*ECG1(I2) ) * SQRTH
            ELSE
              WN(IK) = SIG(IK)*SIG(IK)/GRAV
              CG(IK) = 0.5 * GRAV / SIG(IK)
            END IF
#ifdef W3_T
            WRITE (NDST,9011) IK, TPI/SIG(IK), WN(IK), CG(IK)
#endif
            !
          END DO
          !
          ! Computes 2nd order spectrum
          !
#ifdef W3_IG1
          IF (IGPARS(2).EQ.1) THEN
            IF(IGPARS(1).EQ.1) THEN
              CALL W3ADDIG(SPCO(:,J),DPO(J),WN,CG,0)
            ELSE
              CALL W3ADD2NDORDER(SPCO(:,J),DPO(J),WN,CG,0)
            END IF
          END IF
#endif
          !
          !
          ! 3.  Prepare spectra etc.
          ! 3.a Mean wave parameters.
          !
          ET     = 0.
          EWN    = 0.
          ETR    = 0.
          ETX    = 0.
          ETY    = 0.
          DO IK=1, NK
            EBND   = 0.
            EBX    = 0.
            EBY    = 0.
            DO ITH=1, NTH
              ISP    = ITH + (IK-1)*NTH
              E(IK,ITH) = SPCO(ISP,J)
              EBND   = EBND + SPCO(ISP,J)
              EBX    = EBX  + SPCO(ISP,J)*ECOS(ITH)
              EBY    = EBY  + SPCO(ISP,J)*ESIN(ITH)
            END DO
            E1(IK) = EBND * DTH
            APM(IK)= E1(IK) / ( TPI * GRAV**2 / SIG(IK)**5  )
            IF ( E1(IK) .GT. 1.E-5) THEN
              THBND(IK) = MOD(630.- RADE*ATAN2(EBY,EBX),360.)
              SPBND(IK) = RADE * SQRT ( MAX ( 0. , 2.*( 1. -      &
                   SQRT( MAX(0.,(EBX**2+EBY**2)/EBND**2) ) ) ) )
            ELSE
              THBND(IK) = -999.9
              SPBND(IK) = -999.9
            END IF
            EBND   = E1(IK) * DSII(IK) * TPIINV
            ET     = ET  + EBND
            EWN    = EWN + EBND / WN(IK)
            ETR    = ETR + EBND / SIG(IK)
            ETX    = ETX + EBX * DSII(IK)
            ETY    = ETY + EBY * DSII(IK)
          END DO
          !
          ! tail factors for radian action etc ...!
          !
          EBND   = E1(NK) * TPIINV / ( SIG(NK) * DTH )
          ET     = ET  + FTE *EBND
          EWN    = EWN + FTWL*EBND
          ETR    = ETR + FTTR*EBND
          ETX    = DTH*ETX*TPIINV + FTE*EBX*TPIINV/SIG(NK)
          ETY    = DTH*ETY*TPIINV + FTE*EBY*TPIINV/SIG(NK)
          !
          HSIG   = 4. * SQRT ( ET )
          IF ( HSIG .GT. HSMIN ) THEN
            WLEN   = EWN / ET * TPI
            TMEAN  = ETR / ET * TPI
            THMEAN = MOD ( 630. - RADE*ATAN2(ETY,ETX) , 360. )
            THSPRD = RADE * SQRT ( MAX ( 0. , 2.*( 1. - SQRT(     &
                 MAX(0.,(ETX**2+ETY**2)/ET**2) ) ) ) )
            IF ( THSPRD .LT. 0.01*RADE*DTH ) THSPRD = 0.
          ELSE
            WLEN   = 0.
            TMEAN  = 0.
            THMEAN = 0.
            THSPRD = 0.
            E1(1:NK) = 0.
            E(1:NK,1:NTH) = 0.
          END IF
          !
          ! 3.b peak frequency
          !
          EMAX   = E1(NK)
          IKM    = NK
          !
          DO IK=NK-1, 1, -1
            IF ( E1(IK) .GT. EMAX ) THEN
              EMAX   = E1(IK)
              IKM    = IK
            END IF
          END DO
          !
          IF ( HSIG .GE. HSMIN .AND. IKM .NE. NK ) THEN
            IF ( IKM .EQ. 1 ) THEN
              EL = - E1(IKM)
            ELSE
              EL = E1(IKM-1) - E1(IKM)
            END IF

            EH = E1(IKM+1) - E1(IKM)

            DENOM  = XL*EH - XH*EL
            !
            FP     = SIG(IKM) * ( 1. + 0.5 * ( XL2*EH - XH2*EL )  &
                 / SIGN ( MAX(ABS(DENOM),1.E-15) , DENOM ) )
            THP    = THBND(IKM)
            SPP    = SPBND(IKM)
            IF ( SPP .LT. 0.01*RADE*DTH ) SPP = 0.
          ELSE
            FP     = 0.
            THP    = 0.
            SPP    = 0.
          END IF
          !
          ! 3.c spectral partitioning
          !
          IF ( ITYPE.EQ.1 .AND. OTYPE.EQ.4 ) THEN
            CALL W3PART( E, UABS, UDIRCA, DEPTH, WN, NPART, XPART, &
                 DIMXP )
          END IF
          !
          ! 3.d nondimensional parameters
          !
          IF ( ( ITYPE.EQ.2 .AND. (OTYPE.EQ.3.OR.OTYPE.EQ.4) ) .OR. &
               ( ITYPE.EQ.1 .AND. (OTYPE.EQ.2) ) ) THEN
            !
            DO IK=1, NK
              FACTOR = TPIINV * CG(IK) / SIG(IK)
              DO ITH=1, NTH
                ISP    = ITH + (IK-1)*NTH
                A(ITH,IK)   = FACTOR * SPCO(ISP,J)
                WN2(ITH,IK) = WN(IK)
              END DO
            END DO
            !
#ifdef W3_STAB2
            UABS   = UABS / ASFAC
#endif
            !
#ifdef W3_ST0
            ZWND   = 10.
#endif
#ifdef W3_ST1
            ZWND   = 10.
#endif
#ifdef W3_ST2
            ZWND   = ZWIND
#endif
#ifdef W3_ST3
            ZWND   = ZZWND
            TAUWX  = 0.
            TAUWY  = 0.
            LLWS(:)  = .TRUE.
#endif
#ifdef W3_ST4
            LLWS(:)  = .TRUE.
            ZWND   = ZZWND
            TAUWX  = 0.
            TAUWY  = 0.
#endif
#ifdef W3_ST6
            ZWND   = 10.
#endif

            !
#ifdef W3_ST1
            CALL W3SPR1 (A, CG, WN, EMEAN, FMEAN, WNMEAN, AMAX)
            FP     = 0.85 * FMEAN
#endif
#ifdef W3_ST2
            CALL W3SPR2 (A, CG, WN, DEPTH, FP , UABS, USTAR, &
                 EMEAN, FMEAN, WNMEAN, AMAX, ALPHA, FP )
#endif
#ifdef W3_ST3
            CALL W3SPR3 (A, CG, WN, EMEAN, FMEAN, FMEANS,       &
                 WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
                 TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS )
#endif
#ifdef W3_ST4
            CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,       &
                 WNMEAN, AMAX, UABS, UDIRR,             &
#ifdef W3_FLX5
                 TAUA, TAUADIR, RHOAIR,           &
#endif
                 USTAR, USTD, TAUWX, TAUWY, CD, Z0,     &
                 CHARN, LLWS, FMEANWS, DLWMEAN )
#endif
#ifdef W3_ST6
            CALL W3SPR6 (A, CG, WN, EMEAN, FMEAN, WNMEAN, AMAX, FP)
#endif
            !
#ifdef W3_FLX1
            CALL W3FLX1 ( ZWND, UABS, UDIRR,                   &
                 USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX2
            CALL W3FLX2 ( ZWND, DEPTH, FP, UABS, UDIRR,        &
                 USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX3
            CALL W3FLX3 ( ZWND, DEPTH, FP, UABS, UDIRR,        &
                 USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX4
            CALL W3FLX4 ( ZWND, UABS, UDIRR, USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX5
            CALL W3FLX5 ( ZWND, UABS, UDIRR, TAUA, TAUADIR,    &
                 RHOAIR, USTAR, USTD, Z0, CD, CHARN )
#endif
            !
            DO ITT=1, 4
#ifdef W3_ST2
              CALL W3SIN2 (A, CG, WN2, UABS, UDIRR, CD, Z0,    &
                   FPI, XIN, DIA )
              CALL W3SPR2 (A, CG, WN, DEPTH, FPI, UABS, USTAR, &
                   EMEAN, FMEAN, WNMEAN, AMAX, ALPHA, FP )
#endif
#ifdef W3_ST3
              IX=1
              IY=1
              CALL W3SIN3 ( A, CG, WN2, UABS, USTAR, DAIR/DWAT,&
                   ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY,&
                   TAUWNX, TAUWNY, ICE, XIN, DIA, LLWS, IX, IY )
              CALL W3SPR3 (A, CG, WN, EMEAN, FMEAN, FMEANS,       &
                   WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
                   TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS )
#endif
#ifdef W3_ST4
              IX=1
              IY=1
              CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,      &
                   WNMEAN, AMAX, UABS, UDIRR,               &
#ifdef W3_FLX5
                   TAUA, TAUADIR, RHOAIR,             &
#endif
                   USTAR, USTD, TAUWX, TAUWY, CD, Z0,       &
                   CHARN, LLWS, FMEANWS,DLWMEAN )
              CALL W3SDS4 ( A, WN, CG, USTAR,  USTD, DEPTH, DAIR, XDS, &
                   DIA, IX, IY, LAMBDA, WHITECAP, DLWMEAN )
              CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT,     &
                   ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY, TAUWNX, &
                   TAUWNY, XIN, DIA, LLWS, IX, IY, LAMBDA )
#endif
#ifdef W3_FLX2
              CALL W3FLX2 ( ZWND, DEPTH, FP, UABS, UDIRR,      &
                   USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX3
              CALL W3FLX3 ( ZWND, DEPTH, FP, UABS, UDIRR,      &
                   USTAR, USTD, Z0, CD )
#endif
            END DO
            !
            ! Add alternative flux calculations here as part of !/ST2 option ....
            ! Also add before actual source term calculation !!!
            !
#ifdef W3_STAB2
            UABS   = UABS * ASFAC
#endif
            !
            IF ( WAO(J) .LT. 0.01 ) THEN
              UNORM  = 0.
              ESTAR  = 0.
              FPSTAR = 0.
            ELSE
              IF ( OTYPE.EQ.3 ) THEN
                UNORM  = USTAR
              ELSE
                UNORM  = WAO(J)
              END IF
              ESTAR  = ET * GRAV**2 / UNORM**4
              FPSTAR = FP * TPIINV * UNORM / GRAV
              XSTAR  = PTLOC(1,J) * GRAV / UNORM**2
              YSTAR  = PTLOC(2,J) * GRAV / UNORM**2
              IF ( FLAGLL ) THEN
                XSTAR  = XSTAR * DERA * RADIUS &
                     * COS(PTLOC(2,J)*DERA)
                YSTAR  = YSTAR * DERA * RADIUS
              END IF
            END IF
            !
          END IF ! 3.d

          !
          ! 3.e source terms
          !
          IF ( ITYPE.EQ.3 ) THEN
            !
            DO IK=1, NK
              FACTOR = TPIINV * CG(IK) / SIG(IK)
              DO ITH=1, NTH
                A(ITH,IK)   = FACTOR * SPCO(ITH+(IK-1)*NTH,J)
                WN2(ITH,IK) = WN(IK)
              END DO
            END DO
            !
#ifdef W3_STAB2
            UABS   = UABS / ASFAC
#endif
            !
#ifdef W3_ST0
            ZWND   = 10.
#endif
#ifdef W3_ST1
            ZWND   = 10.
#endif
#ifdef W3_ST2
            ZWND   = ZWIND
#endif
#ifdef W3_ST3
            ZWND   = ZZWND
#endif
#ifdef W3_ST0
            USTAR  = 1.
#endif
#ifdef W3_ST1
            USTAR  = 1.
#endif
#ifdef W3_ST2
            USTAR  = 1.
#endif
#ifdef W3_ST3
            USTAR  = 0.
            USTD   = 0.
            TAUWX  = 0.
            TAUWY  = 0.
#endif
#ifdef W3_ST4
            ZWND   = ZZWND
            USTAR  = 0.
            USTD   = 0.
            TAUWX  = 0.
            TAUWY  = 0.
#endif
#ifdef W3_ST6
            ZWND   = 10.
#endif
            !
#ifdef W3_ST0
            FHIGH  = SIG(NK)
#endif
#ifdef W3_ST1
            CALL W3SPR1 (A, CG, WN, EMEAN, FMEAN, WNMEAN, AMAX)
            FP     = 0.85 * FMEAN
            FH1    = FXFM * FMEAN
            FH2    = FXPM / USTAR
            FHIGH  = MAX ( FH1 , FH2 )
#endif
#ifdef W3_ST2
            CALL W3SPR2 (A, CG, WN, DEPTH, FP , UABS, USTAR, &
                 EMEAN, FMEAN, WNMEAN, AMAX, ALPHA, FP )
#endif
#ifdef W3_ST3
            CALL W3SPR3 (A, CG, WN, EMEAN, FMEAN, FMEANS,       &
                 WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
                 TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS )
#endif
#ifdef W3_ST4
            CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN,  FMEAN1,        &
                 WNMEAN, AMAX, UABS, UDIRR,               &
#ifdef W3_FLX5
                 TAUA, TAUADIR, RHOAIR,             &
#endif
                 USTAR, USTD, TAUWX, TAUWY, CD, Z0,       &
                 CHARN, LLWS, FMEANWS, DLWMEAN )
            CALL W3SDS4 ( A, WN, CG, USTAR,  USTD, DEPTH, DAIR, XDS, &
                 DIA, IX, IY, LAMBDA, WHITECAP, DLWMEAN )
#endif
#ifdef W3_ST6
            CALL W3SPR6 (A, CG, WN, EMEAN, FMEAN, WNMEAN, AMAX, FP)
            FHIGH  = SIG(NK)
#endif
            !
#ifdef W3_FLX1
            CALL W3FLX1 ( ZWND, UABS, UDIRR,                   &
                 USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX2
            CALL W3FLX2 ( ZWND, DEPTH, FP, UABS, UDIRR,        &
                 USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX3
            CALL W3FLX3 ( ZWND, DEPTH, FP, UABS, UDIRR,        &
                 USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX4
            CALL W3FLX4 ( ZWND, UABS, UDIRR, USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX5
            CALL W3FLX5 ( ZWND, UABS, UDIRR, TAUA, TAUADIR,    &
                 RHOAIR, USTAR, USTD, Z0, CD, CHARN )
#endif
            !
            DO ITT=1, 3
#ifdef W3_ST2
              CALL W3SIN2 (A, CG, WN2, UABS, UDIRR, CD, Z0,    &
                   FPI, XIN, DIA )
              CALL W3SPR2 (A, CG, WN, DEPTH, FPI, UABS, USTAR, &
                   EMEAN, FMEAN, WNMEAN, AMAX, ALPHA, FP )
#endif
#ifdef W3_ST3
              CALL W3SPR3 (A, CG, WN, EMEAN, FMEAN, FMEANS,       &
                   WNMEAN, AMAX, UABS, UDIRR, USTAR, USTD,&
                   TAUWX, TAUWY, CD, Z0, CHARN, LLWS, FMEANWS )
              CALL W3SIN3 ( A, CG, WN2, UABS, USTAR, DAIR/DWAT,&
                   ASO(J), UDIRR, Z0, CD,TAUWX, TAUWY, &
                   TAUWNX, TAUWNY, ICE, XIN, DIA, LLWS, IX, IY )
#endif
#ifdef W3_ST4
              CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,       &
                   WNMEAN, AMAX, UABS, UDIRR,               &
#ifdef W3_FLX5
                   TAUA, TAUADIR, RHOAIR,              &
#endif
                   USTAR, USTD,  TAUWX, TAUWY, CD, Z0,      &
                   CHARN, LLWS, FMEANWS, DLWMEAN )
              CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT,    &
                   ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY,TAUWNX,&
                   TAUWNY, XIN, DIA, LLWS, IX, IY, LAMBDA )
#endif
#ifdef W3_FLX2
              CALL W3FLX2 ( ZWND, DEPTH, FP, UABS, UDIRR,      &
                   USTAR, USTD, Z0, CD )
#endif
#ifdef W3_FLX3
              CALL W3FLX3 ( ZWND, DEPTH, FP, UABS, UDIRR,      &
                   USTAR, USTD, Z0, CD )
#endif
            END DO
            !
#ifdef W3_ST2
            FHIGH  = XFC * FPI
#endif
            !
            IF ( FLSRCE(2) ) THEN
#ifdef W3_LN1
              CALL W3SLN1 (WN, FHIGH, USTAR, UDIRR, XLN )
#endif
              !
#ifdef W3_ST1
              CALL W3SIN1 (A, WN2, USTAR, UDIRR,  XIN, DIA )
#endif
#ifdef W3_ST2
              CALL W3SIN2 (A, CG, WN2, UABS, UDIRR, CD, Z0,&
                   FPI, XIN, DIA )
#endif
#ifdef W3_ST3
              CALL W3SIN3 ( A, CG, WN2, UABS, USTAR,       &
                   DAIR/DWAT, ASO(J), UDIRR,      &
                   Z0, CD, TAUWX, TAUWY,TAUWNX, TAUWNY, &
                   ICE, XIN, DIA, LLWS, IX, IY )
#endif
              !
#ifdef W3_ST4
              CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,     &
                   WNMEAN, AMAX, UABS, UDIRR,               &
#ifdef W3_FLX5
                   TAUA, TAUADIR, RHOAIR,              &
#endif
                   USTAR, USTD, TAUWX, TAUWY, CD, Z0,       &
                   CHARN, LLWS, FMEANWS, DLWMEAN )
              CALL W3SDS4 ( A, WN, CG, USTAR,  USTD, DEPTH, DAIR, XDS, &
                   DIA, IX, IY, LAMBDA, WHITECAP, DLWMEAN )
              CALL W3SIN4 (A, CG, WN2, UABS, USTAR, DAIR/DWAT,    &
                   ASO(J), UDIRR, Z0, CD, TAUWX, TAUWY, TAUWNX, &
                   TAUWNY, XIN, DIA, LLWS, IX, IY, LAMBDA )
#endif
#ifdef W3_ST6
              CALL W3SIN6 (A, CG, WN2, UABS, USTAR, UDIRR, CD, DAIR, &
                   TAUWX, TAUWY, TAUWNX, TAUWNY, XIN, DIA )
#endif
            END IF
            IF ( FLSRCE(3) ) THEN
#ifdef W3_NL1
              IF (IQTPE.GT.0) THEN
                CALL W3SNL1 ( A, CG, WNMEAN*DEPTH,  XNL, DIA )
              ELSE
                CALL W3SNLGQM ( A, CG, WN, DEPTH,  XNL, DIA )
              END IF
#endif
#ifdef W3_NL2
              CALL W3SNL2 ( A, CG, DEPTH,         XNL, DIA )
#endif
#ifdef W3_NL3
              CALL W3SNL3 ( A, CG, WN, DEPTH,     XNL, DIA )
#endif
#ifdef W3_NL4
              CALL W3SNL4 ( A, CG, WN, DEPTH,     XNL, DIA )
#endif
            END IF
            IF ( FLSRCE(4) ) THEN
#ifdef W3_ST1
              CALL W3SDS1 ( A, WN2, EMEAN, FMEAN, WNMEAN,  &
                   XDS, DIA )
#endif
#ifdef W3_ST2
              CALL W3SDS2 ( A, CG, WN, FPI, USTAR,         &
                   ALPHA,                XDS, DIA )
#endif
#ifdef W3_ST3
              CALL W3SDS3 ( A, WN, CG, EMEAN, FMEANS, WNMEAN,  &
                   USTAR, USTD, DEPTH, XDS, DIA, IX, IY )
#endif
#ifdef W3_ST4
              CALL W3SPR4 (A, CG, WN, EMEAN, FMEAN, FMEAN1,       &
                   WNMEAN, AMAX, UABS, UDIRR,                 &
#ifdef W3_FLX5
                   TAUA, TAUADIR, RHOAIR,               &
#endif
                   USTAR, USTD, TAUWX, TAUWY, CD, Z0,         &
                   CHARN, LLWS, FMEANWS, DLWMEAN )
              CALL W3SDS4 ( A, WN, CG,  USTAR, USTD, DEPTH, DAIR, XDS, &
                   DIA, IX, IY, LAMBDA, WHITECAP , DLWMEAN)
#endif
#ifdef W3_ST6
              CALL W3SDS6 ( A, CG, WN, XDS, DIA )
              IF (SWL6S6) CALL W3SWL6 ( A, CG, WN, XWL, DIA )
#endif
              !
#ifdef W3_DB1
              CALL W3SDB1 ( I, A, DEPTH, EMEAN, FMEAN, &
                   WNMEAN, CG, LBREAK, XDB, DIA )
#endif
            END IF
            IF ( FLSRCE(5) ) THEN
#ifdef W3_BT1
              CALL W3SBT1 ( A, CG, WN, DEPTH,     XBT, DIA )
#endif
#ifdef W3_BT4
              IX=1    ! to be fixed later
              IY=1    ! to be fixed later
              ISEA=1  ! to be fixed later
              D50 = SED_D50(ISEA)
              PSIC= SED_PSIC(ISEA)
              CALL W3SBT4 ( A, CG, WN, DEPTH, D50, PSIC, TAUBBL,   &
                   BEDFORM, XBT, DIA, IX, IY )
#endif

              ! see remarks about BT8 and BT9 in ww3_outp.ftn
              !....broken....!/BT8        CALL W3SBT8 ( SPEC, DEPTH, VSBT, VDBT, IX, IY )
              !....broken....!/BT9        CALL W3SBT9 ( SPEC, DEPTH, VSBT, VDBT, IX, IY )

              !
#ifdef W3_BS1
              CALL W3SBS1 ( A, CG, WN, DEPTH,              &
                   CAO(J)*COS(CDO(J)), CAO(J)*SIN(CDO(J)), &
                   TAUSCX, TAUSCY,   XBS, DIA )
#endif
              !
            END IF
            IF ( FLSRCE(6) ) THEN
              IF (IICEDISP) THEN
                CALL LIU_FORWARD_DISPERSION (ICETHICK,0.,DEPTH, &
                     SIG,WN_R,CG_ICE,ALPHA_LIU)
              ELSE
                WN_R=WN
                CG_ICE=CG
              END IF
              !
#ifdef W3_IS2
              CALL W3SIS2(A, DEPTH, ICECON, ICETHICK, ICEF, ICEDMAX,  &
                   IX, IY, XIS, DIA, DIA2, WN, CG, WN_R, CG_ICE, R)
#endif
            END IF
            !
#ifdef W3_STAB2
            UABS   = UABS * ASFAC
#endif
            !
            IF ( ISCALE.EQ.0 .OR. ISCALE.EQ.3 ) THEN
              FACF   = TPIINV
              FACE   = 1.
              FACS   = 1.
            ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.4 ) THEN
              FACF   = TPIINV * UABS / GRAV
              FACE   = GRAV**3 / UABS**5
              FACS   = GRAV**2 / UABS**4
            ELSE IF ( ISCALE.EQ.2 .OR. ISCALE.EQ.5 ) THEN
              FACF   = TPIINV * USTAR / GRAV
              FACE   = GRAV**3 / USTAR**5
              FACS   = GRAV**2 / USTAR**4
            END IF
            !
            DO IK=1, NK
              FACTOR = TPI / CG(IK) * SIG(IK)
              E1  (IK) = 0.
              SIN1(IK) = 0.
              SNL1(IK) = 0.
              SDS1(IK) = 0.
              SBT1(IK) = 0.
              SIS1(IK) = 0.
              STT1(IK) = 0.
              DO ITH=1, NTH
                ISP         = ITH + (IK-1)*NTH
                E  (IK,ITH) = SPCO(ISP,J)
                SWN(IK,ITH) = ( XLN(ITH,IK) + XIN(ITH,IK) ) * FACTOR
                SNL(IK,ITH) = ( XNL(ITH,IK) + XTR(ITH,IK) ) * FACTOR
                SDS(IK,ITH) = ( XDS(ITH,IK) + XDB(ITH,IK) ) * FACTOR
#ifdef W3_ST6
                SDS(IK,ITH) =   SDS(IK,ITH) +(XWL(ITH,IK)   * FACTOR)
#endif
                SBT(IK,ITH) = ( XBT(ITH,IK) + XBS(ITH,IK) ) * FACTOR
                SIS(IK,ITH) = XIS(ITH,IK) * FACTOR
                STT(IK,ITH) = SWN(IK,ITH) + SNL(IK,ITH) + SDS(IK,ITH) + &
                     SBT(IK,ITH) + SIS(IK,ITH) + XXX(ITH,IK) * FACTOR
                E1  (IK) = E1  (IK) + E(IK,ITH)
                SIN1(IK) = SIN1(IK) + SWN(IK,ITH)
                SNL1(IK) = SNL1(IK) + SNL(IK,ITH)
                SDS1(IK) = SDS1(IK) + SDS(IK,ITH)
                SBT1(IK) = SBT1(IK) + SBT(IK,ITH)
                SIS1(IK) = SIS1(IK) + SIS(IK,ITH)
              END DO
              E1  (IK) = E1(IK)   * DTH * FACE
              SIN1(IK) = SIN1(IK) * DTH * FACS
              SNL1(IK) = SNL1(IK) * DTH * FACS
              SDS1(IK) = SDS1(IK) * DTH * FACS
              SBT1(IK) = SBT1(IK) * DTH * FACS
              SIS1(IK) = SIS1(IK) * DTH * FACS
            END DO
            !
            STT1       = SIN1 + SNL1 + SDS1 + SBT1 + SIS1
            E1ALL(:,1) = SIN1
            E1ALL(:,2) = SNL1
            E1ALL(:,3) = SDS1
            E1ALL(:,4) = SBT1
            E1ALL(:,5) = SIS1
            E1ALL(:,6) = STT1
            !
          END IF ! 3.e

          !
          ! 4.a Perform output type 1 ( print plots / tables / file )
          !
          IF ( ITYPE .EQ. 1 ) THEN
            !
            !  Format Time
            !
            IF ( OTYPE .NE. 1 ) THEN

              CALL T2D(TIME,CURDATE,IERR)
              OUTJULDAY=TSUB(REFDATE,CURDATE)
              IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))
              CALL CHECK_ERR(IRET,16)
            END IF


            !
            !  Performs subtype 1
            !
            IF ( OTYPE .EQ. 1 ) THEN
              !
              IF ( SCALE1 .GE. 0. )                             &
                   CALL PRT1DS (NDSO, NK, E1, SIG(1:NK), 'RAD/S',&
                   17, SCALE1, 'E(f)', 'm^2s', PTNME(J) )
              IF ( SCALE2 .GE. 0. )                             &
                   CALL PRT2DS (NDSO, NK, NK, NTH, E, SIG(1:NK), &
                   'RAD/S', 1., SCALE2, 0.0001, 'E(f,th)',  &
                   'm^2s', PTNME(J) )
              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,910) DPO(J), UABS
              IF ( (WAO(J) .GT. 0.) .AND. (IAPROC .EQ. NAPOUT) ) WRITE (NDSO,911) UDIR
              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,912) ASO(J), CAO(J)
              IF ( (CAO(J) .GT. 0.) .AND. (IAPROC .EQ. NAPOUT) ) WRITE (NDSO,913) CDIR
              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,914) HSIG, WLEN, TMEAN, THMEAN, THSPRD


              !
              !  Performs subtype 2
              !
            ELSE IF ( OTYPE .EQ. 2 ) THEN

              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              CALL CHECK_ERR(IRET,17)
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(7),DPO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(8),USTAR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(10),UDIR,(/J1,IOUT/))

              IF ( FP .EQ. 0. ) FP = SIG(NK)
              IRET=NF90_PUT_VAR(NCID,VARID(11),SIG(1:NK)/FP,start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(12),E1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(13),THBND(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(14),SPBND(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(15),APM(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))


              !
              !  Performs subtype 3
              !
            ELSE IF ( OTYPE .EQ. 3 ) THEN
              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              CALL CHECK_ERR(IRET,18)
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IF (NCVARTYPE.LE.3) THEN
                IRET=NF90_PUT_VAR(NCID,VARID(11),NINT(DPO(J)/0.5),(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(12),NINT(WAO(J)/0.1),(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(13),NINT(UDIR/0.1),(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(14),NINT(CAO(J)/0.1),(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(15),NINT(CDIR/0.1),(/J1,IOUT/))
              ELSE
                IRET=NF90_PUT_VAR(NCID,VARID(11),DPO(J),(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(12),WAO(J),(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(13),UDIR,(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(14),CAO(J),(/J1,IOUT/))
                IRET=NF90_PUT_VAR(NCID,VARID(15),CDIR,(/J1,IOUT/))
              END IF
              IF (NCVARTYPE.LE.3) THEN
                WHERE(E.GE.0) E=NINT(ALOG10(E+1E-12)/0.0004)
              END IF
              IRET=NF90_PUT_VAR(NCID,VARID(10),TRANSPOSE(E(1:NK,1:NTH)), &
                   start=(/1,1,J1,IOUT/),count=(/NTH,NK,1,1/))
              !
              !  Performs subtype 4
              !
            ELSE IF ( OTYPE .EQ. 4 ) THEN
              !
              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              CALL CHECK_ERR(IRET,19)
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(6),NPART,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(7),DEPTH,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(8),WAO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(9),UDIR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(10),CAO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(11),CDIR,(/J1,IOUT/))
              ! XPART infos - see w3partmd.ftn - SUBROUTINE PTMEAN
              IRET=NF90_PUT_VAR(NCID,VARID(12),XPART(1,0:NPART),  &
                   start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(13),XPART(2,0:NPART),  &
                   start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(14),XPART(3,0:NPART),start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(15),XPART(4,0:NPART),start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(16),XPART(5,0:NPART),start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(17),XPART(6,0:NPART),start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(18),XPART(12,0:NPART),start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(19),XPART(13,0:NPART),start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(20),XPART(14,0:NPART),start=(/1,J1,IOUT/),count=(/NPART,1,1/))
              !
            END IF

            !
            ! 4.b Perform output type 2 ( tables )
            !
          ELSE IF ( ITYPE .EQ. 2 ) THEN

            !  Format Time
            CALL T2D(TIME,CURDATE,IERR)
            OUTJULDAY=TSUB(REFDATE,CURDATE)
            IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))

            !
            !  Performs subtype 1
            !
            IF ( OTYPE .EQ. 1 ) THEN

              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(6),DPO(J),(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(7),CAO(J),(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(8),CDIR,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(10),UDIR,(/ J1,IOUT /))
#ifdef W3_SETUP
              IRET=NF90_PUT_VAR(NCID,VARID(11),ZET_SETO,(/ J1,IOUT /))
#endif

              !
              !  Performs subtype 2
              !
            ELSE IF ( OTYPE .EQ. 2 ) THEN

              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(6),HSIG,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(7),WLEN,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(8),TMEAN,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(9),THP,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(10),SPP,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(11),FP*TPIINV,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(12),THMEAN,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(13),THSPRD,(/ J1,IOUT /))

              !
              !  Performs subtype 3
              !
            ELSE IF ( OTYPE .EQ. 3 ) THEN

              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),1.E-4*XSTAR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),1.E-4*YSTAR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(6),UNORM,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(7),ESTAR,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(8),FPSTAR,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(9),CD*1000.,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(10),APM(NK)*100.,(/ J1,IOUT /))

              !
              !  Performs subtype 4
              !
            ELSE IF ( OTYPE .EQ. 4 ) THEN

              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),XSTAR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),YSTAR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(6),UNORM,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(7),ESTAR,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(8),FPSTAR,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(9),CD*1000.,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(10),APM(NK)*100.,(/ J1,IOUT /))

              !
              !  Performs subtype 5
              !
            ELSE IF ( OTYPE .EQ. 5 ) THEN
              HMAT   = MIN ( 100. , 3.33*GRAV*HSIG/UABS**2 )
              IF ( HSIG .GE. HSMIN ) THEN
                CALL WAVNU1 ( FP, DPO(J), WNA, XYZ )
                AGE1   = MIN ( 100. , FP / WNA / UABS )
                AFR    = TPI / TMEAN
                CALL WAVNU1 ( AFR, DPO(J), WNA, XYZ )
                AGE2   = MIN ( 100. , AFR / WNA / UABS )
              ELSE
                AGE1   = NF90_FILL_FLOAT
                AGE2   = NF90_FILL_FLOAT
              END IF

              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(6),WAO(J),(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(7),UDIR,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(8),HSIG,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(9),HMAT,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(10),AGE1,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(11),AGE2,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(12),ASO(J),(/ J1,IOUT /))

              !
              !  Performs subtype 6
              !
            ELSE IF ( OTYPE .EQ. 6 ) THEN

              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(6),WAO(J),(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(7),UDIR,(/ J1,IOUT /))
              IRET=NF90_PUT_VAR(NCID,VARID(8),HSIG,(/ J1,IOUT /))

              IF ( HSIG .GE. HSMIN ) THEN
                IRET=NF90_PUT_VAR(NCID,VARID(9),TPI/FP,(/ J1,IOUT /))
              ELSE
                IRET=NF90_PUT_VAR(NCID,VARID(9),0.0,(/ J1,IOUT /))
              END IF
            END IF ! OTYPE

            !
            ! 4.c Perform output type 3 ( source terms )
            !
          ELSE IF ( ITYPE .EQ. 3 ) THEN
            !
            !  Format Time
            !
            IF ( OTYPE .NE. 1 ) THEN

              CALL T2D(TIME,CURDATE,IERR)
              OUTJULDAY=TSUB(REFDATE,CURDATE)
              IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,(/IOUT/))
            END IF
            !
            !  Performs subtype 1
            !
            IF ( OTYPE .EQ. 1 ) THEN
              !
              IF ( SCALE1 .GE. 0. ) THEN
                IF ( FLSRCE(1) )                              &
                     CALL PRT1DS (NDSO, NK, E1, SIG(1:NK),     &
                     'RAD/S', 17,  0., 'E(f)', 'm^2s',    &
                     PTNME(J) )
                IF (FLSRCE(2) .OR. FLSRCE(3) .OR.             &
                     FLSRCE(4) .OR. FLSRCE(5) .OR.             &
                     FLSRCE(6) .OR. FLSRCE(7) )                &
                     CALL PRT1DM (NDSO, NK, 6, E1ALL, SIG(1:NK),&
                     'RAD/S', 17, SCALE1, VAR1, 'M2',     &
                     PTNME(J) )
              END IF
              IF ( SCALE2 .GE. 0. ) THEN
                IF ( FLSRCE(1) )                              &
                     CALL PRT2DS (NDSO, NK, NK, NTH, E,        &
                     SIG(1:NK), 'RAD/S', 1., 0., 0.0001,  &
                     'E(f,th)', 'm^2s', PTNME(J) )
                IF ( FLSRCE(2) )                              &
                     CALL PRT2DS (NDSO, NK, NK, NTH, SWN,      &
                     SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
                     'Sin(f,th)', 'm^2', PTNME(J) )
                IF ( FLSRCE(3) )                              &
                     CALL PRT2DS (NDSO, NK, NK, NTH, SNL,      &
                     SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
                     'Snl(f,th)', 'm^2', PTNME(J) )
                IF ( FLSRCE(4) )                              &
                     CALL PRT2DS (NDSO, NK, NK, NTH, SDS,      &
                     SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
                     'Sds(f,th)', 'm^2', PTNME(J) )
                IF ( FLSRCE(5) )                              &
                     CALL PRT2DS (NDSO, NK, NK, NTH, SBT,      &
                     SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
                     'Sbt(f,th)', 'm^2', PTNME(J) )
                IF ( FLSRCE(6) )                              &
                     CALL PRT2DS (NDSO, NK, NK, NTH, SIS,      &
                     SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
                     'Sice(f,th)', 'm^2', PTNME(J) )
                IF ( FLSRCE(7) )                              &
                     CALL PRT2DS (NDSO, NK, NK, NTH, STT,      &
                     SIG(1:NK), 'RAD/S', 1., SCALE2, 0.0001,&
                     'Stot(f,th)', 'm^2', PTNME(J) )
              END IF
              !
              !  Performs subtype 2
              !
            ELSE IF ( OTYPE .EQ. 2 ) THEN
              !
              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(7),DPO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(8),USTAR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/J1,IOUT/))
              IF ( ISCALE.GE.3 ) FACF = 1. / FP
              IRET=NF90_PUT_VAR(NCID,VARID(6),FACF*SIG(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(10),E1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(11),SIN1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(12),SNL1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(13),SDS1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(14),SBT1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(15),SIS1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))
              IRET=NF90_PUT_VAR(NCID,VARID(16),STT1(1:NK),start=(/1,J1,IOUT /),count=(/NK,1,1/))

              !
              !  Performs subtype 3
              !
            ELSE IF ( OTYPE .EQ. 3 ) THEN
              !
              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(7),DPO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(8),USTAR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(9),WAO(J),(/J1,IOUT/))
              IF ( ISCALE.GE.3 ) FACF = 1. / FP
              DO IK=1, NK
                FACT   = 1. / MAX ( 1.E-10 , E1(IK) )
                IRET=NF90_PUT_VAR(NCID,VARID(6),FACF*SIG(IK),(/ IK,J1,IOUT /))
                IRET=NF90_PUT_VAR(NCID,VARID(10),E1(IK),(/ IK,J1,IOUT /))
                IF ( E1(IK) .GT. 1.E-10 ) THEN
                  IRET=NF90_PUT_VAR(NCID,VARID(11),FACT*SIN1(IK),(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(12),FACT*SNL1(IK),(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(13),FACT*SDS1(IK),(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(14),FACT*SBT1(IK),(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(15),FACT*SIS1(IK),(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(16),FACT*STT1(IK),(/ IK,J1,IOUT /))
                ELSE
                  IRET=NF90_PUT_VAR(NCID,VARID(11),NF90_FILL_FLOAT,(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(12),NF90_FILL_FLOAT,(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(13),NF90_FILL_FLOAT,(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(14),NF90_FILL_FLOAT,(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(15),NF90_FILL_FLOAT,(/ IK,J1,IOUT /))
                  IRET=NF90_PUT_VAR(NCID,VARID(16),NF90_FILL_FLOAT,(/ IK,J1,IOUT /))
                END IF
              END DO

              !
              !  Performs subtype 4
              !
            ELSE IF ( OTYPE .EQ. 4 ) THEN
              !
              IRET=NF90_PUT_VAR(NCID,VARID(27),J,(/J1/))
              IRET=NF90_PUT_VAR(NCID,VARID(2),PTNME(J),start=(/1,J1/),count=(/LEN_TRIM(PTNME(J)) ,1/))
              IF (FLWW3.NE.0) IRET=NF90_PUT_VAR(NCID,VARID(3),FLWW3,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(4),M2KM*PTLOC(1,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(5),M2KM*PTLOC(2,J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(10),DPO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(11),WAO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(12),UDIR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(13),CAO(J),(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(14),CDIR,(/J1,IOUT/))
              IRET=NF90_PUT_VAR(NCID,VARID(15),USTAR,(/J1,IOUT/))

              IF ( FLSRCE(1) )  IRET=NF90_PUT_VAR(NCID,VARID(16),   &
                   TRANSPOSE(E(1:NK,1:NTH)), start=(/1,1,J1,IOUT/),   &
                   count=(/NTH,NK,1,1/)                               )
              IF ( FLSRCE(2) )  IRET=NF90_PUT_VAR(NCID,VARID(17),   &
                   TRANSPOSE(SWN(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
                   count=(/NTH,NK,1,1/)                               )
              IF ( FLSRCE(3) )  IRET=NF90_PUT_VAR(NCID,VARID(18),   &
                   TRANSPOSE(SNL(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
                   count=(/NTH,NK,1,1/)                               )
              IF ( FLSRCE(4) )  IRET=NF90_PUT_VAR(NCID,VARID(19),   &
                   TRANSPOSE(SDS(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
                   count=(/NTH,NK,1,1/)                               )
              IF ( FLSRCE(5) )  IRET=NF90_PUT_VAR(NCID,VARID(20),   &
                   TRANSPOSE(SBT(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
                   count=(/NTH,NK,1,1/)                               )
              IF ( FLSRCE(6) )  IRET=NF90_PUT_VAR(NCID,VARID(21),   &
                   TRANSPOSE(SIS(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
                   count=(/NTH,NK,1,1/)                               )
              IF ( FLSRCE(7) )  IRET=NF90_PUT_VAR(NCID,VARID(22),   &
                   TRANSPOSE(STT(1:NK,1:NTH)), start=(/1,1,J1,IOUT/), &
                   count=(/NTH,NK,1,1/)                               )
              !
            END IF
            !
          END IF ! ITYPE

          !
          ! ... End of fields loop
          !
          IF (TOGETHER) J1=J1+1

        END IF ! FLREQ(J)
        !
        ! Selects next station index or end up if not together
        !
        IF (TOGETHER) THEN
          J=J+1
          IF (J.GT.NOPTS) LASTSTATION=.TRUE.
        ELSE
          LASTSTATION=.TRUE.
        END IF

      END DO ! DO WHILE (.NOT. LASTSTATION)

    END IF  ! NOT SHORT ...
    !
    RETURN
    !
    ! Formats
    !
910 FORMAT (/15X,' Water depth       :',F7.1,'  (m)'/               &
         15X,' Wind speed        :',F8.2,' (m/s)')
911 FORMAT ( 15X,' Wind direction    :',F7.1,'  (degr)')
912 FORMAT ( 15X,' Air-sea temp. dif.:',F7.1,'  (degr)'/            &
         15X,' sea water speed     :',F8.2,' (m/s)')
913 FORMAT ( 15X,' direction of current (from) :',F7.1,'  (degr)')
914 FORMAT ( 15X,' Wave height       :',F8.2,' (m)'/                &
         15X,' Mean wave length  :',F6.0,'   (m)'/              &
         15X,' Mean wave period  :',F7.1,'  (s)'/               &
         15X,' Mean wave direct. :',F7.1,'  (degr)'/            &
         15X,' Direct. spread    :',F7.1,'  (degr)'/)
    !
#ifdef W3_T
9000 FORMAT (' TEST W3EXNC : FLAGS :',40L2)
9001 FORMAT (' TEST W3EXNC : ITPYE  :',I4/                        &
         '               OTPYE  :',I4/                        &
         '               NREQ   :',I4/                        &
         '               SCALE1 :',E10.3/                     &
         '               SCALE2 :',E10.3/                     &
         '               FLSRCE :',6L2)
9002 FORMAT (' TEST W3EXNC : OUTPUT POINT : ',A)
9010 FORMAT (' TEST W3EXNC : DEPTH =',F7.1,'  IK, T, K, CG :')
9011 FORMAT ('               ',I3,F8.2,F8.4,F8.2)
#endif
    !/
    !/ End of W3EXNC ----------------------------------------------------- /
    !/
  END SUBROUTINE W3EXNC





  !--------------------------------------------------------------------------
  !> @brief Desc not available.
  !>
  !> @param[in] ITYPE
  !> @param[in] OTYPE
  !> @param[in] NCTYPE
  !> @param[in] NCFILE
  !> @param[out] NCID
  !> @param[out] DIMID
  !> @param[in] DIMLN
  !> @param[out] VARID
  !> @param[in] ONE
  !> @param[in] TWO
  !> @param[in] FLSRCE
  !> @param[in] NCVARTYPE
  !>
  !> @author NA  @date NA
  SUBROUTINE W3CRNC (ITYPE, OTYPE, NCTYPE, NCFILE, NCID, DIMID, DIMLN, VARID, ONE, TWO, FLSRCE, NCVARTYPE)


    USE W3GDATMD
    USE NETCDF

    implicit none


    INTEGER, INTENT(IN)               :: ITYPE,OTYPE,NCTYPE, ONE, TWO
    CHARACTER*(128), INTENT(IN)       :: NCFILE
    INTEGER, INTENT(IN)               :: DIMLN(5)
    INTEGER, INTENT(OUT)              :: DIMID(7), VARID(28),NCID
    LOGICAL, INTENT(IN), OPTIONAL     :: FLSRCE(7)
    INTEGER, INTENT(IN), OPTIONAL     :: NCVARTYPE

    ! local parameters
    INTEGER                           :: IRET
    INTEGER                             :: DEFLATE=1
    !
    REAL(kind=4)                      :: FREQ(NK), FREQ1(NK),FREQ2(NK), DIR(NTH)


    !
    ! Creation in netCDF3 or netCDF4
    !
    IF(NCTYPE.EQ.3) IRET = NF90_CREATE(TRIM(NCFILE), NF90_CLOBBER, NCID)
    IF(NCTYPE.EQ.4) IRET = NF90_CREATE(TRIM(NCFILE), NF90_NETCDF4, NCID)
    CALL CHECK_ERR(IRET,20)

    !
    !     Define generals dimensions
    !
    IRET = NF90_DEF_DIM(NCID, 'time', DIMLN(1), DIMID(1))
    CALL CHECK_ERR(IRET,21)
    IRET = NF90_DEF_DIM(NCID, 'station', DIMLN(2), DIMID(2))
    CALL CHECK_ERR(IRET,22)
    IRET = NF90_DEF_DIM(NCID, 'string40', DIMLN(3), DIMID(3))
    CALL CHECK_ERR(IRET,23)

    !
    !     define generals variables
    !

    !  time
    IRET=NF90_DEF_VAR(NCID, 'time', NF90_DOUBLE, DIMID(1), VARID(1))
    CALL CHECK_ERR(IRET,24)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(1), 1, 1, DEFLATE)
    SELECT CASE (TRIM(CALTYPE))
    CASE ('360_day')
      IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','time in 360 day calendar')
    CASE ('365_day')
      IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','time in 365 day calendar')
    CASE ('standard')
      IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','julian day (UT)')
    END SELECT
    IRET=NF90_PUT_ATT(NCID,VARID(1),'standard_name','time')
    IRET=NF90_PUT_ATT(NCID,VARID(1),'units','days since 1990-01-01 00:00:00')
    IRET=NF90_PUT_ATT(NCID,VARID(1),'conventions','Relative julian days with decimal part (as parts of the day)')
    IRET=NF90_PUT_ATT(NCID,VARID(1),'axis','T')
    IRET=NF90_PUT_ATT(NCID,VARID(1),'calendar',TRIM(CALTYPE))

    !  station
    IRET=NF90_DEF_VAR(NCID, 'station', NF90_INT, (/DIMID(2)/), VARID(27))
    CALL CHECK_ERR(IRET,25)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(27), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(27),'long_name','station id')
    IRET=NF90_PUT_ATT(NCID,VARID(27),'_FillValue',NF90_FILL_INT)
    IRET=NF90_PUT_ATT(NCID,VARID(27),'axis','X')

    !  string40
    IRET=NF90_DEF_VAR(NCID, 'string40', NF90_INT, (/DIMID(3)/), VARID(28))
    CALL CHECK_ERR(IRET,26)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(28), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(28),'long_name','station_name number of characters')
    IRET=NF90_PUT_ATT(NCID,VARID(28),'_FillValue',NF90_FILL_INT)
    IRET=NF90_PUT_ATT(NCID,VARID(28),'axis','W')

    !  station_name
    IRET=NF90_DEF_VAR(NCID, 'station_name', NF90_CHAR, (/DIMID(3),DIMID(2)/), VARID(2))
    CALL CHECK_ERR(IRET,27)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(2), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','station name')
    IRET=NF90_PUT_ATT(NCID,VARID(2),'content','XW')
    IRET=NF90_PUT_ATT(NCID,VARID(2),'associates','station string40')

    IF (FLWW3.NE.0) THEN
      !  wwIII param version
      IRET=NF90_DEF_VAR(NCID, 'WWIII_param_version', NF90_SHORT, (/DIMID(TWO),DIMID(ONE)/), VARID(3))
      CALL CHECK_ERR(IRET,28)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(3), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','WaveWatch III parameters version')
      IRET=NF90_PUT_ATT(NCID,VARID(3),'standard_name','WWIII_param_version')
      IRET=NF90_PUT_ATT(NCID,VARID(3),'globwave_name','WWIII_param_version')
      IRET=NF90_PUT_ATT(NCID,VARID(3),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(3),'scale_factor',1)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'add_offset',0)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_min',1)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_max',999)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'_FillValue',NF90_FILL_SHORT)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(3),'associates','time station')
    END IF

    IF (FLAGLL) THEN
      !  longitude
      IRET=NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, (/DIMID(TWO),DIMID(ONE)/), VARID(4))
      CALL CHECK_ERR(IRET,29)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'long_name','longitude')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'standard_name','longitude')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'globwave_name','longitude')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'units','degree_east')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_min',-180.0)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'associates','time station')

      !  latitude
      IRET=NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, (/DIMID(TWO),DIMID(ONE)/), VARID(5))
      CALL CHECK_ERR(IRET,30)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(5), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'long_name','latitude')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'standard_name','latitude')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'globwave_name','latitude')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'units','degree_north')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_min',-90.0)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_max',180.)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'associates','time station')


    ELSE
      !  longitude
      IRET=NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, (/DIMID(TWO),DIMID(ONE)/), VARID(4))
      CALL CHECK_ERR(IRET,31)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'long_name','x')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'standard_name','x')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'globwave_name','x')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'units','km')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(4),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(4),'associates','time station')


      !  latitude
      IRET=NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, (/DIMID(TWO),DIMID(ONE)/), VARID(5))
      CALL CHECK_ERR(IRET,32)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(5), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'long_name','y')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'standard_name','y')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'globwave_name','y')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'units','km')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(5),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(5),'associates','time station')

    END IF

    ! Process FREQ and DIR dimension values
    FREQ(1:NK)=SIG(1:NK)*TPIINV
    FREQ1(1:NK)=FREQ(1:NK)-0.5*(FREQ(1:NK)-(FREQ(1:NK)/XFR))
    FREQ2(1:NK)=FREQ(1:NK)+0.5*(-FREQ(1:NK)+(FREQ(1:NK)*XFR))
    FREQ1(1)=SIG(1)*TPIINV
    FREQ2(NK)=SIG(NK)*TPIINV
    DIR(1:NTH)=MOD(450-THD(1:NTH),360.)

    !
    ! ... ITYPE = 1 AND OTYPE = 2
    !

    IF (ITYPE.EQ.1 .AND. OTYPE.EQ.2) THEN
      !
      !     Define specifics dimensions
      !
      IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
      CALL CHECK_ERR(IRET,33)

      !
      !     define specifics variables
      !

      !  frequency
      IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, DIMID(4), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center band')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
      !d
      IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
      !Ust
      IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','friction velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','wind direction')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(10),'direction_reference','Rotated Pole Grid North')
      END IF
#endif


      !f/fp
      IRET=NF90_DEF_VAR(NCID, 'ffp', NF90_FLOAT, (/ DIMID(4),DIMID(TWO),DIMID(ONE) /), VARID(11))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','ffp')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','ffp')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','ffp')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'units','1')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station frequency')
      !F
      IRET=NF90_DEF_VAR(NCID, 'f', NF90_FLOAT, (/ DIMID(4),DIMID(TWO),DIMID(ONE) /), VARID(12))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(22), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','f')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','f')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','f')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',1000.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station frequency')
      !th1m
      IRET=NF90_DEF_VAR(NCID, 'th1m', NF90_FLOAT, (/ DIMID(4),DIMID(TWO),DIMID(ONE) /), VARID(13))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','mean wave direction from spectral moments')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','mean_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','mean_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station frequency')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(13),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(13),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !sth1m
      IRET=NF90_DEF_VAR(NCID, 'sth1m', NF90_FLOAT,(/ DIMID(4),DIMID(TWO),DIMID(ONE) /), VARID(14))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','directional spread from spectral moments')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','mean_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','mean_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station frequency')
      !alpha
      IRET=NF90_DEF_VAR(NCID, 'alpha', NF90_FLOAT, (/ DIMID(4),DIMID(TWO),DIMID(ONE) /), VARID(15))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','spectral intensity coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','spectral_intensity_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','spectral_intensity_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station frequency')


      ! Add values in netCDF file
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,34)
      IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
      CALL CHECK_ERR(IRET,35)


      !
      ! ... ITYPE = 1 AND OTYPE = 3
      !

    ELSE IF (ITYPE.EQ.1 .AND. OTYPE.EQ.3) THEN
      !
      !     Define specifics dimensions
      !
      IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
      CALL CHECK_ERR(IRET,36)
      IRET = NF90_DEF_DIM(NCID, 'direction', DIMLN(5), DIMID(5))
      CALL CHECK_ERR(IRET,37)

      !
      !     define specifics variables
      !

      !frequency
      IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, (/DIMID(4)/), VARID(6))
      CALL CHECK_ERR(IRET,38)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center band')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')

      !frequency1
      IRET=NF90_DEF_VAR(NCID, 'frequency1', NF90_FLOAT, (/DIMID(4)/), VARID(7))
      CALL CHECK_ERR(IRET,39)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','frequency of lower band')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','frequency_of_lower_band')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','frequency_lower_band')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','Y')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','frequency')

      !frequency2
      IRET=NF90_DEF_VAR(NCID, 'frequency2', NF90_FLOAT, (/DIMID(4)/), VARID(8))
      CALL CHECK_ERR(IRET,40)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','frequency of upper band')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','frequency_of_upper_band')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','frequency_upper_band')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','Y')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','frequency')

      !direction
      IRET=NF90_DEF_VAR(NCID, 'direction', NF90_FLOAT, (/DIMID(5)/), VARID(9))
      CALL CHECK_ERR(IRET,41)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','sea surface wave to direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','sea_surface_wave_to_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'axis','Z')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Efth
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_DEF_VAR(NCID,'efth',NF90_SHORT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(10))
      ELSE
        IRET=NF90_DEF_VAR(NCID,'efth',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(10))
      END IF
      CALL CHECK_ERR(IRET,42)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name',&
           'sea surface wave directional variance spectral density')
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name',&
             'base_ten_logarithm_of_sea_surface_wave_directional_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','directional_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'units','log10(m2 s rad-1 +1E-12)')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',0.0004)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',1.E20)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_SHORT)
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name',&
             'sea_surface_wave_directional_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','directional_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m2 s rad-1')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',1.E20)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      END IF
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TXYZ')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station frequency direction')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(10),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !d
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_SHORT, (/ DIMID(TWO),DIMID(ONE) /), VARID(11))
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(11))
      END IF
      CALL CHECK_ERR(IRET,43)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m')
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',0.5)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-200)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',200000)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_SHORT)
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-100.)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',10000.)
        IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
      END IF
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')

      !U10
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_SHORT, (/ DIMID(TWO),DIMID(ONE) /), VARID(12))
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(12))
      END IF
      CALL CHECK_ERR(IRET,44)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m s-1')
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',0.1)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',1000)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_SHORT)
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',100.)
        IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
      END IF
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')
      !Dir
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_SHORT, (/ DIMID(TWO),DIMID(ONE) /), VARID(13))
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(13))
      END IF
      CALL CHECK_ERR(IRET,45)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wind direction')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'units','degree')
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',0.1)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',3600)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_SHORT)
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',360.)
        IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
      END IF
      IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(13),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(13),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Uc
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'cur', NF90_SHORT, (/ DIMID(TWO),DIMID(ONE) /), VARID(14))
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(14))
      END IF
      CALL CHECK_ERR(IRET,46)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','sea water speed')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m s-1')
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',0.1)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',1000)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_SHORT)
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',100.)
        IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
      END IF
      IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station')

      !Dir
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_SHORT, (/ DIMID(TWO),DIMID(ONE) /), VARID(15))
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(15))
      END IF
      CALL CHECK_ERR(IRET,47)
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','direction from of sea water velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'units','degree')
      IF (NCVARTYPE.LE.3) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',0.1)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',3600)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_SHORT)
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',360.)
        IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
      END IF
      IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(15),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(15),'direction_reference','Rotated Pole Grid North')
      END IF
#endif


      ! Add values in netCDF file
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,48)
      IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
      CALL CHECK_ERR(IRET,49)
      IRET=NF90_PUT_VAR(NCID,VARID(7),FREQ1(1:NK))
      CALL CHECK_ERR(IRET,50)
      IRET=NF90_PUT_VAR(NCID,VARID(8),FREQ2(1:NK))
      CALL CHECK_ERR(IRET,51)
      IRET=NF90_PUT_VAR(NCID,VARID(9),DIR(1:NTH))
      CALL CHECK_ERR(IRET,52)




      !
      ! ... ITYPE = 1 AND OTYPE = 4
      !

    ELSE IF (ITYPE.EQ.1 .AND. OTYPE.EQ.4) THEN
      !
      !     Define specifics dimensions
      !
      IRET = NF90_DEF_DIM(NCID, 'npart', DIMLN(4), DIMID(4))
      CALL CHECK_ERR(IRET,53)

      !
      !     define specifics variables
      !

      !npart
      IRET=NF90_DEF_VAR(NCID, 'npart', NF90_INT, (/ DIMID(TWO),DIMID(ONE) /), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','npart')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','npart')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','npart')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_INT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')
      !d
      IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Uc
      IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','sea water speed')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(11))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name',' direction from of sea water velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(11),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(11),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Hs
      IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(12))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','spectral estimate of significant wave height')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','sea_surface_wave_significant_height')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','significant_wave_height')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station npart')
      !Tp
      IRET=NF90_DEF_VAR(NCID, 'tp', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(13))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','dominant_wave_period')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','dominant_wave_period')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','dominant_wave_period')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'units','s')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station npart')
      !L
      IRET=NF90_DEF_VAR(NCID, 'lp', NF90_FLOAT, (/DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(14))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','peak wave length')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','peak_wave_length')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','peak_wave_length')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station npart')
      !th1m
      IRET=NF90_DEF_VAR(NCID, 'th1m', NF90_FLOAT, (/DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(15))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','mean wave direction from spectral moments')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','mean_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','mean_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station npart')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(15),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(15),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !sth1m
      IRET=NF90_DEF_VAR(NCID, 'sth1m', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(16))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','directional spread from spectral moments')
      IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','mean_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','mean_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(16),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station npart')
      !ws
      IRET=NF90_DEF_VAR(NCID, 'ws', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(17))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(17), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(17),'long_name','wind sea fraction')
      IRET=NF90_PUT_ATT(NCID,VARID(17),'standard_name','wind_sea_fraction')
      IRET=NF90_PUT_ATT(NCID,VARID(17),'globwave_name','wind_sea_fraction')
      IRET=NF90_PUT_ATT(NCID,VARID(17),'units','%')
      IRET=NF90_PUT_ATT(NCID,VARID(17),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(17),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(17),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(17),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(17),'associates','time station npart')
      !TM10
      IRET=nf90_def_var(ncid, 'tm10', NF90_FLOAT,(/dimid(4),dimid(2),dimid(1)/),varid(18))
      IF (NCTYPE.EQ.4) IRET=nf90_def_var_deflate(ncid, varid(18), 1, 1, deflate)
      IRET=NF90_PUT_ATT(NCID,VARID(18),'long_name','mean wave period from spectral moments (-1,0)')
      IRET=NF90_PUT_ATT(NCID,VARID(18),'standard_name','mean_wave_period_tm10')
      IRET=NF90_PUT_ATT(NCID,VARID(18),'globwave_name','mean_wave_period_tm10')
      IRET=NF90_PUT_ATT(NCID,VARID(18),'units','seconds')
      IRET=NF90_PUT_ATT(NCID,VARID(18),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(18),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_max',30.)
      IRET=NF90_PUT_ATT(NCID,VARID(18),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(18),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(18),'associates','time station npart')
      !T01
      IRET=nf90_def_var(ncid, 't01', NF90_FLOAT,(/dimid(4),dimid(2),dimid(1)/),varid(19))
      IF (NCTYPE.EQ.4) IRET=nf90_def_var_deflate(ncid, varid(19), 1, 1, deflate)
      IRET=NF90_PUT_ATT(NCID,VARID(19),'long_name','mean wave period from spectral moments (0,1)')
      IRET=NF90_PUT_ATT(NCID,VARID(19),'standard_name','mean_wave_period_t01')
      IRET=NF90_PUT_ATT(NCID,VARID(19),'globwave_name','mean_wave_period_t01')
      IRET=NF90_PUT_ATT(NCID,VARID(19),'units','seconds')
      IRET=NF90_PUT_ATT(NCID,VARID(19),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(19),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_max',30.)
      IRET=NF90_PUT_ATT(NCID,VARID(19),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(19),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(19),'associates','time station npart')
      !T02
      IRET=nf90_def_var(ncid, 't02', NF90_FLOAT,(/dimid(4),dimid(2),dimid(1)/),varid(20))
      IF (NCTYPE.EQ.4) IRET=nf90_def_var_deflate(ncid, varid(20), 1, 1, deflate)
      IRET=NF90_PUT_ATT(NCID,VARID(20),'long_name','mean wave period from spectral moments (0,2)')
      IRET=NF90_PUT_ATT(NCID,VARID(20),'standard_name','mean_wave_period_t02')
      IRET=NF90_PUT_ATT(NCID,VARID(20),'globwave_name','mean_wave_period_t02')
      IRET=NF90_PUT_ATT(NCID,VARID(20),'units','seconds')
      IRET=NF90_PUT_ATT(NCID,VARID(20),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(20),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_max',30.)
      IRET=NF90_PUT_ATT(NCID,VARID(20),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(20),'content','TXY')
      IRET=NF90_PUT_ATT(NCID,VARID(20),'associates','time station npart')

      ! NF90_ENDDEF function
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,54)

      !
      ! ... ITYPE = 2 AND OTYPE = 1
      !


    ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.1) THEN
      !d
      IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',-100.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')

      !Uc
      IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','sea water speed')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','direction from of sea water velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(8),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(8),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','wind direction')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(10),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !zeta_setup
#ifdef W3_SETUP
      IRET=NF90_DEF_VAR(NCID, 'wave_setup', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(11))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','wave setup')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','wave_induced_setup')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','wave_induced_setup')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-100.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
#endif

      ! NF90_ENDDEF function
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,55)

      !
      ! ... ITYPE = 2 AND OTYPE = 2
      !

    ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.2) THEN
      !Hs
      IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','spectral estimate of significant wave height')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_significant_height')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','significant_wave_height')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
      !L
      IRET=NF90_DEF_VAR(NCID, 'lm', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','mean wave length')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','mean_wave_length')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','mean_wave_length')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
      !Tr
      IRET=NF90_DEF_VAR(NCID, 'tr', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','mean period normalised by the relative frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','mean_period_normalised_by_the_relative_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','mean period normalised by the relative frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','s')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
      !th1p
      IRET=NF90_DEF_VAR(NCID, 'th1p', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','mean wave direction from spectral moments at spectral peak')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','dominant_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','dominant_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !sth1p
      IRET=NF90_DEF_VAR(NCID, 'sth1p', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','directional spread at spectral peak')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','dominant_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','dominant_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
      !fp
      IRET=NF90_DEF_VAR(NCID, 'fp', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(11))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','  peak frequency (Fp=1/Tp)')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','dominant_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','dominant_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
      !th1m
      IRET=NF90_DEF_VAR(NCID, 'th1m', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(12))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','mean wave direction from spectral moments')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','mean_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','mean_wave_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(12),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(12),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !sth1m
      IRET=NF90_DEF_VAR(NCID, 'sth1m', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(13))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','directional spread from spectral moments')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','mean_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','mean_wave_spreading')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station')

      ! NF90_ENDDEF function
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,56)


      !
      ! ... ITYPE = 2 AND OTYPE = 3
      !

    ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.3) THEN
      !Ust
      IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','friction velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
      !Efst
      IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name',                     &
           'nondimensionalized using surface elevation variance spectrum')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','sea_surface_wave_variance_spectral_density')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','variance_spectral_density')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !fpst
      IRET=NF90_DEF_VAR(NCID, 'fpst', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','nondimensionalized using peak frequency (Fp=1/Tp)')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','dominant_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','dominant_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
      !Cd
      IRET=NF90_DEF_VAR(NCID, 'cd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','drag coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','drag_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','drag_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','*1000')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
      !alpha
      IRET=NF90_DEF_VAR(NCID, 'alpha', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','alpha')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','alpha')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','alpha')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','*100')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')


      ! NF90_ENDDEF function
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,57)


      !
      ! ... ITYPE = 2 AND OTYPE = 4
      !

    ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.4) THEN
      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
      !Efst
      IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name',                    &
           'nondimensionalized using surface elevation variance spectrum')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','sea_surface_wave_variance_spectral_density')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','variance_spectral_density')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !fpst
      IRET=NF90_DEF_VAR(NCID, 'fpst', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name',                    &
           'nondimensionalized using peak frequency (Fp=1/Tp)')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','dominant_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','dominant_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
      !Cd
      IRET=NF90_DEF_VAR(NCID, 'cd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','drag coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','drag_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','drag_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','*1000')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
      !alpha
      IRET=NF90_DEF_VAR(NCID, 'alpha', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','spectral intensity coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','spectral_intensity_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','spectral_intensity_coefficient')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','*100')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')

      ! NF90_ENDDEF function
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,58)


      !
      ! ... ITYPE = 2 AND OTYPE = 5
      !

    ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.5) THEN
      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','wind direction')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Hs
      IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','spectral estimate of significant wave height')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','sea_surface_wave_significant_height')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','significant_wave_height')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
      !Hsst
      IRET=NF90_DEF_VAR(NCID, 'hsst', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name',                    &
           'nondimensionalized using spectral estimate of significant wave height')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','sea_surface_wave_significant_height')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','significant_wave_height')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')
      !cp/U
      IRET=NF90_DEF_VAR(NCID, 'cpu', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','phase speed at peak frequency on friction velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','peak_wave_age')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','peak_wave_age')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')
      !cm/U
      IRET=NF90_DEF_VAR(NCID, 'cmu', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(11))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','phase speed at mean frequency on friction velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','mean_wave_age')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','mean_wave_age')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'units','-')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
      !Dt
      IRET=NF90_DEF_VAR(NCID, 'ast', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(12))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','air sea temperature difference')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','air_sea_temperature_difference')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','air_sea_temperature_difference')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')


      ! NF90_ENDDEF function
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,59)


      !
      ! ... ITYPE = 2 AND OTYPE = 6
      !

    ELSE IF (ITYPE.EQ.2 .AND. OTYPE.EQ.6) THEN
      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','wind direction')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(7),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Hs
      IRET=NF90_DEF_VAR(NCID, 'hs', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','spectral estimate of significant wave height')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','sea_surface_wave_significant_height')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','significant_wave_height')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')
      !Tp
      IRET=NF90_DEF_VAR(NCID, 'tp', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','dominant wave period')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','dominant_wave_period')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','dominant_wave_period')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','s')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')

      ! NF90_ENDDEF function
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,60)

      !
      ! ... ITYPE = 3 AND OTYPE = 2
      !
    ELSE IF (ITYPE.EQ.3 .AND. OTYPE.EQ.2) THEN
      !
      !     Define specifics dimensions
      !
      IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
      CALL CHECK_ERR(IRET,61)

      !
      !     define specifics variables
      !

      !  frequency / frequencyst / ffp
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, DIMID(4), VARID(6))
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_DEF_VAR(NCID, 'frequencyst', NF90_FLOAT, DIMID(4), VARID(6))
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'ffp', NF90_FLOAT, DIMID(4), VARID(6))
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center band')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')

      !d
      IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')

      !Ust
      IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','friction velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')

      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')

      !Ef / Efst
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'ef', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(10))
        IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','surface elevation variance spectrum')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_wave_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m2 s')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(10))
        IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name',                 &
             'nondimensionalized using surface elevation variance spectrum')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_wave_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station ffp')
      END IF

      !Sin / Sinst
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'sin', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(11))
        IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','wind input source term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'sinst', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(11))
        IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name',                 &
             'nondimensionalized using wind input source term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station ffp')
      END IF

      !Snl / Snlst
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'snl', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(12))
        IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','nonlinear 4 wave source term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'snlst', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(12))
        IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name',                 &
             'nondimensionalized using nonlinear 4 wave source term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station ffp')
      END IF

      !Sds / Sdsst
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'sds', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(13))
        IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wave breaking source term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'sdsst', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(13))
        IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name',                 &
             'nondimensionalized using wave breaking source term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station ffp')
      END IF

      !Sbt / Sbtst
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'sbt', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(14))
        IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','depth induced breaking source term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'sbtst', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(14))
        IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name',                 &
             'nondimensionalized using depth induced breaking source term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station ffp')
      END IF

      !Sice / Sicest
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'sice', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(15))
        IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','wave-ice interactions source term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'sicest', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(15))
        IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','nondimensionalized using wave-ice interactions source term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station ffp')
      END IF

      !Stot / Stotst
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'stot', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(16))
        IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','total source term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'stotst', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(16))
        IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','nondimensionalized using total source term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station ffp')
      END IF

      !  Add values in netCDF file
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,62)
      IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
      CALL CHECK_ERR(IRET,63)


      !
      ! ... ITYPE = 3 AND OTYPE = 3
      !
    ELSE IF (ITYPE.EQ.3 .AND. OTYPE.EQ.3) THEN
      !
      !     Define specifics dimensions
      !
      IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
      CALL CHECK_ERR(IRET,64)

      !
      !     define specifics variables
      !

      !  frequency / frequencyst / ffp
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, DIMID(4), VARID(6))
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_DEF_VAR(NCID, 'frequencyst', NF90_FLOAT, DIMID(4), VARID(6))
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'ffp', NF90_FLOAT, DIMID(4), VARID(6))
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center band')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')

      !d
      IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-100.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time station')

      !Ust
      IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','friction velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time station')

      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time station')

      !Ef / Efst
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'ef', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(10))
        IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','surface elevation variance spectrum')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_wave_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m2 s')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'efst', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(10))
        IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name',                 &
             'nondimensionalized using surface elevation variance spectrum')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','sea_surface_wave_variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','variance_spectral_density')
        IRET=NF90_PUT_ATT(NCID,VARID(10),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station ffp')
      END IF

      !Tini / Tinist
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'tini', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(11))
        IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','wind input source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','inverse_time_scales_wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','inverse_time_scales_wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'tinist', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(11))
        IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','nondimensionalized using wind input source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','inverse_time_scales_wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','inverse_time_scales_wind_input_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(11),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station ffp')
      END IF


      !Tnli / Tnlist
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'tnli', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(12))
        IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','nonlinear 4 wave source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','inverse_time_scales_nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','inverse_time_scales_nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'tnlist', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(12))
        IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','nondimensionalized using nonlinear 4 wave source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','inverse_time_scales_nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','inverse_time_scales_nonlinear_4_wave_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(12),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station ffp')
      END IF


      !Tdsi / Tdsist
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'tdsi', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(13))
        IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wave breaking source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','inverse_time_scales_wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','inverse_time_scales_wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'tdsist', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(13))
        IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','nondimensionalized using wave breaking source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','inverse_time_scales_wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','inverse_time_scales_wave_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(13),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station ffp')
      END IF

      !Tbti / Tbtist
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'tbti', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(14))
        IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','depth induced breaking source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','inverse_time_scales_depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','inverse_time_scales_depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'tbtist', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(14))
        IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','nondimensionalized using depth induced breaking source term &
             normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','inverse_time_scales_depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','inverse_time_scales_depth_induced_breaking_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(14),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station ffp')
      END IF

      !Ticei / Ticeist
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'ticei', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(15))
        IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','wave ice interactions source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','inverse_time_scales_wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','inverse_time_scales_wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'ticeist', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(15))
        IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','nondimensionalized using wave ice interactions source term &
             normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','inverse_time_scales_wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','inverse_time_scales_wave_ice_interactions_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(15),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station ffp')
      END IF

      !Ttoti / Ttotist
      IF (ISCALE.EQ.0 .OR. ISCALE.EQ.3) THEN
        IRET=NF90_DEF_VAR(NCID, 'ttoti', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(16))
        IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','total source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','inverse_time_scales_total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','inverse_time_scales_total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'units','m2')
      ELSE
        IRET=NF90_DEF_VAR(NCID, 'ttotist', NF90_FLOAT,(/DIMID(4),DIMID(TWO),DIMID(ONE)/), VARID(16))
        IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','nondimensionalized using total source term normalised by Ef')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','inverse_time_scales_total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','inverse_time_scales_total_source_term')
        IRET=NF90_PUT_ATT(NCID,VARID(16),'units','-')
      END IF
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',-1.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue', NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXY')
      IF (ISCALE.EQ.0) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station frequency')
      ELSE IF ( ISCALE.EQ.1 .OR. ISCALE.EQ.2 ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station frequencyst')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station ffp')
      END IF

      !  Add values in netCDF file
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,65)
      IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
      CALL CHECK_ERR(IRET,66)


      !
      ! ... ITYPE = 3 AND OTYPE = 4
      !
    ELSE IF (ITYPE.EQ.3 .AND. OTYPE.EQ.4) THEN
      !
      !     Define specifics dimensions
      !
      IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(4), DIMID(4))
      IRET = NF90_DEF_DIM(NCID, 'direction', DIMLN(5), DIMID(5))
      CALL CHECK_ERR(IRET,67)

      !
      !     define specifics variables
      !

      !frequency
      IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, (/DIMID(4)/), VARID(6))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','frequency of center band')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'globwave_name','frequency')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Y')

      !frequency1
      IRET=NF90_DEF_VAR(NCID, 'frequency1', NF90_FLOAT, (/DIMID(4)/), VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','frequency of lower band')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','frequency_of_lower_band')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'globwave_name','frequency_lower_band')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','Y')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','frequency')

      !frequency2
      IRET=NF90_DEF_VAR(NCID, 'frequency2', NF90_FLOAT, (/DIMID(4)/), VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','frequency of upper band')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','frequency_of_upper_band')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'globwave_name','frequency_upper_band')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',10.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','Y')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','frequency')


      !direction
      IRET=NF90_DEF_VAR(NCID, 'direction', NF90_FLOAT, (/DIMID(5)/), VARID(9))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name','sea surface wave to direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name','sea_surface_wave_to_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name','direction')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(9),'axis','Z')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(9),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !d
      IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(10))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','depth')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_min',-100.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'valid_max',10000.)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(10),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time station')

      !U10
      IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(11))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','wind speed at 10m')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','wind_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(11),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time station')
      !Dir
      IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(12))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','wind direction')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','wind_from_direction')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(12),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(12),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(12),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Uc
      IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(13))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','sea water speed')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','sea_water_speed')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(13),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time station')

      !Dir
      IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(14))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','direction from of sea water velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','direction_of_sea_water_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'units','degree')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(14),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time station')
#ifdef W3_RTD
      IF ( FLAGUNR ) THEN
        IRET=NF90_PUT_ATT(NCID,VARID(14),'direction_reference','True North')
      ELSE
        IRET=NF90_PUT_ATT(NCID,VARID(14),'direction_reference','Rotated Pole Grid North')
      END IF
#endif

      !Ust
      IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT, (/ DIMID(TWO),DIMID(ONE) /), VARID(15))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','friction velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','friction_velocity')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m s-1')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_min',0.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'valid_max',100.)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(15),'content','TX')
      IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time station')

      !Efth
      IF ( PRESENT(FLSRCE) ) THEN
        IF ( FLSRCE(1) )  THEN
          IRET=NF90_DEF_VAR(NCID,'efth',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(16))
          IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, DEFLATE)
          IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name',&
               'sea surface wave directional variance spectral density')
          IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name',&
               'sea_surface_wave_directional_variance_spectral_density')
          IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','directional_variance_spectral_density')
          IRET=NF90_PUT_ATT(NCID,VARID(16),'units','m2 s rad-1')
          IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_min',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(16),'valid_max',1.E20)
          IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue',NF90_FILL_FLOAT)
          IRET=NF90_PUT_ATT(NCID,VARID(16),'content','TXYZ')
          IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time station frequency direction')
#ifdef W3_RTD
          IF ( FLAGUNR ) THEN
            IRET=NF90_PUT_ATT(NCID,VARID(16),'direction_reference','True North')
          ELSE
            IRET=NF90_PUT_ATT(NCID,VARID(16),'direction_reference','Rotated Pole Grid North')
          END IF
#endif
        ENDIF

        !Swn
        IF ( FLSRCE(2) )  THEN
          IRET=NF90_DEF_VAR(NCID,'sin',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(17))
          IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(17), 1, 1, DEFLATE)
          IRET=NF90_PUT_ATT(NCID,VARID(17),'long_name','wind input source term')
          IRET=NF90_PUT_ATT(NCID,VARID(17),'standard_name','wind_input_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(17),'globwave_name','wind_input_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(17),'units','m2 rad-1')
          IRET=NF90_PUT_ATT(NCID,VARID(17),'scale_factor',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(17),'add_offset',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_min',-1.)
          IRET=NF90_PUT_ATT(NCID,VARID(17),'valid_max',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(17),'_FillValue',NF90_FILL_FLOAT)
          IRET=NF90_PUT_ATT(NCID,VARID(17),'content','TXYZ')
          IRET=NF90_PUT_ATT(NCID,VARID(17),'associates','time station frequency direction')
#ifdef W3_RTD
          IRET=NF90_PUT_ATT(NCID,VARID(17),'direction_reference','Rotated Pole Grid North')
#endif
        ENDIF

        !Snl
        IF ( FLSRCE(3) )  THEN
          IRET=NF90_DEF_VAR(NCID,'snl',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(18))
          IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(18), 1, 1, DEFLATE)
          IRET=NF90_PUT_ATT(NCID,VARID(18),'long_name','nonlinear 4 wave source term')
          IRET=NF90_PUT_ATT(NCID,VARID(18),'standard_name','nonlinear_4_wave_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(18),'globwave_name','nonlinear_4_wave_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(18),'units','m2 rad-1')
          IRET=NF90_PUT_ATT(NCID,VARID(18),'scale_factor',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(18),'add_offset',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_min',-1.)
          IRET=NF90_PUT_ATT(NCID,VARID(18),'valid_max',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(18),'_FillValue',NF90_FILL_FLOAT)
          IRET=NF90_PUT_ATT(NCID,VARID(18),'content','TXYZ')
          IRET=NF90_PUT_ATT(NCID,VARID(18),'associates','time station frequency direction')
#ifdef W3_RTD
          IRET=NF90_PUT_ATT(NCID,VARID(18),'direction_reference','Rotated Pole Grid North')
#endif
        ENDIF

        !Sds
        IF ( FLSRCE(4) )  THEN
          IRET=NF90_DEF_VAR(NCID,'sds',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(19))
          IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(19), 1, 1, DEFLATE)
          IRET=NF90_PUT_ATT(NCID,VARID(19),'long_name','wave breaking source term')
          IRET=NF90_PUT_ATT(NCID,VARID(19),'standard_name','wave_breaking_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(19),'globwave_name','wave_breaking_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(19),'units','m2 rad-1')
          IRET=NF90_PUT_ATT(NCID,VARID(19),'scale_factor',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(19),'add_offset',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_min',-1.)
          IRET=NF90_PUT_ATT(NCID,VARID(19),'valid_max',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(19),'_FillValue',NF90_FILL_FLOAT)
          IRET=NF90_PUT_ATT(NCID,VARID(19),'content','TXYZ')
          IRET=NF90_PUT_ATT(NCID,VARID(19),'associates','time station frequency direction')
#ifdef W3_RTD
          IRET=NF90_PUT_ATT(NCID,VARID(19),'direction_reference','Rotated Pole Grid North')
#endif
        ENDIF

        !Sbt
        IF ( FLSRCE(5) )  THEN
          IRET=NF90_DEF_VAR(NCID,'sbt',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(20))
          IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(20), 1, 1, DEFLATE)
          IRET=NF90_PUT_ATT(NCID,VARID(20),'long_name','depth induced breaking source term')
          IRET=NF90_PUT_ATT(NCID,VARID(20),'standard_name','depth_induced_breaking_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(20),'globwave_name','depth_induced_breaking_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(20),'units','m2 rad-1')
          IRET=NF90_PUT_ATT(NCID,VARID(20),'scale_factor',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(20),'add_offset',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_min',-1.)
          IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_max',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(20),'_FillValue',NF90_FILL_FLOAT)
          IRET=NF90_PUT_ATT(NCID,VARID(20),'content','TXYZ')
          IRET=NF90_PUT_ATT(NCID,VARID(20),'associates','time station frequency direction')
#ifdef W3_RTD
          IRET=NF90_PUT_ATT(NCID,VARID(20),'direction_reference','Rotated Pole Grid North')
#endif
        ENDIF

        !Sice
        IF ( FLSRCE(6) )  THEN
          IRET=NF90_DEF_VAR(NCID,'sice',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(21))
          IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(21), 1, 1, DEFLATE)
          IRET=NF90_PUT_ATT(NCID,VARID(21),'long_name','wave-ice interactions source term')
          IRET=NF90_PUT_ATT(NCID,VARID(21),'standard_name','wave_ice_intercations_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(21),'globwave_name','wave_ice_intercations_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(21),'units','m2 rad-1')
          IRET=NF90_PUT_ATT(NCID,VARID(21),'scale_factor',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(21),'add_offset',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(21),'valid_min',-1.)
          IRET=NF90_PUT_ATT(NCID,VARID(21),'valid_max',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(21),'_FillValue',NF90_FILL_FLOAT)
          IRET=NF90_PUT_ATT(NCID,VARID(21),'content','TXYZ')
          IRET=NF90_PUT_ATT(NCID,VARID(21),'associates','time station frequency direction')
#ifdef W3_RTD
          IRET=NF90_PUT_ATT(NCID,VARID(21),'direction_reference','Rotated Pole Grid North')
#endif
        ENDIF

        !Stt
        IF ( FLSRCE(7) )  THEN
          IRET=NF90_DEF_VAR(NCID,'stt',NF90_FLOAT,(/DIMID(5),DIMID(4),DIMID(TWO),DIMID(ONE)/),VARID(22))
          IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(22), 1, 1, DEFLATE)
          IRET=NF90_PUT_ATT(NCID,VARID(22),'long_name','total source term')
          IRET=NF90_PUT_ATT(NCID,VARID(22),'standard_name','total_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(22),'globwave_name','total_source_term')
          IRET=NF90_PUT_ATT(NCID,VARID(22),'units','m2 rad-1')
          IRET=NF90_PUT_ATT(NCID,VARID(22),'scale_factor',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(22),'add_offset',0.)
          IRET=NF90_PUT_ATT(NCID,VARID(22),'valid_min',-1.)
          IRET=NF90_PUT_ATT(NCID,VARID(22),'valid_max',1.)
          IRET=NF90_PUT_ATT(NCID,VARID(22),'_FillValue',NF90_FILL_FLOAT)
          IRET=NF90_PUT_ATT(NCID,VARID(22),'content','TXYZ')
          IRET=NF90_PUT_ATT(NCID,VARID(22),'associates','time station frequency direction')
#ifdef W3_RTD
          IRET=NF90_PUT_ATT(NCID,VARID(22),'direction_reference','Rotated Pole Grid North')
#endif
        ENDIF
      ENDIF

      !  Add values in netCDF file
      IRET=NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET,68)
      IRET=NF90_PUT_VAR(NCID,VARID(6),FREQ(1:NK))
      CALL CHECK_ERR(IRET,69)
      IRET=NF90_PUT_VAR(NCID,VARID(7),FREQ1(1:NK))
      CALL CHECK_ERR(IRET,70)
      IRET=NF90_PUT_VAR(NCID,VARID(8),FREQ2(1:NK))
      CALL CHECK_ERR(IRET,71)
      IRET=NF90_PUT_VAR(NCID,VARID(9),DIR(1:NTH))
      CALL CHECK_ERR(IRET,72)
      !
    END IF
    !
    RETURN

  END SUBROUTINE W3CRNC

  !==============================================================================
  !> @brief Desc not available.
  !>
  !> @param IRET
  !> @param ICODE
  !>
  !> @author NA  @date NA
  SUBROUTINE CHECK_ERR(IRET,ICODE)

    USE NETCDF
    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE

    IMPLICIT NONE

    INTEGER IRET, ICODE

    IF (IRET .NE. NF90_NOERR) THEN
      WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN OUNP :'
      WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
      WRITE(NDSE,*) NF90_STRERROR(IRET)
      WRITE(NDSE,*) ' ICODE: '
      WRITE(NDSE,*) ICODE
      CALL EXTCDE ( ICODE )
    END IF
    RETURN

  END SUBROUTINE CHECK_ERR

  !==============================================================================


  !/
  !/ End of W3OUNP ----------------------------------------------------- /
  !/
END PROGRAM W3OUNP
