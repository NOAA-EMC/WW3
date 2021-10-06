#include "w3macros.h"
#define CHECK_ERR(I) CHECK_ERROR(I, __LINE__)
!/ ------------------------------------------------------------------- /
      PROGRAM W3OUNF
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           F. Ardhuin              |
!/                  |           M. Accensi              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Sep-2021 |
!/                  +-----------------------------------+
!/
!/    17-Mar-2010 : Creation                            ( version 3.14_SHOM )
!/    07-Nov-2011 : Debug for spectral output on UNST   ( version 4.04 )
!/    13-Mar-2012 : Update of NC attributes             ( version 4.04 )
!/    02-Apr-2013 : New structure of output fields.     ( version 4.10 )
!/    02-Jul-2013 : Bug correction for lat in unst grid ( version 4.11 )
!/    02-Nov-2013 : Removes unnecessary IDFM            ( version 4.12 )
!/    30-Apr-2014 : Correct group3 freq dim.            ( version 5.00 )
!/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
!/    14-Oct-2014 : Keep the output files opened        ( version 5.01 )
!/    27-Aug-2015 : ICEH and ICEF added as output       ( version 5.10 )
!/    10-Jan-2017 : Changes for US3D and USSP output    ( version 6.01 )
!/    01-May-2017 : Adds directional MSS parameters     ( version 6.04 )
!/    01-Mar-2018 : RTD option add variable de-rotation,( version 6.02 )
!/                  standard lat-lons and rotated grid
!/                  metadata
!/    15-May-2018 : Add namelist feature                ( version 6.05 )
!/    06-Jun-2018 : Add DEBUG/SETUP                     ( version 6.04 )
!/    27-Jun-2018 : Updated to handle SMC output.       ( version 6.05 )
!/    26-Jul-2018 : Changed reading of TABIPART         ( version 6.05 )
!/    12-Sep-2018 : Added extra partitioned fields      ( version 6.06 )
!/    25-Sep-2018 : Add WBT parameter                   ( version 6.06 )
!/    28-Mar-2019 : Bugfix to NBIPART check.            ( version 6.07 )
!/    18-Jun-2020 : Support for 360-day calendar.       ( version 7.08 )
!/    07-Oct-2019 : RTD option with standard lat-lon
!/                  grid when nesting to rotated grid   ( version 7.11 )
!/    03-Nov-2020 : Moved NetCDF metadata to separate   ( version 7.12 )
!/                  module.
!/    09-Dec-2020 : Set fixed values for VARID indices  ( version 7.12 )
!/    06-Jan-2021 : Added forecast_period and           ( version 7.12 )
!/                  forecast_reference_time variables.
!/    12-Jan-2021 : Alternative vartype and units for   ( version 7.12 )
!/                  time variables.
!/    26-Jan-2021 : Added TP output (derived from fp)   ( version 7.12 )
!/                  and alternative dir/mag output.
!/    02-Feb-2021 : Make default global meta optional   ( version 7.12 )
!/    22-Mar-2021 : New coupling fields output          ( version 7.12 )
!/    02-Sep-2021 : Added coordinates attribute         ( version 7.12 )
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Post-processing of grid output to NetCDF files
!
!  2. Method :
!
!     Data is read from the grid output file out_grd.ww3 (raw data)
!     and from the file ww3_ounf.nml or ww3_ounf.inp (NDSI)
!     Model definition and raw data files are read using WAVEWATCH III
!     subroutines. Extra global NetCDF attributes may be read from
!     ASCII file NC_globatt.inp.
!
!     Output types :
!      4 : NetCDF files
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
!      W2NAUX    Subr. W3ADATMD Set number of model for aux data.
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
!      W3EXNC    Subr. Internal Execute grid netcdf output.
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
!     The VARID array stores netCDF variable IDs for all variables in
!     file. The first 20 elements are reserved for dimension/auxiliary
!     variables as defined below:
!
!       Index     Variable
!       =====     ========
!         1       Lon
!         2       Lat
!         3       Time
!         4       Tri (UGRD)
!         5       SMC CX (SMC)
!         6       SMC CY (SMC)
!         7       Standard longitude (SMC/RTD)
!         8       Standard latitude (SMC/RTD)
!         9       Coordinate reference system (upcoming feature / RTD)
!        10       Freq (extradim)
!        11       Forecast period (upcoming feature)
!        12       Forecast reference time (upcoming feature)
!        13-19    [Reserved for future use]
!        20       MAPSTA
!
!    Indices 21 - 300 are for storage of field output variable IDs.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S     Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS

!/
      USE W3WDATMD, ONLY: W3NDAT, W3SETW
      USE W3ADATMD, ONLY: W3NAUX, W3SETA
      USE W3ODATMD, ONLY: W3NOUT, W3SETO
      USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE, STR_TO_UPPER
#ifdef W3_S
      USE W3SERVMD, ONLY : STRACE
#endif
      USE W3TIMEMD
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3IOGOMD, ONLY: W3IOGO, W3READFLGRD, W3FLGRDFLAG
      USE W3INITMD, ONLY: WWVER, SWITCHES
      USE W3ODATMD, ONLY: NAPROC, NOSWLL, PTMETH, PTFCUT
#ifdef W3_DEBUG
      USE W3ODATMD, only : IAPROC
#endif
!/
      USE W3GDATMD
      USE W3WDATMD, ONLY: TIME, WLV, ICE, ICEH, ICEF, BERG,            &
                          UST, USTDIR, RHOAIR
#ifdef W3_SETUP
     USE W3WDATMD, ONLY: ZETA_SETUP
#endif
      USE W3ADATMD, ONLY: DW, UA, UD, AS, CX, CY, HS, WLM, T0M1, THM,  &
                          THS, FP0, THP0, DTDYN, FCUT,                 &
                          ABA, ABD, UBA, UBD, SXX, SYY, SXY, USERO,    &
                          PHS, PTP, PLP, PDIR, PSI, PWS, PWST, PNR,    &
                          PTM1, PT1, PT2, PEP, TAUOCX, TAUOCY,         &
                          PTHP0, PQP, PSW, PPE, PGW, QP,               &
                          TAUOX, TAUOY, TAUWIX,                        &
                          TAUWIY, PHIAW, PHIOC, TUSX, TUSY, PRMS, TPMS,&
                          USSX, USSY, MSSX, MSSY, MSSD, MSCX, MSCY,    &
                          MSCD, CHARN, TWS, TAUA, TAUADIR,             &
                          TAUWNX, TAUWNY, BHD, T02, HSIG, CGE,         &
                          T01, BEDFORMS, WHITECAP, TAUBBL, PHIBBL,     &
                          CFLTHMAX, CFLXYMAX, CFLKMAX, TAUICE, PHICE,  &
                          STMAXE, STMAXD, HMAXE, HCMAXE, HMAXD, HCMAXD,&
                          P2SMS, EF, US3D, TH1M, STH1M, TH2M, STH2M,   &
                          WN, USSP, WBT, WNMEAN
      USE W3ODATMD, ONLY: NDSO, NDSE, SCREEN, NOGRP, NGRPP, IDOUT,     &
                          UNDEF, FLOGRD, FNMPRE, NOSWLL, NOGE
!
      USE W3NMLOUNFMD
!
      USE W3OUNFMETAMD, ONLY: INIT_META, TEARDOWN_META, GETMETA,       &
                              WRITE_META, WRITE_GLOBAL_META,           &
                              WRITE_FREEFORM_META_LIST,                &
                              META_T, NCVARTYPE, CRS_META, CRS_NAME,   &
                              FL_DEFAULT_GBL_META, COORDS_ATTR
!
      USE NETCDF

#ifdef W3_SMC
      USE W3SMCOMD, SMCNOVAL=>NOVAL
#endif

      IMPLICIT NONE

!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      TYPE(NML_FIELD_T)       :: NML_FIELD
      TYPE(NML_FILE_T)        :: NML_FILE
      TYPE(NML_SMC_T)         :: NML_SMC
!
      INTEGER                 :: NDSI, NDSM, NDSOG,                    &
                                 NDSTRC, NTRACE, IERR, I, I1F, I2F,    &
                                 IOTEST, NOUT,                         &
                                 IFI, IFJ, NCTYPE, IX1, IXN, IY1, IYN, &
                                 IOUT, S3, IRET,                       &
                                 NBIPART, CNTIPART, NCVARTYPEI, IPART, &
                                 RTDNX, RTDNY
      INTEGER                 :: TOUT(2), TDUM(2), TREF(2), TEPOCH(2), &
                                 STOPDATE(8), REFDATE(8)
!
      INTEGER, ALLOCATABLE    :: TABIPART(:), NCIDS(:,:,:)
!
#ifdef W3_S
      INTEGER, SAVE           :: IENT = 0
#endif
!
      REAL                    :: DTREQ, DTEST
!
      CHARACTER*30            :: STRSTOPDATE, FILEPREFIX, STRINGIPART
      CHARACTER*1024          :: FLDOUT
      CHARACTER               :: COMSTR*1, IDTIME*23, IDDDAY*11, TTYPE*1
!
      LOGICAL                 :: FLG2D(NOGRP,NGRPP), FLG1D(NOGRP),     &
                                 VECTOR, TOGETHER, FLGNML, FLGFC
      LOGICAL                 :: MAPSTAOUT = .TRUE.
      LOGICAL                 :: SMCGRD = .FALSE.
#ifdef W3_RTD
      LOGICAL                 :: RTDL = .FALSE.
#endif

      INTEGER                 :: TVARTYPE = NF90_DOUBLE
      CHARACTER(LEN=32)       :: EPOCH_ISO
      CHARACTER(LEN=64)       :: EPOCH
      CHARACTER               :: TIMEUNIT*1 ! 'D' = days, or 'S' for seconds
!
      REAL                    :: NOVAL      ! Fill value for seapoints with no value
!/
!/ ------------------------------------------------------------------- /
!/
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
!
      NDSTRC =  6
      NTRACE = 10
      CALL ITRACE ( NDSTRC, NTRACE )
!
#ifdef W3_S
      CALL STRACE (IENT, 'W3OUNF')
#endif
!
      WRITE (NDSO,900)
!
      ! Default epoch time:
      TEPOCH(1) = 19900101
      TEPOCH(2) = 0
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Read model definition file.
!
      CALL W3IOGR ( 'READ', NDSM )
      WRITE (NDSO,920) GNAME
!
#ifdef W3_RTD
 ! Is the grid really rotated?
      IF ( Polat < 90. ) RTDL = .True.
 !
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Read general data and first fields from file
!
#ifdef W3_DEBUG
      WRITE (NDSO,*) 'Before FLOGRD(2,1)=', FLOGRD(2,1)
      WRITE (NDSO,*) 'IAPROC=', IAPROC
      WRITE(740+IAPROC,*) 'Calling W3IOGO from ww3_ounf'
      FLUSH(740+IAPROC)
#endif
      CALL W3IOGO ( 'READ', NDSOG, IOTEST )
!
      WRITE (NDSO,930)
      DO IFI=1, NOGRP
        DO IFJ=1, NGRPP
          IF ( FLOGRD(IFI,IFJ) ) WRITE (NDSO,931) IDOUT(IFI,IFJ)
        END DO
      END DO
!
#ifdef W3_SMC
      IF( GTYPE .EQ. SMCTYPE )  THEN
          SMCGRD = .TRUE.
          WRITE (NDSO, *) " Conversion for SMCTYPE:", GTYPE
      ENDIF 
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4.  Read requests from input file.
!
! process ww3_ounf namelist
!
      INQUIRE(FILE=TRIM(FNMPRE)//"ww3_ounf.nml", EXIST=FLGNML)
      IF (FLGNML) THEN
        ! Read namelist
        CALL W3NMLOUNF (NDSI, TRIM(FNMPRE)//'ww3_ounf.nml', NML_FIELD, &
                        NML_FILE, NML_SMC, IERR)

! 4.1 Time setup
        READ(NML_FIELD%TIMESTRIDE, *)  DTREQ
        READ(NML_FIELD%TIMECOUNT, *)   NOUT
        READ(NML_FIELD%TIMESTART, *)   TOUT(1), TOUT(2)
        READ(NML_FIELD%TIMEREF, *)     TREF(1), TREF(2)
        READ(NML_FIELD%TIMEEPOCH, *)   TEPOCH(1), TEPOCH(2)

! 4.2 Output fields
        FLDOUT = NML_FIELD%LIST
        CALL W3FLGRDFLAG ( NDSO, SCREEN, NDSE, FLDOUT, FLG1D,       &
                           FLG2D, 1, 1, IERR )
        IF (IERR.NE.0) GOTO 800

! 4.3 Output type
        NCTYPE = NML_FILE%NETCDF
        NCVARTYPE = NML_FIELD%TYPE
        STRINGIPART = NML_FIELD%PARTITION
        TOGETHER = NML_FIELD%SAMEFILE
        VECTOR = NML_FIELD%VECTOR
        FILEPREFIX = NML_FILE%PREFIX
        FLGFC = NML_FIELD%FCVARS
        S3 = NML_FIELD%TIMESPLIT
        TTYPE = NML_FIELD%TIMEVAR
        TIMEUNIT = NML_FIELD%TIMEUNIT
        NOVAL = NML_FIELD%NOVAL
        MAPSTAOUT = NML_FIELD%MAPSTA
        IF(SMCGRD) THEN
#ifdef W3_SMC
          SMCOTYPE = NML_SMC%TYPE
          SXO = NML_SMC%SXO
          SYO = NML_SMC%SYO
          EXO = NML_SMC%EXO
          EYO = NML_SMC%EYO
          CELFAC = NML_SMC%CELFAC
          SMCNOVAL = NOVAL
#endif
        ELSE
          IX1 = NML_FILE%IX0
          IXN = NML_FILE%IXN
          IY1 = NML_FILE%IY0
          IYN = NML_FILE%IYN
        ENDIF ! SMCGRD
      END IF ! FLGNML
!
! process old ww3_ounf.inp format
!
      IF (.NOT. FLGNML) THEN
        OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_ounf.inp',STATUS='OLD',ERR=800,IOSTAT=IERR)
        REWIND (NDSI)

        READ (NDSI,'(A)',END=801,ERR=802,IOSTAT=IERR) COMSTR
        IF (COMSTR.EQ.' ') COMSTR = '$'
        WRITE (NDSO,901) COMSTR
        CALL NEXTLN ( COMSTR , NDSI , NDSE )

! 4.1 Time setup
        READ (NDSI,*,END=801,ERR=802) TOUT, DTREQ, NOUT

! 4.1.1 Forecast period and forecast reference time
!        CALL NEXTLN ( COMSTR , NDSI , NDSE )
!        READ (NDSI,*,END=801,ERR=802) FLGFC
!        IF( FLGFC ) READ(NDSI,*,END=801,ERR=802) TREF
!
        ! ChrisB: Forecast variables flag and reference time
        ! only configurable via namelist input. Set forecast
        ! reference time to first time here:
        TREF = TOUT

! 4.2 Output fields
        CALL W3READFLGRD ( NDSI, NDSO, SCREEN, NDSE, COMSTR, FLG1D,      &
                           FLG2D, 1, 1, IERR )
        IF (IERR.NE.0) GOTO 800

! 4.3 Output type
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=801,ERR=802) NCTYPE, NCVARTYPE
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,'(A)',END=801,ERR=802) STRINGIPART
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=801,ERR=802) TOGETHER

!       The following are only configurable via the namelist input
!       and are hardcoded for .inp files:
        TTYPE = "D"
        TIMEUNIT = "D"
        NOVAL = UNDEF 
        VECTOR = .TRUE.

        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        FILEPREFIX= 'ww3.'
        READ (NDSI,*,END=801,ERR=802) FILEPREFIX
        CALL NEXTLN ( COMSTR , NDSI , NDSE )
        READ (NDSI,*,END=801,ERR=802) S3
        CALL NEXTLN ( COMSTR , NDSI , NDSE )

        IF(SMCGRD) THEN
#ifdef W3_SMC
        ! SMC output type (1 or 2)
        READ (NDSI,*,END=801,ERR=802) SMCOTYPE
        IF(SMCOTYPE .EQ. 1) THEN  ! Flat sea point output
           CALL NEXTLN ( COMSTR , NDSI , NDSE )
           READ (NDSI,*,END=801,ERR=802) SXO, SYO, EXO, EYO
        ELSE IF(SMCOTYPE .EQ. 2) THEN  ! Regular grid output
           CALL NEXTLN ( COMSTR , NDSI , NDSE )
           READ (NDSI,*,END=801,ERR=802) SXO, SYO, EXO, EYO, CELFAC
        ENDIF
        SMCNOVAL = NOVAL
#endif
        ELSE 
           READ (NDSI,*,END=801,ERR=802) IX1, IXN, IY1, IYN
        ENDIF

        CLOSE(NDSI,ERR=800,IOSTAT=IERR)
      END IF ! .NOT. FLGNML

      CALL STR_TO_UPPER(TTYPE)
      CALL STR_TO_UPPER(TIMEUNIT)

      IF(TIMEUNIT /= 'S' .AND. TIMEUNIT /= 'D') THEN
        WRITE(NDSE, 1013) TIMEUNIT
        CALL EXTCDE(14)
      ENDIF

      SELECT CASE(TTYPE)
        CASE('D')
          TVARTYPE = NF90_DOUBLE
        CASE('I')
          TVARTYPE = NF90_INT64
        CASE DEFAULT
          WRITE(NDSE, 1014) TTYPE
          CALL EXTCDE(14)
      END SELECT

      IF(TTYPE .EQ. 'I' .AND. TIMEUNIT .EQ. 'D') THEN
        WRITE(NDSE, 1015)
        CALL EXTCDE(14)
      ENDIF

      ! If TVARTPE is INT64 check that we are using netCDF4:
      IF(TVARTYPE .EQ. NF90_INT64 .AND. NCTYPE .LT. 4) THEN
        WRITE(NDSE, 1016)
        CALL EXTCDE(14)
      ENDIF

      ! Keep track of original NCVARTYPE, as it may change
      NCVARTYPEI = NCVARTYPE

      ! Get forecast reference time from TREF
      CALL T2D(TREF, REFDATE, IERR)
!

! 4.1 Time setup
      DTREQ  = MAX ( 0. , DTREQ )
      IF ( DTREQ.EQ.0. ) NOUT = 1
      NOUT   = MAX ( 1 , NOUT )
      CALL STME21 ( TOUT , IDTIME )
      WRITE (NDSO,940) IDTIME
      TDUM = 0
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

      IF(FLGFC) THEN
        CALL STME21 ( TREF , IDTIME )
        WRITE(NDSO,942) IDTIME
      ENDIF

! 4.2 Output fields
      DO IFI=1, NOGRP
        DO IFJ=1, NGRPP
          IF ( FLG2D(IFI,IFJ) ) THEN
            IF ( FLOGRD(IFI,IFJ) ) THEN
              WRITE (NDSO,946) IDOUT(IFI,IFJ), ' '
            ELSE
              WRITE (NDSO,946) IDOUT(IFI,IFJ), '*** NOT AVAILABLE ***'
              FLG2D(IFI,IFJ) = .FALSE.
            END IF
          END IF
        END DO
      END DO


! 4.3 Output type
      ALLOCATE(TABIPART(NOSWLL + 1))
      ALLOCATE(NCIDS(NOGRP,NGRPP,NOSWLL + 1))
      NBIPART=0
      DO I=1,30
        IF(STRINGIPART(I:I) .EQ. ' ') CYCLE
        READ(STRINGIPART(I:I),'(I1)') IPART
        IF(IPART .GT. NOSWLL) THEN
           WRITE(NDSO, 1500) IPART, NOSWLL
           CYCLE
        ENDIF
        NBIPART = NBIPART + 1
        IF(NBIPART .GT. NOSWLL + 1) THEN
           GOTO 803
        ENDIF
        TABIPART(NBIPART) = IPART
      ENDDO
!
      IF ( NCTYPE.LT.3 .OR. NCTYPE.GT.4 ) THEN
        WRITE (NDSE,1010) NCTYPE
        CALL EXTCDE ( 1 )
      END IF

      IF(SMCGRD) THEN
#ifdef W3_SMC
      WRITE(NDSO, 4100)
      IF(SMCOTYPE .EQ. 1) THEN  ! Flat sea point output
        ALLOCATE(SMCMASK(NSEA))
        ALLOCATE(SMCIDX(NSEA))
        SMCMASK(:) = .FALSE.
        CALL SMC_INTERP()
        SMCNOUT = COUNT(SMCMASK)
        NXO = SMCNOUT
        NYO = 1
        WRITE(NDSO, 4120) SMCNOUT
      ELSE IF(SMCOTYPE .EQ. 2) THEN  ! Regular grid output
        ! Calculate regridding weights:
        ALLOCATE(XIDX(NSEA), YIDX(NSEA), XSPAN(NSEA),                   &
                 YSPAN(NSEA), WTS(NSEA), SMCIDX(NSEA))
        CALL SMC_INTERP()
        WRITE(NDSO, 4110) NXO, NYO, SXO, SYO, DXO, DYO

        ! Allocate space for coverage array and new MAPSTA array
        ALLOCATE(COV(NXO,NYO), MAPSMC(NXO,NYO))
      ELSE IF(SMCOTYPE .EQ. 3 .OR. SMCOTYPE .EQ. 4) THEN  ! Nearest neighbour interpolation
        CALL READ_SMCINT()
      ENDIF

      ! CB: IXN and IXY are calculated by SMC_INTERP for SMC GRID
      IX1 = 1
      IXN = NXO
      IY1 = 1
      IYN = NYO

      ! Also store NXO and NYO in __local__ RTDNX and RTDNY variables.
      ! This avoids compilation errors when the RTD switch is enabled
      ! but the SMC switch is not. TODO: Remove this when C-preprocessor
      ! is used in preference to switches.
      RTDNX = NXO
      RTDNY = NYO

#ifdef W3_RTD
      ! SMC type 3/4 outputs are currently on standard pole grid only
      IF(SMCOTYPE .EQ. 3 .OR. SMCOTYPE .EQ. 4) RTDL = .FALSE.
#endif
#endif
      ELSE 
        IX1    = MAX ( IX1 , 1 )
        IXN    = MIN ( IXN , NX )
        IY1    = MAX ( IY1 , 1 )
        IYN    = MIN ( IYN , NY )
        WRITE (NDSO,3940) IX1, IXN, IY1, IYN
      ENDIF ! SMCGRD
!
! 4.4 Initialise meta-data
      CALL INIT_META(VECTOR)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  Time management.
!
      IOUT = 0
      NCIDS(:,:,:) = 0
      WRITE (NDSO,970)

! 5.1 Loops on out_grd.ww3 to read the time and data
      DO
        DTEST  = DSEC21 ( TIME , TOUT )
        IF ( DTEST .GT. 0. ) THEN
          CALL W3IOGO ( 'READ', NDSOG, IOTEST )
            IF ( IOTEST .EQ. -1 ) THEN
              WRITE (NDSO,944)
              EXIT
            END IF
          CYCLE
        END IF
        IF ( DTEST .LT. 0. ) THEN
          CALL TICK21 ( TOUT , DTREQ )
          CYCLE
        END IF


! 5.1.1 Increments the time counter IOUT
        IOUT   = IOUT + 1
        CALL STME21 ( TOUT , IDTIME )
        WRITE (NDSO,971) IDTIME


! 5.1.2  Processes the variable value for the time step IOUT
        CALL W3EXNC ( NX, NY, IX1, IXN, IY1, IYN, NSEA, FILEPREFIX,   &
                      E3DF, P2MSF, US3DF, USSPF, NCTYPE, TOGETHER, NCVARTYPEI,&
                      FLG2D, NCIDS, S3, STRSTOPDATE )

! 5.1.3 Defines the stop date
        CALL T2D(TOUT,STOPDATE,IERR)
        WRITE(STRSTOPDATE,'(I4.4,A,4(I2.2,A),I2.2)') STOPDATE(1),'-',STOPDATE(2), &
              '-',STOPDATE(3),' ',STOPDATE(5),':',STOPDATE(6),':',STOPDATE(7)

        CALL TICK21 ( TOUT , DTREQ )
        IF ( IOUT .GE. NOUT ) EXIT
      END DO

      CALL TEARDOWN_META()


! 5.2 Closes the netCDF file
      IF (TOGETHER .AND. NCIDS(1,1,1).NE.0) THEN
        IRET = NF90_REDEF(NCIDS(1,1,1))
        CALL CHECK_ERR(IRET)
        IF(FL_DEFAULT_GBL_META) THEN
          IRET=NF90_PUT_ATT(NCIDS(1,1,1),NF90_GLOBAL,'stop_date',STRSTOPDATE)
          CALL CHECK_ERR(IRET)
        ENDIF
        IRET=NF90_CLOSE(NCIDS(1,1,1))
        CALL CHECK_ERR(IRET)
      END IF
!
      DO IFI=1, NOGRP
        DO IFJ=1, NGRPP
          IF ( FLG2D(IFI,IFJ) ) THEN
            IF ( FLOGRD(IFI,IFJ) ) THEN
              IF (.NOT. TOGETHER) THEN
                IF (NCIDS(IFI,IFJ,1).NE.0) THEN
                  IRET = NF90_REDEF(NCIDS(IFI,IFJ,1))
                  CALL CHECK_ERR(IRET)
                  IF(FL_DEFAULT_GBL_META) THEN
                    IRET=NF90_PUT_ATT(NCIDS(IFI,IFJ,1),NF90_GLOBAL,'stop_date',STRSTOPDATE)
                    CALL CHECK_ERR(IRET)
                  ENDIF
                  IRET=NF90_CLOSE(NCIDS(IFI,IFJ,1))
                  CALL CHECK_ERR(IRET)
                END IF ! NCIDS
                ! close partition files (except part 0 which is already closed by (IFI,IFJ,1)
                IF ((IFI.EQ.4).AND.(IFJ.LE.NOGE(IFI))) THEN
                  DO IPART=1,NOSWLL
                    IF (NCIDS(IFI,IFJ,IPART+1).NE.0) THEN
                      IRET = NF90_REDEF(NCIDS(IFI,IFJ,IPART+1))
                      CALL CHECK_ERR(IRET)
                      IF(FL_DEFAULT_GBL_META) THEN
                        IRET=NF90_PUT_ATT(NCIDS(IFI,IFJ,IPART+1),NF90_GLOBAL,'stop_date',STRSTOPDATE)
                        CALL CHECK_ERR(IRET)
                      ENDIF
                      IRET=NF90_CLOSE(NCIDS(IFI,IFJ,IPART+1))
                      CALL CHECK_ERR(IRET)
                    END IF ! NCIDS
                  END DO ! IPART
                END IF ! partition
              ! else if together
              ELSE
                ! close frequency file
                IF ( ((IFI.EQ.6).AND.(IFJ.EQ.8)) .OR.                 &
                     ((IFI.EQ.6).AND.(IFJ.EQ.9)) .OR.                 &
                     (IFI.EQ.3) ) THEN
                  IF (NCIDS(IFI,IFJ,1).NE.0) THEN
                    IRET = NF90_REDEF(NCIDS(IFI,IFJ,1))
                    CALL CHECK_ERR(IRET)
                    IF(FL_DEFAULT_GBL_META) THEN
                      IRET=NF90_PUT_ATT(NCIDS(IFI,IFJ,1),NF90_GLOBAL,'stop_date',STRSTOPDATE)
                      CALL CHECK_ERR(IRET)
                    ENDIF
                    IRET=NF90_CLOSE(NCIDS(IFI,IFJ,1))
                    CALL CHECK_ERR(IRET)
                  END IF ! NCIDS
                END IF ! IFI
              END IF ! TOGETHER
            END IF ! FLOGRD
          END IF ! FLG2D
        END DO ! IFJ
      END DO ! IFI

!
      GOTO 888
!
! Escape locations read errors :
!
  800 CONTINUE
      WRITE (NDSE,1000) IERR
      CALL EXTCDE ( 10 )
!
  801 CONTINUE
      WRITE (NDSE,1001)
      CALL EXTCDE ( 11 )
!
  802 CONTINUE
      WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 12 )
!
  803 CONTINUE
      WRITE (NDSE,1003) NBIPART, NOSWLL
      CALL EXTCDE (13)
!
  888 CONTINUE
      WRITE (NDSO,999)
!
! Formats
!
  900 FORMAT (/15X,'   *** WAVEWATCH III Field output postp. ***   '/ &
               15X,'==============================================='/)
  901 FORMAT ( '  Comment character is ''',A,''''/)
!
  920 FORMAT ( '  Grid name : ',A/)
!
  930 FORMAT ( '  Fields in file : '/                                 &
               ' --------------------------')
  931 FORMAT ( '      ',A)
!
  940 FORMAT (/'  Output time data : '/                               &
               ' --------------------------------------------------'/ &
               '      First time         : ',A)
  941 FORMAT ( '      Interval           : ',A/                       &
               '      Number of requests : ',I10)
  942 FORMAT ( '      Reference time     : ',A)
  944 FORMAT (/'      End of file reached '/)
  946 FORMAT ( '      ',A,2X,A)
!
 3940 FORMAT ( '      X range : ',2I7/                                &
               '      Y range : ',2I7)
!
#ifdef W3_SMC
 4100 FORMAT (//'  SMC grid output :' /                               &
!
               ' --------------------------------------------------')
 4110 FORMAT ( '   SMC to regular lat/lon grid using cell averaging' /&
               '   Aligned output grid definition: ' /                &
               '      NX, NY           : ', 2I8 /                     &
               '      X0, Y0           : ', 2F8.3 /                   &
               '      DX, DY           : ', 2F8.5 )
 4120 FORMAT ( '   Flat seapoint dimensioned SMC output file' /       &
               '      Num seapoints    : ',I9 )
!
 4130 FORMAT ( '   SMC regridding to regular lat/lon grid.' /         &
               '   Output grid definition: ' /                        &
               '      NX, NY           : ', 2I8 /                     &
               '      X0, Y0           : ', 2F8.3 /                   &
               '      DX, DY           : ', 2F8.5 /                   &
               '      Interpolate ?    : ', L )
#endif
!
  970 FORMAT (/'  Generating files '/                                 &
               ' --------------------------------------------------')
  971 FORMAT ( '      Files for ',A)
!
  999 FORMAT (/'  End of program '/                                   &
               ' ========================================='/          &
               '         WAVEWATCH III Field output '/)
!
! Error format strings
!
 1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     ERROR IN OPENING INPUT FILE'/                    &
               '     IOSTAT =',I5/)
!
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     PREMATURE END OF INPUT FILE'/)
!
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     ERROR IN READING FROM INPUT FILE'/               &
               '     IOSTAT =',I5/)
!
 1003 FORMAT (/' *** WAVEWATCH III WERROR IN W3OUNF : '/              &
               '     OUT OF RANGE REQUEST FOR NBIPART =',I2, /        &
               '     MAX SWELL PARTITIONS (NOSW) =',I2 /)
!
 1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     ILLEGAL TYPE, NCTYPE =',I4/)
!
 1013 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     TIMEUNITS MUST BE ONE OF "S" OR "D"' /           &
               '     GOT: ',A /)
!
 1014 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     TIMEVAR TYPE MUST BE ONE OF "I" OR "D"' /        &
               '     GOT: ',A /)
!
 1015 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     CANNONT HAVE TIME UNITS OF DAYS WITH'/           &
               '     TIME VARYTPE OF INT64' /)
!
 1016 FORMAT (/' *** WAVEWATCH III ERROR IN W3OUNF : '/               &
               '     INT64 TIME ENCODING REQUIRES NETCDF4' /          &
               '     FILE FORMAT' /)
!
! Warning format strings
!
 1500 FORMAT (/' *** WAVEWATCH III WARNING IN W3OUNF : '/             &
               '     IGNORING REQUEST FOR IPART =',I2, /              &
               '     MAX SWELL PARTITIONS (NOSW) =',I2 /)
!
!/
!/ Internal subroutine W3EXNC ---------------------------------------- /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3EXNC ( NX, NY, IX1, IXN, IY1, IYN, NSEA,             &
                          FILEPREFIX, E3DF, P2MSF, US3DF, USSPF,NCTYPE, &
                          TOGETHER, NCVARTYPEI, FLG2D, NCIDS, S3, STRSTOPDATE )
!/
!/                  +-----------------------------------+
!/                  |           F. Ardhuin              |
!/                  |           M. Accensi              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         22-Mar-2021 |
!/                  +-----------------------------------+
!/
!/    17-Mar-2010 : Creation                            ( version 3.14_SHOM )
!/    28-Feb-2013 : New option for float output         ( version 4.08 )
!/    02-Apr-2013 : New structure of output fields.     ( version 4.09 )
!/    12-Apr-2013 : Allows curvilinear grids            ( version 4.10 )
!/    30-Apr-2014 : Correct group3 freq dim.            ( version 5.00 )
!/    23-May-2014 : Adding ice fluxes to W3SRCE         ( version 5.01 )
!/    14-Oct-2014 : Keep the output files opened        ( version 5.01 )
!/    03-Nov-2020 : NetCDF metadata moved to separate   ( version 7.12 )
!/                  module.
!/    09-Dec-2020 : Set fixed values for VARID indices  ( version 7.12 )
!/    26-Jan-2021 : Added TP output (derived from fp)   ( version 7.12 )
!/                  and alternative dir/mag output.
!/    02-Feb-2021 : Make default global meta optional   ( version 7.12 )
!/    22-Mar-2021 : New coupling fields output          ( version 7.13 )
!/
!  1. Purpose :
!
!     Perform actual grid output in NetCDF file.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NX/Y    Int.  I  Grid dimensions.
!       IX1/IXN Int.  I  Grid indexes along X
!       IY1/IYN Int.  I  Grid indexes along Y
!       NSEA    Int.  I  Number of sea points.
!     ----------------------------------------------------------------
!
!     Internal parameters
!     ----------------------------------------------------------------
!       FLTWO   Log.  Flags for two-dimensional field X Y.
!       FLDIR   Log.  Flags for two-dimensional, directional field.
!       FLFRQ   Log.  Flags for frequency array (3D field)
!       X1, X2, XX, XY
!               R.A.  Output fields
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
!      W3S2XY    Subr.   Id.    Convert from storage to spatial grid.
!      PRTBLK    Subr. W3ARRYMD Print plot of array.
!      OUTA2I    Subr.   Id.    Print array of INTEGERS.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Main program in which it is contained.
!
!  6. Error messages :
!
!       None.
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
!       !/S  Enable subroutine tracing.
!       !/T  Enable test output.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY : W3S2XY, UV_TO_MAG_DIR
#ifdef W3_RTD
      USE W3SERVMD, ONLY : W3THRTN, W3XYRTN, W3EQTOLL
#endif
      USE W3ARRYMD, ONLY : OUTA2I, PRTBLK
      USE W3GDATMD, ONLY : SIG, GTYPE, FLAGLL, MAPSTA, MAPST2
      USE W3GDATMD, ONLY : NK, UNGTYPE, MAPSF, NTRI, CLGTYPE, RLGTYPE, &
                           XGRD, YGRD, SX, SY, X0, Y0, XYB, TRIGP, USSP_WN
#ifdef W3_RTD
 ! Rotated pole data from the mod_def file
      USE W3GDATMD, ONLY : POLAT, POLON, FLAGUNR, AnglD
#endif
#ifdef W3_T
      USE W3ODATMD, ONLY : NDST
#endif
      USE NETCDF
      IMPLICIT NONE

!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NX, NY, IX1, IXN, IY1, IYN, NSEA,     &
                                 E3DF(3,5), P2MSF(3), US3DF(3),        &
                                 USSPF(2), NCTYPE, NCVARTYPEI
      CHARACTER(30)           :: FILEPREFIX
      LOGICAL, INTENT(IN)     :: TOGETHER
      LOGICAL, INTENT(IN)     :: FLG2D(NOGRP,NGRPP)
      INTEGER, INTENT(INOUT)  :: NCIDS(NOGRP,NGRPP,NOSWLL + 1), S3
      CHARACTER*30,INTENT(IN) :: STRSTOPDATE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IFI, IFJ, MFILL, I, J, ISEA, IX, IY,  &
                                 I1, J1, IPART, INDEXIPART, COORDTYPE
      INTEGER                 :: S1, S2, S4, S5, NCID, OLDNCID, NDSDAT,&
                                 NFIELD, N, IRET, IK, EXTRADIM, IVAR,  &
                                 IVAR1
      INTEGER                 :: DIMID(6), VARID(300), START(4),       &
                                 COUNT(4), DIMLN(6),START1D(2),        &
                                 COUNT1D(2), DIMFIELD(3),              &
                                 STARTDATE(8), CURDATE(8),             &
                                 EPOCHDATE(8),                         &
                                 MAP(NX+1,NY), MP2(NX+1,NY)
!
      INTEGER                  :: DEFLATE=1
#ifdef W3_S
      INTEGER, SAVE           :: IENT   =   0
#endif
!
      INTEGER, ALLOCATABLE    :: TRIGP2(:,:)
      ! Make the below allocatable to avoid stack overflow on some machines
      INTEGER(KIND=2), ALLOCATABLE    :: MX1(:,:), MXX(:,:), MYY(:,:), &
                                         MXY(:,:), MAPOUT(:,:)
!
      REAL                    :: CABS, UABS, MFILLR
#ifdef W3_BT4
   REAL, PARAMETER            :: LOG2=LOG(2.)
#endif
!
      REAL,DIMENSION(:),  ALLOCATABLE    :: LON, LAT, FREQ
      REAL,DIMENSION(:,:),  ALLOCATABLE  :: LON2D, LAT2D, ANGLD2D
#ifdef W3_RTD
      REAL,DIMENSION(:,:),  ALLOCATABLE  :: LON2DEQ, LAT2DEQ
#endif
      ! Make the below allocatable to avoid stack overflow on some machines
      REAL, ALLOCATABLE       :: X1(:,:), X2(:,:), XX(:,:), XY(:,:),   &
                                 XK(:,:,:), XXK(:,:,:), XYK(:,:,:),    &
                                 MX1R(:,:), MXXR(:,:), MYYR(:,:),      &
                                 MXYR(:,:), AUX1(:)
!
      DOUBLE PRECISION        :: OUTJULDAY
      INTEGER(KIND=8)         :: OUTSECS
      DOUBLE PRECISION        :: SXD, SYD, X0D, Y0D
!
      CHARACTER*120           :: STR2
      CHARACTER*512           :: PARTCOM
      !CHARACTER*30            :: UNITVAR(3),FORMAT1
      CHARACTER*30            :: FORMAT1
      CHARACTER*30            :: STRSTARTDATE
      CHARACTER               :: FNAMENC*128,                           &
                                 FORMF*11
      CHARACTER, SAVE         :: OLDTIMEID*16 = '0000000000000000'
      CHARACTER, SAVE         :: TIMEID*16 = '0000000000000000'
!
      LOGICAL                 :: FLFRQ, FLDIR, FEXIST, FREMOVE
      LOGICAL                 :: CUSTOMFRQ=.FALSE.
#ifdef W3_T
      LOGICAL                 :: LTEMP(NGRPP)
#endif

      TYPE(META_T)            :: META(3)
      !TYPE(META_T)            :: META
!/
!/ ------------------------------------------------------------------- /
!/
!
#ifdef W3_S
      CALL STRACE (IENT, 'W3EXNC')
#endif
!
#ifdef W3_T
      DO IFI=1, NOGRP
        LTEMP  = FLG2D(IFI,:)
        WRITE (NDST,9000) IFI, LTEMP
        END DO
      WRITE (NDST,9001) NCTYPE, IX1, IXN, IY1, IYN, VECTOR
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Preparations
!
      ! Allocate output storage. This is required with the introduction
      ! of the SMC grid output as the regridded output grid dimensions could
      ! conceivably be larger than the NX and NY values. Making these (large)
      ! arrays allocatable also moves them to the heap and avoids stack
      ! overflow issues that can occur on some architectures. (Chris Bunney)
      IF(SMCGRD) THEN
#ifdef W3_SMC
        ALLOCATE(X1(NXO,NYO), X2(NXO,NYO), XX(NXO,NYO), XY(NXO,NYO))
        ALLOCATE(XK(NXO,NYO,NK), XXK(NXO,NYO,NK), XYK(NXO,NYO,NK))

        ALLOCATE(MX1(NXO,NYO), MXX(NXO,NYO), MYY(NXO,NYO),               &
                 MXY(NXO,NYO), MAPOUT(NXO,NYO))
        ALLOCATE(MX1R(NXO,NYO), MXXR(NXO,NYO), MYYR(NXO,NYO), MXYR(NXO,NYO))
#endif
      ELSE
        ALLOCATE(X1(NX+1,NY),X2(NX+1,NY),XX(NX+1,NY),XY(NX+1,NY))
        ALLOCATE(XK(NX+1,NY,NK), XXK(NX+1,NY,NK), XYK(NX+1,NY,NK))
        ALLOCATE(MX1(NX,NY), MXX(NX,NY), MYY(NX,NY), MXY(NX,NY), MAPOUT(NX,NY))
        ALLOCATE(MX1R(NX,NY), MXXR(NX,NY), MYYR(NX,NY), MXYR(NX,NY))
      ENDIF ! SMCGRD
      ALLOCATE(AUX1(NSEA))

      X1     = UNDEF
      X2     = UNDEF
      XX     = UNDEF
      XY     = UNDEF
      ! CB: Dont output MAPSTA for SMC grid - it does not make sense
      IF( SMCGRD .AND. MAPSTAOUT) THEN
        WRITE(NDSO,*) "MAPSTA output disabled for SMC grids"
        MAPSTAOUT = .FALSE.
      ENDIF
      NCVARTYPE  = NCVARTYPEI
      NDSDAT=30
      NCID = 0
!
!
      !CHRISB: Allow alternative time units:
      CALL T2ISO(TEPOCH, EPOCH_ISO)
      SELECT CASE(TIMEUNIT)
        CASE('D')
          EPOCH = 'days since ' // EPOCH_ISO
        CASE('S')
          EPOCH = 'seconds since ' // EPOCH_ISO
        CASE DEFAULT
          PRINT*,'Unknown time units: ', TIMEUNIT
          CALL EXTCDE(10)
      END SELECT

      CALL U2D(EPOCH, EPOCHDATE, IERR)

! 1.1 Set-up transfer files
      MFILL  = NF90_FILL_SHORT
      MFILLR  = NF90_FILL_FLOAT
      IF (GTYPE.NE.UNGTYPE) THEN
        COORDTYPE=1
      ELSE
        COORDTYPE=2
      ENDIF

! 1.2 Sets the date as ISO8601 convention
      ! S3 defines the number of characters in the date for the filename
      ! S3=0 -> field, S3=4-> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDHH
      ! Setups min and max date format
      IF (S3.GT.0 .AND. S3.LT.4) S3=4
      IF (S3.GT.10) S3=10
!
      ! Defines the format of FILETIME
      S5=S3-8
      S4=S3
      OLDTIMEID=TIMEID
      ! if S3=>nodate then filetime='field'
      IF (S3.EQ.0) THEN
        S4=5
        TIMEID="field"
      ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHZ'
      ELSE IF (S3.EQ.10) THEN
        S4=S4+2 ! add chars for ISO8601 : day T hours Z
        WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I8.8,A1,I',S5,'.',S5,',A1)'
        WRITE (TIMEID,FORMAT1) TIME(1), 'T', &
               FLOOR(REAL(TIME(2))/NINT(10.**(6-S5))), 'Z'
      ! if S3=>YYYYMMDD then filetime='YYYYMMDD'
      ELSE IF (S3.EQ.8) THEN
        WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
        WRITE (TIMEID,FORMAT1) TIME(1)
      ! if S3=>YYYYMM then filetime='YYYYMM'
      ! or S3=>YYYY then filetime='YYYY'
      ELSE
        WRITE(FORMAT1,'(A,I1,A,I1,A)') '(I',S3,'.',S3,')'
        WRITE (TIMEID,FORMAT1) FLOOR(REAL(TIME(1))/NINT(10.**(8-S3)))
      END IF
      ! redefines filename with updated date format
      S1=LEN_TRIM(FILEPREFIX)
      FNAMENC=''
      FNAMENC(1:S1)=FILEPREFIX(1:S1)
      FNAMENC(S1+1:S1+S4) = TIMEID(1:S4)

      !
#ifdef W3_SMC
!
!---  Update MAPSMC for SMC type 2 output. This needs to be
!     done at each timestep as MAPSTA could change if there
!     are water level or ice input chagnes.
!
      IF( SMCGRD .AND. (SMCOTYPE .EQ. 2) ) CALL MAPSTA_SMC()
#endif
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Loop over output fields.
!

      ! Instanciates the field and group indexes
      I1=0
      J1=0
!
      DO IFI=1, NOGRP
        DO IFJ=1, NGRPP
          ! If the flag for the variable IFI of the group IFJ is .TRUE.
          IF ( FLG2D(IFI,IFJ) ) THEN
            ! Instanciates the partition array
            INDEXIPART=1
            IPART=TABIPART(INDEXIPART)
            NFIELD=1 ! Default is one field


!  Loop over IPART for partition variables
555         CONTINUE

            ! Initializes the index of field and group at the first flag FLG2D at .TRUE.
            IF (I1.EQ.0) I1=IFI
            IF (J1.EQ.0) J1=IFJ
            FORMF  = '(1X,32I5)'
#ifdef W3_T
            WRITE (NDST,9020) IDOUT(IFI,IFJ)
#endif
!
! 2.1 Set output arrays and parameters
!
            ! Initializes the flags for freq and direction dimensions
            FLFRQ = .FALSE.
            FLDIR = .FALSE.
            IF (NCVARTYPEI.EQ.3) NCVARTYPE=2
!
            ! Depth
            IF ( IFI .EQ. 1 .AND. IFJ .EQ. 1 ) THEN
              CALL S2GRID(DW(1:NSEA), X1)

            ! Surface current
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 2 ) THEN
              !! Note - CX and CY read in from .ww3 file are X-Y vectors
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, CX(1:NSEA), CY(1:NSEA), AnglD)
#endif
!
              IF( .NOT. VECTOR ) THEN
                CALL UV_TO_MAG_DIR(CX(1:NSEA), CY(1:NSEA), NSEA,       &
                                   TOLERANCE=0.05, CONV='O')
              ENDIF
!
              CALL S2GRID(CX(1:NSEA), XX)
              CALL S2GRID(CY(1:NSEA), XY)
              NFIELD=2
!
            ! Wind
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 3 ) THEN
              !! Note - UA and UD read in from .ww3 file are UX,UY
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, UA(1:NSEA), UD(1:NSEA), AnglD)
#endif
!
              IF( .NOT. VECTOR ) THEN
                CALL UV_TO_MAG_DIR(UA(1:NSEA), UD(1:NSEA), NSEA,       &
                                 TOLERANCE=1.0, CONV='N')
              ENDIF
!
              CALL S2GRID(UA(1:NSEA), XX)
              CALL S2GRID(UD(1:NSEA), XY)
              NFIELD=2
!
            ! Air-sea temperature difference
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 4 ) THEN
              CALL S2GRID(AS(1:NSEA), X1)
!
            ! Sea surface height above sea level
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 5 ) THEN
              CALL S2GRID(WLV, X1)
!
            ! Sea ice area fraction
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 6 ) THEN
              CALL S2GRID(ICE(1:NSEA), X1)

            ! Icebergs_damping
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 7 ) THEN
              CALL S2GRID(BERG, X1)
              WHERE ( X1.NE.UNDEF) X1 = X1*0.1
!
            ! Atmospheric momentum
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 8 ) THEN
              !! Note - TAUA and TAUADIR read in from .ww3 file are TAUAX,TAUAY
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUA(1:NSEA), TAUADIR(1:NSEA), AnglD)
#endif

              IF( SMCGRD ) THEN
#ifdef W3_SMC
                 CALL W3S2XY_SMC( TAUA   (1:NSEA), XX )
                 CALL W3S2XY_SMC( TAUADIR(1:NSEA), XY )
#endif
              ELSE ! IF(SMCGRD)
                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUA(1:NSEA)      &
                                                        , MAPSF, XX )
                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUADIR(1:NSEA)   &
                                                        , MAPSF, XY )
              ENDIF
              NFIELD=2
!
            ! Air density
            ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 9 ) THEN
              IF( SMCGRD ) THEN
#ifdef W3_SMC
                 CALL W3S2XY_SMC(RHOAIR, X1)
#endif
              ELSE
                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, RHOAIR, MAPSF, X1 )
              ENDIF
!
#ifdef W3_BT4
 ! Krumbein phi scale
 ELSE IF ( IFI .EQ. 1 .AND. IFJ .EQ. 10 ) THEN
              CALL S2GRID(SED_D50, X1)
              WHERE ( X1.NE.UNDEF) X1 = -LOG(X1/0.001)/LOG2
              NFIELD=1
#endif
!
#ifdef W3_IS2
 ! Ice thickness
 ELSE IF (IFI .EQ. 1 .AND. IFJ .EQ. 11 ) THEN
              CALL S2GRID(ICEH(1:NSEA), X1)
              NFIELD=1
#endif
!
#ifdef W3_IS2
 ! Maximum ice floe diameter
 ELSE IF (IFI .EQ. 1 .AND. IFJ .EQ. 12 ) THEN
              CALL S2GRID(ICEF(1:NSEA), X1)
              NFIELD=1
#endif

            ! Significant wave height
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 1 ) THEN
              IF (NCVARTYPEI.EQ.3) NCVARTYPE=2
              CALL S2GRID(HS, X1)

            ! Mean wave length
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 2 ) THEN
              CALL S2GRID(WLM, X1)
!
            ! Mean period T02
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 3 ) THEN
              CALL S2GRID(T02, X1)
!
            ! Mean period T0m1
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 4 ) THEN
              CALL S2GRID(T0M1, X1)
!
            ! Mean period T01
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 5 ) THEN
              CALL S2GRID(T01, X1)
!
            ! Wave peak frequency
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 6 ) THEN
              CALL S2GRID(FP0, X1)
!
            ! Wave mean direction
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 7 ) THEN
#ifdef W3_RTD
              ! Rotate direction back to standard pole
              IF ( FLAGUNR ) CALL W3THRTN(NSEA, THM, AnglD, .FALSE.)
#endif

              CALL S2GRID(THM, X1, .TRUE.)
!              IF( SMCGRD ) THEN
!!/SMC                CALL W3S2XY_SMC( THM, X1, .TRUE. )
!              ELSE
!                 DO ISEA=1, NSEA
!                   IF ( THM(ISEA) .NE. UNDEF )  THEN
!                     THM(ISEA) = MOD ( 630. - RADE*THM(ISEA) , 360. )
!                     END IF
!                   END DO
!                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THM, MAPSF, X1 )
!              ENDIF
!
            ! Directional spread
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 8 ) THEN
              CALL S2GRID(THS, X1)
!
            ! Peak direction
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 9 ) THEN
#ifdef W3_RTD
              ! Rotate direction back to standard pole
              IF ( FLAGUNR ) CALL W3THRTN(NSEA, THP0, AnglD, .FALSE.)
#endif
              CALL S2GRID(THP0, X1, .TRUE.)
!              IF( SMCGRD ) THEN
!!/SMC                CALL W3S2XY_SMC( THP0, X1, .TRUE. )
!              ELSE
!                 DO ISEA=1, NSEA
!                   IF ( THP0(ISEA) .NE. UNDEF ) THEN
!                     THP0(ISEA) = MOD ( 630-RADE*THP0(ISEA) , 360. )
!                     END IF
!                   END DO
!                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, THP0  , MAPSF, X1 )
!              ENDIF
!
            ! Infragravity wave height
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 10 ) THEN
              CALL S2GRID(HSIG, X1)
!
            ! Expected maximum sea surface elevation
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 11 ) THEN
              CALL S2GRID(STMAXE, X1)
!
            ! Standard deviation of maximum sea surface elevation
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 12 ) THEN
              CALL S2GRID(STMAXD, X1)
!
            ! Expected maximum wave height
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 13 ) THEN
              CALL S2GRID(HMAXE, X1)
!
            ! Expected maximum wave height from crest
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 14 ) THEN
              CALL S2GRID(HCMAXE, X1)
!
            ! STD of maximum wave height
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 15 ) THEN
              CALL S2GRID(HMAXD, X1)
!
            ! STD of maximum wave height from crest
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 16 ) THEN
              CALL S2GRID(HCMAXD, X1)
!
            ! Dominant wave breaking probability
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 17 ) THEN
              CALL S2GRID(WBT, X1)
!
            ! Wave peak period (derived from peak freq field)
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 18 ) THEN
              DO I=1,NSEA
                IF(FP0(I) .NE. UNDEF) THEN
                  AUX1(I) = 1.0 / FP0(I)
                ELSE
                  AUX1(I) = UNDEF
                ENDIF
              ENDDO
!
              CALL S2GRID(AUX1, X1)
!
            ! Mean wave number
            ELSE IF ( IFI .EQ. 2 .AND. IFJ .EQ. 19 ) THEN
              IF( SMCGRD ) THEN
#ifdef W3_SMC
                CALL W3S2XY_SMC( WNMEAN, X1 )
#endif
              ELSE
                 CALL W3S2XY ( NSEA, NSEA, NX+1, NY, WNMEAN, MAPSF, X1 )
              END IF
!
            ! Wave elevation spectrum
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 1 ) THEN
              ! Information for spectral
              FLFRQ  = .TRUE.
              I1F=E3DF(2,1)
              I2F=E3DF(3,1)
              DO IK=I1F,I2F
                CALL S2GRID(EF(:,IK), XX)
                IF (NCVARTYPE.EQ.2) WHERE ( XX.GE.0.) XX = ALOG10(XX+1E-12)
                XK(:,:,IK)=XX
              END DO
!
            ! Mean wave direction frequency spectrum
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 2 ) THEN
              ! Information for spectral
              FLFRQ  = .TRUE.
              I1F=E3DF(2,2)
              I2F=E3DF(3,2)
              DO IK=I1F,I2F
#ifdef W3_RTD
                ! Rotate direction back to standard pole
                IF ( FLAGUNR ) CALL W3THRTN(NSEA, TH1M(:,IK), AnglD, .FALSE.)
#endif
                CALL S2GRID(TH1M(:,IK), XX)
                XK(:,:,IK)=XX
              END DO
!
            ! Spreading frequency spectrum
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 3 ) THEN
              ! Information for spectral
              FLFRQ  = .TRUE.
              I1F=E3DF(2,3)
              I2F=E3DF(3,3)
              DO IK=I1F,I2F
                CALL S2GRID(STH1M(:,IK), XX)
                XK(:,:,IK)=XX
              END DO
!
            ! Second mean wave direction frequency spectrum
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 4 ) THEN
              ! Information for spectral
              FLFRQ  = .TRUE.
              I1F=E3DF(2,4)
              I2F=E3DF(3,4)
              DO IK=I1F,I2F
#ifdef W3_RTD
                ! Rotate direction back to standard pole
                IF ( FLAGUNR ) CALL W3THRTN(NSEA, TH2M(:,IK), AnglD, .FALSE.)
#endif
                CALL S2GRID(TH2M(:,IK), XX)
                XK(:,:,IK)=XX
              END DO
!
            ! Second spreading frequency spectrum
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 5 ) THEN
              ! Information for spectral
              FLFRQ  = .TRUE.
              I1F=E3DF(2,5)
              I2F=E3DF(3,5)
              DO IK=I1F,I2F
                CALL S2GRID(STH2M(:,IK), XX)
                XK(:,:,IK)=XX
              END DO
!
            ! Wave numbers
            ELSE IF ( IFI .EQ. 3 .AND. IFJ .EQ. 6 ) THEN
              ! Information for spectral
              FLFRQ  = .TRUE.
              I1F=1
              I2F=NK
              DO IK=1,NK
                CALL S2GRID(WN(IK,:), XX)
                XK(:,:,IK)=XX
              END DO
!
            ! Partition wave significant height
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 1 ) THEN
              CALL S2GRID(PHS(:,IPART), X1)
!
            ! Partition peak period
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 2 ) THEN
              CALL S2GRID(PTP(:,IPART), X1)

            ! Partition peak wave length
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 3 ) THEN
              CALL S2GRID(PLP(:,IPART), X1)
!
            ! Partition wave mean direction
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 4 ) THEN
#ifdef W3_RTD
                ! Rotate direction back to standard pole
                IF ( FLAGUNR ) CALL W3THRTN(NSEA, PDIR(:,IPART), AnglD, .FALSE.)
#endif
              CALL S2GRID(PDIR(:,IPART), X1, .TRUE.)
!              IF( SMCGRD ) THEN
!!/SMC                CALL W3S2XY_SMC( PDIR(:,IPART), X1, .TRUE. )
!              ELSE
!                DO ISEA=1, NSEA
!                  IF ( PDIR(ISEA,IPART) .NE. UNDEF ) THEN
!                     PDIR(ISEA,IPART) = MOD ( 630-RADE*PDIR(ISEA,IPART) , 360. )
!                  END IF
!                END DO
!                CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PDIR(:,IPART), MAPSF, X1 )
!              ENDIF
!
            ! Partition directional spread
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 5 ) THEN
              CALL S2GRID(PSI(:,IPART), X1)
!
            ! Partition wind sea fraction
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 6 ) THEN
              CALL S2GRID(PWS(:,IPART), X1)
!
            ! Partition peak direction
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 7 ) THEN
#ifdef W3_RTD
                ! Rotate direction back to standard pole
                IF ( FLAGUNR ) CALL W3THRTN(NSEA, PTHP0(:,IPART), AnglD, .FALSE.)
#endif
              CALL S2GRID(PTHP0(:,IPART), X1, .TRUE.)
!              IF( SMCGRD ) THEN
!!/SMC                CALL W3S2XY_SMC( PTHP0(:,IPART), X1, .TRUE. )
!              ELSE
!                DO ISEA=1, NSEA
!                  IF ( PTHP0(ISEA,IPART) .NE. UNDEF ) THEN
!                    PTHP0(ISEA,IPART) = MOD ( 630-RADE*PTHP0(ISEA,IPART) , 360. )
!                    END IF
!                  END DO
!                CALL W3S2XY ( NSEA, NSEA, NX+1, NY, PTHP0(:,IPART), MAPSF, X1 )
!              END IF
!
            ! Partition peakedness
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 8 ) THEN
              CALL S2GRID(PQP(:,IPART), X1)
!
            ! Partition peak enhancement factor
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 9 ) THEN
              CALL S2GRID(PPE(:,IPART), X1)
!
            ! Partition frequency width
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 10 ) THEN
              CALL S2GRID(PGW(:,IPART), X1)
!
            ! Partition spectral width
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 11 ) THEN
              CALL S2GRID(PSW(:,IPART), X1)
!
            ! Partition mean period Tm10
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 12 ) THEN
              CALL S2GRID(PTM1(:,IPART), X1)
!
            ! Partition mean period T01
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 13 ) THEN
              CALL S2GRID(PT1(:,IPART), X1)
!
            ! Partition mean period T02
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 14 ) THEN
              CALL S2GRID(PT2(:,IPART), X1)
!
            ! Partition energy at peak frequency
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 15 ) THEN
              CALL S2GRID(PEP(:,IPART), X1)
              NFIELD=1
!
            ! Partition wind sea fraction
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 16 ) THEN
              CALL S2GRID(PWST(:), X1)
!
            ! Number of wave partitions
            ELSE IF ( IFI .EQ. 4 .AND. IFJ .EQ. 17 ) THEN
              CALL S2GRID(PNR(:), X1)
!
            ! Friction velocity
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 1 ) THEN
              !! Note - UST and USTDIR read in from .ww3 file are X-Y vectors
              DO ISEA=1, NSEA
                UABS = SQRT(UST(ISEA)**2+USTDIR(ISEA)**2)
                IF (UABS.GE.10.) THEN
                  UST(ISEA)=UNDEF
                  USTDIR(ISEA)=UNDEF
                END IF
              END DO
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, UST(1:NSEA), USTDIR(1:NSEA), AnglD)
#endif
              CALL S2GRID(UST(1:NSEA), XX)
              CALL S2GRID(USTDIR(1:NSEA), XY)
              !! Commented out unnecessary statements below for time being
              !! UST,USTDIR are in north-east convention and X1,X2
              !! are not actually written out below
              !DO ISEA=1, NSEA
              !  UABS   = SQRT(UST(ISEA)**2+USTDIR(ISEA)**2)
              !  IF ( UST(ISEA) .EQ. UNDEF ) THEN
              !      USTDIR(ISEA) = UNDEF
              !      UABS         = UNDEF
              !    ELSE IF ( UABS .GT. 0.05 ) THEN
              !      USTDIR(ISEA) = MOD ( 630. -                     &
              !        RADE*ATAN2(USTDIR(ISEA),UST(ISEA)) , 360. )
              !    ELSE
              !      USTDIR(ISEA) = UNDEF
              !    END IF
              !  UST(ISEA) = UABS
              !  END DO
              !CALL W3S2XY (NSEA,NSEA,NX+1,NY, UST   (1:NSEA) , MAPSF, X1 )
              !CALL W3S2XY (NSEA,NSEA,NX+1,NY, USTDIR(1:NSEA) , MAPSF, X2 )
              NFIELD=2
!
            ! Charnock coefficient
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 2 ) THEN
              CALL S2GRID(CHARN(1:NSEA), X1)
!
            ! Wave energy flux
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 3 ) THEN
              CGE=CGE*0.001  ! from W / m to kW / m
              CALL S2GRID(CGE(1:NSEA), X1)
!
            ! Wind to wave energy flux
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 4 ) THEN
              IF (NCVARTYPEI.EQ.3) NCVARTYPE=4
              CALL S2GRID(PHIAW(1:NSEA), X1)
!
            ! Wave supported wind stress
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 5 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUWIX(1:NSEA), TAUWIY(1:NSEA), AnglD)
#endif
              CALL S2GRID(TAUWIX(1:NSEA), XX)
              CALL S2GRID(TAUWIY(1:NSEA), XY)

              !! Commented out unnecessary statements below for time being
              !! TAUWIX, TAUWIY are in north-east convention and X1,X2
              !! are not actually written out below
              !DO ISEA=1, NSEA
              !  CABS   = SQRT(TAUWIX(ISEA)**2+TAUWIY(ISEA)**2)
              !  IF ( CABS .NE. UNDEF ) THEN
              !      TAUWIY(ISEA) = MOD ( 630. -                         &
              !            RADE*ATAN2(TAUWIY(ISEA),TAUWIX(ISEA)) , 360. )
              !    ELSE
              !      TAUWIY(ISEA) = UNDEF
              !    END IF
              !  TAUWIX(ISEA) = CABS
              !  END DO
              !CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIX, MAPSF, X1 )
              !CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUWIY, MAPSF, X2 )
              NFIELD=2
!
            ! Wave to wind stress
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 6 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUWNX(1:NSEA), TAUWNY(1:NSEA), AnglD)
#endif
              CALL S2GRID(TAUWNX(1:NSEA), XX)
              CALL S2GRID(TAUWNY(1:NSEA), XY)
              NFIELD=2
!
            ! Whitecap coverage
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 7 ) THEN
              CALL S2GRID(WHITECAP(1:NSEA,1), X1)
!
            ! Whitecap foam thickness
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 8 ) THEN
              CALL S2GRID(WHITECAP(1:NSEA,2), X1)
!
            ! Significant breaking wave height
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 9 ) THEN
              CALL S2GRID(WHITECAP(1:NSEA,3), X1)
!
            ! Whitecap moment
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 10 ) THEN
              CALL S2GRID(WHITECAP(1:NSEA,4), X1)
!
            ! Wind sea mean period T0M1
            ELSE IF ( IFI .EQ. 5 .AND. IFJ .EQ. 11 ) THEN
              CALL S2GRID(TWS(1:NSEA), X1)
!
            ! Radiation stress
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 1 ) THEN
#ifdef W3_RTD
         ! Radition stress components are always left on rotated pole
         ! at present - need to confirm how to de-rotate
#endif

              CALL S2GRID(SXX(1:NSEA), X1)
              CALL S2GRID(SYY(1:NSEA), X2)
              CALL S2GRID(SXY(1:NSEA), XY)
              NFIELD=3
!
            ! Wave to ocean stress
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 2 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUOX(1:NSEA), TAUOY(1:NSEA), AnglD)
#endif
              CALL S2GRID(TAUOX(1:NSEA), XX)
              CALL S2GRID(TAUOY(1:NSEA), XY)
              NFIELD=2
!
            ! Radiation pressure (Bernouilli Head)
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 3 ) THEN
              CALL S2GRID(BHD(1:NSEA), X1)
!
            ! Wave to ocean energy flux
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 4 ) THEN
              IF (NCVARTYPEI.EQ.3) NCVARTYPE=4
              DO ISEA=1, NSEA
                PHIOC(ISEA)=MIN(3000.,PHIOC(ISEA))
              END DO
              CALL S2GRID(PHIOC(1:NSEA), X1)
!
            ! Stokes transport
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 5 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TUSX(1:NSEA), TUSY(1:NSEA), AnglD)
#endif
              CALL S2GRID(TUSX(1:NSEA), XX)
              CALL S2GRID(TUSY(1:NSEA), XY)
! X1, X2 will not be output when NFIELD == 2
! ( Like for .cur, .wnd, .ust, .taw, and .uss ) (CHA at FCOO 2019-06-13):
              !! Commented out unnecessary statements below for time being
              !! (...) X1,X2 are not actually written out below
              !DO ISEA=1, NSEA
              !  CABS   = SQRT(TUSX(ISEA)**2+TUSY(ISEA)**2)
              !  IF ( CABS .NE. UNDEF ) THEN
              !      TUSY(ISEA) = MOD ( 630. -                         &
              !            RADE*ATAN2(TUSY(ISEA),TUSX(ISEA)) , 360. )
              !    ELSE
              !      TUSY(ISEA) = UNDEF
              !    END IF
              !  TUSX(ISEA) = CABS
              !  END DO
              !IF( SMCGRD ) THEN
#ifdef W3_SMC
                !CALL W3S2XY_SMC( TUSX(:), X1 )
                !CALL W3S2XY_SMC( TUSY(:), X2 ) ! TODO: CHRISB: TUSY is in degrees....W3S2XY_SMC expects radians...
#endif
              !ELSE
              !  CALL W3S2XY ( NSEA, NSEA, NX+1, NY,TUSX,MAPSF, X1 )
              !  CALL W3S2XY ( NSEA, NSEA, NX+1, NY,TUSY,MAPSF, X2 )
              !ENDIF ! SMCGRD
              NFIELD=2
!
            ! Surface stokes drift
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 6 ) THEN
              DO ISEA=1, NSEA
                USSX(ISEA)=MAX(-0.9998,MIN(0.9998,USSX(ISEA)))
                USSY(ISEA)=MAX(-0.9998,MIN(0.9998,USSY(ISEA)))
              END DO
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, USSX(1:NSEA), USSY(1:NSEA), AnglD)
#endif
              CALL S2GRID(USSX(1:NSEA), XX)
              CALL S2GRID(USSY(1:NSEA), XY)
              !! Commented out unnecessary statements below for time being
              !! TAUWIX, TAUWIY are in north-east convention and X1,X2
              !! are not actually written out below
              !DO ISEA=1, NSEA
              !  CABS   = SQRT(USSX(ISEA)**2+USSY(ISEA)**2)
              !  IF ( CABS .NE. UNDEF ) THEN
              !      USSY(ISEA) = MOD ( 630. -                         &
              !            RADE*ATAN2(USSY(ISEA),USSX(ISEA)) , 360. )
              !    ELSE
              !      USSY(ISEA) = UNDEF
              !    END IF
              !  USSX(ISEA) = CABS
              !  END DO
              !CALL W3S2XY ( NSEA, NSEA, NX+1, NY,USSX,MAPSF, X1 )
              !CALL W3S2XY ( NSEA, NSEA, NX+1, NY,USSY,MAPSF, X2 )
              NFIELD=2
!
            ! Power spectral density of equivalent surface pressure
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 7 ) THEN
              NFIELD=2
              CALL S2GRID(PRMS(1:NSEA), XX)
              CALL S2GRID(TPMS(1:NSEA), XY)
!
            ! Spectral variance of surface stokes drift
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 8 ) THEN
              ! Information for spectral distribution of surface Stokes drift (2nd file)
              FLFRQ=.TRUE.
              NFIELD=2
              I1F=US3DF(2)
              I2F=US3DF(3)
              DO IK= I1F,I2F
#ifdef W3_RTD
                ! Rotate x,y vector back to standard pole
                IF ( FLAGUNR ) CALL W3XYRTN(NSEA, US3D(:,IK), US3D(:,NK+IK), AnglD)
#endif
                CALL S2GRID(US3D(:,IK), XX)
                CALL S2GRID(US3D(:,NK+IK), XY)
                XXK(:,:,IK)=XX
                XYK(:,:,IK)=XY
              END DO
!
            ! Base10 logarithm of power spectral density of equivalent surface pressure
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ.  9 ) THEN
                ! Information for spectral microseismic generation data (2nd file)
                FLFRQ=.TRUE.
                I1F=P2MSF(2)
                I2F=P2MSF(3)
                DO IK=I1F,I2F
                  CALL S2GRID(P2SMS(:,IK), XX)

                  IF (NCVARTYPE.EQ.2) THEN
                     WHERE ( XX.GE.0.) XX = ALOG10(XX*(DWAT*GRAV)**2+1E-12)
                  ELSE
                     WHERE ( XX.GE.0.) XX = XX*(DWAT*GRAV)**2
                  END IF

                  XK(:,:,IK)=XX
                END DO
!
            ! Wave to sea ice stress
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 10 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUICE(1:NSEA,1), TAUICE(1:NSEA,2), AnglD)
#endif
              CALL S2GRID(TAUICE(1:NSEA,1), XX)
              CALL S2GRID(TAUICE(1:NSEA,2), XY)
              NFIELD=2
!
            ! Wave to sea ice energy flux
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 11 ) THEN
              IF (NCVARTYPEI.EQ.3) NCVARTYPE=4
              DO ISEA=1, NSEA
                PHIOC(ISEA)=MIN(3000.,PHIOC(ISEA))
              END DO
              CALL S2GRID(PHICE(1:NSEA), X1)
!
           ! Partitioned surface stokes drift
           ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 12 ) THEN
              ! Information for spectral distribution of surface Stokes drift (2nd file)
              FLFRQ=.TRUE.
              IF (USSPF(1)==1) THEN
                CUSTOMFRQ=.TRUE.
              ENDIF
              NFIELD=2
              I1F=1
              I2F=USSPF(2)
              DO IK= I1F,I2F
#ifdef W3_RTD
                ! Rotate x,y vector back to standard pole
                IF ( FLAGUNR ) CALL W3XYRTN(NSEA, USSP(:,IK), USSP(:,NK+IK), AnglD)
#endif
                CALL S2GRID(USSP(:,IK), XX)
                CALL S2GRID(USSP(:,NK+IK), XY)
                XXK(:,:,IK) = XX
                XYK(:,:,IK) = XY
              END DO
!
            ! Total momentum to the ocean
            ELSE IF ( IFI .EQ. 6 .AND. IFJ .EQ. 13 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUOCX(1:NSEA), TAUOCY(1:NSEA), AnglD)
#endif
              IF( SMCGRD ) THEN
#ifdef W3_SMC
                CALL W3S2XY_SMC( TAUOCX(1:NSEA), XX )
                CALL W3S2XY_SMC( TAUOCY(1:NSEA), XY )
#endif
              ELSE
                CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUOCX(1:NSEA)     &
                                                        , MAPSF, XX )
                CALL W3S2XY ( NSEA, NSEA, NX+1, NY, TAUOCY(1:NSEA)     &
                                                        , MAPSF, XY )
              ENDIF ! SMCGRD
              NFIELD=2
!
            ! RMS of bottom displacement amplitude
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 1 ) THEN
              ! NB: ABA and ABD are the X and Y components of the bottom displacement
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, ABA(1:NSEA), ABD(1:NSEA), AnglD)
#endif
              CALL S2GRID(ABA(1:NSEA), XX)
              CALL S2GRID(ABD(1:NSEA), XY)
              NFIELD=2
!
            ! RMS of bottom velocity amplitude
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 2 ) THEN
              ! NB: UBA and UBD are the X and Y components of the bottom velocity
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, UBA(1:NSEA), UBD(1:NSEA), AnglD)
#endif
              CALL S2GRID(UBA(1:NSEA), XX)
              CALL S2GRID(UBD(1:NSEA), XY)
              NFIELD=2
!
            ! Bottom roughness
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 3 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, BEDFORMS(1:NSEA,2), &
                                           BEDFORMS(1:NSEA,3), AnglD)
#endif
              CALL S2GRID(BEDFORMS(1:NSEA,1), X1)
              CALL S2GRID(BEDFORMS(1:NSEA,2), X2)
              CALL S2GRID(BEDFORMS(1:NSEA,3), XY)
              NFIELD=3
!
            ! Wave dissipation in bottom boundary layer
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 4 ) THEN
              CALL S2GRID(PHIBBL(1:NSEA), X1)
!
            ! Wave to bottom boundary layer stress
            ELSE IF ( IFI .EQ. 7 .AND. IFJ .EQ. 5 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, TAUBBL(1:NSEA,1), &
                                           TAUBBL(1:NSEA,2), AnglD)
#endif
              CALL S2GRID(TAUBBL(1:NSEA,1), XX)
              CALL S2GRID(TAUBBL(1:NSEA,2), XY)
              NFIELD=2
!
            ! Mean square slope
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 1 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, MSSX, MSSY, AnglD)
#endif
              CALL S2GRID(MSSX, XX)
              CALL S2GRID(MSSY, XY)
              NFIELD=2
!
            ! Phillips constant
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 2 ) THEN
#ifdef W3_RTD
              ! Rotate x,y vector back to standard pole
              IF ( FLAGUNR ) CALL W3XYRTN(NSEA, MSCX, MSCY, AnglD)
#endif
              CALL S2GRID(MSCX, XX)
              CALL S2GRID(MSCY, XY)
              NFIELD=2
!
            ! u direction for mss
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 3 ) THEN
#ifdef W3_RTD
                ! Rotate direction back to standard pole
                IF ( FLAGUNR ) CALL W3THRTN(NSEA, MSSD, AnglD, .FALSE.)
#endif
              DO ISEA=1, NSEA
                IF ( MSSD(ISEA) .NE. UNDEF )  THEN
                  MSSD(ISEA) = MOD ( 630. - RADE*MSSD(ISEA) , 180. )
                END IF
              END DO
              CALL S2GRID(MSSD, X1)
!
            ! x direction for msc
            ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 4 ) THEN
#ifdef W3_RTD
                ! Rotate direction back to standard pole
                IF ( FLAGUNR ) CALL W3THRTN(NSEA, MSCD, AnglD, .FALSE.)
#endif
              DO ISEA=1, NSEA
                IF ( MSCD(ISEA) .NE. UNDEF )  THEN
                  MSCD(ISEA) = MOD ( 630. - RADE*MSCD(ISEA) , 180. )
                END IF
              END DO
              CALL S2GRID(MSCD, X1)
!
             ! Peakedness
             ELSE IF ( IFI .EQ. 8 .AND. IFJ .EQ. 5 ) THEN
              CALL S2GRID(QP, X1)
!
            ! Dynamic time step
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 1 ) THEN
              DO ISEA=1, NSEA
                IF ( DTDYN(ISEA) .NE. UNDEF ) THEN
                  DTDYN(ISEA) = DTDYN(ISEA) / 60.
                END IF
              END DO
              CALL S2GRID(DTDYN, X1)
!
            ! Cut off frequency
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 2 ) THEN
              CALL S2GRID(FCUT, X1)
!
            ! Maximum CFL for spatial advection
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 3 ) THEN
              CALL S2GRID(CFLXYMAX, X1)
!
            ! Maximum CFL for direction advection
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 4 ) THEN
              CALL S2GRID(CFLTHMAX, X1)
!
            ! Maximum CFL for frequency advection
            ELSE IF ( IFI .EQ. 9 .AND. IFJ .EQ. 5 ) THEN
              CALL S2GRID(CFLKMAX, X1)
!
            ! User defined...
            ELSE IF ( IFI .EQ. 10 ) THEN
              !CB WRITE (ENAME,'(A2,I2.2)') '.u', IFJ
              CALL S2GRID(USERO(:,IFJ), X1)
            ELSE
              WRITE (NDSE,999) IFI, IFJ
              CALL EXTCDE ( 1 )
!
            END IF ! IFI AND IFJ

            ! CB Get netCDF metadata for IFI, IFJ combination (all components).
            DO I=1,NFIELD
              META(I) = GETMETA(IFI, IFJ, ICOMP=I, IPART=IPART)
            ENDDO

! 2.2 Make map

            ! CB: TODO - need to handle MAPSTA differently for SMC grid output.
            IF( .NOT. SMCGRD ) THEN
            DO IX=1, NX
              DO IY=1, NY
                MAPOUT(IX,IY)=INT2(MAPSTA(IY,IX) + 8*MAPST2(IY,IX))
                IF ( MAPSTA(IY,IX) .EQ. 0 ) THEN
                  X1(IX,IY) = UNDEF
                  X2(IX,IY) = UNDEF
                  XX(IX,IY) = UNDEF
                  XY(IX,IY) = UNDEF
                END IF
                IF ( X1(IX,IY) .EQ. UNDEF ) THEN
                  MAP(IX,IY) = 0
                ELSE
                  MAP(IX,IY) = 1
                END IF
                IF ( X2(IX,IY) .EQ. UNDEF ) THEN
                  MP2(IX,IY) = 0
                ELSE
                  MP2(IX,IY) = 1
                END IF
              END DO
            END DO
            ENDIF ! CB


! 2.3 Setups the output type 4 ( NetCDF file )

            S2=LEN_TRIM(META(1)%ENAME)
            S1=LEN_TRIM(FILEPREFIX)+S4
            FNAMENC(S1+1:128)='       '
            FNAMENC(S1+1:S1+1) = '_'

            ! If flag TOGETHER and not variable with freq dim &
            ! (ef, p2l, ...), no variable name in file name
            IF (TOGETHER.AND.(.NOT.FLFRQ)) THEN
              S2=0
            ! If NOT flag TOGETHER or variable with freq dim &
            ! (ef, p2l, ...), add variable name in file name
            ELSE
              FNAMENC(S1+2:S1+S2) = META(1)%ENAME(2:S2)
            ENDIF
            ! Defines the netcdf extension
            FNAMENC(S1+S2+1:S1+S2+3) = '.nc'
            FNAMENC(S1+S2+4:S1+S2+6) = '   '
            ! If the flag frequency is .TRUE., defines the fourth dimension
            IF (FLFRQ) THEN
              DIMLN(4)=I2F-I1F+1
              EXTRADIM=1
            ELSE
              DIMLN(4)=0
              EXTRADIM=0
            END IF

            ! If regular grid, initializes the lat/lon or x/y dimension lengths
            IF (GTYPE.NE.UNGTYPE) THEN
              IF( SMCGRD ) THEN
#ifdef W3_SMC
                IF( SMCOTYPE .EQ. 1 ) THEN
                  ! Flat seapoints file
                  !dimln(2) = NSEA
                  dimln(2) = SMCNOUT
                  dimln(3) = -1  ! not used
                ELSE
                  ! Regular gridded lat/lon file:
                  dimln(2) = NXO
                  dimln(3) = NYO
                ENDIF ! SMCOTYPE
#endif
              ELSE ! SMCGRD
                DIMLN(2)=IXN-IX1+1
                DIMLN(3)=IYN-IY1+1
              ENDIF ! SMCGRD
            ! If unstructured mesh, initializes the nelem,tri dimension lengths
            ELSE
              DIMLN(2)=IXN-IX1+1
              DIMLN(3)=NTRI
            ENDIF

            ! Defines index of first field variable
            IVAR1=21


! 2.4.1 Save the id of the previous file

            IF (TOGETHER.AND.(.NOT.FLFRQ)) THEN
              OLDNCID = NCIDS(1,1,1)
            ELSE
              OLDNCID = NCIDS(IFI,IFJ,IPART+1)
            END IF


! 2.4.2 Remove the new file (if not created by the run)

            INQUIRE(FILE=FNAMENC, EXIST=FEXIST)
            IF (FEXIST) THEN
              FREMOVE = .FALSE.
              ! time splitted condition
              IF (INDEX(TIMEID,OLDTIMEID).EQ.0) THEN
                ! all variables in the samefile
                IF (TOGETHER.AND.(.NOT.FLFRQ).AND.NCID.EQ.0) FREMOVE = .TRUE.
                ! a file per variable
                IF (.NOT.TOGETHER.OR.FLFRQ) FREMOVE = .TRUE.
              END IF

              IF (FREMOVE) THEN
                OPEN(UNIT=1234, IOSTAT=IRET, FILE=FNAMENC, STATUS='old')
                IF (IRET == 0) CLOSE(1234, STATUS='delete')
                FEXIST=.FALSE.
              ELSE
                NCID = OLDNCID
              END IF
            END IF

! 2.4.3 Finalize the previous file (if a new one will be created)

            IF (.NOT.FEXIST) THEN
              IF (INDEX('0000000000000000',OLDTIMEID).EQ.0 .AND. INDEX(TIMEID,OLDTIMEID).EQ.0) THEN
                IRET = NF90_REDEF(OLDNCID)
                CALL CHECK_ERR(IRET)
                IF(FL_DEFAULT_GBL_META) THEN
                  IRET=NF90_PUT_ATT(OLDNCID,NF90_GLOBAL,'stop_date',STRSTOPDATE)
                  CALL CHECK_ERR(IRET)
                ENDIF
                IRET=NF90_CLOSE(OLDNCID)
                CALL CHECK_ERR(IRET)
              END IF
            END IF


! 2.5 Creates the netcdf file

            IF (.NOT.FEXIST) THEN

              ! Initializes the time dimension length
              DIMLN(1)=1

              ! If NOT unstructure mesh (i.e. regular grid)
!! CHRISB: VARNM for lat/lon not actually used below.
!              IF (GTYPE.NE.UNGTYPE) THEN
!                ! If spherical coordinate
!                IF (FLAGLL) THEN
!                  VARNM(NFIELD+1)='Longitude'
!                  VARNM(NFIELD+2)='Latitude'
!                ! If cartesian coordinate
!                ELSE
!                  VARNM(NFIELD+1)='x'
!                  VARNM(NFIELD+2)='y'
!                END IF
!              END IF

              ! Initializes the time iteration counter n
              N=1

! 2.5.1 Creates the NetCDF file
              CALL W3CRNC(FNAMENC,NCID,DIMID,DIMLN,VARID, &
                          EXTRADIM,NCTYPE,MAPSTAOUT)

              ! Saves the NCID to keep the file opened to write all the variables
              ! and open/close at each time step
              IF (TOGETHER.AND.(.NOT.FLFRQ)) THEN
                NCIDS(1,1,1)=NCID
              ELSE
                NCIDS(IFI,IFJ,IPART+1)=NCID
              END IF

              ! If curvilinear grid, instanciates lat / lon
              IF (GTYPE.EQ.CLGTYPE) THEN
                IF (.NOT.ALLOCATED(LON2D)) ALLOCATE(LON2D(NX,NY),LAT2D(NX,NY))
                LON2D=TRANSPOSE(XGRD)
                LAT2D=TRANSPOSE(YGRD)
                IF(FL_DEFAULT_GBL_META) THEN
                  IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
                                       'latitude_resolution','n/a')
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
                                       'longitude_resolution','n/a')
                  CALL CHECK_ERR(IRET)
                ENDIF
              ! If NOT curvilinear grid,
              ELSE
                IF( SMCGRD ) THEN
#ifdef W3_SMC
                   IF(SMCOTYPE .EQ. 1) THEN
                     ! Flat seapoints file
                     IF(.NOT.ALLOCATED(lon)) ALLOCATE(lon(SMCNOUT))
                     IF(.NOT.ALLOCATED(lat)) ALLOCATE(lat(SMCNOUT))
                     IF(.NOT.ALLOCATED(smccx)) ALLOCATE(smccx(SMCNOUT))
                     IF(.NOT.ALLOCATED(smccy)) ALLOCATE(smccy(SMCNOUT))
                   ELSE
                     ! Regular gridded file
                     IF(.NOT.ALLOCATED(lon)) ALLOCATE(lon(NXO))
                     IF(.NOT.ALLOCATED(lat)) ALLOCATE(lat(NYO))
#endif
#ifdef W3_RTD
                     ! Intermediate EQUatorial lat/lon arrays for de-rotation
                     ! of rotated pole coordinates:
                     !!IF(.NOT.ALLOCATED(LON2DEQ)) ALLOCATE(LON2DEQ(NXO,NYO))
                     !!IF(.NOT.ALLOCATED(LAT2DEQ)) ALLOCATE(LAT2DEQ(NXO,NYO))
                     !
                     ! Use local RTDNX/RTDNY variables until CPP implemented to
                     ! avoid compile error when SMC switch not enabled (C.Bunney):
                     IF(.NOT.ALLOCATED(LON2DEQ)) ALLOCATE(LON2DEQ(RTDNX,RTDNY))
                     IF(.NOT.ALLOCATED(LAT2DEQ)) ALLOCATE(LAT2DEQ(RTDNX,RTDNY))
#endif
#ifdef W3_SMC
                   ENDIF
#endif
#ifdef W3_RTD
                   ! Arrays for de-rotated lat/lon coordinates:
                   IF(.NOT.ALLOCATED(LON2D)) THEN
                      !!ALLOCATE(LON2D(NXO,NYO), LAT2D(NXO,NYO))
                      !!ALLOCATE(ANGLD2D(NXO,NYO))
                      !
                      ! Use local RTDNX/RTDNY variables until CPP implemented to
                      ! avoid compile error when SMC switch not enabled (C.Bunney):
                      ALLOCATE(LON2D(RTDNX,RTDNY), LAT2D(RTDNX,RTDNY))
                      ALLOCATE(ANGLD2D(RTDNX,RTDNY))
                   ENDIF
#endif
                ELSE ! SMCGRD
                  ! instanciates lon with x/lon for regular grid or nodes for unstructured mesh
                  IF (.NOT.ALLOCATED(LON)) ALLOCATE(LON(NX))
#ifdef W3_RTD
                  ! 2d longitude array for standard grid coordinates
                  IF ( RTDL .AND. .NOT.ALLOCATED(LON2D)) &
                    ALLOCATE(LON2D(NX,NY),LON2DEQ(NX,NY),ANGLD2D(NX,NY))
#endif
                  IF (.NOT.ALLOCATED(LAT)) THEN
                    ! If regular grid, instanciates lat with y/lat
                    IF (GTYPE.EQ.RLGTYPE) THEN
                      ALLOCATE(LAT(NY))
#ifdef W3_RTD
                      ! 2d latitude array for standard grid coordinates
                      IF ( RTDL .AND. .NOT.ALLOCATED(LAT2D)) &
                        ALLOCATE(LAT2D(NX,NY),LAT2DEQ(NX,NY))
#endif
                    ! If unstructured mesh, instanciates lat with nodes
                    ELSE
                      ALLOCATE(LAT(NX))
                    END IF
                  END IF
                END IF ! SMCGRD
              END IF


! 2.5.2 Generates Lat-Lon arrays

              ! If regular grid
              IF (GTYPE.EQ.RLGTYPE .OR. GTYPE.EQ.SMCTYPE) THEN
                IF( SMCGRD ) THEN
#ifdef W3_SMC
                  ! CB: Calculate lat/lons of SMC grid
                  IF( SMCOTYPE .EQ. 1 ) THEN
                    ! CB: Flat seapoints file
                    DO i=1,SMCNOUT
                       j = SMCIDX(i)
                       lon(i) = (X0-0.5*SX) + (IJKCel(1,j) + 0.5 * IJKCel(3,j)) * dlon
                       lat(i) = (Y0-0.5*SY) + (IJKCel(2,j) + 0.5 * IJKCel(4,j)) * dlat
                       smccx(i) = IJKCel(3,j)
                       smccy(i) = IJKCel(4,j)
                    ENDDO
#endif
#ifdef W3_RTD
                    !!CALL W3EQTOLL(lat, lon, LAT2D(:,1), LON2D(:,1),       &
                    !!              ANGLD2D(:,1), POLAT, POLON, NYO*NXO)
                    !
                    ! Use local RTDNX/RTDNY variables until CPP implemented to
                    ! avoid compile error when SMC switch not enabled (C.Bunney):
                    CALL W3EQTOLL(lat, lon, LAT2D(:,1), LON2D(:,1),       &
                                  ANGLD2D(:,1), POLAT, POLON, RTDNY*RTDNX)
#endif
#ifdef W3_SMC
                  ELSE
                    ! CB: Regridded SMC data
                    SXD=DBLE(0.000001d0*DNINT(1d6*(DBLE(DXO)) ))
                    SYD=DBLE(0.000001d0*DNINT(1d6*(DBLE(DYO)) ))
                    X0D=DBLE(0.000001d0*DNINT(1d6*(DBLE(SXO)) ))
                    Y0D=DBLE(0.000001d0*DNINT(1d6*(DBLE(SYO)) ))
                    DO i=1,NXO
                      lon(i)=REAL(X0D+SXD*DBLE(i-1))
#endif
#ifdef W3_RTD
                      LON2DEQ(i,:) = lon(i)
#endif
#ifdef W3_SMC
                    END DO
                    DO i=1,NYO
                      lat(i)=REAL(Y0D+SYD*DBLE(i-1))
#endif
#ifdef W3_RTD
                      LAT2DEQ(:,i) = lat(i)
#endif
#ifdef W3_SMC
                    END DO
                    WRITE(STR2,'(F12.7)') DYO
                    STR2=ADJUSTL(STR2)
                    IF(FL_DEFAULT_GBL_META) THEN
                      IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,   &
                               'latitude_resolution', TRIM(str2))
                      WRITE(STR2,'(F12.7)') DXO
                      STR2=ADJUSTL(STR2)
                      IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,   &
                               'longitude_resolution',TRIM(str2))
                    ENDIF
#endif
#ifdef W3_RTD
                    !!CALL W3EQTOLL(LAT2DEQ, LON2DEQ, LAT2D, LON2D,       &
                    !!              ANGLD2D, POLAT, POLON, NYO*NXO)
                    !
                    ! Use local RTDNX/RTDNY variables until CPP implemented to
                    ! avoid compile error when SMC switch not enabled (C.Bunney):
                    CALL W3EQTOLL(LAT2DEQ, LON2DEQ, LAT2D, LON2D,       &
                                  ANGLD2D, POLAT, POLON, RTDNY*RTDNX)
#endif
#ifdef W3_SMC
                  ENDIF ! SMCOTYPE
#endif
                ELSE ! SMCGRD
                    SXD=DBLE(0.000001d0*DNINT(1d6*(DBLE(SX)) ))
                    SYD=DBLE(0.000001d0*DNINT(1d6*(DBLE(SY)) ))
                    X0D=DBLE(0.000001d0*DNINT(1d6*(DBLE(X0)) ))
                    Y0D=DBLE(0.000001d0*DNINT(1d6*(DBLE(Y0)) ))
                    DO I=1,NX
                      LON(I)=REAL(X0D+SXD*DBLE(I-1))
                    END DO
                    DO I=1,NY
                      LAT(I)=REAL(Y0D+SYD*DBLE(I-1))
                    END DO
#ifdef W3_RTD
                  IF ( RTDL ) THEN
                    ! Calculate the standard grid coordinates
                    DO I=1,NX
                      LON2DEQ(I,:)=LON(I)
                    END DO
                    DO I=1,NY
                      LAT2DEQ(:,I)=LAT(I)
                    END DO
                    CALL W3EQTOLL(LAT2DEQ, LON2DEQ, LAT2D, LON2D,       &
                                  ANGLD2D, POLAT, POLON, NY*NX)
                  END IF ! RTDL
#endif
                  IF(FL_DEFAULT_GBL_META) THEN
                    WRITE(STR2,'(F12.0)') SY
                    STR2=ADJUSTL(STR2)
                    IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,   &
                             'latitude_resolution', TRIM(STR2))
                    CALL CHECK_ERR(IRET)
                    WRITE(STR2,'(F12.0)') SX
                    STR2=ADJUSTL(STR2)
                    IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,   &
                              'longitude_resolution',TRIM(STR2))
                    CALL CHECK_ERR(IRET)
                  ENDIF
                END IF ! SMCGRD
              END IF

              ! If unstructured mesh
              IF (GTYPE.EQ.UNGTYPE) THEN
                LON(:)=XYB(:,1)
                LAT(:)=XYB(:,2)
                IF (.NOT.ALLOCATED(TRIGP2)) ALLOCATE(TRIGP2(3,NTRI))
                DIMLN(2)=NX
                DIMLN(3)=NTRI
                TRIGP2=TRANSPOSE(TRIGP)
                IF(FL_DEFAULT_GBL_META) THEN
                  IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
                                       'latitude_resolution','n/a')
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL, &
                                     'longitude_resolution','n/a')
                  CALL CHECK_ERR(IRET)
                ENDIF
              END IF

              ! Finishes declaration part in file by adding geographical bounds
              IF(FL_DEFAULT_GBL_META) THEN
                IF(SMCGRD) THEN
                  WRITE(STR2,'(F12.0)') MINVAL(LAT)
                ELSE
                  WRITE(STR2,'(F12.0)') MINVAL(YGRD)
                ENDIF
                STR2=ADJUSTL(STR2)
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                     'southernmost_latitude',TRIM(STR2))
                CALL CHECK_ERR(IRET)

                IF(SMCGRD) THEN
                  WRITE(STR2,'(F12.0)') MAXVAL(LAT)
                ELSE
                  WRITE(STR2,'(F12.0)') MAXVAL(YGRD)
                ENDIF
                STR2=ADJUSTL(STR2)
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                     'northernmost_latitude',TRIM(STR2))
                CALL CHECK_ERR(IRET)

                IF(SMCGRD) THEN
                  WRITE(STR2,'(F12.0)') MINVAL(LON)
                ELSE
                  WRITE(STR2,'(F12.0)') MINVAL(XGRD)
                ENDIF
                STR2=ADJUSTL(STR2)
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                    'westernmost_longitude',TRIM(STR2))
                CALL CHECK_ERR(IRET)


                IF(SMCGRD) THEN
                  WRITE(STR2,'(F12.0)') MAXVAL(LON)
                ELSE
                  WRITE(STR2,'(F12.0)') MAXVAL(XGRD)
                ENDIF
                STR2=ADJUSTL(STR2)
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                    'easternmost_longitude',TRIM(STR2))
                CALL CHECK_ERR(IRET)
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                    'minimum_altitude','-12000 m')
                CALL CHECK_ERR(IRET)
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                    'maximum_altitude','9000 m')
                CALL CHECK_ERR(IRET)
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                    'altitude_resolution','n/a')
                CALL CHECK_ERR(IRET)

#ifdef W3_RTD
                IF ( RTDL ) THEN
                    IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                        'grid_north_pole_latitude',POLAT)
                    IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,  &
                        'grid_north_pole_longitude',POLON)
                  END IF
#endif
              ENDIF ! FL_DEFAULT_GBL_META

              CALL T2D(TIME,STARTDATE,IERR)
              WRITE(STRSTARTDATE,'(I4.4,A,4(I2.2,A),I2.2)') STARTDATE(1),'-',STARTDATE(2),'-', &
                    STARTDATE(3),' ',STARTDATE(5),':',STARTDATE(6),':',STARTDATE(7)

              ! End of define mode of NetCDF file
              IRET = NF90_ENDDEF(NCID)
              CALL CHECK_ERR(IRET)

! 2.5.3 Writes longitudes, latitudes, triangles, frequency and status map (mapsta) to netcdf file

              ! If regular grid
              IF (GTYPE.EQ.RLGTYPE .OR. GTYPE.EQ.SMCTYPE) THEN
                IF(SMCGRD) THEN ! CB: shelter original code from SMC grid
#ifdef W3_SMC
                  IRET=NF90_PUT_VAR(NCID,VARID(1),LON(:))
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_VAR(NCID,VARID(2),LAT(:))
                  CALL CHECK_ERR(IRET)
                  IF(SMCOTYPE .EQ. 1) THEN
                    ! For type 1 SCM file also put lat/lons and cell sizes:
                    IRET=NF90_PUT_VAR(NCID,VARID(5),SMCCX)
                    CALL CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(6),SMCCY)
                    CALL CHECK_ERR(IRET)
                  ENDIF
#endif
                ELSE ! SMCGRD
                  IRET=NF90_PUT_VAR(NCID,VARID(1),LON(IX1:IXN))
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_VAR(NCID,VARID(2),LAT(IY1:IYN))
                  CALL CHECK_ERR(IRET)
                ENDIF ! SMCGRD
#ifdef W3_RTD
                IF ( RTDL ) THEN
                  IRET=NF90_PUT_VAR(NCID,VARID(7),LON2D(IX1:IXN,IY1:IYN))
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_VAR(NCID,VARID(8),LAT2D(IX1:IXN,IY1:IYN))
                  CALL CHECK_ERR(IRET)
                  END IF
#endif
              END IF

              ! If curvilinear grid
              IF (GTYPE.EQ.CLGTYPE) THEN
                IRET=NF90_PUT_VAR(NCID,VARID(1),LON2D(IX1:IXN,IY1:IYN))
                CALL CHECK_ERR(IRET)
                IRET=NF90_PUT_VAR(NCID,VARID(2),LAT2D(IX1:IXN,IY1:IYN))
                CALL CHECK_ERR(IRET)
              END IF

              ! If unstructured mesh
              IF (GTYPE.EQ.UNGTYPE) THEN
                IRET=NF90_PUT_VAR(NCID,VARID(1),LON(IX1:IXN))
                CALL CHECK_ERR(IRET)
                IRET=NF90_PUT_VAR(NCID,VARID(2),LAT(IX1:IXN))
                CALL CHECK_ERR(IRET)
              END IF

              ! Writes frequencies to netcdf file
              IF (EXTRADIM.EQ.1) THEN
                ALLOCATE(FREQ(I2F-I1F+1))
                !BGR Here is where we should tell it what frequencies are.
                IF (CUSTOMFRQ) THEN
                   DO i=1,usspf(2)
                      FREQ(i)=sqrt(GRAV*USSP_WN(i))*TPIINV
                   ENDDO
                ELSE
                   DO i=1,I2F-I1F+1
                      FREQ(i)=SIG(I1F-1+i)*TPIINV
                   END DO
                ENDIF
                IRET=NF90_PUT_VAR(NCID,VARID(10),FREQ)
                CALL CHECK_ERR(IRET)
                DEALLOCATE(FREQ)
              END IF

              ! Writes triangles to netcdf file
              IF (GTYPE.EQ.UNGTYPE) THEN
                IRET=NF90_PUT_VAR(NCID,VARID(4),TRIGP2)
                CALL CHECK_ERR(IRET)
              END IF

              ! Writes status map array at variable index 2+1+coordtype+idim-4
              IF (MAPSTAOUT) THEN 
                START(1)=1
                START(2)=1
                COUNT(1)=IXN-IX1+1
                COUNT(2)=IYN-IY1+1
                IF (GTYPE.NE.UNGTYPE) THEN
                  IRET=NF90_PUT_VAR(NCID,VARID(20),MAPOUT(IX1:IXN,IY1:IYN), &
                                     (/START(1:2)/),(/COUNT(1:2)/))
                ELSE
                  IRET=NF90_PUT_VAR(NCID,VARID(20),MAPOUT(IX1:IXN,1),(/START(1)/),(/COUNT(1)/))
                ENDIF
                CALL CHECK_ERR(IRET)
              END IF

              ! Write forecast reference time, if requested:
              IF (FLGFC) THEN
                IF(TIMEUNIT .EQ. 'S') THEN
                  OUTSECS = TSUBSEC(EPOCHDATE, REFDATE)
                  IRET = NF90_PUT_VAR(NCID, VARID(12), OUTSECS)
                ELSE
                  OUTJULDAY = TSUB(EPOCHDATE, REFDATE)
                  IRET = NF90_PUT_VAR(NCID, VARID(12), OUTJULDAY)
                ENDIF
                CALL CHECK_ERR(IRET)
              ENDIF

              WRITE (NDSO,973) FNAMENC

! 2.5.4  Defines the field(LON,LAT,time) of the variable (i.e. ucur,vcur for current variable)

              IRET = NF90_REDEF(NCID)
              CALL CHECK_ERR(IRET)
              DO I=1,NFIELD
                IVAR=IVAR1+I
                IF (COORDTYPE.EQ.1) THEN
                  IF (NCVARTYPE.EQ.2) THEN
                    IF( SMCGRD ) THEN
#ifdef W3_SMC
                      IF( SMCOTYPE .EQ. 1 ) THEN
                        ! SMC Flat file
                        IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHORT, (/DIMID(2), DIMID(4+EXTRADIM)/), VARID(IVAR))
                      ELSE
                        ! SMC Regridded file
                        IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHORT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                      ENDIF
                      CALL CHECK_ERR(IRET)
#endif
                    ELSE ! SMCGRD
                      IRET=NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_SHORT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                      CALL CHECK_ERR(IRET)
                    ENDIF ! SMCGRD
                    IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                    IF (NCTYPE.EQ.4) CALL CHECK_ERR(IRET)
                  ELSE
                    IF( SMCGRD ) THEN
#ifdef W3_SMC
                      IF( SMCOTYPE .EQ. 1 ) THEN
                        ! SMC Flat file
                        IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOAT, (/DIMID(2), DIMID(4+EXTRADIM)/), VARID(IVAR))
                      ELSE
                        ! SMC Regridded file
                        IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOAT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                      ENDIF
                      CALL CHECK_ERR(IRET)
#endif
                    ELSE ! SMCGRD
                      IRET=NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_FLOAT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                      CALL CHECK_ERR(IRET)
                    ENDIF ! SMCGRD
                    IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                    IF (NCTYPE.EQ.4) CALL CHECK_ERR(IRET)
                  END IF
                ELSE
                  DIMFIELD(1)=DIMID(2)
                  DIMFIELD(2)=DIMID(4)
                  DIMFIELD(3)=DIMID(5)
                  IF (NCVARTYPE.EQ.2) THEN
                    IRET = NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_SHORT, DIMFIELD(1:2+EXTRADIM), VARID(IVAR))
                    CALL CHECK_ERR(IRET)
                    IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                    IF (NCTYPE.EQ.4) CALL CHECK_ERR(IRET)
                  ELSE 
                    IRET = NF90_DEF_VAR(NCID,META(I)%VARNM, NF90_FLOAT, DIMFIELD(1:2+EXTRADIM), VARID(IVAR))
                    CALL CHECK_ERR(IRET)
                    IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                    IF (NCTYPE.EQ.4) CALL CHECK_ERR(IRET)
                  END IF  
                END IF

                ! Set scale factor to 1.0 if using FLOAT variables for output
                IF(NCVARTYPE .GT. 2) META(I)%FSC = 1.0

                !! CB - USE NEW W3META MODULE
                CALL WRITE_META(NCID, VARID(IVAR), META(I), IRET) ! CB
                CALL CHECK_ERR(IRET) ! CB
!
             !! CHRISB: Commenting out below - will be handled by w3oundmeta module
#ifdef W3_RTD

        !        IF ( RTDL ) THEN
        !          ! Add grid mapping attribute for rotated pole grids:
        !          IRET=NF90_PUT_ATT(NCID,VARID(IVAR),'grid_mapping',    &
        !                            'rotated_pole')
        !          CALL CHECK_ERR(IRET)
        !          END IF

#endif
              END DO
!
              ! put START date in global attribute
              IF(FL_DEFAULT_GBL_META) THEN
                IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'start_date',STRSTARTDATE)
                CALL CHECK_ERR(IRET)
              ENDIF
!
              IRET = NF90_ENDDEF(NCID)
              CALL CHECK_ERR(IRET)


! 2.6 Append data to the existing file

            ELSE  ! FEXIST

! 2.6.1 Get the dimensions from the netcdf header

              ! If it is an unstructured mesh
              IF (GTYPE.EQ.UNGTYPE) THEN
                IRET=NF90_INQ_VARID (NCID, 'tri', VARID(4))
                CALL CHECK_ERR(IRET)
              ! If it is a regular grid
              ELSE
                ! If it is spherical coordinate
                IF (FLAGLL) THEN
                  IF(SMCGRD) THEN
#ifdef W3_SMC
                    IF(SMCOTYPE .EQ. 1) THEN
                      IRET=NF90_INQ_DIMID (NCID, 'seapoint', DIMID(2))
                    ELSE
                      IRET=NF90_INQ_DIMID (NCID, 'longitude', DIMID(2))
                      IRET=NF90_INQ_DIMID (NCID, 'latitude', DIMID(3))
                    ENDIF
#endif
                  ELSE
                    IRET=NF90_INQ_DIMID (NCID, 'longitude', DIMID(2))
                    IRET=NF90_INQ_DIMID (NCID, 'latitude', DIMID(3))
                  ENDIF ! SMCGRD
                  IRET=NF90_INQ_VARID (NCID, 'longitude', VARID(1))
                  IRET=NF90_INQ_VARID (NCID, 'latitude', VARID(2))
                ! If it is cartesian coordinate
                ELSE
                  IRET=NF90_INQ_DIMID (NCID, 'x', DIMID(2))
                  IRET=NF90_INQ_VARID (NCID, 'x', VARID(1))
                  IRET=NF90_INQ_DIMID (NCID, 'y', DIMID(3))
                  IRET=NF90_INQ_VARID (NCID, 'y', VARID(2))
                END IF
                CALL CHECK_ERR(IRET)
              END IF
              ! Get the dimension time
              IRET=NF90_INQ_DIMID (NCID, 'time', DIMID(4+EXTRADIM))
              IRET=NF90_INQUIRE_DIMENSION (NCID, DIMID(4+EXTRADIM),len=N)
              CALL CHECK_ERR(IRET)
              IRET=NF90_INQ_VARID (NCID, 'time', VARID(3))
              IF( FLGFC ) THEN
                IRET = NF90_INQ_VARID(NCID, 'forecast_period', VARID(11))
                CALL CHECK_ERR(IRET)
              ENDIF
              ! Get the dimension f
              IF (EXTRADIM.EQ.1) IRET=NF90_INQ_DIMID (NCID, 'f', DIMID(4))

! 2.6.2 Increments the time step for existing file

              ! If it is the first field of the file in mode together
              ! or NOT together or variable with freq dim (ef or p2l)
              ! ChrisBunney: Also - check IPART=TABIPART in case first
              ! requested output is a partitioned field.
              IF((TOGETHER .AND. IFI.EQ.I1 .AND. IFJ.EQ.J1 .AND. IPART.EQ.TABIPART(1))  &
                 .OR.(.NOT.TOGETHER).OR.FLFRQ) n=n+1

! 2.6.3 Defines or gets the variables identifiers

              ! If it is the first time step, define all the variables and attributes
              IF (N.EQ.1) THEN
                IRET = NF90_REDEF(NCID)
                CALL CHECK_ERR(IRET)

                ! Loops on all the fields of the variable (i.e. ucur/vcur for current)
                DO I=1,NFIELD
                  IVAR=IVAR1+I
                  IF (COORDTYPE.EQ.1) THEN
                    IF (NCVARTYPE.EQ.2) THEN
                      IF( SMCGRD ) THEN
#ifdef W3_SMC
                        IF( SMCOTYPE .EQ. 1 ) THEN
                          ! SMC Flat file
                          IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHORT, (/DIMID(2), DIMID(4+EXTRADIM)/), VARID(IVAR))
                        ELSE
                          ! SMC Regridded file
                          IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHORT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                        ENDIF
#endif
                      ELSE
                         IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHORT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                         CALL CHECK_ERR(IRET)
                      ENDIF ! SMCGRD
                      IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                    ELSE
                      IF( SMCGRD ) THEN
#ifdef W3_SMC
                        IF( SMCOTYPE .EQ. 1 ) THEN
                          ! SMC Flat file
                          IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOAT, (/DIMID(2), DIMID(4+EXTRADIM)/), VARID(IVAR))
                        ELSE
                          ! SMC Regridded file
                          IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOAT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                        ENDIF
#endif
                      ELSE
                        IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOAT, DIMID(2:4+EXTRADIM), VARID(IVAR))
                        CALL CHECK_ERR(IRET)
                      ENDIF ! SMCGRD
                      IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                      IF (NCTYPE.EQ.4) CALL CHECK_ERR(IRET)
                    END IF
                  ELSE
                    DIMFIELD(1)=DIMID(2)
                    DIMFIELD(2)=DIMID(4)
                    DIMFIELD(3)=DIMID(5)
                    IF (NCVARTYPE.EQ.2) THEN
                      IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_SHORT, DIMFIELD(1:2+EXTRADIM), VARID(IVAR))
                      CALL CHECK_ERR(IRET)
                      IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                      IF (NCTYPE.EQ.4) CALL CHECK_ERR(IRET)
                    ELSE
                      IRET = NF90_DEF_VAR(NCID,META(I)%varnm, NF90_FLOAT, DIMFIELD(1:2+EXTRADIM), VARID(IVAR))
                      CALL CHECK_ERR(IRET)
                      IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(IVAR), 1, 1, DEFLATE)
                      CALL CHECK_ERR(IRET)
                    END IF
                  END IF
!
                  ! Set scale factor to 1.0 if using FLOAT variables for output
                  IF(NCVARTYPE .GT. 2) META(I)%FSC = 1.0

                  !! CB - USE NEW W3META MODULE
                  CALL WRITE_META(NCID, VARID(IVAR), META(I), IRET) ! CB
                  CALL CHECK_ERR(IRET) ! CB
!
             !! CHRISB: Commenting out below - will be handled by w3oundmeta module
#ifdef W3_RTD

        !          IF ( RTDL ) THEN
        !            ! Add grid mapping attribute for rotated pole grids:
        !            IRET=NF90_PUT_ATT(NCID,VARID(IVAR),'grid_mapping',   &
        !                              'rotated_pole')
        !            CALL CHECK_ERR(IRET)
        !            END IF

#endif
                END DO
                IRET = NF90_ENDDEF(NCID)
                CALL CHECK_ERR(IRET)

              ! If it is not the first time step, get all VARID from the netcdf file opened
              ELSE
                IRET=NF90_REDEF(NCID)
                CALL CHECK_ERR(IRET)
                DO I=1,NFIELD
                  ! Get meta-data for field
                  !META = GETMETA(IFI, IFJ, ICOMP=I, IPART=IPART)
                  IVAR=IVAR1+I
                  IRET=NF90_INQ_VARID (NCID, META(I)%VARNM, VARID(IVAR))
                  CALL CHECK_ERR(IRET)
                END DO
                IRET=NF90_ENDDEF(NCID)
                CALL CHECK_ERR(IRET)
              END IF !   N.EQ.1
            END IF  ! FEXIST

! 2.6.4 Defines the current time step and index

            CALL T2D(TIME,CURDATE,IERR)
            WRITE(NDSO,'(A,A9,A,I6,A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,2A)')        &
                    'Writing new record ', META(1)%ENAME(2:) ,'number ',N,    &
                    ' for ',CURDATE(1),':',CURDATE(2),':',CURDATE(3),'T',CURDATE(5),&
                    ':',CURDATE(6),':',CURDATE(7),' in file ',TRIM(FNAMENC)



            ! Defines starting point and size of arrays to be written
            START(1)=1
            START(2)=1
            START(3)=1
            START(4)=1

            ! Sets time index
            START(3+1-COORDTYPE+EXTRADIM)=N
            COUNT(1)=IXN-IX1+1
            COUNT(2)=IYN-IY1+1
            COUNT(3)=1
            COUNT(4)=1
            START1D(1)=1
            START1D(2)=N
            COUNT1D(1)=IXN-IX1+1
            COUNT1D(2)=1

            ! Puts time in NetCDF file
            IF((IFI.EQ.I1.AND.IFJ.EQ.J1.AND.TOGETHER)  &
               .OR.(.NOT.TOGETHER).OR.FLFRQ) THEN
              IVAR1 = 21

              IF(TIMEUNIT .EQ. 'S') THEN
                ! Time in seconds
                OUTSECS = TSUBSEC(EPOCHDATE,CURDATE)
                IRET = NF90_PUT_VAR(NCID, VARID(3), OUTSECS, (/N/))
              ELSE
                ! Time in days
                OUTJULDAY = TSUB(EPOCHDATE,CURDATE)
                IRET = NF90_PUT_VAR(NCID, VARID(3), OUTJULDAY, (/N/))
              ENDIF
              CALL CHECK_ERR(IRET)

              ! ChrisB: Calculate forecast period w.r.t. forecast reference time:
              IF (FLGFC) THEN
                OUTSECS = TSUBSEC(REFDATE, CURDATE)
                IRET = NF90_PUT_VAR(NCID, VARID(11), OUTSECS, (/N/))
                CALL CHECK_ERR(IRET)
              ENDIF
            END IF
!
! 2.6.5 Puts field(s) in NetCDF file

! NFIELD=3
            IF (NCVARTYPE.EQ.2) THEN
              IF ( NFIELD.EQ.3 ) THEN
                IF (SMCGRD) THEN
#ifdef W3_SMC
                  DO IX=IX1, IXN
                    DO IY=IY1, IYN
                      ! TODO: Find some other way to access MAPSTA
                      IF ( X1(IX,IY) .EQ. UNDEF ) THEN
                        MXX(IX,IY) = MFILL
                        MYY(IX,IY) = MFILL
                        MXY(IX,IY) = MFILL
                      ELSE
                        MXX(IX,IY) = NINT(X1(IX,IY)/META(1)%FSC)
                        MYY(IX,IY) = NINT(X2(IX,IY)/META(2)%FSC)
                        MXY(IX,IY) = NINT(XY(IX,IY)/META(3)%FSC)
                      END IF
                    END DO
                  END DO
                  IF(SMCOTYPE .EQ. 1) THEN
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                        MXX(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                        MYY(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),               &
                        MXY(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                    call CHECK_ERR(IRET)
                  ELSE
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                        MXX(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                        MYY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),               &
                        MXY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    call CHECK_ERR(IRET)
                  ENDIF
#endif
                ELSE ! IF(SMCGRD)
                  DO IX=IX1, IXN
                    DO IY=IY1, IYN
                      IF ( MAPSTA(IY,IX) .LE. 0 .OR. X1(IX,IY) .EQ. UNDEF ) THEN
                        MXX(IX,IY) = MFILL
                        MYY(IX,IY) = MFILL
                        MXY(IX,IY) = MFILL
                      ELSE
                        MXX(IX,IY) = NINT(X1(IX,IY)/META(1)%FSC)
                        MYY(IX,IY) = NINT(X2(IX,IY)/META(2)%FSC)
                        MXY(IX,IY) = NINT(XY(IX,IY)/META(3)%FSC)
                      END IF
                    END DO
                  END DO

                  IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),              &
                          MXX(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),            &
                          MYY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),            &
                          MXY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                  CALL CHECK_ERR(IRET)
                ENDIF ! SMCGRD
! NFIELD=2
              ELSE IF (NFIELD.EQ.2 ) THEN
! EXTRADIM=0
                IF (EXTRADIM.EQ.0) THEN
                  IF (SMCGRD) THEN
#ifdef W3_SMC
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        ! TODO: Find some other way to access MAPSTA
                        IF ( XX(IX,IY) .EQ. UNDEF ) THEN
                          MXX(IX,IY) = MFILL
                          MYY(IX,IY) = MFILL
                        ELSE
                          MXX(IX,IY) = NINT(XX(IX,IY)/META(1)%FSC)
                          MYY(IX,IY) = NINT(XY(IX,IY)/META(2)%FSC)
                        END IF
                      END DO
                    END DO
                    IF(SMCOTYPE .EQ. 1) THEN
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MXX(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                      call CHECK_ERR(IRET)
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                          MYY(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                      call CHECK_ERR(IRET)
                    ELSE
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MXX(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                      call CHECK_ERR(IRET)
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                          MYY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                      call CHECK_ERR(IRET)
                    ENDIF
#endif
                  ELSE ! IF(SMCGRD)
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        IF ( MAPSTA(IY,IX) .LE. 0 .OR. XX(IX,IY) .EQ. UNDEF ) THEN
                          MXX(IX,IY) = MFILL
                          MYY(IX,IY) = MFILL
                        ELSE
                    !PRINT*,XX(IX,IY),XY(IX,IY)
                    !STOP
                          MXX(IX,IY) = NINT(XX(IX,IY)/META(1)%FSC)
                          MYY(IX,IY) = NINT(XY(IX,IY)/META(2)%FSC)
                        END IF
                      END DO
                    END DO
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             &
                              MXX(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    CALL CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),           &
                            MYY(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    CALL CHECK_ERR(IRET)
                  ENDIF ! SMCGRD
! EXTRADIM=1
                ELSE
                  START(3+1-COORDTYPE)=0
                  DO IK=I1F,I2F
                    START(3+1-COORDTYPE)=START(3+1-COORDTYPE)+1

                    IF (SMCGRD) THEN
#ifdef W3_SMC
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          ! TODO: Find some other way to access MAPSTA
                          IF ( XXK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MXX(IX,IY) = MFILL
                            MYY(IX,IY) = MFILL
                          ELSE
                            MXX(IX,IY) = NINT(XXK(IX,IY,IK)/META(1)%FSC)
                            MYY(IX,IY) = NINT(XYK(IX,IY,IK)/META(2)%FSC)
                          END IF
                        END DO
                      END DO
                      IF(SMCOTYPE .EQ. 1) THEN
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),                     &
                            MXX(IX1:IXN,IY1:IYN),(/START(1), START(3), START(4)/), &
                            (/COUNT(1), COUNT(3), COUNT(4)/))
                        call CHECK_ERR(IRET)
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),                     &
                            MXY(IX1:IXN,IY1:IYN),(/START(1), START(3), START(4)/), &
                            (/COUNT(1), COUNT(3), COUNT(4)/))
                        call CHECK_ERR(IRET)
                      ELSE
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MXX(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                        call CHECK_ERR(IRET)
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MXX(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                        call CHECK_ERR(IRET)
                      ENDIF
#endif
                    ELSE ! IF(SMCGRD)
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          IF ( MAPSTA(IY,IX) .LE. 0 .OR.XXK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MXX(IX,IY) = MFILL
                            MYY(IX,IY) = MFILL
                          ELSE
                            MXX(IX,IY) = NINT(XXK(IX,IY,IK)/META(1)%FSC)
                            MYY(IX,IY) = NINT(XYK(IX,IY,IK)/META(2)%FSC)
                          END IF
                        END DO
                      END DO
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                              MXX(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),             &
                              MYY(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                    ENDIF ! SMCGRD
                  END DO
                END IF  ! EXTRADIM
! NFIELD=1
              ELSE
! EXTRADIM=0
                IF (EXTRADIM.EQ.0) THEN
                  IF (SMCGRD) THEN
#ifdef W3_SMC
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        ! TODO: Find some other way to access MAPSTA
                        IF ( X1(IX,IY) .EQ. UNDEF ) THEN
                          MX1(IX,IY) = MFILL
                        ELSE
                          MX1(IX,IY) = NINT(X1(IX,IY)/META(1)%FSC)
                        END IF
                      END DO
                    END DO
                    IF(SMCOTYPE .EQ. 1) THEN
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MX1(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                      call CHECK_ERR(IRET)
                    ELSE
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MX1(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                      call CHECK_ERR(IRET)
                    ENDIF
#endif
                  ELSE ! IF(SMCGRD)
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        IF ( MAPSTA(IY,IX) .LE. 0 .OR.X1(IX,IY) .EQ. UNDEF ) THEN
                          MX1(IX,IY) = MFILL
                        ELSE
                          MX1(IX,IY) = NINT(X1(IX,IY)/META(1)%FSC)
                        END IF
                      END DO
                    END DO
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MX1(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    CALL CHECK_ERR(IRET)
                  ENDIF ! SMCGRD
! EXTRADIM=1
                ELSE
                  START(3+1-COORDTYPE)=0
                  DO IK=I1F,I2F
                    START(3+1-COORDTYPE)=START(3+1-COORDTYPE)+1

                    IF (SMCGRD) THEN
#ifdef W3_SMC
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          ! TODO: Find some other way to access MAPSTA
                          IF ( XK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MX1(IX,IY) = MFILL
                          ELSE
                            MX1(IX,IY) = NINT(XK(IX,IY,IK)/META(1)%FSC)
                          END IF
                        END DO
                      END DO
                      IF(SMCOTYPE .EQ. 1) THEN
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MX1(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                        call CHECK_ERR(IRET)
                      ELSE
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MX1(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                        call CHECK_ERR(IRET)
                      ENDIF
#endif
                    ELSE ! IF(SMCGRD)
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          IF ( MAPSTA(IY,IX) .LE. 0 .OR.XK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MX1(IX,IY) = MFILL
                          ELSE
                            MX1(IX,IY) = NINT(XK(IX,IY,IK)/META(1)%FSC)
                          END IF
                        END DO
                      END DO
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MX1(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                      CALL CHECK_ERR(IRET)
                    ENDIF ! SMCGRD
                  END DO
                END IF   ! EXTRADIM
              END IF   ! NFIELD
!
! Real output (NCVARTYPE.GE.3)
!
            ELSE
              IF ( NFIELD.EQ.3 ) THEN
                IF (SMCGRD) THEN
#ifdef W3_SMC
                  DO IX=IX1, IXN
                    DO IY=IY1, IYN
                      ! TODO: Find some other way to access MAPSTA
                      IF ( X1(IX,IY) .EQ. UNDEF ) THEN
                        MXXR(IX,IY) = MFILLR
                        MYYR(IX,IY) = MFILLR
                        MXYR(IX,IY) = MFILLR
                      ELSE
                        MXXR(IX,IY) = X1(IX,IY)
                        MYYR(IX,IY) = X2(IX,IY)
                        MXYR(IX,IY) = XY(IX,IY)
                      END IF
                    END DO
                  END DO
                  IF(SMCOTYPE .EQ. 1) THEN
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                        MXXR(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                        MYYR(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),               &
                        MXYR(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                    call CHECK_ERR(IRET)
                  ELSE
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                        MXXR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                        MYYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    call CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),               &
                        MXYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    call CHECK_ERR(IRET)
                  ENDIF
#endif
                ELSE ! IF(SMCGRD)
                  DO IX=IX1, IXN
                    DO IY=IY1, IYN
                      IF ( MAPSTA(IY,IX) .LE. 0 .OR. X1(IX,IY) .EQ. UNDEF ) THEN
                        MXXR(IX,IY) = MFILLR
                        MYYR(IX,IY) = MFILLR
                        MXYR(IX,IY) = MFILLR
                      ELSE
                        MXXR(IX,IY) = X1(IX,IY)
                        MYYR(IX,IY) = X2(IX,IY)
                        MXYR(IX,IY) = XY(IX,IY)
                      END IF
                    END DO
                  END DO

                  IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),              &
                          MXXR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),            &
                          MYYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                  CALL CHECK_ERR(IRET)
                  IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+3),            &
                          MXYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                  CALL CHECK_ERR(IRET)
                ENDIF ! SMCGRD
! NFIELD=2
              ELSE IF (NFIELD.EQ.2 ) THEN
! EXTRADIM=0
                IF (EXTRADIM.EQ.0) THEN
                  IF (SMCGRD) THEN
#ifdef W3_SMC
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        ! TODO: Find some other way to access MAPSTA
                        IF ( XX(IX,IY) .EQ. UNDEF ) THEN
                          MXXR(IX,IY) = MFILLR
                          MYYR(IX,IY) = MFILLR
                        ELSE
                          MXXR(IX,IY) = XX(IX,IY)
                          MYYR(IX,IY) = XY(IX,IY)
                        END IF
                      END DO
                    END DO
                    IF(SMCOTYPE .EQ. 1) THEN
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MXXR(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                      call CHECK_ERR(IRET)
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                          MYYR(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                      call CHECK_ERR(IRET)
                    ELSE
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MXXR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                      call CHECK_ERR(IRET)
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                          MYYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                      call CHECK_ERR(IRET)
                    ENDIF
#endif
                  ELSE ! IF SMCGRD
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        IF ( MAPSTA(IY,IX) .LE. 0 .OR. XX(IX,IY) .EQ. UNDEF ) THEN
                          MXXR(IX,IY) = MFILLR
                          MYYR(IX,IY) = MFILLR
                        ELSE
                          MXXR(IX,IY) = XX(IX,IY)
                          MYYR(IX,IY) = XY(IX,IY)
                        END IF
                      END DO
                    END DO
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),             &
                              MXXR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    CALL CHECK_ERR(IRET)
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),           &
                            MYYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    CALL CHECK_ERR(IRET)
                  ENDIF ! SMCGRD
  ! EXTRADIM=1
                ELSE
                  START(4-COORDTYPE)=0
                  DO IK=I1F,I2F
                    START(4-COORDTYPE)=START(4-COORDTYPE)+1

                    IF (SMCGRD) THEN
#ifdef W3_SMC
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          ! TODO: Find some other way to access MAPSTA
                          IF ( XXK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MXXR(IX,IY) = MFILLR
                            MYYR(IX,IY) = MFILLR
                          ELSE
                            MXXR(IX,IY) = XXK(IX,IY,IK)
                            MYYR(IX,IY) = XYK(IX,IY,IK)
                          END IF
                        END DO
                      END DO
                      IF(SMCOTYPE .EQ. 1) THEN
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MXXR(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                        call CHECK_ERR(IRET)
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                            MYYR(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                        call CHECK_ERR(IRET)
                      ELSE
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MXXR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                        call CHECK_ERR(IRET)
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),               &
                            MYYR(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                        call CHECK_ERR(IRET)
                      ENDIF
#endif
                    ELSE ! IF SMCGRD
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          IF ( MAPSTA(IY,IX) .LE. 0 .OR.XXK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MXXR(IX,IY) = MFILLR
                            MYYR(IX,IY) = MFILLR
                          ELSE
                            MXXR(IX,IY) = XXK(IX,IY,IK)
                            MYYR(IX,IY) = XYK(IX,IY,IK)
                          END IF
                        END DO
                      END DO
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                              MXXR(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+2),             &
                              MYYR(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                    ENDIF ! SMCGRD
                  END DO
                END IF  ! EXTRADIM
! NFIELD=1
              ELSE
! EXTRADIM=0
                IF (EXTRADIM.EQ.0) THEN
                  IF (SMCGRD) THEN
#ifdef W3_SMC
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        ! TODO: Find some other way to access MAPSTA
                        IF ( X1(IX,IY) .EQ. UNDEF ) THEN
                          MX1R(IX,IY) = MFILLR
                        ELSE
                          MX1R(IX,IY) = X1(IX,IY)
                        END IF
                      END DO
                    END DO
                    IF(SMCOTYPE .EQ. 1) THEN
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MX1R(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                      call CHECK_ERR(IRET)
                    ELSE
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MX1R(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                      call CHECK_ERR(IRET)
                    ENDIF
#endif
                  ELSE ! IF SMCGRD
                    DO IX=IX1, IXN
                      DO IY=IY1, IYN
                        IF ( MAPSTA(IY,IX) .LE. 0 .OR.X1(IX,IY) .EQ. UNDEF ) THEN
                          MX1R(IX,IY) = MFILLR
                        ELSE
                          MX1R(IX,IY) = X1(IX,IY)
                        END IF
                      END DO
                    END DO
                    IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MX1R(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                    CALL CHECK_ERR(IRET)
                  ENDIF ! SMCGRD
! EXTRADIM=1
                ELSE
                  START(4-COORDTYPE)=0
                  DO IK=I1F,I2F
                    START(4-COORDTYPE)=START(4-COORDTYPE)+1
                    IF (SMCGRD) THEN
#ifdef W3_SMC
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          ! TODO: Find some other way to access MAPSTA
                          IF ( XK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MX1R(IX,IY) = MFILLR
                          ELSE
                            MX1R(IX,IY) = XK(IX,IY,IK)
                          END IF
                        END DO
                      END DO
                      IF(SMCOTYPE .EQ. 1) THEN
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MX1R(IX1:IXN,IY1:IYN),(/START(1), START(3)/),(/COUNT(1), COUNT(3)/))
                        call CHECK_ERR(IRET)
                      ELSE
                        IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                            MX1R(IX1:IXN,IY1:IYN),(/START(1:3)/),(/COUNT(1:3)/))
                        call CHECK_ERR(IRET)
                      ENDIF
#endif
                    ELSE ! IF SMCGRD
                      DO IX=IX1, IXN
                        DO IY=IY1, IYN
                          IF ( MAPSTA(IY,IX) .LE. 0 .OR.XK(IX,IY,IK) .EQ. UNDEF ) THEN
                            MX1R(IX,IY) = MFILLR
                          ELSE
                            MX1R(IX,IY) = XK(IX,IY,IK)
                          END IF
                        END DO
                      END DO
                      IRET=NF90_PUT_VAR(NCID,VARID(IVAR1+1),               &
                          MX1R(IX1:IXN,IY1:IYN),(/START(1:4)/),(/COUNT(1:4)/))
                      CALL CHECK_ERR(IRET)
                    END IF ! SMCGRD
                  END DO
                END IF   ! EXTRADIM
              END IF   ! NFIELD
            END IF   ! NCVARTYPE

            ! updates the variable index
            IVAR1=IVAR1+NFIELD


            ! Loops over IPART for partition variables
            ! ChrisBunney: Don't loop IPART for last two entries in section 4
            ! (16: total wind sea fraction, 17: number of parts) as these fields
            ! do not have partitions.
            IF (IFI .EQ. 4 .AND. IFJ .LE. NOGE(IFI) - 2) THEN
560           CONTINUE
              IF (INDEXIPART.LT.NBIPART) THEN
                INDEXIPART=INDEXIPART+1
                IF (TABIPART(INDEXIPART).EQ.-1) GOTO 560
                IPART=TABIPART(INDEXIPART)
                GOTO 555
              END IF
            ELSE
              INDEXIPART=1
            END IF
!
          END IF  ! FLG2D(IFI,IFJ)
        END DO  ! IFI=1, NOGRP
      END DO  ! IFJ=1, NGRPP
!
! Clean up
      DEALLOCATE(X1, X2, XX, XY, XK, XXK, XYK)
      DEALLOCATE(MX1, MXX, MYY, MXY, MAPOUT)
      DEALLOCATE(MX1R, MXXR, MYYR, MXYR)
      DEALLOCATE(AUX1)
      IF (ALLOCATED(LON)) DEALLOCATE(LON, LAT)
      IF (ALLOCATED(LON2D)) DEALLOCATE(LON2D, LAT2D)
#ifdef W3_RTD
      IF (ALLOCATED(LON2DEQ)) DEALLOCATE(LAT2DEQ, LON2DEQ, ANGLD2D)
#endif
!
      RETURN
!
! Error escape locations
!

!
! Formats
!
  973 FORMAT ( 'NEW NETCDF FILE WAS CREATED ',A)
  999 FORMAT (/' *** WAVEWATCH III ERROR IN W3EXNC :'/                &
               '     PLEASE UPDATE FIELDS !!! '/                      &
               '     IFI = ',I2, '- IFJ = ',I2/)
!
#ifdef W3_T
 9000 FORMAT (' TEST W3EXNC : FLAGS :',I3,2X,20L2)
 9001 FORMAT (' TEST W3EXNC : ITPYE :',I4/                         &
              '             IX1/N   :',2I7/                        &
              '             IY1/N   :',2I7/                        &
              '             VECTOR  :',1L2)
#endif
!
#ifdef W3_T
 9012 FORMAT (' TEST W3EXNC : BLOK PARS    : ',3I4)
 9014 FORMAT ('           BASE NAME : ',A)
#endif
!
#ifdef W3_T
 9020 FORMAT (' TEST W3EXNC : OUTPUT FIELD : ',A)
#endif
!/



!/ End of W3EXNC ----------------------------------------------------- /
!/
      END SUBROUTINE W3EXNC




!--------------------------------------------------------------------------
      SUBROUTINE W3CRNC (NCFILE, NCID, DIMID, DIMLN, VARID,  &
                         EXTRADIM, NCTYPE, MAPSTAOUT )
!
      USE W3GDATMD, ONLY : GTYPE, FLAGLL, UNGTYPE, CLGTYPE, RLGTYPE
#ifdef W3_RTD
 ! Rotated pole parameters from the mod_def file
      USE W3GDATMD, ONLY : POLAT, POLON
#endif
      USE NETCDF
      USE W3TIMEMD

      IMPLICIT NONE



      INTEGER, INTENT(IN)               :: EXTRADIM
      INTEGER, INTENT(IN)               :: NCTYPE
      CHARACTER*(*), INTENT(IN)         :: NCFILE
      INTEGER, INTENT(OUT)              :: NCID
      INTEGER, INTENT(OUT)              :: DIMID(6)
      INTEGER, INTENT(IN)               :: DIMLN(6)
      INTEGER, INTENT(OUT)              :: VARID(300)
      LOGICAL, INTENT(IN)               :: MAPSTAOUT
!
!/ ------------------------------------------------------------------- /
!   Local parameters
!
      INTEGER                           :: IVAR,IRET,ICODE,STRL,STRL2
      INTEGER                           :: DIMTRI(2)
      INTEGER                           :: DEFLATE=1
!
      CHARACTER                         :: ATTNAME*120,ATTVAL*120
!
      COORDS_ATTR = ''
!
! Creation in netCDF3 or netCDF4
!
      IF(NCTYPE.EQ.3) IRET = NF90_CREATE(TRIM(NCFILE), NF90_CLOBBER, NCID)
      IF(NCTYPE.EQ.4) IRET = NF90_CREATE(TRIM(NCFILE), NF90_NETCDF4, NCID)
      CALL CHECK_ERR(IRET)
!
! Define dimensions
!
      IRET = NF90_DEF_DIM(NCID, 'level', DIMLN(1), DIMID(1))

!
! Regular structured case
!
      IF (GTYPE.NE.UNGTYPE) THEN
        IF (FLAGLL) THEN
          IF (SMCGRD) THEN
#ifdef W3_SMC
            IF(SMCOTYPE .EQ. 1) THEN
               ! Flat seapoints file
               IRET = NF90_DEF_DIM(NCID, 'seapoint', dimln(2), DIMID(2))
            ELSE
               ! Regular gridded file:
               IRET = NF90_DEF_DIM(NCID, 'longitude', dimln(2), DIMID(2))
               IRET = NF90_DEF_DIM(NCID, 'latitude', dimln(3), DIMID(3))
            ENDIF
#endif
          ELSE
            IRET = NF90_DEF_DIM(NCID, 'longitude', DIMLN(2), DIMID(2))
            IRET = NF90_DEF_DIM(NCID, 'latitude', DIMLN(3), DIMID(3))
          ENDIF ! SMCGRD
        ELSE
          IRET = NF90_DEF_DIM(NCID, 'x', DIMLN(2), DIMID(2))
          IRET = NF90_DEF_DIM(NCID, 'y', DIMLN(3), DIMID(3))
          END IF
        CALL CHECK_ERR(IRET)
!
! Unstructured case
!
      ELSE
        IRET = NF90_DEF_DIM(NCID, 'node', DIMLN(2), DIMID(2))
        IRET = NF90_DEF_DIM(NCID, 'element', DIMLN(3), DIMID(3))
        CALL CHECK_ERR(IRET)
      ENDIF
!
!


      IF (EXTRADIM.EQ.1) THEN
        IRET = NF90_DEF_DIM(NCID, 'f', DIMLN(4), DIMID(4))
        CALL CHECK_ERR(IRET)
      ENDIF

      IRET = NF90_DEF_DIM(NCID, 'time',NF90_UNLIMITED, DIMID(4+EXTRADIM))
      CALL CHECK_ERR(IRET)

      IF (GTYPE.EQ.UNGTYPE) THEN
        IRET = NF90_DEF_DIM(NCID, 'noel',3, DIMID(5+EXTRADIM))
        CALL CHECK_ERR(IRET)
      ENDIF


!
!     define variables
!
      IF (FLAGLL) THEN
!longitude
        IF (GTYPE.EQ.RLGTYPE .OR. GTYPE.EQ.SMCTYPE) THEN
          IF (SMCGRD) THEN
#ifdef W3_SMC
            IF(SMCOTYPE .EQ. 1) THEN
              ! Flat SMC grid - use seapoint dimension:
              IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, DIMID(2), VARID(1))
              CALL CHECK_ERR(IRET)
              IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, DIMID(2), VARID(2))
              CALL CHECK_ERR(IRET)

              ! Latitude and longitude are auxililary variables in type 1 sea point
              ! SMC file; add to "coordinates" attribute:
              COORDS_ATTR = TRIM(COORDS_ATTR) // " latitude longitude"

              ! For seapoint style SMC grid, also define out cell size variables:
              IRET = NF90_DEF_VAR(NCID, 'cx', NF90_SHORT, DIMID(2), VARID(5))
              CALL CHECK_ERR(IRET)
              IRET = NF90_PUT_ATT(NCID, VARID(5), 'long_name',        &
                                  'longitude cell size factor')
              IRET = NF90_PUT_ATT(NCID, VARID(5), 'valid_min', 1)
              IRET = NF90_PUT_ATT(NCID, VARID(5), 'valid_max', 256)

              IRET = NF90_DEF_VAR(NCID, 'cy', NF90_SHORT, DIMID(2), VARID(6))
              call CHECK_ERR(IRET)
              IRET = NF90_PUT_ATT(NCID, VARID(6), 'long_name',        &
                                  'latitude cell size factor')
              IRET = NF90_PUT_ATT(NCID, VARID(6), 'valid_min', 1)
              IRET = NF90_PUT_ATT(NCID, VARID(6), 'valid_max', 256)
            ELSE
              ! Regirdded regular SMC grid - use lon/lat dimensions:
              IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, DIMID(2), VARID(1))
              call CHECK_ERR(IRET)
              IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, DIMID(3), VARID(2))
              call CHECK_ERR(IRET)
            ENDIF
#endif
          ELSE
            IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, DIMID(2), VARID(1))
            IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, DIMID(3), VARID(2))
          ENDIF ! SMCGRD
        ELSE IF (GTYPE.EQ.CLGTYPE) THEN
          IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, (/ DIMID(2), DIMID(3)/), &
                                                                            VARID(1))
          IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, (/ DIMID(2), DIMID(3)/), &
                                                                            VARID(2))
        ELSE
          IRET = NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, DIMID(2), VARID(1))
          IRET = NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, DIMID(2), VARID(2))
          END IF
        IRET=NF90_PUT_ATT(NCID,VARID(1),'units','degree_east')
#ifdef W3_RTD
 ! Is the grid really rotated
        IF ( .NOT. RTDL ) THEN
#endif
        IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','longitude')
        IRET=NF90_PUT_ATT(NCID,VARID(1),'standard_name','longitude')
#ifdef W3_RTD
        ELSE
        ! Override the above for RTD pole:
          IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','longitude in rotated pole grid')
          IRET=NF90_PUT_ATT(NCID,VARID(1),'standard_name','grid_longitude')
        END IF
#endif
        IRET=NF90_PUT_ATT(NCID,VARID(1),'valid_min',-180.0)
        IRET=NF90_PUT_ATT(NCID,VARID(1),'valid_max',360.)
!
        IRET=NF90_PUT_ATT(NCID,VARID(2),'units','degree_north')
#ifdef W3_RTD
        IF ( .NOT. RTDL ) THEN
#endif
        IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','latitude')
        IRET=NF90_PUT_ATT(NCID,VARID(2),'standard_name','latitude')
#ifdef W3_RTD
        ELSE
        ! Override the above for RTD pole:
        IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','latitude in rotated pole grid')
        IRET=NF90_PUT_ATT(NCID,VARID(2),'standard_name','grid_latitude')
        END IF
#endif
        IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_min',-90.0)
        IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_max',90.)
!
        IF(SMCGRD) THEN
#ifdef W3_SMC
          IF(SMCOTYPE .EQ. 1) THEN
#endif
#ifdef W3_RTD
            IF ( RTDL ) THEN
              ! For SMC grid type 1, standard lat/lon variables are 1D:
              IRET = NF90_DEF_VAR(NCID, 'standard_longitude', NF90_FLOAT, &
                      (/ DIMID(2) /), VARID(7))
              call CHECK_ERR(IRET)

              IRET = NF90_DEF_VAR(NCID, 'standard_latitude', NF90_FLOAT, &
                      (/ DIMID(2) /), VARID(8))
              call CHECK_ERR(IRET)
            ENDIF ! RTDL
#endif
#ifdef W3_SMC
          ELSE
#endif
#ifdef W3_RTD
            IF ( RTDL ) THEN
              IRET = NF90_DEF_VAR(NCID, 'standard_longitude', NF90_FLOAT, &
                      (/ DIMID(2), DIMID(3)/), VARID(7))
              call CHECK_ERR(IRET)

              IRET = NF90_DEF_VAR(NCID, 'standard_latitude', NF90_FLOAT, &
                      (/ DIMID(2), DIMID(3)/), VARID(8))
              call CHECK_ERR(IRET)
            ENDIF ! RTDL
#endif
#ifdef W3_SMC
          ENDIF
#endif
        ELSE
#ifdef W3_RTD
      IF ( RTDL ) THEN
        !Add secondary coordinate system linking rotated grid back to standard lat-lon
        IRET = NF90_DEF_VAR(NCID, 'standard_longitude', NF90_FLOAT, (/ DIMID(2), DIMID(3)/), &
                             VARID(7))
        call CHECK_ERR(IRET)

        IRET = NF90_DEF_VAR(NCID, 'standard_latitude', NF90_FLOAT, (/ DIMID(2), DIMID(3)/), &
                             VARID(8))
        call CHECK_ERR(IRET)
      END IF
#endif
        ENDIF ! SMCGRD
#ifdef W3_RTD

      IF ( RTDL ) THEN
        ! Attributes for standard_longitude:
        IRET=NF90_PUT_ATT(NCID,VARID(7),'units','degree_east')
        IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','longitude')
        IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','longitude')
        IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-180.0)
        IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',360.)

        ! Attributes for standard_latitude:
        IRET=NF90_PUT_ATT(NCID,VARID(8),'units','degree_north')
        IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','latitude')
        IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','latitude')
        IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',-90.0)
        IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',90.)

        ! Add rotated pole grid mapping variable (dummy scalar variable
        ! used to simply store rotated pole information; see CF1.6 conventions).
        ! TODO: FUTURE WW3_OUNF DEVELOPMENT WILL ALLOW USER TO DEFINE THE
        ! COORDINATE REFERENCE SYSTEM - THIS WILL REQUIRE THE BELOW TO BE
        ! HANDLED DIFFERENTLY. C. Bunney.
#endif

             !! CHRISB: Commenting out below - will be handled by w3oundmeta module
#ifdef W3_RTD
        !!IRET=NF90_DEF_VAR(NCID, 'rotated_pole', NF90_CHAR, VARID(12))
        !!IRET=NF90_PUT_ATT(NCID, VARID(12), 'grid_north_pole_latitude',POLAT)
        !!IRET=NF90_PUT_ATT(NCID, VARID(12), 'grid_north_pole_longitude',POLON)
        !!IRET=NF90_PUT_ATT(NCID, VARID(12), 'grid_mapping_name',              &
        !!                           'rotated_latitude_longitude')
      END IF
#endif
!
      ELSE
        IF (GTYPE.EQ.RLGTYPE) THEN
          IRET = NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, DIMID(2), VARID(1))
          IRET = NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, DIMID(3), VARID(2))
        ELSE IF (GTYPE.EQ.CLGTYPE) THEN
          IRET = NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, (/ DIMID(2), DIMID(3)/), &
                                                                            VARID(1))
          IRET = NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, (/ DIMID(2), DIMID(3)/), &
                                                                            VARID(2))
        ELSE
          IRET = NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, DIMID(2), VARID(1))
          IRET = NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, DIMID(2), VARID(2))
          END IF
!
        IRET=NF90_PUT_ATT(NCID,VARID(1),'units','m')
        IRET=NF90_PUT_ATT(NCID,VARID(1),'long_name','x')
        IRET=NF90_PUT_ATT(NCID,VARID(2),'units','m')
        IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','y')
!
      END IF  ! FLAGLL
!
      IRET=NF90_PUT_ATT(NCID,VARID(1),'axis','X')
      IRET=NF90_PUT_ATT(NCID,VARID(2),'axis','Y')
      IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(1), 1, 1, DEFLATE)
      IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(2), 1, 1, DEFLATE)

!
! frequency
!
      if (EXTRADIM.EQ.1) THEN
        IRET = NF90_DEF_VAR(NCID, 'f', NF90_FLOAT, DIMID(4), VARID(10))
        IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','wave_frequency')
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','wave_frequency')
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'units','s-1')
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,VARID(10),'axis','Hz')
        CALL CHECK_ERR(IRET)
      END IF


!
!  time
!
      ! CHRISB: Allow different time variable types:
      IRET = NF90_DEF_VAR(NCID, 'time', TVARTYPE, DIMID(4+EXTRADIM), VARID(3))
      CALL CHECK_ERR(IRET)
      IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(3), 1, 1, DEFLATE)
      CALL CHECK_ERR(IRET)
      SELECT CASE (TRIM(CALTYPE))
        CASE ('360_day')
          IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','time in 360 day calendar')
        CASE ('365_day')
          IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','time in 365 day calendar')
        CASE ('standard')
          !IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','julian day (UT)') ! CB
          IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','time')
      END SELECT
      CALL CHECK_ERR(IRET)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'standard_name','time')
      CALL CHECK_ERR(IRET)
      ! CHRISB: Allow alternative time units:
      !IRET=NF90_PUT_ATT(NCID,VARID(3),'units','days since 1990-01-01 00:00:00')
      IRET=NF90_PUT_ATT(NCID,VARID(3),'units', EPOCH)
      CALL CHECK_ERR(IRET)
      ! CHRISB: Not sure this is useful - required information is in "units"
      !IRET=NF90_PUT_ATT(NCID,VARID(3),'conventions', &
      !  'relative julian days with decimal part (as parts of the day )')
      IRET=NF90_PUT_ATT(NCID,VARID(3),'axis','T')
      CALL CHECK_ERR(IRET)
      IRET=NF90_PUT_ATT(NCID,VARID(3),'calendar',TRIM(CALTYPE))
      CALL CHECK_ERR(IRET)
!
! forecast period and (forecast reference time), if requested
!
      IF (FLGFC) THEN
         IRET = NF90_DEF_VAR(NCID, 'forecast_period', NF90_INT,         &
            DIMID(4+EXTRADIM), VARID(11))
         CALL CHECK_ERR(IRET)
         IRET = NF90_PUT_ATT(NCID, VARID(11), 'long_name',              &
            'forecast period')
         CALL CHECK_ERR(IRET)
         IRET = NF90_PUT_ATT(NCID, VARID(11), 'standard_name',          &
            'forecast_period')
         CALL CHECK_ERR(IRET)
         IRET = NF90_PUT_ATT(NCID, VARID(11), 'units', 's')
         CALL CHECK_ERR(IRET)

         ! Forecast reference time is a scalar variable:
         IRET = NF90_DEF_VAR(NCID, 'forecast_reference_time',           &
            TVARTYPE, varid=VARID(12))
         CALL CHECK_ERR(IRET)

         IRET = NF90_PUT_ATT(NCID, VARID(12), 'long_name',              &
            'forecast reference time')
         CALL CHECK_ERR(IRET)

         IRET = NF90_PUT_ATT(NCID, VARID(12), 'standard_name',          &
            'forecast_reference_time')
         CALL CHECK_ERR(IRET)

         IRET = NF90_PUT_ATT(NCID, VARID(12), 'units', EPOCH)
         !   'days since 1990-01-01 00:00:00')
         CALL CHECK_ERR(IRET)

         IRET = NF90_PUT_ATT(NCID, VARID(12), 'calendar', 'gregorian')
         CALL CHECK_ERR(IRET)

         ! Add these to auxiliary coordinates list:
         COORDS_ATTR = TRIM(COORDS_ATTR) // " forecast_period forecast_reference_time"
      ENDIF
!
! triangles for irregular grids
!
      IF (GTYPE.EQ.UNGTYPE) THEN
        DIMTRI(1)=DIMID(4+EXTRADIM+1)
        DIMTRI(2)=DIMID(3)
        IRET = NF90_DEF_VAR(NCID, 'tri', NF90_INT, DIMTRI, VARID(4))
        IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1, DEFLATE)
      END IF
!
!  Status map: useful for grid combination
!
      IF (MAPSTAOUT) THEN 
        IF (GTYPE.EQ.UNGTYPE) THEN 
          IRET = NF90_DEF_VAR(NCID,'MAPSTA', NF90_SHORT,(/ DIMID(2) /), VARID(20))
        ELSE
          IRET = NF90_DEF_VAR(NCID,'MAPSTA', NF90_SHORT,(/ DIMID(2) , DIMID(3) /), &
                                                                       VARID(20))
          ENDIF
          IF (NCTYPE.EQ.4) IRET = NF90_DEF_VAR_DEFLATE(NCID, VARID(20), 1, 1, DEFLATE)
!
        IRET=NF90_PUT_ATT(NCID,VARID(20),'long_name','status map')
        IRET=NF90_PUT_ATT(NCID,VARID(20),'standard_name','status map')
        IRET=NF90_PUT_ATT(NCID,VARID(20),'units','1')
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_min',-32)
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,VARID(20),'valid_max',32)
        CALL CHECK_ERR(IRET)
      END IF
!
! Optional (user-defined) coordinate reference system (scalar variable)
!
      IF(CRS_META%N .GT. 0) THEN
        IRET = NF90_DEF_VAR(NCID, CRS_NAME, NF90_CHAR, varid=IVAR)
        CALL CHECK_ERR(IRET)

        !CALL WRITE_FREEFORM_META(NCID, IVAR, CRS_META, N_CRSMETA, IERR)
        CALL WRITE_FREEFORM_META_LIST(NCID, IVAR, CRS_META, IERR)
        CALL CHECK_ERR(IRET)
      ENDIF
!
! Global attributes
!
      IF(FL_DEFAULT_GBL_META) THEN
        IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'WAVEWATCH_III_version_number' ,TRIM(WWVER))
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'WAVEWATCH_III_switches',TRIM(SWITCHES))
        CALL CHECK_ERR(IRET)
#ifdef W3_ST4
      IF (ZZWND.NE.10)      IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SIN4 namelist parameter ZWD',ZZWND)
      IF (AALPHA.NE.0.0095) IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SIN4 namelist parameter ALPHA0',AALPHA)
      IF (BBETA.NE.1.43)    IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SIN4 namelist parameter BETAMAX',BBETA)
      IF(SSDSC(7).NE.0.3)   IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SDS4 namelist parameter WHITECAPWIDTH', SSDSC(7))
#endif
! ... TO BE CONTINUED ...

        IF(SMCGRD) THEN
#ifdef W3_SMC
           IF(SMCOTYPE .EQ. 1) THEN
             IRET = NF90_PUT_ATT(NCID, NF90_GLOBAL, 'first_lat', Y0)
             call CHECK_ERR(IRET)
             IRET = NF90_PUT_ATT(NCID, NF90_GLOBAL, 'first_lon', X0)
             call CHECK_ERR(IRET)
             IRET = NF90_PUT_ATT(NCID, NF90_GLOBAL, 'base_lat_size', dlat)
             call CHECK_ERR(IRET)
             IRET = NF90_PUT_ATT(NCID, NF90_GLOBAL, 'base_lon_size', dlon)
             call CHECK_ERR(IRET)
             IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SMC_grid_type','seapoint')
             call CHECK_ERR(IRET)
           ELSE IF(SMCOTYPE .EQ. 2) THEN
             IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'SMC_grid_type','regular_regridded')
             call CHECK_ERR(IRET)
           ENDIF
#endif
        ENDIF
      ENDIF ! FL_DEFAULT_GBL_META

      ! ChrisB: Write user global attributes:
      CALL WRITE_GLOBAL_META(NCID, IRET)
      CALL CHECK_ERR(IRET)

      ! ChrisB: Below is the old way of writing Global attributes, this
      ! is now deprecated, but still supported at the moment...
      open(unit=994,file='NC_globatt.inp',status='old',iostat=ICODE)
      IF (ICODE.EQ.0) THEN
        DO WHILE (ICODE.EQ.0)
          read(994,'(a)',iostat=ICODE) ATTNAME
          read(994,'(a)',iostat=ICODE) ATTVAL
          IF (ICODE.EQ.0) THEN
            STRL=LEN_TRIM(ATTNAME)
            STRL2=LEN_TRIM(ATTVAL)
            IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,ATTNAME(1:STRL),ATTVAL(1:STRL2))
            CALL CHECK_ERR(IRET)
          END IF
        END DO
      ENDIF
      CLOSE(994)
      IF(FL_DEFAULT_GBL_META) THEN
        IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'product_name' ,TRIM(NCFILE))
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'area',TRIM(GNAME))
        CALL CHECK_ERR(IRET)
      ENDIF

      RETURN

      END SUBROUTINE W3CRNC


!/ ------------------------------------------------------------------- /
      SUBROUTINE S2GRID(S, X, FLDIRN)
!/
!/                  +-----------------------------------+
!/                  |           C . Bunney              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         03-Nov-2020 |
!/                  +-----------------------------------+
!/
!/    03-Nov-2020 : Creation                            ( version 7.13 )
!/
!  1. Purpose :
!
!     Exapand the seapoint array to full grid with handling of
!     SMC regridding. The FLDIRN flag should be set to true for
!     directional fields. In this case, they will be decomposed
!     into U/V components for SMC grid interpolation and converted
!     to oceanograhic convention.
!
!  2. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       S       Real. I  Sea point array
!       X       Real. O  Gridded array
!       FLDIRN  Bool. I  Directional field flag
!     ----------------------------------------------------------------
!
!/ ------------------------------------------------------------------- /
      USE W3SERVMD, ONLY : W3S2XY

      IMPLICIT NONE

      REAL, INTENT(INOUT)  :: S(:)
      REAL, INTENT(OUT) :: X(:,:)
      LOGICAL, OPTIONAL, INTENT(IN) :: FLDIRN

      LOGICAL :: FLDR
      INTEGER :: ISEA

      FLDR = .FALSE.
      IF(PRESENT(FLDIRN)) FLDR = FLDIRN

#ifdef W3_SMC
     IF( SMCGRD ) THEN
        CALL W3S2XY_SMC( S, X, FLDR )
     ELSE ! IF(SMCGRD)
#endif
      IF(FLDR) THEN
        DO ISEA=1, NSEA
          IF (S(ISEA) .NE. UNDEF )  THEN
            S(ISEA) = MOD ( 630. - RADE * S(ISEA) , 360. )
          END IF
        END DO
      ENDIF

      ! Change UNDEF sea points to NOVAL, if set differently
      IF(NOVAL .NE. UNDEF) WHERE(S .EQ. UNDEF) S = NOVAL

      CALL W3S2XY ( NSEA, NSEA, NX+1, NY, S, MAPSF, X )
#ifdef W3_SMC
     ENDIF
#endif

     END SUBROUTINE S2GRID


     SUBROUTINE UV_TO_MAG_DIR(U, V, TOLERANCE)
     ! Converts fields formulated as U/V vectors into
     ! magnitude and direction fields. Conversion is
     ! done in-place. U becomes magnitude, V becomes
     ! direction. Optional TOLERANCE sets minimum
     ! magnitude.
     IMPLICIT NONE

     REAL, INTENT(INOUT) :: U(:), V(:)
     REAL, INTENT(IN), OPTIONAL :: TOLERANCE

     REAL :: TOL = 1.0
     REAL :: MAG ! Magnitude
     INTEGER :: ISEA

     IF(PRESENT(TOLERANCE)) TOL = TOLERANCE

     DO ISEA=1, NSEA
       MAG   = SQRT(U(ISEA)**2 + V(ISEA)**2)
       IF(MAG .GT. TOL) THEN
         V(ISEA) = MOD( 630. - RADE * ATAN2(U(ISEA), V(ISEA)), 360. )
       ELSE
         V(ISEA) = UNDEF
         ! TODO - Setting V to undef does not work as later the write
         ! function only checks the U value. Set both to udef?
       END IF
       U(ISEA) = MAG
     END DO

     END SUBROUTINE UV_TO_MAG_DIR

!==============================================================================

      SUBROUTINE CHECK_ERROR(IRET, ILINE)

      USE NETCDF
      USE W3ODATMD, ONLY: NDSE
      USE W3SERVMD, ONLY: EXTCDE

      IMPLICIT NONE

      INTEGER IRET, ILINE

      IF (IRET .NE. NF90_NOERR) THEN
        WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN OUNF :'
        WRITE(NDSE,*) ' LINE NUMBER ', ILINE
        WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
        WRITE(NDSE,*) NF90_STRERROR(IRET)
        CALL EXTCDE ( 59 )
      END IF
      RETURN

      END SUBROUTINE CHECK_ERROR

!==============================================================================


!/
!/ End of W3OUNF ----------------------------------------------------- /
!/
      END PROGRAM W3OUNF
