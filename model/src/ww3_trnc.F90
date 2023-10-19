!> @file
!> @brief Contains program W3TRNC.
!>
!> @author M. Accensi @date 15-May-2018

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Convert direct access track output file to netCDF file.
!>
!> @details Info read from track_o.ww3, written to track.nc.
!>
!> @author M. Accensi  @date 15-May-2018
!>
PROGRAM W3TRNC
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           M. Accensi              |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         15-May-2018 |
  !/                  +-----------------------------------+
  !/
  !/    17-Feb-2016 : Creation                            ( version 5.11 )
  !/    11-Apr-2016 : Adapted to use more options         ( version 5.11 )
  !/    15-May-2018 : Add namelist feature                ( version 6.05 )
  !/    18-Jun-2020 : Support for 360-day calendar.       ( version 7.08 )
  !/
  !/    Copyright 2014 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Convert direct access track output file to netCDF file.
  !
  !  2. Method :
  !
  !     Info read from track_o.ww3, written to track.nc
  !
  !  3. Parameters :
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3NMOD    Subr. W3GDATMD Set number of model.
  !      W3NOUT    Subr. W3ODATMD Set number of model for output.
  !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  !     ----------------------------------------------------------------
  !
  !  5. Called by :
  !
  !     None, stand-alone program.
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
  !       !/S    Enable subroutine tracing.
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  USE CONSTANTS

#ifdef W3_NL1
    USE W3ADATMD, ONLY : W3NAUX, W3SETA
#endif
  USE W3GDATMD, ONLY : W3NMOD, W3SETG, FLAGLL, XFR, GNAME
  USE W3ODATMD, ONLY : W3NOUT, W3SETO, FNMPRE
  USE W3SERVMD, ONLY : ITRACE, NEXTLN, EXTCDE
#ifdef W3_S
  USE W3SERVMD, ONLY : STRACE
#endif
  USE W3TIMEMD
  USE W3IOGRMD, ONLY: W3IOGR
  !
  USE W3ODATMD, ONLY: NDSO, NDSE
  !
  USE W3NMLTRNCMD
  USE NETCDF
  !
  IMPLICIT NONE
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
  TYPE(NML_TRACK_T)       :: NML_TRACK
  TYPE(NML_FILE_T)        :: NML_FILE
  !
  INTEGER                 :: NDSI, NDSINP, NDSM,                  &
       NDSOUT, NDSTRC, NTRACE,              &
       NSPEC, IERR, MK, MTH, IT,            &
       ILOC, ISPEC, S3, IOUT,               &
       IRET, NCTYPE,NCID, ITH

  INTEGER                 :: TIME(2), TOUT(2), NOUT, TDUM(2),     &
       DIMID(4), VARID(18), DIMLN(4),       &
       STOPDATE(8)
#ifdef W3_S
  INTEGER, SAVE           :: IENT   = 0
#endif
  !
  REAL                    :: TH1, DTH, X, Y, DW, CX, CY, CAO, CDO,&
       WX, WY, WAO, WDO, UST, AS, DTEST,    &
       DTREQ, DTHD, RTH0, M2KM
  !
  REAL, ALLOCATABLE       :: FREQ(:), FREQ1(:), FREQ2(:), DSIP(:),&
       SPEC(:,:), E(:,:), THD(:), DIR(:)
  !
  CHARACTER*34, PARAMETER ::                                      &
       IDTST  = 'WAVEWATCH III TRACK OUTPUT SPECTRA'
  CHARACTER*30            :: FILEPREFIX, STRSTOPDATE
  CHARACTER*20            :: FORMAT1
  CHARACTER               :: IDTIME*23, IDDDAY*11, TRCKID*32,     &
       COMSTR*1, IDSTR*34, TSTSTR*3, STIME*23
  !
  LOGICAL                 :: FLGNML


  !/
  !/ ------------------------------------------------------------------- /
  !/
  !
  ! 0.  Initialize data structure
  !
  CALL W3NMOD ( 1, 6, 6 )
  CALL W3SETG ( 1, 6, 6 )
#ifdef W3_NL1
  CALL W3NAUX (    6, 6 )
  CALL W3SETA ( 1, 6, 6 )
#endif
  CALL W3NOUT (    6, 6 )
  CALL W3SETO ( 1, 6, 6 )
  !
  ! 1.  IO set-up.
  !
  NDSI   = 10
  NDSM   = 20
  NDSINP = 11
  NDSOUT = 51
  !
  NDSTRC =  6
  NTRACE = 10
  CALL ITRACE ( NDSTRC, NTRACE )
  !
#ifdef W3_S
  CALL STRACE ( IENT, 'W3TRNC' )
#endif
  !
  WRITE (NDSO,900)
  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 2.  Read model definition file.
  !
  CALL W3IOGR ( 'READ', NDSM )
  WRITE (NDSO,920) GNAME


  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 3.   Read requests from input file.
  !

  !
  ! process ww3_trnc namelist
  !
  INQUIRE(FILE=TRIM(FNMPRE)//"ww3_trnc.nml", EXIST=FLGNML)
  IF (FLGNML) THEN
    ! Read namelist
    CALL W3NMLTRNC (NDSI, TRIM(FNMPRE)//'ww3_trnc.nml', NML_TRACK, NML_FILE, IERR)

    ! 3.1 Time setup IDTIME, DTREQ, NOUT
    READ(NML_TRACK%TIMESTRIDE, *)  DTREQ
    READ(NML_TRACK%TIMECOUNT, *)   NOUT
    READ(NML_TRACK%TIMESTART, *)   TOUT(1), TOUT(2)


    ! 3.2 Output type
    NCTYPE = NML_FILE%NETCDF
    FILEPREFIX = NML_FILE%PREFIX
    S3 = NML_TRACK%TIMESPLIT


  END IF ! FLGNML

  !
  ! process old ww3_trnc.inp format
  !
  IF (.NOT. FLGNML) THEN
    OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_trnc.inp',STATUS='OLD',ERR=805,IOSTAT=IERR)
    REWIND (NDSI)

    READ (NDSI,'(A)',END=806,ERR=807,IOSTAT=IERR) COMSTR
    IF (COMSTR.EQ.' ') COMSTR = '$'
    WRITE (NDSO,901) COMSTR


    ! 3.1 Time setup IDTIME, DTREQ, NOUT
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=806,ERR=807) TOUT, DTREQ, NOUT


    ! 3.2 Output type
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=806,ERR=807) NCTYPE
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    FILEPREFIX= 'ww3.'
    READ (NDSI,*,END=806,ERR=807) FILEPREFIX
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*,END=806,ERR=807) S3

  END IF ! .NOT. FLGNML




  ! 3.3 Time setup IDTIME, DTREQ, NOUT
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


  ! 3.4 Output type
  IF ( NCTYPE.LT.3 .OR. NCTYPE.GT.4 ) THEN
    WRITE (NDSE,1010) NCTYPE
    CALL EXTCDE ( 1 )
  END IF
  ! S3 defines the number of characters in the date for the filename
  ! S3=4-> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDTHHZ ...



  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 4.  Check consistency with input file and track_o.ww3
  !
  OPEN (NDSINP,FILE=TRIM(FNMPRE)//'track_o.ww3',form='UNFORMATTED', convert=file_endian, &
       STATUS='OLD',ERR=800,IOSTAT=IERR)
  READ (NDSINP,ERR=801,IOSTAT=IERR) IDSTR, FLAGLL, MK, MTH, XFR
  !
  IF ( FLAGLL ) THEN
    M2KM  = 1.
  ELSE
    M2KM  = 1.E-3
  END IF
  !
  IF ( IDSTR .NE. IDTST ) GOTO 810

  WRITE (NDSO,902) MK, MTH
  NSPEC  = MK * MTH
  ALLOCATE ( FREQ(MK), FREQ1(MK), FREQ2(MK), DSIP(MK), &
       SPEC(MK,MTH), E(MK,MTH), THD(MTH), DIR(MTH) )
  !
  READ (NDSINP,ERR=801,IOSTAT=IERR) TH1, DTH, FREQ, DSIP

  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 5.  Time management.
  !
  IOUT = 0
  NCID = 0
  WRITE (NDSO,970)
  READ (NDSINP,END=444, ERR=801,IOSTAT=IERR) TIME
  BACKSPACE (NDSINP)


  ! 5.1 Loops on track_o.ww3 to read the time and data
  DO
    DTEST  = DSEC21 ( TIME , TOUT )

    ! cycle to reach the start time of input file
    IF ( DTEST .LT. 0. ) THEN
      CALL TICK21 ( TOUT , DTREQ )
      CYCLE
    END IF

    IF ( DTEST .GE. 0. ) THEN
      TRCKID=''
      READ (NDSINP,END=444, ERR=801,IOSTAT=IERR) TIME, X, Y, TSTSTR, TRCKID
      IF ( TSTSTR .EQ. 'SEA' ) THEN
        READ (NDSINP,ERR=801,IOSTAT=IERR) DW, CX, CY, WX, WY, UST, &
             AS, SPEC
      END IF
      IF ( IERR .EQ. -1 ) THEN
        WRITE (NDSO,944)
        EXIT
      END IF


      IF ( TIME(1).EQ.TOUT(1) .AND. TIME(2).EQ.TOUT(2) ) THEN
        ILOC = ILOC + 1
        IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
      ENDIF
      IF ( TIME(1).GT.TOUT(1) .OR. TIME(2).GT.TOUT(2) ) THEN
        CALL STME21 ( TIME , STIME )
        WRITE (NDSO,945) STIME, ILOC, ISPEC
        ILOC    = 1
        ISPEC   = 0
        IF ( TSTSTR .EQ. 'SEA' ) ISPEC = ISPEC + 1
        TOUT(1) = TIME(1)
        TOUT(2) = TIME(2)
      ENDIF
    END IF


    ! 5.1.1 Increments the global time counter IOUT
    IOUT = IOUT + 1
    CALL STME21 ( TOUT , IDTIME )
    WRITE (NDSO,971) IDTIME


    ! 5.1.2  Processes the variable value for the time step IOUT
    CALL W3EXNC ( FILEPREFIX, NCTYPE, NCID, S3, STRSTOPDATE, MK, MTH )


    ! 5.1.3 Defines the stop date
    CALL T2D(TOUT,STOPDATE,IERR)
    WRITE(STRSTOPDATE,'(I4.4,A,4(I2.2,A),I2.2)') STOPDATE(1),'-',STOPDATE(2), &
         '-',STOPDATE(3),' ',STOPDATE(5),':',STOPDATE(6),':',STOPDATE(7)

    IF ( IOUT .GE. NOUT ) EXIT
  END DO


444 CONTINUE

  ! 5.2 Closes the netCDF file
  IF (NCID.NE.0) THEN
    IRET = NF90_REDEF(NCID)
    CALL CHECK_ERR(IRET)
    IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'stop_date',STRSTOPDATE)
    CALL CHECK_ERR(IRET)
    IRET=NF90_CLOSE(NCID)
    CALL CHECK_ERR(IRET)
  END IF

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
805 CONTINUE
  WRITE (NDSE,1004) IERR
  CALL EXTCDE ( 14 )
  !
806 CONTINUE
  WRITE (NDSE,1005) IERR
  CALL EXTCDE ( 15 )
  !
807 CONTINUE
  WRITE (NDSE,1006) IERR
  CALL EXTCDE ( 16 )
  !

810 CONTINUE
  WRITE (NDSE,1010) IDSTR, IDTST
  CALL EXTCDE ( 20 )
  !
888 CONTINUE
  WRITE (NDSO,999)
  !
  ! Formats
  !
900 FORMAT (/15X,'   *** WAVEWATCH III Track output postp. ***   '/ &
       15X,'==============================================='/)
901 FORMAT ( '  Comment character is ''',A,''''/)
  !
902 FORMAT ( '      Spectral grid size : ',I3,' by ',I3//              &
       '  Opening file : '/                               &
       ' -----------------------------------------------')
920 FORMAT ( '  Grid name : ',A/)
  !
940 FORMAT (/'  Output time data : '/                               &
       ' --------------------------------------------------'/ &
       '      First time         : ',A)
941 FORMAT ( '      Interval           : ',A/                       &
       '      Number of requests : ',I10)
  !
944 FORMAT (/'      End of file reached '/)
  !
945 FORMAT ( '     ',A,' :',I6,' points and',I6,'  spectra.')
  !
970 FORMAT (//'  Generating files '/                                &
       ' --------------------------------------------------')
971 FORMAT ( '      Files for ',A)
  !
999 FORMAT (/'  End of program '/                                   &
       ' ========================================='/          &
       '         WAVEWATCH III Track output '/)
  !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
       '     ERROR IN OPENING INPUT FILE'/                    &
       '     IOSTAT =',I5/)
  !
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
       '     PREMATURE END OF INPUT FILE'/)
  !
1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
       '     ERROR IN OPENING INPUT FILE'/                    &
       '     IOSTAT =',I5/)
  !
1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
       '     ERROR IN READING FROM INPUT FILE'/               &
       '     IOSTAT =',I5/)
  !
1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
       '     ERROR IN OPENING OUTPUT FILE'/                   &
       '     IOSTAT =',I5/)
  !
1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3TRNC : '/               &
       '     ILLEGAL TYPE, NCTYPE =',I4/)
  !
  !/
  !/ Internal subroutine W3EXNC ---------------------------------------- /
  !/
CONTAINS
  !/ ------------------------------------------------------------------- /

  !> @brief Perform actual track output in NetCDF file.
  !>
  !> @param[in] FILEPREFIX
  !> @param[in] NCTYPE
  !> @param[inout] NCID
  !> @param[inout] S3
  !> @param[in] STRSTOPDATE
  !> @param[in] MK
  !> @param[in] MTH
  !>
  !> @author M. Accensi  @date 08-Apr-2016
  !>
  SUBROUTINE W3EXNC ( FILEPREFIX, NCTYPE, NCID, S3, STRSTOPDATE, MK, MTH )
    !/
    !/                  +-----------------------------------+
    !/                  |           M. Accensi              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          8-Apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/     8-apr-2016 : Creation                            ( version 5.11 )
    !/
    !  1. Purpose :
    !
    !     Perform actual track output in NetCDF file.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !     Internal parameters
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
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
    !       None.
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
    USE NETCDF
    IMPLICIT NONE

    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)       :: NCTYPE, MK, MTH
    CHARACTER(30), INTENT(IN) :: FILEPREFIX, STRSTOPDATE
    INTEGER, INTENT(INOUT)    :: NCID, S3
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: S1, S2, S4, S5, NDSDAT, IRET
    INTEGER                 :: STARTDATE(8), CURDATE(8), REFDATE(8)
    INTEGER                  :: DEFLATE=1
#ifdef W3_S
    INTEGER, SAVE           :: IENT   =   0
#endif
    !
    DOUBLE PRECISION        :: OUTJULDAY
    !
    CHARACTER*30            :: STRSTARTDATE
    CHARACTER               :: FNAMENC*50, ENAME*6
    CHARACTER, SAVE         :: OLDTIMEID*16 = '0000000000000000'
    CHARACTER, SAVE         :: TIMEID*16 = '0000000000000000'

    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
#ifdef W3_S
    CALL STRACE (IENT, 'W3EXNC')
#endif
    !
    CALL U2D('days since 1990-01-01 00:00:00',REFDATE,IERR)

    ! 1.1 Sets the date as ISO8601 convention
    ! S3 defines the number of characters in the date for the filename
    ! S3=4-> YYYY, S3=6 -> YYYYMM, S3=10 -> YYYYMMDDHH
    ! Setups min and max date format
    IF (S3.LT.4) S3=4
    IF (S3.GT.10) S3=10
    !
    ! Defines the format of FILETIME
    S5=S3-8
    S4=S3
    OLDTIMEID=TIMEID
    ! if S3=>YYYYMMDDHH then filetime='YYYYMMDDTHHMMSSZ'
    IF (S3.EQ.10) THEN
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


    ! 1.2 Setups the output type 4 ( NetCDF file )

    ENAME='.trck'
    S2=LEN_TRIM(ENAME)
    S1=LEN_TRIM(FILEPREFIX)+S4
    FNAMENC(S1+1:50)='       '
    FNAMENC(S1+1:S1+1) = '_'

    ! add variable name in file name
    FNAMENC(S1+2:S1+S2) = ENAME(2:S2)

    ! Defines the netcdf extension
    FNAMENC(S1+S2+1:S1+S2+3) = '.nc'
    FNAMENC(S1+S2+4:S1+S2+6) = '   '

    ! Defines the dimensions
    DIMLN(1)=NF90_UNLIMITED ! time
    DIMLN(2)=MK ! frequency
    DIMLN(3)=MTH ! direction
    DIMLN(4)=32  ! string track name length


    ! 1.3 Gets the netcdf id

    NDSDAT=30
    OPEN (NDSDAT,FILE=FNAMENC,status='new',IOSTAT=IRET)
    IF (IRET.EQ.0) THEN
      ! CLOSE old file
      IF (INDEX('0000000000000000',OLDTIMEID).EQ.0 .AND. INDEX(TIMEID,OLDTIMEID).EQ.0) THEN
        IRET = NF90_REDEF(NCID)
        CALL CHECK_ERR(IRET)
        IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'stop_date',STRSTOPDATE)
        CALL CHECK_ERR(IRET)
        IRET=NF90_CLOSE(NCID)
        CALL CHECK_ERR(IRET)
      END IF
      NCID=0
    ELSE
      NCID=NCID
    END IF


    ! 1.4 Creates the netcdf file

    IF (NCID.EQ.0) THEN

      ! Initializes the time iteration counter n
      IT      = 0
      ILOC    = 0
      ISPEC   = 0

      ! 1.4.1 Creates the NetCDF file

      CALL W3CRNC(NCTYPE,FNAMENC,NCID,DIMID,DIMLN,VARID)

      ! put start date in global attribute
      CALL T2D(TIME,STARTDATE,IERR)
      WRITE(STRSTARTDATE,'(I4.4,A,4(I2.2,A),I2.2)') STARTDATE(1),'-',STARTDATE(2),'-', &
           STARTDATE(3),' ',STARTDATE(5),':',STARTDATE(6),':',STARTDATE(7)
      !
      IRET=NF90_PUT_ATT(NCID,NF90_GLOBAL,'start_date',STRSTARTDATE)
      CALL CHECK_ERR(IRET)

      ! End of define mode of NetCDF file
      IRET = NF90_ENDDEF(NCID)
      CALL CHECK_ERR(IRET)

      ! Process lower band and higher band frequencies
      FREQ1(1:MK)=FREQ(1:MK)-0.5*(FREQ(1:MK)-(FREQ(1:MK)/XFR))
      FREQ2(1:MK)=FREQ(1:MK)+0.5*(-FREQ(1:MK)+(FREQ(1:MK)*XFR))
      FREQ1(1)=FREQ(1)
      FREQ2(MK)=FREQ(MK)

      ! Converts direction unit in degree
      DTHD=360./MTH
      RTH0=TH1/DTH
      DO ITH=1, MTH
        THD(ITH)=DTHD*(RTH0+REAL(ITH-1))
      END DO
      DIR(1:MTH)=MOD(360-THD(1:MTH),360.)


      ! 1.4.2 Adds general variables to NetCDF file
      IRET=NF90_PUT_VAR(NCID,VARID(2),FREQ)
      CALL CHECK_ERR(IRET)

      IRET=NF90_PUT_VAR(NCID,VARID(3),FREQ1)
      CALL CHECK_ERR(IRET)

      IRET=NF90_PUT_VAR(NCID,VARID(4),FREQ2)
      CALL CHECK_ERR(IRET)

      IRET=NF90_PUT_VAR(NCID,VARID(5),DSIP)
      CALL CHECK_ERR(IRET)

      IRET=NF90_PUT_VAR(NCID,VARID(6),DIR)
      CALL CHECK_ERR(IRET)

      WRITE (NDSO,973) FNAMENC

    END IF  ! IERR.EQ.0


    ! 1.5 Defines the current time step and index

    CALL T2D(TIME,CURDATE,IERR)
    OUTJULDAY=TSUB(REFDATE,CURDATE)
    WRITE(NDSO,'(3A,I6,A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2,2A)')       &
         'Writing new record ', ENAME(2:) ,'number ',IT,                  &
         ' for ',CURDATE(1),':',CURDATE(2),':',CURDATE(3),'T',CURDATE(5), &
         ':',CURDATE(6),':',CURDATE(7),' in file ',TRIM(FNAMENC)


    !
    ! 1.6 Exit from W3EXNC if not sea point
    !
    IF ( TSTSTR .NE. 'SEA' ) GOTO 888


    !
    ! 1.6.1 Process speed and direction components
    !
    WAO = SQRT ( WX**2 + WY**2 )
    IF ( WAO.GT.1.E-7 ) THEN
      WDO = MOD(270.-ATAN2(WY,WX)*RADE,360.)
    ELSE
      WDO = 0.
    END IF

    CAO = SQRT ( CX**2 + CY**2 )
    IF ( CAO.GT.1.E-7 ) THEN
      CDO = MOD(270.-ATAN2(CY,CX)*RADE,360.)
    ELSE
      CDO = 0.
    END IF

    !
    ! 1.7.1 Puts dimensions variables in NetCDF file
    !
    IT=IT+1
    IF ( UST .LT. 0. ) UST = -1.0

    ! time
    IRET=NF90_PUT_VAR(NCID,VARID(1),OUTJULDAY,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! longitude
    IRET=NF90_PUT_VAR(NCID,VARID(7),M2KM*X,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! latitude
    IRET=NF90_PUT_VAR(NCID,VARID(8),M2KM*Y,start=(/IT/))
    CALL CHECK_ERR(IRET)



    ! 1.7.2 Puts fields in NetCDF file


    ! 1.7.2.a Write spectrum

    IRET=NF90_PUT_VAR(NCID,VARID(9),                               &
         TRANSPOSE(SPEC),start=(/1,1,IT/), count=(/MTH,MK,1/))
    CALL CHECK_ERR(IRET)

    ! 1.7.2.b Write the basic stuff

    ! Write DW (depth)
    IRET=NF90_PUT_VAR(NCID, VARID(10),DW ,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! Write CAO (current - x direction)
    IRET=NF90_PUT_VAR(NCID, VARID(11),CAO ,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! Write CDO (current - y direction)
    IRET=NF90_PUT_VAR(NCID,VARID(12),CDO ,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! Write WAO (wind velocity - x direction)
    IRET=NF90_PUT_VAR(NCID,VARID(13),WAO ,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! Write WDO (wind velocity - y direction)
    IRET=NF90_PUT_VAR(NCID,VARID(14),WDO ,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! Write UST (friction velocity)
    IRET=NF90_PUT_VAR(NCID,VARID(15),UST,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! Write AS (air sea temperature difference)
    IRET=NF90_PUT_VAR(NCID,VARID(16),AS ,start=(/IT/))
    CALL CHECK_ERR(IRET)
    ! Track name
    IRET=NF90_PUT_VAR(NCID,VARID(18),TRCKID,start=(/1,IT/),count=(/LEN_TRIM(TRCKID),1/))
    CALL CHECK_ERR(IRET)


    !
888 CONTINUE
    !
    RETURN

    !
    ! Formats
    !
973 FORMAT ( 'NEW NetCDF file was created ',A)


    !/ End of W3EXNC ----------------------------------------------------- /
    !/
  END SUBROUTINE W3EXNC




  !--------------------------------------------------------------------------
  !> @brief Desc not available.
  !>
  !> @param[in] NCTYPE
  !> @param[in] NCFILE
  !> @param[out] NCID
  !> @param[out] DIMID
  !> @param[in] DIMLN
  !> @param[out] VARID
  !>
  !> @author NA  @date NA
  !>
  SUBROUTINE W3CRNC (NCTYPE,NCFILE,NCID,DIMID,DIMLN,VARID)

    USE NETCDF

    IMPLICIT NONE

    INTEGER, INTENT(IN)               :: NCTYPE
    CHARACTER*(*), INTENT(IN)         :: NCFILE
    INTEGER, INTENT(IN)               :: DIMLN(:)
    INTEGER, INTENT(OUT)              :: DIMID(:), VARID(:), NCID
    INTEGER                           :: IRET
    INTEGER                           :: DEFLATE=1

    !
    ! Creation in netCDF3 or netCDF4
    !
    IF(NCTYPE.EQ.3)  IRET = NF90_CREATE(TRIM(NCFILE), NF90_CLOBBER, NCID)
    IF(NCTYPE.EQ.4) IRET = NF90_CREATE(TRIM(NCFILE), NF90_NETCDF4, NCID)
    CALL CHECK_ERR(IRET)

    !
    !     Define generals dimensions
    !
    IRET = NF90_DEF_DIM(NCID, 'time', DIMLN(1), DIMID(1))
    CALL CHECK_ERR(IRET)
    IRET = NF90_DEF_DIM(NCID, 'frequency', DIMLN(2), DIMID(2))
    CALL CHECK_ERR(IRET)
    IRET = NF90_DEF_DIM(NCID, 'direction', DIMLN(3), DIMID(3))
    CALL CHECK_ERR(IRET)
    IRET = NF90_DEF_DIM(NCID, 'string32', DIMLN(4), DIMID(4))
    CALL CHECK_ERR(IRET)

    !
    !     define generals variables
    !

    !  time
    IRET=NF90_DEF_VAR(NCID, 'time', NF90_DOUBLE, (/DIMID(1)/), VARID(1))
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
    IRET=NF90_PUT_ATT(NCID,VARID(1),'conventions',                   &
         'Relative julian days with decimal part (as parts of the day)')
    IRET=NF90_PUT_ATT(NCID,VARID(1),'axis','T')
    IRET=NF90_PUT_ATT(NCID,VARID(1),'calendar',TRIM(CALTYPE))

    ! frequency
    IRET=NF90_DEF_VAR(NCID, 'frequency', NF90_FLOAT, (/DIMID(2)/),VARID(2))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(2), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(2),'long_name','center frequencies for spectra')
    IRET=NF90_PUT_ATT(NCID,VARID(2),'standard_name','frequency')
    IRET=NF90_PUT_ATT(NCID,VARID(2),'units','s-1')
    IRET=NF90_PUT_ATT(NCID,VARID(2),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(2),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_min',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(2),'valid_max',10.)
    IRET=NF90_PUT_ATT(NCID,VARID(2),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(2),'axis','Y')

    !frequency1
    IRET=NF90_DEF_VAR(NCID, 'frequency1', NF90_FLOAT, (/DIMID(2)/), VARID(3))
    CALL CHECK_ERR(IRET)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(3), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(3),'long_name','frequency of lower band')
    IRET=NF90_PUT_ATT(NCID,VARID(3),'standard_name','frequency_of_lower_band')
    IRET=NF90_PUT_ATT(NCID,VARID(3),'globwave_name','frequency_lower_band')
    IRET=NF90_PUT_ATT(NCID,VARID(3),'units','s-1')
    IRET=NF90_PUT_ATT(NCID,VARID(3),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(3),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_min',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(3),'valid_max',10.)
    IRET=NF90_PUT_ATT(NCID,VARID(3),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(3),'content','Y')
    IRET=NF90_PUT_ATT(NCID,VARID(3),'associates','frequency')

    !frequency2
    IRET=NF90_DEF_VAR(NCID, 'frequency2', NF90_FLOAT, (/DIMID(2)/), VARID(4))
    CALL CHECK_ERR(IRET)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(4), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(4),'long_name','frequency of upper band')
    IRET=NF90_PUT_ATT(NCID,VARID(4),'standard_name','frequency_of_upper_band')
    IRET=NF90_PUT_ATT(NCID,VARID(4),'globwave_name','frequency_upper_band')
    IRET=NF90_PUT_ATT(NCID,VARID(4),'units','s-1')
    IRET=NF90_PUT_ATT(NCID,VARID(4),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(4),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_min',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(4),'valid_max',10.)
    IRET=NF90_PUT_ATT(NCID,VARID(4),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(4),'content','Y')
    IRET=NF90_PUT_ATT(NCID,VARID(4),'associates','frequency')

    ! frequency area
    IRET=NF90_DEF_VAR(NCID, 'frequency_area', NF90_FLOAT,(/DIMID(2)/),VARID(5))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(5), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(5),'long_name','frequency spectral bin width')
    IRET=NF90_PUT_ATT(NCID,VARID(5),'standard_name','frequency_area')
    IRET=NF90_PUT_ATT(NCID,VARID(5),'units','s-2')
    IRET=NF90_PUT_ATT(NCID,VARID(5),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(5),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_min',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(5),'valid_max',10.)
    IRET=NF90_PUT_ATT(NCID,VARID(5),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(5),'content','Y')
    IRET=NF90_PUT_ATT(NCID,VARID(5),'associates','frequency')

    ! direction
    IRET=NF90_DEF_VAR(NCID, 'direction', NF90_FLOAT, (/DIMID(3)/),VARID(6))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(6), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(6),'long_name','sea surface wave to direction')
    IRET=NF90_PUT_ATT(NCID,VARID(6),'standard_name','sea_surface_wave_to_direction')
    IRET=NF90_PUT_ATT(NCID,VARID(6),'units','degree')
    IRET=NF90_PUT_ATT(NCID,VARID(6),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(6),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_min',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(6),'valid_max',360.)
    IRET=NF90_PUT_ATT(NCID,VARID(6),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(6),'axis','Z')

    IF (FLAGLL) THEN
      !  longitude
      IRET=NF90_DEF_VAR(NCID, 'longitude', NF90_FLOAT, (/DIMID(1)/),VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','longitude')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','longitude')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','degree_east')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_min',-180.0)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'valid_max',360.)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','T')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time')


      !  latitude
      IRET=NF90_DEF_VAR(NCID, 'latitude', NF90_FLOAT, (/DIMID(1)/),VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','latitude')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','latitude')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','degree_north')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_min',-90.0)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'valid_max',180.)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','T')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time')
    ELSE
      !  longitude
      IRET=NF90_DEF_VAR(NCID, 'x', NF90_FLOAT, (/DIMID(1)/),VARID(7))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(7), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'long_name','x')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'standard_name','x')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(7),'content','T')
      IRET=NF90_PUT_ATT(NCID,VARID(7),'associates','time')

      !  latitude
      IRET=NF90_DEF_VAR(NCID, 'y', NF90_FLOAT, (/DIMID(1)/),VARID(8))
      IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(8), 1, 1, DEFLATE)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'long_name','y')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'standard_name','y')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'units','m')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'_FillValue',NF90_FILL_FLOAT)
      IRET=NF90_PUT_ATT(NCID,VARID(8),'content','T')
      IRET=NF90_PUT_ATT(NCID,VARID(8),'associates','time')

    END IF


    ! Efth
    IRET=NF90_DEF_VAR(NCID,'efth',NF90_FLOAT,(/DIMID(3),DIMID(2),DIMID(1)/),VARID(9))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(9), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(9),'long_name',                     &
         'sea surface wave directional variance spectral density')
    IRET=NF90_PUT_ATT(NCID,VARID(9),'standard_name',                 &
         'sea_surface_wave_directional_variance_spectral_density')
    IRET=NF90_PUT_ATT(NCID,VARID(9),'globwave_name',                 &
         'directional_variance_spectral_density')
    IRET=NF90_PUT_ATT(NCID,VARID(9),'units','m2 s rad-1')
    IRET=NF90_PUT_ATT(NCID,VARID(9),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(9),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_min',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(9),'valid_max',10.)
    IRET=NF90_PUT_ATT(NCID,VARID(9),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(9),'content','TYZ')
    IRET=NF90_PUT_ATT(NCID,VARID(9),'associates','time frequency direction')

    ! DW - depth
    IRET=NF90_DEF_VAR(NCID, 'dpt', NF90_FLOAT, (/DIMID(1)/),VARID(10))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(10), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(10),'long_name','depth')
    IRET=NF90_PUT_ATT(NCID,VARID(10),'standard_name','depth')
    IRET=NF90_PUT_ATT(NCID,VARID(10),'globwave_name','depth')
    IRET=NF90_PUT_ATT(NCID,VARID(10),'units','m')
    IRET=NF90_PUT_ATT(NCID,VARID(10),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(10),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(10),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(10),'content','T')
    IRET=NF90_PUT_ATT(NCID,VARID(10),'associates','time')

    ! CAO - current speed (m/s)
    IRET=NF90_DEF_VAR(NCID, 'cur', NF90_FLOAT,(/DIMID(1)/), VARID(11))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(11), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(11),'long_name','sea water speed')
    IRET=NF90_PUT_ATT(NCID,VARID(11),'standard_name','sea_water_speed')
    IRET=NF90_PUT_ATT(NCID,VARID(11),'globwave_name','sea_water_speed')
    IRET=NF90_PUT_ATT(NCID,VARID(11),'units','m s-1')
    IRET=NF90_PUT_ATT(NCID,VARID(11),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(11),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(11),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(11),'content','T')
    IRET=NF90_PUT_ATT(NCID,VARID(11),'associates','time')

    ! CDO - current direction (degree)
    IRET=NF90_DEF_VAR(NCID, 'curdir', NF90_FLOAT,(/DIMID(1)/), VARID(12))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(12), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(12),'long_name','direction from of sea water velocity')
    IRET=NF90_PUT_ATT(NCID,VARID(12),'standard_name','direction_of_sea_water_velocity')
    IRET=NF90_PUT_ATT(NCID,VARID(12),'globwave_name','direction_of_sea_water_velocity')
    IRET=NF90_PUT_ATT(NCID,VARID(12),'units','degree')
    IRET=NF90_PUT_ATT(NCID,VARID(12),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(12),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(12),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(12),'content','T')
    IRET=NF90_PUT_ATT(NCID,VARID(12),'associates','time')

    ! WAO - wind speed (m/s)
    IRET=NF90_DEF_VAR(NCID, 'wnd', NF90_FLOAT,(/DIMID(1)/), VARID(13))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(13), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(13),'long_name','wind speed at 10m')
    IRET=NF90_PUT_ATT(NCID,VARID(13),'standard_name','wind_speed')
    IRET=NF90_PUT_ATT(NCID,VARID(13),'globwave_name','wind_speed')
    IRET=NF90_PUT_ATT(NCID,VARID(13),'units','m s-1')
    IRET=NF90_PUT_ATT(NCID,VARID(13),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(13),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(13),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(13),'content','T')
    IRET=NF90_PUT_ATT(NCID,VARID(13),'associates','time')

    ! WDO - wind direction (degree)
    IRET=NF90_DEF_VAR(NCID, 'wnddir', NF90_FLOAT,(/DIMID(1)/), VARID(14))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(14), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(14),'long_name','wind direction')
    IRET=NF90_PUT_ATT(NCID,VARID(14),'standard_name','wind_from_direction')
    IRET=NF90_PUT_ATT(NCID,VARID(14),'globwave_name','wind_from_direction')
    IRET=NF90_PUT_ATT(NCID,VARID(14),'units','m s-1')
    IRET=NF90_PUT_ATT(NCID,VARID(14),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(14),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(14),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(14),'content','T')
    IRET=NF90_PUT_ATT(NCID,VARID(14),'associates','time')

    ! UST - friction velocity  (m/s)
    IRET=NF90_DEF_VAR(NCID, 'ust', NF90_FLOAT,(/DIMID(1)/), VARID(15))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(15), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(15),'long_name','friction velocity')
    IRET=NF90_PUT_ATT(NCID,VARID(15),'standard_name','friction_velocity')
    IRET=NF90_PUT_ATT(NCID,VARID(15),'globwave_name','friction_velocity')
    IRET=NF90_PUT_ATT(NCID,VARID(15),'units','m s-1')
    IRET=NF90_PUT_ATT(NCID,VARID(15),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(15),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(15),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(15),'content','T')
    IRET=NF90_PUT_ATT(NCID,VARID(15),'associates','time')

    ! AS - air-sea temperature difference (deg C)
    IRET=NF90_DEF_VAR(NCID, 'ast',NF90_FLOAT,(/DIMID(1)/), VARID(16))
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(16), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(16),'long_name','air sea temperature difference')
    IRET=NF90_PUT_ATT(NCID,VARID(16),'standard_name','air_sea_temperature_difference')
    IRET=NF90_PUT_ATT(NCID,VARID(16),'globwave_name','air_sea_temperature_difference')
    IRET=NF90_PUT_ATT(NCID,VARID(16),'units','degree')
    IRET=NF90_PUT_ATT(NCID,VARID(16),'scale_factor',1.)
    IRET=NF90_PUT_ATT(NCID,VARID(16),'add_offset',0.)
    IRET=NF90_PUT_ATT(NCID,VARID(16),'_FillValue',NF90_FILL_FLOAT)
    IRET=NF90_PUT_ATT(NCID,VARID(16),'content','T')
    IRET=NF90_PUT_ATT(NCID,VARID(16),'associates','time')

    !  string32
    IRET=NF90_DEF_VAR(NCID, 'string32', NF90_INT, (/DIMID(4)/), VARID(17))
    CALL CHECK_ERR(IRET)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(17), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(17),'long_name','track_name number of characters')
    IRET=NF90_PUT_ATT(NCID,VARID(17),'_FillValue',NF90_FILL_INT)
    IRET=NF90_PUT_ATT(NCID,VARID(17),'axis','W')

    !  track_name
    IRET=NF90_DEF_VAR(NCID, 'track_name', NF90_CHAR, (/DIMID(4),DIMID(1)/), VARID(18))
    CALL CHECK_ERR(IRET)
    IF (NCTYPE.EQ.4) IRET=NF90_DEF_VAR_DEFLATE(NCID, VARID(18), 1, 1, DEFLATE)
    IRET=NF90_PUT_ATT(NCID,VARID(18),'long_name','track name')
    IRET=NF90_PUT_ATT(NCID,VARID(18),'content','TX')
    IRET=NF90_PUT_ATT(NCID,VARID(18),'associates','time string16')

    RETURN

  END SUBROUTINE W3CRNC

  !==============================================================================
  !>
  !> @brief Desc not available.
  !>
  !> @param IRET
  !> @author NA  @date NA
  !>
  SUBROUTINE CHECK_ERR(IRET)

    USE NETCDF
    USE W3ODATMD, ONLY: NDSE
    USE W3SERVMD, ONLY: EXTCDE

    IMPLICIT NONE

    INTEGER IRET

    IF (IRET .NE. NF90_NOERR) THEN
      WRITE(NDSE,*) ' *** WAVEWATCH III ERROR IN TRNC :'
      WRITE(NDSE,*) ' NETCDF ERROR MESSAGE: '
      WRITE(NDSE,*) NF90_STRERROR(IRET)
      CALL EXTCDE ( 59 )
    END IF
    RETURN

  END SUBROUTINE CHECK_ERR

  !==============================================================================


  !/
  !/ End of W3TRNC ----------------------------------------------------- /
  !/
END PROGRAM W3TRNC
