      MODULE W3NETCDF
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 95 |
!/                  | Last update :         1-June-2018 |
!/                  +-----------------------------------+
!/
!/    01-June-2016 : Origination                        ( version 6.04 )
!/
!  1. Purpose : NETCDF native output from ww3
!
!  2. Method :  To be described
!
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
!/S      USE W3SERVMD, ONLY: STRACE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
      INTEGER TIME0_NETCDF_QAD(2)
      INTEGER TIMEN_NETCDF_QAD(2)
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE DATE2JD(year, month, day, hour, min, sec, eJD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion 
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(in) :: year, month, day, hour, min, sec
      real(8), intent(out) :: eJD
      real(8) :: eJDbase, eFracDay
      integer a, y, m
      a = floor((DBLE(14) - DBLE(month))/DBLE(12));
      y = year + 4800 - a;
      m = month + 12*a - 3;
      ! For a date in the Gregorian calendar:
      eJDbase = DBLE(day)                                            &
     & + DBLE(floor((DBLE(153)*DBLE(m) + DBLE(2))/DBLE(5)))  &
     & + DBLE(y)*DBLE(365)                                         &
     & + DBLE(floor(DBLE(y)/DBLE(4)))                            &
     & - DBLE(floor(DBLE(y)/DBLE(100)))                          &
     & + DBLE(floor(DBLE(y)/DBLE(400))) - DBLE(32045)
      eFracDay=(DBLE(sec) +                                          &
     &          DBLE(60)*DBLE(min) +                               &
     &          DBLE(3600)*(DBLE(hour) - DBLE(12))               &
     &          )/DBLE(86400)
      eJD=eJDbase + eFracDay
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE DATE_ConvertSix2mjd(year, month, day, hour, min, sec, eMJD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion 
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(in) :: year, month, day, hour, min, sec
      real(8), intent(out) :: eMJD
      real(8) :: eJD1, eJD2
      CALL DATE2JD(year, month, day, hour, min, sec, eJD1)
!      CALL DATE2JD(1968, 5, 23, 0, 0, 0, eJD2)
      CALL DATE2JD(1858, 11, 17, 0, 0, 0, eJD2)
      eMJD=eJD1-eJD2
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE DATE_ConvertString2six(year, month, day, hour, min, sec, eTimeStr)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
!  2. Method : linear shape function 
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(out) :: year, month, day, hour, min, sec
      character(len=15), intent(in) :: eTimeStr
      character(len=4) eYear
      character(len=2) eMonth, eDay, eHour, eMin, eSec
      eYear(1:1)  = eTimeStr(1:1)
      eYear(2:2)  = eTimeStr(2:2)
      eYear(3:3)  = eTimeStr(3:3)
      eYear(4:4)  = eTimeStr(4:4)
      eMonth(1:1) = eTimeStr(5:5)
      eMonth(2:2) = eTimeStr(6:6)
      eDay(1:1)   = eTimeStr(7:7)
      eDay(2:2)   = eTimeStr(8:8)
      eHour(1:1)  = eTimeStr(10:10)
      eHour(2:2)  = eTimeStr(11:11)
      eMin(1:1)   = eTimeStr(12:12)
      eMin(2:2)   = eTimeStr(13:13)
      eSec(1:1)   = eTimeStr(14:14)
      eSec(2:2)   = eTimeStr(15:15)
      read(eYear , '(i10)' ) year
      read(eMonth, '(i10)' ) month
      read(eDay  , '(i10)' ) day
      read(eHour , '(i10)' ) hour
      read(eMin  , '(i10)' ) min
      read(eSec  , '(i10)' ) sec
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE DATE_ConvertSix2string(year, month, day, hour, min, sec, eTimeStr)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(in) :: year, month, day, hour, min, sec
      character(len=15), intent(out) :: eTimeStr
      WRITE(eTimeStr, 20) year, month, day, hour, min, sec
  20  FORMAT (i4.4, i2.2, i2.2, '.', i2.2, i2.2, i2.2)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE MONTH_LEN(year, month, lenmonth)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : days in month
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(in) :: year, month
      integer, intent(out) :: lenmonth
      IF ((month .eq. 1).or.(month .eq. 3).or.(month .eq. 5).or.(month .eq. 7)) THEN
        lenmonth=31
      END IF
      IF ((month .eq. 8).or.(month .eq. 10).or.(month .eq. 12)) THEN
        lenmonth=31
      END IF
      IF ((month .eq. 4).or.(month .eq. 6).or.(month .eq. 9).or.(month .eq. 11)) THEN
        lenmonth=30
      END IF
      IF (month .eq. 2) THEN
        IF (MOD(year, 4) .ne. 0) THEN
          lenmonth=28
        ELSE
          IF (MOD(year, 100) .ne. 0) THEN
            lenmonth=29
          ELSE
            IF (MOD(year, 400) .ne. 0) THEN
              lenmonth=28
            ELSE
              lenmonth=29
            END IF
          END IF
        END IF
      END IF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE WW3_TO_SIX(TIME, year, month, day, hour, min, sec)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      INTEGER, intent(in) :: TIME(2)
      integer, intent(out) :: year, month, day, hour, min, sec
      integer res1, res2
      day=MOD(TIME(1), 100)
      res1=(TIME(1) - day)/100
      month=MOD(res1, 100)
      year=(res1 - month)/100
      !
      sec=MOD(TIME(2), 100)
      res2=(TIME(2) - sec)/100
      min=MOD(res2, 100)
      hour=(res2 - min)/100
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE WW3_TO_JD(TIME, eJD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      USE W3ODATMD, only : IAPROC
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      INTEGER, intent(in) :: TIME(2)
      real(8), intent(out) :: eJD
      real(8) eJD1, eJD2
      integer year, month, day, hour, min, sec
      CALL WW3_TO_SIX(TIME, year, month, day, hour, min, sec)
      CALL DATE2JD(year, month, day, hour, min, sec, eJD1)
      CALL DATE2JD(1858, 11, 17, 0, 0, 0, eJD2)
      eJD=eJD1-eJD2
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE WW3_TO_string(TIME, eTimeStr)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      INTEGER, intent(in) :: TIME(2)
      character(len=15), intent(out) :: eTimeStr
      integer year, month, day, hour, min, sec
      CALL WW3_TO_SIX(TIME, year, month, day, hour, min, sec)
      CALL DATE_ConvertSix2string(year, month, day, hour, min, sec, eTimeStr)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE JD2DATE(year, month, day, hour, min, sec, eJD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      integer, intent(out) :: year, month, day, hour, min, sec
      real(8), intent(in) :: eJD
      integer ijd, a, b, c, d, e, m
      integer secNear, lenmonth
      real(8) :: fjd, second
      ijd = floor(eJD + 0.5_8)
      !
      a = ijd + 32044;
      b = floor((DBLE(4)*DBLE(a) + DBLE(3)) / DBLE(146097))
      c = a - floor((DBLE(b) * DBLE(146097)) / DBLE(4));
      !
      d = floor((DBLE(4)*DBLE(c) + DBLE(3)) / DBLE(1461))
      e = c - floor((DBLE(1461)*DBLE(d)) / DBLE(4));
      m = floor((DBLE(5) * DBLE(e) + DBLE(2)) / DBLE(153))
      !
      day   = e - floor((DBLE(153) * DBLE(m) + DBLE(2)) / DBLE(5)) + 1;
      month = m + 3 - 12 * floor(DBLE(m) / DBLE(10))
      year  = b * 100 + d - 4800 + floor(DBLE(m) / DBLE(10))
      !
      fjd    = eJD - DBLE(ijd) + 0.5_8
      second = DBLE(86400) * fjd
      hour   = floor(second/DBLE(3600))
      second = second - DBLE(3600)*DBLE(hour)
      min    = floor(second/DBLE(60))
      sec    = floor(second - DBLE(60)*min)
      !
      ! Now renormalizing
      !
      secNear=NINT(second - DBLE(60)*min)
      IF (secNear .eq. 60) THEN
        sec=0
        min=min+1
      END IF
      IF (min .eq. 60) THEN
        min=0
        hour=hour+1
      END IF
      IF (hour .eq. 24) THEN
        hour=0
        day=day+1
      END IF
      CALL MONTH_LEN(year, month, lenmonth)
      IF (day .eq. lenmonth+1) THEN
        day=1
        month=month+1
      END IF
      IF (month .eq. 13) THEN
        month=1
        year=year+1
      END IF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE CT2MJD(STIME,XMJD)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      CHARACTER(LEN=15), INTENT(IN) :: STIME
      real(8), INTENT(OUT) :: XMJD
      integer year, month, day, hour, min, sec
      real(8) XMJD_1858
      CALL DATE_ConvertString2six(year, month, day, hour, min, sec, STIME)
      CALL DATE_ConvertSix2mjd(year, month, day, hour, min, sec, XMJD)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE MJD2CT(XMJD,STIME)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : date conversion
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      CHARACTER(LEN=15), INTENT(OUT) :: STIME
      real(8), INTENT(IN) :: XMJD
      integer year, month, day, hour, min, sec
      real(8) XMJD_1858, eMJD
      CALL DATE2JD(1858, 11, 17, 0, 0, 0, XMJD_1858)
      eMJD = XMJD + XMJD_1858
      CALL JD2DATE(year, month, day, hour, min, sec, eMJD)
      CALL DATE_ConvertSix2string(year, month, day, hour, min, sec, STIME)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE IsFullHour(XMJD,result)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : time managment 
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      real(8), INTENT(IN) :: XMJD
      LOGICAL, INTENT(OUT) :: result
      integer year, month, day, hour, min, sec
      real(8) XMJD_1858, eMJD
      CALL DATE2JD(1858, 11, 17, 0, 0, 0, XMJD_1858)
      eMJD = XMJD + XMJD_1858
      CALL JD2DATE(year, month, day, hour, min, sec, eMJD)
      IF ((sec.eq.0).and.(min.eq.0)) THEN
        result=.TRUE.
      ELSE
        result=.FALSE.
      END IF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE GENERIC_NETCDF_ERROR(CallFct, idx, iret)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : error handling
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
!/S      USE W3SERVMD, ONLY: STRACE
!
      USE NETCDF
      implicit none
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!
      integer, intent(in) :: iret, idx
      character(*), intent(in) :: CallFct
      character(len=500) :: CHRERR
      character(len=1000)  :: wwmerr
      IF (iret .NE. nf90_noerr) THEN
        CHRERR = nf90_strerror(iret)
        WRITE(*,*) TRIM(CallFct), ' -', idx, '-', TRIM(CHRERR)
        STOP
      ENDIF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE GET_BOUNDARY_STATUS(STATUS)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : boundary status (code duplication)
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
!/S      USE W3SERVMD, ONLY: STRACE
!

!/PDLIB      use yowElementpool, only: ne_global, INE_global
!/PDLIB      use yowNodepool, only: np_global
      implicit none
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(inout) :: STATUS(np_global)
      INTEGER :: COLLECTED(np_global), NEXTVERT(np_global), PREVVERT(np_global)
      INTEGER          :: ISFINISHED, INEXT, IPREV
      INTEGER          :: IPNEXT, IPPREV, ZNEXT, IP, I, IE
      STATUS(:) = 0
      DO IE=1,ne_global
        DO I=1,3
          IF (I.EQ.1) THEN
            IPREV=3
          ELSE
            IPREV=I-1
          END IF
          IF (I.EQ.3) THEN
            INEXT=1
          ELSE
            INEXT=I+1
          END IF
          IP=INE_global(I,IE)
          IPNEXT=INE_global(INEXT,IE)
          IPPREV=INE_global(IPREV,IE)
          IF (STATUS(IP).EQ.0) THEN
            STATUS(IP)=1
            PREVVERT(IP)=IPPREV
            NEXTVERT(IP)=IPNEXT
          END IF
        END DO
      END DO
      STATUS(:)=0
      DO
        COLLECTED(:)=0
        DO IE=1,ne_global
          DO I=1,3
            IF (I.EQ.1) THEN
              IPREV=3
            ELSE
              IPREV=I-1
            END IF
            IF (I.EQ.3) THEN
              INEXT=1
            ELSE
              INEXT=I+1
            END IF
            IP=INE_global(I,IE)
            IPNEXT=INE_global(INEXT,IE)
            IPPREV=INE_global(IPREV,IE)
            IF (STATUS(IP).eq.0) THEN
              ZNEXT=NEXTVERT(IP)
              IF (ZNEXT.eq.IPPREV) THEN
                COLLECTED(IP)=1
                NEXTVERT(IP)=IPNEXT
                IF (NEXTVERT(IP).eq.PREVVERT(IP)) THEN
                  STATUS(IP)=1
                END IF
              END IF
            END IF
          END DO
        END DO
        ISFINISHED=1
        DO IP=1,np_global
          IF ((COLLECTED(IP).eq.0).and.(STATUS(IP).eq.0)) THEN
            STATUS(IP)=-1
          END IF
          IF (STATUS(IP).eq.0) THEN
            ISFINISHED=0
          END IF
        END DO
        IF (ISFINISHED.eq.1) THEN
          EXIT
        END IF
      END DO
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE NETCDF_QAD_DEFINE_VAR(VarName, nx_dims, ny_dims, ntime_dims, ncid)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : define netcdfvar 
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      USE W3GDATMD, ONLY : GTYPE, UNGTYPE
      USE NETCDF
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(in) :: nx_dims, ny_dims, ntime_dims, ncid
      character(len=*), intent(in) :: VarName
      character (len = *), parameter :: CallFct="NETCDF_QAD_DEFINE_VAR"
      integer iret, var_id
      IF (GTYPE .eq. UNGTYPE) THEN
        iret=nf90_def_var(ncid,TRIM(VarName),NF90_REAL,(/ nx_dims, ntime_dims /),var_id)
        CALL GENERIC_NETCDF_ERROR(CallFct, 1, iret)
      ELSE
        iret=nf90_def_var(ncid,TRIM(VarName),NF90_REAL,(/ ny_dims, nx_dims, ntime_dims /),var_id)
        CALL GENERIC_NETCDF_ERROR(CallFct, 2, iret)
      END IF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE NETCDF_QAD_PUT_VAR(VarName, fid, nVar, VAR_TOT, ncid, pos)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : put value
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      USE W3GDATMD, ONLY : GTYPE, UNGTYPE
      USE W3GDATMD, ONLY : NY, NX, NSEA, MAPFS, MAPSF
      USE NETCDF
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      REAL, intent(in) :: VAR_TOT(nVar, NSEA)
      integer, intent(in) :: fid, nVar, ncid, pos
      character(len=*), intent(in) :: VarName
      INTEGER iret, var_id
      INTEGER IP_glob, ISEA, IX, IY
      character (len = *), parameter :: CallFct="NETCDF_QAD_PUT_VAR"
      real varWrite_FE(NX)
      real varWrite_FD(NY,NX)
      iret=nf90_inq_varid(ncid, TRIM(VarName), var_id)
      CALL GENERIC_NETCDF_ERROR(CallFct, 1, iret)
      IF (GTYPE .eq. UNGTYPE) THEN
        varWrite_FE = 0
        DO IP_glob=1,NX
          ISEA=MAPFS(1,IP_glob)
          IF (ISEA .gt. 0) THEN
            varWrite_FE(IP_glob)=VAR_TOT(fid, ISEA)
          END IF
        END DO
        iret=nf90_put_var(ncid,var_id,varWrite_FE,start = (/1, pos/), count = (/ NX, 1 /))
        CALL GENERIC_NETCDF_ERROR(CallFct, 2, iret)
      ELSE
        varWrite_FD = 0
        DO ISEA=1,NSEA
          IX = MAPSF(ISEA,1)
          IY = MAPSF(ISEA,2)
          varWrite_FD(IY,IX)=VAR_TOT(fid,ISEA)
        END DO
        iret=nf90_put_var(ncid,var_id,varWrite_FD,start = (/1, 1, pos/), count = (/ NY, NX, 1 /))
        CALL GENERIC_NETCDF_ERROR(CallFct, 2, iret)
      END IF
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE OUTPUT_NETCDF_QUICK_AND_DIRTY(IMOD, DTG)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : output driver
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

      integer, intent(in) :: IMOD
      real, intent(in) :: DTG
      real(8) eTime, eJD
      LOGICAL result
      INTEGER, SAVE :: pos_idx = 0
      CALL WW3_TO_JD(TIME0_NETCDF_QAD, eJD)
      eTime = eJD + DBLE(pos_idx)*DBLE(DTG)/DBLE(86400)
      !CALL IsFullHour(eTime,result)
!      IF (.true.) THEN
        CALL OUTPUT_NETCDF_QUICK_AND_DIRTY_KERNEL(IMOD, DTG, eTime)
!      END IF
      pos_idx=pos_idx+1
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      SUBROUTINE OUTPUT_NETCDF_QUICK_AND_DIRTY_KERNEL(IMOD, DTG, eTime)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |                                   |  
!/                  | Mathieu Dutour-Sikiric (IRB)      |
!/                  | Aron Roland (BGS IT&E GmbH)       |
!/                  |                                   |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         01-Mai-2018 |
!/                  +-----------------------------------+
!/
!/    01-Mai-2018 : Origination.                        ( version 6.04 )
!/
!  1. Purpose : actual output routine
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
!/S      USE W3SERVMD, ONLY: STRACE
!

      USE W3WDATMD, ONLY : WLV
      USE W3GDATMD, ONLY : FTE, GTYPE, UNGTYPE, ZB, NSPEC
      USE W3ADATMD, ONLY : U10, CX, CY
      USE W3ADATMD, ONLY : DW, CG
      USE W3WDATMD, ONLY : VA, TIME
      USE W3GDATMD, ONLY : MAPSTA, XGRD, YGRD, NX, NY, DDEN, FLAGLL
      USE W3GDATMD, ONLY : ECOS, ESIN, SIG, DTH, XFR, IOBP, IOBPD
      USE W3GDATMD, ONLY : NK, NTH, NSEA, NSEAL, NTRI, XYB, TRIGP, GRIDS
!/SETUP      USE W3WDATMD, ONLY: ZETA_SETUP
!/MPI      USE W3ADATMD, ONLY: MPI_COMM_WCMP
      USE W3ODATMD, only : IAPROC, NAPROC, NTPROC
!/PDLIB      USE yowNodepool, only: ListIPLG, ListNP, ListNPA
!/PDLIB      USE yowNodepool, only: np, iplg
!/PDLIB      use yowDatapool, only: rtype
!/PDLIB      USE yowRankModule, only : IPGL_TO_PROC
      USE W3PARALL, only: INIT_GET_ISEA, INIT_GET_JSEA_ISPROC
      USE W3GDATMD, only : MAPFS
      USE NETCDF
      USE constants, only : TPI, TPIINV, RADE
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETERs
!/
!/S      INTEGER, SAVE           :: IENT = 0
!/
!/ ------------------------------------------------------------------- /
!/
!/S      CALL STRACE (IENT, 'VA_SETUP_IOBPD')
!

!/MPI      INCLUDE "mpif.h"
!/MPI      integer :: istatus(MPI_STATUS_SIZE)
      integer, intent(in) :: IMOD
      real, intent(in) :: DTG
      real(8), intent(in) :: eTime
      character (len = *), parameter :: CallFct="OUTPUT_NETCDF_QUICK_AND_DIRTY"
      character (len = *), parameter :: FILENAME = "WW3_output.nc"
      character (len = *), parameter :: UNITS = "units"
      integer, parameter :: nVar = 14
      REAL VAR_TOT_NSEA(nVar, NSEA), rVect(nVar, NSEA), EBND, ET, HSIG
      INTEGER rStatus(NSEA)
      INTEGER IK, ISP, ITH, ISEA, I, IPROC, J, JSEA, nbPlus
      logical, save :: IsInitDone = .FALSE.
      character (len=15) :: eTimeStr
      character eChar
      integer :: recs_his
      INTEGER Status(NSEA)
      integer iret, ncid, var_id, irec_dim, ierr, ntime_dims, nnode_dims
      integer one_dims, eight_dims, nfreq_dims, ndir_dims, iwbmnpgl_dims
      integer three_dims, mne_dims, fifteen_dims, nbIncorr
      integer nx_dims, ny_dims
      integer, allocatable :: INEtotal(:,:)
      integer eIdx(1)
      real(8) eJD, eVar(1)
      real EBD(NK)
      integer oceantimestr_id
      real DEPTHwrite(NX)
      integer pos, IP_glob
      real varWrite(NX)
      real FACTOR
      real eVal
      real eFP, eDSPR, eT02, eDIR, eEBD
      real ETX, ETY, eAB, eABX, eABY, EC
      real EBAND, eQuotET
      integer IKP0, ICEN, ILOW, IHGH
      integer ISPROC
      real XL, XH, XL2, XH2, DENOM, FP0, EL, EH
      real ETHS, eTHM, ThrET
      integer IOBP_NX(NX), IOBP_relevant(NX)
      integer IWBMNPGL, IX, eIOBP_NX, IWB, istat, idx
      integer, allocatable :: ListIdx(:)
      real, allocatable :: SPPARM(:,:)
      real, allocatable :: WBACbound(:,:,:), WBACexch(:,:,:)
      integer nbBound_IOBP, nbBound_ISEA
      integer ListIndexes(NX)
      integer nbexport, idxbnd, IP, eSend, NPloc
      integer sumIOBP_NX, sumIOBP_relevant, eIOBPrel
      integer, allocatable :: iVect(:)
      integer ListFirst(NAPROC)
      logical DOPEAK
      integer eMap, eIOBP
      integer nbIOBP0, nbIOBP1
      integer nbMAP2_IOBP0
      REAL,    PARAMETER :: THR_FP0  = 0.00000001
      DOPEAK = .true.
      IF (GRIDS(IMOD)%GTYPE .ne. UNGTYPE) THEN
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Exiting because the grid is not unstructured'
!/DEBUGNETCDF        FLUSH(740+IAPROC)
        RETURN
      END IF
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Entering the NETCDF output routine'
!/DEBUGNETCDF        FLUSH(740+IAPROC)
      !
      ! There are several arrays in WW3 for handling boundary condition
      ! One is the MAPSTA array which is defined in ww3_grid (see computation
      ! of TMPSTA there)
      !
      ! But we have also the IOBP array for that. The IOBP is varying in time.
      ! IT depends on the wet/dry mechanism.
      !
      ! The boundary conditions are applied in the nest.ww3 file. The boundary is
      ! set there.
      IWBMNPGL = 0
      ListIndexes = 0
      sumIOBP_NX=0
      sumIOBP_relevant=0
      nbBound_IOBP=0
      nbBound_ISEA=0
      nbMAP2_IOBP0=0
      nbIOBP0=0
      nbIOBP1=0
      DO IX=1,NX
        ISEA=MAPFS(1,IX)
        eMap=MAPSTA(1,IX)
        eIOBP=IOBP(IX)
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'IX=', IX, ' IOBP/MAP=', eIOBP, eMap
        IF (ISEA .eq. 0) THEN
          nbBound_ISEA=nbBound_ISEA+1
        END IF
        IF (IOBP(IX) .eq. 0) nbIOBP0 = nbIOBP0 + 1
        IF (IOBP(IX) .eq. 1) nbIOBP1 = nbIOBP1 + 1
        IF (IOBP(IX) .eq. 0) THEN
          eIOBP_NX=2
          nbBound_IOBP=nbBound_IOBP+1
        ELSE
          eIOBP_NX=0
        END IF
        IF ((eMap .eq. 2).and.(eIOBP .eq.0)) THEN
          nbMAP2_IOBP0=nbMAP2_IOBP0 + 1
        END IF
        IF ((eMap .eq. 2).and.(ISEA.ne.0)) THEN
          IWBMNPGL = IWBMNPGL + 1
          ListIndexes(IX)=IWBMNPGL
          eIOBPrel=2
        ELSE
          eIOBPrel=0
        END IF
        IOBP_NX(IX)=eIOBP_NX
        IOBP_relevant(IX)=eIOBPrel
        sumIOBP_NX = sumIOBP_NX + 1 - eIOBP_NX
        sumIOBP_relevant = sumIOBP_relevant + 1 - eIOBPrel
      END DO
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'nbIOBP0=', nbIOBP0
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'nbIOBP1=', nbIOBP1
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'nbMAP2_IOBP0=', nbMAP2_IOBP0
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'nbBound_ISEA=', nbBound_ISEA
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'nbBound_IOBP=', nbBound_IOBP
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'sumIOBP_NX=', sumIOBP_NX
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'sumIOBP_relevant=', sumIOBP_relevant
!/DEBUGNETCDF        FLUSH(740+IAPROC)
      allocate(ListIdx(IWBMNPGL), SPPARM(8,IWBMNPGL), stat=istat)
      idx=0
      DO IX=1,NX
        IF (IOBP_relevant(IX) .eq. 2) THEN
          idx=idx+1
          ListIdx(idx)=IX
        END IF
      END DO
      !
      VAR_TOT_NSEA=0
      Status=0
      DO JSEA=1,NSEAL
        CALL INIT_GET_ISEA(ISEA, JSEA)
        IP=JSEA
        IX=iplg(IP)
        ET = 0
        ETX = 0
        ETY = 0
        ET02 = 0
        DO IK=1, NK
          EBND   = 0.
          eAB = 0
          eABX = 0
          eABY = 0
          FACTOR       = DDEN(IK) / CG(IK,ISEA)
          DO ITH=1, NTH
            ISP    = ITH + (IK-1)*NTH
            eEBD   = VA(ISP,JSEA) * FACTOR
            EBND   = EBND + eEBD
            eAB    = eAB  + VA(ISP,JSEA)
            eABX   = eABX + VA(ISP,JSEA)*ECOS(ITH)
            eABY   = eABY + VA(ISP,JSEA)*ESIN(ITH)
          END DO
          EBD(IK) = eAB*FACTOR
          ETX = ETX + eABX*FACTOR
          ETY = ETY + eABY*FACTOR
          ET     = ET   + EBND
          ET02   = ET02 + EBD(IK)*SIG(IK)**2
        END DO
        EBAND = eAB / CG(NK,ISEA)
        ET02 = ET02 + EBAND*0.5*SIG(NK)**4 * DTH
        IF ( ET02 .GT. 1.E-7 ) THEN
          eT02 = TPI * SQRT(ET / ET02 )
        ELSE
          eT02 = TPI / SIG(NK)
        ENDIF
        !
        ! Computing FP0
        !
        EC=EBD(NK)
        IKP0=0
        DO IK=NK-1,2,-1
          IF (EC .LT. EBD(IK)) THEN
            EC=EBD(IK)
            IKP0=IK
          END IF
        END DO
        FP0 = 0
!        WRITE(*,*) 'IKP0=', IKP0, '  size(SIG)=', size(SIG)
        IF (IKP0 .ne. 0) THEN
          FP0 = SIG(IKP0)*TPIINV
        END IF
        ILOW   = MAX (  1 , IKP0-1 )
        ICEN   = MAX (  1 , IKP0   )
        IHGH   = MIN ( NK , IKP0+1 )
        EL     = EBD(ILOW) - EBD(ICEN)
        EH     = EBD(IHGH) - EBD(ICEN)
        XL     = 1./XFR - 1.
        XH     =  XFR - 1.
        XL2    = XL**2
        XH2    = XH**2
        DENOM  = XL*EH - XH*EL
        FP0    = FP0*(1. + 0.5*(XL2*EH - XH2*EL)     &
               / SIGN(MAX(ABS(DENOM),1.E-15),DENOM))
        !
        ET     = ET  + FTE *EBND
        ETX    = ETX + FTE * eABX/CG(NK,ISEA)
        ETY    = ETY + FTE * eABY/CG(NK,ISEA)
        ThrET=10e-8
        IF (ET .gt. ThrET) THEN
!          WRITE(*,*) 'ET/ETX/ETY=', ET, ETX, ETY
          eQuotET=(ETX**2+ETY**2)/ET**2
        ELSE
          eQuotET=100
        END IF
        eTHS  = RADE*SQRT(MAX(0., 2. * ( 1. - SQRT (    &
                MAX(0.,eQuotET) ) ) ) )
        eTHM  = ATAN2(ETY,ETX) * 360 / TPI

        HSIG   = 4. * SQRT ( ET )
        VAR_TOT_NSEA(1, ISEA)=HSIG
        VAR_TOT_NSEA(2, ISEA)=CX(ISEA)
        VAR_TOT_NSEA(3, ISEA)=CY(ISEA)
        VAR_TOT_NSEA(4, ISEA)=WLV(ISEA)
        VAR_TOT_NSEA(5, ISEA)=ZB(ISEA)
        VAR_TOT_NSEA(6, ISEA)=FP0
        VAR_TOT_NSEA(7, ISEA)=eT02
        VAR_TOT_NSEA(8, ISEA)=eTHS
        VAR_TOT_NSEA(9, ISEA)=eTHM
        VAR_TOT_NSEA(10, ISEA)=DW(ISEA)
        VAR_TOT_NSEA(11, ISEA)=U10(ISEA)
        VAR_TOT_NSEA(12, ISEA)=ETX
        VAR_TOT_NSEA(13, ISEA)=ETY
!/SETUP        VAR_TOT_NSEA(14, ISEA)=ZETA_SETUP(ISEA)
        Status(ISEA)=1
      END DO
      !
      ! Now find global arrays
      !
!/MPI      IF (IAPROC .eq. 1) THEN
!/MPI        DO iProc=2,NAPROC
!/MPI          CALL MPI_RECV(rVect,NSEA*nVar,MPI_REAL, iProc-1, 19, MPI_COMM_WCMP, istatus, ierr)
!/MPI          CALL MPI_RECV(rStatus,NSEA,MPI_INTEGER, iProc-1, 23, MPI_COMM_WCMP, istatus, ierr)
!/MPI          DO I=1,NSEA
!/MPI            IF (rStatus(I) .eq. 1) THEN
!/MPI              VAR_TOT_NSEA(:,I)=rVect(:,I)
!/MPI              Status(I)=1
!/MPI            END IF
!/MPI          END DO
!/MPI        END DO
!/MPI      ELSE
!/MPI        CALL MPI_SEND(VAR_TOT_NSEA,NSEA*nVar,MPI_REAL, 0, 19, MPI_COMM_WCMP, ierr)
!/MPI        CALL MPI_SEND(Status,NSEA,MPI_INTEGER, 0, 23, MPI_COMM_WCMP, ierr)
!/MPI      END IF
      allocate(WBACbound(NK, NTH, IWBMNPGL), stat=istat)
      nbexport=0
      DO IP=1,NP
        IP_glob=iplg(IP)
        IF (IOBP_relevant(IP_glob) .eq. 2) THEN
          ISEA=MAPFS(1,IP_glob)
          IF (ISEA .eq. 0) THEN
            WRITE(*,*) 'Error in the assignment value ISEA=0'
            CALL ABORT
          END IF
          CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
          IF (ISPROC .ne. IAPROC) THEN
            WRITE(*,*) 'ISPROC != IAPROC, this is a priori an error'
            CALL ABORT
          END IF
          nbexport=nbexport + 1
          idxbnd=ListIndexes(IP_glob)
          DO IK=1,NK
            DO ITH=1,NTH
              ISP    = ITH + (IK-1)*NTH
              eVal=VA(ISP,JSEA)
              WBACbound(IK,ITH,idxbnd)=eVal
            END DO
          END DO
        END IF
      END DO
!/MPI!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'NAPROC=', NAPROC
!/MPI!/DEBUGNETCDF        FLUSH(740+IAPROC)
!/MPI      IF (GTYPE .eq. UNGTYPE) THEN
!/MPI        allocate(iVect(1), stat=istat)
!/MPI        ListFirst=0
!/MPI        DO IPROC=2,NAPROC
!/MPI          ListFirst(IPROC) = ListFirst(IPROC-1) + ListNPA(IPROC-1)
!/MPI        END DO
!/MPI        IF (IAPROC .eq. 1) THEN
!/MPI          DO IPROC=2,NAPROC
!/MPI            CALL MPI_RECV(iVect,1,MPI_INT, iProc-1, 19, MPI_COMM_WCMP, istatus, ierr)
!/MPI            IF (iVect(1) .gt. 0) THEN
!/MPI              allocate(WBACexch(NK,NTH,iVect(1)), stat=istat)
!/MPI!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Before MPI_RECV, tag 21'
!/MPI!/DEBUGNETCDF        FLUSH(740+IAPROC)
!/MPI              CALL MPI_RECV(WBACexch,NK*NTH*iVect(1),MPI_REAL, iProc-1, 21, MPI_COMM_WCMP, istatus, ierr)
!/MPI!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'After MPI_RECV, tag 21'
!/MPI!/DEBUGNETCDF        FLUSH(740+IAPROC)
!/MPI              NPloc=ListNP(IPROC)
!/MPI              idx=0
!/MPI              DO IP=1,NPloc
!/MPI                IP_glob=ListIPLG(IP + ListFirst(IPROC))
!/MPI                IF (IOBP_relevant(IP_glob) .eq. 2) THEN
!/MPI                  idx=idx + 1
!/MPI                  idxbnd=ListIndexes(IP_glob)
!/MPI                  WBACbound(:,:,idxbnd)=WBACexch(:,:,idx)
!/MPI                END IF
!/MPI              END DO
!/MPI              deallocate(WBACexch)
!/MPI            END IF
!/MPI          END DO
!/MPI        ELSE
!/MPI          iVect(1)=nbexport
!/MPI          CALL MPI_SEND(iVect,1,MPI_INT, 0, 19, MPI_COMM_WCMP, ierr)
!/MPI          IF (nbexport .gt. 0) THEN
!/MPI            allocate(WBACexch(NK,NTH,nbexport), stat=istat)
!/MPI            idx=0
!/MPI            DO IP=1,NP
!/MPI              IP_glob=iplg(IP)
!/MPI              IF (IOBP_relevant(IP_glob) .eq. 2) THEN
!/MPI                ISEA=MAPFS(1,IP_glob)
!/MPI                CALL INIT_GET_JSEA_ISPROC(ISEA, JSEA, ISPROC)
!/MPI                idx=idx+1
!/MPI                DO IK=1,NK
!/MPI                  DO ITH=1,NTH
!/MPI                    ISP    = ITH + (IK-1)*NTH
!/MPI                    WBACexch(IK,ITH,idx) = VA(ISP,JSEA)
!/MPI                  END DO
!/MPI                END DO
!/MPI              END IF
!/MPI            END DO
!/MPI!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Before MPI_SEND, tag 21'
!/MPI!/DEBUGNETCDF        FLUSH(740+IAPROC)
!/MPI            CALL MPI_SEND(WBACexch,NSPEC*nbexport,MPI_REAL, 0, 21, MPI_COMM_WCMP, ierr)
!/MPI!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'After MPI_SEND, tag 21'
!/MPI!/DEBUGNETCDF        FLUSH(740+IAPROC)
!/MPI            deallocate(WBACexch)
!/MPI          END IF
!/MPI        END IF
!/MPI      END IF
      IF (IAPROC .eq. 1) THEN
        nbIncorr=0
        DO I=1,NSEA
          IF (Status(I) .eq. 0) THEN
            nbIncorr=nbIncorr+1
          END IF
        END DO
        IF (nbIncorr .gt. 0) THEN
          WRITE(*,*) '    nbIncorr=', nbIncorr
          WRITE(*,*) 'NSEA - NSEAL=', NSEA - NSEAL
          STOP
        END IF
        !
        ! Initialization des fichiers netcdf
        !
        IF (IsInitDone.eqv..FALSE.) THEN
          IsInitDone=.TRUE.
          iret = nf90_create(TRIM(FILENAME), NF90_CLOBBER, ncid)
          CALL GENERIC_NETCDF_ERROR(CallFct, 1, iret)
          !
          ! Defining the dimensions
          !
          iret = nf90_def_dim(ncid, 'ocean_time', NF90_UNLIMITED, ntime_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 2, iret)
          !
          iret = nf90_def_dim(ncid, 'three', 3, three_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 3, iret)
          !
          iret = nf90_def_dim(ncid, 'mnp', NX, nnode_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 4, iret)
          !
          iret = nf90_def_dim(ncid, 'one', 1, one_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 5, iret)
          !
          iret = nf90_def_dim(ncid, 'fifteen', 15, fifteen_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 6, iret)
          iret = nf90_def_dim(ncid, 'eight', 8, eight_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 7, iret)
          IF (IWBMNPGL .gt. 0) THEN
            iret = nf90_def_dim(ncid, 'IWBMNPGL', IWBMNPGL, iwbmnpgl_dims)
            CALL GENERIC_NETCDF_ERROR(CallFct, 8, iret)
          END IF
          iret = nf90_def_dim(ncid, 'nfreq', NK, nfreq_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 9, iret)
          iret = nf90_def_dim(ncid, 'ndir', NTH, ndir_dims)
          CALL GENERIC_NETCDF_ERROR(CallFct, 10, iret)
          !
          ! Defining the time
          !
          iret=nf90_def_var(ncid,"ocean_time",NF90_DOUBLE,(/ ntime_dims /),var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 11, iret)
          iret=nf90_put_att(ncid,var_id,UNITS,'days since 1858-11-17 00:00:00')
          CALL GENERIC_NETCDF_ERROR(CallFct, 12, iret)
          iret=nf90_put_att(ncid,var_id,"calendar",'gregorian')
          CALL GENERIC_NETCDF_ERROR(CallFct, 13, iret)
          iret=nf90_def_var(ncid,'ocean_time_str',NF90_CHAR,(/ fifteen_dims, ntime_dims/), var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 14, iret)
          iret=nf90_def_var(ncid,'LSPHE',NF90_INT,(/ one_dims/), var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 15, iret)
          !
          ! Defining the variables
          !
          IF (GTYPE .eq. UNGTYPE) THEN
            iret = nf90_def_dim(ncid, 'mne', NTRI, mne_dims)
            CALL GENERIC_NETCDF_ERROR(CallFct, 16, iret)
          END IF
          IF (GTYPE .eq. UNGTYPE) THEN
            IF (FLAGLL) THEN
              iret=nf90_def_var(ncid,"lon",NF90_REAL,(/ nnode_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 17, iret)
              iret=nf90_def_var(ncid,"lat",NF90_REAL,(/ nnode_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 18, iret)
            ELSE
              iret=nf90_def_var(ncid,"x",NF90_REAL,(/ nnode_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 17, iret)
              iret=nf90_def_var(ncid,"y",NF90_REAL,(/ nnode_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 18, iret)
            END IF
            iret=nf90_def_var(ncid,"IOBP",NF90_INT,(/ nnode_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 19, iret)
            iret=nf90_def_var(ncid,"IOBP_WW3",NF90_INT,(/ nnode_dims, ntime_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 20, iret)
            iret=nf90_def_var(ncid,"IOBPD",NF90_INT,(/ ndir_dims, nnode_dims, ntime_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 21, iret)
            iret=nf90_def_var(ncid,"depth",NF90_REAL,(/ nnode_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 22, iret)
            iret=nf90_def_var(ncid,"ele",NF90_INT,(/ three_dims, mne_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 23, iret)
            iret=nf90_def_var(ncid,"MAPSTA",NF90_INT,(/ nnode_dims, ntime_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 24, iret)
            nx_dims = nnode_dims
            ny_dims = -1
          ELSE
            iret = nf90_def_dim(ncid, 'nx', NX, nx_dims)
            CALL GENERIC_NETCDF_ERROR(CallFct, 25, iret)
            iret = nf90_def_dim(ncid, 'ny', NY, ny_dims)
            CALL GENERIC_NETCDF_ERROR(CallFct, 26, iret)
            !
            IF (FLAGLL) THEN
              iret=nf90_def_var(ncid,"lon",NF90_REAL,(/ ny_dims, nx_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 27, iret)
              iret=nf90_def_var(ncid,"lat",NF90_REAL,(/ ny_dims, nx_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 28, iret)
            ELSE
              iret=nf90_def_var(ncid,"x",NF90_REAL,(/ ny_dims, nx_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 27, iret)
              iret=nf90_def_var(ncid,"y",NF90_REAL,(/ ny_dims, nx_dims /),var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 28, iret)
            END IF
            iret=nf90_def_var(ncid,"MAPSTA",NF90_INT,(/ ny_dims, nx_dims, ntime_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 29, iret)
          END IF
          CALL NETCDF_QAD_DEFINE_VAR("HS", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("UsurfCurr", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("VsurfCurr", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("WATLEV", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("ZB", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("WINDMAG", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("DW", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("FP", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("TM02", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("DSPR", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("DM", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("ETOTC", nx_dims, ny_dims, ntime_dims, ncid)
          CALL NETCDF_QAD_DEFINE_VAR("ETOTS", nx_dims, ny_dims, ntime_dims, ncid)
!/SETUP          CALL NETCDF_QAD_DEFINE_VAR("ZETA_SETUP", nx_dims, ny_dims, ntime_dims, ncid)
          !
          IF ((GTYPE .eq. UNGTYPE).and.(IWBMNPGL.gt.0)) THEN
            iret=nf90_def_var(ncid,"SPPARM",NF90_REAL,(/ eight_dims, iwbmnpgl_dims, ntime_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 30, iret)
            iret=nf90_def_var(ncid,"WBAC",NF90_REAL,(/ nfreq_dims, ndir_dims, iwbmnpgl_dims, ntime_dims /),var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 31, iret)
          END IF
          iret=nf90_def_var(ncid,"ocean_idx",NF90_INT,(/ ntime_dims /),var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 32, iret)
          iret=nf90_close(ncid)
          CALL GENERIC_NETCDF_ERROR(CallFct, 33, iret)
          !
          ! Constant writes
          !
          iret=nf90_open(TRIM(FILENAME), nf90_write, ncid)
          CALL GENERIC_NETCDF_ERROR(CallFct, 34, iret)
          !
          IF (FLAGLL) THEN
            eIdx(1)=1
          ELSE
            eIdx(1)=0
          END IF
          iret=nf90_inq_varid(ncid, "LSPHE", var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 35, iret)
          iret=nf90_put_var(ncid,var_id,eIdx,start = (/1/), count = (/1/))
          CALL GENERIC_NETCDF_ERROR(CallFct, 36, iret)
          !
          IF (GTYPE .eq. UNGTYPE) THEN
            IF (FLAGLL) THEN
              iret=nf90_inq_varid(ncid, "lon", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 37, iret)
              iret=nf90_put_var(ncid,var_id,XYB(:,1),start = (/1/), count = (/NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 38, iret)
              !
              iret=nf90_inq_varid(ncid, "lat", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 39, iret)
              iret=nf90_put_var(ncid,var_id,XYB(:,2),start = (/1/), count = (/NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 40, iret)
            ELSE
              iret=nf90_inq_varid(ncid, "x", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 37, iret)
              iret=nf90_put_var(ncid,var_id,XYB(:,1),start = (/1/), count = (/NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 38, iret)
              !
              iret=nf90_inq_varid(ncid, "y", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 39, iret)
              iret=nf90_put_var(ncid,var_id,XYB(:,2),start = (/1/), count = (/NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 40, iret)
            END IF
            !
            iret=nf90_inq_varid(ncid, "IOBP", var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 41, iret)
            iret=nf90_put_var(ncid,var_id,IOBP_relevant,start = (/1/), count = (/NX/))
            CALL GENERIC_NETCDF_ERROR(CallFct, 42, iret)
            !
            iret=nf90_inq_varid(ncid, "depth", var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 43, iret)
            DEPTHwrite=-XYB(:,3)
            iret=nf90_put_var(ncid,var_id,DEPTHwrite,start = (/1/), count = (/NX/))
            CALL GENERIC_NETCDF_ERROR(CallFct, 44, iret)
            !
            ALLOCATE(INEtotal(3, NTRI))
            DO J=1,3
              INEtotal(J,:)=TRIGP(:,J)
            END DO
            iret=nf90_inq_varid(ncid, "ele", var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 53, iret)
            iret=nf90_put_var(ncid,var_id,INEtotal,start = (/1, 1/), count = (/3, NTRI/))
            CALL GENERIC_NETCDF_ERROR(CallFct, 54, iret)
            DEALLOCATE(INEtotal)
          ELSE
            IF (FLAGLL) THEN
              iret=nf90_inq_varid(ncid, "lon", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 47, iret)
              iret=nf90_put_var(ncid,var_id,XGRD,start = (/1, 1/), count = (/NY, NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 48, iret)
              !
              iret=nf90_inq_varid(ncid, "lat", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 49, iret)
              iret=nf90_put_var(ncid,var_id,YGRD,start = (/1, 1/), count = (/NY, NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 50, iret)
            ELSE
              iret=nf90_inq_varid(ncid, "x", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 47, iret)
              iret=nf90_put_var(ncid,var_id,XGRD,start = (/1, 1/), count = (/NY, NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 48, iret)
              !
              iret=nf90_inq_varid(ncid, "y", var_id)
              CALL GENERIC_NETCDF_ERROR(CallFct, 49, iret)
              iret=nf90_put_var(ncid,var_id,YGRD,start = (/1, 1/), count = (/NY, NX/))
              CALL GENERIC_NETCDF_ERROR(CallFct, 50, iret)
            END IF
          END IF
          !
          iret=nf90_close(ncid)
          CALL GENERIC_NETCDF_ERROR(CallFct, 55, iret)
        END IF
        !
        ! Now writing the results
        !
        iret=nf90_open(TRIM(FILENAME), nf90_write, ncid)
        CALL GENERIC_NETCDF_ERROR(CallFct, 56, iret)
        iret=nf90_inquire(ncid, unlimitedDimId = irec_dim)
        CALL GENERIC_NETCDF_ERROR(CallFct, 57, iret)
        iret=nf90_inquire_dimension(ncid, irec_dim,len = recs_his)
        CALL GENERIC_NETCDF_ERROR(CallFct, 58, iret)
        !
        CALL WW3_TO_JD(TIME0_NETCDF_QAD, eJD)
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'OUTPUT_NETCDF_QUICK_AND_DIRTY'
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'QAD: eJD=', eJD
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'QAD: DTG=', DTG
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'QAD: recs_his=', recs_his
        !
        pos=recs_his+1
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'pos=', pos
!/DEBUGNETCDF        FLUSH(740+IAPROC)
        eVar(1)=eTime
        iret=nf90_inq_varid(ncid, "ocean_time", var_id)
        CALL GENERIC_NETCDF_ERROR(CallFct, 59, iret)

        iret=nf90_put_var(ncid,var_id,eVar,start = (/pos/), count = (/ 1 /))
        CALL GENERIC_NETCDF_ERROR(CallFct, 60, iret)
        !
        CALL MJD2CT(eTime, eTimeStr)
        iret=nf90_inq_varid(ncid, 'ocean_time_str', var_id)
        CALL GENERIC_NETCDF_ERROR(CallFct, 61, iret)
        DO i=1,15
          eChar=eTimeStr(i:i)
          iret=nf90_put_var(ncid,var_id,eChar,start=(/i, pos/) )
          CALL GENERIC_NETCDF_ERROR(CallFct, 62, iret)
        END DO
        !
        eIdx(1)=pos
        iret=nf90_inq_varid(ncid, "ocean_idx", var_id)
        CALL GENERIC_NETCDF_ERROR(CallFct, 63, iret)
        iret=nf90_put_var(ncid,var_id,eIdx,start = (/pos/), count = (/ 1 /))
        CALL GENERIC_NETCDF_ERROR(CallFct, 64, iret)
        !
        ! Boundary and company arrays writes:
        !
        IF (GTYPE .eq. UNGTYPE) THEN
          iret=nf90_inq_varid(ncid, "MAPSTA", var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 45, iret)
          iret=nf90_put_var(ncid,var_id,MAPSTA(1,:),start = (/1, pos/), count = (/NX, 1/))
          CALL GENERIC_NETCDF_ERROR(CallFct, 46, iret)
        ELSE
          iret=nf90_inq_varid(ncid, "MAPSTA", var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 51, iret)
          iret=nf90_put_var(ncid,var_id,MAPSTA,start = (/1, 1, pos/), count = (/NY, NX, 1/))
          CALL GENERIC_NETCDF_ERROR(CallFct, 52, iret)
        END IF
        IF (GTYPE .eq. UNGTYPE) THEN
          iret=nf90_inq_varid(ncid, "IOBP_WW3", var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 65, iret)
          iret=nf90_put_var(ncid,var_id,IOBP,start = (/1, pos/), count = (/ NX, 1 /))
          CALL GENERIC_NETCDF_ERROR(CallFct, 66, iret)
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'sum(IOBP)=', sum(IOBP)
!/DEBUGNETCDF        FLUSH(740+IAPROC)
          !
          iret=nf90_inq_varid(ncid, "IOBPD", var_id)
          CALL GENERIC_NETCDF_ERROR(CallFct, 67, iret)
          iret=nf90_put_var(ncid,var_id,IOBPD,start = (/1, 1, pos/), count = (/ NTH, NX, 1 /))
          CALL GENERIC_NETCDF_ERROR(CallFct, 68, iret)
        END IF
        !
        CALL NETCDF_QAD_PUT_VAR("HS", 1, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("UsurfCurr", 2, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("VsurfCurr", 3, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("WATLEV", 4, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("ZB", 5, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("FP", 6, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("TM02", 7, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("DSPR", 8, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("DM", 9, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("DW", 10, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("WINDMAG", 11, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("ETOTC", 12, nVar, VAR_TOT_NSEA, ncid, pos)
        CALL NETCDF_QAD_PUT_VAR("ETOTS", 13, nVar, VAR_TOT_NSEA, ncid, pos)
!/SETUP        CALL NETCDF_QAD_PUT_VAR("ZETA_SETUP", 14, nVar, VAR_TOT_NSEA, ncid, pos)
        IF (GTYPE .eq. UNGTYPE) THEN
          DO IWB=1,IWBMNPGL
            IX=ListIdx(IWB)
            ISEA=MAPFS(1,IX)
            SPPARM(1,IWB)=VAR_TOT_NSEA(1,ISEA)
            SPPARM(3,IWB)=VAR_TOT_NSEA(9,ISEA)
            SPPARM(4,IWB)=VAR_TOT_NSEA(8,ISEA)
            SPPARM(6,IWB)=1
            SPPARM(7,IWB)=0.2
            SPPARM(8,IWB)=3.3
            IF (DOPEAK) THEN
              SPPARM(2,IWB)=1/MAX(THR_FP0, VAR_TOT_NSEA(6,ISEA))
              SPPARM(5,IWB)=2
            ELSE
              SPPARM(2,IWB)=VAR_TOT_NSEA(7,ISEA)
              SPPARM(5,IWB)=-2
            END IF
          END DO
          !
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Before writing of SPPARM'
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'pos=', pos
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'IWBMNPGL=', IWBMNPGL
!/DEBUGNETCDF        FLUSH(740+IAPROC)
          IF (IWBMNPGL .gt. 0) THEN
            iret=nf90_inq_varid(ncid, "SPPARM", var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 69, iret)
            iret=nf90_put_var(ncid,var_id,SPPARM,start = (/1,1,pos/), count = (/ 8,IWBMNPGL /))
            CALL GENERIC_NETCDF_ERROR(CallFct, 70, iret)
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'After writing of SPPARM'
!/DEBUGNETCDF        FLUSH(740+IAPROC)
            !
            iret=nf90_inq_varid(ncid, "WBAC", var_id)
            CALL GENERIC_NETCDF_ERROR(CallFct, 71, iret)
            iret=nf90_put_var(ncid,var_id,WBACbound,start = (/1,1,1,pos/), count = (/ NK,NTH,IWBMNPGL /))
            CALL GENERIC_NETCDF_ERROR(CallFct, 72, iret)
          END IF
        END IF
        !
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'HS values'
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'HS(min/max)=', minval(VAR_TOT_NSEA(1,:)), maxval(VAR_TOT_NSEA(1,:))
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Ucurr(min/max)=', minval(VAR_TOT_NSEA(2,:)), maxval(VAR_TOT_NSEA(2,:))
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Vcurr(min/max)=', minval(VAR_TOT_NSEA(3,:)), maxval(VAR_TOT_NSEA(3,:))
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'sum(varWrite)=', sum(varWrite)
!/DEBUGNETCDF        FLUSH(740+IAPROC)
        !
        iret=nf90_close(ncid)
        CALL GENERIC_NETCDF_ERROR(CallFct, 73, iret)
      END IF
      deallocate(ListIdx, SPPARM)
!/DEBUGNETCDF        WRITE(740+IAPROC,*) 'Leaving the NETCDF output routine'
!/DEBUGNETCDF        FLUSH(740+IAPROC)
      END SUBROUTINE
!**********************************************************************
!*                                                                    *
!**********************************************************************
      END MODULE W3NETCDF
