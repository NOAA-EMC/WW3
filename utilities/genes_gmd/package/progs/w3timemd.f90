!/ ------------------------------------------------------------------- /
      MODULE W3TIMEMD
!/
!/    Copyright 2009-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!$$$  MODULE DOCUMENTATION BLOCK
!                .      .    .                                       .
! MODULE: WWATCH3_TIME_MOD   Time management routines.
!   PRGMMR: H.L. Tolman      ORG: W/NP21      DATE: 2001-01-05
!
! ABSTRACT: This module contains a set of subroutines to deal with
!   date and time, using a four digit year and a integer array of
!   size 2 with one element containing the data and the other the
!   time.
!
! PROGRAM HISTORY LOG:
! 2001-01-05  Tolman      Origination.
!
! PUBLIC VARIABLES :      See documentation below.
! PUBLIC TYPES :          See documentation below.
! PUBLIC SUBPROGRAMS :    See documentation below.
!
! REMARKS:                None.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Portable code
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jan-2001 |
!/                  +-----------------------------------+
!/
!  1. Purpose :
!
!     Routines for management of date and time.
!
!  2. Variables and types :
!
!     None.
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      TICK21    Subr. Public   Increment a date and time array with
!                               a given number of seconds.
!      IYMD21    I.F.  TICK21   Date increment function.
!      DSEC21    R.F.  Public   Calculate the difference in seconds
!                               between two data/time arrays.
!      MYMD21    I.F.  DSEC21   Julian date function.
!      STME21    Subr. Public   Converts integer time to string.
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
!
!  6. Switches :
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!
      PUBLIC
!
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE TICK21 ( TIME, DTIME )
!/
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    TICK21      Increment date and time
!   PRGMMR: H.L. Tolman      ORG: W/NP21     DATE: 1999-11-29
!
! ABSTRACT: Increment integer TIME(2) in yyyymmdd hhmmss format by
!   a given number of seconds.
!
! PROGRAM HISTORY LOG:
! 1993-03-29  Tolman      Origination.
! 1998-10-29  Chen/Tolman Adding docblock.
! 1999-11-29  Tolman      FORTRAN 90 version.
!
! USAGE: See dodumentation below.
!
! REMARKS: See dodumentation below.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Portable code.
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-Nov-1999 |
!/                  +-----------------------------------+
!/                                Based on TICK of the GLA GCM.
!/
!/    23-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Updates time information, DTIME=0 converts to "legal" time.
!     Goes into the 21st century.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
!                          (2) Current time in HHMMSS format.
!       DTIME   Real   I   Time step in seconds.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      IYMD21    Func. Internal Increment date in YYYYMMDD format.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any other routine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(INOUT)  :: TIME(2)
      REAL, INTENT(IN)        :: DTIME
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NYMD, NHMS, NSEC
!/
!/ ------------------------------------------------------------------- /
!/
!
! Zero increment: get "legal" date
!
      NYMD   = TIME(1)
      NHMS   = TIME(2)
      IF (DTIME.EQ.0.) THEN
          NYMD = IYMD21 (NYMD,-1)
          NYMD = IYMD21 (NYMD, 1)
        END IF
!
! Convert and increment time :
!
      NSEC = NHMS/10000*3600 + MOD(NHMS,10000)/100* 60 +        &
             MOD(NHMS,100) + NINT(DTIME)
!
! Check change of date :
!
  100 CONTINUE
      IF (NSEC.GE.86400)  THEN
          NSEC = NSEC - 86400
          NYMD = IYMD21 (NYMD,1)
          GOTO 100
        END IF
!
  200 CONTINUE
      IF (NSEC.LT.00000)  THEN
          NSEC = 86400 + NSEC
          NYMD = IYMD21 (NYMD,-1)
          GOTO 200
        END IF
!
      NHMS = NSEC/3600*10000 + MOD(NSEC,3600)/60*100 + MOD(NSEC,60)
!
      TIME(1) = NYMD
      TIME(2) = NHMS
!
      RETURN
!/
!/ Internal function IYMD21 ------------------------------------------ /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      INTEGER FUNCTION IYMD21 ( NYMD ,M )
!/
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    IYMD21      Increment date by +- 1
!   PRGMMR: H.L. Tolman      ORG: W/NP21     DATE: 1999-11-29
!
! ABSTRACT: Increment date by +/- 1
!
! PROGRAM HISTORY LOG:
! 1998-10-18  Tolman      Origination.
! 1998-10-29  Chen/Tolman Adding docblock.
! 1999-11-29  Tolman      Conversion to FORTRAN 90.
!
! USAGE: See documentation below.
!
! REMARKS: See documentation below.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Portable code
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-Oct-1998 |
!/                  +-----------------------------------+
!/                                Based on INCYMD of the GLA GCM.
!/
!/    18-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Increment date in YYYYMMDD format by +/- 1 day.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Old date in YYMMDD format.
!       M       Int.   I   +/- 1 (Day adjustment)
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
!     Any subroutine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NYMD, M
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NY, NM, ND
      INTEGER, SAVE           :: NDPM(12)
      DATA     NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
      LOGICAL                 :: LEAP
!/
!/ ------------------------------------------------------------------- /
!/
!
! "Unpack" and increment date :
!
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      NM   = MIN ( 12 , MAX(1,NM) )
      ND   = MOD(NYMD,100) + M
      LEAP = MOD(NY,400).EQ.0 .OR.                              &
              ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
!
! M = -1, change month if necessary :
!
      IF (ND.EQ.0) THEN
          NM   = NM - 1
          IF (NM.EQ.0) THEN
              NM   = 12
              NY   = NY - 1
            ENDIF
          ND   = NDPM(NM)
          IF (NM.EQ.2 .AND. LEAP)  ND = 29
        END IF
!
! M = 1, leap year
!
      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP)  GO TO 20
!
!        next month
!
      IF (ND.GT.NDPM(NM)) THEN
          ND = 1
          NM = NM + 1
          IF (NM.GT.12) THEN
              NM = 1
              NY = NY + 1
          ENDIF
        END IF
!
   20 CONTINUE
      IYMD21 = NY*10000 + NM*100 + ND
!
      RETURN
!/
!/ End of IYMD21 ----------------------------------------------------- /
!/
      END FUNCTION IYMD21
!/
!/ End of TICK21 ----------------------------------------------------- /
!/
      END SUBROUTINE TICK21
!/ ------------------------------------------------------------------- /
      REAL FUNCTION DSEC21 ( TIME1, TIME2 )
!/
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    DSEC21      Time difference in seconds
!   PRGMMR: H.L. Tolman      ORG: W/NP21     DATE: 2001-01-05
!
! ABSTRACT: Calculate time difference in seconds between two times
!   in yyyymmdd hhmmss format.
!
! PROGRAM HISTORY LOG:
! 1993-03-29  Tolman      Origination.
! 1998-10-29  Chen/Tolman Adding docblock.
! 1999-11-29  Tolman      FORTRAN 90 version.
! 2001-01-05  Tolman      Y2K leap year fix ported from operational.
!
! USAGE: See documentation below.
!
! REMARKS: See documentation below.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Portable code.
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jan-2001 |
!/                  +-----------------------------------+
!/
!/    23-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    05-Jan-2001 : Y2K leap year error correction.     ( version 2.05 )
!/
!/
!  1. Purpose :
!
!     Calculate the time difference in seconds between two times in
!     YYMMD HHMMMSS formats.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIMEn   I.A.   I   Times, TIMEn(1) is date in YYYYMMDD format,
!                          TIMEn(2) is time in HHMMSS format.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      MYMD21    Func. Internal Calculate Julian date.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any routine.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: TIME1(2), TIME2(2)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NY1, ND1, NY2, ND2, NS1, NS2, NS,   &
                                 ND, NST
!/
!/ ------------------------------------------------------------------- /
!/
!
! Convert dates and times :
!
      NY1    = TIME1(1) / 10000
      ND1    = MYMD21 ( TIME1(1) )
      NS1    = TIME1(2)/10000*3600 + MOD(TIME1(2),10000)/100*60 + &
               MOD(TIME1(2),100)
!
      NY2    = TIME2(1) / 10000
      ND2    = MYMD21 ( TIME2(1) )
      NS2    = TIME2(2)/10000*3600 + MOD(TIME2(2),10000)/100*60 + &
               MOD(TIME2(2),100)
!
! Number of days and seconds in difference :
!
      ND     = ND2 - ND1
!
      IF ( NY1 .NE. NY2 ) THEN
          NST    = SIGN ( 1 , NY2-NY1 )
  100     CONTINUE
          IF (NY1.EQ.NY2) GOTO 200
          IF (NST.GT.0) THEN
              NY2    = NY2 - 1
              ND     = ND  + MYMD21 ( NY2*10000 + 1231 )
            ELSE
              ND     = ND  - MYMD21 ( NY2*10000 + 1231 )
              NY2    = NY2 + 1
            ENDIF
          GOTO 100
  200     CONTINUE
        END IF
!
      NS     = NS2 - NS1
!
! Output of time difference :
!
      DSEC21 = REAL(NS) + 86400.*REAL(ND)
!
      RETURN
!/
!/ Internal function MYMD21 ------------------------------------------ /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      INTEGER FUNCTION MYMD21 ( NYMD )
!/
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    MYMD21      Calculate Julian date
!   PRGMMR: H.L. Tolman      ORG: W/NP21     DATE: 1999-11-29
!
! ABSTRACT: Calculate Julian date from date in yyyymmdd format.
!
! PROGRAM HISTORY LOG:
! 1998-10-19  Tolman      Origination.
! 1998-10-29  Chen/Tolman Adding docblock.
! 1999-11-29  Tolman      Conversion to FORTRAN 90.
!
! USAGE: See documentation below.
!
! REMARKS: See documentation below.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Portable code
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-Oct-1998 |
!/                  +-----------------------------------+
!/                                Based on MODYMD of the GLA GCM.
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Convert date in YYMMDD format to julian date.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Date in YYMMDD format.
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
!     Any subroutine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NYMD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NY, NM, ND
      INTEGER, SAVE           :: NDPM(12)
      DATA    NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
      LOGICAL                 :: LEAP
!/
!/ ------------------------------------------------------------------- /
!/
!
! "Unpack" and increment date :
!
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      ND   = MOD(NYMD,100)
      LEAP = MOD(NY,400).EQ.0 .OR.                              &
              ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
!
! Loop over months :
!
      IF (NM.GT.2 .AND. LEAP)  ND = ND + 1
!
   40 CONTINUE
      IF (NM.LE.1)  GO TO 60
      NM = NM - 1
      ND = ND + NDPM(NM)
      GO TO 40
!
   60 CONTINUE
      MYMD21 = ND
!
      RETURN
!/
!/ End of MYMD21 ----------------------------------------------------- /
!/
      END FUNCTION MYMD21
!/
!/ End of DSEC21 ----------------------------------------------------- /
!/
      END FUNCTION DSEC21
!/ ------------------------------------------------------------------- /
      SUBROUTINE STME21 ( TIME , DTME21 )
!/
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM:    STME21      Generates date/time string.
!   PRGMMR: H.L. Tolman      ORG: W/NP21     DATE: 1999-11-23
!
! ABSTRACT: Generates a character date/time string form integer data.
!
! PROGRAM HISTORY LOG:
! 1993-06-21  Tolman      Origination.
! 1998-10-29  Chen/Tolman Adding docblock.
! 1999=11=23  Tolman      FORTRAN 90 conversion.
!
! USAGE: See documentation blow.
!
! REMARKS: See documentation blow.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Portable code
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         23-Nov-1999 |
!/                  +-----------------------------------+
!/
!/    21-Jun-1993 : Final FORTRAN 77                    ( version 1.18 )
!/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Converts time to more readable string.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIME    I.A.  I   Time in YYYYMMDD HHMMSS format.
!                         TIME(1) < 0 indicates that time is not set.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       None.
!
!  5. Called by :
!
!       Any subroutine/program.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: TIME(2)
      CHARACTER, INTENT(OUT)  :: DTME21*23
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IY, IMO, ID, IH, IMI, IS
!/
!/ ------------------------------------------------------------------- /
!/
      IF ( TIME(1) .LT. 0 ) THEN
          DTME21 = ' date and time not set.'
        ELSE
          IY     = TIME(1) / 10000
          IMO    = MOD(TIME(1),10000) / 100
          ID     = MOD(TIME(1),100)
          IH     = TIME(2) / 10000
          IMI    = MOD(TIME(2),10000) / 100
          IS     = MOD(TIME(2),100)
          WRITE (DTME21,900) IY, IMO, ID, IH, IMI, IS
        ENDIF
!
      RETURN
!
! Formats
!
  900 FORMAT (I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,' UTC')
!/
!/ End of STME21 ----------------------------------------------------- /
!/
      END SUBROUTINE STME21
!/
!/ End of module W3TIMEMD -------------------------------------------- /
!/
      END MODULE W3TIMEMD
