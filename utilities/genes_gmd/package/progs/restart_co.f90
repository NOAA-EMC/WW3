!/ ------------------------------------------------------------------- /
      PROGRAM RESTART
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         15-Nov-2008 |
!/                  +-----------------------------------+
!/
!/    15-Nov-2008 : Origination.
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Combine two retsrta files into one.
!
!  2. Method :
!
!     Reading files, assuming single spectrum, then combine.
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!     - See error escape locations at end of code.
!
!  7. Remarks :
!
!  8. Structure :
!
!     - See source code.
!
!  9. Switches :
!
!     - This is a true FORTRAN file without switches.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!     USE CONSTANTS
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local PARAMETER statements
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NFR, NTH, NSPEC, LRECL, IERR
      INTEGER, PARAMETER      :: LRB = 4
      REAL, ALLOCATABLE       :: SPEC1(:), SPEC2(:)
      CHARACTER(LEN=11)       :: FNAME
!/
!/ ------------------------------------------------------------------- /
!/
!     DATA T0 / 19680606 , 0 /
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 0.  Initialization
!
      WRITE (*,900)
!
      READ (*,*) NFR, NTH
      WRITE (*,901) NFR, NTH
      NSPEC  = NFR * NTH
      LRECL  = LRB * NSPEC
!
      FNAME  = 'restart.1'
      WRITE (*,902) FNAME
      OPEN (10,FILE=FNAME,FORM='UNFORMATTED',STATUS='OLD',            &
            ACCESS='DIRECT',RECL=LRECL,ERR=801,IOSTAT=IERR)
!
      FNAME  = 'restart.2'
      WRITE (*,902) FNAME
      OPEN (11,FILE=FNAME,FORM='UNFORMATTED',STATUS='OLD',            &
            ACCESS='DIRECT',RECL=LRECL,ERR=801,IOSTAT=IERR)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read spectra
!
      ALLOCATE ( SPEC1(NSPEC), SPEC2(NSPEC) )
      WRITE (*,*)
!
      FNAME  = 'restart.1'
      WRITE (*,910) FNAME
      READ (10,REC=3,ERR=802,IOSTAT=IERR) SPEC1
!
      FNAME  = 'restart.2'
      WRITE (*,910) FNAME
      READ (11,REC=3,ERR=802,IOSTAT=IERR) SPEC2
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Combining spectra
!
      WRITE (*,920)
      SPEC1  = SPEC1 + SPEC2
      SPEC2  = SPEC1
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Read spectra
!
      FNAME  = 'restart.1'
      WRITE (*,930) FNAME
      WRITE (10,REC=3,ERR=803,IOSTAT=IERR) SPEC1
!
      FNAME  = 'restart.2'
      WRITE (*,930) FNAME
      WRITE (11,REC=3,ERR=803,IOSTAT=IERR) SPEC2
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Normal end of program
!
      GOTO 888
!
! Error scape locations
!
  801 CONTINUE
      WRITE (*,1001) FNAME, IERR
      STOP
!
  802 CONTINUE
      WRITE (*,1002) FNAME, IERR
      STOP
!
  803 CONTINUE
      WRITE (*,1003) FNAME, IERR
      STOP
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' restart_co.x: combine restarts :'/                   &
               ' --------------------------------')
  901 FORMAT ( '    Spectral size ',2I4)
  902 FORMAT ( '    Opening file  ',A)
!
  910 FORMAT ( '    Reading file  ',A)
!
  920 FORMAT (/'    Combining spectra ...'/)
!
  930 FORMAT ( '    Writing file  ',A)
!
  999 FORMAT (/' End of restart_co.x'/)
!
 1001 FORMAT (/' *** ERROR IN OPENING FILE ***'/                      &
               '     File name :',A/                                  &
               '     IOSTAT    : ',I8)
!
 1002 FORMAT (/' *** ERROR IN READING FILE ***'/                      &
               '     File name :',A/                                  &
               '     IOSTAT    : ',I8)
!
 1003 FORMAT (/' *** ERROR IN WRITING FILE ***'/                      &
               '     File name :',A/                                  &
               '     IOSTAT    : ',I8)
!
!/
!/ End of RESTART ---------------------------------------------------- /
!/
      END PROGRAM RESTART
