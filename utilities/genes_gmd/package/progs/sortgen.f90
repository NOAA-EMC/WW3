!/ ------------------------------------------------------------------- /
      PROGRAM SORTGEN
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         14-Jan-2009 |
!/                  +-----------------------------------+
!/
!/    22-Dec-2008 : Origination.
!/    23-Dec-2008 : Ading file pop_clean.
!/    14-Jan-2009 : Increase accuracy of error.
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Sort generation by error value.
!
!  2. Method :
!
!     Read file record by record and sort on the fly.
!
!  3. Parameters :
!
!  4. Subroutines used :
!     
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      QSORT     Subr  QTOOLSMD Sort set of quadruplets.
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
      USE QTOOLSMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NQ, NPOP, NDSI, NDSO, NDSC, IERR,    &
                                 LGEN, J, I, IX
      REAL, ALLOCATABLE       :: DATA (:,:), MEMBER(:)
      LOGICAL                 :: FLAGOK
!/
!/ ------------------------------------------------------------------- /
!
! 0.  Initialization
!
      READ (*,*) NQ, NPOP
      WRITE (*,900) NQ, NPOP
      NDSI    = 10
      NDSO    = 50
      NDSC    = 51
!
      WRITE (*,901)
      OPEN (NDSI,FILE='population',STATUS='OLD',ERR=801,IOSTAT=IERR)
      WRITE (*,902)
      OPEN (NDSO,FILE='population.sort',ERR=801,IOSTAT=IERR)
      WRITE (*,903)
      OPEN (NDSC,FILE='pop_clean',ERR=801,IOSTAT=IERR)
!
      LGEN   = 5*NQ + 2
      ALLOCATE ( DATA(0:LGEN,NPOP) , MEMBER(0:LGEN))
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Loop to read data
!
      WRITE (*,910)
!
      DO J=1, NPOP
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Read data and check error
!
        READ (NDSI,*,END=802,ERR=802,IOSTAT=IERR) MEMBER
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Sort on the fly
!
        IF ( J .EQ. 1 ) THEN
            DATA(:,1) = MEMBER
          ELSE
            DO I=1, J-1
              IF ( MEMBER(0) .LT. DATA(0,I) ) EXIT
              END DO
          IX     = I
            DO I=J, IX+1, -1
              DATA(:,I) = DATA(:,I-1)
              END DO
            DATA(:,IX) = MEMBER
          END IF
!
! ... End loop 1.
!
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4.  Write to output files
!
      WRITE (*,940)
!
      DO J=1, NPOP
!
        IF ( NQ .EQ. 1 ) THEN 
            WRITE (NDSO,941) DATA(0,J), DATA(1:7,J)
          ELSE
            WRITE (NDSO,942) DATA(0,J), DATA(1:5,J)
            DO I=2, NQ-1
              WRITE (NDSO,943) DATA((I-1)*5+1:I*5,J)
              END DO
            WRITE (NDSO,944) DATA((NQ-1)*5+1:NQ*5+2,J)
          END IF
!
        MEMBER = DATA (:,J)
!
        CALL QSORT ( NQ, LGEN, MEMBER, MEMBER, FLAGOK )
!
        IF ( NQ .EQ. 1 ) THEN 
            WRITE (NDSC,941) MEMBER(0), MEMBER(1:7)
          ELSE
            WRITE (NDSC,942) MEMBER(0), MEMBER(1:5)
            DO I=2, NQ-1
              WRITE (NDSC,943) MEMBER((I-1)*5+1:I*5)
              END DO
            WRITE (NDSC,944) MEMBER((NQ-1)*5+1:NQ*5+2)
          END IF
!
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  Normal end of program
!
      GOTO 888
!
! Error scape locations
!
  801 CONTINUE
      WRITE (*,1001) IERR
      STOP 1001
!
  802 CONTINUE
      WRITE (*,1002) IERR
      STOP 1002
!
! 830 CONTINUE
!     WRITE (*,1030) FNAME, IERR
!     STOP 1030
!
! 831 CONTINUE
!     WRITE (*,1031) FNAME, IERR
!     STOP 1031
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' sortgen.x: check errors in generation :     '/       &
               ' --------------------------------------------'/       &
               '    Number of quadruplets  :',I4/                     &
               '    Sizde of population    :',I4)
  901 FORMAT ( '    Opening input file ...')
  902 FORMAT ( '    Opening output file ...')
  903 FORMAT ( '    Opening second output file ...')
  910 FORMAT ( '    Sorting data on the fly ...')
  940 FORMAT ( '    Writing data to file ...')
  941 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  942 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3)
  943 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3)
  944 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  999 FORMAT (/' End of sortgen.x')
!
 1001 FORMAT (/'   *** ERROR IN OPENING FILE ***'/                    &
               '       IOSTAT = ',I10/)
 1002 FORMAT (/'   *** ERROR IN READING INPUT FILE ***'/              &
               '       IOSTAT = ',I10/)
!1030 FORMAT (/'   *** ERROR IN OPENING FILE ',A,' ***'/              &
!              '       IOSTAT = ',I10/)
!1031 FORMAT (/'   *** ERROR IN READING FILE ',A,' ***'/              &
!              '       IOSTAT = ',I10/)
!/
!/ End of SORTGEN ---------------------------------------------------- /
!/
      END PROGRAM SORTGEN
