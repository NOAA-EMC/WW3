!/ ------------------------------------------------------------------- /
      PROGRAM GMEMBER
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         10-Sep-2010 |
!/                  +-----------------------------------+
!/
!/    29-Dec-2008 : Origination.
!/    14-Jan-2009 : Increase accuracy of error.
!/    05-Mar-2010 : Add SORTED option.
!/    13-Aug-2010 : Move from !/NLX to !/NL3.
!/    10-Sep-2010 : Switch SORTED off to avoid issues with sorting
!/                  and non-homogeneous masking.
!/
!/    Copyright 2008-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Extract member information from population.
!
!  2. Method :
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      QTEST     L.F.  QTOOLSMD Test validity of quadruplet layout.
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
      INTEGER                 :: IERR, IPOP, NDSI, NDSO, NQ, LGEN, I, &
                                 IQ, JQ, MQ
      REAL                    :: L, M, DT
      REAL, ALLOCATABLE       :: MEMBER(:)
      LOGICAL                 :: SORTED = .FALSE.
!/
!/ ------------------------------------------------------------------- /
!
! 0.  Initialization
!
      READ (*,*,ERR=801,IOSTAT=IERR) IPOP, NQ
      WRITE (*,900) IPOP, NQ
      NDSI    = 10
      NDSO    = 50
      LGEN   = 5*NQ + 2
      ALLOCATE ( MEMBER(0:LGEN) )
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from input file
!
      IF ( SORTED ) THEN
          OPEN (NDSI,FILE='pop_clean',STATUS='OLD',ERR=801,IOSTAT=IERR)
        ELSE
          OPEN (NDSI,FILE='population',STATUS='OLD',ERR=801,IOSTAT=IERR)
        END IF
!
      DO I=1, IPOP
        READ (NDSI,*,ERR=802,IOSTAT=IERR) MEMBER
        END DO
!
      IF ( SORTED ) MEMBER(0) = 999.999
!
      CLOSE (NDSI) 
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Generate base output file
!
      WRITE (*,920)
!
      OPEN (NDSO,FILE='member')
!
      IF ( NQ .EQ. 1 ) THEN
          WRITE (NDSO,922) MEMBER(0), MEMBER(1:7)
        ELSE
          WRITE (NDSO,923) MEMBER(0), MEMBER(1:5)
          DO I=2, NQ-1
            WRITE (NDSO,924) MEMBER((I-1)*5+1:I*5)
            END DO
          WRITE (NDSO,925) MEMBER((NQ-1)*5+1:NQ*5+2)
        END IF
!
      CLOSE (NDSO)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Generate namelist file
!
      OPEN (NDSO,FILE='namelist')
!
      JQ     = 0
      DO IQ=1, NQ
        I      = (IQ-1)*5+1
        IF ( QTEST(MEMBER(I),MEMBER(I+1),MEMBER(I+2),L,M,DT) .AND.   &
             ( MEMBER(I+3).GT.0. .OR. MEMBER(I+4).GT.0. ) .AND.      &
              MAX(MEMBER(I),MEMBER(I+1)) .GT. 0. ) JQ = JQ + 1
        END DO
      MQ     = JQ
!
      WRITE (NDSO,930) JQ, MEMBER(LGEN-1), MEMBER(LGEN)
      JQ     = 0
      DO IQ=1, NQ
        I      = (IQ-1)*5+1
        IF ( QTEST(MEMBER(I),MEMBER(I+1),MEMBER(I+2),L,M,DT) .AND.   &
             ( MEMBER(I+3).GT.0. .OR. MEMBER(I+4).GT.0. ) .AND.      &
              MAX(MEMBER(I),MEMBER(I+1)) .GT. 0. ) THEN
            JQ     = JQ + 1
            I      = (IQ-1)*5+4
            IF ( MQ .EQ. 1 ) THEN
                WRITE (NDSO,931) L, M, DT, MEMBER(I:I+1)
              ELSE
                IF ( JQ .EQ. 1 ) THEN
                    WRITE (NDSO,932) L, M, DT, MEMBER(I:I+1)
                  ELSE IF ( JQ .EQ. MQ ) THEN
                    WRITE (NDSO,934) L, M, DT, MEMBER(I:I+1)
                  ELSE
                    WRITE (NDSO,933) L, M, DT, MEMBER(I:I+1)
                  END IF
              END IF
          END IF
        END DO
!
      CLOSE (NDSO)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Normal end of program
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
  803 CONTINUE
      WRITE (*,1003) IERR
      STOP 1003
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' getmember.x: extract snl info :             '/       &
               ' --------------------------------------------'/       &
               '    Member number in population : ',I4/               &
               '    Number of quadruplets       : ',I4)
  920 FORMAT (/'    Writing output files ...')
  922 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  923 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3)
  924 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3)
  925 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  930 FORMAT (' &SNL3 NQDEF =',I3,', MSC =',F6.2,', NSC =',F6.2,' /')
  931 FORMAT (' &ANL3 QPARMS =',F6.3,',',F6.3,',',F6.1,2(',',E10.3),  &
              ' /')
  932 FORMAT (' &ANL3 QPARMS =',F6.3,',',F6.3,',',F6.1,2(',',E10.3),',')
  933 FORMAT (15X,F6.3,',',F6.3,',',F6.1,2(',',E10.3),',')
  934 FORMAT (15X,F6.3,',',F6.3,',',F6.1,2(',',E10.3),' /')
!
  999 FORMAT (/' End of getmember.x')
!
 1001 FORMAT (/' *** ERROR IN READING FORM STDIN ***'/                &
               '     IOSTAT = ',I8/)
 1002 FORMAT (/' *** ERROR IN OPENING INPUT FILE ***'/                &
               '     IOSTAT = ',I8/)
 1003 FORMAT (/' *** ERROR IN READING INPUT FILE ***'/                &
               '     IOSTAT = ',I8/)
!/
!/ End of GMEMBER ---------------------------------------------------- /
!/
      END PROGRAM GMEMBER
