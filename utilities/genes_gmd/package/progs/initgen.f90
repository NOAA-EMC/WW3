!/ ------------------------------------------------------------------- /
      PROGRAM INITGEN
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         14-Jan-2009 |
!/                  +-----------------------------------+
!/
!/    20-Dec-2008 : Origination.
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
!     Put together initial generation.
!
!  2. Method :
!
!     Read all setup info from file input, and write data to file
!     population.
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      RAN2      Subr  RANDOM   Random number generator.
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
      USE RANDOM
      USE QTOOLSMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NDSI, NDSO, NDST, IERR, NQ, NPOP,    &
                                 ISEED, LGEN, NPAR, I, IQ, J, IP
      REAL                    :: QUAD(5), RN, ERR, X1, X2, X3
      REAL, ALLOCATABLE       :: DEFAULT(:), MEMBER(:)
      LOGICAL                 :: FLOK
      LOGICAL, ALLOCATABLE    :: FLAGS(:)
      CHARACTER(LEN=6)        :: ID(5)
      CHARACTER(LEN=43)       :: QSTRNG
!
      TYPE STAT
        REAL                  :: MIN, MAX, LMIN, LMAX
        CHARACTER(LEN=3)      :: TYPE
      END TYPE STAT
      TYPE(STAT), ALLOCATABLE :: STATS(:)
!
      DATA ID / 'lambda', 'mu    ', 'Dtheta', 'Cd    ', 'Cs    ' /
!/
!/ ------------------------------------------------------------------- /
!
! 0.  Initialization
!
      WRITE (*,900)
      NDSI    = 10
      NDSO    = 50
      NDST    = 51
      ERR     = 999.999
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from input file
!
      OPEN (NDSO,FILE='input',STATUS='OLD',ERR=801,IOSTAT=IERR)
      READ (NDSO,*,ERR=802,IOSTAT=IERR) NQ, NPOP, ISEED
      LGEN   = 5*NQ + 2
      NPAR   = 5 + 2
      WRITE (*,910) NQ, NPOP, ISEED, LGEN
!
      ALLOCATE (FLAGS(LGEN), DEFAULT(LGEN), MEMBER(LGEN) )
      WRITE (*,911) 'flags'
      READ (NDSO,*,ERR=803,IOSTAT=IERR) FLAGS
      WRITE (*,911) 'default setting'
      READ (NDSO,*,ERR=804,IOSTAT=IERR) DEFAULT
!
      READ (NDSO,*)
      ALLOCATE ( STATS(NPAR) )
      DO I=1, NPAR
        READ (NDSO,*,ERR=805,IOSTAT=IERR) STATS(I)%MIN, STATS(I)%MAX, &
                                          STATS(I)%TYPE
        STATS(I)%LMIN = LOG10(STATS(I)%MIN)
        STATS(I)%LMAX = LOG10(STATS(I)%MAX)
        END DO
!
      J      = 0
      DO IQ=1, NQ
        DO I=1, 5
          J      = J + 1
          IF ( FLAGS(J) ) THEN
              WRITE (*,912) J, ID(I), DEFAULT(J), FLAGS(J),           &
                            STATS(I)%MIN, STATS(I)%MAX, STATS(I)%TYPE
            ELSE
              WRITE (*,913) J, ID(I), DEFAULT(J), FLAGS(J)
            END IF
          END DO
        END DO
!
      J      = J + 1
      I      = 6
      IF ( FLAGS(J) ) THEN
          WRITE (*,912) J, 'm     ', DEFAULT(J), FLAGS(J),            &
                        STATS(I)%MIN, STATS(I)%MAX, STATS(I)%TYPE
        ELSE
          WRITE (*,913) J, 'm     ', DEFAULT(J), FLAGS(J)
        END IF
!
      J      = J + 1
      I      = I + 1
      IF ( FLAGS(J) ) THEN
          WRITE (*,912) J, 'n     ', DEFAULT(J), FLAGS(J),            &
                        STATS(I)%MIN, STATS(I)%MAX, STATS(I)%TYPE
        ELSE
          WRITE (*,913) J, 'n     ', DEFAULT(J), FLAGS(J)
        END IF
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Generate population
!
      WRITE (*,920)
!     OPEN (NDSO,FILE='population.ieee',FORM='UNFORMATTED')
!     OPEN (NDST,FILE='population.text')
      OPEN (NDST,FILE='population')
!
! 2.a Loop over members
!
      DO IP=1, NPOP
!
! 2.b Create quadruplets
!
        DO IQ=1, NQ
          I      = (IQ-1)*5
          QUAD   = DEFAULT(I+1:I+5)
!
! 2.b.1 First guess
!
          DO
!
            DO J=1, 5
              IF ( FLAGS(I+J) ) THEN
                  RN     = RAN2(ISEED)
                  IF ( STATS(J)%TYPE .EQ. 'LIN' ) THEN
                      QUAD(J) = (1.-RN)*STATS(J)%MIN + RN*STATS(J)%MAX
                    ELSE IF ( STATS(J)%TYPE .EQ. 'EXP' ) THEN
                      QUAD(J) = 10. ** ( (1.-RN)*STATS(J)%LMIN +      &
                                            RN*STATS(J)%LMAX )
                    END IF
                END IF
              END DO
!
! 2.b.2 Truncate
!
            WRITE (QSTRNG,921) QUAD
            READ (QSTRNG,*) QUAD
!
! 2.b.3 Test validity
!
            FLOK   = QTEST ( QUAD(1), QUAD(2), QUAD(3), X1, X2, X3 )
            IF ( FLOK ) EXIT
!           WRITE (*,926) IP, IQ, QUAD(1:3)
!
! ... End of loop 2.b.1
!
            END DO
!
! 2.b.4 Store results
!
          MEMBER(I+1:I+5) = QUAD
!
! ... End of loop 2.b
!
          END DO
!
! 2.c Compute additional parameters
!
        DO I=6, 7
          J      = (NQ-1)*5 + I
          IF ( FLAGS(J) ) THEN
            RN     = RAN2(ISEED)
              IF ( STATS(I)%TYPE .EQ. 'LIN' ) THEN
                  MEMBER(J) = (1.-RN)*STATS(I)%MIN + RN*STATS(I)%MAX
                ELSE IF ( STATS(I)%TYPE .EQ. 'EXP' ) THEN
                  MEMBER(J) = 10. ** ( (1.-RN)*STATS(I)%LMIN +        &
                                              RN*STATS(I)%LMAX )
                END IF
            ELSE
              MEMBER(J) = DEFAULT(J)
            END IF
          END DO
!
! 2.d Save results to file
!
!       WRITE (NDSO) ERR, MEMBER
!
        IF ( NQ .EQ. 1 ) THEN
            WRITE (NDST,922) ERR, MEMBER(1:7)
          ELSE
            WRITE (NDST,923) ERR, MEMBER(1:5)
            DO I=2, NQ-1
              WRITE (NDST,924) MEMBER((I-1)*5+1:I*5)
              END DO
            WRITE (NDST,925) MEMBER((NQ-1)*5+1:NQ*5+2)
          END IF
!
! ... End of loop 2.a
!
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  All work done
!
      WRITE (*,930)
!
!     CLOSE (NDSO)
      CLOSE (NDST)
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
  804 CONTINUE
      WRITE (*,1004) IERR
      STOP 1004
!
  805 CONTINUE
      WRITE (*,1005) IERR
      STOP 1005
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' initgen.x: create initial generation :      '/       &
               ' --------------------------------------------')
  910 FORMAT ( '    Number of quadruplets  : ',I6/                    &
               '    Size of population     : ',I6/                    &
               '    Initial random seed    : ',I6/                    &
               '    Length of genome       : ',I6)
  911 FORMAT ( '    Reading ',A)
  912 FORMAT (5X,I3,2X,A,E11.3,L3,2E11.3,2X,A)
  913 FORMAT (5X,I3,2X,A,E11.3,L3)
  920 FORMAT (/'    Generating initial population ...')
  921 FORMAT (F7.3,F7.3,F7.1,2E11.3)
  922 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  923 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3)
  924 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3)
  925 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  926 FORMAT ( '       Illegal quadruplet for IP =',I4,', IQ =',I3,   &
               '    pars : ',2F7.3,F7.1)
  930 FORMAT ( '       Population has been generated.')
!
  999 FORMAT (/' End of initgen.x')
!
 1001 FORMAT (/' *** ERROR IN OPENING INPUT FILE ***'/                &
               '     IOSTAT = ',I8/)
 1002 FORMAT (/' *** ERROR IN READING INPUT FILE (2) ***'/            &
               '     IOSTAT = ',I8/)
 1003 FORMAT (/' *** ERROR IN READING INPUT FILE (3) ***'/            &
               '     IOSTAT = ',I8/)
 1004 FORMAT (/' *** ERROR IN READING INPUT FILE (4) ***'/            &
               '     IOSTAT = ',I8/)
 1005 FORMAT (/' *** ERROR IN READING INPUT FILE (5) ***'/            &
               '     IOSTAT = ',I8/)
!/
!/ End of INITGEN ---------------------------------------------------- /
!/
      END PROGRAM INITGEN
