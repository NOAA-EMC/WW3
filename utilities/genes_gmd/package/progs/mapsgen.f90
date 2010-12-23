!/ ------------------------------------------------------------------- /
      PROGRAM MAPSGEN
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         14-Jan-2009 |
!/                  +-----------------------------------+
!/
!/    14-Jan-2009 : Origination.
!/
!/    Copyright 2009-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Put together generation for error mapping.
!
!  2. Method :
!
!     Read all setup info from file input, and write data to file
!     mapping.
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
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NDSI, IERR, NQ, LGEN, NPAR, I, IQ,   &
                                 J, NPOP, NDSO, IINC, JJ
      INTEGER, ALLOCATABLE    :: NP(:), IP(:)
      REAL                    :: ERR, QUAD(5)
!     REAL                    :: QUAD(5), RN, ERR, X1, X2, X3
      REAL, ALLOCATABLE       :: DEFAULT(:), MEMBER(:)
      LOGICAL                 :: FLAG
      LOGICAL, ALLOCATABLE    :: FLAGS(:)
      CHARACTER(LEN=6)        :: ID(5)
      CHARACTER(LEN=43)       :: QSTRNG
!
      TYPE STAT
        REAL                  :: MIN, MAX, LMIN, LMAX, STP
        INTEGER               :: NX
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
      ERR     = 999.999
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from input file
!
      OPEN (NDSI,FILE='input',STATUS='OLD',ERR=801,IOSTAT=IERR)
      READ (NDSI,*,ERR=802,IOSTAT=IERR) NQ
      LGEN   = 5*NQ + 2
      NPAR   = 5 + 2
      WRITE (*,910) NQ, LGEN, NPAR
!
      ALLOCATE (FLAGS(LGEN), DEFAULT(LGEN), MEMBER(LGEN) )
      WRITE (*,911) 'flags'
      READ (NDSI,*,ERR=803,IOSTAT=IERR) FLAGS
      WRITE (*,911) 'default setting'
      READ (NDSI,*,ERR=804,IOSTAT=IERR) DEFAULT
!
      ALLOCATE ( STATS(NPAR) )
      DO I=1, NPAR
        READ (NDSI,*,ERR=805,IOSTAT=IERR) STATS(I)%MIN, STATS(I)%MAX, &
                                          STATS(I)%TYPE, STATS(I)%NX
        STATS(I)%NX = MAX ( 2 , STATS(I)%NX )
        STATS(I)%LMIN = LOG10(STATS(I)%MIN)
        STATS(I)%LMAX = LOG10(STATS(I)%MAX)
        IF ( STATS(I)%TYPE .EQ. 'LIN' ) THEN
            STATS(I)%STP = ( STATS(I)%MAX - STATS(I)%MIN ) /          &
                                REAL(STATS(I)%NX-1)
          END IF
        IF ( STATS(I)%TYPE .EQ. 'EXP' ) THEN
            STATS(I)%STP = ( STATS(I)%LMAX - STATS(I)%LMIN ) /        &
                                REAL(STATS(I)%NX-1)
          END IF
        END DO
!
      ALLOCATE ( NP(LGEN), IP(LGEN) )
      NPOP   = 1
      J      = 0
      DO IQ=1, NQ
        DO I=1, 5
          J      = J + 1
          IF ( FLAGS(J) ) THEN
              WRITE (*,912) J, ID(I), DEFAULT(J), FLAGS(J),           &
                            STATS(I)%MIN, STATS(I)%MAX,               &
                            STATS(I)%TYPE, STATS(I)%NX, STATS(I)%STP
              NP(J)  = STATS(I)%NX
              IP(J)  = 1
              NPOP   = NPOP * NP(J)
            ELSE
              WRITE (*,913) J, ID(I), DEFAULT(J), FLAGS(J)
              NP(J)  = 0
              IP(J)  = 0
            END IF
          END DO
        END DO
!
      J      = J + 1
      I      = 6
      IF ( FLAGS(J) ) THEN
          WRITE (*,912) J, 'm     ', DEFAULT(J), FLAGS(J),            &
                            STATS(I)%MIN, STATS(I)%MAX,               &
                            STATS(I)%TYPE, STATS(I)%NX, STATS(I)%STP
          NP(J)  = STATS(I)%NX
          IP(J)  = 1
          NPOP   = NPOP * NP(J)
        ELSE
          WRITE (*,913) J, 'm     ', DEFAULT(J), FLAGS(J)
          NP(J)  = 0
          IP(J)  = 0
        END IF
!
      J      = J + 1
      I      = I + 1
      IF ( FLAGS(J) ) THEN
          WRITE (*,912) J, 'n     ', DEFAULT(J), FLAGS(J),            &
                            STATS(I)%MIN, STATS(I)%MAX,               &
                            STATS(I)%TYPE, STATS(I)%NX, STATS(I)%STP
          NP(J)  = STATS(I)%NX
          IP(J)  = 1
          NPOP   = NPOP * NP(J)
        ELSE
          WRITE (*,913) J, 'n     ', DEFAULT(J), FLAGS(J)
          NP(J)  = 0
          IP(J)  = 0
        END IF
!
      CLOSE (NDSI)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Generate population
!
      WRITE (*,920)
      OPEN (NDSO,FILE='mapping')
!
      DO I=1, LGEN
        IF ( FLAGS(I) ) THEN
            IINC   = I
            EXIT
          END IF
        END DO
!
! 2.a Loop over total population
!
      DO J=1, NPOP
!
! 2.b Fill in quad parameters
!
        DO IQ=1, NQ
          I      = (IQ-1)*5
          QUAD   = DEFAULT(I+1:I+5)
!
          DO JJ=1, 5
            IF ( FLAGS(I+JJ) ) THEN
                IF ( STATS(JJ)%TYPE .EQ. 'LIN' ) THEN
                    QUAD(JJ) = STATS(JJ)%MIN +                        &
                               REAL(IP(I+JJ)-1)*STATS(JJ)%STP
                  ELSE IF ( STATS(JJ)%TYPE .EQ. 'EXP' ) THEN
                    QUAD(JJ) = 10. ** ( STATS(JJ)%LMIN +              &
                               REAL(IP(I+JJ)-1)*STATS(JJ)%STP )
                  END IF
              END IF
            END DO
!
          WRITE (QSTRNG,921) QUAD
          READ (QSTRNG,*) QUAD
!
          MEMBER(I+1:I+5) = QUAD
!
          END DO
!
! 2.c Compute additional parameters
!
        DO I=6, 7
          JJ     = (NQ-1)*5 + I
          IF ( FLAGS(JJ) ) THEN
              IF ( STATS(JJ)%TYPE .EQ. 'LIN' ) THEN
                  MEMBER(JJ) = STATS(JJ)%MIN +                        &
                             REAL(IP(JJ)-1)*STATS(JJ)%STP
                ELSE IF ( STATS(JJ)%TYPE .EQ. 'EXP' ) THEN
                  MEMBER(JJ) = 10. ** ( STATS(JJ)%LMIN +              &
                             REAL(IP(JJ)-1)*STATS(JJ)%STP )
                END IF
            ELSE
              MEMBER(JJ) = DEFAULT(JJ)
            END IF
          END DO
!
! 2.d Save results to file
!
        IF ( NQ .EQ. 1 ) THEN
            WRITE (NDSO,922) ERR, MEMBER(1:7)
          ELSE
            WRITE (NDSO,923) ERR, MEMBER(1:5)
            DO I=2, NQ-1
              WRITE (NDSO,924) MEMBER((I-1)*5+1:I*5)
              END DO
            WRITE (NDSO,925) MEMBER((NQ-1)*5+1:NQ*5+2)
          END IF
!
! 2.e Increment appropriate counter
!
        IF ( J .EQ. NPOP ) CYCLE
!
        IP(IINC) = IP(IINC) + 1
        IF ( IP(IINC) .GT. NP(IINC) ) THEN
            IP(IINC) = 1
            FLAG   = .TRUE.
            DO
              DO I=IINC+1, LGEN
                IF ( FLAGS(I) ) THEN
                    IINC   = I
                    EXIT
                  END IF
                END DO
              IP(IINC) = IP(IINC) + 1
              IF ( IP(IINC) .GT. NP(IINC) ) THEN
                  IP(IINC) = 1
                ELSE
                  FLAG   = .FALSE.
                  IINC   = 0
                END IF
              IF ( .NOT. FLAG ) EXIT
              END DO

          END IF
!
        IF ( IINC .EQ. 0 ) THEN
            DO I=1, LGEN
              IF ( FLAGS(I) ) THEN
                  IINC   = I
                  EXIT
                END IF
              END DO
          END IF
!
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  All work done
!
      WRITE (*,930)
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
  900 FORMAT (/' mapsgen.x: create generation for mapping:   '/       &
               ' --------------------------------------------')
  910 FORMAT ( '    Number of quadruplets  : ',I6/                    &
               '    Length of genome       : ',I6/                    &
               '    Number of parameters   : ',I6)
  911 FORMAT ( '    Reading ',A)
  912 FORMAT (5X,I3,2X,A,E11.3,L3,2E11.3,2X,A,1X,I3,E11.3)
  913 FORMAT (5X,I3,2X,A,E11.3,L3)
  920 FORMAT (/'    Generating mapping population ...')
  921 FORMAT (F7.3,F7.3,F7.1,2E11.3)
  922 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  923 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3)
  924 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3)
  925 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  930 FORMAT ( '       Population has been generated.')
!
  999 FORMAT (/' End of mapsgen.x')
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
!/ End of MAPSGEN ---------------------------------------------------- /
!/
      END PROGRAM MAPSGEN
