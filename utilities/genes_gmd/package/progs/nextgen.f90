!/ ------------------------------------------------------------------- /
      PROGRAM NEXTGEN
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         23-Mar-2010 |
!/                  +-----------------------------------+
!/
!/    02-Jan-2009 : Origination.
!/    14-Jan-2009 : Increase accuracy of error.
!/                  Retain more children if close.
!/    16-Nov-2009 : Bug fixes for shift perturbation.
!/                  ( J instead of JS in child address.)
!/                  ( Properly center on CGSX.)
!/    04-Dec-2009 : Allow for double crossover for two-par string.
!/    16-Dec-2009 : Allow switching off of quadruplet if layout
!/                  is not optimized.
!/    24-Dec-2009 : Rescale for deep and shallow scaling separately.
!/    04-Feb-2010 : Fix FORMAT warning.   
!/    23-Mar-2010 : Add old.NNNN file generation.
!/
!/    Copyright 2009-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Put together the next generation
!
!  2. Method :
!
!     Read all setup info from file input, and from previous
!     population in file previous and write new data to file
!     next. For method see report.
!
!  3. Parameters :
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      RAN2/3    Subr  RANDOM   Random number generator.
!      QSORT     Subr  QTOOLSMD Sort string of quadruplets.
!      QTEST     L.F.  QTOOLSMD Test validity of quadruplet layout.
!      QDIFF     L.F.  QTOOLSMD See f sorted strings are close.
!      QDIFFA    L.F.  QTOOLSMD See f sorted strings are different.
!      SETCGS    Subr  CGAUSSMD Initialize interpolation tables for
!                               truncated normal distribution.
!      CGSX      R.F.  CGAUSSMD Get X from P (normal distr.)
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
      USE CGAUSSMD
      USE QTOOLSMD
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NDSI, NDSO, IERR, NQ, NPOP, ISEED,   &
                                 LGEN, NPAR, I, J, NACT, IQ, IC, NC,  &
                                 NPL, NPH, NPA, IP1, IP2, NCR, IC1,   &
                                 IC2, NMUT(0:2), MTYPE, JS, JQD, JQS, &
                                 NDSE
      INTEGER, ALLOCATABLE    :: IADDR(:)
      REAL                    :: FACC, FACP1, FACP2, FACP3, RAND,     &
                                 CROSS0, CROSS1, CROSS2, CROSSM,      &
                                 SUM, EMUT, PMUT, X1, X2, X3, MUT1,   &
                                 MRSTD, RN, AVGP1, AVGP2, STDB, STD,  &
                                 UN, FACT, FACTLW
      REAL, ALLOCATABLE       :: DEFAULT(:), OLD(:,:), SOLD(:,:),     &
                                 NEW(:,:), SNEW(:,:), PPAR(:),        &
                                 CHILD(:,:), SCH(:)
      LOGICAL                 :: FLAG
      LOGICAL, ALLOCATABLE    :: FLAGS(:), FLQOPT(:)
      CHARACTER(LEN=6)        :: ID(5)
      CHARACTER(LEN=8)        :: EFILE
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
      NDSE    = 51
!
      CALL SETCGS ( -1 )
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 1.  Read data from input file
!
      OPEN (NDSI,FILE='input',STATUS='OLD',ERR=801,IOSTAT=IERR)
      READ (NDSI,*,ERR=802,IOSTAT=IERR) NQ, NPOP, ISEED
      LGEN   = 5*NQ + 2
      NPAR   = 5 + 2
      WRITE (*,910) NQ, NPOP, ISEED, LGEN
!
      ALLOCATE ( FLAGS(LGEN), DEFAULT(LGEN), FLQOPT(NQ) )
      WRITE (*,911) 'flags'
      READ (NDSI,*,ERR=803,IOSTAT=IERR) FLAGS
      WRITE (*,911) 'default setting'
      READ (NDSI,*,ERR=804,IOSTAT=IERR) DEFAULT
      WRITE (*,911) 'base procreation info'
      READ (NDSI,*,ERR=804,IOSTAT=IERR) FACC, FACP1, FACP2, FACP3,    &
                                        CROSS0, CROSS1, CROSS2,       &
                                        CROSSM, EMUT, MUT1, MRSTD
      FACC   = MAX ( 0. , MIN ( 1. , FACC ) )
      FACP1  = MIN ( 1. , MAX ( FACC , FACP1 ) )
      FACP2  = MIN ( MAX ( FACP1 , FACP2 ) , 1. )
      FACP3  = MAX ( 1.5 , FACP3 )
      CROSS0 = MAX ( 0. , CROSS0 )
      CROSS1 = MAX ( 0. , CROSS1 )
      CROSS2 = MAX ( 0. , CROSS2 )
      SUM    = MAX ( 0.001 , CROSS0+CROSS1+CROSS2 )
      CROSS0 = CROSS0 / SUM
      CROSS1 = CROSS1 / SUM
      CROSS2 = CROSS2 / SUM
      CROSSM = MAX ( 0. , MIN ( 1. , CROSSM ) )
      EMUT   = MAX ( 0. , EMUT )
      MUT1   = MAX ( 0. , MIN ( 1. , MUT1 ) )
      MRSTD  = MAX ( 0. , MRSTD )
      WRITE (*,912) FACC, FACP1, FACP2, FACP3, CROSS0, CROSS1, CROSS2,&
                    CROSSM, EMUT, MUT1, 1.-MUT1, MRSTD
!
      ALLOCATE ( STATS(NPAR) )
      WRITE (*,911) 'stat info per parameter'
      DO I=1, NPAR
        READ (NDSI,*,ERR=805,IOSTAT=IERR) STATS(I)%MIN, STATS(I)%MAX, &
                                          STATS(I)%TYPE
        STATS(I)%LMIN = LOG10(STATS(I)%MIN)
        STATS(I)%LMAX = LOG10(STATS(I)%MAX)
        END DO
!
      J      = 0
      NACT   = 0
      DO IQ=1, NQ
        DO I=1, 5
          J      = J + 1
          IF ( FLAGS(J) ) THEN
              WRITE (*,913) J, ID(I), DEFAULT(J), FLAGS(J),           &
                            STATS(I)%MIN, STATS(I)%MAX, STATS(I)%TYPE
              NACT   = NACT + 1
            ELSE
              WRITE (*,914) J, ID(I), DEFAULT(J), FLAGS(J)
            END IF
          END DO
        END DO
!
      J      = J + 1
      I      = 6
      IF ( FLAGS(J) ) THEN
          WRITE (*,913) J, 'm     ', DEFAULT(J), FLAGS(J),            &
                        STATS(I)%MIN, STATS(I)%MAX, STATS(I)%TYPE
          NACT   = NACT + 1
        ELSE
          WRITE (*,914) J, 'm     ', DEFAULT(J), FLAGS(J)
        END IF
!
      J      = J + 1
      I      = I + 1
      IF ( FLAGS(J) ) THEN
          WRITE (*,913) J, 'n     ', DEFAULT(J), FLAGS(J),            &
                        STATS(I)%MIN, STATS(I)%MAX, STATS(I)%TYPE
          NACT   = NACT + 1
        ELSE
          WRITE (*,914) J, 'n     ', DEFAULT(J), FLAGS(J)
        END IF
!
      WRITE (*,915) NACT
      CLOSE (NDSI)
!
      FLQOPT = .FALSE.
      DO IQ=1, NQ
        DO J=1,3
            FLQOPT(IQ) = FLQOPT(IQ) .OR. FLAGS(J+(IQ-1)*5)
          END DO
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 2.  Read the previous population
! 2.a Get data from file
!
      WRITE (*,920)
      ALLOCATE ( OLD(0:LGEN,NPOP), SOLD(0:LGEN,NPOP) )
!
      OPEN (NDSI,FILE='previous',STATUS='OLD',ERR=811,IOSTAT=IERR)
!
      DO I=1, NPOP
!
        READ (NDSI,*,END=812,ERR=812,IOSTAT=IERR) OLD(:,I)
!
        JQD    = 0
        JQS    = 0
        FLAG   = .FALSE.
!
        DO IQ=1, NQ
          IF ( OLD((IQ-1)*5+4,I).GT.0. ) JQD = JQD + 1
          IF ( OLD((IQ-1)*5+5,I).GT.0. ) JQS = JQS + 1
          END DO
!
        IF ( JQD.NE.NQ .AND. JQD.GE.1 ) THEN
            FACT   = REAL(NQ) / REAL(JQD)
            FLAG   = .TRUE.
            DO IQ=1, NQ
              OLD((IQ-1)*5+4,I) = FACT * OLD((IQ-1)*5+4,I)
              END DO
          END IF
!
        IF ( JQS.NE.NQ .AND. JQS.GE.1 ) THEN
            FACT   = REAL(NQ) / REAL(JQS)
            FLAG   = .TRUE.
            DO IQ=1, NQ
              OLD((IQ-1)*5+5,I) = FACT * OLD((IQ-1)*5+5,I)
              END DO
          END IF
!
        IF ( FLAG ) WRITE (*,922) I, JQD, JQS
!
        CALL QSORT ( NQ, LGEN, OLD(:,I), SOLD(:,I), FLAG )
!
        END DO
!
      WRITE (*,921)
      CLOSE (NDSI)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 3.  Set up new population, copy first part
!
      WRITE (*,930)
      ALLOCATE ( NEW(0:LGEN,NPOP+1), SNEW(0:LGEN,NPOP+1) )
      NEW    = -1.
      SNEW   = -1.
!
      NC     = MIN ( NPOP-1 , MAX ( NINT(REAL(NPOP)*FACC) , 1 ) )
      WRITE (*,931) NC
!
      IC     = 1
      NEW (:,IC) = OLD (:,IC)
      SNEW(:,IC) = SOLD(:,IC)
      WRITE (EFILE,'(A4,I4.4)') 'old.', IC
      OPEN (NDSE,FILE=EFILE)
      WRITE (NDSE,'(I4.4)') IC
      CLOSE (NDSE)
!
      DO I=2, NPOP
        IF ( QDIFF(NQ,LGEN,SNEW(:,IC),SOLD(:,I)) ) THEN
            IC     = IC + 1
            NEW (:,IC) = OLD (:,I)
            SNEW(:,IC) = SOLD(:,I)
            WRITE (EFILE,'(A4,I4.4)') 'old.', IC
            OPEN (NDSE,FILE=EFILE)
            WRITE (NDSE,'(I4.4)') I
            CLOSE (NDSE)
          ELSE
            WRITE (*,932) I
          END IF
        IF ( IC .GE. NC ) EXIT
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 4.  Set up new population, offspring
! 4.a Set up selection of parents
!
      NPL    = NINT(REAL(NPOP)*FACP1)
      NPH    = NINT(REAL(NPOP)*FACP2)
      ALLOCATE ( CHILD(0:LGEN,2), SCH(0:LGEN) )
!
      DO NPA=NPL, NPH-1
        IF ( OLD(0,NPA)/OLD(0,1) .GT. FACP3 ) EXIT
        END DO
!
      WRITE (*,940) NPA
      ALLOCATE ( PPAR(NPA) )
!
      DO I=1, NPA
        PPAR(I) = 1./OLD(0,I) - 1./OLD(0,NPA+1)
        IF ( I.GT.1 ) PPAR(I) = PPAR(I) + PPAR(I-1)
        END DO
!
      PPAR   = PPAR / PPAR(NPA)
      PPAR(NPA) = PPAR(NPA) + 1.E-7
!
      ALLOCATE ( IADDR(NACT) )
      J      = 0
      DO I=1, LGEN
        IF ( FLAGS(I) ) THEN
            J        = J + 1
            IADDR(J) = I
          END IF
        END DO
!
      PMUT   = EMUT / REAL(NACT)
!
! 4.b Endless looop until enough offspring is created
!
      DO
!
! 4.c Select two parents
!
        RAND   = RAN2 ( ISEED )
        DO IP1=1, NPA
          IF ( RAND .LT. PPAR(IP1) ) EXIT
          END DO
        IP1    = MIN ( IP1 , NPA )
!
        RAND   = RAN2 ( ISEED )
        DO IP2=1, NPA
          IF ( RAND .LT. PPAR(IP2) ) EXIT
          END DO
        IP2    = MIN ( IP2 , NPA )
!
        IF ( IP1 .EQ. IP2 ) CYCLE
        WRITE (*,941) IP1, IP2
!
! 4.d Perform cross-over
!
        RAND   = RAN2 ( ISEED )
!
        IF ( RAND .LE. CROSS0 ) THEN
            NCR    = 0
          ELSE IF ( RAND .LE. CROSS0+CROSS1 ) THEN
            NCR    = 1
          ELSE
            NCR    = 2
          END IF
!       NCR    = MIN ( NCR , NACT-1 )
        NCR    = MIN ( NCR , NACT )
!
        WRITE (*,942) NCR
!
        IF ( NCR .EQ. 0 ) THEN
!
            CHILD(:,1) = OLD(:,IP1)
            CHILD(:,2) = OLD(:,IP2)
!
          ELSE IF ( NCR .EQ. 1 ) THEN
!
            RAND   = RAN2 ( ISEED )
            IC1    = 1 + INT(0.9999*REAL(NACT)*RAND)
            IC1    = MIN ( IC1 , NACT )
            IC1    = IADDR(IC1)
!
            CHILD(0,1) = 999.999
            CHILD(0,2) = 999.999
            CHILD(1:IC1,1) = OLD(1:IC1,IP1)
            CHILD(1:IC1,2) = OLD(1:IC1,IP2)
            CHILD(IC1+1:LGEN,1) = OLD(IC1+1:LGEN,IP2)
            CHILD(IC1+1:LGEN,2) = OLD(IC1+1:LGEN,IP1)
!
          ELSE
!
            DO
              RAND   = RAN2 ( ISEED )
              IC1    = 1 + INT(0.9999*REAL(NACT)*RAND)
              IC1    = MIN ( IC1 , NACT )
              RAND   = RAN2 ( ISEED )
              IC2    = 1 + INT(0.9999*REAL(NACT)*RAND)
              IC2    = MIN ( IC2 , NACT )
              IF ( IC1 .LT. IC2 ) EXIT
              IF ( IC1 .GT. IC2 ) THEN
                  J       = IC1
                  IC1     = IC2
                  IC2     = J
                  EXIT
                END IF
              END DO
            IC1    = IADDR(IC1)
            IC2    = IADDR(IC2)
!
            CHILD(0,1) = 999.999
            CHILD(0,2) = 999.999
            CHILD(1:IC1,1) = OLD(1:IC1,IP1)
            CHILD(1:IC1,2) = OLD(1:IC1,IP2)
            CHILD(IC1+1:IC2,1) = OLD(IC1+1:IC2,IP2)
            CHILD(IC1+1:IC2,2) = OLD(IC1+1:IC2,IP1)
            CHILD(IC2+1:LGEN,1) = OLD(IC2+1:LGEN,IP1)
            CHILD(IC2+1:LGEN,2) = OLD(IC2+1:LGEN,IP2)
!
          END IF
!
        IF ( NCR .GE. 1 ) THEN
            RAND   = RAN2 ( ISEED )
            IF ( RAND .LE. CROSSM ) THEN
                J      = IC1
                RAND   = RAN2 ( ISEED )
                IF ( RAND .GT. 0.5 ) J = J + 1
                RAND   = RAN2 ( ISEED )
                AVGP1  = RAND*CHILD(J,1) + (1.-RAND)*CHILD(J,2)
                AVGP2  = RAND*CHILD(J,2) + (1.-RAND)*CHILD(J,1)
                CHILD(J,1) = AVGP1
                CHILD(J,2) = AVGP2
              END IF
          END IF
!
        IF ( NCR .GE. 2 ) THEN
            RAND   = RAN2 ( ISEED )
            IF ( RAND .LE. CROSSM ) THEN
                J      = IC2
                RAND   = RAN2 ( ISEED )
                IF ( RAND .GT. 0.5 ) J = J + 1
                RAND   = RAN2 ( ISEED )
                AVGP1  = RAND*CHILD(J,1) + (1.-RAND)*CHILD(J,2)
                AVGP2  = RAND*CHILD(J,2) + (1.-RAND)*CHILD(J,1)
                CHILD(J,1) = AVGP1
                CHILD(J,2) = AVGP2
              END IF
          END IF
!
! 4.e Perform mutation
!
        NMUT   = 0
!
        DO I=1, NACT
          RAND   = RAN2 ( ISEED )
!
          IF ( RAND .GT. PMUT ) CYCLE
          NMUT(0) = NMUT(0) + 1
          J       = IADDR(I)
!
          RN     = RAN3 ( ISEED )
!
          IF ( RN .LE. MUT1 ) THEN
              MTYPE  = 1
            ELSE 
              MTYPE  = 2
            END IF
!
          NMUT(MTYPE) = NMUT(MTYPE) + 1
!
          IQ     = 1 + (J-1)/5
          IQ     = MIN ( IQ , NQ )
          JS     = J - (IQ-1)*5
!
          IF ( IQ .GT. NQ ) THEN
              FACTLW = 1.
            ELSE
              IF ( .NOT.FLQOPT(IQ) .AND. JS.GT.3 ) THEN
                  FACTLW = 0.95
                ELSE
                  FACTLW = 1.
                END IF
            END IF
!
! 4.e.1 Type 1 (normal)
!
          IF ( MTYPE .EQ. 1 ) THEN
!
              IF ( STATS(JS)%TYPE .EQ. 'LIN' ) THEN
                  RN     = RAN3 ( ISEED )
                  CHILD(J,1) = (1.-RN)*STATS(JS)%MIN*FACTLW           &
                                  + RN*STATS(JS)%MAX
                  RN     = RAN3 ( ISEED )
                  CHILD(J,2) = (1.-RN)*STATS(JS)%MIN*FACTLW           &
                                  + RN*STATS(JS)%MAX
                ELSE IF ( STATS(JS)%TYPE .EQ. 'EXP' ) THEN
                  RN     = RAN3 ( ISEED )
                  CHILD(J,1) = 10. ** ( (1.-RN)*STATS(JS)%LMIN*FACTLW &
                                          + RN*STATS(JS)%LMAX )
                  RN     = RAN3 ( ISEED )
                  CHILD(J,2) = 10. ** ( (1.-RN)*STATS(JS)%LMIN*FACTLW &
                                          + RN*STATS(JS)%LMAX )
                END IF
!
! 4.e.2 Type 2 (local perturbation)
!
            ELSE
!
              SELECT CASE (JS)
                CASE (1)
                  STDB    = MRSTD * 0.25
                CASE (2)
                  STDB    = MRSTD * 0.25
                CASE (3)
                  STDB    = MRSTD * 45.
                CASE (6)
                  STDB    = MRSTD * 4.
                CASE (7)
                  STDB    = MRSTD * 4.
                CASE DEFAULT
                  STDB    = -1.
              END SELECT
!
              RN     = RAN3 ( ISEED )
              IF ( STDB .LT. 0. ) THEN
                  STD    = MRSTD * MAX ( CHILD(J,1) , STATS(JS)%LMIN )
                ELSE
                  STD    = STDB
                END IF
              UN     = STD * 10.
              CHILD(J,1) = CHILD(J,1) + CGSX ( RN, UN, STD ) - UN
              IF ( STATS(JS)%TYPE .EQ. 'EXP' ) THEN
                  CHILD(J,1) = MAX ( 10. ** (FACTLW*STATS(JS)%LMIN)    &
                                                          , CHILD(J,1) )
                  CHILD(J,1) = MIN ( 10. ** STATS(JS)%LMAX, CHILD(J,1) )
                ELSE
                  CHILD(J,1) = MAX ( STATS(JS)%MIN*FACTLW, CHILD(J,1) )
                  CHILD(J,1) = MIN ( STATS(JS)%MAX, CHILD(J,1) )
                END IF
!
              RN     = RAN3 ( ISEED )
              IF ( STDB .LT. 0. ) THEN
                  STD    = MRSTD * MAX ( CHILD(J,2) , STATS(JS)%LMIN )
                ELSE
                  STD    = STDB
                END IF
              UN     = STD * 10.
              CHILD(J,2) = CHILD(J,2) + CGSX ( RN, UN, STD ) - UN
              IF ( STATS(JS)%TYPE .EQ. 'EXP' ) THEN
                  CHILD(J,2) = MAX ( 10. ** (FACTLW*STATS(JS)%LMIN)    &
                                                          , CHILD(J,2) )
                  CHILD(J,2) = MIN ( 10. ** STATS(JS)%LMAX, CHILD(J,2) )
                ELSE
                  CHILD(J,2) = MAX ( STATS(JS)%MIN*FACTLW, CHILD(J,2) )
                  CHILD(J,2) = MIN ( STATS(JS)%MAX, CHILD(J,2) )
                END IF
!
            END IF
!
          IF ( IQ .LE. NQ ) THEN
              IF ( .NOT.FLQOPT(IQ) .AND. JS.GT.3 ) THEN
                  IF ( STATS(JS)%TYPE .EQ. 'EXP' ) THEN
                      IF ( CHILD(J,1) .LT. 10.**STATS(JS)%LMIN )       &
                           CHILD(J,1) = 0.
                      IF ( CHILD(J,2) .LT. 10.**STATS(JS)%LMIN )       &
                           CHILD(J,2) = 0.
                    ELSE
                      IF ( CHILD(J,1) .LT. STATS(JS)%MIN )             &
                           CHILD(J,1) = 0.
                      IF ( CHILD(J,2) .LT. STATS(JS)%MIN )             &
                           CHILD(J,2) = 0.
                    END IF
                END IF
            END IF
!
! ... End loop 4.e
!
          END DO
!
        WRITE (*,943) NMUT
!
! 4.f Check validity of offspring
!
        DO I=1, 2
!
! 4.f.1 Truncate offspring
!
          DO IQ=1, NQ
            WRITE (QSTRNG,951) CHILD((IQ-1)*5+1:IQ*5,I)
            READ (QSTRNG,951) CHILD((IQ-1)*5+1:IQ*5,I)
            END DO
!
          WRITE (QSTRNG,956) CHILD(NQ*5+1:NQ*5+2,I)
          READ (QSTRNG,956) CHILD(NQ*5+1:NQ*5+2,I)
!
! 4.f.2 Validity of quadruplet
!
          FLAG   = .TRUE.
          DO IQ=1, NQ
            FLAG   = FLAG .AND. QTEST ( CHILD((IQ-1)*5+1,I),          &
               CHILD((IQ-1)*5+2,I), CHILD((IQ-1)*5+3,I), X1, X2, X3 )
            END DO
!
          IF ( .NOT. FLAG ) THEN
              WRITE (*,948) I
              CYCLE
            END IF
!
! 4.f.3 Check if offspring is a duplicate
!
          CALL QSORT ( NQ, LGEN, CHILD(:,I), SCH(:), FLAG )
          FLAG   = .FALSE.
!
          DO J=1, IC
            SCH(0) = SNEW(0,J)
            IF ( .NOT. QDIFFA ( NQ, LGEN, SCH, SNEW(:,J) ) ) THEN
                FLAG   = .TRUE.
                EXIT
              END IF
            END DO
!
          IF ( FLAG ) THEN
              WRITE (*,949) I, J
              CYCLE
            END IF
!
! 4.f.4 Accepted, therefore store 
!
          IC     = IC + 1
          CHILD(0,I) = 999.999
          NEW (:,IC) = CHILD(:,I)
          SCH  (0)   = 999.999
          SNEW(:,IC) = SCH  (:)
!
          SCH(0) = SOLD(0,IP1)
          IF ( .NOT.  QDIFFA ( NQ, LGEN, SCH, SOLD(:,IP1) ) ) THEN
              NEW (0,IC) = OLD (0,IP1)
              SNEW(0,IC) = SOLD(0,IP1)
              WRITE (*,1949) IC, IP1
              WRITE (EFILE,'(A4,I4.4)') 'old.', IC
              OPEN (NDSE,FILE=EFILE)
              WRITE (NDSE,'(I4.4)') IP1
              CLOSE (NDSE)
            END IF
!
          SCH(0) = SOLD(0,IP2)
          IF ( .NOT.  QDIFFA ( NQ, LGEN, SCH, SOLD(:,IP2) ) ) THEN
              NEW (0,IC) = OLD (0,IP2)
              SNEW(0,IC) = SOLD(0,IP2)
              WRITE (*,1949) IC, IP2
              WRITE (EFILE,'(A4,I4.4)') 'old.', IC
              OPEN (NDSE,FILE=EFILE)
              WRITE (NDSE,'(I4.4)') IP2
              CLOSE (NDSE)
            END IF
!
! ... End loop 4.f
!
          IF ( IC .GE. NPOP ) EXIT
          END DO
!
! ... End loop 4.b
!
        IF ( IC .GE. NPOP ) EXIT
        END DO
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  Write out new population
!
      WRITE (*,950)
      OPEN (NDSO,FILE='next',ERR=821,IOSTAT=IERR)
!
      DO I=1, NPOP
!
        JQD    = 0
        JQS    = 0
        FLAG   = .FALSE.
!
        DO IQ=1, NQ
          IF ( NEW((IQ-1)*5+4,I).GT.0. ) JQD = JQD + 1
          IF ( NEW((IQ-1)*5+5,I).GT.0. ) JQS = JQS + 1
          END DO
!
        IF ( JQD.NE.NQ .AND. JQD.GE.1 ) THEN
            FACT   = REAL(JQD) / REAL(NQ)
            FLAG   = .TRUE.
            DO IQ=1, NQ
              NEW((IQ-1)*5+4,I) = FACT * NEW((IQ-1)*5+4,I)
              END DO
          END IF
!
        IF ( JQS.NE.NQ .AND. JQS.GE.1 ) THEN
            FACT   = REAL(JQS) / REAL(NQ)
            FLAG   = .TRUE.
            DO IQ=1, NQ
              NEW((IQ-1)*5+5,I) = FACT * NEW((IQ-1)*5+5,I)
              END DO
          END IF
!
        IF ( FLAG ) WRITE (*,922) I, JQD, JQS
!
        IF ( NQ .EQ. 1 ) THEN
            WRITE (NDSO,952) NEW(0:7,I)
          ELSE
            WRITE (NDSO,953) NEW(0:5,I)
            DO J=2, NQ-1
              WRITE (NDSO,954) NEW((J-1)*5+1:J*5,I)
              END DO
            WRITE (NDSO,955) NEW((NQ-1)*5+1:NQ*5+2,I)
          END IF
!
        END DO
!
      CLOSE (NDSO)
!
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 6.  All work done
!
      WRITE (*,960)
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
  806 CONTINUE
      WRITE (*,1006) IERR
      STOP 1006
!
  811 CONTINUE
      WRITE (*,1011) IERR
      STOP 1011
!
  812 CONTINUE
      WRITE (*,1012) IERR
      STOP 1012
!
  821 CONTINUE
      WRITE (*,1021) IERR
      STOP 1021
!
  888 CONTINUE
!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     Finalize program
!
      WRITE (*,999)
!
! Formats
!
  900 FORMAT (/' nextgen.x: create initial generation :      '/       &
               ' --------------------------------------------')
  910 FORMAT ( '    Number of quadruplets  : ',I6/                    &
               '    Size of population     : ',I6/                    &
               '    Initial random seed    : ',I6/                    &
               '    Length of genome       : ',I6)
  911 FORMAT ( '    Reading ',A)
  912 FORMAT ( '    Fraction copied        : ',F9.2/                  &
               '    Min parent fraction    : ',F9.2/                  &
               '    Max parent fraction    : ',F9.2/                  &
               '    Max parent error fact. : ',F9.2/                  &
               '    Crossover chances      : ',F9.2,2F6.2/            &
               '    Arithmetic recomb. p.  : ',F9.2/                  &
               '    Mutation expextation   : ',F9.2/                  &
               '    Mutation chances       : ',F9.2,F6.2/             &
               '    Mutation rel std.      : ',F9.2)
  913 FORMAT (5X,I3,2X,A,E11.3,L3,2E11.3,2X,A)
  914 FORMAT (5X,I3,2X,A,E11.3,L3)
  915 FORMAT ( '    Number of active pars. : ',I6)
!
  920 FORMAT (/'    Reading previous population ...')
  921 FORMAT ( '       File succesfully read.')
  922 FORMAT ( '       Rescaling member ',I4,'  (',I2.2,',',I2.2,')')
!
  930 FORMAT (/'    Starting new population ...')
  931 FORMAT ( '       Copying',I3,' members of old population.')
  932 FORMAT ( '       Member',I4,' from old population is a near duplicate')
!
  940 FORMAT ( '       Number of parents :',I4)
  941 FORMAT ( '          Selected parents :',2I4)
  942 FORMAT ( '          Number of crossovers :',I2)
  943 FORMAT ( '          Number of mutations :',I2,' (',I2,1(',',I2),')')
  948 FORMAT ( '          Child',I2,' has illegal quad(s)')
  949 FORMAT ( '          Child',I2,' is identical to member',I5)
 1949 FORMAT ( '          New',I5,' is identical to old',I5)
!
  950 FORMAT (/'    Writing out new population ...')
  951 FORMAT (F7.3,F7.3,F7.1,2E11.3)
  952 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  953 FORMAT (F8.3,F7.3,F7.3,F7.1,2E11.3)
  954 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3)
  955 FORMAT (8X,F7.3,F7.3,F7.1,2E11.3,F6.2,F6.2)
  956 FORMAT (F6.2,F6.2)
!
  960 FORMAT ( '       New population has been generated.')
!
  999 FORMAT (/' End of nextgen.x')
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
 1006 FORMAT (/' *** ERROR IN READING INPUT FILE (6) ***'/            &
               '     IOSTAT = ',I8/)
!
 1011 FORMAT (/' *** ERROR IN OPENING PREVIOUS FILE ***'/             &
               '     IOSTAT = ',I8/)
 1012 FORMAT (/' *** ERROR IN READING PREVIOUS FILE ***'/             &
               '     IOSTAT = ',I8/)
!
 1021 FORMAT (/' *** ERROR IN OPENING NEXT FILE ***'/                 &
               '     IOSTAT = ',I8/)
!/
!/ End of NEXTGEN ---------------------------------------------------- /
!/
      END PROGRAM NEXTGEN
