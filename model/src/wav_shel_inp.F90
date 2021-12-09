module wav_shel_inp

 implicit none
 private ! except

 public :: read_shel_inp

 contains
 
 !subroutine read_shel_inp(....inputs)?
 subroutine read_shel_inp(mpi_comm)
 
      USE W3GDATMD, ONLY: FLAGLL
      USE W3WDATMD, ONLY: TIME, VA, W3NDAT, W3DIMW, W3SETW
      USE W3ADATMD, ONLY: W3NAUX, W3DIMA, W3SETA
      USE W3IDATMD, ONLY: INFLAGS1, INFLAGS2, FLAGSC
      USE W3ODATMD, ONLY: W3NOUT, W3SETO, NDS
      USE W3ODATMD, ONLY: NAPROC, IAPROC, NAPOUT, NAPERR, NOGRP,      &
                          NGRPP, IDOUT, FNMPRE, IOSTYP, NOTYPE
      USE W3ODATMD, ONLY: FLOGRR, FLOGR, OFILES
      USE W3IOGRMD, ONLY: W3IOGR
      USE W3IOGOMD, ONLY: W3READFLGRD, FLDOUT, W3FLGRDFLAG
      USE W3IORSMD, ONLY: OARST
      USE W3SERVMD, ONLY: NEXTLN, EXTCDE
      USE W3TIMEMD, ONLY: DSEC21, STME21, TICK21
!
      INCLUDE "mpif.h"

      INTEGER, INTENT(IN) :: MPI_COMM
!/ ------------------------------------------------------------------- /
!/ Local PARAMETER statements
!/
      INTEGER, PARAMETER  :: NHMAX =    200
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER             :: NDSI, NDSI2, NDSS, NDSO, NDSE, NDST, NDSL,&
                             NDSEN, IERR, J, I, ILOOP, IPTS, NPTS
      INTEGER             :: NDSF(-7:9), NTRACE(2), &
                             TIME0(2), TIMEN(2), TTIME(2),    &
                             NH(-7:10), THO(2,-7:10,NHMAX),   &
                             ODAT(40), IPRT(6) = 0
      INTEGER             :: jfirst, IERR_MPI
      REAL                :: FACTOR, DTTST, XX, YY, HA(NHMAX,-7:10), &
                             HD(NHMAX,-7:10), HS(NHMAX,-7:10)
      REAL, ALLOCATABLE   :: X(:), Y(:)

      CHARACTER(LEN=1)    :: COMSTR, FLAGTFC(-7:10)
      CHARACTER(LEN=3)    :: IDSTR(-7:10), IDTST
      CHARACTER(LEN=6)    :: YESXNO
      CHARACTER(LEN=40)   :: PN
      CHARACTER(LEN=40),                                               &
              ALLOCATABLE :: PNAMES(:)
      CHARACTER(LEN=13)   :: IDFLDS(-7:10)
      CHARACTER(LEN=20)   :: STRNG
      CHARACTER(LEN=23)   :: DTME21
      CHARACTER(LEN=30)   :: IDOTYP(8)
      CHARACTER(LEN=80)   :: LINE
      CHARACTER(LEN=1024) :: FLDRST=''
      CHARACTER(LEN=80)   :: LINEIN
      CHARACTER(LEN=8)    :: WORDS(7)=''
      LOGICAL             :: FLLSTL, FLLSTI, FLLSTR, FLFLG, FLHOM,     &
                             TFLAGI, PRTFRM, FLAGSCI, FLGNML
      LOGICAL             :: FLGRD(NOGRP,NGRPP), FLGD(NOGRP),          &
                             FLGR2(NOGRP,NGRPP), FLG2(NOGRP),          &
                             FLH(-7:10),    &
                             FLLST_ALL(-7:10)
      INTEGER             :: THRLEV

      DATA IDFLDS / 'ice param. 1 ' , 'ice param. 2 ' ,               &
                    'ice param. 3 ' , 'ice param. 4 ' ,               &
                    'ice param. 5 ' ,                                 &
                    'mud density  ' , 'mud thkness  ' ,               &
                    'mud viscos.  ' ,                                 &
                    'water levels ' , 'currents     ' ,               &
                    'winds        ' , 'ice fields   ' ,               &
                    'momentum     ' , 'air density  ' ,               &
                    'mean param.  ' , '1D spectra   ' ,               &
                    '2D spectra   ' , 'moving grid  ' /
      DATA IDOTYP / 'Fields of mean wave parameters' ,                &
                    'Point output                  ' ,                &
                    'Track point output            ' ,                &
                    'Restart files                 ' ,                &
                    'Nesting data                  ' ,                &
                    'Partitioned wave field data   ' ,                &
                    'Fields for coupling           ' ,                &
                    'Restart files second request  '/
      DATA IDSTR  / 'IC1', 'IC2', 'IC3', 'IC4', 'IC5', 'MDN', 'MTH',  &
                    'MVS', 'LEV', 'CUR', 'WND', 'ICE', 'TAU', 'RHO',  &
                    'DT0', 'DT1', 'DT2', 'MOV' /
!
      FLGR2 = .FALSE.
      FLH(:)       = .FALSE.
!
! IO setup comes next---do we want to move it from initreal?

      NTRACE(1) =  NDS(3)
      NTRACE(2) =  10

      NDSI   = 10
      NDSS   = 90
      NDSO   =  6
      NDSE   =  6
      NDST   =  6
      NDSL   = 50

      IF ( IAPROC .EQ. NAPERR ) THEN
        NDSEN  = NDSE
      ELSE
        NDSEN  = -1
      END IF
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,905) &
                                        MPI_THREAD_FUNNELED, THRLEV
      NDSF(-7)  = 1008
      NDSF(-6)  = 1009
      NDSF(-5)  = 1010
      NDSF(-4)  = 1011
      NDSF(-3)  = 1012
      NDSF(-2)  = 1013
      NDSF(-1)  = 1014
      NDSF(0)   = 1015

      NDSF(1)  = 11
      NDSF(2)  = 12
      NDSF(3)  = 13
      NDSF(4)  = 14
      NDSF(5)  = 15
      NDSF(6)  = 16
      NDSF(7)  = 17
      NDSF(8)  = 18
      NDSF(9)  = 19
! 1.c Local parameters

! Default COMSTR to "$" (for when using nml input files)
      COMSTR = "$"
! inferred from context: these flags (FL) are to indicate that the last (LST)
!   field has been read from a file.
      FLLSTL = .FALSE. ! This is associated with J.EQ.1 (wlev)
      FLLSTI = .FALSE. ! This is associated with J.EQ.4 (ice)
      FLLSTR = .FALSE. ! This is associated with J.EQ.6 (rhoa)
      FLLST_ALL = .FALSE. ! For all
! If using experimental mud or ice physics, additional lines will
!  be read in from ww3_shel.inp and applied, so JFIRST is changed from
!  its initialization setting "JFIRST=1" to some lower value.
      JFIRST=1

! process old ww3_shel.inp format
!      IF (.NOT. FLGNML) THEN
        OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_shel.inp',STATUS='OLD',IOSTAT=IERR)
        REWIND (NDSI)
        READ (NDSI,'(A)') COMSTR
        IF (COMSTR.EQ.' ') COMSTR = '$'
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR

! 2.1 forcing flags

        FLH(-7:10) = .FALSE.
        DO J=JFIRST, 9
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          IF ( J .LE. 6 ) THEN
            READ (NDSI,*) FLAGTFC(J), FLH(J)
          ELSE
            READ (NDSI,*) FLAGTFC(J)
          END IF
        END DO

        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,920)
        DO J=JFIRST, 9
          IF (FLAGTFC(J).EQ.'T') THEN
            INFLAGS1(J)=.TRUE.
            FLAGSC(J)=.FALSE.
          END IF
          IF (FLAGTFC(J).EQ.'F') THEN
            INFLAGS1(J)=.FALSE.
            FLAGSC(J)=.FALSE.
          END IF
          IF (FLAGTFC(J).EQ.'C') THEN
            INFLAGS1(J)=.TRUE.
            FLAGSC(J)=.TRUE.
          END IF
          IF ( J .LE. 6 ) THEN
            FLH(J) = FLH(J) .AND. INFLAGS1(J)
          END IF
          IF ( INFLAGS1(J) ) THEN
            YESXNO = 'YES/--'
          ELSE
            YESXNO = '---/NO'
          END IF
          IF ( FLH(J) ) THEN
            STRNG  = '(homogeneous field) '
          ELSE IF ( FLAGSC(J) ) THEN
            STRNG  = '(coupling field) '
          ELSE
            STRNG  = '                    '
          END IF
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,921) IDFLDS(J), YESXNO, STRNG
        END DO

        INFLAGS1(10) = .FALSE.
        IF ( INFLAGS1(10) .AND. IAPROC.EQ.NAPOUT )                         &
             WRITE (NDSO,921) IDFLDS(10), 'YES/--', ' '
        FLFLG  = INFLAGS1(-7) .OR. INFLAGS1(-6) .OR. INFLAGS1(-5) .OR. INFLAGS1(-4) &
                 .OR. INFLAGS1(-3) .OR. INFLAGS1(-2) .OR. INFLAGS1(-1)           &
                 .OR. INFLAGS1(0)  .OR. INFLAGS1(1)  .OR. INFLAGS1(2)            &
                 .OR. INFLAGS1(3)  .OR. INFLAGS1(4)  .OR. INFLAGS1(5)            &
                 .OR. INFLAGS1(6)  .OR. INFLAGS1(7)  .OR. INFLAGS1(8)            &
                 .OR. INFLAGS1(9)
        FLHOM  = FLH(-7) .OR. FLH(-6) .OR. FLH(-5) .OR. FLH(-4)       &
                 .OR. FLH(-3) .OR. FLH(-2) .OR. FLH(-1) .OR. FLH(0)   &
                 .OR. FLH(1) .OR. FLH(2) .OR. FLH(3) .OR. FLH(4)      &
                 .OR. FLH(5) .OR. FLH(6) .OR. FLH(10)
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,922)
!
!       INFLAGS2 is just "initial value of INFLAGS1", i.e. does *not* get
!          changed when model reads last record of ice.ww3
        INFLAGS2=INFLAGS1
! 2.2 Time setup

        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
        READ (NDSI,*) TIME0
        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
        READ (NDSI,*) TIMEN

! 2.3 Domain setup

        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
        READ (NDSI,*) IOSTYP
        CALL W3IOGR ( 'GRID', NDSF(7) )
        IF ( FLAGLL ) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1.E-3
        END IF

! 2.4 Output dates

        NPTS   = 0
        NOTYPE = 6
        DO J = 1, NOTYPE
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
! CHECKPOINT
        IF(J .EQ. 4) THEN
          ODAT(38)=0
          WORDS(1:7)=''
          READ (NDSI,'(A)') LINEIN
          READ(LINEIN,*,iostat=ierr) WORDS
          READ(WORDS( 1 ), * ) ODAT(16)
          READ(WORDS( 2 ), * ) ODAT(17)
          READ(WORDS( 3 ), * ) ODAT(18)
          READ(WORDS( 4 ), * ) ODAT(19)
          READ(WORDS( 5 ), * ) ODAT(20)
          IF (WORDS(6) .EQ. 'T') THEN
            CALL NEXTLN ( COMSTR , NDSI , NDSEN )
            READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(8-1)+1,5*8)
            WRITE(*,*)(ODAT(I),I=5*(8-1)+1,5*8)
          END IF
          IF (WORDS(7) .EQ. 'T') THEN
            CALL NEXTLN ( COMSTR , NDSI , NDSEN )
            READ (NDSI,'(A)',END=2001,ERR=2002) FLDRST
          END IF
          CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDRST, FLOGR,  &
                             FLOGRR, IAPROC, NAPOUT, IERR )
          IF ( IERR .NE. 0 ) GOTO 2222
        ELSE
!INLINE NEW VARIABLE TO READ IF PRESENT OFILES(J), IF NOT ==0
!          READ (NDSI,*) (ODAT(I),I=5*(J-1)+1,5*J)
!          READ (NDSI,*,IOSTAT=IERR) (ODAT(I),I=5*(J-1)+1,5*J),OFILES(J)
        IF(J .LE. 2) THEN
          WORDS(1:6)=''
!          READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J),OFILES(J)
          READ (NDSI,'(A)') LINEIN
          READ(LINEIN,*,iostat=ierr) WORDS
          IF(J .EQ. 1) THEN
            READ(WORDS( 1 ), * ) ODAT(1)
            READ(WORDS( 2 ), * ) ODAT(2)
            READ(WORDS( 3 ), * ) ODAT(3)
            READ(WORDS( 4 ), * ) ODAT(4)
            READ(WORDS( 5 ), * ) ODAT(5)
          ELSE
            READ(WORDS( 1 ), * ) ODAT(6)
            READ(WORDS( 2 ), * ) ODAT(7)
            READ(WORDS( 3 ), * ) ODAT(8)
            READ(WORDS( 4 ), * ) ODAT(9)
            READ(WORDS( 5 ), * ) ODAT(10)
          END IF

          IF (WORDS(6) .NE. '0' .AND. WORDS(6) .NE. '1') THEN
            OFILES(J)=0
          ELSE
            READ(WORDS( 6 ), * ) OFILES(J)
          END IF
        ELSE
          OFILES(J)=0
          READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J)
        END IF
          ODAT(5*(J-1)+3) = MAX ( 0 , ODAT(5*(J-1)+3) )

! 2.5 Output types

          IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN

! Type 1: fields of mean wave parameters
            IF ( J .EQ. 1 ) THEN
              CALL W3READFLGRD ( NDSI, NDSO, 9, NDSEN, COMSTR, FLGD,   &
                                 FLGRD, IAPROC, NAPOUT, IERR )
              IF ( IERR .NE. 0 ) GOTO 2222
! Type 2: point output
            ELSE IF ( J .EQ. 2 ) THEN
              DO ILOOP=1,2
                IF ( ILOOP .EQ. 1 ) THEN
                  NDSI2  = NDSI
                  IF ( IAPROC .EQ. 1 ) OPEN                       &
                       (NDSS,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
                ELSE
                  NDSI2  = NDSS
                  CALL MPI_BARRIER (MPI_COMM,IERR_MPI)
                  OPEN (NDSS,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
                  REWIND (NDSS)
!
                  IF ( .NOT.ALLOCATED(X) ) THEN
                    IF ( NPTS.GT.0 ) THEN
                      ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
                    ELSE
                      ALLOCATE ( X(1), Y(1), PNAMES(1) )
                      GOTO 2054
                    END IF
                  END IF
                END IF
!
                NPTS   = 0
                DO
                  CALL NEXTLN ( COMSTR , NDSI , NDSEN )
                  READ (NDSI2,*) XX, YY, PN
                  print *,'XXX ',j,xx,yy,pn
                  IF ( ILOOP.EQ.1 .AND. IAPROC.EQ.1 ) THEN
                    BACKSPACE (NDSI)
                    READ (NDSI,'(A)') LINE
                    WRITE (NDSS,'(A)') LINE
                  END IF
                  IF ( INDEX(PN,"STOPSTRING").NE.0 ) EXIT
                  NPTS   = NPTS + 1
                  IF ( ILOOP .EQ. 1 ) CYCLE
                  X(NPTS)      = XX
                  Y(NPTS)      = YY
                  PNAMES(NPTS) = PN
                  IF ( IAPROC .EQ. NAPOUT ) THEN
                    IF ( FLAGLL ) THEN
                      IF ( NPTS .EQ. 1 ) THEN
                        WRITE (NDSO,2945)                     &
                               FACTOR*XX, FACTOR*YY, PN
                      ELSE
                        WRITE (NDSO,2946) NPTS,               &
                               FACTOR*XX, FACTOR*YY, PN
                      END IF
                    ELSE
                      IF ( NPTS .EQ. 1 ) THEN
                        WRITE (NDSO,2955)                     &
                               FACTOR*XX, FACTOR*YY, PN
                      ELSE
                        WRITE (NDSO,2956) NPTS,               &
                               FACTOR*XX, FACTOR*YY, PN
                      END IF
                    END IF
                  END IF
                END DO
                IF ( IAPROC.EQ.1 .AND. ILOOP.EQ.1 ) CLOSE (NDSS)
              END DO
              IF ( NPTS.EQ.0 .AND. IAPROC.EQ.NAPOUT )               &
                   WRITE (NDSO,2947)
              IF ( IAPROC .EQ. 1 ) THEN
                CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
                CLOSE (NDSS,STATUS='DELETE')
              ELSE
                CLOSE (NDSS)
                CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
              END IF

! Type 3: track output
            ELSE IF ( J .EQ. 3 ) THEN
              CALL NEXTLN ( COMSTR , NDSI , NDSEN )
              READ (NDSI,*) TFLAGI
              IF ( .NOT. TFLAGI ) NDS(11) = -NDS(11)
              IF ( IAPROC .EQ. NAPOUT ) THEN
                IF ( .NOT. TFLAGI ) THEN
                  WRITE (NDSO,3945) 'input', 'UNFORMATTED'
                ELSE
                  WRITE (NDSO,3945) 'input', 'FORMATTED'
                END IF
              END IF

! Type 6: partitioning
            ELSE IF ( J .EQ. 6 ) THEN
!             IPRT: IX0, IXN, IXS, IY0, IYN, IYS
              CALL NEXTLN ( COMSTR , NDSI , NDSEN )
              READ (NDSI,*) IPRT, PRTFRM
              IF ( IAPROC .EQ. NAPOUT ) THEN
                IF ( PRTFRM ) THEN
                  YESXNO = 'YES/--'
                ELSE
                  YESXNO = '---/NO'
                END IF
                WRITE (NDSO,6945) IPRT, YESXNO
              END IF
            END IF ! J
          END IF ! ODAT
        END IF ! IF J=4
        END DO ! J

        ! force minimal allocation to avoid memory seg fault
        IF ( .NOT.ALLOCATED(X) .AND. NPTS.EQ.0 ) ALLOCATE ( X(1), Y(1), PNAMES(1) )

! 2.6 Homogeneous field data

        IF ( FLHOM ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
                          'Homogeneous field data (and moving grid) ...'
          NH     = 0
          ! Start of loop
          DO
            CALL NEXTLN ( COMSTR , NDSI , NDSEN )
            READ (NDSI,*) IDTST

            ! Exit if illegal id
            IF ( IDTST.NE.IDSTR(-7) .AND. IDTST.NE.IDSTR(-6) .AND.   &
                 IDTST.NE.IDSTR(-5) .AND. IDTST.NE.IDSTR(-4) .AND.   &
                 IDTST.NE.IDSTR(-3) .AND. IDTST.NE.IDSTR(-2) .AND.   &
                 IDTST.NE.IDSTR(-1) .AND. IDTST.NE.IDSTR(0)  .AND.   &
                 IDTST.NE.IDSTR(1)  .AND. IDTST.NE.IDSTR(2)  .AND.   &
                 IDTST.NE.IDSTR(3)  .AND. IDTST.NE.IDSTR(4)  .AND.   &
                 IDTST.NE.IDSTR(5)  .AND. IDTST.NE.IDSTR(6)  .AND.   &
                 IDTST.NE.IDSTR(10)  .AND. IDTST.NE.'STP' ) GOTO 2005

            ! Stop conditions
            IF ( IDTST .EQ. 'STP' ) THEN
              EXIT
            ELSE
              BACKSPACE ( NDSI )
            END IF

            ! Store data
            DO J=LBOUND(IDSTR,1), 10
              IF ( IDTST .EQ. IDSTR(J) ) THEN
                NH(J)    = NH(J) + 1
                IF ( NH(J) .GT. NHMAX ) GOTO 2006
                IF ( J .LE. 1  ) THEN ! water levels, etc. : get HA
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J)
                ELSE IF ( J .EQ. 2 ) THEN ! currents: get HA and HD
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J), HD(NH(J),J)
                ELSE IF ( J .EQ. 3 ) THEN ! wind: get HA HD and HS
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J), HD(NH(J),J), HS(NH(J),J)
                ELSE IF ( J .EQ. 4 ) THEN ! ice
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J)
                ELSE IF ( J .EQ. 5 ) THEN ! atmospheric momentum
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J), HD(NH(J),j)
                ELSE IF ( J .EQ. 6 ) THEN ! air density
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J)
                ELSE IF ( J .EQ. 10 ) THEN ! mov: HA and HD
                  READ (NDSI,*) IDTST,           &
                        THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                        HA(NH(J),J), HD(NH(J),J)
                END IF
              END IF
            END DO
          END DO
#ifdef W3_O7
          DO J=JFIRST, 10
            IF ( FLH(J) .AND. IAPROC.EQ.NAPOUT ) THEN
              WRITE (NDSO,952) NH(J), IDFLDS(J)
              DO I=1, NH(J)
                IF ( ( J .LE. 1 ) .OR. ( J .EQ. 4 ) .OR.      &
                     ( J .EQ. 6 ) ) THEN
                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
                                   HA(I,J)
                ELSE IF ( ( J .EQ. 2 ) .OR. ( J .EQ. 5 ) .OR. &
                          ( J .EQ. 10 ) ) THEN
                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
                                   HA(I,J), HD(I,J)
                ELSE IF ( J .EQ. 3 ) THEN
                  WRITE (NDSO,953) I, THO(1,J,I), THO(2,J,I), &
                                   HA(I,J), HD(I,J), HS(I,J)
                END IF
              END DO
            END IF
          END DO
#endif
          IF ( ( FLH(-7) .AND. (NH(-7).EQ.0) ) .OR.                     &
               ( FLH(-6) .AND. (NH(-6).EQ.0) ) .OR.                     &
               ( FLH(-5) .AND. (NH(-5).EQ.0) ) .OR.                     &
               ( FLH(-4) .AND. (NH(-4).EQ.0) ) .OR.                     &
               ( FLH(-3) .AND. (NH(-3).EQ.0) ) .OR.                     &
               ( FLH(-2) .AND. (NH(-2).EQ.0) ) .OR.                     &
               ( FLH(-1) .AND. (NH(-1).EQ.0) ) .OR.                     &
               ( FLH(0)  .AND. (NH(0).EQ.0)  ) .OR.                     &
               ( FLH(1)  .AND. (NH(1).EQ.0)  ) .OR.                     &
               ( FLH(2)  .AND. (NH(2).EQ.0)  ) .OR.                     &
               ( FLH(3)  .AND. (NH(3).EQ.0)  ) .OR.                     &
               ( FLH(4)  .AND. (NH(4).EQ.0)  ) .OR.                     &
               ( FLH(5)  .AND. (NH(5).EQ.0)  ) .OR.                     &
               ( FLH(6)  .AND. (NH(6).EQ.0)  ) .OR.                     &
               ( FLH(10) .AND. (NH(10).EQ.0) ) ) GOTO 2007
        END IF ! FLHOM

       print *,'XXX end if not flgnml'
!      END IF ! if not flgnml
!
! ----------------
!

! 2.1 input fields

! 2.1.a Opening field and data files
!
!      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,950)
!      IF ( FLFLG ) THEN
!        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
!                                         'Preparing input files ...'
!!
!
!        DO J=JFIRST, 6
!          IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
!            IF ( FLH(J) ) THEN
!              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
!            ELSE
!              FLAGTIDE = 0
!              CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST,     &
!                            NDSEN, NX, NY, GTYPE,               &
!                            IERR, FPRE=TRIM(FNMPRE), TIDEFLAGIN=FLAGTIDE )
!              IF ( IERR .NE. 0 ) GOTO 2222
!              IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,955) IDFLDS(J)
!            END IF
!          ELSE
!            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
!          END IF
!        END DO
!!
!        DO J=7, 9
!          IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
!            CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST, NDSEN, &
!                         RCLD(J), NY, NODATA(J),                 &
!                         IERR, FPRE=TRIM(FNMPRE) )
!            IF ( IERR .NE. 0 ) GOTO 2222
!            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,956) IDFLDS(J),&
!                                             RCLD(J), NODATA(J)
!          ELSE
!            IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
!          END IF
!        END DO
!!
!      END IF ! FLFLG

! 2.2 Time setup

      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930)
      CALL STME21 ( TIME0 , DTME21 )
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,931) DTME21
      TIME = TIME0
      CALL STME21 ( TIMEN , DTME21 )
      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) DTME21
      DTTST  = DSEC21 ( TIME0 , TIMEN )
      IF ( DTTST .LE. 0. ) GOTO 2003

! 2.3 Domain setup

      IOSTYP = MAX ( 0 , MIN ( 3 , IOSTYP ) )
      IF ( IAPROC .EQ. NAPOUT ) THEN
        IF ( IOSTYP .EQ. 0 ) THEN
          WRITE (NDSO,940) 'No dedicated output process, ' //   &
                           'parallel file system required.'
        ELSE IF ( IOSTYP .EQ. 1 ) THEN
          WRITE (NDSO,940) 'No dedicated output process, ' //   &
                           'any file system.'
        ELSE IF ( IOSTYP .EQ. 2 ) THEN
          WRITE (NDSO,940) 'Single dedicated output process.'
        ELSE IF ( IOSTYP .EQ. 3 ) THEN
          WRITE (NDSO,940) 'Multiple dedicated output processes.'
        ELSE
          WRITE (NDSO,940) 'IOSTYP NOT RECOGNIZED'
        END IF
      END IF

! 2.4 Output dates

      DO J = 1, NOTYPE
        IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) J, IDOTYP(J)
          TTIME(1) = ODAT(5*(J-1)+1)
          TTIME(2) = ODAT(5*(J-1)+2)
          CALL STME21 ( TTIME , DTME21 )
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) DTME21
          TTIME(1) = ODAT(5*(J-1)+4)
          TTIME(2) = ODAT(5*(J-1)+5)
          CALL STME21 ( TTIME , DTME21 )
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) DTME21
          TTIME(1) = 0
          TTIME(2) = 0
          DTTST    = REAL ( ODAT(5*(J-1)+3) )
          CALL TICK21 ( TTIME , DTTST  )
          CALL STME21 ( TTIME , DTME21 )
          IF ( ( ODAT(5*(J-1)+1) .NE. ODAT(5*(J-1)+4) .OR.          &
                 ODAT(5*(J-1)+2) .NE. ODAT(5*(J-1)+5) ) .AND.       &
                 IAPROC .EQ. NAPOUT ) THEN
            IF ( DTME21(9:9) .NE. '0' ) THEN
              WRITE (NDSO,1944) DTME21( 9:19)
            ELSE IF ( DTME21(10:10) .NE. '0' ) THEN
              WRITE (NDSO,2944) DTME21(10:19)
            ELSE
              WRITE (NDSO,3944) DTME21(12:19)
            END IF
          END IF
        END IF
      END DO

! CHECKPOINT
      J=8
      IF (ODAT(38) .NE. 0) THEN
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,941) J, IDOTYP(J)
        TTIME(1) = ODAT(5*(J-1)+1)
        TTIME(2) = ODAT(5*(J-1)+2)
        CALL STME21 ( TTIME , DTME21 )
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,942) DTME21
        TTIME(1) = ODAT(5*(J-1)+4)
        TTIME(2) = ODAT(5*(J-1)+5)
        CALL STME21 ( TTIME , DTME21 )
        IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,943) DTME21
        TTIME(1) = 0
        TTIME(2) = 0
        DTTST    = REAL ( ODAT(5*(J-1)+3) )
        CALL TICK21 ( TTIME , DTTST  )
        CALL STME21 ( TTIME , DTME21 )
        IF ( ( ODAT(5*(J-1)+1) .NE. ODAT(5*(J-1)+4) .OR.          &
               ODAT(5*(J-1)+2) .NE. ODAT(5*(J-1)+5) ) .AND.       &
               IAPROC .EQ. NAPOUT ) THEN
          IF ( DTME21(9:9) .NE. '0' ) THEN
            WRITE (NDSO,1944) DTME21( 9:19)
          ELSE IF ( DTME21(10:10) .NE. '0' ) THEN
            WRITE (NDSO,2944) DTME21(10:19)
          ELSE
            WRITE (NDSO,3944) DTME21(12:19)
          END IF
        END IF
      END IF

! 2.5 Output types
! For outputs with non-zero time step, check dates :
! If output ends before run start OR output starts after run end,
! deactivate output cleanly with output time step = 0
! This is usefull for IOSTYP=3 (Multiple dedicated output processes)
! to avoid the definition of dedicated proc. for unused output.
!
      DO J = 1, NOTYPE
        DTTST  = DSEC21 ( TIME0 , ODAT(5*(J-1)+4:5*(J-1)+5) )
        IF ( DTTST .LT. 0 ) THEN
          ODAT(5*(J-1)+3) = 0
          IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
          CONTINUE
        END IF
        DTTST  = DSEC21 ( ODAT(5*(J-1)+1:5*(J-1)+2), TIMEN )
        IF ( DTTST .LT. 0 ) THEN
          ODAT(5*(J-1)+3) = 0
          IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
          CONTINUE
        END IF
      END DO
! CHECKPOINT
      J = 8
      DTTST  = DSEC21 ( TIME0 , ODAT(5*(J-1)+4:5*(J-1)+5) )
      IF ( DTTST .LT. 0 ) THEN
        ODAT(5*(J-1)+3) = 0
        IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
        CONTINUE
      END IF
      DTTST  = DSEC21 ( ODAT(5*(J-1)+1:5*(J-1)+2), TIMEN )
      IF ( DTTST .LT. 0 ) THEN
        ODAT(5*(J-1)+3) = 0
        IF ( IAPROC .EQ. NAPOUT )  WRITE (NDSO,8945) TRIM(IDOTYP(J))
        CONTINUE
      END IF

!--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 5.  Initializations

      IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) 'Wave model ...'
!
!     CALL W3INIT ( 1, .FALSE., 'ww3', NDS, NTRACE, ODAT, FLGRD, FLGR2, FLGD,    &
!                   FLG2, NPTS, X, Y, PNAMES, IPRT, PRTFRM, MPI_COMM,   &
!                   FLAGSTIDEIN=FLAGSTIDE )

!     End of shel
      GOTO 2222
!
! Error escape locations
!
 2001 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
      CALL EXTCDE ( 1001 )
 2002 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
      CALL EXTCDE ( 1002 )
 2003 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1003)
      CALL EXTCDE ( 1003 )
 2005 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1005) IDTST
      CALL EXTCDE ( 1005 )
 2054 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1054)
      CALL EXTCDE ( 1054 )
 2006 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1006) IDTST, NH(J)
      CALL EXTCDE ( 1006 )
 2007 CONTINUE
      IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1007)
      CALL EXTCDE ( 1007 )

 2222 CONTINUE

! Formats
  900 FORMAT (/15X,'      *** WAVEWATCH III Program shell ***      '/ &
               15X,'==============================================='/)
  901 FORMAT ( '  Comment character is ''',A,''''/)
  905 FORMAT ( '  Hybrid MPI/OMP thread support level:'/        &
               '     Requested: ', I2/                          &
               '      Provided: ', I2/ )
  920 FORMAT (/'  Input fields : '/                                   &
               ' --------------------------------------------------')
  921 FORMAT ( '       ',A,2X,A,2X,A)
  922 FORMAT ( ' ' )
  930 FORMAT (/'  Time interval : '/                                  &
               ' --------------------------------------------------')
  931 FORMAT ( '       Starting time : ',A)
  932 FORMAT ( '       Ending time   : ',A/)
  940 FORMAT (/'  Output requests : '/                                &
               ' --------------------------------------------------'/ &
               '       ',A)
  941 FORMAT (/'       Type',I2,' : ',A/                              &
               '      -----------------------------------------')
  942 FORMAT ( '            From     : ',A)
  943 FORMAT ( '            To       : ',A)
 1944 FORMAT ( '            Interval : ', 8X,A11/)
 2944 FORMAT ( '            Interval : ', 9X,A10/)
 2945 FORMAT ( '            Point  1 : ',2F8.2,2X,A)
 2955 FORMAT ( '            Point  1 : ',2(F8.1,'E3'),2X,A)
 2946 FORMAT ( '              ',I6,' : ',2F8.2,2X,A)
 2956 FORMAT ( '              ',I6,' : ',2(F8.1,'E3'),2X,A)
 2947 FORMAT ( '            No points defined')
 3945 FORMAT ( '            The file with ',A,' data is ',A,'.')
 6945 FORMAT ( '            IX first,last,inc :',3I5/                 &
               '            IY first,last,inc :',3I5/                 &
               '            Formatted file    :    ',A)
 3944 FORMAT ( '            Interval : ',11X,A8/)

 8945 FORMAT ( '            output dates out of run dates : ', A,     &
               ' deactivated')
  951 FORMAT ( '       ',A)
  952 FORMAT ( '       ',I6,2X,A)
  953 FORMAT ( '          ',I6,I11.8,I7.6,3E12.4)
 1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     PREMATURE END OF INPUT FILE'/)
 1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ERROR IN READING FROM INPUT FILE'/               &
               '     IOSTAT =',I5/)
 1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ILLEGAL TIME INTERVAL'/)
 1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ILLEGAL ID STRING HOMOGENEOUS FIELD : ',A/)
 1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     TOO MANY HOMOGENEOUS FIELDS : ',A,1X,I4/)
 1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     INSUFFICIENT DATA FOR HOMOGENEOUS FIELDS'/)
 1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     ERROR IN OPENING OUTPUT FILE'/                   &
               '     IOSTAT =',I5/)
 1054 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
               '     POINT OUTPUT ACTIVATED BUT NO POINTS DEFINED'/)
 end subroutine read_shel_inp
end module wav_shel_inp
