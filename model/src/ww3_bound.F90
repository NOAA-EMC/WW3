!> @file
!> @brief Contains program W3BOUND for boundary conditions.
!>
!> @author F. Ardhuin @date 21-Jul-2020

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!> @brief Combines spectra files into a nest.\ ww3 file for boundary conditions.
!>
!> @details Finds nearest points and performs linear interpolation.
!>  The initial conditions are written to the restart.ww3 using the
!>  subroutine W3IORS. Note that the name of the restart file is set
!>  in W3IORS.
!> @author F. Ardhuin @date 21-Jul-2020
PROGRAM W3BOUND
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           F. Ardhuin              |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         27-May-2021 |
  !/                  +-----------------------------------+
  !/
  !/    28-Aug-2012 : adaptation from SHOM/Ifremer code   ( version 4.08 )
  !/    01-Nov-2012 : Bug correction for NKI != NK        ( version 4.08 )
  !/    20-Oct-2016 : Error statement updates             ( version 5.15 )
  !/    21-Jul-2020 : Support rotated pole grid           ( version 7.11 )
  !/                                  Chris Bunney, UKMO.
  !/    27-May-2021 : Add namelist feature                ( version 7.XX )
  !/
  !/    Copyright 2012-2012 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Combines spectra files into a nest.ww3 file for boundary conditions
  !
  !  2. Method :
  !
  !     Finds nearest points and performs linear interpolation
  !
  !     The initial conditions are written to the restart.ww3 using the
  !     subroutine W3IORS. Note that the name of the restart file is set
  !     in W3IORS.
  !
  !  3. Parameters :
  !
  !     Local parameters.
  !     ----------------------------------------------------------------
  !       NDSI    Int.  Input unit number ("ww3_bound.inp").
  !       ITYPE   Int.  Type of data
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      STRACE    Subr.   Id.    Subroutine tracing.
  !      NEXTLN    Subr.   Id.    Get next line from input filw
  !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  !      WAVNU1    Subr. W3DISPMD Solve dispersion relation.
  !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  !      W3EQTOLL  Subr  W3SERVMD Convert coordinates from rotated pole.
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
  !     - Can be used also to diagnose contents of nest.ww3 file
  !       in read mode
  !
  !     - Input spectra are assumed to be formulated on a standard
  !       pole. However, the model grid can be on a rotated pole.
  !
  !  8. Structure :
  !
  !     ----------------------------------------------------
  !        1.a  Set up data structures.
  !                            ( W3NMOD , W3NDAT , W3NOUT
  !                              W3SETG , W3SETW , W3SETO )
  !          b  I-O setup.
  !        ....
  !        9.   Convert energy to action
  !       10.   Write restart file.              ( W3IORS )
  !     ----------------------------------------------------
  !
  !  9. Switches :
  !
  !     !/SHRD  Switch for shared / distributed memory architecture.
  !     !/DIST  Id.
  !
  !     !/SHRD  Switch for message passing method.
  !     !/MPI   Id.
  !
  !     !/S     Enable subroutine tracing.
  !
  !     !/O4    Output normalized 1-D energy spectrum.
  !     !/O5    Output normalized 2-D energy spectrum.
  !     !/O6    Output normalized wave heights (not MPP adapted).
  !
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  USE CONSTANTS
  USE W3WDATMD, ONLY: W3NDAT, W3SETW
  USE W3ADATMD, ONLY: W3NAUX, W3SETA
  USE W3ODATMD, ONLY: W3NOUT, W3SETO, FLBPI

  USE W3GDATMD, ONLY: NK, NTH, XFR, FR1, GNAME, W3NMOD, W3SETG, &
       NSEA, MAPSTA, GTYPE, XGRD, YGRD, X0, Y0, &
       SX, SY, MAPSF, UNGTYPE, CLGTYPE, RLGTYPE
#ifdef W3_RTD
  USE W3GDATMD, ONLY : POLAT, POLON
#endif
  USE W3ODATMD, ONLY: NDSO, NDSE, FNMPRE
  USE W3IOBCMD, ONLY: VERBPTBC, IDSTRBC
  USE W3IOGRMD, ONLY: W3IOGR
  USE W3TIMEMD
  USE W3NMLBOUNDMD
  USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
#ifdef W3_RTD
  USE W3SERVMD, ONLY: W3EQTOLL
#endif
#ifdef W3_S
  USE W3SERVMD, ONLY : STRACE
#endif
  !/
  IMPLICIT NONE
  !
#ifdef W3_MPI
  INCLUDE "mpif.h"
#endif
  !/
  !/ ------------------------------------------------------------------- /
  !/ Local parameters
  !/
  TYPE(NML_BOUND_T)       :: NML_BOUND
  !
  INTEGER                 :: TIME1(2)      !< initial time
  INTEGER                 :: TIME2(2)      !< next time
  INTEGER                 :: IX            !< x index
  INTEGER                 :: IY            !< y index
  INTEGER                 :: ISEA          !< isea index
  INTEGER                 :: I             !< i index
  INTEGER                 :: JJ            !< jj index
  INTEGER                 :: IP            !< boundary index
  INTEGER                 :: IP1           !< boundary index
  INTEGER                 :: J             !< j index
  INTEGER                 :: K             !< k index
  INTEGER                 :: ITIME         !< time index
  INTEGER                 :: NDSI          !< input file unit
  INTEGER                 :: NDSM          !< mod_def file unit
  INTEGER                 :: NDSI2         !< tmp input file unit
  INTEGER                 :: NDSS          !< scratch file unit
  INTEGER                 :: NDSB          !< nest file unit
  INTEGER                 :: NDSTRC        !< trace file unit
  INTEGER                 :: NTRACE        !< trace file unit
  INTEGER                 :: NK1           !< frequency bins number
  INTEGER                 :: NTH1          !< direction bins number
  INTEGER                 :: NSPEC1        !< spectral bins number
  INTEGER                 :: NBI           !< number of input boundary points
  INTEGER                 :: NBI2          !< number of input boundary points
  INTEGER                 :: NKI           !< frequency bins number from input spec file
  INTEGER                 :: NTHI          !< direction bins number from input spec file
  INTEGER                 :: NBO           !< number of boundary outputs
  INTEGER                 :: NBO2          !< number of boundary outputs
  INTEGER                 :: IERR          !< error code
  INTEGER                 :: INTERP        !< interpolation method
  INTEGER                 :: ILOOP         !< loop indice
  INTEGER                 :: IFMIN         !< min freq value from input spec file
  INTEGER                 :: IFMIN2        !< min freq value for output spectra
  INTEGER                 :: IFMAX         !< max freq value from input spec file
  INTEGER                 :: VERBOSE       !< verbose flag
  INTEGER                 :: NDSL          !< input spec listing file unit
#ifdef W3_S
  INTEGER, SAVE           :: IENT = 0      !< strace error code
#endif
  INTEGER                 :: IBO           !< indice of boundary output
  INTEGER,  ALLOCATABLE   :: IPBPI(:,:)    !< interp. data input bound. point
  INTEGER,  ALLOCATABLE   :: IPBPO(:,:)    !< interp. data output bound. point
  !
  REAL                    :: FR1I          !< first freq from mod_def
  REAL                    :: TH1I          !< first dir. from mod_def
  REAL                    :: depth         !< depth from input spec
  REAL                    :: U10           !< wind speed from input spec
  REAL                    :: Udir          !< wind dir. from input spec
  REAL                    :: Curr          !< cur. speed from input spec
  REAL                    :: Currdir       !< cur. dir. from input spec
  REAL                    :: DMIN          !< min. dist from grid point to spec
  REAL                    :: DIST          !< dist from grid point to spec
  REAL                    :: DMIN2         !< second min. dist from grid point to spec
  REAL                    :: COS1          !< cosine for linear interpolation
  !
  REAL, ALLOCATABLE       :: LATS(:)       !< latitude coordinates of spec
  REAL, ALLOCATABLE       :: LONS(:)       !< longitude coordiantes of spec
  REAL, ALLOCATABLE       :: SPEC2D(:,:)   !< spectrum from input spec
  REAL, ALLOCATABLE       :: FREQ(:)       !< frequency array from input spec
  REAL, ALLOCATABLE       :: THETA(:)      !< direction array from input spec
  REAL, ALLOCATABLE       :: XBPI(:)       !< x position of input boundary
  REAL, ALLOCATABLE       :: YBPI(:)       !< y position of input boundary
  REAL, ALLOCATABLE       :: RDBPI(:,:)    !< interpolation factor input boundray point
  REAL, ALLOCATABLE       :: XBPO(:)       !< x position of output bound. point
  REAL, ALLOCATABLE       :: YBPO(:)       !< y position of output bound. point
  REAL, ALLOCATABLE       :: RDBPO(:,:)    !< interp. factor output bound. point
  REAL, ALLOCATABLE       :: ABPIN(:,:)    !< intepolated spectrum
#ifdef W3_RTD
  REAL, ALLOCATABLE       :: XTMP(:)       !< temporary x position
  REAL, ALLOCATABLE       :: YTMP(:)       !< temporary y position
  REAL, ALLOCATABLE       :: ANGTMP(:)     !< temporary angle
  LOGICAL                 :: ISRTD         !< rotated grid flag
#endif
  CHARACTER               :: COMSTR        !< comment character
  CHARACTER(LEN=80)       :: LINE          !< input file line
  CHARACTER(LEN=128)      :: BNDFILE       !< input boundary file name
  CHARACTER(LEN=5)        :: INXOUT        !< read/write mode
  CHARACTER(LEN=10)       :: VERTEST       !< date of last nest.ww3 change
  CHARACTER(LEN=32)       :: IDTST         !< 'WAVEWATCH III BOUNDARY DATA FILE'
  CHARACTER(LEN=120)      :: FILENAME      !< input boundary file name
  CHARACTER(LEN=120)      :: string1       !< temporary string
  CHARACTER(LEN=120)      :: buoyname      !< input boundary point name
  CHARACTER               :: space         !< space character
  !
  CHARACTER(LEN=120), ALLOCATABLE :: SPECFILES(:)  !< list of input boundary points file names
  !
  LOGICAL                  :: FLGNML       !< flag for namelist use
  !/
  !/ ------------------------------------------------------------------- /


  !/
  ! 1.  IO set-up.
  !
  CALL W3NMOD ( 1, 6, 6 )
  CALL W3SETG ( 1, 6, 6 )
  CALL W3NDAT (    6, 6 )
  CALL W3SETW ( 1, 6, 6 )
  CALL W3NAUX (    6, 6 )
  CALL W3SETA ( 1, 6, 6 )
  CALL W3NOUT (    6, 6 )
  CALL W3SETO ( 1, 6, 6 )
  !
  NDSI   = 10
  NDSB   = 33
  NDSM   = 20
  NDSS   = 40
  NDSL   = 50
  !
  NDSTRC =  6
  NTRACE = 10
  CALL ITRACE ( NDSTRC, NTRACE )
  !
#ifdef W3_S
  CALL STRACE (IENT, 'W3BOUND')
#endif
  !

  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 2.  Read model definition file.
  !
  CALL W3IOGR ( 'READ', NDSM )
  WRITE (NDSO,920) GNAME
#ifdef W3_RTD
  !
  ISRTD = POLAT .LT. 90.0
  !
#endif
  !
  ! 3. Read input file
  !
  ! process ww3_bound namelist
  !
  INQUIRE(FILE=TRIM(FNMPRE)//"ww3_bound.nml", EXIST=FLGNML)
  IF (FLGNML) THEN
    ! Read namelist
    CALL W3NMLBOUND (NDSI, TRIM(FNMPRE)//'ww3_bound.nml', NML_BOUND, IERR)

    INXOUT = NML_BOUND%MODE
    INTERP = NML_BOUND%INTERP
    VERBOSE = NML_BOUND%VERBOSE
    BNDFILE = NML_BOUND%FILE

    NBO2 = 0
    OPEN(NDSL,FILE=TRIM(BNDFILE),STATUS='OLD',ERR=809,IOSTAT=IERR)
    REWIND (NDSL)
    DO
      READ (NDSL,*,END=400,ERR=802)
      NBO2 = NBO2 + 1
    END DO
400 CONTINUE
    ALLOCATE(SPECFILES(NBO2))
    REWIND (NDSL)
    DO I=1,NBO2
      READ (NDSL,'(A120)',END=801,ERR=802) SPECFILES(I)
    END DO
    CLOSE(NDSL)

  END IF ! FLGNML

  !
  ! process old ww3_bound.inp format
  !
  IF (.NOT. FLGNML) THEN
    OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_bound.inp',STATUS='OLD',ERR=803,IOSTAT=IERR)
    REWIND (NDSI)

    READ (NDSI,'(A)',END=801,ERR=802) COMSTR
    IF (COMSTR.EQ.' ') COMSTR = '$'


    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*) INXOUT
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*) INTERP
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    READ (NDSI,*) VERBOSE
    CALL NEXTLN ( COMSTR , NDSI , NDSE )
    !
    NBO2 = 0
    !
    !       ILOOP = 1 to count NBO2
    !       ILOOP = 2 to read the file names
    !
    DO ILOOP = 1, 2
      OPEN (NDSS,FILE='ww3_bound.scratch',FORM='FORMATTED',          &
           status='UNKNOWN')
      IF ( ILOOP .EQ. 1 ) THEN
        NDSI2 = NDSI
      ELSE
        NDSI2 = NDSS
        ALLOCATE(SPECFILES(NBO2))
        NBO2=0
      ENDIF

      NBO2=0
      !         Read input file names
      DO
        CALL NEXTLN ( COMSTR , NDSI2 , NDSE )
        READ (NDSI2,'(A120)') FILENAME
        JJ     = LEN_TRIM(FILENAME)
        IF ( ILOOP .EQ. 1 ) THEN
          BACKSPACE (NDSI)
          READ (NDSI,'(A)') LINE
          WRITE (NDSS,'(A)') LINE
        END IF
        IF (FILENAME(:JJ).EQ."'STOPSTRING'") EXIT
        NBO2=NBO2+1
        IF (ILOOP.EQ.1) CYCLE
        SPECFILES(NBO2)=FILENAME
      END DO

      IF ( ILOOP .EQ. 1 ) CLOSE ( NDSS)

      IF ( ILOOP .EQ. 2 ) CLOSE ( NDSS, STATUS='DELETE' )
    END DO ! ILOOP = 1, 2
    CLOSE(NDSI)
  END IF ! .NOT. FLGNML


  !
  !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ! 4. Tests the reading of the file
  !
  IF ( INXOUT.EQ.'READ') THEN
    OPEN(NDSB,FILE='nest.ww3',form='UNFORMATTED', convert=file_endian,status='old')
    READ(NDSB) IDTST, VERTEST, NK1, NTH1, XFR, FR1I, TH1I, NBI
    NSPEC1  = NK1 * NTH1
    IF ( IDTST .NE. IDSTRBC ) THEN
      WRITE (NDSO,901) IDTST, IDSTRBC
    END IF
    WRITE(NDSO,*) "FORMAT VERSION: '",VERTEST,"'"
    WRITE(NDSO,*) "FILE TYPE: '",IDTST,"'"
    IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,2I5,3F12.6,I5)') 'NK,NTH,XFR, FR1I, TH1I, NBI :', &
         NK1,NTH1,XFR, FR1I, TH1I, NBI
    ALLOCATE (XBPI(NBI),YBPI(NBI))
    ALLOCATE (IPBPI(NBI,4),RDBPI(NBI,4))
    READ(NDSB) (XBPI(I),I=1,NBI),           &
         (YBPI(I),I=1,NBI),             &
         ((IPBPI(I,J),I=1,NBI),J=1,4),  &
         ((RDBPI(I,J),I=1,NBI),J=1,4)
    IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'XBPI:',XBPI
    IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'YBPI:',YBPI
    IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'IPBPI:'
    DO I=1,NBI
      IF (VERBOSE.EQ.1) WRITE(NDSO,*) I,' interpolated from:',IPBPI(I,1:4)
      IF (VERBOSE.EQ.1) WRITE(NDSO,*) I,' with coefficient :',RDBPI(I,1:4)
    END DO
    !
    READ (NDSB) TIME2, NBI2
    BACKSPACE (NDSB)
    ALLOCATE (ABPIN(NSPEC1,NBI2))
    IERR=0
    DO WHILE (IERR.EQ.0)
      READ (NDSB,IOSTAT=IERR) TIME2, NBI2
      IF (IERR.EQ.0) THEN
        IF (VERBOSE.EQ.1) WRITE(NDSO,*)      'TIME2,NBI2:',TIME2, NBI2,IERR
        DO IP=1, NBI2
          READ (NDSB,IOSTAT=IERR) ABPIN(:,IP)
        END DO
      END IF
    END DO
    CLOSE(NDSB)
  END IF
  !
  !
  IF ( INXOUT.EQ.'WRITE') THEN
    IF ( FLBPI) THEN
      !
      !  Defines position of active boundary points
      !
      NBO = 0
      DO ISEA=1,NSEA
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        IF (MAPSTA(IY,IX).EQ.2) THEN
          NBO=NBO+1
        END IF
      END DO
      ALLOCATE(XBPO(NBO),YBPO(NBO))
#ifdef W3_RTD
      IF (ISRTD) ALLOCATE(XTMP(NBO), YTMP(NBO), ANGTMP(NBO))
#endif
      ALLOCATE (IPBPO(NBO,4),RDBPO(NBO,4))
      IBO=0
      DO ISEA=1,NSEA
        IX     = MAPSF(ISEA,1)
        IY     = MAPSF(ISEA,2)
        IF (MAPSTA(IY,IX).EQ.2) THEN
          IBO=IBO+1
          SELECT CASE ( GTYPE )
          CASE ( RLGTYPE )
            XBPO(IBO)=X0+SX*(IX-1)
            YBPO(IBO)=Y0+SY*(IY-1)
          CASE ( CLGTYPE )
            XBPO(IBO)= XGRD(IY,IX)
            YBPO(IBO)= YGRD(IY,IX)
          CASE (UNGTYPE)
            XBPO(IBO)= XGRD(1,IX)
            YBPO(IBO)= YGRD(1,IX)
          END SELECT !GTYPE
        END IF
      END DO
#ifdef W3_RTD
      ! Convert grid boundary cell locations to standard pole
      IF( ISRTD ) THEN
        XTMP = XBPO
        YTMP = YBPO
        CALL W3EQTOLL(YTMP, XTMP, YBPO, XBPO, ANGTMP, POLAT, POLON, NBO)
        DEALLOCATE(XTMP, YTMP, ANGTMP)
      ENDIF
#endif
      OPEN(NDSB,FILE='nest.ww3',form='UNFORMATTED', convert=file_endian,status='unknown')
      ALLOCATE(LATS(NBO2),LONS(NBO2))
      DO IP=1,NBO2
        OPEN(200+IP,FILE=SPECFILES(IP),status='old',iostat=IERR)
        IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,I5,3A,I5)') &
             'IP, file, I/O stat:',IP,', ', &
             TRIM(SPECFILES(IP)), ', ',IERR
        IF (IERR.NE.0) GOTO 810
        READ(200+IP,'(A1,A22,A1,X,2I6)',iostat=IERR)  &
             space,string1,space,NKI,NTHI
        IF (VERBOSE.EQ.1) WRITE(NDSO,'(A,3I5)') 'IP and spectral dimensions:',IP, NKI,NTHI
        IF (IP.EQ.1) THEN
          NK1=NKI
          NTH1=NTHI
          NSPEC1  = NK1 * NTH1
          ALLOCATE (FREQ(NK1),THETA(NTH1))
          ALLOCATE (SPEC2D(NK1,NTH1))
          ALLOCATE (ABPIN(NK*NTH1,NBO2))
        ELSE
          !
          !  To be cleaned up later ...
          !
          IF (NK1.NE.NKI.OR.NTH1.NE.NTHI) THEN
            WRITE(NDSE,'(A,A,4I5)') 'ERROR, SPECTRAL GRID IN FILE:',     &
                 TRIM(SPECFILES(IP)),NK1,NKI,NTH1,NTHI
            STOP
          END IF
        END IF
        !
        READ(200+IP,*) FREQ(1:NK1)
        READ(200+IP,*) THETA(1:NTH1)
      END DO

      !
      ! Defines frequency range in spectra
      !

      ! Checks consistency of NK
      IF (NKI.GT.NK) GOTO 808
      !
      ! HERE we define IFMIN IFMIN2 IFMAX and IFMAX2 frequency indices
      ! such that source spec SPEC (read in input) links with output spec
      ! APBIN with APBIN(IFMIN2:IFMAX2) = SPEC(IFMIN:IFMAX)
      ! Then APBIN(1:IFMIN2) = 0 and APBIN(IFMAX2:end) = 0
      IFMIN=1  ! index of first freq. in source spectrum
      IFMIN2=1 ! index of first freq. in output spectrum
      IFMAX=NK1 ! index of last freq. in source spectrum
      !        IFMAX2=NK ! index of last freq. in output spectrum
      !
      ! Checks consistency of XFR
      IF (ABS((FREQ(IFMIN+1)/FREQ(IFMIN))-XFR).GT.0.005) GOTO 806
      !
      ! Checks consistency of NTH
      ! WARNING: check is only done on number of directions, no check
      ! is done on the relative offset of first direction in terms of
      ! the directional increment [-0.5,0.5] (last parameter of the
      ! spectral definition in ww3_grid.inp, on second active line)
      IF (NTHI.NE.NTH) GOTO 807

      IF ((FR1-FREQ(1))/FR1.GT. 0.03) THEN
        DO J=1,MIN(NK1,NK)
          IF (ABS(FREQ(J)-FR1) .LT. ABS(FREQ(IFMIN)-FR1)) THEN
            IFMIN=J
          END IF
        END DO
      END IF
      !
      IF ((FREQ(1)-FR1)/FR1.GT. 0.03) THEN
        DO J=1,MIN(NK,NK1)
          IF (ABS(FREQ(J)-FR1*XFR**(J-1)) .LT. ABS(FREQ(IFMIN2)-FR1)) THEN
            IFMIN2=J
          END IF
        END DO
      END IF
      !
      IF ((FREQ(NK1)-FR1*XFR**(NK-1))/FREQ(NK1) .GT.0.03) THEN
        DO J=1,NK
          IF (ABS(FREQ(J)-FR1*XFR**(NK1-1)) .LT. ABS(FREQ(IFMAX)-FR1*XFR**(NK1-1))) THEN
            IFMAX=J
          END IF
        END DO
      END IF
      !
      IERR=0
      ITIME=0
      !
      ! Loop on times
      !
      DO WHILE (IERR.EQ.0)
        DO IP=1,NBO2
          READ(200+IP,*,IOSTAT=IERR) TIME2
          IF (IERR.EQ.0) THEN
            !
            IF (IP.EQ.1) THEN
              TIME1=TIME2
            ELSE
              IF (TIME1(1).NE.TIME2(1).OR.TIME1(2).NE.TIME2(2)) THEN
                WRITE(NDSE,*) 'AT POINT ',IP,', BAD TIMES:',TIME1, TIME2
              END IF
            END IF
            !
            READ(200+IP,'(A1,A10,A1,2F7.2,F10.1,F7.2,F6.1,F7.2,F6.1)') &
                 space,buoyname,space,LATS(IP),LONS(IP),depth,U10,Udir,Curr,Currdir
#ifdef W3_RTD
            IF (ISRTD) THEN
              ! Rotated coordinates are scaled in range 0 - 360
              IF(LONS(IP) .LT. 0) LONS(IP) = LONS(IP) + 360.0
              IF(LONS(IP) .GT. 360) LONS(IP) = LONS(IP) - 360.0
            ENDIF
#endif
            READ(200+IP,*,IOSTAT=IERR) SPEC2D
            IF (IFMIN2.GT.1) THEN
              !
              !  Fills in the low frequency end of the spectrum
              !
              ABPIN(1:(IFMIN2-1)*NTH,IP)=0.
            END IF
            DO I=IFMIN,IFMAX
              DO J=1,NTH
                ABPIN((I-IFMIN+(IFMIN2-1))*NTH+J,IP)=SPEC2D(I,J)*tpiinv
              END DO
            END DO
            IF (IFMAX-IFMIN+IFMIN2.LT.NK1) THEN
              !IF (VERBOSE.EQ.1) WRITE(NDSO,*) 'FILLING TAIL',IFMAX-IFMIN,NK1,IFMAX-IFMIN+(IFMIN2-1)
              ABPIN((IFMAX-IFMIN+IFMIN2)*NTH+1:NK1*NTH,IP)=0.
            END IF
          END IF  ! ned of test on IERR
        END DO
        !
        ! Writes header
        !
        IF (IERR.EQ.0) THEN
          IF (ITIME.EQ.0) THEN
            ! Correction for rounding error in ASCII files ...
            IF (ABS(THETA(1)-0.5*PI).LT.0.01) THETA(1)=0.5*PI
            ! Writes header in nest.ww3 file
            WRITE(NDSB) IDSTRBC, VERBPTBC, NK1, NTH, XFR, FREQ(1), &
                 MOD(2.5*PI-THETA(1),TPI), NBO
            IPBPO(:,:)=1
            RDBPO(:,1)=1.
            RDBPO(:,2:4)=0.
            !
            DO IP1=1,NBO
              DMIN=360.+180.
              DMIN2=360.+180.
              DO IP=1,NBO2
                !
                ! Searches for the nearest 2 points where spectra are available
                !
                DIST=SQRT((LONS(IP)-XBPO(IP1))**2+(LATS(IP)-YBPO(IP1))**2)
                IF (DMIN.EQ.(360.+180.)) THEN
                  IF(DIST.LT.DMIN) THEN
                    IPBPO(IP1,1)=IP
                    DMIN=DIST
                  END IF
                ELSE
                  IF(DIST.LT.DMIN2) THEN
                    IF(DIST.LT.DMIN) THEN
                      IPBPO(IP1,2)=IPBPO(IP1,1)
                      DMIN2=DMIN
                      IPBPO(IP1,1)=IP
                      DMIN=DIST
                    ELSE
                      IPBPO(IP1,2)=IP
                      DMIN2=DIST
                    END IF
                  ENDIF
                END IF
              END DO
              !IF (VERBOSE.EQ.1)  WRITE(NDSO,*) 'DIST:',DMIN,DMIN2,IP1,IPBPO(IP1,1),IPBPO(IP1,2), &
              !                    LONS(IPBPO(IP1,1)),LONS(IPBPO(IP1,2)),XBPO(IP1), &
              !                    LATS(IPBPO(IP1,1)),LATS(IPBPO(IP1,2)),YBPO(IP1)
              !
              !  Computes linear interpolation coefficient between the nearest 2 points
              !
              IF (INTERP.GT.1.AND.NBO2.GT.1) THEN
                DIST=SQRT((LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))**2   &
                     +(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2)))**2)
                COS1=( (XBPO(IP1)-LONS(IPBPO(IP1,1)))  &
                     *(LONS(IPBPO(IP1,2))-LONS(IPBPO(IP1,1))) &
                     + (YBPO(IP1)-LATS(IPBPO(IP1,1)))  &
                     *(LATS(IPBPO(IP1,2))-LATS(IPBPO(IP1,1))))/(DIST**2)
                !COS2=( (XBPO(IP1)-LONS(IPBPO(IP1,2)))  &
                !      *(LONS(IPBPO(IP1,1))-LONS(IPBPO(IP1,2)))
                !      + (YBPO(IP1)-LATS(IPBPO(IP1,2)))  &
                !      *(LATS(IPBPO(IP1,1))-LATS(IPBPO(IP1,2))))/(DIST**2)
                RDBPO(IP1,1)=1-MIN(1.,MAX(0.,COS1))
                RDBPO(IP1,2)=MIN(1.,MAX(0.,COS1))
              END IF
              !
              IF (VERBOSE.EQ.1) WRITE(NDSO,*) 'IPBP:',IP1,(IPBPO(IP1,J),J=1,4)
              IF (VERBOSE.EQ.1) WRITE(NDSO,*) 'RDBP:',IP1,(RDBPO(IP1,J),J=1,4)
              !
            END DO
            WRITE(NDSB) (XBPO(I),I=1,NBO),           &
                 (YBPO(I),I=1,NBO),           &
                 ((IPBPO(I,J),I=1,NBO),J=1,4),&
                 ((RDBPO(I,J),I=1,NBO),J=1,4)
          END IF

          WRITE(NDSO,*) 'Writing boundary data for time:', TIME2, NBO2
          WRITE(NDSB,IOSTAT=IERR) TIME2, NBO2
          DO IP=1, NBO2
            WRITE (NDSB) ABPIN(:,IP)
          END DO

          ITIME=ITIME+1
        END IF
      END DO
      CLOSE(NDSB)
    END IF
  END IF
  STOP
  !
  ! Escape locations read errors :
  !

801 CONTINUE
  WRITE (NDSE,1001)
  CALL EXTCDE ( 61 )
  !
802 CONTINUE
  WRITE (NDSE,1002) IERR
  CALL EXTCDE ( 62 )
  !
803 CONTINUE
  WRITE (NDSE,1003)
  CALL EXTCDE ( 63 )
  !
806 CONTINUE
  WRITE (NDSE,1006) XFR
  CALL EXTCDE ( 66 )
  !
807 CONTINUE
  WRITE (NDSE,1007) NTH, NTHI
  CALL EXTCDE ( 67 )
  !
808 CONTINUE
  WRITE (NDSE,1008) NK, NKI
  CALL EXTCDE ( 68 )
  !
809 CONTINUE
  WRITE (NDSE,1009) BNDFILE, IERR
  CALL EXTCDE ( 69 )
  !
810 CONTINUE
  WRITE (NDSE,1010) SPECFILES(IP)
  CALL EXTCDE ( 70 )

  !
  ! Formats
  !
901 FORMAT (/' *** WAVEWATCH-III ERROR IN W3IOBC :'/                &
       '     ILEGAL IDSTR, READ : ',A/                        &
       '                  CHECK : ',A/)
  !
920 FORMAT ( '  Grid name : ',A/)
  !
1001 FORMAT (/' *** WAVEWATCH-III ERROR IN W3BOUND : '/              &
       '     PREMATURE END OF INPUT FILE'/)
  !
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
       '     ERROR IN READING ',A,' FROM INPUT FILE'/         &
       '     IOSTAT =',I5/)
  !
1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUNC : '/              &
       '     ERROR IN OPENING INPUT FILE: ', A/               &
       '     IOSTAT =',I5/)
  !
1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
       '     ILLEGAL XFR, XFR =',F12.6/)
  !
1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
       '     ILLEGAL NTH, NTH =',I3,' DIFFERS FROM NTHI =',I3/)
  !
1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND: '/               &
       '     ILLEGAL NK, NK =',I3,' DIFFERS FROM NKI =',I3/   &
       '     IT WILL BE MANAGED SOON BY SPCONV')
  !
1009 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND : '/              &
       '     ERROR IN OPENING SPEC FILE: ', A/                &
       '     IOSTAT =',I5/)
  !
1010 FORMAT (/' *** WAVEWATCH III ERROR IN W3BOUND : '/              &
       '     SPEC FILE NOT EXISTING: ', A/)


  !
  !/
  !/ End of W3BOUND ---------------------------------------------------- /
  !/
END PROGRAM W3BOUND
!/ ------------------------------------------------------------------- /
