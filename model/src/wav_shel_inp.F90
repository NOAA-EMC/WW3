module wav_shel_inp

  use w3odatmd, only: nogrp, ngrpp

  implicit none
  private ! except

  public  :: set_shel_inp
  public  :: read_shel_inp

  integer, public :: odat(40)
  character(len=40), allocatable, public :: pnames(:)

  integer, public           :: npts
  integer, public           :: iprt(6)
  logical, public           :: prtfrm
  logical, public           :: flgrd(nogrp,ngrpp) !flags for gridded output
  logical, public           :: flgr2(nogrp,ngrpp) !flags for coupling output
  logical, public           :: flgd(nogrp)        !flags for whole group - not currently used in cesm
  logical, public           :: flg2(nogrp)        !flags for whole group - not currently used in cesm
  real, allocatable, public :: x(:), y(:)

  include "mpif.h"

!===============================================================================
contains
!===============================================================================

  subroutine set_shel_inp(dtime_sync)

    use w3idatmd    , only : inflags1, inflags2
    use w3odatmd    , only : noge, idout, nds, notype, iaproc, napout
    use w3wdatmd    , only : time
    use wav_shr_mod , only : wav_coupling_to_cice

    ! Input parameter
    integer , intent(in)  :: dtime_sync

    ! Local parameters
    logical :: flt
    integer :: i,j,j0
    !---------------------------------------------------

    !--------------------------------------------------------------------
    ! Define input fields inflags1 and inflags2 settings
    !--------------------------------------------------------------------

    !  fllev   inflags1(1)  flag for water level input.
    !  flcur   inflags1(2)  flag for current input.
    !  flwind  inflags1(3)  flag for wind input.
    !  flice   inflags1(4)  flag for ice input (ice fraction)

    !  inflags1 array consolidating the above flags, as well as four additional data flags.
    !  inflags2 like inflags1 but does *not* get changed when model reads last record of ice.ww3
    !  inflags2 is just "initial value of INFLAGS1"

    ! flags for passing variables from coupler to ww3, lev, curr, wind, ice and mixing layer depth
    ! ice params        : inflags1(-7) => inflags1(-3)
    ! mud density       : inflags1(-2)
    ! mud thickness     : inflags1(-1)
    ! muc viscos        : inflags1(0)
    ! water levels      : inflags1(1)
    ! currents          : inflags1(2)
    ! winds             : inflags1(3)
    ! ice fields        : inflags1(4)
    ! momentum fluxes   : inflags1(5)

    inflags1(:)   = .false.
    inflags1(1:4) = .true.
    inflags2(:)   = .false.
    if (wav_coupling_to_cice) then
       inflags1(-7) = .true. ! ice thickness
       inflags1(-3) = .true. ! ice floe size
       inflags2(-7) = .true. ! thickness
       inflags2(-3) = .true. ! floe size
       inflags2( 4) = .true. ! inflags2(4) is true if ice concentration was read during initialization
    end if

    !--------------------------------------------------------------------
    ! Define output type and fields
    !--------------------------------------------------------------------

    ! Set number of output types. This is nomally set in w3_shel, CMB made 7.
    notype = 7

    if (iaproc == napout) then
       write(nds(1),'(a)') '  Output requests : '
       write(nds(1),'(a)')'--------------------------------------------------'
       write(nds(1),'(a)')' no dedicated output process on any file system '
    end if

    ! Initialize ODAT. Normally set in w3_shel.
    ! ODAT is initializated in w3initmd
    ! Output data, five parameters per output type
    !       1 YYYMMDD for first output.
    !       2 HHMMSS for first output.
    !       3 Output interval in seconds.
    !       4 YYYMMDD for last output.
    !       5 HHMMSS for last output.
    !  1-5  Data for OTYPE = 1; gridded fields.
    !  6-10 Id.  for OTYPE = 2; point output.
    ! 11-15 Id.  for OTYPE = 3; track point output.
    ! 16-20 Id.  for OTYPE = 4; restart files.
    ! 21-25 Id.  for OTYPE = 5; boundary data.
    ! 26-30 Id.  for OTYPE = 6; ?
    ! 31-35 Id.  for OTYPE = 7; coupled fields
    ! Hardwire gridded output for now
    ! - first output time stamp is now read from file
    ! - 1-5 for history files, 16-20 for restart files
    ! - restart output interval is set to the total time of run, restart is taken over by rstwr
    ! - output interval is set to coupling interval, so that variables calculated in W3IOGO
    !   could be updated at every coupling interval
    ! - changed odat so all 35 values are set, only permitting one frequency controlled by histwr
    do j=1,7
       J0 = (j-1)*5
       odat(J0+1) = time(1)     ! YYYYMMDD for first output
       odat(J0+2) = time(2)     ! HHMMSS for first output
       odat(J0+3) = dtime_sync  ! output interval in sec
       odat(J0+4) = 99990101    ! YYYYMMDD for last output
       odat(J0+5) = 0           ! HHMMSS for last output
    end do

    ! FLGRD   L.A.   I   Flags for gridded output.
    ! NPT     Int.   I   Number of output points
    ! X/YPT   R.A.   I   Coordinates of output points.
    ! PNAMES  C.A.   I   Output point names.
    ! output index is now a in a 2D array

    flgrd(:,:)  = .false.   ! gridded fields
    flgr2(:,:)  = .false.   ! coupled fields, w3init w3iog are not ready to deal with these yet

    ! 1) Forcing fields
    flgrd( 1, 1)  = .false. ! Water depth
    flgrd( 1, 2)  = .false. ! Current vel.
    flgrd( 1, 3)  = .true.  ! Wind speed
    flgrd( 1, 4)  = .false. ! Air-sea temp. dif.
    flgrd( 1, 5)  = .false. ! Water level
    flgrd( 1, 6)  = .true.  ! Ice concentration
    flgrd( 1, 7)  = .false. ! Iceberg damp coeffic

    ! 2) Standard mean wave parameters
    flgrd( 2, 1)  = .true.  ! Wave height
    flgrd( 2, 2)  = .false. ! Mean wave length
    flgrd( 2, 3)  = .true.  ! Mean wave period(+2)
    flgrd( 2, 4)  = .true.  ! Mean wave period(-1)
    flgrd( 2, 5)  = .true.  ! Mean wave period(+1)
    flgrd( 2, 6)  = .true.  ! Peak frequency
    flgrd( 2, 7)  = .true.  ! Mean wave dir. a1b1
    flgrd( 2, 8)  = .false. ! Mean dir. spr. a1b1
    flgrd( 2, 9)  = .false. ! Peak direction
    flgrd( 2, 10) = .false. ! Infragravity height
    flgrd( 2, 11) = .false. ! Space-Time Max E
    flgrd( 2, 12) = .false. ! Space-Time Max Std
    flgrd( 2, 13) = .false. ! Space-Time Hmax
    flgrd( 2, 14) = .false. ! Spc-Time Hmax^crest
    flgrd( 2, 15) = .false. ! STD Space-Time Hmax
    flgrd( 2, 16) = .false. ! STD ST Hmax^crest
    flgrd( 2, 17) = .false. ! Dominant wave bT

    ! 3) Frequency-dependent standard parameters
    ! Whether the 1D Freq. Spectrum gets allocated is decided in the grid_inp file
    ! ~/ww3_toolbox/grids/grid_inp/ww3_grid.inp.ww3a namelist section:  &OUTS E3D = 1 /
    flgrd( 3, 1)  = .true.  ! 1D Freq. Spectrum
    flgrd( 3, 2)  = .false. ! Mean wave dir. a1b1
    flgrd( 3, 3)  = .false. ! Mean dir. spr. a1b1
    flgrd( 3, 4)  = .false. ! Mean wave dir. a2b2
    flgrd( 3, 5)  = .false. ! Mean dir. spr. a2b2
    flgrd( 3, 6)  = .false. ! Wavenumber array   '

    ! 4) Spectral Partitions parameters
    flgrd( 4, 1)  =  .false. ! Part. wave height   '
    flgrd( 4, 2)  =  .false. ! Part. peak period   '
    flgrd( 4, 3)  =  .false. ! Part. peak wave len.'
    flgrd( 4, 4)  =  .false. ! Part. mean direction'
    flgrd( 4, 5)  =  .false. ! Part. dir. spread   '
    flgrd( 4, 6)  =  .false. ! Part. wind sea frac.'
    flgrd( 4, 7)  =  .false. ! Part. peak direction'
    flgrd( 4, 8)  =  .false. ! Part. peakedness    '
    flgrd( 4, 9)  =  .false. ! Part. peak enh. fac.'
    flgrd( 4,10)  =  .false. ! Part. gaussian width'
    flgrd( 4,11)  =  .false. ! Part. spectral width'
    flgrd( 4,12)  =  .false. ! Part. mean per. (-1)'
    flgrd( 4,13)  =  .false. ! Part. mean per. (+1)'
    flgrd( 4,14)  =  .false. ! Part. mean per. (+2)'
    flgrd( 4,15)  =  .false. ! Part. peak density  '
    flgrd( 4,16)  =  .false. ! Total wind sea frac.'
    flgrd( 4,17)  =  .false. ! Number of partitions'

    ! 5) Atmosphere-waves layer
    flgrd( 5, 1)  = .false. ! Friction velocity   '
    flgrd( 5, 2)  = .false. ! Charnock parameter  '
    flgrd( 5, 3)  = .false. ! Energy flux         '
    flgrd( 5, 4)  = .false. ! Wind-wave enrgy flux'
    flgrd( 5, 5)  = .false. ! Wind-wave net mom. f'
    flgrd( 5, 6)  = .false. ! Wind-wave neg.mom.f.'
    flgrd( 5, 7)  = .false. ! Whitecap coverage   '
    flgrd( 5, 8)  = .false. ! Whitecap mean thick.'
    flgrd( 5, 9)  = .false. ! Mean breaking height'
    flgrd( 5,10)  = .false. ! Dominant break prob '
    flgrd( 5,11)  = .false. ! Breaker passage rate'

    ! 6) Wave-ocean layer
    flgrd( 6, 1)  = .false. ! 'Radiation stresses  '
    flgrd( 6, 2)  = .false. ! 'Wave-ocean mom. flux'
    flgrd( 6, 3)  = .false. ! 'wave ind p Bern Head'
    flgrd( 6, 4)  = .false. ! 'Wave-ocean TKE  flux'
    flgrd( 6, 5)  = .false. ! 'Stokes transport    '
    flgrd( 6, 6)  = .true.  ! 'Stokes drift at z=0 '
    flgrd( 6, 7)  = .false. ! '2nd order pressure  '
    flgrd( 6, 8)  = .false. ! 'Stokes drft spectrum'
    flgrd( 6, 9)  = .false. ! '2nd ord press spectr'
    flgrd( 6,10)  = .false. ! 'Wave-ice mom. flux  '
    flgrd( 6,11)  = .false. ! 'Wave-ice energy flux'
    flgrd( 6,12)  = .false. ! 'Split Surface Stokes'
    flgrd( 6,13)  = .false. ! 'Tot wav-ocn mom flux'
    flgrd( 6,13)  = .true.  ! 'Turbulent Langmuir number (La_t)'

    ! 7) Wave-bottom layer
    flgrd( 7, 1)  = .false. ! 'Bottom rms ampl.    '
    flgrd( 7, 2)  = .false. ! 'Bottom rms velocity '
    flgrd( 7, 3)  = .false. ! 'Bedform parameters  '
    flgrd( 7, 4)  = .false. ! 'Energy diss. in WBBL'
    flgrd( 7, 5)  = .false. ! 'Moment. loss in WBBL'

    ! 8) Spectrum parameters
    flgrd( 8, 1)  = .false. ! 'Mean square slopes  '
    flgrd( 8, 2)  = .false. ! 'Phillips tail const'
    flgrd( 8, 3)  = .false. ! 'Slope direction     '
    flgrd( 8, 4)  = .false. ! 'Tail slope direction'
    flgrd( 8, 5)  = .false. ! 'Goda peakedness parm'

    ! 9) Numerical diagnostics
    flgrd( 9, 1)  = .false. ! 'Avg. time step.     '
    flgrd( 9, 2)  = .false. ! 'Cut-off freq.       '
    flgrd( 9, 3)  = .false. ! 'Maximum spatial CFL '
    flgrd( 9, 4)  = .false. ! 'Maximum angular CFL '
    flgrd( 9, 5)  = .false. ! 'Maximum k advect CFL'

    ! 10) is user defined

    ! write out which fields will be output to first hist file
    ! IDOUT(NOGRP,NGRPP)
    !   NOGRP = number of output field groups
    !   NGRPP = Max num of parameters per output
    !   NOGE(NOGRP) = number of output group elements
    if (iaproc == napout) then
       flt = .true.
       do i=1, nogrp
          do j=1, noge(i)
             if ( flgrd(i,j) ) then
                if ( flt ) then
                   write (nds(1),'(a)') '            Fields   : '//trim(idout(i,j))
                   flt = .false.
                else
                   write (nds(1),'(a)')'                       '//trim(idout(i,j))
                end if
             end if
          end do
       end do
       if ( flt ) then
          write (nds(1),'(a)') '            Fields   : '//'no fields defined'
       end if
    end if

    ! npts, pnames are fpr point output
    allocate ( x(1), y(1), pnames(1) )
    npts = 0
    pnames(1) = ' '
    prtfrm = .false.

  end subroutine set_shel_inp

  !===============================================================================
  subroutine read_shel_inp(mpi_comm)

    USE W3GDATMD, ONLY: FLAGLL
    USE W3WDATMD, ONLY: TIME, VA, W3NDAT, W3DIMW, W3SETW
    USE W3ADATMD, ONLY: W3NAUX, W3DIMA, W3SETA
    USE W3IDATMD, ONLY: INFLAGS1, INFLAGS2, FLAGSC
    USE W3ODATMD, ONLY: W3NOUT, W3SETO, NDS
    USE W3ODATMD, ONLY: NAPROC, IAPROC, NAPOUT, NAPERR
    USE W3ODATMD, ONLY: IDOUT, FNMPRE, IOSTYP, NOTYPE
    USE W3ODATMD, ONLY: FLOGRR, FLOGR, OFILES
    USE W3IOGRMD, ONLY: W3IOGR
    USE W3IOGOMD, ONLY: W3READFLGRD, W3FLGRDFLAG
    USE W3SERVMD, ONLY: NEXTLN, EXTCDE
    USE W3TIMEMD, ONLY: DSEC21, STME21, TICK21

    INTEGER, INTENT(IN) :: MPI_COMM

    ! Local parameters
    INTEGER, PARAMETER  :: NHMAX =    200

    INTEGER             :: NDSI, NDSI2, NDSS, NDSO, NDSE, NDST, NDSL,&
                           NDSEN, IERR, J, I, ILOOP, IPTS
    INTEGER             :: NDSF(-7:9), &
                           NH(-7:10), THO(2,-7:10,NHMAX)
    INTEGER             :: jfirst, IERR_MPI
    REAL                :: FACTOR, DTTST, XX, YY, HA(NHMAX,-7:10), &
                           HD(NHMAX,-7:10), HS(NHMAX,-7:10)

    CHARACTER(LEN=1)    :: COMSTR, FLAGTFC(-7:10)
    CHARACTER(LEN=3)    :: IDSTR(-7:10), IDTST
    CHARACTER(LEN=6)    :: YESXNO
    CHARACTER(LEN=40)   :: PN
    CHARACTER(LEN=13)   :: IDFLDS(-7:10)
    CHARACTER(LEN=20)   :: STRNG
    CHARACTER(LEN=23)   :: DTME21
    CHARACTER(LEN=30)   :: IDOTYP(8)
    CHARACTER(LEN=80)   :: LINE
    CHARACTER(LEN=1024) :: FLDRST=''
    CHARACTER(LEN=80)   :: LINEIN
    CHARACTER(LEN=8)    :: WORDS(7)=''
    LOGICAL             :: FLFLG, FLHOM, TFLAGI, FLH(-7:10)
    INTEGER             :: THRLEV = 1
    INTEGER             :: TIME0(2), TIMEN(2), TTIME(2)

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
    !---------------------------------------------------
    !
    FLGR2 = .FALSE.
    FLH(:) = .FALSE.
    iprt(:) = 0

    ! IO setup comes next---do we want to move it from initreal?

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
    ! If using experimental mud or ice physics, additional lines will
    !  be read in from ww3_shel.inp and applied, so JFIRST is changed from
    !  its initialization setting "JFIRST=1" to some lower value.
    JFIRST=1

    ! process old ww3_shel.inp format
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
             if(iaproc .eq. naproc) WRITE(*,*)'odat(j=4): ',(ODAT(I),I=5*(8-1)+1,5*8)
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

                      IF ( .NOT.ALLOCATED(X) ) THEN
                         IF ( NPTS.GT.0 ) THEN
                            ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
                         ELSE
                            ALLOCATE ( X(1), Y(1), PNAMES(1) )
                            GOTO 2054
                         END IF
                      END IF
                   END IF

                   NPTS   = 0
                   DO
                      CALL NEXTLN ( COMSTR , NDSI , NDSEN )
                      READ (NDSI2,*) XX, YY, PN
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

    !      END IF ! if not flgnml

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
    GOTO 2222

    ! Error escape locations
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
