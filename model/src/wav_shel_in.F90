!> @file wav_shel_inp
!!
!>  Set up for running in shel mode
!!
!> @details Contains public routines to sets up IO unit numbers and to
!! either reads a shel.inp file (UWM) or set the required values directly
!! (CESM).
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
module wav_shel_inp

  use w3odatmd, only: nogrp, ngrpp

  implicit none
  private ! except

  public  :: set_shel_io      !< @public set the IO unit numbers
  public  :: set_shel_inp     !< @public directly set required input variabls (CESM)
  public  :: read_shel_inp    !< @public read ww3_shel.inp (UWM)

  integer, public :: odat(40) !< @public output dates
  character(len=40), allocatable, public :: pnames(:) !< @public point names

  integer, public           :: npts               !< @public number of points for point output
  integer, public           :: iprt(6)            !< @public partitioning grid information
  logical, public           :: prtfrm             !< @public partitioning format flag
  logical, public           :: flgrd(nogrp,ngrpp) !< @public flags for gridded output
  logical, public           :: flgr2(nogrp,ngrpp) !< @public flags for coupling output
  logical, public           :: flgd(nogrp)        !< @public flags for whole group - not currently used in cesm
  logical, public           :: flg2(nogrp)        !< @public flags for whole group - not currently used in cesm
  real, allocatable, public :: x(:)               !< @public x locations for point output
  real, allocatable, public :: y(:)               !< @public y locations for point output

  include "mpif.h"

!===============================================================================
contains
!===============================================================================
!> Set IO unit numbers
!!
!! @param[in]    stdout           unit number for stdout
!! @param[out]   mds              an array of 13 unit numbers
!! @param[out]   ntrace           an array of 2 unit numbers used for trace output
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
  subroutine set_shel_io(stdout,mds,ntrace)

    use ESMF, only : ESMF_UtilIOUnitGet

    ! Input parameter
    integer , intent(in)   :: stdout
    integer , intent(out)  :: mds(13), ntrace(2)

    ! Note that nds is set to mds in w3initmd.F90 - mds is a local array
    ! The following units are referenced in module w3initmd
    ! NDS(1) ! OUTPUT LOG: General output unit number ("log file")
    ! NDS(2) ! OUTPUT LOG: Error output unit number
    ! NDS(3) ! OUTPUT LOG: Test output unit number
    ! NDS(4) ! OUTPUT LOG: Unit for 'direct' output (SCREEN)
    ! NDS(5) ! INPUT: mod_def.ww3 file (model definition) unit number
    ! NDS(9) ! INPUT: unit for read in boundary conditions (based on FLBPI)

    ! The following units are referenced in module w3wavemd for output
    ! NDS( 6) ! OUTPUT DATA: restart(N).ww3 file (model restart) unit number
    ! NDS( 7) ! OUTPUT DATA: unit for output for FLOUT(1) flag grid unformmatted output
    ! NDS( 8) ! OUTPUT DATA: unit for output for FLOUT(2) flag point unformmatted output
    ! etc through 13

    mds(1) = stdout
    mds(2) = stdout
    mds(3) = stdout
    mds(4) = stdout

    ! Identify available unit numbers
    ! Each ESMF_UtilIOUnitGet is followed by an OPEN statement for that
    ! unit so that subsequent ESMF_UtilIOUnitGet calls do not return the
    ! the same unit.  After getting all the available unit numbers, close
    ! the units since they will be opened within W3INIT.
    ! By default, unit numbers between 50 and 99 are scanned to find an
    ! unopened unit number

    call ESMF_UtilIOUnitGet(mds(5)) ; open(unit=mds(5)  , status='scratch')
    call ESMF_UtilIOUnitGet(mds(6)) ; open(unit=mds(6)  , status='scratch')
    call ESMF_UtilIOUnitGet(mds(7)) ; open(unit=mds(7)  , status='scratch')
    call ESMF_UtilIOUnitGet(mds(8)) ; open(unit=mds(8)  , status='scratch')
    call ESMF_UtilIOUnitGet(mds(9)) ; open(unit=mds(9)  , status='scratch')
    call ESMF_UtilIOUnitGet(mds(10)); open(unit=mds(10) , status='scratch')
    call ESMF_UtilIOUnitGet(mds(11)); open(unit=mds(11) , status='scratch')
    call ESMF_UtilIOUnitGet(mds(12)); open(unit=mds(12) , status='scratch')
    call ESMF_UtilIOUnitGet(mds(13)); open(unit=mds(13) , status='scratch')
    close(mds(5)); close(mds(6)); close(mds(7)); close(mds(8)); close(mds(9)); close(mds(10))
    close(mds(11)); close(mds(12)); close(mds(13))

    ntrace(1) = mds(3)
    ntrace(2) = 10

  end subroutine set_shel_io
!> Set up variables used in shel mode directly (CESM)
!!
!! @param[in]  dtime_sync         coupling interval in s
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
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
!> Read ww3_shel.inp (UWM)
!!
!! @param[in]  mpi_comm           mpi communicator
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
  subroutine read_shel_inp(mpi_comm)

    use w3nmlshelmd

    USE W3GDATMD, ONLY: FLAGLL, DTMAX, NX, NY, GTYPE
    USE W3WDATMD, ONLY: TIME, W3NDAT, W3DIMW, W3SETW
    USE W3ADATMD, ONLY: W3NAUX, W3DIMA, W3SETA
    USE W3IDATMD, ONLY: INFLAGS1, INFLAGS2, FLAGSC
    USE W3ODATMD, ONLY: W3NOUT, W3SETO, NDS
    USE W3ODATMD, ONLY: NAPROC, IAPROC, NAPOUT, NAPERR
    USE W3ODATMD, ONLY: IDOUT, FNMPRE, IOSTYP, NOTYPE
    USE W3ODATMD, ONLY: FLOGRR, FLOGR, OFILES
    USE W3IOGRMD, ONLY: W3IOGR
    USE W3IOGOMD, ONLY: W3READFLGRD, FLDOUT, W3FLGRDFLAG
    USE W3SERVMD, ONLY: NEXTLN, EXTCDE
    USE W3TIMEMD, ONLY: DSEC21, STME21, TICK21, T2D, D2J
    USE W3FLDSMD, ONLY: W3FLDO
#ifdef W3_OASIS
    USE W3WDATMD, ONLY: TIME00, TIMEEND
#endif
#ifdef W3_NL5
    USE W3WDATMD, ONLY: QI5TBEG
#endif
    use wav_shr_flags, only : debuginit_flag, couple_flag, oasis_flag
    use wav_shr_flags, only : O7_flag, t_flag, mgw_flag, mgp_flag
    use wav_shr_flags, only : nl5_flag, ic1_flag, ic2_flag, is2_flag
    use wav_shr_flags, only : ic3_flag, bt8_flag, bt9_flag, ic4_flag
    use wav_shr_flags, only : ic5_flag, nco_flag
    use wav_shr_flags, only : debuginit_msg

    INTEGER, INTENT(IN) :: MPI_COMM

    ! Local parameters
    INTEGER, PARAMETER  :: NHMAX =    200

    TYPE(NML_DOMAIN_T)       :: NML_DOMAIN
    TYPE(NML_INPUT_T)        :: NML_INPUT
    TYPE(NML_OUTPUT_TYPE_T)  :: NML_OUTPUT_TYPE
    TYPE(NML_OUTPUT_DATE_T)  :: NML_OUTPUT_DATE
    TYPE(NML_HOMOG_COUNT_T)  :: NML_HOMOG_COUNT
    TYPE(NML_HOMOG_INPUT_T), ALLOCATABLE  :: NML_HOMOG_INPUT(:)

    INTEGER             :: NDSI, NDSI2, NDSS, NDSO, NDSE, NDST, NDSL,&
                           NDSEN, IERR, J, I, ILOOP, IPTS
    INTEGER             :: NDSF(-7:9), &
                           NH(-7:10), THO(2,-7:10,NHMAX), RCLD(7:9), &
                           NODATA(7:9), STARTDATE(8), STOPDATE(8), IHH(-7:10)
    INTEGER             :: jfirst, IERR_MPI, flagtide, ih, n_tot
    REAL                :: FACTOR, DTTST, XX, YY, HA(NHMAX,-7:10), &
                           HD(NHMAX,-7:10), HS(NHMAX,-7:10)
    DOUBLE PRECISION    :: STARTJULDAY, STOPJULDAY
    CHARACTER(LEN=1)    :: COMSTR, FLAGTFC(-7:10)
    CHARACTER(LEN=3)    :: IDSTR(-7:10), IDTST
    CHARACTER(LEN=6)    :: YESXNO
    CHARACTER(LEN=40)   :: PN
    CHARACTER(LEN=13)   :: IDFLDS(-7:10)
    CHARACTER(LEN=20)   :: STRNG
    CHARACTER(LEN=23)   :: DTME21
    CHARACTER(LEN=30)   :: IDOTYP(8)
    CHARACTER(LEN=80)   :: LINE
    CHARACTER(LEN=256)  :: TMPLINE, TEST
    CHARACTER(LEN=1024) :: FLDRST=''
    CHARACTER(LEN=80)   :: LINEIN
    CHARACTER(LEN=30)   :: OFILE ! w3_cou only
    CHARACTER(LEN=8)    :: WORDS(7)=''
    LOGICAL             :: FLFLG, FLHOM, TFLAGI, PRTFRM, FLGNML, FLGSTIDE(4), FLH(-7:10)
    INTEGER             :: THRLEV = 1
    INTEGER             :: TIME0(2), TIMEN(2), TTIME(2)
    character(len=80)   :: printmsg

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
    !---------------------------------------------------
    FLGR2 = .FALSE.
    FLH(:) = .FALSE.
    iprt(:) = 0
    call debuginit_msg(740+iaproc, 'wav_shel_inp, step 1', debuginit_flag)

    NDSI   = 10
    NDSS   = 90
    NDSO   =  6
    NDSE   =  6
    NDST   =  6
    NDSL   = 50
    if (couple_flag) then
       NDSO   =  333
       NDSE   =  333
       NDST   =  333
    end if

    IF ( IAPROC .EQ. NAPERR ) THEN
       NDSEN  = NDSE
    ELSE
       NDSEN  = -1
    END IF
#ifdef W3_OMPH
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,905) &
         MPI_THREAD_FUNNELED, THRLEV
#endif
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
    call debuginit_msg(740+iaproc, 'wav_shel_inp, step 2', debuginit_flag)

    if (nco_flag) then
       NDSI   = 11
       NDSS   = 90
       NDSO   =  6
       NDSE   = NDSO
       NDST   = NDSO
       NDSF(1)  = 12
       NDSF(2)  = 13
       NDSF(3)  = 14
       NDSF(4)  = 15
       NDSF(5)  = 16
       NDSF(6)  = 17
       NDSF(7)  = 18
       NDSF(8)  = 19
       NDSF(9)  = 20
    end if
    ! 1.c Local parameters

    ! Default COMSTR to "$" (for when using nml input files)
    COMSTR = "$"
    call debuginit_msg(740+iaproc, 'wav_shel_inp, step 2', debuginit_flag)

    ! If using experimental mud or ice physics, additional lines will
    !  be read in from wav_shel_inp.inp and applied, so JFIRST is changed from
    !  its initialization setting "JFIRST=1" to some lower value.
    JFIRST=1
    if (ic1_flag) jfirst = -7
    if (ic2_flag) jfirst = -7
    if (is2_flag) jfirst = -7
    if (ic3_flag) jfirst = -7
    if (bt8_flag) jfirst = -7
    if (bt9_flag) jfirst = -7
    if (ic4_flag) jfirst = -7
    if (ic5_flag) jfirst = -7

    call debuginit_msg(740+iaproc, 'wav_shel_inp, step 4', debuginit_flag)
    print *,'YYY ',size(inflags1),size(inflags2)
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 2.  Define input fields
    !
    !
    ! process ww3_prnc namelist
    !
    INQUIRE(FILE=TRIM(FNMPRE)//"ww3_shel.nml", EXIST=FLGNML)
#ifdef test
    IF (FLGNML) THEN
       ! Read namelist
       CALL W3NMLSHEL (MPI_COMM, NDSI, TRIM(FNMPRE)//'ww3_shel.nml',  &
            NML_DOMAIN, NML_INPUT, NML_OUTPUT_TYPE,        &
            NML_OUTPUT_DATE, NML_HOMOG_COUNT,             &
            NML_HOMOG_INPUT, IERR)

       ! 2.1 forcing flags

       FLH(-7:10)=.FALSE.
       FLAGTFC(-7)=TRIM(NML_INPUT%FORCING%ICE_PARAM1)
       FLAGTFC(-6)=TRIM(NML_INPUT%FORCING%ICE_PARAM2)
       FLAGTFC(-5)=TRIM(NML_INPUT%FORCING%ICE_PARAM3)
       FLAGTFC(-4)=TRIM(NML_INPUT%FORCING%ICE_PARAM4)
       FLAGTFC(-3)=TRIM(NML_INPUT%FORCING%ICE_PARAM5)
       FLAGTFC(-2)=TRIM(NML_INPUT%FORCING%MUD_DENSITY)
       FLAGTFC(-1)=TRIM(NML_INPUT%FORCING%MUD_THICKNESS)
       FLAGTFC(0)=TRIM(NML_INPUT%FORCING%MUD_VISCOSITY)
       FLAGTFC(1)=TRIM(NML_INPUT%FORCING%WATER_LEVELS)
       FLAGTFC(2)=TRIM(NML_INPUT%FORCING%CURRENTS)
       FLAGTFC(3)=TRIM(NML_INPUT%FORCING%WINDS)
       FLAGTFC(4)=TRIM(NML_INPUT%FORCING%ICE_CONC)
       FLAGTFC(5)=TRIM(NML_INPUT%FORCING%ATM_MOMENTUM)
       FLAGTFC(6)=TRIM(NML_INPUT%FORCING%AIR_DENSITY)
       FLAGTFC(7)=TRIM(NML_INPUT%ASSIM%MEAN)
       FLAGTFC(8)=TRIM(NML_INPUT%ASSIM%SPEC1D)
       FLAGTFC(9)=TRIM(NML_INPUT%ASSIM%SPEC2D)

       IF (TRIM(NML_INPUT%FORCING%ICE_PARAM1) .EQ. 'H') THEN
          FLAGTFC(-7)='T'
          FLH(-7)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%ICE_PARAM2) .EQ. 'H') THEN
          FLAGTFC(-6)='T'
          FLH(-6)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%ICE_PARAM3) .EQ. 'H') THEN
          FLAGTFC(-5)='T'
          FLH(-5)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%ICE_PARAM4) .EQ. 'H') THEN
          FLAGTFC(-4)='T'
          FLH(-4)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%ICE_PARAM5) .EQ. 'H') THEN
          FLAGTFC(-3)='T'
          FLH(-3)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%MUD_DENSITY) .EQ. 'H') THEN
          FLAGTFC(-2)='T'
          FLH(-2)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%MUD_THICKNESS) .EQ. 'H') THEN
          FLAGTFC(-1)='T'
          FLH(-1)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%MUD_VISCOSITY) .EQ. 'H') THEN
          FLAGTFC(0)='T'
          FLH(0)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%WATER_LEVELS) .EQ. 'H') THEN
          FLAGTFC(1)='T'
          FLH(1)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%CURRENTS) .EQ. 'H') THEN
          FLAGTFC(2)='T'
          FLH(2)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%WINDS) .EQ. 'H') THEN
          FLAGTFC(3)='T'
          FLH(3)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%ICE_CONC) .EQ. 'H') THEN
          FLAGTFC(4)='T'
          FLH(4)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%ATM_MOMENTUM) .EQ. 'H') THEN
          FLAGTFC(5)='T'
          FLH(5)=.TRUE.
       END IF
       IF (TRIM(NML_INPUT%FORCING%AIR_DENSITY) .EQ. 'H') THEN
          FLAGTFC(6)='T'
          FLH(6)=.TRUE.
       END IF

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
       if (couple_flag) then
          IF (FLAGSC(1) .AND. INFLAGS1(2) .AND. .NOT. FLAGSC(2)) GOTO 2102
          IF (FLAGSC(2) .AND. INFLAGS1(1) .AND. .NOT. FLAGSC(1)) GOTO 2102
       end if


       INFLAGS1(10) = .FALSE.
       if (mgw_flag .or. mgp_flag) then
          INFLAGS1(10) = .TRUE.
          FLH(10)   = .TRUE.
       end if
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
       if (t_flag) then
          WRITE (NDST,9020) FLFLG, INFLAGS1, FLHOM, FLH
       end if



       ! 2.2 Time setup

       READ(NML_DOMAIN%START,*) TIME0
       CALL T2D(TIME0,STARTDATE,IERR)
       CALL D2J(STARTDATE,STARTJULDAY,IERR)
       READ(NML_DOMAIN%STOP,*) TIMEN
       CALL T2D(TIMEN,STOPDATE,IERR)
       CALL D2J(STOPDATE,STOPJULDAY,IERR)

       ! 2.3 Domain setup

       IOSTYP = NML_DOMAIN%IOSTYP

#ifdef W3_PDLIB
       IF (IOSTYP .gt. 1) THEN
          WRITE(*,*) 'IOSTYP not supported in domain decomposition mode'
          CALL EXTCDE ( 6666 )
       ENDIF
#endif

       CALL W3IOGR ( 'GRID', NDSF(7) )
       IF ( FLAGLL ) THEN
          FACTOR = 1.
       ELSE
          FACTOR = 1.E-3
       END IF

       ! 2.4 Output dates

       READ(NML_OUTPUT_DATE%FIELD%START, *)   ODAT(1), ODAT(2)
       READ(NML_OUTPUT_DATE%FIELD%STRIDE, *)  ODAT(3)
       READ(NML_OUTPUT_DATE%FIELD%STOP, *)    ODAT(4), ODAT(5)

       READ(NML_OUTPUT_DATE%FIELD%OUTFFILE, *)  OFILES(1)
       !        OUTPTS(I)%OUTSTRIDE(1)=ODAT(3,I)

       READ(NML_OUTPUT_DATE%POINT%START, *)   ODAT(6), ODAT(7)
       READ(NML_OUTPUT_DATE%POINT%STRIDE, *)  ODAT(8)
       READ(NML_OUTPUT_DATE%POINT%STOP, *)    ODAT(9), ODAT(10)

       READ(NML_OUTPUT_DATE%POINT%OUTFFILE, *)  OFILES(2)
       !        OUTPTS(I)%OUTSTRIDE(2)=ODAT(8,I)

       READ(NML_OUTPUT_DATE%TRACK%START, *)   ODAT(11), ODAT(12)
       READ(NML_OUTPUT_DATE%TRACK%STRIDE, *)  ODAT(13)
       READ(NML_OUTPUT_DATE%TRACK%STOP, *)    ODAT(14), ODAT(15)
       READ(NML_OUTPUT_DATE%RESTART%START, *)   ODAT(16), ODAT(17)
       READ(NML_OUTPUT_DATE%RESTART%STRIDE, *)  ODAT(18)
       READ(NML_OUTPUT_DATE%RESTART%STOP, *)    ODAT(19), ODAT(20)
       READ(NML_OUTPUT_DATE%RESTART2%START, *)   ODAT(36), ODAT(37)
       READ(NML_OUTPUT_DATE%RESTART2%STRIDE, *)  ODAT(38)
       READ(NML_OUTPUT_DATE%RESTART2%STOP, *)    ODAT(39), ODAT(40)
       READ(NML_OUTPUT_DATE%BOUNDARY%START, *)   ODAT(21), ODAT(22)
       READ(NML_OUTPUT_DATE%BOUNDARY%STRIDE, *)  ODAT(23)
       READ(NML_OUTPUT_DATE%BOUNDARY%STOP, *)    ODAT(24), ODAT(25)
       READ(NML_OUTPUT_DATE%PARTITION%START, *)   ODAT(26), ODAT(27)
       READ(NML_OUTPUT_DATE%PARTITION%STRIDE, *)  ODAT(28)
       READ(NML_OUTPUT_DATE%PARTITION%STOP, *)    ODAT(29), ODAT(30)
       READ(NML_OUTPUT_DATE%COUPLING%START, *)   ODAT(31), ODAT(32)
       READ(NML_OUTPUT_DATE%COUPLING%STRIDE, *)  ODAT(33)
       READ(NML_OUTPUT_DATE%COUPLING%STOP, *)    ODAT(34), ODAT(35)

       ! set the time stride at 0 or more
       ODAT(3) = MAX ( 0 , ODAT(3) )
       ODAT(8) = MAX ( 0 , ODAT(8) )
       ODAT(13) = MAX ( 0 , ODAT(13) )
       ODAT(18) = MAX ( 0 , ODAT(18) )
       ODAT(23) = MAX ( 0 , ODAT(23) )
       ODAT(28) = MAX ( 0 , ODAT(28) )
       ODAT(33) = MAX ( 0 , ODAT(33) )
       ODAT(38) = MAX ( 0 , ODAT(38) )

       if (couple_flag) then
          ! Test the validity of the coupling time step
          IF (ODAT(33) == 0) THEN
             IF ( IAPROC .EQ. NAPOUT ) THEN
                WRITE(NDSO,1010) ODAT(33), INT(DTMAX)
             END IF
             ODAT(33) = INT(DTMAX)
          ELSE IF (MOD(ODAT(33),INT(DTMAX)) .NE. 0) THEN
             GOTO 2009
          END IF
       end if
       !
       ! 2.5 Output types

      NPTS   = 0
      NOTYPE = 6
      if (couple_flag) then
         NOTYPE = 7
      end if
      DO J = 1, NOTYPE
         ! OUTPTS(I)%OFILES(J)=OFILES(J)
         IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN

            ! Type 1: fields of mean wave parameters
            IF ( J .EQ. 1 ) THEN
               FLDOUT = NML_OUTPUT_TYPE%FIELD%LIST
               CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDOUT, FLGD,     &
                    FLGRD, IAPROC, NAPOUT, IERR )
               IF ( IERR .NE. 0 ) GOTO 2222


               ! Type 2: point output
            ELSE IF ( J .EQ. 2 ) THEN
               OPEN (NDSL, FILE=TRIM(FNMPRE)//TRIM(NML_OUTPUT_TYPE%POINT%FILE), &
                    FORM='FORMATTED', STATUS='OLD', ERR=2104, IOSTAT=IERR)

               ! first loop to count the number of points
               ! second loop to allocate the array and store the points
               IPTS = 0
               DO ILOOP=1,2
                  REWIND (NDSL)

                  IF ( ILOOP.EQ.2) THEN
                     NPTS = IPTS
                     IF ( NPTS.GT.0 ) THEN
                        ALLOCATE ( X(NPTS), Y(NPTS), PNAMES(NPTS) )
                        IPTS = 0 ! reset counter to be reused for next do loop
                     ELSE
                        ALLOCATE ( X(1), Y(1), PNAMES(1) )
                        GOTO 2054
                     END IF
                  END IF

                  DO
                     READ (NDSL,*,ERR=2004,IOSTAT=IERR) TMPLINE
                     ! if end of file or stopstring, then exit
                     IF ( IERR.NE.0 .OR. INDEX(TMPLINE,"STOPSTRING").NE.0 ) EXIT
                     ! leading blanks removed and placed on the right
                     TEST = ADJUSTL ( TMPLINE )
                     IF ( TEST(1:1).EQ.COMSTR .OR. LEN_TRIM(TEST).EQ.0 ) THEN
                        ! if comment or blank line, then skip
                        CYCLE
                     ELSE
                        ! otherwise, backup to beginning of line
                        BACKSPACE ( NDSL, ERR=2004, IOSTAT=IERR)
                        READ (NDSL,*,ERR=2004,IOSTAT=IERR) XX, YY, PN
                     END IF
                     IPTS = IPTS + 1
                     IF ( ILOOP .EQ. 1 ) CYCLE
                     IF ( ILOOP .EQ. 2 ) THEN
                        X(IPTS)      = XX
                        Y(IPTS)      = YY
                        PNAMES(IPTS) = PN
                        IF ( IAPROC .EQ. NAPOUT ) THEN
                           IF ( FLAGLL ) THEN
                              IF ( IPTS .EQ. 1 ) THEN
                                 WRITE (NDSO,2945)                     &
                                      FACTOR*XX, FACTOR*YY, PN
                              ELSE
                                 WRITE (NDSO,2946) IPTS,               &
                                      FACTOR*XX, FACTOR*YY, PN
                              END IF
                           ELSE
                              IF ( IPTS .EQ. 1 ) THEN
                                 WRITE (NDSO,2955)                     &
                                      FACTOR*XX, FACTOR*YY, PN
                              ELSE
                                 WRITE (NDSO,2956) IPTS,               &
                                      FACTOR*XX, FACTOR*YY, PN
                              END IF
                           END IF
                        END IF
                     END IF ! ILOOP.EQ.2
                  END DO ! end of file
               END DO ! ILOOP
               CLOSE(NDSL)

               ! Type 3: track output
            ELSE IF ( J .EQ. 3 ) THEN
               TFLAGI = NML_OUTPUT_TYPE%TRACK%FORMAT
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
               IPRT(1) = NML_OUTPUT_TYPE%PARTITION%X0
               IPRT(2) = NML_OUTPUT_TYPE%PARTITION%XN
               IPRT(3) = NML_OUTPUT_TYPE%PARTITION%NX
               IPRT(4) = NML_OUTPUT_TYPE%PARTITION%Y0
               IPRT(5) = NML_OUTPUT_TYPE%PARTITION%YN
                IPRT(6) = NML_OUTPUT_TYPE%PARTITION%NY
                PRTFRM = NML_OUTPUT_TYPE%PARTITION%FORMAT

                IF ( IAPROC .EQ. NAPOUT ) THEN
                   IF ( PRTFRM ) THEN
                      YESXNO = 'YES/--'
                   ELSE
                      YESXNO = '---/NO'
                   END IF
                   WRITE (NDSO,6945) IPRT, YESXNO
                END IF

#ifdef W3_COU
                ! Type 7: coupling
             ELSE IF ( J .EQ. 7 ) THEN
                FLDOUT = NML_OUTPUT_TYPE%COUPLING%SENT
                CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDOUT, FLG2,  &
                     FLGR2, IAPROC, NAPOUT, IERR )
                IF ( IERR .NE. 0 ) GOTO 2222
                FLDIN = NML_OUTPUT_TYPE%COUPLING%RECEIVED
                CPLT0 = NML_OUTPUT_TYPE%COUPLING%COUPLET0
#endif

             END IF ! J
          END IF ! ODAT
       END DO ! J

       ! Extra fields to be written in the restart
       FLDRST = NML_OUTPUT_TYPE%RESTART%EXTRA
       CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDRST, FLOGR,  &
            FLOGRR, IAPROC, NAPOUT, IERR )
       IF ( IERR .NE. 0 ) GOTO 2222

       ! force minimal allocation to avoid memory seg fault
       IF ( .NOT.ALLOCATED(X) .AND. NPTS.EQ.0 ) ALLOCATE ( X(1), Y(1), PNAMES(1) )

       ! 2.6 Homogeneous field data

       IF ( FLHOM ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                   &
               'Homogeneous field data (and moving grid) ...'

          NH(-7) = NML_HOMOG_COUNT%N_IC1
          NH(-6) = NML_HOMOG_COUNT%N_IC2
          NH(-5) = NML_HOMOG_COUNT%N_IC3
          NH(-4) = NML_HOMOG_COUNT%N_IC4
          NH(-3) = NML_HOMOG_COUNT%N_IC5
          NH(-2) = NML_HOMOG_COUNT%N_MDN
          NH(-1) = NML_HOMOG_COUNT%N_MTH
          NH(0)  = NML_HOMOG_COUNT%N_MVS
          NH(1)  = NML_HOMOG_COUNT%N_LEV
          NH(2)  = NML_HOMOG_COUNT%N_CUR
          NH(3)  = NML_HOMOG_COUNT%N_WND
          NH(4)  = NML_HOMOG_COUNT%N_ICE
          NH(5)  = NML_HOMOG_COUNT%N_TAU
          NH(6)  = NML_HOMOG_COUNT%N_RHO
          NH(10)  = NML_HOMOG_COUNT%N_MOV

          N_TOT = NML_HOMOG_COUNT%N_TOT

          DO J=JFIRST,10
             IF ( NH(J) .GT. NHMAX ) GOTO 2006
          END DO


          ! Store homogeneous fields
          IF ( N_TOT .GT. 0 ) THEN
             IHH(:)=0
             DO IH=1,N_TOT
                READ(NML_HOMOG_INPUT(IH)%NAME,*) IDTST
                SELECT CASE (IDTST)
                CASE ('IC1')
                   J=-7
                CASE ('IC2')
                   J=-6
                CASE ('IC3')
                   J=-5
                CASE ('IC4')
                   J=-4
                CASE ('IC5')
                   J=-3
                CASE ('MDN')
                   J=-2
                CASE ('MTH')
                   J=-1
                CASE ('MVS')
                   J=0
                CASE ('LEV')
                   J=1
                CASE ('CUR')
                   J=2
                CASE ('WND')
                   J=3
                CASE ('ICE')
                   J=4
                CASE ('TAU')
                   J=5
                CASE ('RHO')
                   J=6
                CASE ('MOV')
                   J=10
                CASE DEFAULT
                   GOTO 2062
                END SELECT
                IHH(J)=IHH(J)+1
                READ(NML_HOMOG_INPUT(IH)%DATE,*) THO(:,J,IHH(J))
                HA(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE1
                HD(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE2
                HS(IHH(J),J) = NML_HOMOG_INPUT(IH)%VALUE3
             END DO
          END IF

          if (O7_flag) then
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
          end if

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


    END IF ! FLGNML
#endif

    !
    ! process old wav_shel_inp.inp format
    !
    IF (.NOT. FLGNML) THEN
       call debuginit_msg(740+iaproc, ' FNMPRE'//TRIM(FNMPRE), debuginit_flag)
       OPEN (NDSI,FILE=TRIM(FNMPRE)//'ww3_shel.inp',STATUS='OLD',IOSTAT=IERR)
       REWIND (NDSI)
       call debuginit_msg(740+iaproc, 'Before read 2002, case 1', debuginit_flag)
       !AR: I changed the error handling for err=2002, see commit message ...
       READ (NDSI,'(A)') COMSTR
       call debuginit_msg(740+iaproc, ' COMSTR='//trim(COMSTR), debuginit_flag)
       call debuginit_msg(740+iaproc, ' After read 2002, case 1', debuginit_flag)
       IF (COMSTR.EQ.' ') COMSTR = '$'
       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,901) COMSTR

       ! 2.1 forcing flags

       FLH(-7:10) = .FALSE.
       DO J=JFIRST, 9
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          IF ( J .LE. 6 ) THEN
             call debuginit_msg(740+iaproc, 'Before read 2002, case 2', debuginit_flag)
             READ (NDSI,*) FLAGTFC(J), FLH(J)

             write(printmsg,*)'     J=', J, ' FLAGTFC=', FLAGTFC(J), ' FLH=', FLH(J)
             call debuginit_msg(740+iaproc, trim(printmsg), debuginit_flag)
             call debuginit_msg(740+iaproc, ' After read 2002, case 2', debuginit_flag)
          ELSE
             call debuginit_msg(740+iaproc, 'Before read 2002, case 3', debuginit_flag)
             READ (NDSI,*) FLAGTFC(J)

             write(printmsg,*) '     J=', J, ' FLAGTFC=', FLAGTFC(J)
             call debuginit_msg(740+iaproc, trim(printmsg), debuginit_flag)
             call debuginit_msg(740+iaproc, ' After read 2002, case 3 ', debuginit_flag)
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
       if (couple_flag) then
          IF (FLAGSC(1) .AND. INFLAGS1(2) .AND. .NOT. FLAGSC(2)) GOTO 2102
          IF (FLAGSC(2) .AND. INFLAGS1(1) .AND. .NOT. FLAGSC(1)) GOTO 2102
       end if

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'wav_shel_inp SECTION 2b'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

       call debuginit_msg(740+iaproc, 'wav_shel_inp, step 5', debuginit_flag)

       INFLAGS1(10) = .FALSE.
       if (mgw_flag .or. mgp_flag) then
          INFLAGS1(10) = .TRUE.
          FLH(10)   = .TRUE.
       end if
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
       ! INFLAGS2 is just "initial value of INFLAGS1", i.e. does *not* get
       ! changed when model reads last record of ice.ww3
       INFLAGS2=INFLAGS1

       if (t_flag) then
          WRITE (NDST,9020) FLFLG, INFLAGS1, FLHOM, FLH
       end if
          write (*,*)'XXX format 9020 line ',size(INFLAGS1),size (FLH)
          WRITE (*,9020) FLFLG, INFLAGS1, FLHOM, FLH


       ! 2.2 Time setup

       CALL NEXTLN ( COMSTR , NDSI , NDSEN )
       call debuginit_msg(740+iaproc, 'Before read 2002, case 4', debuginit_flag)
       READ (NDSI,*) TIME0
       call debuginit_msg(740+iaproc, ' After read 2002, case 4', debuginit_flag)

#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'wav_shel_inp SECTION 2c'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

       CALL NEXTLN ( COMSTR , NDSI , NDSEN )
       call debuginit_msg(740+iaproc, 'Before read 2002, case 5', debuginit_flag)
       READ (NDSI,*) TIMEN
       call debuginit_msg(740+iaproc, ' After read 2002, case 5', debuginit_flag)
       call debuginit_msg(740+iaproc, 'wav_shel_inp, step 6', debuginit_flag)
       !
#ifdef W3_MEMCHECK
       write(740+IAPROC,*) 'memcheck_____:', 'wav_shel_inp SECTION 2d'
       call getMallocInfo(mallinfos)
       call printMallInfo(IAPROC,mallInfos)
#endif

       ! 2.3 Domain setup

       call debuginit_msg(740+iaproc, 'wav_shel_inp, step 7', debuginit_flag)
       CALL NEXTLN ( COMSTR , NDSI , NDSEN )
       call debuginit_msg(740+iaproc, 'Before read 2002, case 6', debuginit_flag)
       READ (NDSI,*) IOSTYP
#ifdef W3_PDLIB
       IF (IOSTYP .gt. 1) THEN
          WRITE(*,*) 'IOSTYP not supported in domain decomposition mode'
          CALL EXTCDE ( 6666 )
       ENDIF
#endif
       call debuginit_msg(740+iaproc, ' After read 2002, case 6', debuginit_flag)
       CALL W3IOGR ( 'GRID', NDSF(7) )
       IF ( FLAGLL ) THEN
          FACTOR = 1.
       ELSE
          FACTOR = 1.E-3
       END IF
       call debuginit_msg(740+iaproc, 'wav_shel_inp, step 8', debuginit_flag)

       ! 2.4 Output dates

       NPTS   = 0
       NOTYPE = 6
       if (couple_flag) then
          NOTYPE = 7
       end if
       call debuginit_msg(740+iaproc, 'Before NOTYPE loop', debuginit_flag)
       DO J = 1, NOTYPE
          write(printmsg,*)'J=', J, '/ NOTYPE=', NOTYPE
          print *,'XXX ',trim(printmsg)
          call debuginit_msg(740+iaproc, trim(printmsg), debuginit_flag)
          CALL NEXTLN ( COMSTR , NDSI , NDSEN )
          call debuginit_msg(740+iaproc, 'Before read 2002, case 7', debuginit_flag)
          !
          ! CHECKPOINT
          IF(J .EQ. 4) THEN
          print *,'XXX  2.4 start j = 4'
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
                !if(iaproc .eq. naproc) WRITE(*,*)'odat(j=4): ',(ODAT(I),I=5*(8-1)+1,5*8)
                WRITE(740+iaproc,*)'odat(j=4): ',(ODAT(I),I=5*(8-1)+1,5*8)
             END IF
             IF (WORDS(7) .EQ. 'T') THEN
                CALL NEXTLN ( COMSTR , NDSI , NDSEN )
                READ (NDSI,'(A)',END=2001,ERR=2002) FLDRST
             END IF
             CALL W3FLGRDFLAG ( NDSO, NDSO, NDSE, FLDRST, FLOGR,  &
                  FLOGRR, IAPROC, NAPOUT, IERR )
             IF ( IERR .NE. 0 ) GOTO 2222
          ELSE
             !
             !INLINE NEW VARIABLE TO READ IF PRESENT OFILES(J), IF NOT ==0
             ! READ (NDSI,*) (ODAT(I),I=5*(J-1)+1,5*J)
             ! READ (NDSI,*,IOSTAT=IERR) (ODAT(I),I=5*(J-1)+1,5*J),OFILES(J)
             IF(J .LE. 2) THEN
          print *,'XXX  2.4 start j le 2 ',j
                WORDS(1:6)=''
                ! READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J),OFILES(J)
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
#ifdef W3_COU
             ELSE IF(J .EQ. 7) THEN
                WORDS(1:6)=''
                READ (NDSI,'(A)') LINEIN
                READ(LINEIN,*,iostat=ierr) WORDS

                READ(WORDS( 1 ), * ) ODAT(31)
                READ(WORDS( 2 ), * ) ODAT(32)
                READ(WORDS( 3 ), * ) ODAT(33)
                READ(WORDS( 4 ), * ) ODAT(34)
                READ(WORDS( 5 ), * ) ODAT(35)

                IF (WORDS(6) .EQ. 'T') THEN
                   CPLT0 = .TRUE.
                ELSE
                   CPLT0 = .FALSE.
                END IF
#endif
             ELSE
          print *,'XXX 2.4 else j gt 2 ',j
                OFILES(J)=0
                READ (NDSI,*,END=2001,ERR=2002)(ODAT(I),I=5*(J-1)+1,5*J)
             END IF !j le 2
             ! WRITE(*,*) 'OFILES(J)= ', OFILES(J),J
             !
             call debuginit_msg(740+iaproc, ' After read 2002, case 7', debuginit_flag)
             ODAT(5*(J-1)+3) = MAX ( 0 , ODAT(5*(J-1)+3) )
             !
#ifdef W3_MEMCHECK
             write(740+IAPROC,*) 'memcheck_____:', 'wav_shel_inp NOTTYPE', J
             call getMallocInfo(mallinfos)
             call printMallInfo(IAPROC,mallInfos)
#endif
             ! 2.5 Output types

             IF ( ODAT(5*(J-1)+3) .NE. 0 ) THEN

                ! Type 1: fields of mean wave parameters
                call debuginit_msg(740+iaproc, ' Case analysis', debuginit_flag)
                IF ( J .EQ. 1 ) THEN
          print *,'XXX  2.5 j = 1'
                   CALL W3READFLGRD ( NDSI, NDSO, 9, NDSEN, COMSTR, FLGD,   &
                        FLGRD, IAPROC, NAPOUT, IERR )
                   IF ( IERR .NE. 0 ) GOTO 2222


                ! Type 2: point output
                ELSE IF ( J .EQ. 2 ) THEN
          print *,'XXX  2.5 j = 2' 
                   DO ILOOP=1,2
                      IF ( ILOOP .EQ. 1 ) THEN
                         NDSI2  = NDSI
                         IF ( IAPROC .EQ. 1 ) OPEN                       &
                              (NDSS,FILE=TRIM(FNMPRE)//'ww3_shel.scratch')
                      ELSE
                         NDSI2  = NDSS
#ifdef W3_MPI
                         CALL MPI_BARRIER (MPI_COMM,IERR_MPI)
#endif
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
                         call debuginit_msg(740+iaproc, 'Before read 2002, case 8', debuginit_flag)
                         READ (NDSI2,*) XX, YY, PN
                         call debuginit_msg(740+iaproc, ' After read 2002, case 8', debuginit_flag)
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
#ifdef W3_MPI
                      CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
#endif
                      CLOSE (NDSS,STATUS='DELETE')
                   ELSE
#ifdef W3_MPI
                      CALL MPI_BARRIER ( MPI_COMM, IERR_MPI )
#endif
                      CLOSE (NDSS)
                   END IF

                         call debuginit_msg(740+iaproc, ' After read 2002, case 8 3', debuginit_flag)
                ! Type 3: track output
                ELSE IF ( J .EQ. 3 ) THEN
                 print *,'XXX 2.5 j= 3'
                         call debuginit_msg(740+iaproc, ' After read 2002, case 8 4', debuginit_flag)
                   CALL NEXTLN ( COMSTR , NDSI , NDSEN )
                   call debuginit_msg(740+iaproc, 'Before read 2002, case 9', debuginit_flag)
                   READ (NDSI,*) TFLAGI
                   call debuginit_msg(740+iaproc, ' After read 2002, case 9', debuginit_flag)

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
                 print *,'XXX 2.5 j= 6'
                   CALL NEXTLN ( COMSTR , NDSI , NDSEN )
                   call debuginit_msg(740+iaproc, 'Before reading IPRT', debuginit_flag)
                   call debuginit_msg(740+iaproc, 'Before read 2002, case 10', debuginit_flag)
                   READ (NDSI,*) IPRT, PRTFRM
                   call debuginit_msg(740+iaproc, ' After read 2002, case 10', debuginit_flag)

                   IF ( IAPROC .EQ. NAPOUT ) THEN
                      IF ( PRTFRM ) THEN
                         YESXNO = 'YES/--'
                      ELSE
                         YESXNO = '---/NO'
                      END IF
                      WRITE (NDSO,6945) IPRT, YESXNO
                   END IF


#ifdef W3_COU
                ! Type 7: coupling
                ELSE IF ( J .EQ. 7 ) THEN
                   CALL W3READFLGRD ( NDSI, NDSO, NDSS, NDSEN, COMSTR, FLG2,     &
                        FLGR2, IAPROC, NAPOUT, IERR )
                   IF ( IERR .NE. 0 ) GOTO 2222
                   CALL NEXTLN ( COMSTR , NDSI , NDSEN )
                   READ (NDSI,'(A)',END=2001,ERR=2002,IOSTAT=IERR) FLDIN
#endif

                END IF ! J
                 print *,'XXX done endif j ',j
             END IF ! ODAT
                 print *,'XXX done endif odat=something f(j)'
          END IF ! IF J=4
                 print *,'XXX done endif j=4'
       END DO ! J
                 print *,'XXX done j ',j

       ! force minimal allocation to avoid memory seg fault
       IF ( .NOT.ALLOCATED(X) .AND. NPTS.EQ.0 ) ALLOCATE ( X(1), Y(1), PNAMES(1) )

       ! 2.6 Homogeneous field data
                 print *,'XXX homogenous j ',j

       IF ( FLHOM ) THEN
          IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
               'Homogeneous field data (and moving grid) ...'
          NH     = 0

          ! Start of loop
          DO
             CALL NEXTLN ( COMSTR , NDSI , NDSEN )
             call debuginit_msg(740+iaproc, 'Before read 2002, case 11', debuginit_flag)
             READ (NDSI,*) IDTST
             call debuginit_msg(740+iaproc, ' After read 2002, case 11', debuginit_flag)


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
                      call debuginit_msg(740+iaproc, 'Before read 2002, case 12', debuginit_flag)
                      READ (NDSI,*) IDTST,           &
                           THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                           HA(NH(J),J)
                      call debuginit_msg(740+iaproc, ' After read 2002, case 12', debuginit_flag)
                   ELSE IF ( J .EQ. 2 ) THEN ! currents: get HA and HD
                      call debuginit_msg(740+iaproc, 'Before read 2002, case 13', debuginit_flag)
                      READ (NDSI,*) IDTST,           &
                           THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                           HA(NH(J),J), HD(NH(J),J)
                      call debuginit_msg(740+iaproc, ' After read 2002, case 13', debuginit_flag)
                   ELSE IF ( J .EQ. 3 ) THEN ! wind: get HA HD and HS
                      call debuginit_msg(740+iaproc, 'Before read 2002, case 14', debuginit_flag)
                      READ (NDSI,*) IDTST,           &
                           THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                           HA(NH(J),J), HD(NH(J),J), HS(NH(J),J)
                      call debuginit_msg(740+iaproc, ' After read 2002, case 14', debuginit_flag)
                   ELSE IF ( J .EQ. 4 ) THEN ! ice
                      call debuginit_msg(740+iaproc, 'Before read 2002, case 15', debuginit_flag)
                      READ (NDSI,*) IDTST,           &
                           THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                           HA(NH(J),J)
                      call debuginit_msg(740+iaproc, ' After read 2002, case 15', debuginit_flag)
                   ELSE IF ( J .EQ. 5 ) THEN ! atmospheric momentum
                      call debuginit_msg(740+iaproc, 'Before read 2002, case 16', debuginit_flag)
                      READ (NDSI,*) IDTST,           &
                           THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                           HA(NH(J),J), HD(NH(J),j)
                      call debuginit_msg(740+iaproc, ' After read 2002, case 16', debuginit_flag)
                   ELSE IF ( J .EQ. 6 ) THEN ! air density
                      call debuginit_msg(740+iaproc, 'Before read 2002, case 17', debuginit_flag)
                      READ (NDSI,*) IDTST,           &
                           THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                           HA(NH(J),J)
                      call debuginit_msg(740+iaproc, ' After read 2002, case 17', debuginit_flag)
                   ELSE IF ( J .EQ. 10 ) THEN ! mov: HA and HD
                      call debuginit_msg(740+iaproc, 'Before read 2002, case 18', debuginit_flag)
                      READ (NDSI,*) IDTST,           &
                           THO(1,J,NH(J)), THO(2,J,NH(J)),            &
                           HA(NH(J),J), HD(NH(J),J)
                      call debuginit_msg(740+iaproc, ' After read 2002, case 18', debuginit_flag)
                   END IF
                END IF
             END DO
          END DO

#ifdef W3_MEMCHECK
          write(740+IAPROC,*) 'memcheck_____:', 'wav_shel_inp SECTION 3'
          call getMallocInfo(mallinfos)
          call printMallInfo(IAPROC,mallInfos)
#endif
          if (O7_flag) then
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
          end if

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

    END IF
                 print *,'XXX end flgnml j ',j

    !
    ! ----------------
    !

    ! 2.1 input fields

    ! 2.1.a Opening field and data files

    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,950)
    print *,'ZZZ flflg ',flflg
!    IF ( FLFLG ) THEN
!       IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951)                  &
!            'Preparing input files ...'
!                 print *,'XXX prep input j ',j
!          call debuginit_msg(740+iaproc, 'Preparing input files ...', debuginit_flag)
!
!       DO J=JFIRST, 6
!          write(printmsg,*)'J=',J,'INFLAGS1(J)=',INFLAGS1(J), 'FLAGSC(J)=', FLAGSC(J)
!          call debuginit_msg(740+iaproc, trim(printmsg), debuginit_flag)
!          IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
!             IF ( FLH(J) ) THEN
!                IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
!             ELSE
!                FLAGTIDE = 0
!                CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST,     &
!                     NDSEN, NX, NY, GTYPE,               &
!                     IERR, FPRE=TRIM(FNMPRE), TIDEFLAGIN=FLAGTIDE )
!                IF ( IERR .NE. 0 ) GOTO 2222
!#ifdef W3_TIDE
!                !?? is ifdef even required?
!                IF (FLAGTIDE.GT.0.AND.J.EQ.1) FLAGSTIDE(1)=.TRUE.
!                IF (FLAGTIDE.GT.0.AND.J.EQ.2) FLAGSTIDE(2)=.TRUE.
!#endif
!                IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,955) IDFLDS(J)
!             END IF
!          ELSE
!             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
!          END IF
!       END DO
!
!       DO J=7, 9
!          IF ( INFLAGS1(J) .AND. .NOT. FLAGSC(J)) THEN
!             CALL W3FLDO ('READ', IDSTR(J), NDSF(J), NDST, NDSEN, &
!                  RCLD(J), NY, NODATA(J),                 &
!                  IERR, FPRE=TRIM(FNMPRE) )
!             IF ( IERR .NE. 0 ) GOTO 2222
!             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,956) IDFLDS(J),&
!                  RCLD(J), NODATA(J)
!          ELSE
!             IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,954) IDFLDS(J)
!          END IF
!       END DO
!
!    END IF ! FLFLG

    print *,'XXXX done fflg j ',j
#ifdef W3_MEMCHECK
    write(740+IAPROC,*) 'memcheck_____:', 'wav_shel_inp SECTION 4'
    call getMallocInfo(mallinfos)
    call printMallInfo(IAPROC,mallInfos)
#endif


    ! 2.2 Time setup
    print *,'XXXX timesetup j ',j

    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,930)
    CALL STME21 ( TIME0 , DTME21 )
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,931) DTME21
    TIME = TIME0
    CALL STME21 ( TIMEN , DTME21 )
    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,932) DTME21
#ifdef W3_OASIS
    TIME00 = TIME0
    TIMEEND = TIMEN
#endif
#ifdef W3_NL5
    QI5TBEG = TIME0
#endif

    DTTST  = DSEC21 ( TIME0 , TIMEN )
    IF ( DTTST .LE. 0. ) GOTO 2003
    print *,'XXXX timesetup j ',j


    ! 2.3 Domain setup

    IOSTYP = MAX ( 0 , MIN ( 3 , IOSTYP ) )
#ifdef W3_PDLIB
    IF (IOSTYP .gt. 1) THEN
       WRITE(*,*) 'IOSTYP not supported in domain decomposition mode'
       CALL EXTCDE ( 6666 )
    ENDIF
#endif

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
    !
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
    !
    ! 2.5 Output types

    if (t_flag) then
       WRITE (NDST,9040) ODAT
       WRITE (NDST,9041) FLGRD
       WRITE (NDST,9042) IPRT, PRTFRM
    end if

    !
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
    !
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
    !
#ifdef W3_MEMCHECK
    write(740+IAPROC,*) 'memcheck_____:', 'wav_shel_inp SECTION 5'
    call getMallocInfo(mallinfos)
    call printMallInfo(IAPROC,mallInfos)
#endif
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    IF ( IAPROC .EQ. NAPOUT ) WRITE (NDSO,951) 'Wave model ...'
    GOTO 2222

    ! Error escape locations
2001 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
    CALL EXTCDE ( 1001 )
2002 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
    CALL EXTCDE ( 1002 )
2102 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1102)
    CALL EXTCDE ( 1102 )
2003 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1003)
    CALL EXTCDE ( 1003 )
2104 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1104) IERR
    CALL EXTCDE ( 1104 )
2004 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1004) IERR
    CALL EXTCDE ( 1004 )
2005 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1005) IDTST
    CALL EXTCDE ( 1005 )
2054 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1054)
    CALL EXTCDE ( 1054 )
2006 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1006) IDTST, NH(J)
    CALL EXTCDE ( 1006 )
2062 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1062) IDTST
    CALL EXTCDE ( 1062 )
2007 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1007)
    CALL EXTCDE ( 1007 )
2009 CONTINUE
    IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1009) ODAT(33), NINT(DTMAX)
    CALL EXTCDE ( 1009 )
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
954 FORMAT ( '            ',A,': file not needed')
955 FORMAT ( '            ',A,': file OK')
956 FORMAT ( '            ',A,': file OK, recl =',I3,               &
         '  undef = ',E10.3)
1944 FORMAT ( '            Interval : ', 8X,A11/)
2944 FORMAT ( '            Interval : ', 9X,A10/)
3944 FORMAT ( '            Interval : ',11X,A8/)
2945 FORMAT ( '            Point  1 : ',2F8.2,2X,A)
2955 FORMAT ( '            Point  1 : ',2(F8.1,'E3'),2X,A)
2946 FORMAT ( '              ',I6,' : ',2F8.2,2X,A)
2956 FORMAT ( '              ',I6,' : ',2(F8.1,'E3'),2X,A)
2947 FORMAT ( '            No points defined')
3945 FORMAT ( '            The file with ',A,' data is ',A,'.')
6945 FORMAT ( '            IX first,last,inc :',3I5/                 &
         '            IY first,last,inc :',3I5/                 &
         '            Formatted file    :    ',A)
8945 FORMAT ( '            output dates out of run dates : ', A,     &
         ' deactivated')
950 FORMAT (/'  Initializations :'/                                 &
         ' --------------------------------------------------')
951 FORMAT ( '       ',A)
952 FORMAT ( '       ',I6,2X,A)
953 FORMAT ( '          ',I6,I11.8,I7.6,3E12.4)
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     PREMATURE END OF INPUT FILE'/)
1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     ERROR IN READING FROM INPUT FILE'/               &
         '     IOSTAT =',I5/)
1102 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     LEVEL AND CURRENT ARE MIXING COUPLED AND FORCED'/&
         '     IT MUST BE FULLY COUPLED OR DISABLED '/)
1003 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     ILLEGAL TIME INTERVAL'/)
1104 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     ERROR IN OPENING POINT FILE'/                    &
         '     IOSTAT =',I5/)
1004 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     ERROR IN READING FROM POINT FILE'/               &
         '     IOSTAT =',I5/)
1005 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     ILLEGAL ID STRING HOMOGENEOUS FIELD : ',A/)
1006 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     TOO MANY HOMOGENEOUS FIELDS : ',A,1X,I4/)
1062 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : ***'/             &
         '     HOMOGENEOUS NAME NOT RECOGNIZED : ', A/)
1007 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     INSUFFICIENT DATA FOR HOMOGENEOUS FIELDS'/)
1008 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     ERROR IN OPENING OUTPUT FILE'/                   &
         '     IOSTAT =',I5/)
1009 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     COUPLING TIME STEP NOT MULTIPLE OF'/             &
         '     MODEL TIME STEP: ',I6, I6/)
1010 FORMAT (/' *** WAVEWATCH III WARNING IN W3SHEL : *** '/         &
         '     COUPLING TIME STEP NOT DEFINED, '/               &
         '     IT WILL BE OVERRIDEN TO DEFAULT VALUE'/          &
         '     FROM ',I6, ' TO ',I6/)
1054 FORMAT (/' *** WAVEWATCH III ERROR IN W3SHEL : *** '/           &
         '     POINT OUTPUT ACTIVATED BUT NO POINTS DEFINED'/)
9000 FORMAT ( ' TEST W3SHEL : UNIT NUMBERS  :',12I4)
9001 FORMAT ( ' TEST W3SHEL : SUBR. TRACING :',2I4)
9020 FORMAT ( ' TEST W3SHEL : FLAGS DEF / HOM  : ',9L2,2X,9L2)
9040 FORMAT ( ' TEST W3SHEL : ODAT   : ',I9.8,I7.6,I7,I9.8,I7.6,  &
         4(/24X,I9.8,I7.6,I7,I9.8,I7.6) )
9041 FORMAT ( ' TEST W3SHEL : FLGRD  : ',20L2)
9042 FORMAT ( ' TEST W3SHEL : IPR, PRFRM : ',6I6,1X,L1)

  end subroutine read_shel_inp

end module wav_shel_inp
