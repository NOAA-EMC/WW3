!> @file wav_shel_inp
!!
!>  Set up for running in shel mode
!!
!> @details Contains public routines to sets up IO unit numbers and to
!! either reads a shel configuration file (either ww3_shel.inp or ww3_shel.nml)
!!
!> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
!> @date 01-05-2022
module wav_shel_inp

  use w3odatmd, only: nogrp, ngrpp

  implicit none
  private ! except

  public  :: set_shel_io         !< @public set the IO unit numbers
  public  :: read_shel_config    !< @public reads ww3_shel.nml if present, otherwise
                                 !! read ww3_shel.inp

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

  !===============================================================================
  !> Read ww3_shel.inp Or ww3_shel.nml
  !!
  !! @param[in]  mpi_comm           mpi communicator
  !!
  !> @author mvertens@ucar.edu, Denise.Worthen@noaa.gov
  !> @date 01-05-2022
  subroutine read_shel_config(mpi_comm, mds, time0_overwrite, timen_overwrite)

    use wav_shr_flags
    use w3nmlshelmd    , only : nml_domain_t, nml_input_t, nml_output_type_t
    use w3nmlshelmd    , only : nml_output_date_t, nml_homog_count_t, nml_homog_input_t
    use w3nmlshelmd    , only : w3nmlshel
    use w3gdatmd       , only : flagll, dtmax, nx, ny, gtype
    use w3wdatmd       , only : time, w3ndat, w3dimw, w3setw
    use w3adatmd       , only : w3naux, w3dima, w3seta
    use w3idatmd       , only : inflags1, inflags2, flagsc
    use w3odatmd       , only : w3nout, w3seto, nds
    use w3odatmd       , only : naproc, iaproc, napout, naperr
    use w3odatmd       , only : idout, fnmpre, iostyp, notype
    use w3odatmd       , only : flogrr, flogr, ofiles
    use w3iogrmd       , only : w3iogr
    use w3iogomd       , only : w3readflgrd, fldout, w3flgrdflag
    use w3servmd       , only : nextln, extcde, print_memcheck
    use w3timemd       , only : dsec21, stme21, tick21, t2d, d2j
#ifdef W3_OASIS
    use w3wdatmd       , only : time00, timeend
#endif
#ifdef W3_NL5
    use w3wdatmd       , only : qi5tbeg
#endif

    ! input/output parameters
    integer, intent(in) :: mpi_comm
    integer, intent(in) :: mds(:)
    integer, intent(in), optional :: time0_overwrite(2)
    integer, intent(in), optional :: timen_overwrite(2)

    ! local parameters
    integer, parameter  :: nhmax =    200

    type(nml_domain_t)       :: nml_domain
    type(nml_input_t)        :: nml_input
    type(nml_output_type_t)  :: nml_output_type
    type(nml_output_date_t)  :: nml_output_date
    type(nml_homog_count_t)  :: nml_homog_count
    type(nml_homog_input_t), allocatable  :: nml_homog_input(:)

    integer             :: ndsi, ndsi2, ndss, ndso, ndse, ndst, ndsl
    integer             :: ndsm, ndsen, ierr, j, i, iloop, ipts
    integer             :: nh(-7:10), tho(2,-7:10,nhmax), rcld(7:9)
    integer             :: nodata(7:9), startdate(8), stopdate(8), ihh(-7:10)
    integer             :: jfirst, ierr_mpi, flagtide, ih, n_tot
    real                :: factor, dttst, xx, yy, ha(nhmax,-7:10)
    real                :: hd(nhmax,-7:10), hs(nhmax,-7:10)
    double precision    :: startjulday, stopjulday
    character(len=1)    :: comstr, flagtfc(-7:10)
    character(len=3)    :: idstr(-7:10), idtst
    character(len=6)    :: yesxno
    character(len=40)   :: pn
    character(len=13)   :: idflds(-7:10)
    character(len=20)   :: strng
    character(len=23)   :: dtme21
    character(len=30)   :: idotyp(8)
    character(len=80)   :: line
    character(len=256)  :: tmpline, test
    character(len=1024) :: fldrst=''
    character(len=80)   :: linein
    character(len=30)   :: ofile ! w3_cou only
    character(len=8)    :: words(7)=''
    logical             :: flflg, flhom, tflagi, prtfrm, flgnml, flh(-7:10)
    integer             :: thrlev = 1
    integer             :: time0(2), timen(2), ttime(2)
    character(len=80)   :: msg1
    logical             :: is_open
    integer             :: memunit

    data idflds / 'ice param. 1 ' , 'ice param. 2 ' , &
         'ice param. 3 ' , 'ice param. 4 ' ,          &
         'ice param. 5 ' ,                            &
         'mud density  ' , 'mud thkness  ' ,          &
         'mud viscos.  ' ,                            &
         'water levels ' , 'currents     ' ,          &
         'winds        ' , 'ice fields   ' ,          &
         'momentum     ' , 'air density  ' ,          &
         'mean param.  ' , '1D spectra   ' ,          &
         '2D spectra   ' , 'moving grid  ' /
    data idotyp / 'Fields of mean wave parameters' ,  &
         'Point output                  ' ,           &
         'Track point output            ' ,           &
         'Restart files                 ' ,           &
         'Nesting data                  ' ,           &
         'Partitioned wave field data   ' ,           &
         'Fields for coupling           ' ,           &
         'Restart files second request  '/
    data idstr  / 'IC1', 'IC2', 'IC3', 'IC4', 'IC5', 'MDN', 'MTH', 'MVS', 'LEV', 'CUR', &
         'WND', 'ICE', 'TAU', 'RHO', 'DT0', 'DT1', 'DT2', 'MOV' /
    !---------------------------------------------------
    !
    !---------------------------------------------------

    flgr2 = .false.
    flh(:) = .false.
    iprt(:) = 0
    memunit = 740+IAPROC
    call print_logmsg(740+IAPROC, 'read_shel_config, step 1', w3_debuginit_flag)

    ! ndso, ndse, ndst are set in w3initmd using mds;  w3initmd is called by either
    ! cesm_init or uwm_int after calling the read_shel_config routine
    ndso =  mds(1)
    ndse =  mds(1)
    ndst =  mds(1)
    ! set a unit number passed to w3iogr routine for reading mod_def file; this unit
    ! is closed at the end of w3iogr
    ndsm = 17
    inquire(unit=ndsm, opened=is_open)
    if (is_open) then
      call extcde (60, msg='unit ndsm is already in use ')
    end if
    ndss = 90
    inquire(unit=ndss, opened=is_open)
    if (is_open) then
      call extcde (60, msg='unit ndss is already in use ')
    end if
    ! naperr is set in InitializeRealize
    if ( iaproc .eq. naperr ) then
      ndsen  = ndse
    else
      ndsen  = -1
    end if
#ifdef W3_OMPH
    if ( iaproc .eq. napout ) write (ndso,905) MPI_THREAD_FUNNELED, thrlev
#endif

    ! 1.c Local parameters

    ! Default COMSTR to "$" (for when using nml input files)
    COMSTR = "$"
    call print_logmsg(740+IAPROC, 'read_shel_config, step 2', w3_debuginit_flag)

    ! If using experimental mud or ice physics, additional lines will
    !  be read in from read_shel_config.inp and applied, so JFIRST is
    !  changed from its initialization setting "JFIRST=1" to some
    !  lower value.
    jfirst=1
    if (w3_ic1_flag) jfirst = -7
    if (w3_ic2_flag) jfirst = -7
    if (w3_is2_flag) jfirst = -7
    if (w3_ic3_flag) jfirst = -7
    if (w3_bt8_flag) jfirst = -7
    if (w3_bt9_flag) jfirst = -7
    if (w3_ic4_flag) jfirst = -7
    if (w3_ic5_flag) jfirst = -7

    write(msg1,*)'JFIRST=', JFIRST
    call print_logmsg(740+IAPROC, 'read_shel_config, step 4', trim(msg1), w3_debuginit_flag)

    !--------------------
    ! Read nml file if available
    !--------------------

    inquire(file=trim(fnmpre)//"ww3_shel.nml", exist=flgnml)

    if (flgnml) then
      open(newunit=ndsi, file=trim(fnmpre)//"ww3_shel.nml", status='old', iostat=ierr)

      !--------------------
      ! Read namelist
      !--------------------

      call w3nmlshel (mpi_comm, ndsi, trim(fnmpre)//'ww3_shel.nml', nml_domain, nml_input, &
           nml_output_type, nml_output_date, nml_homog_count, nml_homog_input, ierr)

      !--------------------
      ! 2.1 forcing flags
      !--------------------

      flh(-7:10)  = .false.
      flagtfc(-7) = trim(nml_input%forcing%ice_param1)
      flagtfc(-6) = trim(nml_input%forcing%ice_param2)
      flagtfc(-5) = trim(nml_input%forcing%ice_param3)
      flagtfc(-4) = trim(nml_input%forcing%ice_param4)
      flagtfc(-3) = trim(nml_input%forcing%ice_param5)
      flagtfc(-2) = trim(nml_input%forcing%mud_density)
      flagtfc(-1) = trim(nml_input%forcing%mud_thickness)
      flagtfc(0)  = trim(nml_input%forcing%mud_viscosity)
      flagtfc(1)  = trim(nml_input%forcing%water_levels)
      flagtfc(2)  = trim(nml_input%forcing%currents)
      flagtfc(3)  = trim(nml_input%forcing%winds)
      flagtfc(4)  = trim(nml_input%forcing%ice_conc)
      flagtfc(5)  = trim(nml_input%forcing%atm_momentum)
      flagtfc(6)  = trim(nml_input%forcing%air_density)
      flagtfc(7)  = trim(nml_input%assim%mean)
      flagtfc(8)  = trim(nml_input%assim%spec1d)
      flagtfc(9)  = trim(nml_input%assim%spec2d)

      if (trim(nml_input%forcing%ice_param1) .eq. 'H') then
        flagtfc(-7)='T'
        flh(-7)=.true.
      end if
      if (trim(nml_input%forcing%ice_param2) .eq. 'H') THEN
        flagtfc(-6)='T'
        flh(-6)=.true.
      end if
      if (trim(nml_input%forcing%ice_param3) .eq. 'H') THEN
        flagtfc(-5)='T'
        flh(-5)=.true.
      end if
      if (trim(nml_input%forcing%ice_param4) .eq. 'H') THEN
        flagtfc(-4)='T'
        flh(-4)=.true.
      end if
      if (trim(nml_input%forcing%ice_param5) .eq. 'H') THEN
        flagtfc(-3)='T'
        flh(-3)=.true.
      end if
      if (trim(nml_input%forcing%mud_density) .eq. 'H') THEN
        flagtfc(-2)='T'
        flh(-2)=.true.
      end if
      if (trim(nml_input%forcing%mud_thickness) .eq. 'H') THEN
        flagtfc(-1)='T'
        flh(-1)=.true.
      end if
      if (trim(nml_input%forcing%mud_viscosity) .eq. 'H') THEN
        flagtfc(0)='T'
        flh(0)=.true.
      end if
      if (trim(nml_input%forcing%water_levels) .eq. 'H') THEN
        flagtfc(1)='T'
        flh(1)=.true.
      end if
      if (trim(nml_input%forcing%currents) .eq. 'H') THEN
        flagtfc(2)='T'
        flh(2)=.true.
      end if
      if (trim(nml_input%forcing%winds) .eq. 'H') THEN
        flagtfc(3)='T'
        flh(3)=.true.
      end if
      if (trim(nml_input%forcing%ice_conc) .eq. 'H') THEN
        flagtfc(4)='T'
        flh(4)=.true.
      end if
      if (trim(nml_input%forcing%atm_momentum) .eq. 'H') THEN
        flagtfc(5)='T'
        flh(5)=.true.
      end if
      if (trim(nml_input%forcing%air_density) .eq. 'H') THEN
        flagtfc(6)='T'
        flh(6)=.true.
      end if

      if ( iaproc .eq. napout ) write (ndso, 920)
      DO J=JFIRST, 9
        if (flagtfc(j).eq.'T') THEN
          inflags1(j)=.true.
          flagsc(j)=.false.
        end if
        if (flagtfc(j).eq.'F') THEN
          inflags1(j)=.false.
          flagsc(j)=.false.
        end if
        if (flagtfc(j).eq.'C') THEN
          inflags1(j)=.true.
          flagsc(j)=.true.
        end if
        if ( j .le. 6 ) then
          flh(j) = flh(j) .and. inflags1(j)
        end if
        if ( inflags1(j) ) then
          yesxno = 'YES/--'
        else
          yesxno = '---/NO'
        end IF
        if ( flh(j) ) then
          strng  = '(homogeneous field) '
        else if ( flagsc(j) ) then
          strng  = '(coupling field) '
        else
          strng  = '                    '
        end if
        if ( iaproc .eq. napout ) write (ndso,921) idflds(j), yesxno, strng
      end do
      if (w3_cou_flag) then
        if (flagsc(1) .and. inflags1(2) .and. .not. flagsc(2)) goto 2102
        if (flagsc(2) .and. inflags1(1) .and. .not. flagsc(1)) goto 2102
      end if

      inflags1(10) = .false.
      if (w3_mgw_flag .or. w3_mgp_flag) then
        inflags1(10) = .true.
        flh(10)   = .true.
      end if
      if ( inflags1(10) .and. iaproc.eq.napout ) write (ndso,921) idflds(10), 'yes/--', ' '

      flflg  = inflags1(-7) .or. inflags1(-6) .or. inflags1(-5) .or. inflags1(-4) &
                            .or. inflags1(-3) .or. inflags1(-2) .or. inflags1(-1) &
                            .or. inflags1(0)  .or. inflags1(1)  .or. inflags1(2)  &
                            .or. inflags1(3)  .or. inflags1(4)  .or. inflags1(5)  &
                            .or. inflags1(6)  .or. inflags1(7)  .or. inflags1(8)  &
                            .or. inflags1(9)
      flhom  = flh(-7) .or. flh(-6) .or. flh(-5) .or. flh(-4)              &
                       .or. flh(-3) .or. flh(-2) .or. flh(-1) .or. flh(0)  &
                       .or. flh(1)  .or. flh(2)  .or. flh(3)  .or. flh(4)  &
                       .or. flh(5)  .or. flh(6)  .or. flh(10)

      if ( iaproc .eq. napout ) write (ndso,922)
      ! inflags2 is just "initial value of inflags1", i.e. does *not* get changed when
      ! model reads last record of ice.ww3
      inflags2=inflags1
      if (w3_t_flag) then
        write (ndst,9020) flflg, inflags1, flhom, flh
      end if

      !--------------------
      ! 2.2 Time setup
      !--------------------

      read (nml_domain%start,*) time0
      call t2d(time0,startdate,ierr)
      call d2j(startdate,startjulday,ierr)
      read(nml_domain%stop,*) timen
      call t2d(timen,stopdate,ierr)
      call d2j(stopdate,stopjulday,ierr)

      !--------------------
      ! 2.3 Domain setup
      !--------------------

      iostyp = nml_domain%iostyp
      if (w3_pdlib_flag) then
        if (iostyp .gt. 1) then
          write(*,*) 'iostyp not supported in domain decomposition mode'
          call extcde ( 6666 )
        endif
      end if

      call w3iogr ( 'GRID', ndsm )
      if ( flagll ) then
        factor = 1.
      else
        factor = 1.e-3
      end if

      !--------------------
      ! 2.4 Output dates
      !--------------------

      read(nml_output_date%field%start,    *)   odat(1), odat(2)
      read(nml_output_date%field%stride,   *)   odat(3)
      read(nml_output_date%field%stop,     *)   odat(4), odat(5)

      read(nml_output_date%field%outffile, *)  ofiles(1)

      read(nml_output_date%point%start,    *)   odat(6), odat(7)
      read(nml_output_date%point%stride,   *)   odat(8)
      read(nml_output_date%point%stop,     *)   odat(9), odat(10)

      read(nml_output_date%point%outffile, *)  ofiles(2)

      read(nml_output_date%track%start,      *) odat(11), odat(12)
      read(nml_output_date%track%stride,     *) odat(13)
      read(nml_output_date%track%stop,       *) odat(14), odat(15)

      read(nml_output_date%restart%start,    *) odat(16), odat(17)
      read(nml_output_date%restart%stride,   *) odat(18)
      read(nml_output_date%restart%stop,     *) odat(19), odat(20)

      read(nml_output_date%restart2%start,   *) odat(36), odat(37)
      read(nml_output_date%restart2%stride,  *) odat(38)
      read(nml_output_date%restart2%stop,    *) odat(39), odat(40)

      read(nml_output_date%boundary%start,   *) odat(21), odat(22)
      read(nml_output_date%boundary%stride,  *) odat(23)
      read(nml_output_date%boundary%stop,    *) odat(24), odat(25)

      read(nml_output_date%partition%start,  *) odat(26), odat(27)
      read(nml_output_date%partition%stride, *) odat(28)
      read(nml_output_date%partition%stop,   *) odat(29), odat(30)

      read(nml_output_date%coupling%start,   *) odat(31), odat(32)
      read(nml_output_date%coupling%stride,  *) odat(33)
      read(nml_output_date%coupling%stop,    *) odat(34), odat(35)

      ! set the time stride at 0 or more
      odat(3) = max ( 0 , odat(3) )
      odat(8) = max ( 0 , odat(8) )
      odat(13) = max ( 0 , odat(13) )
      odat(18) = max ( 0 , odat(18) )
      odat(23) = max ( 0 , odat(23) )
      odat(28) = max ( 0 , odat(28) )
      odat(33) = max ( 0 , odat(33) )
      odat(38) = max ( 0 , odat(38) )

      if (w3_cou_flag) then
        ! test the validity of the coupling time step
        if (odat(33) == 0) then
          if ( iaproc .eq. napout ) then
            write(ndso,1010) odat(33), int(dtmax)
          end if
          odat(33) = int(dtmax)
        else if (mod(odat(33),int(dtmax)) .ne. 0) then
          goto 2009
        end if
      end if

      !--------------------
      ! 2.5 Output types
      !--------------------

      npts   = 0
      notype = 6
      if (w3_cou_flag) then
        notype = 7
      end if
      do j = 1, notype

        ! outpts(i)%ofiles(j)=ofiles(j)
        if ( odat(5*(j-1)+3) .ne. 0 ) then

          if ( j .eq. 1 ) then

            ! type 1: fields of mean wave parameters
            fldout = nml_output_type%field%list
            call w3flgrdflag ( ndso, ndso, ndse, fldout, flgd, flgrd, iaproc, napout, ierr )
            if ( ierr .ne. 0 ) goto 2222

          else if ( j .eq. 2 ) then

            ! type 2: point output
            open (newunit=ndsl, file=trim(fnmpre)//trim(nml_output_type%point%file), &
                 form='formatted', status='old', err=2104, iostat=ierr)

            ! first loop to count the number of points
            ! second loop to allocate the array and store the points
            ipts = 0
            do iloop=1,2
              rewind (ndsl)

              if ( iloop.eq.2) then
                npts = ipts
                if ( npts.gt.0 ) then
                  allocate ( x(npts), y(npts), pnames(npts) )
                  ipts = 0 ! reset counter to be reused for next do loop
                else
                  allocate ( x(1), y(1), pnames(1) )
                  goto 2054
                end if
              end if

              do
                read (ndsl,*,err=2004,iostat=ierr) tmpline
                ! if end of file or stopstring, then exit
                if ( ierr.ne.0 .or. index(tmpline,"STOPSTRING").ne.0 ) exit

                ! leading blanks removed and placed on the right
                test = adjustl ( tmpline )
                if ( test(1:1).eq.comstr .or. len_trim(test).eq.0 ) then
                  ! if comment or blank line, then skip
                  cycle
                else
                  ! otherwise, backup to beginning of line
                  backspace ( ndsl, err=2004, iostat=ierr)
                  read (ndsl,*,err=2004,iostat=ierr) xx, yy, pn
                end if
                ipts = ipts + 1
                if ( iloop .eq. 1 ) cycle
                if ( iloop .eq. 2 ) then
                  x(ipts)      = xx
                  y(ipts)      = yy
                  pnames(ipts) = pn
                  if ( iaproc .eq. napout ) then
                    if ( flagll ) then
                      if ( ipts .eq. 1 ) then
                        write (ndso,2945) factor*xx, factor*yy, pn
                      else
                        write (ndso,2946) ipts, factor*xx, factor*yy, pn
                      end if
                    else
                      if ( ipts .eq. 1 ) then
                        write (ndso,2955) factor*xx, factor*yy, pn
                      else
                        write (ndso,2956) ipts, factor*xx, factor*yy, pn
                      end if
                    end if
                  end if
                end if ! iloop.eq.2
              end do ! end of file
            end do ! iloop
            close(ndsl)

          else if ( j .eq. 3 ) then

            ! Type 3: track output
            tflagi = nml_output_type%track%format
            if ( .not. tflagi ) nds(11) = -nds(11)
            if ( iaproc .eq. napout ) then
              if ( .not. tflagi ) then
                write (ndso,3945) 'input', 'unformatted'
              else
                write (ndso,3945) 'input', 'formatted'
              end if
            end if

          else if ( j .eq. 6 ) then

            ! Type 6: partitioning
            iprt(1) = nml_output_type%partition%x0
            iprt(2) = nml_output_type%partition%xn
            iprt(3) = nml_output_type%partition%nx
            iprt(4) = nml_output_type%partition%y0
            iprt(5) = nml_output_type%partition%yn
            iprt(6) = nml_output_type%partition%ny
            prtfrm = nml_output_type%partition%format

            if ( iaproc .eq. napout ) then
              if ( prtfrm ) then
                yesxno = 'YES/--'
              else
                yesxno = '---/NO'
              end if
              write (ndso,6945) iprt, yesxno
            end if

          else if ( j .eq. 7 ) then
#ifdef W3_COU
            ! Type 7: coupling
            fldout = nml_output_type%coupling%sent
            call w3flgrdflag ( ndso, ndso, ndse, fldout, flg2, flgr2, iaproc, napout, ierr )
            if ( ierr .ne. 0 ) goto 2222
            fldin = nml_output_type%coupling%received
            cplt0 = nml_output_type%coupling%couplet0
#endif

          end if ! j
        end if ! odat
      end do ! j

      ! Extra fields to be written in the restart
      fldrst = nml_output_type%restart%extra
      call w3flgrdflag ( ndso, ndso, ndse, fldrst, flogr, flogrr, iaproc, napout, ierr )
      if ( ierr .ne. 0 ) goto 2222

      ! force minimal allocation to avoid memory seg fault
      if ( .not.allocated(x) .and. npts.eq.0 ) allocate ( x(1), y(1), pnames(1) )

      !--------------------
      ! 2.6 Homogeneous field data
      !--------------------

      if ( flhom ) then
        if ( iaproc .eq. napout ) write (ndso,951)                   &
             'Homogeneous field data (and moving grid) ...'

        nh(-7) = nml_homog_count%n_ic1
        nh(-6) = nml_homog_count%n_ic2
        nh(-5) = nml_homog_count%n_ic3
        nh(-4) = nml_homog_count%n_ic4
        nh(-3) = nml_homog_count%n_ic5
        nh(-2) = nml_homog_count%n_mdn
        nh(-1) = nml_homog_count%n_mth
        nh(0)  = nml_homog_count%n_mvs
        nh(1)  = nml_homog_count%n_lev
        nh(2)  = nml_homog_count%n_cur
        nh(3)  = nml_homog_count%n_wnd
        nh(4)  = nml_homog_count%n_ice
        nh(5)  = nml_homog_count%n_tau
        nh(6)  = nml_homog_count%n_rho
        nh(10)  = nml_homog_count%n_mov

        n_tot = nml_homog_count%n_tot

        do j=jfirst,10
          if ( nh(j) .gt. nhmax ) goto 2006
        end do

        ! Store homogeneous fields
        if ( n_tot .gt. 0 ) then
          ihh(:)=0
          do ih=1,n_tot
            read(nml_homog_input(ih)%name,*) idtst
            select case (idtst)
            case ('IC1')
              j=-7
            case ('IC2')
              j=-6
            case ('IC3')
              j=-5
            case ('IC4')
              j=-4
            case ('IC5')
              j=-3
            case ('MDN')
              j=-2
            case ('MTH')
              j=-1
            case ('MVS')
              j=0
            case ('LEV')
              j=1
            case ('CUR')
              j=2
            case ('WND')
              j=3
            case ('ICE')
              j=4
            case ('TAU')
              j=5
            case ('RHO')
              j=6
            case ('MOV')
              j=10
            case DEFAULT
              goto 2062
            end SELECT
            ihh(j)=ihh(j)+1
            read(nml_homog_input(ih)%date,*) tho(:,j,ihh(j))
            ha(ihh(j),j) = nml_homog_input(ih)%value1
            hd(ihh(j),j) = nml_homog_input(ih)%value2
            hs(ihh(j),j) = nml_homog_input(ih)%value3
          end do
        end if

        if (w3_o7_flag) then
          do j=jfirst, 10
            if ( flh(j) .and. iaproc.eq.napout ) then
              write (ndso,952) nh(j), idflds(j)
              do i=1, nh(j)
                if ( ( j .le. 1 ) .or. ( j .eq. 4 ) .or. ( j .eq. 6 ) ) then
                  write (ndso,953) i, tho(1,j,i), tho(2,j,i), ha(i,j)
                else if ( ( j .eq. 2 ) .or. ( j .eq. 5 ) .or. ( j .eq. 10 ) ) then
                  write (ndso,953) i, tho(1,j,i), tho(2,j,i), ha(i,j), hd(i,j)
                else if ( j .eq. 3 ) then
                  write (ndso,953) i, tho(1,j,i), tho(2,j,i), ha(i,j), hd(i,j), hs(i,j)
                end if
              end do
            end if
          end do
        end if

        if ( ( flh(-7) .and. (nh(-7).eq.0) ) .or. &
             ( flh(-6) .and. (nh(-6).eq.0) ) .or. &
             ( flh(-5) .and. (nh(-5).eq.0) ) .or. &
             ( flh(-4) .and. (nh(-4).eq.0) ) .or. &
             ( flh(-3) .and. (nh(-3).eq.0) ) .or. &
             ( flh(-2) .and. (nh(-2).eq.0) ) .or. &
             ( flh(-1) .and. (nh(-1).eq.0) ) .or. &
             ( flh(0)  .and. (nh(0).eq.0)  ) .or. &
             ( flh(1)  .and. (nh(1).eq.0)  ) .or. &
             ( flh(2)  .and. (nh(2).eq.0)  ) .or. &
             ( flh(3)  .and. (nh(3).eq.0)  ) .or. &
             ( flh(4)  .and. (nh(4).eq.0)  ) .or. &
             ( flh(5)  .and. (nh(5).eq.0)  ) .or. &
             ( flh(6)  .and. (nh(6).eq.0)  ) .or. &
             ( flh(10) .and. (nh(10).eq.0) ) ) goto 2007

      end if ! flhom

    end if ! flgnml

    !
    ! ===============================================================
    ! process old read_shel_config.inp format
    ! ===============================================================
    !
    if (.not. flgnml) then

      call print_logmsg(740+IAPROC, ' fnmpre'//trim(fnmpre), w3_debuginit_flag)
      open (newunit=ndsi,file=trim(fnmpre)//'ww3_shel.inp',status='old',iostat=ierr)
      rewind (ndsi)

      read (ndsi,'(a)') comstr
      if (comstr.eq.' ') comstr = '$'
      if ( iaproc .eq. napout ) write (ndso,901) comstr

      !--------------------
      ! 2.1 forcing flags
      !--------------------

      call print_logmsg(740+IAPROC, '2.1 Forcing flags', w3_debuginit_flag)
      flh(-7:10) = .false.
      do j=jfirst, 9
        call nextln ( comstr , ndsi , ndsen )
        if ( j .le. 6 ) then
          read (ndsi,*) flagtfc(j), flh(j)
        else
          read (ndsi,*) flagtfc(j)
        end if
        write(msg1,*)'     J=', j, ' FLAGTFC=', flagtfc(j), ' FLH=', flh(j)
        call print_logmsg(740+IAPROC, trim(msg1), w3_debuginit_flag)
      end do

      if ( iaproc .eq. napout ) write (ndso,920)
      do j=jfirst, 9
        if (flagtfc(j).eq.'T') then
          inflags1(j)=.true.
          flagsc(j)=.false.
        end if
        if (flagtfc(j).eq.'F') then
          inflags1(j)=.false.
          flagsc(j)=.false.
        end if
        if (flagtfc(j).eq.'C') then
          inflags1(j)=.true.
          flagsc(j)=.true.
        end if
        if ( j .le. 6 ) then
          flh(j) = flh(j) .and. inflags1(j)
        end if
        if ( inflags1(j) ) then
          yesxno = 'YES/--'
        else
          yesxno = '---/NO'
        end if
        if ( flh(j) ) then
          strng  = '(homogeneous field) '
        else if ( flagsc(j) ) then
          strng  = '(coupling field) '
        else
          strng  = '                    '
        end if
        if ( iaproc .eq. napout ) write (ndso,921) idflds(j), yesxno, strng
      end do
      if (w3_cou_flag) then
        if (flagsc(1) .and. inflags1(2) .and. .not. flagsc(2)) goto 2102
        if (flagsc(2) .and. inflags1(1) .and. .not. flagsc(1)) goto 2102
      end if

      call print_memcheck(memunit, 'memcheck_____:'//' read_shel_config SECTION 2b')

      inflags1(10) = .false.
      if (w3_mgw_flag .or. w3_mgp_flag) then
        inflags1(10) = .true.
        flh(10)   = .true.
      end if
      if ( inflags1(10) .and. iaproc.eq.napout )                         &
           write (ndso,921) idflds(10), 'yes/--', ' '

      flflg  = inflags1(-7) .or. inflags1(-6) .or. inflags1(-5) .or. inflags1(-4) &
                            .or. inflags1(-3) .or. inflags1(-2) .or. inflags1(-1) &
                            .or. inflags1(0)  .or. inflags1(1)  .or. inflags1(2)  &
                            .or. inflags1(3)  .or. inflags1(4)  .or. inflags1(5)  &
                            .or. inflags1(6)  .or. inflags1(7)  .or. inflags1(8)  &
                            .or. inflags1(9)
      flhom  = flh(-7) .or. flh(-6) .or. flh(-5) .or. flh(-4)              &
                       .or. flh(-3) .or. flh(-2) .or. flh(-1) .or. flh(0)  &
                       .or. flh(1)  .or. flh(2)  .or. flh(3)  .or. flh(4)  &
                       .or. flh(5)  .or. flh(6)  .or. flh(10)

      if ( iaproc .eq. napout ) write (ndso,922)
      ! inflags2 is just "initial value of inflags1", i.e. does *not* get changed when
      ! model reads last record of ice.ww3
      inflags2=inflags1

      if (w3_t_flag) then
        write (ndst,9020) flflg, inflags1, flhom, flh
      end if

      !--------------------
      ! 2.2 Time setup
      !--------------------
      call print_logmsg(740+IAPROC, '2.2 Time setup ',  w3_debuginit_flag)
      call nextln ( comstr , ndsi , ndsen )
      read (ndsi,*) time0

      call print_memcheck(memunit, 'memcheck_____:'//' read_shel_config SECTION 2c')

      call nextln ( comstr , ndsi , ndsen )
      read (ndsi,*) timen
      call print_memcheck(memunit, 'memcheck_____:'//' read_shel_config SECTION 2d')

      !--------------------
      ! 2.3 Domain setup
      !--------------------

      call print_logmsg(740+IAPROC, '2.3 Domain setup ',  w3_debuginit_flag)
      call nextln ( COMSTR , NDSI , NDSEN )
      read (ndsi,*) iostyp
      if (w3_pdlib_flag) then
        if (iostyp .gt. 1) then
          write(*,*) 'iostyp not supported in domain decomposition mode'
          call extcde ( 6666 )
        endif
      end if
      call w3iogr ( 'GRID', ndsm )
      if ( flagll ) then
        factor = 1.
      else
        factor = 1.e-3
      end if

      !--------------------
      ! 2.4 Output dates
      !--------------------

      call print_logmsg(740+IAPROC, '2.4 Output dates ',  w3_debuginit_flag)
      npts   = 0
      notype = 6
      if (w3_cou_flag) then
        notype = 7
      end if
      do j = 1, notype
        write(msg1,*)'J=', J, '/ NOTYPE=', NOTYPE
        call nextln ( comstr , ndsi , ndsen )

        ! checkpoint
        if (j .eq. 4) then
          odat(38)=0
          words(1:7)=''
          read (ndsi,'(a)') linein
          read(linein,*,iostat=ierr) words
          read(words( 1 ), * ) odat(16)
          read(words( 2 ), * ) odat(17)
          read(words( 3 ), * ) odat(18)
          read(words( 4 ), * ) odat(19)
          read(words( 5 ), * ) odat(20)
          if (words(6) .eq. 'T') then
            call nextln ( comstr , ndsi , ndsen )
            read (ndsi,*,end=2001,err=2002)(odat(i),i=5*(8-1)+1,5*8)
            if(iaproc .eq. naproc) write(*,*)'odat(j=4): ',(odat(i),i=5*(8-1)+1,5*8)
          end if
          if (words(7) .eq. 'T') then
            call nextln ( comstr , ndsi , ndsen )
            read (ndsi,'(a)',end=2001,err=2002) fldrst
          end if
          call w3flgrdflag ( ndso, ndso, ndse, fldrst, flogr, flogrr, iaproc, napout, ierr )
          if ( ierr .ne. 0 ) goto 2222
        else

          !inline new variable to read if present ofiles(j), if not ==0
          ! read (ndsi,*) (odat(i),i=5*(j-1)+1,5*j)
          ! read (ndsi,*,iostat=ierr) (odat(i),i=5*(j-1)+1,5*j),ofiles(j)
          if(j .le. 2) then
            words(1:6)=''
            ! read (ndsi,*,end=2001,err=2002)(odat(i),i=5*(j-1)+1,5*j),ofiles(j)
            read (ndsi,'(a)') linein
            read(linein,*,iostat=ierr) words

            if(j .eq. 1) then
              read(words( 1 ), * ) odat(1)
              read(words( 2 ), * ) odat(2)
              read(words( 3 ), * ) odat(3)
              read(words( 4 ), * ) odat(4)
              read(words( 5 ), * ) odat(5)
            else
              read(words( 1 ), * ) odat(6)
              read(words( 2 ), * ) odat(7)
              read(words( 3 ), * ) odat(8)
              read(words( 4 ), * ) odat(9)
              read(words( 5 ), * ) odat(10)
            end if

            if (words(6) .ne. '0' .and. words(6) .ne. '1') then
              ofiles(j)=0
            else
              read(words( 6 ), * ) ofiles(j)
            end if

          else if(j .eq. 7) then

#ifdef W3_COU
            words(1:6)=''
            read (ndsi,'(a)') linein
            read(linein,*,iostat=ierr) words

            read(words( 1 ), * ) odat(31)
            read(words( 2 ), * ) odat(32)
            read(words( 3 ), * ) odat(33)
            read(words( 4 ), * ) odat(34)
            read(words( 5 ), * ) odat(35)

            if (words(6) .eq. 'T') then
              cplt0 = .true.
            else
              cplt0 = .false.
            end if
#endif

          else

            ofiles(j)=0
            read (ndsi,*,end=2001,err=2002)(odat(i),i=5*(j-1)+1,5*j)

          end if !j le 2
          odat(5*(j-1)+3) = max ( 0 , odat(5*(j-1)+3) )
          write(msg1, *) 'read_shel_config NOTTYPE', J
          call print_memcheck(memunit, 'memcheck_____:'//trim(msg1))

          !--------------------
          ! 2.5 Output types
          !--------------------

          call print_logmsg(740+IAPROC, ' 2.5 Output types ', w3_debuginit_flag)
          if ( odat(5*(j-1)+3) .ne. 0 ) then
            if ( j .eq. 1 ) then

              ! type 1: fields of mean wave parameters
              call w3readflgrd ( ndsi, ndso, 9, ndsen, comstr, flgd, flgrd, iaproc, napout, ierr )
              if ( ierr .ne. 0 ) goto 2222

            else if ( j .eq. 2 ) then

              ! type 2: point output
              do iloop=1,2
                if ( iloop .eq. 1 ) then
                  ndsi2  = ndsi
                  if ( iaproc .eq. 1 ) open (ndss,file=trim(fnmpre)//'ww3_shel.scratch')
                else
                  ndsi2  = ndss
#ifdef W3_MPI
                  call mpi_barrier (mpi_comm,ierr_mpi)
#endif
                  open (ndss,file=trim(fnmpre)//'ww3_shel.scratch')
                  rewind (ndss)

                  if ( .not.allocated(x) ) then
                    if ( npts.gt.0 ) then
                      allocate ( x(npts), y(npts), pnames(npts) )
                    else
                      allocate ( x(1), y(1), pnames(1) )
                      goto 2054
                    end if
                  end if
                end if

                npts   = 0
                do
                  call nextln ( comstr , ndsi , ndsen )
                  read (ndsi2,*) xx, yy, pn
                  if ( iloop.eq.1 .and. iaproc.eq.1 ) then
                    backspace (ndsi)
                    read (ndsi,'(a)') line
                    write (ndss,'(a)') line
                  end if
                  if ( index(pn,"STOPSTRING").ne.0 ) exit
                  npts   = npts + 1
                  if ( iloop .eq. 1 ) cycle
                  x(npts)      = xx
                  y(npts)      = yy
                  pnames(npts) = pn
                  if ( iaproc .eq. napout ) then
                    if ( flagll ) then
                      if ( npts .eq. 1 ) then
                        write (ndso,2945) factor*xx, factor*yy, pn
                      else
                        write (ndso,2946) npts, factor*xx, factor*yy, pn
                      end if
                    else
                      if ( npts .eq. 1 ) then
                        write (ndso,2955) factor*xx, factor*yy, pn
                      else
                        write (ndso,2956) npts, factor*xx, factor*yy, pn
                      end if
                    end if
                  end if
                end do

                if ( iaproc.eq.1 .and. iloop.eq.1 ) close (ndss)
              end do

              if ( npts.eq.0 .and. iaproc.eq.napout ) write (ndso,2947)
              if ( iaproc .eq. 1 ) then
#ifdef W3_MPI
                call mpi_barrier ( mpi_comm, ierr_mpi )
#endif
                close (ndss,status='delete')
              else
#ifdef W3_MPI
                call mpi_barrier ( mpi_comm, ierr_mpi )
#endif
                close (ndss)
              end if

            else if ( j .eq. 3 ) then
              call print_logmsg(740+IAPROC, ' 2.5 Track output ', w3_debuginit_flag)
              ! Type 3: track output
              call nextln ( comstr , ndsi , ndsen )
              read (ndsi,*) tflagi
              if ( .not. tflagi ) nds(11) = -nds(11)
              if ( iaproc .eq. napout ) then
                if ( .not. tflagi ) then
                  write (ndso,3945) 'input', 'UNFORMATTED'
                else
                  write (ndso,3945) 'input', 'FORMATTED'
                end if
              end if

            else if ( j .eq. 6 ) then
              call print_logmsg(740+IAPROC, ' 2.6 Partitioning output ', w3_debuginit_flag)
              ! Type 6: partitioning
              !             IPRT: IX0, IXN, IXS, IY0, IYN, IYS
              call nextln ( comstr , ndsi , ndsen )
              read (ndsi,*) iprt, prtfrm

              if ( iaproc .eq. napout ) then
                if ( prtfrm ) then
                  yesxno = 'YES/--'
                else
                  yesxno = '---/NO'
                end if
                write (ndso,6945) iprt, yesxno
              end if

            else if ( j .eq. 7 ) then

              ! Type 7: coupling
#ifdef W3_COU
              call w3readflgrd ( ndsi, ndso, ndss, ndsen, comstr, flg2, flgr2, iaproc, napout, ierr )
              if ( ierr .ne. 0 ) goto 2222
              call nextln ( comstr , ndsi , ndsen )
              read (ndsi,'(a)',end=2001,err=2002,iostat=ierr) fldin
#endif

            end if ! j

          end if ! odat
        end if ! if j=4
      end do ! j

      ! force minimal allocation to avoid memory seg fault
      if ( .not.allocated(x) .and. npts.eq.0 ) allocate ( x(1), y(1), pnames(1) )

      !--------------------
      ! 2.6 Homogeneous field data
      !--------------------

      call print_logmsg(740+IAPROC, ' 2.6 Homogeneous field data ', w3_debuginit_flag)
      if ( flhom ) then
        if ( iaproc .eq. napout ) write (ndso,951) 'homogeneous field data (and moving grid) ...'
        nh = 0

        ! Start of loop
        do
          call nextln ( comstr , ndsi , ndsen )
          read (ndsi,*) idtst
          ! Exit if illegal id
          if ( idtst.ne.idstr(-7) .and. idtst.ne.idstr(-6) .and.   &
               idtst.ne.idstr(-5) .and. idtst.ne.idstr(-4) .and.   &
               idtst.ne.idstr(-3) .and. idtst.ne.idstr(-2) .and.   &
               idtst.ne.idstr(-1) .and. idtst.ne.idstr(0)  .and.   &
               idtst.ne.idstr(1)  .and. idtst.ne.idstr(2)  .and.   &
               idtst.ne.idstr(3)  .and. idtst.ne.idstr(4)  .and.   &
               idtst.ne.idstr(5)  .and. idtst.ne.idstr(6)  .and.   &
               idtst.ne.idstr(10) .and. idtst.ne.'STP' ) goto 2005

          ! Stop conditions
          if ( idtst .eq. 'STP' ) then
            exit
          else
            backspace ( ndsi )
          end if

          call print_logmsg(740+IAPROC, ' 2.6 Store data ', w3_debuginit_flag)
          ! Store data
          do j=lbound(idstr,1), 10
            if ( idtst .eq. idstr(j) ) then
              nh(j)    = nh(j) + 1
              if ( nh(j) .gt. nhmax ) goto 2006
              IF ( J .LE. 1  ) THEN ! water levels, etc. : get HA
                read (ndsi,*) idtst,           &
                     tho(1,j,nh(j)), tho(2,j,nh(j)),            &
                     ha(nh(j),j)
              ELSE IF ( J .EQ. 2 ) THEN ! currents: get HA and HD
                read (ndsi,*) idtst,           &
                     tho(1,j,nh(j)), tho(2,j,nh(j)),            &
                     ha(nh(j),j), hd(nh(j),j)
              ELSE IF ( J .EQ. 3 ) THEN ! wind: get HA HD and HS
                read (ndsi,*) idtst,           &
                     tho(1,j,nh(j)), tho(2,j,nh(j)),            &
                     ha(nh(j),j), hd(nh(j),j), hs(nh(j),j)
              ELSE IF ( J .EQ. 4 ) THEN ! ice
                read (ndsi,*) idtst,           &
                     tho(1,j,nh(j)), tho(2,j,nh(j)),            &
                     ha(nh(j),j)
              ELSE IF ( J .EQ. 5 ) THEN ! atmospheric momentum
                read (ndsi,*) idtst,           &
                     tho(1,j,nh(j)), tho(2,j,nh(j)),            &
                     ha(nh(j),j), hd(nh(j),j)
              ELSE IF ( J .EQ. 6 ) THEN ! air density
                read (ndsi,*) idtst,           &
                     tho(1,j,nh(j)), tho(2,j,nh(j)),            &
                     ha(nh(j),j)
              ELSE IF ( J .EQ. 10 ) THEN ! mov: HA and HD
                read (ndsi,*) idtst,           &
                     tho(1,j,nh(j)), tho(2,j,nh(j)),            &
                     ha(nh(j),j), hd(nh(j),j)
              END IF
            end if
          end do
        end do
        call print_memcheck(memunit, 'memcheck_____:'//' read_shel_config SECTION 3')

        if (w3_o7_flag) then
          do j=jfirst, 10
            if ( flh(j) .and. iaproc.eq.napout ) then
              write (ndso,952) nh(j), idflds(j)
              do i=1, nh(j)
                if ( ( j .le. 1 ) .or. ( j .eq. 4 ) .or. ( j .eq. 6 ) ) then
                  write (ndso,953) i, tho(1,j,i), tho(2,j,i), ha(i,j)
                else if ( ( j .eq. 2 ) .or. ( j .eq. 5 ) .or. ( j .eq. 10 ) ) then
                  write (ndso,953) i, tho(1,j,i), tho(2,j,i), ha(i,j), hd(i,j)
                else if ( j .eq. 3 ) then
                  write (ndso,953) i, tho(1,j,i), tho(2,j,i), ha(i,j), hd(i,j), hs(i,j)
                end if
              end do
            end if
          end do
        end if

        if ( ( flh(-7) .and. (nh(-7).eq.0) ) .or. &
             ( flh(-6) .and. (nh(-6).eq.0) ) .or. &
             ( flh(-5) .and. (nh(-5).eq.0) ) .or. &
             ( flh(-4) .and. (nh(-4).eq.0) ) .or. &
             ( flh(-3) .and. (nh(-3).eq.0) ) .or. &
             ( flh(-2) .and. (nh(-2).eq.0) ) .or. &
             ( flh(-1) .and. (nh(-1).eq.0) ) .or. &
             ( flh(0)  .and. (nh(0).eq.0)  ) .or. &
             ( flh(1)  .and. (nh(1).eq.0)  ) .or. &
             ( flh(2)  .and. (nh(2).eq.0)  ) .or. &
             ( flh(3)  .and. (nh(3).eq.0)  ) .or. &
             ( flh(4)  .and. (nh(4).eq.0)  ) .or. &
             ( flh(5)  .and. (nh(5).eq.0)  ) .or. &
             ( flh(6)  .and. (nh(6).eq.0)  ) .or. &
             ( flh(10) .and. (nh(10).eq.0) ) ) goto 2007

      end if ! flhom
      close(ndsi)
    end if  ! .not. flgnml

    call print_memcheck(memunit, 'memcheck_____:'//' read_shel_config SECTION 4')

    !--------------------
    ! 2.2 Time setup
    !--------------------

    if (present(time0_overwrite) .and. present(timen_overwrite)) then
      time0(:) = time0_overwrite(:)
      timen(:) = timen_overwrite(:)
      do j = 1,notype
        if (odat(5*(j-1)+3) .ne. 0 ) then    ! non-zero stride
          odat(5*(j-1)+1) = time0(1)
          odat(5*(j-1)+2) = time0(2)
          odat(5*(j-1)+4) = timen(1)
          odat(5*(j-1)+5) = timen(2)
        end if
      end do
      j=8
      if (odat(5*(j-1)+3) .ne. 0) then       ! non-zero stride
        odat(5*(j-1)+1) = time0(1)
        odat(5*(j-1)+2) = time0(2)
        odat(5*(j-1)+4) = timen(1)
        odat(5*(j-1)+5) = timen(2)
      end if
    end if

    if ( iaproc .eq. napout ) write (ndso,930)
    call stme21 ( time0 , dtme21 )
    if ( iaproc .eq. napout ) write (ndso,931) dtme21
    time = time0
    call stme21 ( timen , dtme21 )
    if ( iaproc .eq. napout ) write (ndso,932) dtme21
#ifdef W3_OASIS
    time00 = time0
    timeend = timen
#endif
#ifdef W3_NL5
    qi5tbeg = time0
#endif

    dttst  = dsec21 ( time0 , timen )
    if ( dttst .le. 0. ) goto 2003

    !--------------------
    ! 2.3 Domain setup
    !--------------------

    iostyp = max ( 0 , min ( 3 , iostyp ) )
    if (w3_pdlib_flag) then
      if (iostyp .gt. 1) then
        write(*,*) 'iostyp not supported in domain decomposition mode'
        call extcde ( 6666 )
      end if
    endif

    if ( iaproc .eq. napout ) then
      if ( iostyp .eq. 0 ) then
        write (ndso,940) 'No dedicated output process, parallel file system required.'
      else if ( iostyp .eq. 1 ) then
        write (ndso,940) 'No dedicated output process, any file system.'
      else if ( iostyp .eq. 2 ) then
        write (ndso,940) 'Single dedicated output process.'
      else if ( iostyp .eq. 3 ) then
        write (ndso,940) 'Multiple dedicated output processes.'
      else
        write (ndso,940) 'IOSTYP NOT RECOGNIZED'
      end if
    end if

    ! TODO: the following documents the output dates according to
    ! the nml/inp files. Check if it be removed if user controls
    ! output w/ alarms

    ! 2.4 Output dates

    do j = 1, notype
      if ( odat(5*(j-1)+3) .ne. 0 ) then
        if ( iaproc .eq. napout ) write (ndso,941) j, idotyp(j)
        ttime(1) = odat(5*(j-1)+1)
        ttime(2) = odat(5*(j-1)+2)
        call stme21 ( ttime , dtme21 )
        if ( iaproc .eq. napout ) write (ndso,942) dtme21
        ttime(1) = odat(5*(j-1)+4)
        ttime(2) = odat(5*(j-1)+5)
        call stme21 ( ttime , dtme21 )
        if ( iaproc .eq. napout ) write (ndso,943) dtme21
        ttime(1) = 0
        ttime(2) = 0
        dttst    = real ( odat(5*(j-1)+3) )
        call tick21 ( ttime , dttst  )
        call stme21 ( ttime , dtme21 )
        if ( ( odat(5*(j-1)+1) .ne. odat(5*(j-1)+4) .or. odat(5*(j-1)+2) .ne. odat(5*(j-1)+5) ) &
             .and. iaproc .eq. napout ) then
          if ( dtme21(9:9) .ne. '0' ) then
            write (ndso,1944) dtme21( 9:19)
          else if ( dtme21(10:10) .ne. '0' ) then
            write (ndso,2944) dtme21(10:19)
          else
            write (ndso,3944) dtme21(12:19)
          end if
        end if
      end if
    end do

    ! CHECKPOINT
    j=8
    if (odat(5*(j-1)+3) .ne. 0) then
      if ( iaproc .eq. napout ) write (ndso,941) j, idotyp(j)
      ttime(1) = odat(5*(j-1)+1)
      ttime(2) = odat(5*(j-1)+2)
      call stme21 ( ttime , dtme21 )
      if ( iaproc .eq. napout ) write (ndso,942) dtme21
      ttime(1) = odat(5*(j-1)+4)
      ttime(2) = odat(5*(j-1)+5)
      call stme21 ( ttime , dtme21 )
      if ( iaproc .eq. napout ) write (ndso,943) dtme21
      ttime(1) = 0
      ttime(2) = 0
      dttst    = real ( odat(5*(j-1)+3) )
      call tick21 ( ttime , dttst  )
      call stme21 ( ttime , dtme21 )
      if ( ( odat(5*(j-1)+1) .ne. odat(5*(j-1)+4) .or.        &
           odat(5*(j-1)+2) .ne. odat(5*(j-1)+5) ) .and.       &
           iaproc .eq. napout ) then
        if ( dtme21(9:9) .ne. '0' ) then
          write (ndso,1944) dtme21( 9:19)
        else if ( dtme21(10:10) .ne. '0' ) then
          write (ndso,2944) dtme21(10:19)
        else
          write (ndso,3944) dtme21(12:19)
        end if
      end if
    end if

    ! 2.5 Output types

    if (w3_t_flag) then
      write (ndst,9040) odat
      write (ndst,9041) flgrd
      write (ndst,9042) iprt, prtfrm
    end if
    if (.not. present(time0_overwrite) .and. .not. present(timen_overwrite)) then
      !
      ! For outputs with non-zero time step, check dates :
      ! If output ends before run start OR output starts after run end,
      ! deactivate output cleanly with output time step = 0
      ! This is usefull for IOSTYP=3 (Multiple dedicated output processes)
      ! to avoid the definition of dedicated proc. for unused output.
      !
      do j = 1, notype
        dttst  = dsec21 ( time0 , odat(5*(j-1)+4:5*(j-1)+5) )
        if ( dttst .lt. 0 ) then
          odat(5*(j-1)+3) = 0
          if ( iaproc .eq. napout )  write (ndso,8945) trim(idotyp(j))
          continue
        end if
        dttst  = dsec21 ( odat(5*(j-1)+1:5*(j-1)+2), timen )
        if ( dttst .lt. 0 ) then
          odat(5*(j-1)+3) = 0
          if ( iaproc .eq. napout )  write (ndso,8945) trim(idotyp(j))
          continue
        end if
      end do
    end if

    ! checkpoint
    j = 8
    dttst  = dsec21 ( time0 , odat(5*(j-1)+4:5*(j-1)+5) )
    if ( dttst .lt. 0 ) then
      odat(5*(j-1)+3) = 0
      if ( iaproc .eq. napout )  write (ndso,8945) trim(idotyp(j))
      continue
    end if
    dttst  = dsec21 ( odat(5*(j-1)+1:5*(j-1)+2), timen )
    if ( dttst .lt. 0 ) then
      odat(5*(j-1)+3) = 0
      if ( iaproc .eq. napout )  write (ndso,8945) trim(idotyp(j))
      continue
    end if

    call print_memcheck(memunit, 'memcheck_____:'//' read_shel_config SECTION 5')

    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    if ( iaproc .eq. napout ) write (ndso,951) 'Wave model ...'
    goto 2222

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
900 FORMAT (/15X, '      *** WAVEWATCH III Program shell ***      '/   &
         15X,     '==============================================='/)
901 FORMAT ( '  Comment character is ''',A,''''/)
905 FORMAT ( '  Hybrid MPI/OMP thread support level:'/                 &
             '     Requested: ', I2/                                   &
             '      Provided: ', I2/ )
920 FORMAT (/ '  Input fields : '/                                     &
              ' --------------------------------------------------')
921 FORMAT (  '       ',A,2X,A,2X,A)
922 FORMAT (  ' ' )
930 FORMAT (/ '  Time interval : '/                                    &
              ' --------------------------------------------------')
931 FORMAT (  '       Starting time : ',A)
932 FORMAT (  '       Ending time   : ',A/)
940 FORMAT (/ '  Output requests : '/                                  &
              ' --------------------------------------------------'/   &
              '       ',A)
941 FORMAT (/ '       Type',I2,' : ',A/                                &
              '      -----------------------------------------')
942 FORMAT ( '            From     : ',A)
943 FORMAT ( '            To       : ',A)
954 FORMAT ( '            ',A,': file not needed')
955 FORMAT ( '            ',A,': file OK')
956 FORMAT (  '            ',A,': file OK, recl =',I3,                 &
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
6945 FORMAT ( '            IX first,last,inc :',3I5/                   &
              '            IY first,last,inc :',3I5/                   &
              '            Formatted file    :    ',A)
8945 FORMAT ( '            output dates out of run dates : ', A,       &
              ' deactivated')
950 FORMAT (/ '  Initializations :'/                                   &
              ' --------------------------------------------------')
951 FORMAT ( '       ',A)
952 FORMAT ( '       ',I6,2X,A)
953 FORMAT ( '          ',I6,I11.8,I7.6,3E12.4)
1001 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     PREMATURE END OF INPUT FILE'/)
1002 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     ERROR IN READING FROM INPUT FILE'/                &
               '     IOSTAT =',I5/)
1102 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     LEVEL AND CURRENT ARE MIXING COUPLED AND FORCED'/ &
               '     IT MUST BE FULLY COUPLED OR DISABLED '/)
1003 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     ILLEGAL TIME INTERVAL'/)
1104 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     ERROR IN OPENING POINT FILE'/                     &
               '     IOSTAT =',I5/)
1004 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     ERROR IN READING FROM POINT FILE'/                &
               '     IOSTAT =',I5/)
1005 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     ILLEGAL ID STRING HOMOGENEOUS FIELD : ',A/)
1006 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     TOO MANY HOMOGENEOUS FIELDS : ',A,1X,I4/)
1062 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : ***'/             &
               '     HOMOGENEOUS NAME NOT RECOGNIZED : ', A/)
1007 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     INSUFFICIENT DATA FOR HOMOGENEOUS FIELDS'/)
1008 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     ERROR IN OPENING OUTPUT FILE'/                    &
               '     IOSTAT =',I5/)
1009 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     COUPLING TIME STEP NOT MULTIPLE OF'/              &
               '     MODEL TIME STEP: ',I6, I6/)
1010 FORMAT (/ ' *** WAVEWATCH III WARNING IN W3SHEL : *** '/          &
               '     COUPLING TIME STEP NOT DEFINED, '/                &
               '     IT WILL BE OVERRIDEN TO DEFAULT VALUE'/           &
               '     FROM ',I6, ' TO ',I6/)
1054 FORMAT (/ ' *** WAVEWATCH III ERROR IN W3SHEL : *** '/            &
               '     POINT OUTPUT ACTIVATED BUT NO POINTS DEFINED'/)
9000 FORMAT ( ' TEST W3SHEL : UNIT NUMBERS  :',12I4)
9001 FORMAT ( ' TEST W3SHEL : SUBR. TRACING :',2I4)
9020 FORMAT ( ' TEST W3SHEL : FLAGS DEF / HOM  : ',9L2,2X,9L2)
9040 FORMAT ( ' TEST W3SHEL : ODAT   : ',I9.8,I7.6,I7,I9.8,I7.6, 4(/24X,I9.8,I7.6,I7,I9.8,I7.6) )
9041 FORMAT ( ' TEST W3SHEL : FLGRD  : ',20L2)
9042 FORMAT ( ' TEST W3SHEL : IPR, PRFRM : ',6I6,1X,L1)

  end subroutine read_shel_config

end module wav_shel_inp
