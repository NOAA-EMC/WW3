!> @file wav_restart_mod
!!
!> @brief Handle WW3 restart files as netCDF using PIO
!!
!> @author Denise.Worthen@noaa.gov
!> @date 08-26-2024
module wav_restart_mod

  use w3parall      , only : init_get_isea
  use w3adatmd      , only : nsealm
  use w3gdatmd      , only : nth, nk, nx, ny, mapsf, nspec, nseal, nsea
  use w3odatmd      , only : ndso, iaproc, addrstflds, rstfldlist, rstfldcnt
  use w3wdatmd      , only : ice
  use wav_pio_mod   , only : pio_iotype, pio_ioformat, wav_pio_subsystem
  use wav_pio_mod   , only : handle_err, wav_pio_initdecomp
#ifdef W3_PDLIB
    use yowNodepool , only : ng
#endif
  use pio
  use netcdf

  implicit none

  private

  type(file_desc_t) :: pioid
  type(var_desc_t)  :: varid
  type(io_desc_t)   :: iodesc2dint
  type(io_desc_t)   :: iodesc2d

  integer(kind=Pio_Offset_Kind) :: frame

  public :: write_restart
  public :: read_restart

  ! used/reused in module
  character(len=4)  :: cspec
  character(len=12) :: vname
  integer           :: ik, ith, ix, iy, kk, isea, jsea, ierr, i

  !===============================================================================
contains
  !===============================================================================
  !> Write a WW3 restart file
  !!
  !! @details Called by w3wavemd to write a restart file at a given frequency or
  !! time
  !!
  !! @param[in]     fname    the time-stamped file name
  !! @param[in]     va       the va array
  !! @param[in]     mapsta   the mapsta + 8*mapst2 array
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 08-26-2024
  subroutine write_restart (fname, va, mapsta)

    use w3odatmd , only : time_origin, calendar_name, elapsed_secs

    real            , intent(in) :: va(1:nspec,0:nsealm)
    integer         , intent(in) :: mapsta(ny,nx)
    character(len=*), intent(in) :: fname

    ! local variables
    integer              :: timid, xtid, ytid
    integer              :: nseal_cpl, nmode
    integer              :: dimid(3)
    real   , allocatable :: lva(:,:)
    integer, allocatable :: lmap(:)
    !-------------------------------------------------------------------------------

#ifdef W3_PDLIB
    nseal_cpl = nseal - ng
#else
    nseal_cpl = nseal
#endif
    allocate(lva(1:nseal_cpl,1:nspec))
    allocate(lmap(1:nseal_cpl))
    lva(:,:) = 0.0
    lmap(:) = 0

    ! create the netcdf file
    frame = 1
    pioid%fh = -1
    nmode = pio_clobber
    ! only applies to classic NETCDF files.
    if (pio_iotype == PIO_IOTYPE_NETCDF .or. pio_iotype == PIO_IOTYPE_PNETCDF) then
      nmode = ior(nmode,pio_ioformat)
    endif
    ierr = pio_createfile(wav_pio_subsystem, pioid, pio_iotype, trim(fname), nmode)
    call handle_err(ierr, 'pio_create')
    if (iaproc == 1) write(ndso,'(a)')' Writing restart file '//trim(fname)

    ierr = pio_def_dim(pioid,    'nx',    nx, xtid)
    ierr = pio_def_dim(pioid,    'ny',    ny, ytid)
    ierr = pio_def_dim(pioid,  'time', PIO_UNLIMITED, timid)

    ! define the time variable
    ierr = pio_def_var(pioid, 'time', PIO_DOUBLE, (/timid/), varid)
    call handle_err(ierr,'def_timevar')
    ierr = pio_put_att(pioid, varid, 'units', trim(time_origin))
    call handle_err(ierr,'def_time_units')
    ierr = pio_put_att(pioid, varid, 'calendar', trim(calendar_name))
    call handle_err(ierr,'def_time_calendar')

    ! define the nth,nk sizes
    ierr = pio_def_var(pioid, 'nth', PIO_INT, varid)
    call handle_err(ierr,'def_nth')
    ierr = pio_put_att(pioid, varid, 'long_name', 'number of direction bins')
    ierr = pio_def_var(pioid, 'nk', PIO_INT, varid)
    call handle_err(ierr,'def_nk')
    ierr = pio_put_att(pioid, varid, 'long_name', 'number of frequencies')

    ! write each nspec as separate variable
    do kk = 1,nspec
       write(cspec,'(i4.4)')kk
       vname = 'va'//cspec
       dimid = (/xtid, ytid, timid/)
       ierr = pio_def_var(pioid, trim(vname), PIO_REAL, dimid, varid)
       call handle_err(ierr, 'define variable '//trim(vname))
       ierr = pio_put_att(pioid, varid, '_FillValue', nf90_fill_float)
       call handle_err(ierr, 'define _FillValue '//trim(vname))
     end do

    vname = 'mapsta'
    ierr = pio_def_var(pioid, trim(vname), PIO_INT, (/xtid, ytid, timid/), varid)
    call handle_err(ierr, 'define variable '//trim(vname))
    ierr = pio_put_att(pioid, varid, '_FillValue', nf90_fill_int)
    call handle_err(ierr, 'define _FillValue '//trim(vname))

    ! define any requested additional fields
    if (addrstflds) then
      do i = 1,rstfldcnt
        vname = trim(rstfldlist(i))
        ierr = pio_def_var(pioid, trim(vname), PIO_REAL, (/xtid, ytid, timid/), varid)
        call handle_err(ierr, 'define variable '//trim(vname))
        ierr = pio_put_att(pioid, varid, '_FillValue', nf90_fill_float)
        call handle_err(ierr, 'define _FillValue '//trim(vname))
      end do
    end if
    ! end variable definitions
    ierr = pio_enddef(pioid)
    call handle_err(ierr, 'end variable definition')

    ! write the freq and direction sizes
    ierr = pio_inq_varid(pioid, 'nth', varid)
    call handle_err(ierr, 'inquire variable nth ')
    ierr = pio_put_var(pioid, varid, nth)
    call handle_err(ierr, 'put nth')
    ierr = pio_inq_varid(pioid, 'nk', varid)
    call handle_err(ierr, 'inquire variable nk ')
    ierr = pio_put_var(pioid, varid, nk)
    call handle_err(ierr, 'put nk')

    ! initialize the decomp
    call wav_pio_initdecomp(iodesc2dint, use_int=.true.)
    call wav_pio_initdecomp(iodesc2d)

    ! write the time
    ierr = pio_inq_varid(pioid,  'time', varid)
    call handle_err(ierr, 'inquire variable time ')
    ierr = pio_put_var(pioid, varid, (/1/), real(elapsed_secs,8))
    call handle_err(ierr, 'put time')

    ! mapsta is global
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      lmap(jsea) = mapsta(iy,ix)
    end do

    ! write PE local map
    vname = 'mapsta'
    ierr = pio_inq_varid(pioid,  trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, int(1,kind=Pio_Offset_Kind))
    call pio_write_darray(pioid, varid, iodesc2dint, lmap, ierr)
    call handle_err(ierr, 'put variable '//trim(vname))

    ! write va
    do jsea = 1,nseal_cpl
      kk = 0
      do ik = 1,nk
        do ith = 1,nth
          kk = kk + 1
          lva(jsea,kk) = va(kk,jsea)
        end do
      end do
    end do

    do kk = 1,nspec
      write(cspec,'(i4.4)')kk
      vname = 'va'//cspec
      ierr = pio_inq_varid(pioid,  trim(vname), varid)
      call handle_err(ierr, 'inquire variable '//trim(vname))
      call pio_setframe(pioid, varid, int(1,kind=PIO_OFFSET_KIND))
      call pio_write_darray(pioid, varid, iodesc2d, lva(:,kk), ierr)
      call handle_err(ierr, 'put variable '//trim(vname))
    end do

    ! write requested additional global(nsea) fields
    if (addrstflds) then
      do i = 1,rstfldcnt
        vname = trim(rstfldlist(i))
        if (vname == 'ice')call write_globalfield(vname, nseal_cpl, ice(1:nsea))
      end do
    end if

    call pio_syncfile(pioid)
    call pio_freedecomp(pioid, iodesc2d)
    call pio_freedecomp(pioid, iodesc2dint)
    call pio_closefile(pioid)

  end subroutine write_restart

  !===============================================================================
  !> Read a WW3 restart file
  !!
  !> @details Called by w3init to read a restart file which is known to exist or to
  !! initialize a set of variables when the filename is "none".
  !!
  !! @param[in]     fname     the time-stamped file name
  !! @param[out]    va        the va array, optional
  !! @param[out]    mapsta    the mapsta array, optional
  !! @param[inout]  mapst2    the mapst2 array, optional
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 08-26-2024
  subroutine read_restart (fname, va, mapsta, mapst2)

    use mpi_f08
    use w3adatmd    , only : mpi_comm_wave
    use w3gdatmd    , only : sig
    use w3idatmd    , only : icei
    use w3wdatmd    , only : time, tlev, tice, trho, tic1, tic5, wlv, asf, fpis

    character(len=*)  , intent(in)    :: fname
    real   , optional , intent(out)   :: va(1:nspec,0:nsealm)
    integer, optional , intent(out)   :: mapsta(ny,nx)
    integer, optional , intent(inout) :: mapst2(ny,nx)

    ! local variables
    type(MPI_Comm)       :: wave_communicator  ! needed for mpi_f08
    integer, allocatable :: global_input(:), global_output(:)
    integer              :: nseal_cpl
    integer              :: ifill
    real                 :: rfill
    real   , allocatable :: lva(:,:)
    integer, allocatable :: lmap(:)
    integer, allocatable :: lmap2d(:,:)
    integer, allocatable :: st2init(:,:)
    !-------------------------------------------------------------------------------

    ! cold start, set initial values and return.
    if (trim(fname)  == 'none') then
      tlev(1) = -1
      tlev(2) =  0
      tice(1) = -1
      tice(2) =  0
      trho(1) = -1
      trho(2) =  0
      tic1(1) = -1
      tic1(2) =  0
      tic5(1) = -1
      tic5(2) =  0
      wlv     =  0.
      ice     =  0.
      asf     =  1.
      fpis    =  sig(nk)
      if (iaproc == 1) write(ndso,'(a)')' Initializing WW3 at rest '
      return
    end if

    ! read a netcdf restart
    wave_communicator%mpi_val = MPI_COMM_WAVE
#ifdef W3_PDLIB
    nseal_cpl = nseal - ng
#else
    nseal_cpl = nseal
#endif
    allocate(lva(1:nseal_cpl,1:nspec))
    allocate(lmap(1:nseal_cpl))
    allocate(lmap2d(1:ny,1:nx))
    allocate(st2init(1:ny,1:nx))
    lva(:,:) = 0.0
    lmap(:) = 0
    lmap2d(:,:) = 0

    ! save a copy of initial mapst2 from mod_def
    st2init = mapst2

    ! all times are restart times
    tlev = time
    tice = time
    trho = time
    tic1 = time
    tic5 = time
    frame = 1
    ierr = pio_openfile(wav_pio_subsystem, pioid, pio_iotype, trim(fname), pio_nowrite)
    call handle_err(ierr, 'open file '//trim(fname))
    if (iaproc == 1) write(ndso,'(a)')' Reading restart file '//trim(fname)

    ! check the field dimensions and sizes against the current values
    call checkfile()

    ! initialize the decomp
    call wav_pio_initdecomp(iodesc2dint, use_int=.true.)
    call wav_pio_initdecomp(iodesc2d)

    do kk = 1,nspec
      write(cspec,'(i4.4)')kk
      vname = 'va'//cspec
      ierr = pio_inq_varid(pioid, trim(vname), varid)
      call handle_err(ierr, 'inquire variable '//trim(vname))
      call pio_setframe(pioid, varid, frame)
      ierr = pio_get_att(pioid, varid, "_FillValue", rfill)
      call handle_err(ierr, 'get variable _FillValue'//trim(vname))
      call pio_read_darray(pioid, varid, iodesc2d, lva(:,kk), ierr)
      call handle_err(ierr, 'get variable '//trim(vname))
    end do

    va = 0.0
    do jsea = 1,nseal_cpl
      kk = 0
      do ik = 1,nk
        do ith = 1,nth
          kk = kk + 1
          if (lva(jsea,kk) .ne. rfill) then
            va(kk,jsea) = lva(jsea,kk)
          end if
        end do
      end do
    end do

    vname = 'mapsta'
    ierr = pio_inq_varid(pioid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, frame)
    call pio_read_darray(pioid, varid, iodesc2dint, lmap, ierr)
    call handle_err(ierr, 'get variable '//trim(vname))
    ierr = pio_get_att(pioid, varid, "_FillValue", ifill)
    call handle_err(ierr, 'get variable _FillValue'//trim(vname))

    ! fill global array with PE local values
    allocate(global_input(nsea))
    allocate(global_output(nsea))
    global_input = 0
    global_output = 0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      if (lmap(jsea) .ne. ifill) then
        global_input(isea) = lmap(jsea)
      end if
    end do
    ! reduce across all PEs to create global array
    call MPI_AllReduce(global_input, global_output, nsea, MPI_INTEGER, MPI_SUM, wave_communicator, ierr)

    ! fill global array on each PE
    do isea = 1,nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      lmap2d(iy,ix) = global_output(isea)
    end do
    deallocate(global_input)
    deallocate(global_output)

    mapsta = mod(lmap2d+2,8) - 2
    mapst2 = st2init + (lmap2d-mapsta)/8

    ! read additional global(nsea) restart fields
    if (addrstflds) then
      do i = 1,rstfldcnt
        vname = trim(rstfldlist(i))
        if (vname == 'ice')call read_globalfield(wave_communicator, vname, nseal_cpl, ice(1:nsea), icei)
      end do
    end if

    call pio_syncfile(pioid)
    call pio_freedecomp(pioid, iodesc2d)
    call pio_freedecomp(pioid, iodesc2dint)
    call pio_closefile(pioid)

  end subroutine read_restart

  !===============================================================================
  !>  Write a decomposed array of (nsea) global values
  !!
  !! @param[in]   vname         the variable name
  !! @param[in]   nseal_cpl     the PE local dimension, disregarding halos
  !! @param[in]   global_input  the global array
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 09-22-2024
  subroutine write_globalfield(vname, nseal_cpl, global_input)

    character(len=*) , intent(in)    :: vname
    integer          , intent(in)    :: nseal_cpl
    real             , intent(in)    :: global_input(:)

    ! local variable
    real, allocatable :: lvar(:)

    allocate(lvar(1:nseal_cpl))

    lvar(:) = 0.0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      lvar(jsea) = global_input(isea)
    end do

    !write PE local field
    ierr = pio_inq_varid(pioid,  trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, int(1,kind=Pio_Offset_Kind))
    call pio_write_darray(pioid, varid, iodesc2d, lvar, ierr)
    call handle_err(ierr, 'put variable '//trim(vname))

  end subroutine write_globalfield

  !===============================================================================
  !>  Read a decomposed array of (nsea) global values and return a global field on
  !! each DE
  !!
  !! @param[in]    wave_communicator  the MPI handle
  !! @param[in]    vname              the variable name
  !! @param[in]    nseal_cpl          the PE local dimension, disregarding halos
  !! @param[out]   global_output      the global array, nsea points on each DE
  !! @param[out]   global_2d          the global array, (nx,ny) points on each DE
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 09-22-2024
  subroutine read_globalfield(wave_communicator, vname, nseal_cpl, global_output, global_2d)

    use mpi_f08

    type(MPI_Comm)   , intent(in)    :: wave_communicator  ! needed for mpi_f08
    character(len=*) , intent(in)    :: vname
    integer          , intent(in)    :: nseal_cpl
    real             , intent(out)   :: global_output(:)
    real             , intent(out)   :: global_2d(:,:)

    ! local variables
    real, allocatable :: global_input(:)
    real              :: rfill
    real, allocatable :: lvar(:)

    allocate(lvar(1:nseal_cpl))
    lvar(:) = 0.0

    ierr = pio_inq_varid(pioid, trim(vname), varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    call pio_setframe(pioid, varid, frame)
    call pio_read_darray(pioid, varid, iodesc2d, lvar, ierr)
    call handle_err(ierr, 'get variable '//trim(vname))
    ierr = pio_get_att(pioid, varid, "_FillValue", rfill)
    call handle_err(ierr, 'get variable _FillValue'//trim(vname))

    ! fill global array with PE local values
    allocate(global_input(nsea))
    global_input = 0.0
    global_output = 0.0
    do jsea = 1,nseal_cpl
      call init_get_isea(isea, jsea)
      if (lvar(jsea) .ne. rfill) then
        global_input(isea) = lvar(jsea)
      end if
    end do
    ! reduce across all PEs to create global array
    call MPI_AllReduce(global_input, global_output, nsea, MPI_REAL, MPI_SUM, wave_communicator, ierr)
    deallocate(global_input)

    global_2d = 0.0
    do isea = 1,nsea
      ix = mapsf(isea,1)
      iy = mapsf(isea,2)
      global_2d(ix,iy) = global_output(isea)
    end do

  end subroutine read_globalfield

  !===============================================================================
  !>  Check that a restart file has the expected dimensions and sizes
  !!
  !> author DeniseWorthen@noaa.gov
  !> @date 10-15-2024
  subroutine checkfile()

    use w3odatmd  , only : ndse
    use w3servmd  , only : extcde

    integer :: dimid, ivar
    integer(kind=PIO_OFFSET_KIND) :: dimlen

    ! check dimension nx
    vname = 'nx'
    ierr = pio_inq_dimid(pioid, vname, dimid)
    call handle_err(ierr, 'inquire dimension '//trim(vname))
    ierr = pio_inq_dimlen(pioid, dimid, dimlen)
    if (dimlen /= int(nx,PIO_OFFSET_KIND)) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

    ! check dimension ny
    vname = 'ny'
    ierr = pio_inq_dimid(pioid, vname, dimid)
    call handle_err(ierr, 'inquire dimension '//trim(vname))
    ierr = pio_inq_dimlen(pioid, dimid, dimlen)
    if (dimlen /= int(ny,PIO_OFFSET_KIND)) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

    ! check number of directions
    vname = 'nth'
    ierr = pio_inq_varid(pioid, vname, varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = pio_get_var(pioid, varid, ivar)
    call handle_err(ierr, 'get variable '//trim(vname))
    if (ivar .ne. nth) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

    ! check number of frequencies
    vname = 'nk'
    ierr = pio_inq_varid(pioid, vname, varid)
    call handle_err(ierr, 'inquire variable '//trim(vname))
    ierr = pio_get_var(pioid, varid, ivar)
    call handle_err(ierr, 'get variable '//trim(vname))
    if (ivar .ne. nk) then
      write(ndse,*) '*** WAVEWATCH III restart error: '//trim(vname)//' does not match expected value'
      call extcde ( 49 )
    end if

  end subroutine checkfile

end module wav_restart_mod
