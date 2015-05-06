!===============================================================================
!BOP ===========================================================================
!
! !MODULE: mod_oasis_ioshr -- reads and writes driver files
!
! !DESCRIPTION:
!    Writes attribute vectors to netcdf
!
! !REMARKS:
!
! !REVISION HISTORY:
!
! !INTERFACE: ------------------------------------------------------------------

module mod_oasis_ioshr

#if (PIO_DEFINED)

  ! !USES:

  use mod_oasis_kinds, only: r8 => ip_r8_p, in => ip_intwp_p
  use mod_oasis_kinds, only: cl => ic_long
  use mod_oasis_data
  use mod_oasis_sys
  use mod_oasis_string, only: oasis_string_toupper
  use mod_oasis_mpi
  use mct_mod           ! mct wrappers
  use pio

  implicit none
  private

! !PUBLIC TYPES:

  ! none

! !PUBLIC MEMBER FUNCTIONS:

  public oasis_ioshr_init
  public oasis_ioshr_finalize
  public oasis_ioshr_wopen
  public oasis_ioshr_close
  public oasis_ioshr_redef
  public oasis_ioshr_enddef
  public oasis_ioshr_date2yyyymmdd
  public oasis_ioshr_sec2hms
  public oasis_ioshr_read
  public oasis_ioshr_write
!  public oasis_ioshr_getiosys
  public oasis_ioshr_getiotype
  public oasis_ioshr_getioroot

! !PUBLIC DATA MEMBERS

  ! none

!EOP

  interface oasis_ioshr_read
     module procedure oasis_ioshr_read_av
     module procedure oasis_ioshr_read_int
     module procedure oasis_ioshr_read_int1d
     module procedure oasis_ioshr_read_r8
     module procedure oasis_ioshr_read_r81d
     module procedure oasis_ioshr_read_char
  end interface
  interface oasis_ioshr_write
     module procedure oasis_ioshr_write_av
     module procedure oasis_ioshr_write_int
     module procedure oasis_ioshr_write_int1d
     module procedure oasis_ioshr_write_r8
     module procedure oasis_ioshr_write_r81d
     module procedure oasis_ioshr_write_char
     module procedure oasis_ioshr_write_time
  end interface

!-------------------------------------------------------------------------------
! Local data
!-------------------------------------------------------------------------------

   character(*),parameter :: prefix = "oasis_ioshr_"
   character(CL)          :: wfilename = ''
   real(r8)    ,parameter :: fillvalue = rspval
   character(CL) :: charvar   ! buffer for string read/write

   character(*),parameter :: modName = "(mod_oasis_ioshr) "
   integer(in) ,parameter :: debug = 1 ! internal debug level

   character(*),parameter :: version ='oasis_ioshr_v00'

   type(file_desc_t), save :: pio_file
   type(iosystem_desc_t), save :: pio_iosystem
   integer(IN),save  :: pio_mpicomm
   integer(IN),save  :: pio_iam
   integer(IN),save  :: pio_iotype
   integer(IN),save  :: pio_stride
   integer(IN),save  :: pio_numtasks
   integer(IN),save  :: pio_root

   integer(IN),parameter :: pio_root_default = 0

!=================================================================================
contains
!=================================================================================

!=================================================================================
!BOP =============================================================================
!
! !IROUTINE: oasis_ioshr_init - initialize io for coupler
!
! !DESCRIPTION:
!    Read the pio_inparm namelist and initialize the pio subsystem
!
! !REVISION HISTORY:
!    2009-Sep-30 - B. Kauffman - consolidation, clean up
!    2009-Feb-17 - J. Edwards - initial version
!
! !INTERFACE: --------------------------------------------------------------------

  subroutine oasis_ioshr_init(mpicomm,typename,stride,root,numtasks)
    implicit none
    integer(IN),intent(in) :: mpicomm
    character(len=*),intent(in) :: typename
    integer(IN),intent(in) :: stride
    integer(IN),intent(in) :: numtasks
    integer(IN),intent(in) :: root

    integer :: npes
    character(*),parameter :: subName =   '(oasis_ioshr_init) '
    character(*),parameter :: F00     = "('(oasis_ioshr_init) ',4a)"
    character(*),parameter :: F01     = "('(oasis_ioshr_init) ',a,i6)"

    !--------------------------------------------------------------------------
    ! init pio library
    !--------------------------------------------------------------------------

    pio_mpicomm  = mpicomm
    pio_stride   = stride
    pio_numtasks = numtasks
    pio_root     = root
    call getiotypefromname(typename, pio_iotype, pio_iotype_netcdf)
    call oasis_mpi_commsize(pio_mpicomm,npes)
    call oasis_mpi_commrank(pio_mpicomm,pio_iam)

    call namelist_set(npes, pio_mpicomm, pio_stride, pio_root, pio_numtasks, pio_iotype)

    if(pio_iam==0) then
       write(nulprt,F00) 'pio init parameters for : '
       write(nulprt,F01) '   pio_stride   = ',pio_stride
       write(nulprt,F01) '   pio_root     = ',pio_root
       select case(pio_iotype)
          case (pio_iotype_netcdf)
             write(nulprt,*) '   pio iotype is netcdf'     
          case (pio_iotype_netcdf4p)
             write(nulprt,*) '   pio iotype is netcdf4p'     
          case (pio_iotype_netcdf4c)
             write(nulprt,*) '   pio iotype is netcdf4c'     
          case (pio_iotype_pnetcdf)
             write(nulprt,*) '   pio iotype is pnetcdf'     
       end select
       write(nulprt,F01) '   pio_iotype   = ',pio_iotype
       write(nulprt,F01) '   pio_numtasks = ',pio_numtasks
    end if
    call pio_init(pio_iam, pio_mpicomm, pio_numtasks, 0, pio_stride, &
                  pio_rearr_box, pio_iosystem, base=pio_root)

  end subroutine oasis_ioshr_init

!===============================================================================

  subroutine getiotypefromname(itypename, iotype, defaulttype)
     implicit none
     character(len=*), intent(in) :: itypename
     integer, intent(out) :: iotype
     integer, intent(in) :: defaulttype

     character(len=len(itypename)) :: typename
     character(*),parameter :: subName =   '(oasis_ioshr_getiotypefromname) '

     typename = oasis_string_toUpper(itypename)
     if      ( typename .eq. 'NETCDF' ) then
        iotype = pio_iotype_netcdf
     else if ( typename .eq. 'PNETCDF') then
        iotype = pio_iotype_pnetcdf
     else if ( typename .eq. 'NETCDF4P') then
        iotype = pio_iotype_netcdf4p
     else if ( typename .eq. 'NETCDF4C') then
        iotype = pio_iotype_netcdf4c
     else if ( typename .eq. 'NOTHING') then
        iotype = defaulttype
     else
        write(nulprt,*) 'mod_oasis_ioshr: WARNING Bad io_type argument - using iotype_netcdf'
        iotype=pio_iotype_netcdf
     end if
  end subroutine getiotypefromname

!===============================================================================

  subroutine namelist_set(npes,mycomm, pio_stride, pio_root, pio_numtasks, pio_iotype)
    implicit none
    integer, intent(in) :: npes, mycomm
    integer, intent(inout) :: pio_stride, pio_root, pio_numtasks
    integer, intent(inout) :: pio_iotype
    character(*),parameter :: subName =   '(oasis_ioshr_namelist_set) '


    call oasis_mpi_bcast(pio_iotype  , mycomm)
    call oasis_mpi_bcast(pio_stride  , mycomm)
    call oasis_mpi_bcast(pio_root    , mycomm)
    call oasis_mpi_bcast(pio_numtasks, mycomm)

    !--------------------------------------------------------------------------
    ! check/set/correct io pio parameters
    !--------------------------------------------------------------------------


    if (pio_stride>0.and.pio_numtasks<0) then
       pio_numtasks = npes/pio_stride
    else if(pio_numtasks>0 .and. pio_stride<0) then
       pio_stride = npes/pio_numtasks
    else if(pio_numtasks<0 .and. pio_stride<0) then
       pio_stride = 4
       pio_numtasks = npes/pio_stride
       pio_numtasks = max(1, pio_numtasks)
    end if

    if (pio_root<0) then
       pio_root = pio_root_default
    endif
    pio_root = min(pio_root,npes-1)

    if (pio_root + (pio_stride)*(pio_numtasks-1) >= npes .or. &
         pio_stride<=0 .or. pio_numtasks<=0 .or. pio_root < 0 .or. &
         pio_root > npes-1) then
       if(npes<100) then
          pio_stride = max(1,npes/4)
       else if(npes<1000) then
          pio_stride = max(1,npes/8)
       else
          pio_stride = max(1,npes/16)
       end if
       if(pio_stride>1) then
          pio_numtasks = npes/pio_stride
          pio_root = min(1,npes-1)
       else
          pio_numtasks = npes
          pio_root = 0
       end if
       if(debug>0) then
          write(nulprt,*) subName,'pio_stride, iotasks or root out of bounds - resetting to defaults: ',&
               pio_stride,pio_numtasks, pio_root
       end if
    end if


  end subroutine namelist_set

!===============================================================================
  subroutine oasis_ioshr_finalize
    implicit none
    integer :: ierr
    character(*),parameter :: subName =   '(oasis_ioshr_finalize) '

    call pio_finalize(pio_iosystem, ierr)

  end subroutine oasis_ioshr_finalize

!===============================================================================
!  function oasis_ioshr_getiosys() result(iosystem)
!    implicit none
!    type(iosystem_desc_t), pointer :: iosystem
!    character(*),parameter :: subName =   '(oasis_ioshr_getiosys) '
!
!    iosystem => pio_iosystem
!
!  end function oasis_ioshr_getiosys
!
!===============================================================================
  function oasis_ioshr_getiotype() result(io_type)
    implicit none
    integer :: io_type
    character(*),parameter :: subName =   '(oasis_ioshr_getiotype) '

    io_type = pio_iotype

  end function oasis_ioshr_getiotype
!===============================================================================
  function oasis_ioshr_getioroot() result(io_root)
    implicit none
    integer :: io_root
    character(*),parameter :: subName =   '(oasis_ioshr_getioroot) '

    io_root = pio_root

  end function oasis_ioshr_getioroot


!===============================================================================

subroutine oasis_ioshr_flds_lookup(fldname,longname,stdname,units)
    implicit none
    character(len=*),intent(in)  :: fldname
    character(len=*),intent(out),optional :: longname
    character(len=*),intent(out),optional :: stdname
    character(len=*),intent(out),optional :: units
    character(*),parameter :: subName =   '(oasis_ioshr_flds_lookup) '

    if (present(longname)) then
       longname = 'unknown'
    endif
    if (present(stdname)) then
       stdname  = 'unknown'
    endif
    if (present(units)) then
       units    = 'unknown'
    endif

end subroutine oasis_ioshr_flds_lookup

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_ioshr_wopen - open netcdf file
!
! !DESCRIPTION:
!    open netcdf file
!
! !REVISION HISTORY:
!    2007-Oct-26 - T. Craig - initial version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_ioshr_wopen(filename,clobber,cdf64)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(*),intent(in) :: filename
    logical,optional,intent(in):: clobber
    logical,optional,intent(in):: cdf64

    !EOP

    logical :: exists
    logical :: lclobber
    logical :: lcdf64
    integer :: rcode
    integer :: nmode
    character(CL)  :: lversion
    character(*),parameter :: subName = '(oasis_ioshr_wopen) '
    
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

    lclobber = .false.
    if (present(clobber)) lclobber=clobber

    lcdf64 = .false.
    if (present(cdf64)) lcdf64=cdf64

    if (.not. pio_file_is_open(pio_file)) then
       ! filename not open
       if (pio_iam==0) inquire(file=trim(filename),exist=exists)
       call oasis_mpi_bcast(exists,pio_mpicomm,'oasis_ioshr_wopen exists')
       if (exists) then
          if (lclobber) then
             nmode = pio_clobber
             if (lcdf64) nmode = ior(nmode,PIO_64BIT_OFFSET)
             rcode = pio_createfile(pio_iosystem, pio_file, pio_iotype, trim(filename), nmode)
             if(pio_iam==0) write(nulprt,*) subname,' create file ',trim(filename)
             rcode = pio_put_att(pio_file,pio_global,"file_version",version)
          else

             rcode = pio_openfile(pio_iosystem, pio_file, pio_iotype, trim(filename), pio_write)
             if(pio_iam==0) write(nulprt,*) subname,' open file ',trim(filename)
             call pio_seterrorhandling(pio_file,PIO_BCAST_ERROR)
             rcode = pio_get_att(pio_file,pio_global,"file_version",lversion)
             call pio_seterrorhandling(pio_file,PIO_INTERNAL_ERROR)
             if (trim(lversion) /= trim(version)) then
                rcode = pio_redef(pio_file)
                rcode = pio_put_att(pio_file,pio_global,"file_version",version)
                rcode = pio_enddef(pio_file)
             endif
          endif
       else
          nmode = pio_noclobber
          if (lcdf64) nmode = ior(nmode,PIO_64BIT_OFFSET)
          rcode = pio_createfile(pio_iosystem, pio_file, pio_iotype, trim(filename), nmode)
          if(pio_iam==0) write(nulprt,*) subname,' create file ',trim(filename)
          rcode = pio_put_att(pio_file,pio_global,"file_version",version)
       endif
    elseif (trim(wfilename) /= trim(filename)) then
       ! filename is open, better match open filename
       if(pio_iam==0) write(nulprt,*) subname,' different file currently open ',trim(filename)
       call oasis_abort_noarg()
    else
       ! filename is already open, just return
    endif

end subroutine oasis_ioshr_wopen

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_ioshr_close - close netcdf file
!
! !DESCRIPTION:
!    close netcdf file
!
! !REVISION HISTORY:
!    2007-Oct-26 - T. Craig - initial version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_ioshr_close(filename)

    implicit none

    ! !INPUT/OUTPUT PARAMETERS:
    character(*),intent(in) :: filename

    !EOP

    integer :: rcode
    character(*),parameter :: subName = '(oasis_ioshr_close) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

    if (.not. pio_file_is_open(pio_file)) then
       ! filename not open, just return
    elseif (trim(wfilename) /= trim(filename)) then
       ! filename matches, close it
       call pio_closefile(pio_file)
    else
       ! different filename is open, abort
       if(pio_iam==0) write(nulprt,*) subname,' different file currently open ',trim(filename)
       call oasis_abortnoarg()
    endif

    wfilename = ''

end subroutine oasis_ioshr_close

!===============================================================================

subroutine oasis_ioshr_redef(filename)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: rcode
    character(*),parameter :: subName =   '(oasis_ioshr_redef) '

    rcode = pio_redef(pio_file)
end subroutine oasis_ioshr_redef

!===============================================================================

subroutine oasis_ioshr_enddef(filename)
    implicit none
    character(len=*), intent(in) :: filename
    integer :: rcode
    character(*),parameter :: subName =   '(oasis_ioshr_enddef) '

    rcode = pio_enddef(pio_file)
end subroutine oasis_ioshr_enddef

!===============================================================================

character(len=10) function oasis_ioshr_date2yyyymmdd (date)
   implicit none

! Input arguments

   integer, intent(in) :: date

! Local workspace

   integer :: year    ! year of yyyy-mm-dd
   integer :: month   ! month of yyyy-mm-dd
   integer :: day     ! day of yyyy-mm-dd
   character(*),parameter :: subName =   '(oasis_ioshr_date2yyyymmdd) '

!-------------------------------------------------------------------------------

   if (date < 0) then
       WRITE (nulprt,*)  subname,' abort by model ',compid
       WRITE(nulprt,*) subname,' ERROR: oasis_ioshr_date2yyyymmdd: negative date not allowed'
       CALL oasis_abort_noarg()
   end if

   year  = date / 10000
   month = (date - year*10000) / 100
   day   = date - year*10000 - month*100

   write(oasis_ioshr_date2yyyymmdd,80) year, month, day
80 format(i4.4,'-',i2.2,'-',i2.2)

end function oasis_ioshr_date2yyyymmdd

!===============================================================================

character(len=8) function oasis_ioshr_sec2hms (seconds)
   implicit none

! Input arguments

   integer, intent(in) :: seconds

! Local workspace

   integer :: hours     ! hours of hh:mm:ss
   integer :: minutes   ! minutes of hh:mm:ss
   integer :: secs      ! seconds of hh:mm:ss
   character(*),parameter :: subName =   '(oasis_ioshr_sec2hms) '

!-------------------------------------------------------------------------------

   if (seconds < 0 .or. seconds > 86400) then
       WRITE (nulprt,*)  subname,' abort by model ',compid
       WRITE(nulprt,*) subname,'oasis_ioshr_sec2hms: bad input seconds:', seconds
       CALL oasis_abort_noarg()
   end if

   hours   = seconds / 3600
   minutes = (seconds - hours*3600) / 60
   secs    = (seconds - hours*3600 - minutes*60)

   if (minutes < 0 .or. minutes > 60) then
       WRITE (nulprt,*)  subname,' abort by model ',compid
       WRITE(nulprt,*) subname,'oasis_ioshr_sec2hms: bad minutes = ',minutes
       CALL oasis_abort_noarg()
   end if

   if (secs < 0 .or. secs > 60) then
       WRITE (nulprt,*)  subname,' abort by model ',compid
       WRITE(nulprt,*) subname,'oasis_ioshr_sec2hms: bad secs = ',secs
       CALL oasis_abort_noarg()
   end if

   write(oasis_ioshr_sec2hms,80) hours, minutes, secs
80 format(i2.2,':',i2.2,':',i2.2)

end function oasis_ioshr_sec2hms

!===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_ioshr_write_av - write AV to netcdf file
!
! !DESCRIPTION:
!    Write AV to netcdf file
!
! !REVISION HISTORY:
!    2007-Oct-26 - T. Craig - initial version
!
! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_write_av(filename,gsmap,AV,dname,whead,wdata,nx,ny,nt,fillval,pre,tavg,use_float)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    type(mct_gsMap), intent(in) :: gsmap
    type(mct_aVect) ,intent(in) :: AV       ! data to be written
    character(len=*),intent(in) :: dname    ! name of data
    logical,optional,intent(in) :: whead    ! write header
    logical,optional,intent(in) :: wdata    ! write data
    integer(in),optional,intent(in) :: nx   ! 2d grid size if available
    integer(in),optional,intent(in) :: ny   ! 2d grid size if available
    integer(in),optional,intent(in) :: nt   ! time sample
    real(r8),optional,intent(in) :: fillval ! fill value
    character(len=*),optional,intent(in) :: pre      ! prefix to variable name
    logical,optional,intent(in) :: tavg     ! is this a tavg
    logical,optional,intent(in) :: use_float ! write output as float rather than double

    !EOP

    integer(in) :: rcode
    integer(in) :: nf,ns,ng
    integer(in) :: i,j,k,n
    integer(in),target  :: dimid2(2)
    integer(in),target  :: dimid3(3)
    integer(in),pointer :: dimid(:)
    type(var_desc_t) :: varid
    type(io_desc_t) :: iodesc
    integer(kind=PIO_OffSet) :: frame
    type(mct_string) :: mstring     ! mct char type
    character(CL)    :: itemc       ! string converted to char
    character(CL)    :: name1       ! var name
    character(CL)    :: cunit       ! var units
    character(CL)    :: lname       ! long name
    character(CL)    :: sname       ! standard name
    character(CL)    :: lpre        ! local prefix
    logical :: exists
    logical :: lwhead, lwdata
    integer(in) :: lnx,lny
    real(r8) :: lfillvalue
    type(mct_aVect) :: AVroot
    real(r8),pointer :: fld1(:,:)  ! needed to convert AVroot ng rAttr to 2d nx,ny
    character(*),parameter :: subName = '(oasis_ioshr_write_av) '
    integer :: lbnum
    integer, pointer :: Dof(:)

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    lfillvalue = fillvalue
    if (present(fillval)) then
       lfillvalue = fillval
    endif

    lpre = trim(dname)
    if (present(pre)) then
       lpre = trim(pre)
    endif

    lwhead = .true.
    lwdata = .true.
    if (present(whead)) lwhead = whead
    if (present(wdata)) lwdata = wdata

    if (.not.lwhead .and. .not.lwdata) then
       ! should we write a warning?
       return
    endif

    ng = mct_gsmap_gsize(gsmap)
    lnx = ng
    lny = 1
	
    nf = mct_aVect_nRattr(AV)
    if (nf < 1) then
       WRITE (nulprt,*)  subname,' abort by model ',compid
       write(nulprt,*) subname,' ERROR: nf = ',nf,trim(dname)
       call oasis_abort_noarg()
    endif

    if (present(nx)) then
       if (nx /= 0) lnx = nx
    endif
    if (present(ny)) then
       if (ny /= 0) lny = ny
    endif
    if (lnx*lny /= ng) then
       if(pio_iam==0) WRITE (nulprt,*)  subname,' abort by model ',compid
       if(pio_iam==0) write(nulprt,*) subname,' ERROR: grid2d size not consistent ',ng,lnx,lny,trim(dname)
       call oasis_abort_noarg()
    endif

    if (lwhead) then
       rcode = pio_def_dim(pio_file,trim(lpre)//'_nx',lnx,dimid2(1))
       rcode = pio_def_dim(pio_file,trim(lpre)//'_ny',lny,dimid2(2))

       if (present(nt)) then
          dimid3(1:2) = dimid2
          rcode = pio_inq_dimid(pio_file,'time',dimid3(3))
          dimid => dimid3
       else
          dimid => dimid2
       endif

       do k = 1,nf
          call mct_aVect_getRList(mstring,k,AV)
          itemc = mct_string_toChar(mstring)
          call mct_string_clean(mstring)
! "v0"    name1 = trim(prefix)//trim(dname)//'_'//trim(itemc)
          name1 = trim(lpre)//'_'//trim(itemc)
          call oasis_ioshr_flds_lookup(itemc,longname=lname,stdname=sname,units=cunit)
	  if (present(use_float)) then 
             rcode = pio_def_var(pio_file,trim(name1),PIO_REAL,dimid,varid)
          else
             rcode = pio_def_var(pio_file,trim(name1),PIO_DOUBLE,dimid,varid)
          end if
          rcode = pio_put_att(pio_file,varid,"_FillValue",lfillvalue)
          rcode = pio_put_att(pio_file,varid,"units",trim(cunit))
          rcode = pio_put_att(pio_file,varid,"long_name",trim(lname))
          rcode = pio_put_att(pio_file,varid,"standard_name",trim(sname))
          rcode = pio_put_att(pio_file,varid,"internal_dname",trim(dname))
          if (present(tavg)) then
             if (tavg) then
                rcode = pio_put_att(pio_file,varid,"cell_methods","time: mean")
             endif
          endif
       enddo
       if (lwdata) call oasis_ioshr_enddef(filename)
    end if

    if (lwdata) then
       call mct_gsmap_OrderedPoints(gsmap, pio_iam, Dof)
       call pio_initdecomp(pio_iosystem, pio_double, (/lnx,lny/), dof, iodesc)
       deallocate(dof)

       do k = 1,nf
          call mct_aVect_getRList(mstring,k,AV)
          itemc = mct_string_toChar(mstring)
          call mct_string_clean(mstring)
! "v0"    name1 = trim(prefix)//trim(dname)//'_'//trim(itemc)
          name1 = trim(lpre)//'_'//trim(itemc)
          rcode = pio_inq_varid(pio_file,trim(name1),varid)
          if (present(nt)) then
             frame = nt
          else
             frame = 1
          endif
          call pio_setframe(varid,frame)
          call pio_write_darray(pio_file, varid, iodesc, av%rattr(k,:), rcode, fillval=lfillvalue)
       enddo

       call pio_freedecomp(pio_file, iodesc)

    end if
  end subroutine oasis_ioshr_write_av

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_write_int - write scalar integer to netcdf file
  !
  ! !DESCRIPTION:
  !    Write scalar integer to netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_write_int(filename,idata,dname,whead,wdata)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    integer(in)     ,intent(in) :: idata    ! data to be written
    character(len=*),intent(in) :: dname    ! name of data
    logical,optional,intent(in) :: whead    ! write header
    logical,optional,intent(in) :: wdata    ! write data

    !EOP

    integer(in) :: rcode
    type(var_desc_t) :: varid
    character(CL)    :: cunit       ! var units
    character(CL)    :: lname       ! long name
    character(CL)    :: sname       ! standard name
    logical :: exists
    logical :: lwhead, lwdata
    character(*),parameter :: subName = '(oasis_ioshr_write_int) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    lwhead = .true.
    lwdata = .true.
    if (present(whead)) lwhead = whead
    if (present(wdata)) lwdata = wdata

    if (.not.lwhead .and. .not.lwdata) then
       ! should we write a warning?
       return
    endif

    if (lwhead) then
       call oasis_ioshr_flds_lookup(trim(dname),longname=lname,stdname=sname,units=cunit)
!       rcode = pio_def_dim(pio_file,trim(dname)//'_nx',1,dimid(1))
!       rcode = pio_def_var(pio_file,trim(dname),PIO_INT,dimid,varid)
       rcode = pio_def_var(pio_file,trim(dname),PIO_INT,varid)
       rcode = pio_put_att(pio_file,varid,"units",trim(cunit))
       rcode = pio_put_att(pio_file,varid,"long_name",trim(lname))
       rcode = pio_put_att(pio_file,varid,"standard_name",trim(sname))
       if (lwdata) call oasis_ioshr_enddef(filename)
    endif

    if (lwdata) then
       rcode = pio_inq_varid(pio_file,trim(dname),varid)
       rcode = pio_put_var(pio_file,varid,idata)

       !      write(nulprt,*) subname,' wrote AV ',trim(dname),lwhead,lwdata
    endif

  end subroutine oasis_ioshr_write_int

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_write_int1d - write 1d integer array to netcdf file
  !
  ! !DESCRIPTION:
  !    Write 1d integer array to netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_write_int1d(filename,idata,dname,whead,wdata)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    integer(in)     ,intent(in) :: idata(:) ! data to be written
    character(len=*),intent(in) :: dname    ! name of data
    logical,optional,intent(in) :: whead    ! write header
    logical,optional,intent(in) :: wdata    ! write data

    !EOP

    integer(in) :: rcode
    integer(in) :: dimid(1)
    type(var_desc_t) :: varid
    character(CL)    :: cunit       ! var units
    character(CL)    :: lname       ! long name
    character(CL)    :: sname       ! standard name
    integer(in) :: lnx
    logical :: exists
    logical :: lwhead, lwdata
    character(*),parameter :: subName = '(oasis_ioshr_write_int1d) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    lwhead = .true.
    lwdata = .true.
    if (present(whead)) lwhead = whead
    if (present(wdata)) lwdata = wdata

    if (.not.lwhead .and. .not.lwdata) then
       ! should we write a warning?
       return
    endif

    if (lwhead) then
       call oasis_ioshr_flds_lookup(trim(dname),longname=lname,stdname=sname,units=cunit)
       lnx = size(idata)
       rcode = pio_def_dim(pio_file,trim(dname)//'_nx',lnx,dimid(1))
       rcode = pio_def_var(pio_file,trim(dname),PIO_INT,dimid,varid)
       rcode = pio_put_att(pio_file,varid,"units",trim(cunit))
       rcode = pio_put_att(pio_file,varid,"long_name",trim(lname))
       rcode = pio_put_att(pio_file,varid,"standard_name",trim(sname))
       if (lwdata) call oasis_ioshr_enddef(filename)
    endif

    if (lwdata) then
       rcode = pio_inq_varid(pio_file,trim(dname),varid)
       rcode = pio_put_var(pio_file,varid,idata)
    endif

       !      write(nulprt,*) subname,' wrote AV ',trim(dname),lwhead,lwdata

  end subroutine oasis_ioshr_write_int1d

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_write_r8 - write scalar double to netcdf file
  !
  ! !DESCRIPTION:
  !    Write scalar double to netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_write_r8(filename,rdata,dname,whead,wdata)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    real(r8)        ,intent(in) :: rdata    ! data to be written
    character(len=*),intent(in) :: dname    ! name of data
    logical,optional,intent(in) :: whead    ! write header
    logical,optional,intent(in) :: wdata    ! write data

    !EOP

    integer(in) :: rcode
    type(var_desc_t) :: varid
    character(CL)    :: cunit       ! var units
    character(CL)    :: lname       ! long name
    character(CL)    :: sname       ! standard name
    logical :: exists
    logical :: lwhead, lwdata
    character(*),parameter :: subName = '(oasis_ioshr_write_r8) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    lwhead = .true.
    lwdata = .true.
    if (present(whead)) lwhead = whead
    if (present(wdata)) lwdata = wdata

    if (.not.lwhead .and. .not.lwdata) then
       ! should we write a warning?
       return
    endif

    if (lwhead) then
       call oasis_ioshr_flds_lookup(trim(dname),longname=lname,stdname=sname,units=cunit)
!       rcode = pio_def_dim(pio_file,trim(dname)//'_nx',1,dimid(1))
!       rcode = pio_def_var(pio_file,trim(dname),PIO_DOUBLE,dimid,varid)


       rcode = pio_def_var(pio_file,trim(dname),PIO_DOUBLE,varid)
       if(rcode==PIO_NOERR) then
          rcode = pio_put_att(pio_file,varid,"units",trim(cunit))
          rcode = pio_put_att(pio_file,varid,"long_name",trim(lname))
          rcode = pio_put_att(pio_file,varid,"standard_name",trim(sname))
          if (lwdata) call oasis_ioshr_enddef(filename)
       end if
    endif

    if (lwdata) then
       rcode = pio_inq_varid(pio_file,trim(dname),varid)
       rcode = pio_put_var(pio_file,varid,rdata)
    endif


  end subroutine oasis_ioshr_write_r8

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_write_r81d - write 1d double array to netcdf file
  !
  ! !DESCRIPTION:
  !    Write 1d double array to netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_write_r81d(filename,rdata,dname,whead,wdata)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    real(r8)        ,intent(in) :: rdata(:) ! data to be written
    character(len=*),intent(in) :: dname    ! name of data
    logical,optional,intent(in) :: whead    ! write header
    logical,optional,intent(in) :: wdata    ! write data

    !EOP

    integer(in) :: rcode
    integer(in) :: dimid(1)
    type(var_desc_t) :: varid
    character(CL)    :: cunit       ! var units
    character(CL)    :: lname       ! long name
    character(CL)    :: sname       ! standard name
    integer(in) :: lnx
    logical :: exists
    logical :: lwhead, lwdata
    character(*),parameter :: subName = '(oasis_ioshr_write_r81d) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    lwhead = .true.
    lwdata = .true.
    if (present(whead)) lwhead = whead
    if (present(wdata)) lwdata = wdata

    if (.not.lwhead .and. .not.lwdata) then
       ! should we write a warning?
       return
    endif

    if (lwhead) then
       call oasis_ioshr_flds_lookup(trim(dname),longname=lname,stdname=sname,units=cunit)
       lnx = size(rdata)
       rcode = pio_def_dim(pio_file,trim(dname)//'_nx',lnx,dimid(1))
       rcode = pio_def_var(pio_file,trim(dname),PIO_DOUBLE,dimid,varid)
       rcode = pio_put_att(pio_file,varid,"units",trim(cunit))
       rcode = pio_put_att(pio_file,varid,"long_name",trim(lname))
       rcode = pio_put_att(pio_file,varid,"standard_name",trim(sname))
       if (lwdata) call oasis_ioshr_enddef(filename)
    endif

    if (lwdata) then
       rcode = pio_inq_varid(pio_file,trim(dname),varid)
       rcode = pio_put_var(pio_file,varid,rdata)

       !      write(nulprt,*) subname,' wrote AV ',trim(dname),lwhead,lwdata
    endif

  end subroutine oasis_ioshr_write_r81d

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_write_char - write char string to netcdf file
  !
  ! !DESCRIPTION:
  !    Write char string to netcdf file
  !
  ! !REVISION HISTORY:
  !    2010-July-06 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_write_char(filename,rdata,dname,whead,wdata)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    character(len=*),intent(in) :: rdata    ! data to be written
    character(len=*),intent(in) :: dname    ! name of data
    logical,optional,intent(in) :: whead    ! write header
    logical,optional,intent(in) :: wdata    ! write data

    !EOP

    integer(in) :: rcode
    integer(in) :: dimid(1)
    type(var_desc_t) :: varid
    character(CL)    :: cunit       ! var units
    character(CL)    :: lname       ! long name
    character(CL)    :: sname       ! standard name
    integer(in) :: lnx
    logical :: exists
    logical :: lwhead, lwdata
    character(*),parameter :: subName = '(oasis_ioshr_write_char) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    lwhead = .true.
    lwdata = .true.
    if (present(whead)) lwhead = whead
    if (present(wdata)) lwdata = wdata

    if (.not.lwhead .and. .not.lwdata) then
       ! should we write a warning?
       return
    endif

    if (lwhead) then
       call oasis_ioshr_flds_lookup(trim(dname),longname=lname,stdname=sname,units=cunit)
       lnx = len(charvar)
       rcode = pio_def_dim(pio_file,trim(dname)//'_len',lnx,dimid(1))
       rcode = pio_def_var(pio_file,trim(dname),PIO_CHAR,dimid,varid)
       rcode = pio_put_att(pio_file,varid,"units",trim(cunit))
       rcode = pio_put_att(pio_file,varid,"long_name",trim(lname))
       rcode = pio_put_att(pio_file,varid,"standard_name",trim(sname))
       if (lwdata) call oasis_ioshr_enddef(filename)
    endif

    if (lwdata) then
       charvar = ''
       charvar = trim(rdata)
       rcode = pio_inq_varid(pio_file,trim(dname),varid)
       rcode = pio_put_var(pio_file,varid,charvar)
    endif

  end subroutine oasis_ioshr_write_char

  !===============================================================================
!BOP ===========================================================================
!
! !IROUTINE: oasis_ioshr_write_time - write time variable to netcdf file
!
! !DESCRIPTION:
!    Write time variable to netcdf file
!
! !REVISION HISTORY:
!    2009-Feb-11 - M. Vertenstein - initial version
!
! !INTERFACE: ------------------------------------------------------------------

subroutine oasis_ioshr_write_time(filename,time_units,time_cal,time_val,nt,whead,wdata,tbnds)

! !INPUT/OUTPUT PARAMETERS:
   implicit none
   character(len=*),intent(in) :: filename      ! file
   character(len=*),intent(in) :: time_units    ! units of time
   character(len=*),intent(in) :: time_cal      ! calendar type
   real(r8)        ,intent(in) :: time_val      ! data to be written
   integer(in),optional,intent(in) :: nt
   logical,optional,intent(in) :: whead         ! write header
   logical,optional,intent(in) :: wdata         ! write data
   real(r8),optional,intent(in) :: tbnds(2)     ! time bounds

!EOP

   integer(in) :: rcode
   integer(in) :: dimid(1)
   integer(in) :: dimid2(2)
   type(var_desc_t) :: varid
   integer(in) :: lnx
   logical :: exists
   logical :: lwhead, lwdata
   integer :: start(4),count(4)
   character(len=CL) :: lcalendar
   real(r8) :: time_val_1d(1)
   character(*),parameter :: subName = '(oasis_ioshr_write_time) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   lwhead = .true.
   lwdata = .true.
   if (present(whead)) lwhead = whead
   if (present(wdata)) lwdata = wdata

   if (.not.lwhead .and. .not.lwdata) then
      ! should we write a warning?
      return
   endif

   if (lwhead) then
      rcode = pio_def_dim(pio_file,'time',PIO_UNLIMITED,dimid(1))
      rcode = pio_def_var(pio_file,'time',PIO_DOUBLE,dimid,varid)
      rcode = pio_put_att(pio_file,varid,'units',trim(time_units))
      lcalendar = 'noleap'
      rcode = pio_put_att(pio_file,varid,'calendar',trim(lcalendar))
      if (present(tbnds)) then
         rcode = pio_put_att(pio_file,varid,'bounds','time_bnds')
         dimid2(2)=dimid(1)
         rcode = pio_def_dim(pio_file,'ntb',2,dimid2(1))
         rcode = pio_def_var(pio_file,'time_bnds',PIO_DOUBLE,dimid2,varid)
      endif
      if (lwdata) call oasis_ioshr_enddef(filename)
   endif

   if (lwdata) then
      start = 1
      count = 1
      if (present(nt)) then
         start(1) = nt
      endif
      time_val_1d(1) = time_val
      rcode = pio_inq_varid(pio_file,'time',varid)
      rcode = pio_put_var(pio_file,varid,start,count,time_val_1d)
      if (present(tbnds)) then
         rcode = pio_inq_varid(pio_file,'time_bnds',varid)
         start = 1
         count = 1
         if (present(nt)) then
            start(2) = nt
         endif
         count(1) = 2
         rcode = pio_put_var(pio_file,varid,start,count,tbnds)
      endif

      !      write(nulprt,*) subname,' wrote time ',lwhead,lwdata
   endif

 end subroutine oasis_ioshr_write_time

!===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_read_av - read AV from netcdf file
  !
  ! !DESCRIPTION:
  !    Read AV from netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_read_av(filename,gsmap,AV,dname,pre)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    type(mct_gsMap), intent(in) :: gsmap
    type(mct_aVect) ,intent(inout):: AV     ! data to be written
    character(len=*),intent(in) :: dname    ! name of data
    character(len=*),intent(in),optional :: pre      ! prefix name

    !EOP

    integer(in) :: rcode
    integer(in) :: nf,ns,ng
    integer(in) :: i,j,k,n, ndims
    type(file_desc_t) :: pioid
    integer(in) :: dimid(2)
    type(var_desc_t) :: varid
    integer(in) :: lnx,lny
    type(mct_string) :: mstring     ! mct char type
    character(CL)    :: itemc       ! string converted to char
    logical :: exists
    type(io_desc_t) :: iodesc
    integer(in), pointer :: dof(:)
    character(CL)  :: lversion
    character(CL)  :: name1
    character(CL)  :: lpre
    character(*),parameter :: subName = '(oasis_ioshr_read_av) '
    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    lpre = trim(dname)
    if (present(pre)) then
       lpre = trim(pre)
    endif

    call mct_gsmap_OrderedPoints(gsmap, pio_iam, Dof)

    ns = mct_aVect_lsize(AV)
    nf = mct_aVect_nRattr(AV)

    if (pio_iam==0) inquire(file=trim(filename),exist=exists)
    call oasis_mpi_bcast(exists,pio_mpicomm,'oasis_ioshr_read_av exists')
    if (exists) then
       rcode = pio_openfile(pio_iosystem, pioid, pio_iotype, trim(filename),pio_nowrite)
       if(pio_iam==0) write(nulprt,*) subname,' open file ',trim(filename)
       call pio_seterrorhandling(pioid,PIO_BCAST_ERROR)
       rcode = pio_get_att(pioid,pio_global,"file_version",lversion)
       call pio_seterrorhandling(pioid,PIO_INTERNAL_ERROR)
    else
       if(pio_iam==0) WRITE (nulprt,*)  subname,' abort by model ',compid
       if(pio_iam==0) write(nulprt,*) subname,' ERROR: file invalid ',trim(filename),' ',trim(dname)
       call oasis_abort_noarg()
    endif

    do k = 1,nf
       call mct_aVect_getRList(mstring,k,AV)
       itemc = mct_string_toChar(mstring)
       call mct_string_clean(mstring)
       name1 = trim(lpre)//'_'//trim(itemc)
       call pio_seterrorhandling(pioid, PIO_BCAST_ERROR)
       rcode = pio_inq_varid(pioid,trim(name1),varid)
       if (rcode == pio_noerr) then
          if (k==1) then
             rcode = pio_inq_varndims(pioid, varid, ndims)
             rcode = pio_inq_vardimid(pioid, varid, dimid(1:ndims))
             rcode = pio_inq_dimlen(pioid, dimid(1), lnx)
             if (ndims==2) then
                rcode = pio_inq_dimlen(pioid, dimid(2), lny)
             else
                lny = 1
             end if
             ng = lnx * lny
             if (ng /= mct_gsmap_gsize(gsmap)) then
                 IF (pio_iam==0) WRITE (nulprt,*)  subname,' abort by model ',compid
                 IF (pio_iam==0) WRITE(nulprt,*) subname,' ERROR: dimensions do not match',&
                     lnx,lny,mct_gsmap_gsize(gsmap)
                 CALL oasis_abort_noarg()
             end if
             call pio_initdecomp(pio_iosystem, pio_double, (/lnx,lny/), dof, iodesc)
             deallocate(dof)
          end if
          call pio_read_darray(pioid,varid,iodesc, av%rattr(k,:), rcode)
       else
          write(nulprt,*)'oasis_ioshr_readav warning: field ',trim(itemc),' is not on restart file'
          write(nulprt,*)'for backwards compatibility will set it to 0'
          av%rattr(k,:) = 0.0_r8
       end if
       call pio_seterrorhandling(pioid,PIO_INTERNAL_ERROR)
          
       !--- zero out fill value, this is somewhat arbitrary
       do n = 1,ns
          if (AV%rAttr(k,n) == fillvalue) then
              AV%rAttr(k,n) = 0.0_r8
          endif
       enddo
    enddo

    call pio_freedecomp(pioid, iodesc)
    call pio_closefile(pioid)

  end subroutine oasis_ioshr_read_av

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_read_int - read scalar integer from netcdf file
  !
  ! !DESCRIPTION:
  !    Read scalar integer from netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_read_int(filename,idata,dname)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    integer         ,intent(inout):: idata  ! integer data
    character(len=*),intent(in) :: dname    ! name of data

    !EOP

    integer :: i1d(1)
    character(*),parameter :: subName = '(oasis_ioshr_read_int) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    call oasis_ioshr_read_int1d(filename,i1d,dname)
    idata = i1d(1)

  end subroutine oasis_ioshr_read_int

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_read_int1d - read 1d integer from netcdf file
  !
  ! !DESCRIPTION:
  !    Read 1d integer array from netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_read_int1d(filename,idata,dname)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    integer(in)     ,intent(inout):: idata(:)  ! integer data
    character(len=*),intent(in) :: dname    ! name of data

    !EOP

    integer(in) :: rcode
    type(file_desc_t) :: pioid 
    type(var_desc_t) :: varid
    logical :: exists
    character(CL)  :: lversion
    character(CL)  :: name1
    character(*),parameter :: subName = '(oasis_ioshr_read_int1d) '
    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    if (pio_iam==0) inquire(file=trim(filename),exist=exists)
    call oasis_mpi_bcast(exists,pio_mpicomm,'oasis_ioshr_read_int1d exists')
    if (exists) then
       rcode = pio_openfile(pio_iosystem, pioid, pio_iotype, trim(filename),pio_nowrite)
       !         write(nulprt,*) subname,' open file ',trim(filename)
       call pio_seterrorhandling(pioid,PIO_BCAST_ERROR)
       rcode = pio_get_att(pioid,pio_global,"file_version",lversion)
       call pio_seterrorhandling(pioid,PIO_INTERNAL_ERROR)
    else
        IF(pio_iam==0) WRITE (nulprt,*)  subname,' abort by model ',compid
        IF(pio_iam==0) WRITE(nulprt,*) subname,' ERROR: file invalid ',TRIM(filename),' ',TRIM(dname)
        CALL oasis_abort_noarg()
    endif

    name1 = trim(dname)
    rcode = pio_inq_varid(pioid,trim(name1),varid)
    rcode = pio_get_var(pioid,varid,idata)

    call pio_closefile(pioid)

    !      write(nulprt,*) subname,' read int ',trim(dname)


  end subroutine oasis_ioshr_read_int1d

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_read_r8 - read scalar double from netcdf file
  !
  ! !DESCRIPTION:
  !    Read scalar double from netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_read_r8(filename,rdata,dname)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    real(r8)        ,intent(inout):: rdata  ! real data
    character(len=*),intent(in) :: dname    ! name of data

    !EOP

    real(r8) :: r1d(1)
    character(*),parameter :: subName = '(oasis_ioshr_read_r8) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    call oasis_ioshr_read_r81d(filename,r1d,dname)
    rdata = r1d(1)

  end subroutine oasis_ioshr_read_r8

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_read_r81d - read 1d double array from netcdf file
  !
  ! !DESCRIPTION:
  !    Read 1d double array from netcdf file
  !
  ! !REVISION HISTORY:
  !    2007-Oct-26 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_read_r81d(filename,rdata,dname)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    real(r8)        ,intent(inout):: rdata(:)  ! real data
    character(len=*),intent(in) :: dname    ! name of data

    !EOP

    integer(in) :: rcode
    type(file_desc_T) :: pioid 
    type(var_desc_t) :: varid
    logical :: exists
    character(CL)  :: lversion
    character(CL)  :: name1
    character(*),parameter :: subName = '(oasis_ioshr_read_r81d) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    if (pio_iam==0) inquire(file=trim(filename),exist=exists)
    call oasis_mpi_bcast(exists,pio_mpicomm,'oasis_ioshr_read_r81d exists')
    if (exists) then
       rcode = pio_openfile(pio_iosystem, pioid, pio_iotype, trim(filename),pio_nowrite)
       !         write(nulprt,*) subname,' open file ',trim(filename)
       call pio_seterrorhandling(pioid,PIO_BCAST_ERROR)
       rcode = pio_get_att(pioid,pio_global,"file_version",lversion)
       call pio_seterrorhandling(pioid,PIO_INTERNAL_ERROR)
    else
        IF(pio_iam==0) WRITE (nulprt,*)  subname,' abort by model ',compid
        IF(pio_iam==0) WRITE(nulprt,*) subname,' ERROR: file invalid ',TRIM(filename),' ',TRIM(dname)
        CALL oasis_abort_noarg()
    endif

    name1 = trim(dname)
    rcode = pio_inq_varid(pioid,trim(name1),varid)
    rcode = pio_get_var(pioid,varid,rdata)

    call pio_closefile(pioid)

    !      write(nulprt,*) subname,' read int ',trim(dname)

  end subroutine oasis_ioshr_read_r81d

  !===============================================================================
  !BOP ===========================================================================
  !
  ! !IROUTINE: oasis_ioshr_read_char - read char string from netcdf file
  !
  ! !DESCRIPTION:
  !    Read char string from netcdf file
  !
  ! !REVISION HISTORY:
  !    2010-July-06 - T. Craig - initial version
  !
  ! !INTERFACE: ------------------------------------------------------------------

  subroutine oasis_ioshr_read_char(filename,rdata,dname)

    ! !INPUT/OUTPUT PARAMETERS:
    implicit none
    character(len=*),intent(in) :: filename ! file
    character(len=*),intent(inout):: rdata  ! character data
    character(len=*),intent(in) :: dname    ! name of data

    !EOP

    integer(in) :: rcode
    type(file_desc_T) :: pioid 
    type(var_desc_t) :: varid
    logical :: exists
    character(CL)  :: lversion
    character(CL)  :: name1
    character(*),parameter :: subName = '(oasis_ioshr_read_char) '

    !-------------------------------------------------------------------------------
    !
    !-------------------------------------------------------------------------------

    if (pio_iam==0) inquire(file=trim(filename),exist=exists)
    call oasis_mpi_bcast(exists,pio_mpicomm,'oasis_ioshr_read_char exists')
    if (exists) then
       rcode = pio_openfile(pio_iosystem, pioid, pio_iotype, trim(filename),pio_nowrite)
       !         write(nulprt,*) subname,' open file ',trim(filename)
       call pio_seterrorhandling(pioid,PIO_BCAST_ERROR)
       rcode = pio_get_att(pioid,pio_global,"file_version",lversion)
       call pio_seterrorhandling(pioid,PIO_INTERNAL_ERROR)
    else
        IF(pio_iam==0)WRITE (nulprt,*)  subname,' abort by model ',compid
        IF(pio_iam==0) WRITE(nulprt,*) subname,' ERROR: file invalid ',TRIM(filename),' ',TRIM(dname)
        CALL oasis_abort_noarg()
    endif

    name1 = trim(dname)
    rcode = pio_inq_varid(pioid,trim(name1),varid)
    rcode = pio_get_var(pioid,varid,charvar)
    rdata = trim(charvar)

    call pio_closefile(pioid)

  end subroutine oasis_ioshr_read_char

#endif
  !===============================================================================
!===============================================================================
end module mod_oasis_ioshr
