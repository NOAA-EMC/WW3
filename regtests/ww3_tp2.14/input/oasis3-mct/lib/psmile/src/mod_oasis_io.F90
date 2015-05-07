MODULE mod_oasis_io

   USE mod_oasis_kinds
   USE mod_oasis_data
   USE mod_oasis_parameters
   USE mod_oasis_mpi
   USE mod_oasis_sys
   USE mod_oasis_ioshr
   USE mct_mod
   USE netcdf

   implicit none

   private

   !--- interfaces ---
   public :: oasis_io_read_avfld
   public :: oasis_io_read_avfile
   public :: oasis_io_write_avfile
   public :: oasis_io_read_array
   public :: oasis_io_write_array
   public :: oasis_io_read_avfbf
   public :: oasis_io_write_avfbf
   public :: oasis_io_write_2dgridint_fromroot
   public :: oasis_io_write_2dgridfld_fromroot
   public :: oasis_io_write_3dgridfld_fromroot
   public :: oasis_io_read_field_fromroot

!===========================================================================
CONTAINS
!===========================================================================

!===============================================================================

subroutine oasis_io_read_avfld(filename,av,gsmap,avfld,filefld,fldtype)

   ! ---------------------------------------
   ! Reads single field from file to av
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename   ! filename
   type(mct_aVect) , intent(inout) :: av      ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap
   character(len=*), intent(in) :: avfld      ! av field name
   character(len=*), intent(in) :: filefld    ! file field name
   character(len=*), intent(in),optional :: fldtype       ! int or real

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   integer(ip_i4_p)    :: nx          ! 2d global size nx
   integer(ip_i4_p)    :: ny          ! 2d global size ny
   type(mct_aVect)     :: av_g        ! avect global data
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   real(ip_double_p),allocatable :: array2(:,:)
   integer(ip_i4_p) ,allocatable :: array2i(:,:)
   integer(ip_i4_p)    :: ifldtype     ! field type int (1) or real (2)

   character(len=*),parameter :: subname = 'oasis_io_read_avfld'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   ! empty filename, just return

   call oasis_debug_enter(subname)
   if (len_trim(filename) < 1) then
      call oasis_debug_exit(subname)
      return
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   ifldtype = 2   ! real default
   if (present(fldtype)) then
      ifldtype = 0
      if (trim(fldtype) == 'int')  ifldtype = 1
      if (trim(fldtype) == 'real') ifldtype = 2
      if (ifldtype == 0) then
          WRITE(nulprt,*) subname,' ERROR in fldtype argument'
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif
   endif

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)

   if (iam == master_task) then

      inquire(file=trim(filename),exist=exists)
      if (exists) then
         status = nf90_open(trim(filename),NF90_NOWRITE,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      else
         write(nulprt,*) subname,' ERROR: file missing ',trim(filename)
         WRITE(nulprt,*) subname,' abort by  model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif

      status = nf90_inq_varid(ncid,trim(filefld),varid)
      if (status /= nf90_noerr) then
         write(nulprt,*) subname,':',trim(nf90_strerror(status))
         WRITE(nulprt,*) subname,' ERROR: filefld variable not found '//trim(filefld)
         WRITE(nulprt,*) subname,' abort by  model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif
      status = nf90_inquire_variable(ncid,varid,ndims=dlen,dimids=dimid2)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))
      if (dlen /= 2) then
         write(nulprt,*) subname,' ERROR: variable ndims ne 2 ',trim(filefld),dlen
         WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif
      status = nf90_inquire_dimension(ncid,dimid2(1),len=nx)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))
      status = nf90_inquire_dimension(ncid,dimid2(2),len=ny)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))

      if (size(av_g%rAttr,dim=2) /= nx*ny) then
         write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
         WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif

      if (ifldtype == 1) then
         allocate(array2i(nx,ny))
         status = nf90_get_var(ncid,varid,array2i)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

         n = mct_avect_indexIA(av_g,trim(avfld))
         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            av_g%iAttr(n,n1) = array2i(i,j)
         enddo
         enddo
         deallocate(array2i)
      else
         allocate(array2(nx,ny))
         status = nf90_get_var(ncid,varid,array2)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

         n = mct_avect_indexRA(av_g,trim(avfld))
         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            av_g%rAttr(n,n1) = array2(i,j)
         enddo
         enddo
         deallocate(array2)
      endif

      status = nf90_close(ncid)
      IF (status /= nf90_noerr) THEN
          WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local,':',&
                          TRIM(nf90_strerror(status))
      ENDIF

   endif

   call mct_aVect_scatter(av_g,av,gsmap,master_task,mpicom)
   if (iam == master_task) then
      call mct_aVect_clean(av_g)
   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_read_avfld

!===============================================================================

subroutine oasis_io_write_avfile(rstfile,av,gsmap,nx,ny,nampre)

   ! ---------------------------------------
   ! Writes all fields from av to file
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: rstfile    ! restart filename
   type(mct_aVect) , intent(in) :: av         ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny
   character(len=*), intent(in),optional :: nampre  ! name prepend string

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   integer(ip_i4_p)    :: nxf,nyf     ! field size on file
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! string converted to char
   character(ic_med)   :: lnampre     ! local nampre
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   real(ip_double_p),allocatable :: array2(:,:)

   character(len=*),parameter :: subname = 'oasis_io_write_avfile'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   ! empty filename, just return

   if (len_trim(rstfile) < 1) then
      call oasis_debug_exit(subname)
      return
   endif

   lnampre = ""
   if (present(nampre)) then
      lnampre = trim(nampre)
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)

   if (iam == master_task) then
      if (size(av_g%rAttr,dim=2) /= nx*ny) then
         write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',&
                         size(av_g%rAttr,dim=2),nx,ny
         WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif

      inquire(file=trim(rstfile),exist=exists)
      if (exists) then
         status = nf90_open(trim(rstfile),NF90_WRITE,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_redef(ncid)
      else
         status = nf90_create(trim(rstfile),NF90_CLOBBER,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      endif

      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         itemc = trim(lnampre)//trim(itemc)
         call mct_string_clean(mstring)

         status = nf90_inq_dimid(ncid,trim(itemc)//'_nx',dimid2(1))
         if (status /= nf90_noerr) then
            status = nf90_def_dim(ncid,trim(itemc)//'_nx',nx,dimid2(1))
         endif

         status = nf90_inq_dimid(ncid,trim(itemc)//'_ny',dimid2(2))
         if (status /= nf90_noerr) then
            status = nf90_def_dim(ncid,trim(itemc)//'_ny',ny,dimid2(2))
         endif

         status = nf90_inquire_dimension(ncid,dimid2(1),len=dlen)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         if (dlen /= nx) then
            write(nulprt,*) subname,' ERROR: dlen ne nx ',dlen,nx
            CALL oasis_flush(nulprt)
!            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
!            call oasis_abort_noarg()
         endif

         status = nf90_inquire_dimension(ncid,dimid2(2),len=dlen)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         if (dlen /= ny) then
            write(nulprt,*) subname,' ERROR: dlen ne ny ',dlen,ny
            CALL oasis_flush(nulprt)
!            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
!            call oasis_abort_noarg()
         endif

         status = nf90_inq_varid(ncid,trim(itemc),varid)
         if (status /= nf90_noerr) then
            status = nf90_def_var(ncid,trim(itemc),NF90_DOUBLE,dimid2,varid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
         endif

      enddo

      status = nf90_enddef(ncid)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))

      nxf = 0
      nyf = 0
      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         itemc = trim(lnampre)//trim(itemc)
         call mct_string_clean(mstring)
         status = nf90_inq_varid(ncid,trim(itemc),varid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         if (n == 1) then
            status = nf90_inquire_variable(ncid,varid,ndims=dlen,dimids=dimid2)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_inquire_dimension(ncid,dimid2(1),len=nxf)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_inquire_dimension(ncid,dimid2(2),len=nyf)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            if (dlen /= 2 .or. nx*ny /= nxf*nyf) then
               WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
               WRITE(nulprt,*) subname,' abort ERROR: ndims and size does not match on file'
               CALL oasis_flush(nulprt)
               call oasis_abort_noarg()
            endif
            allocate(array2(nxf,nyf))
         endif

         n1 = 0
         do j = 1,nyf
         do i = 1,nxf
            n1 = n1 + 1
            array2(i,j) = av_g%rAttr(n,n1)
         enddo
         enddo

         status = nf90_put_var(ncid,varid,array2)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      enddo
      deallocate(array2)
      call mct_aVect_clean(av_g)

      status = nf90_close(ncid)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))

   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_write_avfile

!===============================================================================

subroutine oasis_io_read_avfile(rstfile,av,gsmap,abort,nampre,didread)

   ! ---------------------------------------
   ! Reads all fields for av from file
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: rstfile    ! restart filename
   type(mct_aVect) , intent(inout) :: av      ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap
   logical         , intent(in) ,optional :: abort   ! abort on fail flag
   character(len=*), intent(in) ,optional :: nampre  ! name prepend string
   logical         , intent(out),optional :: didread ! was something read

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   integer(ip_i4_p)    :: nx          ! 2d global size nx
   integer(ip_i4_p)    :: ny          ! 2d global size ny
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! string converted to char
   character(ic_med)   :: lnampre     ! local nampre
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   logical             :: labort      ! local abort flag
   real(ip_double_p),allocatable :: array2(:,:)

   character(len=*),parameter :: subname = 'oasis_io_read_avfile'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   IF (mpi_comm_local /= MPI_COMM_NULL) THEN
   call oasis_debug_enter(subname)

   if (present(didread)) didread = .false.

   ! empty filename, just return

   if (len_trim(rstfile) < 1) then
      call oasis_debug_exit(subname)
      return
   endif

   labort = .true.
   if (present(abort)) then
      labort = abort
   endif

   lnampre = ""
   if (present(nampre)) then
      lnampre = trim(nampre)
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)

   if (iam == master_task) then

      inquire(file=trim(rstfile),exist=exists)
      if (.not.exists) then
         write(nulprt,*) subname,' ERROR: file missing ',trim(rstfile)
         WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         IF (labort) THEN
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             CALL oasis_abort_noarg()
         ENDIF
      else
         status = nf90_open(trim(rstfile),NF90_NOWRITE,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

         do n = 1,mct_aVect_nRAttr(av_g)
            call mct_aVect_getRList(mstring,n,av_g)
            itemc = mct_string_toChar(mstring)
            itemc = trim(lnampre)//trim(itemc)
            call mct_string_clean(mstring)

            status = nf90_inq_varid(ncid,trim(itemc),varid)

            if (status /= nf90_noerr) then
               write(nulprt,*) subname,':',trim(itemc),':',trim(nf90_strerror(status))
               WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
               CALL oasis_flush(nulprt)
               IF (labort) THEN
                   WRITE(nulprt,*) subname,'ERROR: var missing'
                   WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                   CALL oasis_flush(nulprt)
                   CALL oasis_abort_noarg()
               ENDIF

            else
               status = nf90_inquire_variable(ncid,varid,ndims=dlen,dimids=dimid2)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))
               if (dlen /= 2) then
                  write(nulprt,*) subname,' ERROR: variable ndims ne 2 ',trim(itemc),dlen
                  WRITE(nulprt,*) subname,' abort by  model :',compid,' proc :',mpi_rank_local
                  CALL oasis_flush(nulprt)
                  call oasis_abort_noarg()
               endif
               status = nf90_inquire_dimension(ncid,dimid2(1),len=nx)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))
               status = nf90_inquire_dimension(ncid,dimid2(2),len=ny)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))

               if (size(av_g%rAttr,dim=2) /= nx*ny) then
                  write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
                  WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                  CALL oasis_flush(nulprt)
                  call oasis_abort_noarg()
               endif

               allocate(array2(nx,ny))

               status = nf90_get_var(ncid,varid,array2)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))

               n1 = 0
               do j = 1,ny
               do i = 1,nx
                  n1 = n1 + 1
                  av_g%rAttr(n,n1) = array2(i,j)
               enddo
               enddo
               if (present(didread)) didread = .true.

               deallocate(array2)
            endif  ! varid valid
         enddo

         status = nf90_close(ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

      endif  ! file exists
   endif

   call mct_aVect_scatter(av_g,av,gsmap,master_task,mpicom)
   if (iam == master_task) then
      call mct_aVect_clean(av_g)
   endif

   call oasis_debug_exit(subname)
   ENDIF

end subroutine oasis_io_read_avfile

!===============================================================================

subroutine oasis_io_read_array(rstfile,iarray,ivarname,rarray,rvarname,abort)

   ! ---------------------------------------
   ! Writes all fields from av to file
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: rstfile    ! restart filename
   integer(ip_i4_p), intent(inout),optional :: iarray(:) ! data on root
   character(len=*), intent(in),optional :: ivarname     ! variable name on file
   real(ip_double_p),intent(inout),optional :: rarray(:) ! data on root
   character(len=*), intent(in),optional :: rvarname     ! variable name on file
   logical         , intent(in),optional :: abort        ! abort flag

   !--- local ---
   integer(ip_i4_p)    :: ncnt
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid1(1),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   logical             :: labort      ! local abort flag

   character(len=*),parameter :: subname = 'oasis_io_read_array'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   IF (mpi_comm_local /= MPI_COMM_NULL) THEN
   call oasis_debug_enter(subname)

   ! empty filename, just return

   if (len_trim(rstfile) < 1) then
      call oasis_debug_exit(subname)
      return
   endif

   labort = .true.
   if (present(abort)) then
      labort = abort
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   if (iam == master_task) then

      inquire(file=trim(rstfile),exist=exists)
      if (.not.exists) then
         write(nulprt,*) subname,' ERROR: file missing ',trim(rstfile)
         WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         IF (labort) THEN
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             CALL oasis_abort_noarg()
         ENDIF
      else
         status = nf90_open(trim(rstfile),NF90_NOWRITE,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

         if (present(iarray)) then
            if (.not. present(ivarname)) then
               write(nulprt,*) subname,' ERROR: iarray must have ivarname set'
               WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
               CALL oasis_flush(nulprt)
               call oasis_abort_noarg()
            endif

            ncnt = size(iarray)

            status = nf90_inq_varid(ncid,trim(ivarname),varid)
            if (status /= nf90_noerr) then
               write(nulprt,*) subname,':',trim(ivarname),':',trim(nf90_strerror(status))
               WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
               CALL oasis_flush(nulprt)
               IF (labort) THEN
                   WRITE(nulprt,*) subname,'ERROR: var missing'
                   WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                   CALL oasis_flush(nulprt)
                   CALL oasis_abort_noarg()
               ENDIF
            else
               status = nf90_inquire_variable(ncid,varid,ndims=dlen,dimids=dimid1)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))
               if (dlen /= 1) then
                  write(nulprt,*) subname,' ERROR: variable ndims ne 1 ',trim(ivarname),dlen
                  WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                  CALL oasis_flush(nulprt)
                  call oasis_abort_noarg()
               endif
               status = nf90_inquire_dimension(ncid,dimid1(1),len=dlen)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))

               if (ncnt /= dlen) then
                  write(nulprt,*) subname,' ERROR: iarray ncnt dlen mismatch ',ncnt,dlen
                  WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                  CALL oasis_flush(nulprt)
                  call oasis_abort_noarg()
               endif

               status = nf90_get_var(ncid,varid,iarray)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))
            endif
         endif

         if (present(rarray)) then
            if (.not. present(rvarname)) then
               write(nulprt,*) subname,' ERROR: rarray must have rvarname set'
               WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
               CALL oasis_flush(nulprt)
               call oasis_abort_noarg()
            endif

            ncnt = size(rarray)

            status = nf90_inq_varid(ncid,trim(rvarname),varid)
            if (status /= nf90_noerr) then
               write(nulprt,*) subname,':',trim(rvarname),':',trim(nf90_strerror(status))
               WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
               CALL oasis_flush(nulprt)
               IF (labort) THEN
                   WRITE(nulprt,*) subname,'ERROR: var missing'
                   WRITE(nulprt,*) subname,' abort by  model :',compid,' proc :',mpi_rank_local
                   CALL oasis_flush(nulprt)
                   CALL oasis_abort_noarg()
               ENDIF
            else
               status = nf90_inquire_variable(ncid,varid,ndims=dlen,dimids=dimid1)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))
               if (dlen /= 1) then
                  write(nulprt,*) subname,' ERROR: variable ndims ne 1 ',trim(rvarname),dlen
                  WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                  CALL oasis_flush(nulprt)
                  call oasis_abort_noarg()
               endif
               status = nf90_inquire_dimension(ncid,dimid1(1),len=dlen)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))

               if (ncnt /= dlen) then
                  write(nulprt,*) subname,' ERROR: rarray ncnt dlen mismatch ',ncnt,dlen
                  WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                  CALL oasis_flush(nulprt)
                  call oasis_abort_noarg()
               endif

               status = nf90_get_var(ncid,varid,rarray)
               IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                         mpi_rank_local,':',TRIM(nf90_strerror(status))
            endif
         endif

         status = nf90_close(ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

      endif
   endif

   if (present(iarray)) then
      call oasis_mpi_bcast(iarray,mpicom,trim(subname)//':iarray',master_task)
   endif

   if (present(rarray)) then
      call oasis_mpi_bcast(rarray,mpicom,trim(subname)//':rarray',master_task)
   endif

   call oasis_debug_exit(subname)
   ENDIF

end subroutine oasis_io_read_array

!===============================================================================

subroutine oasis_io_write_array(rstfile,iarray,ivarname,rarray,rvarname)

   ! ---------------------------------------
   ! Writes all fields from av to file
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: rstfile    ! restart filename
   integer(ip_i4_p), intent(in),optional :: iarray(:)   ! data on root
   character(len=*), intent(in),optional :: ivarname    ! variable name on file
   real(ip_double_p),intent(in),optional :: rarray(:)   ! data on root
   character(len=*), intent(in),optional :: rvarname    ! variable name on file

   !--- local ---
   integer(ip_i4_p)    :: ncnt
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid1(1),varid ! netcdf info
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance

   character(len=*),parameter :: subname = 'oasis_io_write_array'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   ! empty filename, just return

   if (len_trim(rstfile) < 1) then
      call oasis_debug_exit(subname)
      return
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   if (iam == master_task) then

      inquire(file=trim(rstfile),exist=exists)
      if (exists) then
         status = nf90_open(trim(rstfile),NF90_WRITE,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_redef(ncid)
      else
         status = nf90_create(trim(rstfile),NF90_CLOBBER,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      endif

      if (present(iarray)) then
         if (.not. present(ivarname)) then
            write(nulprt,*) subname,' ERROR: iarray must have ivarname set'
            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
            CALL oasis_flush(nulprt)
            call oasis_abort_noarg()
         endif

         ncnt = size(iarray)

         status = nf90_inq_dimid(ncid,trim(ivarname)//'_ncnt',dimid1(1))
         if (status /= nf90_noerr) then
            status = nf90_def_dim(ncid,trim(ivarname)//'_ncnt',ncnt,dimid1(1))
         endif

         status = nf90_inquire_dimension(ncid,dimid1(1),len=dlen)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         if (dlen /= ncnt) then
            write(nulprt,*) subname,' ERROR: iarray dlen ne ncnt ',dlen,ncnt
            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
            CALL oasis_flush(nulprt)
            call oasis_abort_noarg()
         endif

         status = nf90_inq_varid(ncid,trim(ivarname),varid)
         if (status /= nf90_noerr) then
            status = nf90_def_var(ncid,trim(ivarname),NF90_INT,dimid1,varid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
         endif
      endif

      if (present(rarray)) then
         if (.not. present(rvarname)) then
            write(nulprt,*) subname,' ERROR: rarray must have rvarname set'
            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
            CALL oasis_flush(nulprt)
            call oasis_abort_noarg()
         endif

         ncnt = size(rarray)

         status = nf90_inq_dimid(ncid,trim(rvarname)//'_ncnt',dimid1(1))
         if (status /= nf90_noerr) then
            status = nf90_def_dim(ncid,trim(rvarname)//'_ncnt',ncnt,dimid1(1))
         endif

         status = nf90_inquire_dimension(ncid,dimid1(1),len=dlen)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         if (dlen /= ncnt) then
            write(nulprt,*) subname,' ERROR: rarray dlen ne ncnt ',dlen,ncnt
            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
            CALL oasis_flush(nulprt)
            call oasis_abort_noarg()
         endif

         status = nf90_inq_varid(ncid,trim(rvarname),varid)
         if (status /= nf90_noerr) then
            status = nf90_def_var(ncid,trim(rvarname),NF90_DOUBLE,dimid1,varid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
         endif
      endif

      status = nf90_enddef(ncid)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))

      if (present(iarray)) then
         status = nf90_inq_varid(ncid,trim(ivarname),varid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_put_var(ncid,varid,iarray)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      endif

      if (present(rarray)) then
         status = nf90_inq_varid(ncid,trim(rvarname),varid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_put_var(ncid,varid,rarray)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      endif

      status = nf90_close(ncid)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))

   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_write_array

!===============================================================================

subroutine oasis_io_write_avfbf(av,gsmap,nx,ny,msec,f_string,filename)

   ! ---------------------------------------
   ! Write all fields from av to individual field files
   ! This works only for a single av to a file
   ! Optionally can specify time info, and filename info
   ! ---------------------------------------

   implicit none

   type(mct_aVect) , intent(in) :: av         ! avect
   type(mct_gsmap) , intent(in) :: gsmap      ! gsmap
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny
   integer(ip_i4_p), intent(in),optional :: msec    ! time info
   character(len=*), intent(in),optional :: f_string  ! optional f_string to append to filename
   character(len=*), intent(in),optional :: filename   ! optional output filename

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! f_string converted to char
   character(ic_med)   :: lfn         ! local filename
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid3(3),varid ! netcdf info
   integer(ip_i4_p)    :: start3(3),count3(3)        ! netcdf info
   integer(ip_i4_p)    :: start1(1),count1(1)        ! netcdf info
   integer(ip_i4_p)    :: lmsec(1)    ! local msec value
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   logical             :: whead,wdata ! for writing restart/history cdf files
   real(ip_double_p),allocatable :: array3(:,:,:)
   real(ip_double_p)   :: tbnds(2)

   character(len=*),parameter :: subname = 'oasis_io_write_avfbf'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lmsec = 0
   if (present(msec)) then
      lmsec = msec
   endif

   lstring = " "
   if (present(f_string)) then
      lstring = trim(f_string)
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

#if (PIO_DEFINED)
! tcraig, not working as of Oct 2011
   call oasis_ioshr_wopen(lfn,clobber=.true.,cdf64=.true.)

   do fk = fk1,2
      if (fk == 1) then
         whead = .true.
         wdata = .false.
      elseif (fk == 2) then
         whead = .false.
         wdata = .true.
      else
          WRITE(nulprt,*) subname,'ERROR: fk illegal'
          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
         call oasis_abort_noarg()
      end if

      call oasis_ioshr_write(lfn,&
               time_units='seconds',time_cal='seconds',time_val=real(lmsec,ip_double_p),&
               nt=1,whead=whead,wdata=wdata)

      call oasis_ioshr_write(lfn, gsmap, av, 'pout', &
               whead=whead,wdata=wdata,nx=nx,ny=ny,nt=1, &
               use_float=.false.)
   
      if (fk == 1) call oasis_ioshr_enddef(lfn)
   enddo

   call oasis_ioshr_close(lfn)
#else

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)
   if (iam == master_task) then
      if (size(av_g%rAttr,dim=2) /= nx*ny) then
         write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
         WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif

      allocate(array3(nx,ny,1))
      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         call mct_string_clean(mstring)
         if (present(filename)) then
            lfn = trim(filename)
         else
            lfn = trim(itemc)//trim(lstring)//'.nc'
         endif
         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            array3(i,j,1) = av_g%rAttr(n,n1)
         enddo
         enddo

         start1 = 1
         count1 = 1
         start3 = 1
         count3(1) = nx
         count3(2) = ny
         count3(3) = 1

         inquire(file=trim(lfn),exist=exists)
         if (exists) then
            status = nf90_open(lfn,NF90_WRITE,ncid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_inq_dimid(ncid,'time',dimid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_inquire_dimension(ncid,dimid,len=dlen)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            start1(1) = dlen + 1
            start3(3) = start1(1)
         else
            status = nf90_create(lfn,NF90_CLOBBER,ncid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_def_dim(ncid,'nx',nx,dimid3(1))
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_def_dim(ncid,'ny',ny,dimid3(2))
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_def_dim(ncid,'time',NF90_UNLIMITED,dimid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            dimid3(3) = dimid
            status = nf90_def_var(ncid,'time',NF90_INT,dimid,varid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_def_var(ncid,trim(itemc),NF90_DOUBLE,dimid3,varid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
            status = nf90_enddef(ncid)
            IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                      mpi_rank_local,':',TRIM(nf90_strerror(status))
         endif

         status = nf90_inq_varid(ncid,'time',varid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_put_var(ncid,varid,lmsec,start1,count1)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_inq_varid(ncid,trim(itemc),varid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_put_var(ncid,varid,array3,start3,count3)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_close(ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      enddo
      deallocate(array3)
      call mct_aVect_clean(av_g)
   endif


#endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_write_avfbf

!===============================================================================

subroutine oasis_io_read_avfbf(av,gsmap,msec,f_string,filename)

   ! ---------------------------------------
   ! Read all fields to av from individual field files
   ! This works only for a single av from a file
   ! Optionally can specify time info, and filename info
   ! ---------------------------------------

   implicit none

   type(mct_aVect) , intent(inout) :: av     ! avect
   type(mct_gsmap) , intent(in) :: gsmap     ! gsmap
   integer(ip_i4_p), intent(in),optional :: msec    ! time info
   character(len=*), intent(in),optional :: f_string  ! optional f_string to append to filename
   character(len=*), intent(in),optional :: filename   ! optional input filename

   !--- local ---
   integer(ip_i4_p)    :: n,n1,i,j,fk,fk1    ! index
   integer(ip_i4_p)    :: nx,ny       ! grid size from file
   type(mct_aVect)     :: av_g        ! avect global data
   type(mct_string)    :: mstring     ! mct char type
   character(ic_med)   :: itemc       ! f_string converted to char
   character(ic_med)   :: lfn         ! local filename
   character(ic_med)   :: lstring     ! local filename
   integer(ip_i4_p)    :: mpicom,master_task,iam     ! mpi info
   integer(ip_i4_p)    :: ncid,dimid,dimid3(3),varid ! netcdf info
   integer(ip_i4_p)    :: start3(3),count3(3)        ! netcdf info
   integer(ip_i4_p)    :: lmsec(1)    ! local msec value
   integer(ip_i4_p)    :: dlen        ! dimension length
   integer(ip_i4_p)    :: status      ! error code
   logical             :: exists      ! file existance
   logical             :: whead,wdata ! for writing restart/history cdf files
   real(ip_double_p),allocatable :: array3(:,:,:)
   integer(ip_i4_p) ,allocatable :: time(:)
   real(ip_double_p)   :: tbnds(2)

   character(len=*),parameter :: subname = 'oasis_io_read_avfbf'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

   lmsec = 0
   if (present(msec)) then
      lmsec = msec
   endif

   lstring = " "
   if (present(f_string)) then
      lstring = trim(f_string)
   endif

   mpicom = mpi_comm_local
   master_task = 0
   iam = mpi_rank_local

   call mct_aVect_gather(av,av_g,gsmap,master_task,mpicom)
   if (iam == master_task) then
      do n = 1,mct_aVect_nRAttr(av_g)
         call mct_aVect_getRList(mstring,n,av_g)
         itemc = mct_string_toChar(mstring)
         call mct_string_clean(mstring)
         if (present(filename)) then
            lfn = trim(filename)
         else
            lfn = trim(itemc)//trim(lstring)//'.nc'
         endif

         inquire(file=trim(lfn),exist=exists)
         if (.not.exists) then
            write(nulprt,*) subname,' ERROR: file not found ',trim(lfn)
            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
            CALL oasis_flush(nulprt)
            call oasis_abort_noarg()
         endif

         status = nf90_open(lfn,NF90_NOWRITE,ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

         status = nf90_inq_dimid(ncid,'time',dimid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_inquire_dimension(ncid,dimid,len=dlen)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         allocate(time(dlen))
         status = nf90_inq_varid(ncid,'time',varid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_get_var(ncid,varid,time)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         n1 = 0
         do j = 1,dlen
            if (time(j) == lmsec(1)) n1 = j
         enddo
         deallocate(time)
         if (n1 < 1) then
            write(nulprt,*) subname,' ERROR: time not found on file ',trim(lfn),lmsec
            WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
            CALL oasis_flush(nulprt)
            call oasis_abort_noarg()
         endif

         status = nf90_inq_varid(ncid,trim(itemc),varid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_inquire_variable(ncid,varid,dimids=dimid3)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_inquire_dimension(ncid,dimid3(1),len=nx)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_inquire_dimension(ncid,dimid3(2),len=ny)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

         if (size(av_g%rAttr,dim=2) /= nx*ny) then
             write(nulprt,*) subname,' ERROR: av gsize nx ny mismatch ',size(av_g%rAttr,dim=2),nx,ny
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             call oasis_abort_noarg()
         endif

         start3 = 1
         count3(1) = nx
         count3(2) = ny
         count3(3) = 1
         start3(3) = n1
         allocate(array3(nx,ny,1))

         status = nf90_get_var(ncid,varid,array3,start3,count3)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
         status = nf90_close(ncid)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))

         n1 = 0
         do j = 1,ny
         do i = 1,nx
            n1 = n1 + 1
            av_g%rAttr(n,n1) = array3(i,j,1)
         enddo
         enddo

         deallocate(array3)

      enddo
   endif

   call mct_aVect_scatter(av_g,av,gsmap,master_task,mpicom)
   if (iam == master_task) then
      call mct_aVect_clean(av_g)
   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_read_avfbf

!===============================================================================

subroutine oasis_io_read_field_fromroot(filename,fldname,ifld2,fld2,fld3,nx,ny,nz)

   ! ---------------------------------------
   ! Write real fld on rootpe to file
   ! Designed to work with oasis3 write_grid 
   ! ---------------------------------------

   implicit none

   character(len=*) , intent(in) :: filename
   character(len=*) , intent(in) :: fldname
   integer(ip_i4_p) , intent(inout),optional :: ifld2(:,:)
   real(ip_realwp_p), intent(inout),optional :: fld2(:,:)
   real(ip_realwp_p), intent(inout),optional :: fld3(:,:,:)
   integer(ip_i4_p) , intent(inout),optional :: nx         ! global size nx
   integer(ip_i4_p) , intent(inout),optional :: ny         ! global size ny
   integer(ip_i4_p) , intent(inout),optional :: nz         ! global size nz

   !--- local ---
   integer(ip_i4_p)    :: ncid,varid  ! cdf info
   integer(ip_i4_p)    :: n,ndims,xtype
   integer(ip_i4_p),allocatable :: dimid(:),nd(:)
   integer(ip_i4_p)    :: status      ! error code
   integer(ip_i4_p)    :: ind         ! string index
   logical             :: exists      ! file existance
   character(len=ic_med) :: gridname  ! grid name derived from fldname

   character(len=*),parameter :: subname = 'oasis_io_read_field_fromroot'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

!   expects to run only on 1 pe.
!   if (iam == master_task) then

   inquire(file=trim(filename),exist=exists)
   if (exists) then
      status = nf90_open(filename,NF90_NOWRITE,ncid)
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))
      status = nf90_redef(ncid)
   else
      write(nulprt,*) subname,' ERROR: in filename ',trim(filename)
      WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif

   status = nf90_inq_varid(ncid,trim(fldname),varid)
   if (status /= nf90_noerr) then
      write(nulprt,*) subname,' ERROR: in variable name ',trim(fldname)
      WRITE(nulprt,*) subname,' abort by  model :',compid,' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif

   status = nf90_inquire_variable(ncid,varid,ndims=ndims,xtype=xtype)
   IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                             mpi_rank_local,':',TRIM(nf90_strerror(status))

   allocate(dimid(ndims),nd(ndims))

   status = nf90_inquire_variable(ncid,varid,dimids=dimid)
   IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                             mpi_rank_local,':',TRIM(nf90_strerror(status))
   do n = 1,ndims
      status = nf90_inquire_dimension(ncid,dimid(n),len=nd(n))
      IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                mpi_rank_local,':',TRIM(nf90_strerror(status))
   enddo

   if (present(ifld2) .or. present(fld2) .or. present(fld3)) then
      if (xtype == NF90_INT .and. ndims == 2 .and. present(ifld2)) then
         status = nf90_get_var(ncid,varid,ifld2)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      elseif (xtype /= NF90_INT .and. ndims == 2 .and. present(fld2)) then
         status = nf90_get_var(ncid,varid,fld2)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      elseif (xtype /= NF90_INT .and. ndims == 3 .and. present(fld3)) then
         status = nf90_get_var(ncid,varid,fld3)
         IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                   mpi_rank_local,':',TRIM(nf90_strerror(status))
      else
         write(nulprt,*) subname,' ERROR: mismatch in field and data'
         WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif
   endif
    
   status = nf90_close(ncid)
   IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                             mpi_rank_local,':',TRIM(nf90_strerror(status))

   if (present(nx)) then
      nx = nd(1)
   endif

   if (present(ny)) then
      ny = nd(2)
   endif

   if (present(nz)) then
      nz = nd(3)
   endif

   deallocate(dimid,nd)

!   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_read_field_fromroot

!===============================================================================

subroutine oasis_io_write_2dgridfld_fromroot(filename,fldname,fld,nx,ny)

   ! ---------------------------------------
   ! Write real fld on rootpe to file
   ! Designed to work with oasis3 write_grid 
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: fldname
   real(ip_realwp_p), intent(in) :: fld(:,:)
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny

   !--- local ---
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid  ! cdf info
   integer(ip_i4_p)    :: status      ! error code
   integer(ip_i4_p)    :: ind         ! string index
   logical             :: exists      ! file existance
   character(len=ic_med) :: gridname  ! grid name derived from fldname

   character(len=*),parameter :: subname = 'oasis_io_write_2dgridfld_fromroot'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

!   expects to run only on 1 pe.
!   if (iam == master_task) then

    ind = index(trim(fldname),'.')
    if (ind < 2) then
       write(nulprt,*) subname,' ERROR: in fldname ',trim(fldname)
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif
    gridname = fldname(1:ind-1)

    inquire(file=trim(filename),exist=exists)
    if (exists) then
       status = nf90_open(filename,NF90_WRITE,ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_redef(ncid)
    else
       status = nf90_create(filename,NF90_CLOBBER,ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define x dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'x_'//trim(gridname),dimid2(1))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'x_'//trim(gridname),nx,dimid2(1))
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define y dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'y_'//trim(gridname),dimid2(2))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'y_'//trim(gridname),ny,dimid2(2))
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define var if it doesn't exist
    status = nf90_inq_varid(ncid,trim(fldname),varid)
    if (status /= nf90_noerr) then
       status = nf90_def_var(ncid,trim(fldname),NF90_DOUBLE,dimid2,varid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_enddef(ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_put_var(ncid,varid,fld)
       if (status /= nf90_noerr) write(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',trim(nf90_strerror(status))
    else
       status = nf90_enddef(ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    status = nf90_close(ncid)
    IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                              mpi_rank_local,':',TRIM(nf90_strerror(status))

!   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_write_2dgridfld_fromroot

!===============================================================================

subroutine oasis_io_write_2dgridint_fromroot(filename,fldname,fld,nx,ny)

   ! ---------------------------------------
   ! Write int fld on rootpe to file
   ! Designed to work with oasis3 write_grid 
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: fldname
   integer(ip_i4_p), intent(in) :: fld(:,:)
   integer(ip_i4_p), intent(in) :: nx         ! 2d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 2d global size ny

   !--- local ---
   integer(ip_i4_p)    :: ncid,dimid,dimid2(2),varid  ! cdf info
   integer(ip_i4_p)    :: status      ! error code
   integer(ip_i4_p)    :: ind         ! string index
   logical             :: exists      ! file existance
   character(len=ic_med) :: gridname  ! grid name derived from fldname

   character(len=*),parameter :: subname = 'oasis_io_write_2dgridint_fromroot'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

!   expects to run only on 1 pe.
!   if (iam == master_task) then

    ind = index(trim(fldname),'.')
    if (ind < 2) then
       write(nulprt,*) subname,' ERROR: in fldname ',trim(fldname)
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif
    gridname = fldname(1:ind-1)

    inquire(file=trim(filename),exist=exists)
    if (exists) then
       status = nf90_open(filename,NF90_WRITE,ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_redef(ncid)
    else
       status = nf90_create(filename,NF90_CLOBBER,ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define x dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'x_'//trim(gridname),dimid2(1))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'x_'//trim(gridname),nx,dimid2(1))
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define y dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'y_'//trim(gridname),dimid2(2))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'y_'//trim(gridname),ny,dimid2(2))
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define var if it doesn't exist
    status = nf90_inq_varid(ncid,trim(fldname),varid)
    if (status /= nf90_noerr) then
       status = nf90_def_var(ncid,trim(fldname),NF90_INT,dimid2,varid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_enddef(ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_put_var(ncid,varid,fld)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    else
       status = nf90_enddef(ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    status = nf90_close(ncid)
    IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                              mpi_rank_local,':',TRIM(nf90_strerror(status))

!   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_write_2dgridint_fromroot

!===============================================================================

subroutine oasis_io_write_3dgridfld_fromroot(filename,fldname,fld,nx,ny,nc)
 
   ! ---------------------------------------
   ! Write real 3d fld on rootpe to file
   ! Designed to work with oasis3 write_grid (corners)
   ! ---------------------------------------

   implicit none

   character(len=*), intent(in) :: filename
   character(len=*), intent(in) :: fldname
   real(ip_realwp_p), intent(in) :: fld(:,:,:)
   integer(ip_i4_p), intent(in) :: nx         ! 3d global size nx
   integer(ip_i4_p), intent(in) :: ny         ! 3d global size ny
   integer(ip_i4_p), intent(in) :: nc         ! 3d global size nc ncorners

   !--- local ---
   integer(ip_i4_p)    :: ncid,dimid,dimid3(3),varid  ! cdf info
   integer(ip_i4_p)    :: status      ! error code
   integer(ip_i4_p)    :: ind         ! string index
   logical             :: exists      ! file existance
   character(len=ic_med) :: gridname  ! grid name derived from fldname

   character(len=*),parameter :: subname = 'oasis_io_write_3dgridfld_fromroot'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

   call oasis_debug_enter(subname)

!   expects to run only on 1 pe.
!   if (iam == master_task) then

    ind = index(trim(fldname),'.')
    if (ind < 2) then
       write(nulprt,*) subname,' ERROR: in fldname ',trim(fldname)
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif
    gridname = fldname(1:ind-1)

    inquire(file=trim(filename),exist=exists)
    if (exists) then
       status = nf90_open(filename,NF90_WRITE,ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_redef(ncid)
    else
       status = nf90_create(filename,NF90_CLOBBER,ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define x dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'x_'//trim(gridname),dimid3(1))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'x_'//trim(gridname),nx,dimid3(1))
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define y dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'y_'//trim(gridname),dimid3(2))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'y_'//trim(gridname),ny,dimid3(2))
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define crn dimension if it doesn't exist
    status = nf90_inq_dimid(ncid,'crn_'//trim(gridname),dimid3(3))
    if (status /= nf90_noerr) then
       status = nf90_def_dim(ncid,'crn_'//trim(gridname),nc,dimid3(3))
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    ! define var if it doesn't exist
    status = nf90_inq_varid(ncid,trim(fldname),varid)
    if (status /= nf90_noerr) then
       status = nf90_def_var(ncid,trim(fldname),NF90_DOUBLE,dimid3,varid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_enddef(ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
       status = nf90_put_var(ncid,varid,fld)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    else
       status = nf90_enddef(ncid)
       IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                                 mpi_rank_local,':',TRIM(nf90_strerror(status))
    endif

    status = nf90_close(ncid)
    IF (status /= nf90_noerr) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                              mpi_rank_local,':',TRIM(nf90_strerror(status))

!   endif

   call oasis_debug_exit(subname)

end subroutine oasis_io_write_3dgridfld_fromroot

!-------------------------------------------------------------------

END MODULE mod_oasis_io
