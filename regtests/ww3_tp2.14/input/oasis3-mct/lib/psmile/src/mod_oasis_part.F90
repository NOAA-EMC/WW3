MODULE mod_oasis_part

   USE mod_oasis_kinds
   USE mod_oasis_data
   USE mod_oasis_parameters
   USE mod_oasis_sys
   USE mod_oasis_timer
   USE mct_mod

   implicit none

   private

   !--- interfaces ---
   public :: oasis_def_partition
   public :: oasis_part_create

   !--- datatypes ---
   public :: prism_part_type

   integer(kind=ip_intwp_p),parameter :: mpart = 100

   type prism_part_type
      type(mct_gsmap)        :: gsmap
      integer(kind=ip_i4_p)  :: gsize
      integer(kind=ip_i4_p)  :: nx
      integer(kind=ip_i4_p)  :: ny
      character(len=ic_med)  :: gridname
   end type prism_part_type

   integer(kind=ip_intwp_p),public :: prism_npart = 0
   type(prism_part_type),public :: prism_part(mpart)

CONTAINS
!
  SUBROUTINE oasis_def_partition (id_part, kparal, kinfo, ig_size)
!
!*    *** Def_partition ***   PRISM 1.0
!
!     purpose:
!     --------
!        define a decomposition
!
!     interface:
!     ----------
!        id_part : field decomposition id
!        kparal : type of parallel decomposition
!	 kinfo	: output status
!
!     author:
!     -------
!        Arnaud Caubel - FECIT
!
!  ----------------------------------------------------------------
   INTEGER(kind=ip_intwp_p)              ,intent(out) :: id_part
   INTEGER(kind=ip_intwp_p), DIMENSION(:),intent(in)  :: kparal
   INTEGER(kind=ip_intwp_p), optional    ,intent(out) :: kinfo
   INTEGER(kind=ip_intwp_p), optional    ,intent(in)  :: ig_size
!  ----------------------------------------------------------------
   INTEGER(kind=ip_intwp_p) :: n,k,nsegs,numel
   INTEGER(kind=ip_intwp_p) :: len
   integer(kind=ip_intwp_p),pointer :: start(:),length(:)
   character(len=*),parameter :: subname = 'oasis_def_partition'
!  ----------------------------------------------------------------

   call oasis_debug_enter(subname)

   kinfo = OASIS_OK

   call oasis_timer_start('map definition')

   if (prism_npart == 0) then  ! first call
      do n = 1,mpart
         prism_part(n)%gsize = -1
         prism_part(n)%nx    = -1
         prism_part(n)%ny    = -1
         prism_part(n)%gridname = trim(cspval)
      enddo
   endif

   prism_npart = prism_npart + 1
   id_part = prism_npart

   if (prism_npart > mpart) then
      write(nulprt,*) subname,' ERROR prism_npart too large ',prism_npart,mpart
      WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif

   if (kparal(CLIM_Strategy) == CLIM_Serial) then
      nsegs = 1
      allocate(start(nsegs),length(nsegs))
      start (1) = 1
      length(1) = kparal(CLIM_Length)
      numel = nsegs
      if (length(1) == 0) numel = 0
   elseif (kparal(CLIM_Strategy) == CLIM_Apple) then
      nsegs = 1
      allocate(start(nsegs),length(nsegs))
      start (1) = kparal(CLIM_Offset) + 1
      length(1) = kparal(CLIM_Length)
      numel = nsegs
      if (length(1) == 0) numel = 0
   elseif (kparal(CLIM_Strategy) == CLIM_Box) then
      nsegs = kparal(CLIM_SizeY)
      allocate(start(nsegs),length(nsegs))
      do n = 1,nsegs
         start (n) = kparal(CLIM_Offset) + (n-1)*kparal(CLIM_LdX) + 1
         length(n) = kparal(CLIM_SizeX)
      enddo
      numel = nsegs
      if (kparal(CLIM_SizeY)*kparal(CLIM_SizeX) == 0) numel = 0
   elseif (kparal(CLIM_Strategy) == CLIM_Orange) then
      nsegs = kparal(CLIM_Segments)
      allocate(start(nsegs),length(nsegs))
      numel = 0
      do n = 1,nsegs
        len = kparal((n-1)*2 + 4)
        IF (len > 0) THEN
            numel = numel + 1
            start(numel)  = kparal((n-1)*2 + 3) + 1
            length(numel) = len
        ENDIF
      ENDDO 
   elseif (kparal(CLIM_Strategy) == CLIM_Points) then
      nsegs = kparal(CLIM_Segments)
      allocate(start(nsegs),length(nsegs))
      !--- initialize first segment, nsegs=1,n=1,k=3
      nsegs = 1
      n = 1
      k = n+2
      start(nsegs)  = kparal(k)
      length(nsegs) = 1
      !--- compute rest of segments from n=2,k=4
      do n = 2,kparal(CLIM_Segments)
         k = n+2
         if (kparal(k)-kparal(k-1) == 1) then
            length(nsegs) = length(nsegs) + 1
         else
            nsegs = nsegs + 1
            start(nsegs)  = kparal(k)
            length(nsegs) = 1
         endif
      enddo
      numel = nsegs
   else
      write(nulprt,*) subname,' ERROR part strategy unknown ',kparal(CLIM_Strategy)
      WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif

   if (mpi_comm_local /= MPI_COMM_NULL) then
      if (present(ig_size)) then
         call mct_gsmap_init(prism_part(prism_npart)%gsmap,start,length,mpi_root_local,&
                             mpi_comm_local,compid,numel=numel,gsize=ig_size)
      else
         call mct_gsmap_init(prism_part(prism_npart)%gsmap,start,length,mpi_root_local,&
                             mpi_comm_local,compid,numel=numel)
      endif
      prism_part(prism_npart)%gsize = mct_gsmap_gsize(prism_part(prism_npart)%gsmap)
   else
      prism_part(prism_npart)%gsize = -1
   endif

   deallocate(start,length)

   prism_part(prism_npart)%nx = -1
   prism_part(prism_npart)%ny = -1
  
   call oasis_timer_stop('map definition')
   
   if (OASIS_debug >= 2) then
      write(nulprt,*) ' '
      write(nulprt,*) subname,' compid = ',prism_part(prism_npart)%gsmap%comp_id
      write(nulprt,*) subname,' ngseg  = ',prism_part(prism_npart)%gsmap%ngseg
      write(nulprt,*) subname,' gsize  = ',prism_part(prism_npart)%gsmap%gsize
      IF (mpi_comm_local /= MPI_COMM_NULL) THEN
          WRITE(nulprt,*) subname,' start  = ',prism_part(prism_npart)%gsmap%start
          WRITE(nulprt,*) subname,' length = ',prism_part(prism_npart)%gsmap%length
          WRITE(nulprt,*) subname,' pe_loc = ',prism_part(prism_npart)%gsmap%pe_loc
      ENDIF
      write(nulprt,*) ' '
      CALL oasis_flush(nulprt)
   endif

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_def_partition

!------------------------------------------------------------
  SUBROUTINE oasis_part_create(id_part,TYPE,gsize,nx,ny,gridname)

  IMPLICIT NONE

  integer(ip_i4_p),intent(out) :: id_part
  character(len=*),intent(in)  :: type
  integer(ip_i4_p),intent(in)  :: gsize
  integer(ip_i4_p),intent(in)  :: nx
  integer(ip_i4_p),intent(in)  :: ny
  character(len=*),intent(in)  :: gridname
  !--------------------------------------------------------
  integer(ip_i4_p) :: mpicomm
  integer(ip_i4_p) :: mpirank
  integer(ip_i4_p) :: mpisize
  integer(ip_i4_p),pointer :: start(:),length(:)
  integer(ip_i4_p) :: pts
  integer(ip_i4_p) :: n
  character(len=*),parameter :: subname = 'oasis_part_create'
  !--------------------------------------------------------

  call oasis_debug_enter(subname)

  mpicomm = mpi_comm_local
  mpirank = mpi_rank_local
  mpisize = mpi_size_local

  !--- before initializing another gsmap, check if one exists that will work
  do n = 1,prism_npart
     if (prism_part(n)%gsize == gsize .and. &
         trim(prism_part(n)%gridname) == trim(gridname) .and. &
         prism_part(n)%nx == nx .and. &
         prism_part(n)%ny == ny) then
        id_part = n
        call oasis_debug_exit(subname)
        return
     endif
  enddo

  if (trim(type) == '1d') then
     allocate(start(1),length(1))
     length(1) = gsize/mpisize
     pts = gsize - length(1)*mpisize
     if (mpirank < pts) length(1) = length(1) + 1
     start(1) = gsize/mpisize*(mpirank) + min(mpirank,pts) + 1
     prism_npart = prism_npart + 1
     prism_part(prism_npart)%gsize = gsize
     prism_part(prism_npart)%nx = -1
     prism_part(prism_npart)%ny = -1
     call mct_gsmap_init(prism_part(prism_npart)%gsmap,start,length,0,mpicomm,compid)
     deallocate(start,length)
  else
     write(nulprt,*) subname,' ERROR type unknown ',trim(type)
     WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
     CALL oasis_flush(nulprt)
     call oasis_abort_noarg()
  endif

  id_part = prism_npart

  call oasis_debug_exit(subname)

END SUBROUTINE oasis_part_create
!------------------------------------------------------------

END MODULE mod_oasis_part
