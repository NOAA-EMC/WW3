MODULE mod_oasis_method

   USE mod_oasis_kinds
   USE mod_oasis_sys
   USE mod_oasis_data
   USE mod_oasis_parameters
   USE mod_oasis_namcouple
   USE mod_oasis_coupler
   USE mod_oasis_advance
   USE mod_oasis_timer
   USE mod_oasis_ioshr
   USE mod_oasis_grid
   USE mod_oasis_mpi
   USE mod_oasis_string
   USE mct_mod

   IMPLICIT NONE

   private

   public oasis_init_comp
   public oasis_terminate
   public oasis_get_localcomm
   public oasis_set_couplcomm
   public oasis_create_couplcomm
   public oasis_get_debug
   public oasis_set_debug
   public oasis_get_intercomm
   public oasis_get_intracomm
   public oasis_enddef

#ifdef __VERBOSE
   integer(kind=ip_intwp_p),parameter :: debug=2
#else
   integer(kind=ip_intwp_p),parameter :: debug=1
#endif
   logical,save :: lg_mpiflag

CONTAINS

!----------------------------------------------------------------------
   SUBROUTINE oasis_init_comp(mynummod,cdnam,kinfo)

   ! This is COLLECTIVE, all pes must call

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: mynummod     
   CHARACTER(len=*)         ,intent(in)    :: cdnam
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   INTEGER(kind=ip_intwp_p) :: n,nns,iu
   integer(kind=ip_intwp_p) :: icolor,ikey
   CHARACTER(len=ic_med)    :: filename,filename2
   character(len=ic_med)    :: pio_type
   integer(kind=ip_intwp_p) :: pio_stride
   integer(kind=ip_intwp_p) :: pio_root
   integer(kind=ip_intwp_p) :: pio_numtasks
   INTEGER(kind=ip_intwp_p),ALLOCATABLE :: tmparr(:)
   INTEGER(kind=ip_intwp_p) :: k,i,m
   INTEGER(kind=ip_intwp_p) :: nt
   character(len=ic_field)  :: i_name
   character(len=*),parameter :: subname = 'oasis_init_comp'
!  ---------------------------------------------------------

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif
   call oasis_data_zero()

   !------------------------
   !--- Initialize MPI
   !------------------------

   lg_mpiflag = .FALSE.
   CALL MPI_Initialized ( lg_mpiflag, mpi_err )
   IF ( .NOT. lg_mpiflag ) THEN
      if (OASIS_debug >= 0) WRITE (0,FMT='(A)') subname//': Calling MPI_Init'
      CALL MPI_INIT ( mpi_err )
   else
      if (OASIS_debug >= 0) WRITE (0,FMT='(A)') subname//': Not Calling MPI_Init'
   ENDIF

#ifdef use_comm_MPI1
   mpi_comm_global = MPI_COMM_WORLD
#elif defined use_comm_MPI2
   mpi_comm_global = ??
#endif

   CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,mpi_err)

   !------------------------
   !--- nout file, need mpi_rank_global
   !------------------------

   iu=-1

   call oasis_unitsetmin(1024)
   IF (mpi_rank_global == 0) THEN
       CALL oasis_unitget(iu)
       nulprt1 = iu
       WRITE(filename,'(a,i6.6)') 'nout.',mpi_rank_global
       OPEN(nulprt1,file=filename)
   ENDIF

   !------------------------
   !--- Initialize namcouple
   !--- first on rank 0 to write error messages
   !--- then on all other ranks
   !------------------------

   IF (mpi_rank_global == 0) THEN
      call oasis_namcouple_init()
   endif
   call oasis_mpi_barrier(mpi_comm_global)
   IF (mpi_rank_global /= 0) THEN
      call oasis_namcouple_init()
   endif
   OASIS_debug = namlogprt
   TIMER_debug = namtlogprt

   ! If NFIELDS=0 there is no coupling
   ! No information must be written in the debug files as
   ! the different structures are not allocated
   !
   IF ( nnamcpl == 0 ) THEN
       IF (mpi_rank_global == 0) THEN
           WRITE (UNIT = nulprt1,FMT = *) '        ***WARNING***'
           WRITE (UNIT = nulprt1,FMT = *)  &
              ' The models are not exchanging any field ($NFIELDS = 0) '
           WRITE (UNIT = nulprt1,FMT = *)  &
              ' so we force OASIS_debug = 0 for all processors '
           OASIS_debug = 0
           CALL oasis_flush(nulprt1)
       ENDIF
   ENDIF

   ! Determines the total number of fields to avoid a parameter in oasis_def_var
   ! and mod_oasis_coupler
   mvar=0
   DO nns = 1,nnamcpl
     n = namfldsort(nns)
     mvar = mvar + oasis_string_listGetNum(namsrcfld(n))
   ENDDO
   IF (mpi_rank_global == 0) THEN
       WRITE (UNIT = nulprt1,FMT = *) 'Total number of coupling fields :',mvar
       CALL oasis_flush(nulprt1)
   ENDIF

   ALLOCATE(prism_var(mvar))

   ! Store all the names of the fields exchanged in the namcouple
   ! which can be different of namsrcfld(:) and namdstfld(:) if multiple 
   ! fields are exchanged together
   ALLOCATE(total_namsrcfld(mvar))
   ALLOCATE(total_namdstfld(mvar))
   m=0
   DO nns = 1,nnamcpl
     n = namfldsort(nns)
     k=oasis_string_listGetNum(namsrcfld(n))
     DO i=1,k 
       m=m+1
       CALL oasis_string_listGetName(namsrcfld(n),i,i_name)
       total_namsrcfld(m)=trim(i_name)
     ENDDO
   ENDDO
   !
   m=0
   DO nns = 1,nnamcpl
     n = namfldsort(nns)
     k=oasis_string_listGetNum(namdstfld(n))
     DO i=1,k 
       m=m+1
       CALL oasis_string_listGetName(namdstfld(n),i,i_name)
       total_namdstfld(m)=trim(i_name)
     ENDDO
   ENDDO
   DO m=1,mvar
     IF (mpi_rank_global == 0) THEN
         WRITE (UNIT = nulprt1,FMT = *) subname,'Coupling fields  namsrcfld:',&
                                     TRIM(total_namsrcfld(m))
         WRITE (UNIT = nulprt1,FMT = *) subname,'Coupling fields namdstfld:',&
                                     TRIM(total_namdstfld(m))
         CALL oasis_flush(nulprt1)
     ENDIF
   ENDDO

   !
   !------------------------
   !--- Set compid (need namcouple model names)
   !------------------------

   compid = -1
   compnm = trim(cdnam)
   do n = 1,prism_nmodels
      if (trim(cdnam) == trim(prism_modnam(n))) compid = n
   enddo
   mynummod = compid
   IF (mpi_rank_global == 0) THEN
       WRITE(nulprt1,*) subname, 'cdnam :',TRIM(cdnam),' mynummod :',mynummod
       CALL oasis_flush(nulprt1)
   ENDIF

   if (compid < 0) then
       IF (mpi_rank_global == 0) THEN
           WRITE(nulprt1,*) subname,' model not found in namcouple ',&
                            TRIM(cdnam)
           CALL oasis_flush(nulprt1)
       ENDIF
       CALL oasis_abort_noarg()
   endif


   !------------------------
   !--- Re-Set MPI info (need compid for MPI1 COMM_SPLIT)
   !------------------------

#ifdef use_comm_MPI1

   mpi_comm_global = MPI_COMM_WORLD
   ikey = compid
   icolor = compid
   call MPI_COMM_SPLIT(MPI_COMM_WORLD,icolor,ikey,mpi_comm_local,mpi_err)

#elif defined use_comm_MPI2

   mpi_comm_global = ??
   mpi_comm_local = MPI_COMM_WORLD

#endif

!------------------------------------

   CALL MPI_Comm_Size(mpi_comm_global,mpi_size_global,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_global,mpi_rank_global,mpi_err)

   CALL MPI_Comm_Size(mpi_comm_local,mpi_size_local,mpi_err)
   CALL MPI_Comm_Rank(mpi_comm_local,mpi_rank_local,mpi_err)
   mpi_root_local = 0

   !------------------------
   !--- derive mpi_root_global
   !------------------------

   allocate(mpi_root_global(prism_nmodels))
   allocate(tmparr(prism_nmodels))
   tmparr = -1
   do n = 1,prism_nmodels
      if (compid == n .and. &
          mpi_rank_local == mpi_root_local) then
         tmparr(n) = mpi_rank_global
      endif
   enddo
   call oasis_mpi_max(tmparr,mpi_root_global,MPI_COMM_WORLD, &
      string=subname//':mpi_root_global',all=.true.)
   deallocate(tmparr)

   IF (mpi_rank_global == 0) THEN
       DO n = 1,prism_nmodels
         WRITE(nulprt1,*) subname,'   n,prism_model,root = ',&
            n,TRIM(prism_modnam(n)),mpi_root_global(n)
       ENDDO
       CALL oasis_flush(nulprt1)
   ENDIF

   do n = 1,prism_nmodels
      IF (mpi_root_global(n) < 0) THEN
          IF (mpi_rank_global == 0) THEN
              WRITE(nulprt1,*) subname,'   n,prism_model,root = ',&
                 n,TRIM(prism_modnam(n)),mpi_root_global(n)
              WRITE(nulprt1,*) subname,' ERROR: global root invalid, &
                 & check couplcomm for active tasks'
              CALL oasis_flush(nulprt1)
              CALL oasis_abort_noarg()
          ENDIF
      ENDIF
   enddo

#if defined balance
   ! CPP key balance incompatible with OASIS_Debug < 2
   IF ( OASIS_debug < 2 ) THEN
      WRITE (UNIT = nulprt1,FMT = *) '        ***ABORT***'
      WRITE (UNIT = nulprt1,FMT = *)  &
       ' With load balance CPP option (-Dbalance) '
      WRITE (UNIT = nulprt1,FMT = *)  &
       ' you must define a minimum NLOGPRT = 2 '
      CALL oasis_flush(nulprt1)
      CALL oasis_abort_noarg()
   ENDIF
#endif

   IF (mpi_rank_global == 0) CLOSE(nulprt1)

   !------------------------
   !--- debug file
   !------------------------

   iu=-1
   CALL oasis_unitget(iu)

       IF (OASIS_debug <= 1) THEN
           CALL oasis_mpi_bcast(iu,mpi_comm_local,TRIM(subname)//':unit of master',0)
           IF (mpi_rank_local == 0) THEN
               nulprt=iu
               WRITE(filename,'(a,i2.2)') 'debug.root.',compid
               OPEN(nulprt,file=filename)
               WRITE(nulprt,*) subname,' OPEN debug file for root pe, unit :',nulprt
               call oasis_flush(nulprt)
           ELSE
               nulprt=iu+mpi_size_global
               WRITE(filename2,'(a,i2.2)') 'debug_notroot.',compid
               OPEN(nulprt,file=filename2,position='append')
!               WRITE(nulprt,*) subname,' OPEN debug file for not root pe, unit :',nulprt
!               CALL oasis_flush(nulprt)
           ENDIF
       ELSE
           nulprt=iu
           WRITE(filename,'(a,i2.2,a,i6.6)') 'debug.',compid,'.',mpi_rank_local
           OPEN(nulprt,file=filename)
           WRITE(nulprt,*) subname,' OPEN debug file, unit :',nulprt
           CALL oasis_flush(nulprt)
       ENDIF

       IF ( (OASIS_debug == 1) .AND. (mpi_rank_local == 0)) OASIS_debug=10

       IF (OASIS_debug >= 2) THEN
           WRITE(nulprt,*) subname,' model compid ',TRIM(cdnam),compid
           CALL oasis_flush(nulprt)
       ENDIF

   call oasis_debug_enter(subname)

   !------------------------
   !--- PIO
   !------------------------
#if (PIO_DEFINED)
! tcraig, not working as of Oct 2011
   pio_type = 'netcdf'
   pio_stride = -99
   pio_root = -99
   pio_numtasks = -99
   call oasis_ioshr_init(mpi_comm_local,pio_type,pio_stride,pio_root,pio_numtasks)
#endif

   !------------------------
   !--- Timer Initialization
   !------------------------

   ! Allocate timer memory based on mvar
   nt = 7*mvar+30
   call oasis_timer_init (trim(cdnam), trim(cdnam)//'.timers',nt)
   call oasis_timer_start('total after init')

   !------------------------
   !--- Diagnostics
   !------------------------

   if (OASIS_debug >= 2)  then
      write(nulprt,*) subname,' compid         = ',compid
      write(nulprt,*) subname,' compnm         = ',trim(compnm)
      write(nulprt,*) subname,' mpi_comm_world = ',MPI_COMM_WORLD
      write(nulprt,*) subname,' mpi_comm_global= ',mpi_comm_global
      write(nulprt,*) subname,'     size_global= ',mpi_size_global
      write(nulprt,*) subname,'     rank_global= ',mpi_rank_global
      write(nulprt,*) subname,' mpi_comm_local = ',mpi_comm_local
      write(nulprt,*) subname,'     size_local = ',mpi_size_local
      write(nulprt,*) subname,'     rank_local = ',mpi_rank_local
      write(nulprt,*) subname,'     root_local = ',mpi_root_local
      write(nulprt,*) subname,' OASIS_debug    = ',OASIS_debug
      write(nulprt,*) subname,' prism models: '
      call oasis_flush(nulprt)
   endif

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_init_comp

!----------------------------------------------------------------------
   SUBROUTINE oasis_terminate(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   character(len=*),parameter :: subname = 'oasis_terminate'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   call oasis_timer_stop('total after init')
   call oasis_timer_print()

   call oasis_mpi_barrier(mpi_comm_global)
   IF ( .NOT. lg_mpiflag ) THEN
       IF (OASIS_debug >= 2)  THEN
           WRITE (nulprt,FMT='(A)') subname//': Calling MPI_Finalize'
           CALL oasis_flush(nulprt)
       ENDIF
       CALL MPI_Finalize ( mpi_err )
   else
       IF (OASIS_debug >= 2)  THEN
           WRITE (nulprt,FMT='(A)') subname//': Not Calling MPI_Finalize'
           CALL oasis_flush(nulprt)
       ENDIF
   ENDIF

   IF (mpi_rank_local == 0)  THEN
       WRITE(nulprt,*) subname,' SUCCESSFUL RUN'
       CALL oasis_flush(nulprt)
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_terminate

!----------------------------------------------------------------------
   SUBROUTINE oasis_get_localcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_get_localcomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   ! from prism_data
   localcomm = mpi_comm_local
   IF (OASIS_debug >= 2) THEN
       WRITE(nulprt,*) 'localcomm :',localcomm
       CALL oasis_FLUSH(nulprt)
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_get_localcomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_set_couplcomm(localcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: localcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_set_couplcomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   !------------------------
   !--- update mpi_comm_local from component
   !------------------------

   mpi_comm_local = localcomm

   !------------------------
   !--- and now update necessary info
   !------------------------

   mpi_size_local = -1
   mpi_rank_local = -1
   if (mpi_comm_local /= MPI_COMM_NULL) then
      CALL MPI_Comm_Size(mpi_comm_local,mpi_size_local,mpi_err)
      CALL MPI_Comm_Rank(mpi_comm_local,mpi_rank_local,mpi_err)
      mpi_root_local = 0
   endif

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_set_couplcomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_create_couplcomm(icpl,allcomm,cplcomm,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: icpl
   INTEGER (kind=ip_intwp_p),intent(in)   :: allcomm
   INTEGER (kind=ip_intwp_p),intent(out)  :: cplcomm
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer(kind=ip_intwp_p) :: mpi_err
   character(len=*),parameter :: subname = 'oasis_create_couplcomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   !------------------------
   !--- generate cplcomm from allcomm and icpl
   !------------------------

   CALL MPI_COMM_Split(allcomm,icpl,1,cplcomm,mpi_err)
   IF (mpi_err /= 0) THEN
      WRITE (nulprt,*) subname,' ERROR: MPI_Comm_Split abort ',mpi_err
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   ENDIF

   !------------------------
   !--- update mpi_comm_local from component
   !------------------------

   call oasis_set_couplcomm(cplcomm)

   IF (OASIS_debug >= 2)  THEN
       WRITE (nulprt,*) 'New local coupling comm =',cplcomm
       CALL oasis_flush(nulprt)
   ENDIF

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_create_couplcomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_get_debug(debug,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out)   :: debug
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_get_debug'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   debug = OASIS_debug

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_get_debug
!----------------------------------------------------------------------
   SUBROUTINE oasis_set_debug(debug,kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(in)   :: debug
   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_set_debug'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   OASIS_debug = debug
   if (OASIS_debug >= 2) then
      write(nulprt,*) subname,' set OASIS_debug to ',OASIS_debug
      CALL oasis_flush(nulprt)
   endif

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_set_debug
!----------------------------------------------------------------------
   SUBROUTINE oasis_get_intercomm(new_comm, cdnam, kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out) :: new_comm
   CHARACTER(len=*),intent(in) :: cdnam
   INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo

   INTEGER (kind=ip_intwp_p)	:: n, il, ierr, tag
   LOGICAL :: found
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_get_intercomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   found = .false.
   do n = 1,prism_nmodels
      if (trim(cdnam) == trim(prism_modnam(n))) then
         if (found) then
            write(nulprt,*) subname,' ERROR: found same model name twice'
            WRITE(nulprt,*) subname,' abort by model :',compid,&
            ' proc :',mpi_rank_local
            CALL oasis_flush(nulprt)
            call oasis_abort_noarg()
         endif
         il = n
         found = .true.
      endif
   enddo

   if (.not. found) then
      write(nulprt,*) subname,' ERROR: input model name not found'
      WRITE(nulprt,*) subname,' abort by model :',compid,&
      ' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif

   IF (OASIS_debug >= 2) THEN
       WRITE(nulprt,*) subname, 'cdnam :',cdnam,' il :',il, &
                       'mpi_root_global(il) :',mpi_root_global(il),&
                       'mpi_comm_local :',mpi_comm_local
       CALL oasis_flush(nulprt)
   ENDIF

   tag=ICHAR(TRIM(compnm))+ICHAR(TRIM(cdnam))
   CALL mpi_intercomm_create(mpi_comm_local, 0, MPI_COMM_WORLD, &
                             mpi_root_global(il), tag, new_comm, ierr)

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_get_intercomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_get_intracomm(new_comm, cdnam, kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(out) :: new_comm
   CHARACTER(len=*),intent(in) :: cdnam
   INTEGER (kind=ip_intwp_p),intent(out),optional :: kinfo

   INTEGER (kind=ip_intwp_p)	:: tmp_intercomm
   INTEGER (kind=ip_intwp_p)	:: ierr
!  ---------------------------------------------------------
   character(len=*),parameter :: subname = 'oasis_get_intracomm'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   call oasis_get_intercomm(tmp_intercomm, cdnam, kinfo)

   CALL mpi_intercomm_merge(tmp_intercomm,.FALSE., new_comm, ierr)

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_get_intracomm
!----------------------------------------------------------------------
   SUBROUTINE oasis_enddef(kinfo)

   IMPLICIT NONE

   INTEGER (kind=ip_intwp_p),intent(inout),optional :: kinfo
!  ---------------------------------------------------------
   integer (kind=ip_intwp_p) :: n
   integer (kind=ip_intwp_p) :: lkinfo
   character(len=*),parameter :: subname = 'oasis_enddef'
!  ---------------------------------------------------------

   call oasis_debug_enter(subname)
   lkinfo = OASIS_OK

   !------------------------
   !--- write grid info to files one model at a time
   !------------------------

   do n = 1,prism_nmodels
      if (compid == n .and. mpi_rank_local == mpi_root_local) then
         call oasis_write2files()
      endif
      call oasis_mpi_barrier(mpi_comm_global)
   enddo

   !------------------------
   !--- MCT Initialization
   !------------------------

   call mct_world_init(prism_nmodels,mpi_comm_global,mpi_comm_local,compid)
   IF (OASIS_debug >= 2)  THEN
      WRITE(nulprt,*) subname, ' done mct_world_init '
      CALL oasis_flush(nulprt)
   ENDIF

   call oasis_coupler_setup()
   IF (OASIS_debug >= 2)  THEN
      WRITE(nulprt,*) subname, ' done prism_coupler_setup '
      CALL oasis_flush(nulprt)
   ENDIF

   if (mpi_comm_local /= MPI_COMM_NULL) then
      call oasis_advance_init(lkinfo)
      IF (OASIS_debug >= 2)  THEN
         WRITE(nulprt,*) subname, ' done prism_advance_init '
         CALL oasis_flush(nulprt)
      ENDIF
   endif

   !--- Force OASIS_OK here rather than anything else ---

   if (present(kinfo)) then
      kinfo = OASIS_OK
   endif

   call oasis_debug_exit(subname)

 END SUBROUTINE oasis_enddef
!----------------------------------------------------------------------

END MODULE mod_oasis_method
