MODULE mod_oasis_coupler
!     - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
  USE mod_oasis_kinds
  USE mod_oasis_data
  USE mod_oasis_parameters
  USE mod_oasis_namcouple
  USE mod_oasis_sys
  USE mod_oasis_var, only : prism_nvar
  USE mod_oasis_part
  USE mod_oasis_mpi
  USE mod_oasis_string
  USE mod_oasis_io
  USE mod_oasis_timer
  USE mct_mod
  USE grids    ! scrip
  USE netcdf

  IMPLICIT NONE

  private

  public oasis_coupler_setup

! Type of data

  public prism_router_type
  public prism_mapper_type
  public prism_coupler_type

  !--- derived ---
  INTEGER(kind=ip_i4_p),pointer :: model_root(:)

! COUPLING INFO

  type prism_router_type
     !--- fixed at initialization ---
     type(mct_router)      :: router     ! router
  end type prism_router_type

  type prism_mapper_type
     !--- fixed at initialization ---
     type(mct_sMatP),pointer :: sMatP(:)  ! mappers, size nwgts
     integer(kind=ip_i4_p) :: nwgts       ! number of weights in weights file
     character(len=ic_long):: file
     character(len=ic_med) :: loc         ! location: src,dst
     character(len=ic_med) :: opt         ! optimization: bfb,sum,opt
     character(len=ic_med) :: optval      ! mct map option: src,dst
     logical               :: init
     integer(kind=ip_i4_p) :: spart ! src partition
     integer(kind=ip_i4_p) :: dpart ! dst partition
     logical               :: AVred ! AV_ms, AV_md data already read in
     type(mct_aVect)       :: AV_ms ! av for CONSERV src: mask, area, etc
     type(mct_aVect)       :: AV_md ! av for CONSERV dst: mask, area, etc
  end type prism_mapper_type

  integer(kind=ip_i4_p),public,parameter :: prism_coupler_avsmax=5

  type prism_coupler_type
     !--- fixed at initialization ---
     type(mct_aVect)       :: aVect1   ! primary aVect
     type(mct_aVect)       :: aVect1m  ! extra aVect needed for mapping
     type(mct_aVect)       :: aVect2   ! aVects 2-5 handle higher order mapping
     type(mct_aVect)       :: aVect3   ! 
     type(mct_aVect)       :: aVect4   ! 
     type(mct_aVect)       :: aVect5   ! 
     logical               :: aVon(prism_coupler_avsmax)  ! flags indicating whether aVects 2-5 are active
     character(len=ic_xl)  :: rstfile  ! restart file
     character(len=ic_xl)  :: inpfile  ! restart file
     character(len=ic_xl)  :: fldlist  ! field list
     integer(kind=ip_i4_p) :: nflds    ! number of fields
     integer(kind=ip_i4_p),pointer :: varid(:)    ! varid for each field
     integer(kind=ip_i4_p) :: namID    ! namcouple ID
     integer(kind=ip_i4_p) :: partID   ! associated partition ID
     integer(kind=ip_i4_p) :: routerID ! router ID
     integer(kind=ip_i4_p) :: mapperID ! mapper ID
     character(len=ic_med) :: maploc   ! map location: src,dst
     integer(kind=ip_i4_p) :: ops      ! namcouple operation (ip_exported,...)
     integer(kind=ip_i4_p) :: comp     ! other model compid
     integer(kind=ip_i4_p) :: tag      ! comm tag
     integer(kind=ip_i4_p) :: seq      ! sequence number
     integer(kind=ip_i4_p) :: dt       ! coupling period (secs)
     integer(kind=ip_i4_p) :: lag      ! put lag += put sooner (secs)
     integer(kind=ip_i4_p) :: maxtime  ! max time for the coupler
     integer(kind=ip_i4_p) :: trans    ! transformation (ip_average,...)
     integer(kind=ip_i4_p) :: conserv  ! conserve operation (ip_cnone,ip_cglobal,...)
     character(len=ic_med) :: consopt  ! conserve option (bfb, opt)
     integer(kind=ip_i4_p) :: getput   ! get/put flag
     logical               :: sndrcv   ! send recv flag
     logical               :: output   ! output flag
     logical               :: input    ! input flag
     logical               :: snddiag  ! diagnose src fields as part of coupling
     logical               :: rcvdiag  ! diagnose rcv fields as part of coupling
     real(kind=ip_double_p):: sndmult  ! field multiplier term
     real(kind=ip_double_p):: sndadd   ! field addition term
     real(kind=ip_double_p):: rcvmult  ! field multiplier term
     real(kind=ip_double_p):: rcvadd   ! field addition term
     !--- time varying info ---
     integer(kind=ip_i4_p) :: ltime    ! time at last coupling
     integer(kind=ip_i4_p),pointer :: avcnt(:)  ! counter for averaging
     integer(kind=ip_i4_p),pointer :: status(:) ! status of variables in coupler
  end type prism_coupler_type

  integer(kind=ip_i4_p)           :: prism_mrouter   ! max routers
  integer(kind=ip_i4_p)           :: prism_nrouter = 0
  type(prism_router_type) ,public, pointer:: prism_router(:)

  integer(kind=ip_i4_p)           :: prism_mmapper   ! max mappers
  integer(kind=ip_i4_p)           :: prism_nmapper = 0
  type(prism_mapper_type) ,public, pointer :: prism_mapper(:)

  integer(kind=ip_i4_p)           :: prism_mcoupler   ! max couplers
  integer(kind=ip_i4_p)   ,public :: prism_ncoupler = 0
  type(prism_coupler_type),public, pointer :: prism_coupler(:)

  integer(kind=ip_i4_p)   ,public :: lcouplerid    ! last coupler id
  integer(kind=ip_i4_p)   ,public :: lcouplertime  ! last coupler time 


!#include <netcdf.inc>

!------------------------------------------------------------
CONTAINS
!------------------------------------------------------------

  SUBROUTINE oasis_coupler_setup()

  !----------------------------------------------------------
  ! This routine reconciles the coupling stuff
  !----------------------------------------------------------

  IMPLICIT none

  integer(kind=ip_i4_p) :: n,n1,nn,nv,nm,nv1,nns,lnn,nc
  integer(kind=ip_i4_p) :: pe
  integer(kind=ip_i4_p) :: part1, part2
  integer(kind=ip_i4_p) :: spart,dpart,rpart ! src, dst, router partition id
        ! part1 = my local part
        ! part2 = other part
        ! spart = src part for mapping; put=part1, get=part2
        ! dpart = dst part for mapping; put=part2, get=part1
        ! rpart = part used for router init
  integer(kind=ip_i4_p) :: mapID,namID
  type(mct_sMat),pointer :: sMati(:)
  integer(kind=ip_i4_p) :: ncid,dimid,status
  integer(kind=ip_i4_p) :: lsize,gsize
  integer(kind=ip_i4_p) :: svarid
  integer(kind=ip_i4_p),allocatable :: varidtmp(:)
  integer(kind=ip_i4_p) :: part
  character(len=ic_lvar):: cstring
  character(len=ic_lvar):: myfld
  integer(kind=ip_i4_p) :: myfldi
  character(len=ic_lvar):: otfld
  character(len=ic_lvar):: otfldmatch
  integer(kind=ip_i4_p) :: otfldi
  integer(kind=ip_i4_p) :: nx,ny
  character(len=ic_lvar):: gridname
  character(len=ic_long):: tmp_mapfile
  integer(kind=ip_i4_p) :: flag
  logical               :: found, exists
  integer(kind=ip_i4_p) :: mynvar
  integer(kind=ip_i4_p) :: nwgts
  character(len=ic_lvar),pointer :: myvar(:)
  integer(kind=ip_i4_p) ,pointer :: myops(:)
  integer(kind=ip_i4_p) ,pointer :: nallvar(:)
  character(len=ic_lvar),pointer :: allvar(:,:)
  integer(kind=ip_i4_p) ,pointer :: allops(:,:)
  character(len=*),parameter :: subname = 'oasis_coupler_setup'

  !----------------------------------------------------------

  !----------------------------------------------------------
  ! Figure out the global rank of each model's root
  !----------------------------------------------------------

  call oasis_debug_enter(subname)
  call oasis_timer_start('cpl_setup')

  allocate(model_root(prism_nmodels))
  model_root = -99
  do n = 1,prism_nmodels
     pe = -1
     if (compid == n .and. mpi_rank_local == 0) pe = mpi_rank_global
     call oasis_mpi_max(pe,model_root(n),mpi_comm_global, &
        subname//':cg',.true.)
  enddo

  if ( OASIS_debug >= 2 ) write(nulprt,*) subname,' model_root = ',&
                                          model_root(1:prism_nmodels)

  ! allocate prism_router, prism_mapper, prism_coupler based on nnamcpl
  ! there cannot be more than that needed

  lnn=-1
  call oasis_debug_note(subname//' set defaults for datatypes')

  prism_mrouter = nnamcpl
  allocate(prism_router(prism_mrouter))
  prism_nrouter = 0

  prism_mmapper = nnamcpl
  allocate(prism_mapper(prism_mmapper))
  prism_nmapper = 0
  prism_mapper(:)%nwgts = 0
  prism_mapper(:)%file  = ""
  prism_mapper(:)%loc   = ""
  prism_mapper(:)%opt   = ""
  prism_mapper(:)%optval= ""
  prism_mapper(:)%init  = .false.
  prism_mapper(:)%spart = ispval
  prism_mapper(:)%dpart = ispval
  prism_mapper(:)%AVred = .false.

  prism_mcoupler = nnamcpl
  allocate(prism_coupler(prism_mcoupler))
  prism_ncoupler = 0
  prism_coupler(:)%rstfile = ""
  prism_coupler(:)%inpfile = ""
  prism_coupler(:)%fldlist = ""
  prism_coupler(:)%nflds   = 0
  do nc = 1,nnamcpl
     allocate(prism_coupler(nc)%varid(1))
     prism_coupler(nc)%varid(:) = ispval
     prism_coupler(nc)%aVon(:) = .false.
  enddo
  if (OASIS_debug >= 2) write(nulprt,*) subname,' initialize %varid ',&
                                        nnamcpl,size(prism_coupler(nnamcpl)%varid)
  prism_coupler(:)%ops     = ispval
  prism_coupler(:)%comp    = ispval
  prism_coupler(:)%routerID  = ispval
  prism_coupler(:)%mapperID  = ispval
  prism_coupler(:)%maploc  = ""
  prism_coupler(:)%tag     = ispval
  prism_coupler(:)%dt      = ispval
  prism_coupler(:)%lag     = 0
  prism_coupler(:)%maxtime = 0
  prism_coupler(:)%getput  = ispval
  prism_coupler(:)%sndrcv  = .false.
  prism_coupler(:)%output  = .false.
  prism_coupler(:)%input   = .false.
  prism_coupler(:)%trans   = ip_instant
  prism_coupler(:)%conserv = ip_cnone
  prism_coupler(:)%ltime   = ispval
  prism_coupler(:)%snddiag = .false.
  prism_coupler(:)%rcvdiag = .false.
  prism_coupler(:)%sndmult = 1.0_ip_double_p
  prism_coupler(:)%sndadd  = 0.0_ip_double_p
  prism_coupler(:)%rcvmult = 1.0_ip_double_p
  prism_coupler(:)%rcvadd  = 0.0_ip_double_p

  lcouplerid   = ispval
  lcouplertime = ispval

  !----------------------------------------------------------
  ! Share model variable information across all models
  !----------------------------------------------------------

  call oasis_debug_note(subname//' share var info between models')

  allocate(allvar(mvar,prism_nmodels))
  allocate(nallvar(prism_nmodels))
  allocate(allops(mvar,prism_nmodels))
  allocate(myvar(mvar))
  allocate(myops(mvar))

  allvar = " "
  nallvar = 0
  allops = -1
  do n = 1,prism_nmodels
     if (n == compid) then
        myvar = " "
        myops = 0
        mynvar = prism_nvar
        do n1 = 1, prism_nvar
           myvar(n1) = trim(prism_var(n1)%name)
           myops(n1) = prism_var(n1)%ops
        enddo
     endif
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' BCAST from ',n,model_root(n)
        call oasis_flush(nulprt)
     endif
     call oasis_mpi_bcast(mynvar,mpi_comm_global,'mynvar',model_root(n))
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' bcast mynvar ',mynvar
        call oasis_flush(nulprt)
     endif
     nallvar(n) = mynvar
     call oasis_mpi_bcast(myvar,mpi_comm_global,'myvar',model_root(n))
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' bcast myvar ',trim(myvar(1))
        call oasis_flush(nulprt)
     endif
     allvar(:,n) = myvar(:)
     call oasis_mpi_bcast(myops,mpi_comm_global,'myops',model_root(n))
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' bcast myops ',myops(1)
        call oasis_flush(nulprt)
     endif
     allops(:,n) = myops(:)
  enddo

  deallocate(model_root)
  deallocate(myvar,myops)

  if (OASIS_debug >= 2) then
     write(nulprt,*) subname,' model variable info:'
     do nm = 1,prism_nmodels
        write(nulprt,'(8x,a,2i6)') ' model,nvars = ',nm,nallvar(nm)
        do nv = 1,nallvar(nm)
           cstring = 'unknown'
           if (allops(nv,nm) == OASIS_Out) cstring = 'prism_out'
           if (allops(nv,nm) == OASIS_In)  cstring = 'prism_in'
           write(nulprt,'(16x,a,2i6,2x,a,i6,2x,a)') ' model,idx,var,ops = ',nm,nv,&
                                                      trim(allvar(nv,nm)),allops(nv,nm),&
                                                      trim(cstring)
        enddo
     enddo
     write(nulprt,*) ' '
     call oasis_flush(nulprt)
  endif

  !----------------------------------------------------------
  ! Setup couplers based on namcouple and model variable info
  ! These must be paired up consistently, create couplers in
  ! sorted order (nns)
  ! nn = namcpl counter, sorted
  ! nm = model counter, compid is my nm
  ! nv = variable counter
  ! nv1 = my variable counter
  !----------------------------------------------------------

  prism_ncoupler = 0

  !--------------------------------
  ! for all namcoupler input
  !--------------------------------

  call oasis_debug_note(subname//' compare vars and namcouple')
  call oasis_debug_note(subname//' setup couplers')

  do nns = 1,nnamcpl
     nn = namfldsort(nns)

     !--- tcx require for run time error on corail ????----
     call oasis_mpi_barrier(mpi_comm_global)
     IF (OASIS_debug >= 2) THEN
         WRITE(nulprt,*) subname,' check nam ',nns
         CALL oasis_flush(nulprt)
     ENDIF

     !--------------------------------
     ! for all my variables
     !--------------------------------

     do nv1 = 1,nallvar(compid)

        !--- tcx require for run time error on corail ????----
        IF (OASIS_debug >= 2) THEN
           WRITE(nulprt,*) subname,' check var ',nns,nv1
           CALL oasis_flush(nulprt)
        ENDIF

        !--------------------------------
        ! get my parition and fld
        !--------------------------------

        part1  = prism_var(nv1)%part
        myfld  = trim(allvar(nv1,compid))

        IF (OASIS_debug >= 20) THEN
            WRITE(nulprt,*) subname,' get part and fld ',part1,trim(myfld)
            CALL oasis_flush(nulprt)
        ENDIF

        !--------------------------------
        ! check if i'm an In or Out variable
        ! check if my variable matches this namcouple dst or src
        !--------------------------------
        flag = OASIS_NotDef
        if (allops(nv1,compid) == OASIS_Out) then
           myfldi = oasis_string_listGetIndexF(namsrcfld(nn),myfld)
           if (myfldi > 0) flag = OASIS_Out
        elseif (allops(nv1,compid) == OASIS_In) then
           myfldi = oasis_string_listGetIndexF(namdstfld(nn),myfld)
           if (myfldi > 0) flag = OASIS_In
        endif

        IF (OASIS_debug >= 20) THEN
            WRITE(nulprt,*) subname,' check fld ',myfldi,flag
            CALL oasis_flush(nulprt)
        ENDIF

        !--------------------------------
        ! my variable is in this namcouple input
        !--------------------------------

        if (flag /= OASIS_NotDef) then

           !--------------------------------
           ! migrate namcouple info into part
           !--------------------------------

           IF (OASIS_debug >= 20) THEN
               WRITE(nulprt,*) subname,' migrate namcouple '
               CALL oasis_flush(nulprt)
           ENDIF

           if (flag == OASIS_In) then
              if (prism_part(part1)%nx < 1) then
                 prism_part(part1)%nx = namdst_nx(nn)
                 prism_part(part1)%ny = namdst_ny(nn)
                 prism_part(part1)%gridname = trim(namdstgrd(nn))
              endif
           endif
           if (flag == OASIS_Out) then
              if (prism_part(part1)%nx < 1) then
                 prism_part(part1)%nx = namsrc_nx(nn)
                 prism_part(part1)%ny = namsrc_ny(nn)
                 prism_part(part1)%gridname = trim(namsrcgrd(nn))
              endif
           endif

           !--------------------------------
           ! make sure it's either an In or Out, sanity check
           !--------------------------------

           if (flag /= OASIS_In .and. flag /= OASIS_Out) then
              write(nulprt,*) subname,' ERROR flag problems',flag
              WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
              CALL oasis_flush(nulprt)
              call oasis_abort_noarg()
           endif

           if (OASIS_debug >= 5) then
              write(nulprt,'(1x,2a,4i6,2a)') subname,' ca: myfld',nn,compid,&
                                             nv1,myfldi,' ',trim(myfld)
              call oasis_flush(nulprt)
           endif

           !--------------------------------
           ! determine other field
           !--------------------------------

           otfldmatch = 'NOmatchNOyesNOyesNO'
           if (flag == OASIS_Out) then
              call oasis_string_listGetName(namdstfld(nn),myfldi,otfldmatch)
           endif
           if (flag == OASIS_In) then
              call oasis_string_listGetName(namsrcfld(nn),myfldi,otfldmatch)
           endif

           !--------------------------------
           ! look for namcouple coupling variable in all other model variables
           !--------------------------------

           found = .false.
           do nm = 1,prism_nmodels
           do nv = 1,nallvar(nm)

              !--- tcx require for run time error on corail ????----
              IF (OASIS_debug >= 2) THEN
                  WRITE(nulprt,*) subname,' check mod ',nns,nv1,nm,nv
                  CALL oasis_flush(nulprt)
              ENDIF

              otfld  = trim(allvar(nv,nm))
              otfldi = -1

              !--------------------------------
              ! matches if the var field and nam field are the same
              !--------------------------------

              if (trim(otfldmatch) == trim(otfld)) then

                 if (OASIS_debug >= 2) write(nulprt,*) subname,' check fld ',&
                                                       nns,nv1,nm,nv,otfldi

                 if (OASIS_debug >= 5) then
                    write(nulprt,'(1x,2a,4i6,2a)') subname,' ca: otfld',nn,nm,&
                                                   nv,otfldi,' ',trim(otfld)
                    call oasis_flush(nulprt)
                 endif

                 !--------------------------------
                 ! Do not allow src and dst to be on same model for communication
                 ! Make sure one side is In and other side is Out for communication
                 ! If input or output, field name should match
                 !--------------------------------

                 if (namfldops(nn) == ip_exported .or. namfldops(nn) == ip_expout) then
                    if (nm == compid) then
                       write(nulprt,*) subname,' ERROR send recv pair on same model',nm,' ', &
                          trim(myfld),' ',trim(otfld)
                       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                       CALL oasis_flush(nulprt)
                       call oasis_abort_noarg()
                    endif
                    if (flag == OASIS_Out .and. allops(nv,nm) /= OASIS_In) then
                       write(nulprt,*) subname,' ERROR send recv pair both Out ', &
                          trim(myfld),' ',trim(otfld)
                       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                       CALL oasis_flush(nulprt)
                       call oasis_abort_noarg()
                    endif
                    if (flag == OASIS_In .and. allops(nv,nm) /= OASIS_Out) then
                       write(nulprt,*) subname,' ERROR send recv pair both In ', &
                          trim(myfld),' ',trim(otfld)
                       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                       CALL oasis_flush(nulprt)
                       call oasis_abort_noarg()
                    endif
                 endif

                 if (namfldops(nn) == ip_input .or. namfldops(nn) == ip_output) then
                    if (trim(myfld) /= trim(otfld)) then
                       write(nulprt,*) subname,' ERROR namcouple field names to not match for in/out', &
                          trim(myfld),' ',trim(otfld)
                       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                       CALL oasis_flush(nulprt)
                       call oasis_abort_noarg()
                    endif
                 endif

                 !--------------------------------
                 ! Only an error to find two sources for a destination
                 ! Not an error if a two destinations have a single source
                 !--------------------------------

                 if (flag == OASIS_In .and. found) then
                    write(nulprt,*) subname,' ERROR found two sources for ',trim(otfld)
                    WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                    CALL oasis_flush(nulprt)
                    call oasis_abort_noarg()
                 endif
                 found = .true.

                 !--------------------------------
                 ! only increment prism_ncoupler if new nn (coupler), otherwise use prior prism_ncoupler
                 ! this supports multiple fields coupled in one communication step
                 !--------------------------------

                 if (nn /= lnn) prism_ncoupler = prism_ncoupler + 1
                 lnn = nn
                 if (prism_ncoupler > prism_mcoupler) then
                    write(nulprt,*) subname,' ERROR prism_ncoupler too large',prism_ncoupler,prism_mcoupler
                    WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                    CALL oasis_flush(nulprt)
                    call oasis_abort_noarg()
                 endif
                 nc = prism_ncoupler

                 !--------------------------------
                 ! prism_coupler fields, multiple field support
                 !--------------------------------

                 IF (OASIS_debug >= 20) THEN
                     WRITE(nulprt,*) subname,' set prism_coupler '
                     CALL oasis_flush(nulprt)
                 ENDIF

                 prism_coupler(nc)%nflds = prism_coupler(nc)%nflds + 1
                 if (prism_coupler(nc)%nflds == 1) then
                    prism_coupler(nc)%fldlist = trim(myfld)
                 else
                    prism_coupler(nc)%fldlist = trim(prism_coupler(nc)%fldlist)//':'//trim(myfld)
                 endif

                 svarid = size(prism_coupler(nc)%varid)
                 if (prism_coupler(nc)%nflds > svarid) then
                     allocate(varidtmp(svarid))
                     varidtmp(1:svarid) = prism_coupler(nc)%varid(1:svarid)
                     deallocate(prism_coupler(nc)%varid)
                     allocate(prism_coupler(nc)%varid(prism_coupler(nc)%nflds+10))
                     prism_coupler(nc)%varid(1:svarid) = varidtmp(1:svarid)
                     deallocate(varidtmp)
                 endif
                 prism_coupler(nc)%varid(prism_coupler(nc)%nflds) = nv1

                 !--------------------------------
                 ! prism_coupler other settings
                 !--------------------------------

                 prism_coupler(nc)%comp   = nm
                 prism_coupler(nc)%seq    = namfldseq(nn)
                 prism_coupler(nc)%dt     = namflddti(nn)
                 prism_coupler(nc)%lag    = namfldlag(nn)
                 prism_coupler(nc)%maxtime= namruntim
                 prism_coupler(nc)%rstfile= trim(namrstfil(nn))
                 prism_coupler(nc)%inpfile= trim(naminpfil(nn))
                 prism_coupler(nc)%mapperID = -1
                 prism_coupler(nc)%partID = part1
                 prism_coupler(nc)%namID  = nn
                 prism_coupler(nc)%trans  = namfldtrn(nn)
                 prism_coupler(nc)%conserv= namfldcon(nn)
                 prism_coupler(nc)%consopt= namfldcoo(nn)
                 prism_coupler(nc)%ops    = namfldops(nn)
                 prism_coupler(nc)%tag    = compid*100*1000 + compid*1000 + nn
                 prism_coupler(nc)%getput = OASIS_NotDef
                 prism_coupler(nc)%sndrcv = .false.
                 prism_coupler(nc)%output = .false.
                 prism_coupler(nc)%input  = .false.
                 prism_coupler(nc)%sndmult= namfldsmu(nn)
                 prism_coupler(nc)%sndadd = namfldsad(nn)
                 prism_coupler(nc)%rcvmult= namflddmu(nn)
                 prism_coupler(nc)%rcvadd = namflddad(nn)
                 prism_coupler(nc)%snddiag= namchecki(nn)
                 prism_coupler(nc)%rcvdiag= namchecko(nn)

                 !--------------------------------
                 ! prism_coupler input and output flags
                 ! prism_coupler comm flags, need for tags to match up on both sides
                 ! tags assume up to 1000 namcouple inputs and 100 models
                 !--------------------------------

                 IF (OASIS_debug >= 20) THEN
                     WRITE(nulprt,*) subname,' inout flags '
                     CALL oasis_flush(nulprt)
                 ENDIF

                 if (namfldops(nn) == ip_output .or. namfldops(nn) == ip_expout) then
                    prism_coupler(nc)%output = .true.
                    prism_coupler(nc)%getput = OASIS3_PUT
                 endif
                 if (namfldops(nn) == ip_input) then
                    prism_coupler(nc)%input  = .true.
                    prism_coupler(nc)%getput = OASIS3_GET
                 endif

                 if (namfldops(nn) == ip_exported .or. namfldops(nn) == ip_expout) then
                    prism_coupler(nc)%sndrcv = .true.
                    if (flag == OASIS_Out) then
                       prism_coupler(nc)%tag = nm*100*1000 + compid*1000 + nn
                       prism_coupler(nc)%getput = OASIS3_PUT
                    elseif (flag == OASIS_In) then
                       prism_coupler(nc)%tag = compid*100*1000 + nm*1000 + nn
                       prism_coupler(nc)%getput = OASIS3_GET
                    endif
                    !--------------------------------
                    ! prism_coupler router
                    ! cannot reuse router because don't really know what's on the other side
                    !--------------------------------
                    if (prism_coupler(nc)%nflds == 1) prism_nrouter = prism_nrouter + 1
                    if (prism_nrouter > prism_mrouter) then
                       write(nulprt,*) subname,' ERROR prism_nrouter too large',prism_nrouter,&
                                       prism_mrouter
                       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                       CALL oasis_flush(nulprt)
                       call oasis_abort_noarg()
                    endif
                    prism_coupler(nc)%routerID = prism_nrouter
                 endif

                 !--------------------------------
                 ! prism_coupler mapper
                 !--------------------------------

                 IF (OASIS_debug >= 20) THEN
                     WRITE(nulprt,*) subname,' mapper '
                     CALL oasis_flush(nulprt)
                 ENDIF

                 tmp_mapfile = nammapfil(nn)

                 if (trim(tmp_mapfile) == 'idmap' .and. trim(namscrmet(nn)) /= trim(cspval)) then
                    if (trim(namscrmet(nn)) == 'CONSERV') then
                       tmp_mapfile = 'rmp_'//trim(namsrcgrd(nn))//'_to_'//trim(namdstgrd(nn))//&
                                     &'_'//trim(namscrmet(nn))//'_'//trim(namscrnor(nn))//'.nc'
                    else
                       tmp_mapfile = 'rmp_'//trim(namsrcgrd(nn))//'_to_'//trim(namdstgrd(nn))//&
                                     &'_'//trim(namscrmet(nn))//'.nc'
                    endif
                 endif

                 if (trim(tmp_mapfile) /= 'idmap') then
                 prism_coupler(nc)%maploc = trim(nammaploc(nn))
                 if ((flag == OASIS_In  .and. trim(nammaploc(nn)) == 'dst') .or. &
                     (flag == OASIS_Out .and. trim(nammaploc(nn)) == 'src')) then
                    !--------------------------------
                    ! try to reuse mapper already defined
                    ! must match mapping file and partition
                    !--------------------------------
                    mapID = -1
                    do n = 1,prism_nmapper
                       if (trim(prism_mapper(n)%file) == trim(tmp_mapfile) .and. &
                           trim(prism_mapper(n)%opt ) == trim(nammapopt(nn))) then
                          if (flag == OASIS_In  .and. prism_mapper(n)%dpart == part1) mapID = n
                          if (flag == OASIS_Out .and. prism_mapper(n)%spart == part1) mapID = n
                       endif
                    enddo
                    !--------------------------------
                    ! or will need a new mapper
                    !--------------------------------
                    if (mapID < 1) then
                       prism_nmapper = prism_nmapper + 1
                       if (prism_nmapper > prism_mmapper) then
                          write(nulprt,*) subname,' ERROR prism_nmapper too large',prism_nmapper,&
                                          prism_mmapper
                          WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                          CALL oasis_flush(nulprt)
                          call oasis_abort_noarg()
                       endif
                       mapID = prism_nmapper
                       prism_mapper(mapID)%file = trim(tmp_mapfile)
                       prism_mapper(mapID)%loc  = trim(nammaploc(nn))
                       prism_mapper(mapID)%opt  = trim(nammapopt(nn))
                       if (flag == OASIS_In ) prism_mapper(mapID)%dpart = part1
                       if (flag == OASIS_Out) prism_mapper(mapID)%spart = part1
                       if (OASIS_debug > 15) then
                          write(nulprt,*) subname,' DEBUG new mapper for file ',&
                                          trim(prism_mapper(mapID)%file)
                          call oasis_flush(nulprt)
                       endif
                    endif
                    prism_coupler(nc)%mapperID = mapID
                 endif  ! flag and nammaploc match
                 endif  ! nammapfil

                 !--------------------------------
                 ! add this coupler to list of prism_var couplers
                 !--------------------------------

                 prism_var(nv1)%ncpl = prism_var(nv1)%ncpl + 1
                 prism_var(nv1)%cpl(prism_var(nv1)%ncpl) = nc

              endif  ! other var in this namcouple

           enddo  ! nv
           enddo  ! nm

        endif  ! my var in this namcouple

     enddo  ! nv1
  enddo  ! nns

  deallocate(allvar,nallvar,allops)

  if (OASIS_debug >= 20) then
     write(nulprt,*) ' '
     write(nulprt,*) subname,' couplers setup'
     do nc = 1,prism_ncoupler
!tcx can't write here, something uninitialized???
!        call prism_coupler_print(nc)
     enddo
     write(nulprt,*) ' '
     call oasis_flush(nulprt)
  endif

  if (mpi_comm_local == MPI_COMM_NULL) return

  !----------------------------------------------------------
  ! Initialize coupling infrastructure based on couplers above
  !----------------------------------------------------------

  call oasis_debug_note(subname//' initialize coupling datatypes')

  do nc = 1,prism_ncoupler
     if (OASIS_debug >= 5) then
        write(nulprt,*) subname,' DEBUG ci:initialize coupler ',nc
        call oasis_flush(nulprt)
     endif

     namID = prism_coupler(nc)%namID
     part1 = prism_coupler(nc)%partID
     mapID = prism_coupler(nc)%mapperID

     if (part1 <= 0) then
        write(nulprt,*) subname,' ERROR part1 invalid ',part1
        WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
        CALL oasis_flush(nulprt)
        call oasis_abort_noarg()
     endif

     !--------------------------------
     ! initialize avect1
     !--------------------------------

     gsize = mct_gsmap_gsize(prism_part(part1)%gsmap)
     lsize = mct_gsmap_lsize(prism_part(part1)%gsmap,mpi_comm_local)
     if (OASIS_debug >= 15) then
        write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part1 info ',part1,mapID,gsize,lsize
        write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part1a',prism_part(part1)%gsmap%ngseg,&
                                     prism_part(part1)%gsmap%gsize
        do n1 = 1,prism_part(part1)%gsmap%ngseg
           write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part1b',n1,&
                                        prism_part(part1)%gsmap%start(n1),&
                                        prism_part(part1)%gsmap%length(n1),&
                                        prism_part(part1)%gsmap%pe_loc(n1)
        enddo
        call oasis_flush(nulprt)
     endif
     call mct_avect_init(prism_coupler(nc)%avect1,rList=trim(prism_coupler(nc)%fldlist),lsize=lsize)
     call mct_avect_zero(prism_coupler(nc)%avect1)
     prism_coupler(nc)%aVon(1) = .true.
     if (OASIS_debug >= 15) then
        write(nulprt,*) subname,' DEBUG ci:avect1 initialized '
        call oasis_flush(nulprt)
     endif

     !--------------------------------
     ! compute nflds for this coupling and initialize avcnt and status
     !--------------------------------

     prism_coupler(nc)%nflds = mct_aVect_nRAttr(prism_coupler(nc)%avect1)
     allocate(prism_coupler(nc)%status(prism_coupler(nc)%nflds))
     allocate(prism_coupler(nc)%avcnt (prism_coupler(nc)%nflds))
     prism_coupler(nc)%avcnt(:) = 0
     if (prism_coupler(nc)%getput == OASIS3_PUT) prism_coupler(nc)%status = OASIS_COMM_WAIT
     if (prism_coupler(nc)%getput == OASIS3_GET) prism_coupler(nc)%status = OASIS_COMM_READY

     !--------------------------------
     ! initialize mapper
     !--------------------------------

     if (mapID > 0) then

        if (prism_mapper(mapID)%init) then
           !--------------------------------
           ! mapper already initialized
           !--------------------------------
           if (prism_coupler(nc)%getput == OASIS3_PUT) then
              part2 = prism_mapper(mapID)%dpart
           else
              part2 = prism_mapper(mapID)%spart
           endif
           gsize = mct_gsmap_gsize(prism_part(part2)%gsmap)
        else
           !--------------------------------
           ! instantiate mapper
           ! read/generate mapping file
           ! read non local grid size
           ! get gsmap for non local grid
           ! read mapping weights and initialize sMatP
           !--------------------------------
           if (OASIS_debug >= 15) then
              write(nulprt,*) subname,' DEBUG ci:read mapfile ',trim(prism_mapper(mapID)%file)
              call oasis_flush(nulprt)
           endif
           if (mpi_rank_local == 0) then
              inquire(file=trim(prism_mapper(mapID)%file),exist=exists)
              if (OASIS_debug >= 15) then
                 write(nulprt,*) subname,' DEBUG ci: inquire mapfile',&
                                 trim(prism_mapper(mapID)%file),exists
                 call oasis_flush(nulprt)
              endif
              if (.not.exists) then
                 if (trim(namscrmet(namID)) /= trim(cspval)) then
                    !--------------------------------
                    ! generate mapping file on root pe
                    ! taken from oasis3 scriprmp
                    !--------------------------------
                    call oasis_coupler_genmap(mapID,namID)
                 else
                    write(nulprt,*) subname,' ERROR map file does not exist and SCRIPR not set ',&
                                    trim(prism_mapper(mapID)%file)
                    WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
                    CALL oasis_flush(nulprt)
                    call oasis_abort_noarg()
                 endif
              endif

              !--------------------------------
              ! open mapping file
              !--------------------------------
              status = nf90_open(trim(prism_mapper(mapID)%file),nf90_nowrite,ncid)
              if (prism_coupler(nc)%getput == OASIS3_PUT) &
                 status = nf90_inq_dimid(ncid,'dst_grid_size',dimid)
              if (prism_coupler(nc)%getput == OASIS3_GET) &
                 status = nf90_inq_dimid(ncid,'src_grid_size',dimid)
              status = nf90_inquire_dimension(ncid,dimid,len=gsize)
           endif
           call oasis_mpi_bcast(gsize,mpi_comm_local,subname//' gsize')

           if (prism_coupler(nc)%getput == OASIS3_PUT) then
              nx = namdst_nx(namID)
              ny = namdst_ny(namID)
              gridname = trim(namdstgrd(namID))
           else
              nx = namsrc_nx(namID)
              ny = namsrc_ny(namID)
              gridname = trim(namsrcgrd(namID))
           endif

           !tcx improve match here with nx,ny,gridname 
           call oasis_part_create(part2,'1d',gsize,nx,ny,gridname)

           if (prism_part(part2)%nx < 1) then
              prism_part(part2)%nx = nx
              prism_part(part2)%ny = ny
              prism_part(part2)%gridname = trim(gridname)
           endif

           if (prism_coupler(nc)%getput == OASIS3_PUT) then
             !prism_mapper(mapID)%spart = part1   ! set above
              prism_mapper(mapID)%dpart = part2
           else
              prism_mapper(mapID)%spart = part2
             !prism_mapper(mapID)%dpart = part1   ! set above
           endif
           spart = prism_mapper(mapID)%spart
           dpart = prism_mapper(mapID)%dpart

           !--- cstring sets whether src or dst are rearranged in remap
           !--- src = rearrange and map (bfb), dst = map and rearrange (partial sum)
           if (prism_mapper(mapID)%opt == 'opt') then
              if (prism_part(spart)%gsize > prism_part(dpart)%gsize) then
                 cstring = 'dst'
              else
                 cstring = 'src'
              endif
           elseif (prism_mapper(mapID)%opt == 'bfb') then
              cstring = 'src'
           elseif (prism_mapper(mapID)%opt == 'sum') then
              cstring = 'dst'
           else
              write(nulprt,*) subname,' ERROR mapper opt invalid ',trim(prism_mapper(mapID)%opt)
              WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
              CALL oasis_flush(nulprt)
              call oasis_abort_noarg()
           endif
           if (prism_mapper(mapID)%optval /= '' .and. &
               prism_mapper(mapID)%optval /= trim(cstring)) then
              write(nulprt,*) subname,' ERROR mapper opt changed',&
                              trim(prism_mapper(mapID)%optval),' ',trim(cstring)
              WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
              CALL oasis_flush(nulprt)
              call oasis_abort_noarg()
           endif
           prism_mapper(mapID)%optval = trim(cstring)

           !call oasis_timer_start('cpl_smatrd')
           !-------------------------------
           ! smatreaddnc allocates sMati to nwgts
           ! then instantiate an sMatP for each set of wgts
           ! to support higher order mapping
           !-------------------------------
           call oasis_coupler_smatreaddnc(sMati,prism_part(spart)%gsmap,prism_part(dpart)%gsmap, &
              trim(cstring),trim(prism_mapper(mapID)%file),mpi_rank_local,mpi_comm_local, &
              nwgts)
           prism_mapper(mapID)%nwgts = nwgts
           allocate(prism_mapper(mapID)%sMatP(nwgts))
           do n = 1,nwgts
              call mct_sMatP_Init(prism_mapper(mapID)%sMatP(n), sMati(n), &
                 prism_part(spart)%gsmap, prism_part(dpart)%gsmap, 0, mpi_comm_local, compid)
              call mct_sMat_Clean(sMati(n))
           enddo
           deallocate(sMati)
           !call oasis_timer_stop('cpl_smatrd')

           lsize = mct_smat_gNumEl(prism_mapper(mapID)%sMatP(1)%Matrix,mpi_comm_local)
           prism_mapper(mapID)%init = .true.
           if (OASIS_debug >= 15) then
              write(nulprt,*) subname," DEBUG ci:done initializing prism_mapper",mapID,&
                              " nElements = ",lsize," nwgts = ",nwgts
              call oasis_flush(nulprt)
           endif
        endif  ! map init

        !--------------------------------
        ! read mapper mask and area if not already done
        !--------------------------------
        if (.not.prism_mapper(mapID)%AVred .and. prism_coupler(nc)%conserv /= ip_cnone) then
           ! initialize and load AV_ms and AV_md

           spart = prism_mapper(mapID)%spart
           dpart = prism_mapper(mapID)%dpart

           lsize = mct_gsmap_lsize(prism_part(spart)%gsmap,mpi_comm_local)
           call mct_avect_init(prism_mapper(mapID)%av_ms,iList='mask',rList='area',lsize=lsize)
           call mct_avect_zero(prism_mapper(mapID)%av_ms)
           gridname = prism_part(spart)%gridname
           call oasis_io_read_avfld('masks.nc',prism_mapper(mapID)%av_ms, &
              prism_part(spart)%gsmap,'mask',trim(gridname)//'.msk',fldtype='int')
           call oasis_io_read_avfld('areas.nc',prism_mapper(mapID)%av_ms, &
              prism_part(spart)%gsmap,'area',trim(gridname)//'.srf',fldtype='real')

           lsize = mct_gsmap_lsize(prism_part(dpart)%gsmap,mpi_comm_local)
           call mct_avect_init(prism_mapper(mapID)%av_md,iList='mask',rList='area',lsize=lsize)
           call mct_avect_zero(prism_mapper(mapID)%av_md)
           gridname = prism_part(dpart)%gridname
           call oasis_io_read_avfld('masks.nc',prism_mapper(mapID)%av_md, &
              prism_part(dpart)%gsmap,'mask',trim(gridname)//'.msk',fldtype='int')
           call oasis_io_read_avfld('areas.nc',prism_mapper(mapID)%av_md, &
              prism_part(dpart)%gsmap,'area',trim(gridname)//'.srf',fldtype='real')

           prism_mapper(mapID)%AVred = .true.

           if (OASIS_debug >= 30) then
              write(nulprt,*) subname,' DEBUG msi ',minval(prism_mapper(mapID)%av_ms%iAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_ms%iAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_ms%iAttr(:,:))
              write(nulprt,*) subname,' DEBIG msr ',minval(prism_mapper(mapID)%av_ms%rAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_ms%rAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_ms%rAttr(:,:))
              write(nulprt,*) subname,' DEBUG mdi ',minval(prism_mapper(mapID)%av_md%iAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_md%iAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_md%iAttr(:,:))
              write(nulprt,*) subname,' DEBUG mdr ',minval(prism_mapper(mapID)%av_md%rAttr(:,:)),&
                              maxval(prism_mapper(mapID)%av_md%rAttr(:,:)),&
                              sum(prism_mapper(mapID)%av_md%rAttr(:,:))
              CALL oasis_flush(nulprt)
           endif
        endif

        !--------------------------------
        ! initialize avect1m
        !--------------------------------

        lsize = mct_gsmap_lsize(prism_part(part2)%gsmap,mpi_comm_local)
        if (OASIS_debug >= 15) then
           write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part2 info ',part2,mapID,gsize,lsize
           write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part2a',prism_part(part2)%gsmap%ngseg,&
                                        prism_part(part2)%gsmap%gsize
           do n1 = 1,prism_part(part2)%gsmap%ngseg
              write(nulprt,'(1x,2a,4i12)') subname,' DEBUG ci:part2b',n1,prism_part(part2)%gsmap%start(n1),&
                                           prism_part(part2)%gsmap%length(n1),prism_part(part2)%gsmap%pe_loc(n1)
           enddo
           call oasis_flush(nulprt)
        endif
        call mct_avect_init(prism_coupler(nc)%avect1m,rList=trim(prism_coupler(nc)%fldlist),lsize=lsize)
        call mct_avect_zero(prism_coupler(nc)%avect1m)
        if (OASIS_debug >= 15) then
           write(nulprt,*) subname,' DEBUG ci:avect1m initialized '
           call oasis_flush(nulprt)
        endif

        !--------------------------------
        ! router partition is always other part
        !--------------------------------

        rpart = part2

     else

        !--------------------------------
        ! router partition is just coupler part
        !--------------------------------

        rpart = prism_coupler(nc)%partID

     endif  ! no mapper

     !--------------------------------
     ! initialize router based on rpart
     !--------------------------------

     if (prism_coupler(nc)%sndrcv) then

        if (OASIS_debug >= 15) then
           write(nulprt,*) subname,' DEBUG ci:initialize router ',prism_coupler(nc)%routerID,&
                           prism_coupler(nc)%comp,rpart
           call oasis_flush(nulprt)
        endif

        call mct_router_init(prism_coupler(nc)%comp,prism_part(rpart)%gsmap, &
           mpi_comm_local,prism_router(prism_coupler(nc)%routerID)%router)

        if (OASIS_debug >= 15) then
           write(nulprt,*) subname," DEBUG ci:done initializing prism_router",&
                           prism_coupler(nc)%routerID
           call oasis_flush(nulprt)
        endif
     endif

  enddo

  !----------------------------------------------------------
  ! Diagnostics
  !----------------------------------------------------------

  if (OASIS_debug >= 2) then
     write(nulprt,*) ' '
     write(nulprt,*) subname,' couplers initialized'
     do nc = 1,prism_ncoupler
        call oasis_coupler_print(nc)
     enddo
     write(nulprt,*) ' '
     CALL oasis_flush(nulprt)
  endif

  call oasis_timer_stop('cpl_setup')

  call oasis_debug_exit(subname)

  END SUBROUTINE oasis_coupler_setup

!------------------------------------------------------------
  SUBROUTINE oasis_coupler_print(cplid)

  IMPLICIT NONE

  integer(ip_i4_p), intent(in) :: cplid
  !----------------------------------------------------------
  integer(ip_i4_p) :: mapid, rouid, parid, namid, nflds
  integer(ip_i4_p) :: spart,dpart
  character(len=*),parameter :: subname = 'oasis_coupler_print'

  call oasis_debug_enter(subname)

  mapid = prism_coupler(cplid)%mapperid
  rouid = prism_coupler(cplid)%routerid
  parid = prism_coupler(cplid)%partid
  namid = prism_coupler(cplid)%namid
  nflds = prism_coupler(cplid)%nflds

     write(nulprt,*) ' '
     write(nulprt,*) subname,' model and cplid',compid,cplid
  if (prism_coupler(cplid)%getput == OASIS3_PUT) then
     write(nulprt,*) subname,'   send fields  ',trim(prism_coupler(cplid)%fldlist)
     write(nulprt,*) subname,'   from model   ',compid
     write(nulprt,*) subname,'   to model     ',prism_coupler(cplid)%comp
     write(nulprt,*) subname,'   using router ',rouid
     write(nulprt,*) subname,'   transform    ',prism_coupler(cplid)%trans
     write(nulprt,*) subname,'   snd diagnose ',prism_coupler(cplid)%snddiag
     write(nulprt,*) subname,'   snd fld mult ',prism_coupler(cplid)%sndmult
     write(nulprt,*) subname,'   snd fld add  ',prism_coupler(cplid)%sndadd
  endif
  if (prism_coupler(cplid)%getput == OASIS3_GET) then
     write(nulprt,*) subname,'   recv fields  ',trim(prism_coupler(cplid)%fldlist)
     write(nulprt,*) subname,'   from model   ',prism_coupler(cplid)%comp
     write(nulprt,*) subname,'   to model     ',compid
     write(nulprt,*) subname,'   using router ',rouid
     write(nulprt,*) subname,'   rcv diagnose ',prism_coupler(cplid)%rcvdiag
     write(nulprt,*) subname,'   rcv fld mult ',prism_coupler(cplid)%rcvmult
     write(nulprt,*) subname,'   rcv fld add  ',prism_coupler(cplid)%rcvadd
  endif
     write(nulprt,*) subname,'   namcouple op ',prism_coupler(cplid)%ops
     write(nulprt,*) subname,'   namcouple id ',namid
     write(nulprt,*) subname,'   variable ids ',prism_coupler(cplid)%varid(1:nflds)
     write(nulprt,*) subname,'   sndrcv flag  ',prism_coupler(cplid)%sndrcv
     write(nulprt,*) subname,'   output flag  ',prism_coupler(cplid)%output
     write(nulprt,*) subname,'   input flag   ',prism_coupler(cplid)%input
     write(nulprt,*) subname,'   input file   ',trim(prism_coupler(cplid)%inpfile)
     write(nulprt,*) subname,'   restart file ',trim(prism_coupler(cplid)%rstfile)
     write(nulprt,*) subname,'   tag          ',prism_coupler(cplid)%tag
     write(nulprt,*) subname,'   seq          ',prism_coupler(cplid)%seq
     write(nulprt,*) subname,'   maxtime      ',prism_coupler(cplid)%maxtime
     write(nulprt,*) subname,'   dt, lag      ',prism_coupler(cplid)%dt,prism_coupler(cplid)%lag
     write(nulprt,*) subname,'   partid, size ',parid,trim(prism_part(parid)%gridname),&
                                                prism_part(parid)%gsize
     write(nulprt,*) subname,'   partid, nx,ny',prism_part(parid)%nx,prism_part(parid)%ny
     write(nulprt,*) subname,'   maploc       ',trim(prism_coupler(cplid)%maploc)

  if (mapid > 0) then
     write(nulprt,*) subname,'   use map      ',mapid,trim(prism_mapper(mapid)%file)
     write(nulprt,*) subname,'   nwgts        ',mapid,prism_mapper(mapid)%nwgts
     spart = prism_mapper(mapid)%spart
     dpart = prism_mapper(mapid)%dpart
     write(nulprt,*) subname,'   conserve     ',prism_coupler(cplid)%conserv
     write(nulprt,*) subname,'   conserve opt ',prism_coupler(cplid)%consopt
     write(nulprt,*) subname,'   location     ',trim(prism_mapper(mapid)%loc)
     write(nulprt,*) subname,'   opt,optval   ',trim(prism_mapper(mapid)%opt),' ',&
                                                trim(prism_mapper(mapid)%optval)
     write(nulprt,*) subname,'   s/d partids  ',spart,dpart
     if (spart > 0) &
     write(nulprt,*) subname,'   from/to      ',trim(prism_part(spart)%gridname),' ',&
                                                trim(prism_part(dpart)%gridname)
     write(nulprt,*) subname,'   from nx,ny   ',prism_part(spart)%gsize,prism_part(spart)%nx,&
                                                prism_part(spart)%ny
     if (dpart > 0) &
     write(nulprt,*) subname,'   to nx,ny     ',prism_part(dpart)%gsize, prism_part(dpart)%nx,&
                                                prism_part(dpart)%ny
  endif

  call oasis_flush(nulprt)

  call oasis_debug_exit(subname)

  END SUBROUTINE oasis_coupler_print

!------------------------------------------------------------
  SUBROUTINE oasis_coupler_genmap(mapid,namid)

  IMPLICIT NONE

  integer(ip_i4_p), intent(in) :: mapid
  integer(ip_i4_p), intent(in) :: namid
  !----------------------------------------------------------

  integer(ip_i4_p)              :: src_size,src_rank, ncrn_src
  integer(ip_i4_p) ,allocatable :: src_dims(:),src_mask(:)
  real(ip_double_p),allocatable :: src_lon(:),src_lat(:)
  real(ip_double_p),allocatable :: src_corner_lon(:,:),src_corner_lat(:,:)
  integer(ip_i4_p)              :: dst_size,dst_rank, ncrn_dst
  integer(ip_i4_p) ,allocatable :: dst_dims(:),dst_mask(:)
  real(ip_double_p),allocatable :: dst_lon(:),dst_lat(:)
  real(ip_double_p),allocatable :: dst_corner_lon(:,:),dst_corner_lat(:,:)
  integer(ip_i4_p) ,allocatable :: ifld2(:,:)
  real(ip_double_p),allocatable :: fld2(:,:),fld3(:,:,:)
  integer(ip_i4_p) :: i,j,k,icnt,nx,ny,nc
  logical :: lextrapdone
  logical :: do_corners
  character(len=ic_med) :: filename
  character(len=ic_med) :: fldname
  character(len=*),parameter :: subname = 'oasis_coupler_genmap'

  call oasis_debug_enter(subname)

  lextrapdone = .false.
  nx = -1  ! must be read
  ny = -1  ! must be read
  nc =  1  ! might not be read, set to something reasonable

  !--- checks first ---

  if (trim(namscrtyp(namID)) /= 'SCALAR') then
     write(nulprt,*) subname,' ERROR: only scrip type SCALAR mapping supported'
     WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
     CALL oasis_flush(nulprt)
     CALL oasis_abort_noarg()
  endif

  
  do_corners = .false.
  if (trim(namscrmet(namID)) == 'CONSERV') then
     do_corners = .true.
  endif

  !--- src data ---

  filename = 'grids.nc'
  if (do_corners) then
     fldname = trim(namsrcgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny,nz=nc)
  else
     fldname = trim(namsrcgrd(namID))//'.lon'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny)
  endif
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',&
                                         trim(fldname),nx,ny,nc,do_corners
  src_rank = 2
  src_size = nx*ny
  allocate(src_dims(src_rank))
  src_dims(1) = nx
  src_dims(2) = ny
  ncrn_src = nc
  allocate(src_mask(src_size))
  allocate(src_lon (src_size))
  allocate(src_lat (src_size))
  allocate(src_corner_lon(ncrn_src,src_size))
  allocate(src_corner_lat(ncrn_src,src_size))

  allocate(ifld2(nx,ny))
  filename = 'masks.nc'
  fldname = trim(namsrcgrd(namID))//'.msk'
  call oasis_io_read_field_fromroot(filename,fldname,ifld2=ifld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_mask(icnt) = ifld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(src_mask),maxval(src_mask)
  deallocate(ifld2)

  allocate(fld2(nx,ny))
  filename = 'grids.nc'
  fldname = trim(namsrcgrd(namID))//'.lon'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_lon(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(src_lon),maxval(src_lon)
  fldname = trim(namsrcgrd(namID))//'.lat'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     src_lat(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(src_lat),maxval(src_lat)
  deallocate(fld2)

  if (do_corners) then
     allocate(fld3(nx,ny,nc))
     filename = 'grids.nc'
     fldname = trim(namsrcgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           src_corner_lon(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(src_corner_lon),maxval(src_corner_lon)
     fldname = trim(namsrcgrd(namID))//'.cla'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           src_corner_lat(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(src_corner_lat),maxval(src_corner_lat)
     deallocate(fld3)
  else
     src_corner_lon = -9999.
     src_corner_lat = -9999.
  endif

  !--- dst data ---

  filename = 'grids.nc'
  if (do_corners) then
     fldname = trim(namdstgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny,nz=nc)
  else
     fldname = trim(namdstgrd(namID))//'.lon'
     call oasis_io_read_field_fromroot(filename,fldname,nx=nx,ny=ny)
  endif
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname),nx,ny,nc
  dst_rank = 2
  dst_size = nx*ny
  allocate(dst_dims(dst_rank))
  dst_dims(1) = nx
  dst_dims(2) = ny
  ncrn_dst = nc
  allocate(dst_mask(dst_size))
  allocate(dst_lon (dst_size))
  allocate(dst_lat (dst_size))
  allocate(dst_corner_lon(ncrn_dst,dst_size))
  allocate(dst_corner_lat(ncrn_dst,dst_size))

  allocate(ifld2(nx,ny))
  filename = 'masks.nc'
  fldname = trim(namdstgrd(namID))//'.msk'
  call oasis_io_read_field_fromroot(filename,fldname,ifld2=ifld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_mask(icnt) = ifld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(dst_mask),maxval(dst_mask)
  deallocate(ifld2)

  allocate(fld2(nx,ny))
  filename = 'grids.nc'
  fldname = trim(namdstgrd(namID))//'.lon'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_lon(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(dst_lon),maxval(dst_lon)
  fldname = trim(namdstgrd(namID))//'.lat'
  call oasis_io_read_field_fromroot(filename,fldname,fld2=fld2)
  icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
     dst_lat(icnt) = fld2(i,j)
  enddo; enddo
  if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
     minval(dst_lat),maxval(dst_lat)
  deallocate(fld2)

  if (do_corners) then
     allocate(fld3(nx,ny,nc))
     filename = 'grids.nc'
     fldname = trim(namdstgrd(namID))//'.clo'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           dst_corner_lon(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(dst_corner_lon),maxval(dst_corner_lon)
     fldname = trim(namdstgrd(namID))//'.cla'
     call oasis_io_read_field_fromroot(filename,fldname,fld3=fld3)
     icnt = 0; do j = 1,ny; do i = 1,nx; icnt = icnt + 1
        do k = 1,nc
           dst_corner_lat(k,icnt) = fld3(i,j,k)
        enddo
     enddo; enddo
     if (OASIS_debug >= 15) write(nulprt,*) subname,' read ',trim(filename),' ',trim(fldname), &
        minval(dst_corner_lat),maxval(dst_corner_lat)
     deallocate(fld3)
  else
     dst_corner_lon = -9999.
     dst_corner_lat = -9999.
  endif

  IF (OASIS_debug >= 15) THEN
      WRITE(nulprt,*) subname,' call grid_init '
      CALL oasis_flush(nulprt)
  ENDIF

  !--- 0/1 mask convention opposite in scrip vs oasis
  src_mask = 1 - src_mask
  dst_mask = 1 - dst_mask
  call grid_init(namscrmet(namID),namscrres(namID),namscrbin(namID),  &
       src_size, dst_size, src_dims, dst_dims, &
       src_rank, dst_rank, ncrn_src, ncrn_dst, &
       src_mask, dst_mask, namsrcgrd(namID), namdstgrd(namID), &
       src_lat,  src_lon,  dst_lat,  dst_lon, &
       src_corner_lat, src_corner_lon, &
       dst_corner_lat, dst_corner_lon, &
       logunit=nulprt)
  if (OASIS_debug >= 15) then
      WRITE(nulprt,*) subname,' done grid_init '
      CALL oasis_flush(nulprt)
  ENDIF

  IF (OASIS_debug >= 15) THEN
      WRITE(nulprt,*) subname,' call scrip '
      CALL oasis_flush(nulprt)
  ENDIF
  call scrip(prism_mapper(mapid)%file,prism_mapper(mapid)%file,namscrmet(namID), &
             namscrnor(namID),lextrapdone,namscrvam(namID),namscrnbr(namID))
  IF (OASIS_debug >= 15) THEN
      WRITE(nulprt,*) subname,' done scrip '
      CALL oasis_flush(nulprt)
  ENDIF

  deallocate(src_dims, dst_dims)
  deallocate(src_mask)
  deallocate(src_lon)
  deallocate(src_lat)
  deallocate(src_corner_lon)
  deallocate(src_corner_lat)
  deallocate(dst_mask)
  deallocate(dst_lon)
  deallocate(dst_lat)
  deallocate(dst_corner_lon)
  deallocate(dst_corner_lat)


  call oasis_debug_exit(subname)

  END SUBROUTINE oasis_coupler_genmap

!------------------------------------------------------------

!BOP ===========================================================================
!
! !IROUTINE:  oasis_coupler_sMatReaddnc - Do a distributed read of a NetCDF SCRIP file and
!                                return weights in a distributed SparseMatrix
!
! !DESCRIPTION: 
!     Read in mapping matrix data from a SCRIP netCDF data file using
!     a low memory method and then scatter to all pes.  Based on 
!     oasis_coupler_sMatReaddnc from CESM1.0.3
!
! !REMARKS:
!   This routine leverages gsmaps to determine scatter pattern
!   The scatter is implemented as a bcast of all weights then a local
!     computation on each pe to determine with weights to keep based
!     on gsmap information.
!   The algorithm to determine whether a weight belongs on a pe involves
!     creating a couple local arrays (lsstart and lscount) which are
!     the local values of start and length from the gsmap.  these are
!     sorted via a bubble sort and then searched via a binary search
!     to check whether a global index is on the local pe.
!   The local buffer sizes are estimated up front based on ngridcell/npes
!     plus 20% (see 1.2 below).  If the local buffer size fills up, then
!     the buffer is reallocated 50% large (see 1.5 below) and the fill
!     continues.  The idea is to trade off memory reallocation and copy
!     with memory usage.  1.2 and 1.5 are arbitary, other values may
!     result in better performance.
!   Once all the matrix weights have been read, the sMat is initialized,
!     the values from the buffers are copied in, and everything is deallocated.
!
! !INTERFACE:  -----------------------------------------------------------------

subroutine oasis_coupler_sMatReaddnc(sMat,SgsMap,DgsMap,newdom, &
                            fileName,mytask,mpicom,nwgts, &
                            areasrc,areadst,ni_i,nj_i,ni_o,nj_o )

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   type(mct_sMat)  ,intent(out),pointer   :: sMat(:) ! mapping data
   type(mct_gsMap) ,intent(in) ,target    :: SgsMap  ! src gsmap
   type(mct_gSMap) ,intent(in) ,target    :: DgsMap  ! dst gsmap
   character(*)    ,intent(in)            :: newdom  ! type of sMat (src or dst)
        ! src = rearrange and map (bfb), dst = map and rearrange (partial sums)
   character(*)    ,intent(in)            :: filename! netCDF file to read
   integer(IN)     ,intent(in)            :: mytask   ! processor id
   integer(IN)     ,intent(in)            :: mpicom  ! communicator
   integer(IN)     ,intent(out)           :: nwgts   ! number of weights 
   type(mct_Avect) ,intent(out), optional :: areasrc ! area of src grid from mapping file
   type(mct_Avect) ,intent(out), optional :: areadst ! area of dst grid from mapping file
   integer(IN)     ,intent(out), optional :: ni_i    ! number of lons on input grid   
   integer(IN)     ,intent(out), optional :: nj_i    ! number of lats on input grid   
   integer(IN)     ,intent(out), optional :: ni_o    ! number of lons on output grid   
   integer(IN)     ,intent(out), optional :: nj_o    ! number of lats on output grid   

! !EOP

   !--- local ---
   integer           :: n,m     ! generic loop indicies
   integer           :: na      ! size of source domain
   integer           :: nb      ! size of destination domain
   integer           :: ns      ! number of non-zero elements in matrix
   integer           :: ni,nj   ! number of row and col in the matrix
   integer           :: igrow   ! aVect index for matrix row
   integer           :: igcol   ! aVect index for matrix column
   integer           :: iwgt    ! aVect index for matrix element
   integer           :: iarea   ! aVect index for area
   integer           :: rsize   ! size of read buffer
   integer           :: cnt     ! local num of wgts
   integer           :: cntold  ! local num of wgts, previous read
   integer           :: start(1)! netcdf read
   integer           :: count(1)! netcdf read
   integer           :: start2(2)! netcdf read
   integer           :: count2(2)! netcdf read
   integer           :: bsize   ! buffer size
   integer           :: nread   ! number of reads 
   logical           :: mywt    ! does this weight belong on my pe
   integer           :: dims(2) 

   !--- buffers for i/o ---
   real(R8)   ,allocatable :: rtemp(:) ! real temporary
   real(R8)   ,allocatable :: Sbuf(:,:)  ! real weights
   real(R8)   ,allocatable :: remaps(:,:)  ! real weights with num_wgts dim
   integer,allocatable :: Rbuf(:)  ! ints rows
   integer,allocatable :: Cbuf(:)  ! ints cols

   !--- variables associated with local computation of global indices
   integer             :: lsize     ! size of local seg map
   integer             :: commsize  ! size of local communicator
   integer,allocatable :: lsstart(:) ! local seg map info
   integer,allocatable :: lscount(:) ! local seg map info
   type(mct_gsMap),pointer :: mygsmap ! pointer to one of the gsmaps
   integer             :: l1,l2     ! generice indices for sort
   logical             :: found     ! for sort

   !--- variable assocaited with local data buffers and reallocation
   real(R8)   ,allocatable :: Snew(:,:),Sold(:,:)  ! reals
   integer,allocatable :: Rnew(:),Rold(:)  ! ints
   integer,allocatable :: Cnew(:),Cold(:)  ! ints

   character,allocatable :: str(:)  ! variable length char string
   character(len=ic_long):: attstr  ! netCDF attribute name string
   integer           :: status   ! netCDF routine return code
   integer           :: fid     ! netCDF file      ID
   integer           :: vid     ! netCDF variable  ID
   integer           :: did     ! netCDF dimension ID
   !--- arbitrary size of read buffer, this is the chunk size weights reading
   integer,parameter :: rbuf_size = 100000

   !--- global source and destination areas ---
   type(mct_Avect) :: areasrc0   ! area of src grid from mapping file
   type(mct_Avect) :: areadst0   ! area of src grid from mapping file

   character(*),parameter :: areaAV_field = 'aream'

   !--- formats ---
   character(*),parameter :: subName = 'oasis_coupler_sMatReaddnc'
   character(*),parameter :: F00 = '("oasis_coupler_sMatReaddnc",1x,4a)'
   character(*),parameter :: F01 = '("oasis_coupler_sMatReaddnc",1x,2(a,i10))'

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

 call oasis_debug_enter(subname)
 call oasis_mpi_commsize(mpicom,commsize)
 nwgts = -1
 if (mytask == 0) then
   if (OASIS_debug >= 2) write(nulprt,F00) "reading mapping matrix data decomposed..."

   !----------------------------------------------------------------------------
   ! open & read the file
   !----------------------------------------------------------------------------
   if (OASIS_debug >=2 ) write(nulprt,F00) "* file name                  : ",trim(fileName)
   status = nf90_open(trim(filename),NF90_NOWRITE,fid)
   if (status /= NF90_NOERR) then
      write(nulprt,F00) trim(nf90_strerror(status))
      WRITE(nulprt,*) subname,'ERROR filename'
      WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif

   !--- get matrix dimensions ----------
!  status = nf90_inq_dimid (fid, 'n_s', did)  ! size of sparse matrix
   status = nf90_inq_dimid (fid, 'num_links', did)  ! size of sparse matrix
   status = nf90_inquire_dimension(fid, did  , len = ns)
!  status = nf90_inq_dimid (fid, 'n_a', did)  ! size of  input vector
   status = nf90_inq_dimid (fid, 'src_grid_size', did)  ! size of  input vector
   status = nf90_inquire_dimension(fid, did  , len = na)
!  status = nf90_inq_dimid (fid, 'n_b', did)  ! size of output vector
   status = nf90_inq_dimid (fid, 'dst_grid_size', did)  ! size of output vector
   status = nf90_inquire_dimension(fid, did  , len = nb)
   status = nf90_inq_dimid (fid, 'num_wgts', did)  ! size of output vector
   status = nf90_inquire_dimension(fid, did  , len = nwgts)
   
   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
!     status = nf90_inq_dimid (fid, 'ni_a', did)  ! number of lons in input grid
!     status = nf90_inquire_dimension(fid, did  , len = ni_i)
!     status = nf90_inq_dimid (fid, 'nj_a', did)  ! number of lats in input grid
!     status = nf90_inquire_dimension(fid, did  , len = nj_i)
!     status = nf90_inq_dimid (fid, 'ni_b', did)  ! number of lons in output grid
!     status = nf90_inquire_dimension(fid, did  , len = ni_o)
!     status = nf90_inq_dimid (fid, 'nj_b', did)  ! number of lats in output grid
!     status = nf90_inquire_dimension(fid, did  , len = nj_o)
      status = nf90_inq_varid(fid, 'src_grid_dims', vid)
      status = nf90_get_var(fid, vid, dims)
      ni_i = dims(1)
      nj_i = dims(2)
      status = nf90_inq_varid(fid, 'dst_grid_dims', vid)
      status = nf90_get_var(fid, vid, dims)
      ni_o = dims(1)
      nj_o = dims(2)
   end if

   if (OASIS_debug >= 2) write(nulprt,F01) "* matrix dims src x dst      : ",na,' x',nb
   if (OASIS_debug >= 2) write(nulprt,F01) "* number of non-zero elements: ",ns

 endif
 
   !--- read and load area_a ---
   if (present(areasrc)) then
   if (mytask == 0) then
      call mct_aVect_init(areasrc0,' ',areaAV_field,na)
!     status = nf90_inq_varid     (fid,'area_a',vid)
      status = nf90_inq_varid     (fid,'src_grid_area',vid)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,F00) TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
      status = nf90_get_var(fid, vid, areasrc0%rAttr)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,F00) TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
   endif
   call mct_aVect_scatter(areasrc0, areasrc, SgsMap, 0, mpicom, status)
   if (status /= 0) call mct_die(subname,"Error on scatter of areasrc0")
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(nulprt,*) subName,'Size of src ',mct_aVect_lSize(areasrc0)
!            write(nulprt,*) subName,'min/max src ',minval(areasrc0%rAttr(1,:)),maxval(areasrc0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areasrc0)
   end if
   end if

   !--- read and load area_b ---
   if (present(areadst)) then
   if (mytask == 0) then
      call mct_aVect_init(areadst0,' ',areaAV_field,nb)
!     status = nf90_inq_varid     (fid,'area_b',vid)
      status = nf90_inq_varid     (fid,'dst_grid_area',vid)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,F00) TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
      status = nf90_get_var(fid, vid, areadst0%rAttr)
      IF (status /= NF90_NOERR) THEN
          WRITE(nulprt,F00) TRIM(nf90_strerror(status))
          WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
          CALL oasis_flush(nulprt)
      ENDIF
   endif
   call mct_aVect_scatter(areadst0, areadst, DgsMap, 0, mpicom, status)
   if (status /= 0) call mct_die(subname,"Error on scatter of areadst0")
   if (mytask == 0) then
!      if (present(dbug)) then
!         if (dbug > 2) then
!            write(nulprt,*) subName,'Size of dst ',mct_aVect_lSize(areadst0)
!            write(nulprt,*) subName,'min/max dst ',minval(areadst0%rAttr(1,:)),maxval(areadst0%rAttr(1,:))
!         endif
!      end if
      call mct_aVect_clean(areadst0)
   endif
   endif

   if (present(ni_i) .and. present(nj_i) .and. present(ni_o) .and. present(nj_o)) then
      call oasis_mpi_bcast(ni_i,mpicom,subName//" MPI in ni_i bcast")
      call oasis_mpi_bcast(nj_i,mpicom,subName//" MPI in nj_i bcast")
      call oasis_mpi_bcast(ni_o,mpicom,subName//" MPI in ni_o bcast")
      call oasis_mpi_bcast(nj_o,mpicom,subName//" MPI in nj_o bcast")
   end if

   call oasis_mpi_bcast(ns,mpicom,subName//" MPI in ns bcast")
   call oasis_mpi_bcast(na,mpicom,subName//" MPI in na bcast")
   call oasis_mpi_bcast(nb,mpicom,subName//" MPI in nb bcast")
   call oasis_mpi_bcast(nwgts,mpicom,subName//" MPI in nwgts bcast")

   !--- setup local seg map, sorted
   if (newdom == 'src') then
      mygsmap => DgsMap
   elseif (newdom == 'dst') then
      mygsmap => SgsMap
   else
      write(nulprt,F00) 'ERROR: invalid newdom value = ',newdom
      WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
      CALL oasis_flush(nulprt)
      call oasis_abort_noarg()
   endif
   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then
         lsize=lsize+1
      endif
   enddo
   allocate(lsstart(lsize),lscount(lsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Lsstart',status)

   lsize = 0
   do n = 1,size(mygsmap%start)
      if (mygsmap%pe_loc(n) == mytask) then  ! on my pe
         lsize=lsize+1
         found = .false.
         l1 = 1
         do while (.not.found .and. l1 < lsize)         ! bubble sort copy
            if (mygsmap%start(n) < lsstart(l1)) then
               do l2 = lsize, l1+1, -1
                  lsstart(l2) = lsstart(l2-1)
                  lscount(l2) = lscount(l2-1)
               enddo
               found = .true.
            else
               l1 = l1 + 1
            endif
         enddo
         lsstart(l1) = mygsmap%start(n)
         lscount(l1) = mygsmap%length(n)
      endif
   enddo
   do n = 1,lsize-1
      if (lsstart(n) > lsstart(n+1)) then
         write(nulprt,F00) ' ERROR: lsstart not properly sorted'
         WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
         CALL oasis_flush(nulprt)
         call oasis_abort_noarg()
      endif
   enddo

   rsize = min(rbuf_size,ns)                     ! size of i/o chunks
   bsize = ((ns/commsize) + 1 ) * 1.2   ! local temporary buffer size
   if (ns == 0) then
      nread = 0
   else
      nread = (ns-1)/rsize + 1                      ! num of reads to do
   endif

   if (mytask == 0) then
      allocate(remaps(nwgts,rsize),stat=status)
      if (status /= 0) call mct_perr_die(subName,':: allocate remaps',status)
   endif
   allocate(Smat(nwgts),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Smat',status)
   allocate(Sbuf(nwgts,rsize),Rbuf(rsize),Cbuf(rsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Sbuf',status)
   allocate(Snew(nwgts,bsize),Cnew(bsize),Rnew(bsize),stat=status)
   if (status /= 0) call mct_perr_die(subName,':: allocate Snew1',status)

   cnt = 0
   do n = 1,nread
      start(1) = (n-1)*rsize + 1
      count(1) = min(rsize,ns-start(1)+1)
      start2(1) = 1
      count2(1) = nwgts
      start2(2) = start(1)
      count2(2) = count(1)

      !--- read data on root pe
      if (mytask== 0) then
!        status = nf90_inq_varid      (fid,'S'  ,vid)
         status = nf90_inq_varid      (fid,'remap_matrix'  ,vid)
!        status = nf90_get_var(fid,vid,start,count,Sbuf)
         status = nf90_get_var(fid,vid,remaps,start2,count2)
         Sbuf(:,:) = remaps(:,:)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,F00) TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF

!        status = nf90_inq_varid      (fid,'row',vid)
         status = nf90_inq_varid      (fid,'dst_address',vid)
         status = nf90_get_var   (fid,vid,Rbuf,start,count)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,F00) TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF

!        status = nf90_inq_varid      (fid,'col',vid)
         status = nf90_inq_varid      (fid,'src_address',vid)
         status = nf90_get_var   (fid,vid,Cbuf,start,count)
         IF (status /= NF90_NOERR) THEN
             WRITE(nulprt,F00) TRIM(nf90_strerror(status))
             WRITE(nulprt,*) subname,'model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
         ENDIF
      endif

      !--- send S, row, col to all pes
      call oasis_mpi_bcast(Sbuf,mpicom,subName//" MPI in Sbuf bcast")
      call oasis_mpi_bcast(Rbuf,mpicom,subName//" MPI in Rbuf bcast")
      call oasis_mpi_bcast(Cbuf,mpicom,subName//" MPI in Cbuf bcast")

      !--- now each pe keeps what it should
      do m = 1,count(1)
         !--- should this weight be on my pe
         if (newdom == 'src') then
            mywt = check_myindex(Rbuf(m),lsstart,lscount)
         elseif (newdom == 'dst') then
            mywt = check_myindex(Cbuf(m),lsstart,lscount)
         endif

         if (mywt) then
            cntold = cnt
            cnt = cnt + 1

            !--- new arrays need to be bigger
            if (cnt > bsize) then
               !--- allocate old arrays and copy new into old
               allocate(Sold(1:nwgts,cntold),Rold(cntold),Cold(cntold),stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate old',status)
               Sold(1:nwgts,1:cntold) = Snew(1:nwgts,1:cntold)
               Rold(1:cntold) = Rnew(1:cntold)
               Cold(1:cntold) = Cnew(1:cntold)

               !--- reallocate new to bigger size, increase buffer by 50% (arbitrary)
               deallocate(Snew,Rnew,Cnew,stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate new',status)
               bsize = 1.5 * bsize
               if (OASIS_debug > 15) write(nulprt,F01) ' reallocate bsize to ',bsize
               allocate(Snew(nwgts,bsize),Rnew(bsize),Cnew(bsize),stat=status)
               if (status /= 0) call mct_perr_die(subName,':: allocate old',status)

               !--- copy data back into new
               Snew(1:nwgts,1:cntold) = Sold(1:nwgts,1:cntold)
               Rnew(1:cntold) = Rold(1:cntold)
               Cnew(1:cntold) = Cold(1:cntold)
               deallocate(Sold,Rold,Cold,stat=status)
               if (status /= 0) call mct_perr_die(subName,':: deallocate old',status)
            endif

            Snew(1:nwgts,cnt) = Sbuf(1:nwgts,m)
            Rnew(cnt) = Rbuf(m)
            Cnew(cnt) = Cbuf(m)
         endif
      enddo  ! count
   enddo   ! nread

   if (mytask == 0) then
      deallocate(remaps, stat=status)
      if (status /= 0) call mct_perr_die(subName,':: deallocate remaps',status)
   endif
   deallocate(Sbuf,Rbuf,Cbuf, stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate Sbuf',status)

   !----------------------------------------------------------------------------
   ! init the mct sMat data type
   !----------------------------------------------------------------------------
   ! mct_sMat_init must be given the number of rows and columns that
   ! would be in the full matrix.  Nrows= size of output vector=nb.
   ! Ncols = size of input vector = na.
   do n = 1,nwgts
      call mct_sMat_init(sMat(n), nb, na, cnt)
   enddo

   igrow = mct_sMat_indexIA(sMat(1),'grow')
   igcol = mct_sMat_indexIA(sMat(1),'gcol')
   iwgt  = mct_sMat_indexRA(sMat(1),'weight')

   if (cnt /= 0) then
   do n = 1,nwgts
      sMat(n)%data%rAttr(iwgt ,1:cnt) = Snew(n,1:cnt)
      sMat(n)%data%iAttr(igrow,1:cnt) = Rnew(1:cnt)
      sMat(n)%data%iAttr(igcol,1:cnt) = Cnew(1:cnt)
   enddo
   endif
   deallocate(Snew,Rnew,Cnew, stat=status)
   deallocate(lsstart,lscount,stat=status)
   if (status /= 0) call mct_perr_die(subName,':: deallocate new',status)

   if (mytask == 0) then
      status = nf90_close(fid)
      IF (OASIS_debug >= 2) THEN
          WRITE(nulprt,F00) "... done reading file"
          CALL oasis_flush(nulprt)
      ENDIF
   endif

  call oasis_debug_exit(subname)

end subroutine oasis_coupler_sMatReaddnc

!------------------------------------------------------------
! !BOP ===========================================================================
!
! !IROUTINE:  check_myindex - binary search for index in list
!
! !DESCRIPTION: 
!     Do a binary search to see if a value is contained in a list of
!     values.  return true or false.  starti must be monotonically
!     increasing, function does NOT check this.
!
! !INTERFACE:  -----------------------------------------------------------------

logical function check_myindex(index,starti,counti)

! !USES:

   !--- local kinds ---
   integer,parameter :: R8 = ip_double_p
   integer,parameter :: IN = ip_i4_p

! !INPUT/OUTPUT PARAMETERS:

   integer(IN) :: index       ! is this index in start/count list
   integer(IN) :: starti(:)   ! start list
   integer(IN) :: counti(:)   ! count list

! !EOP

   !--- local ---
   integer(IN)    :: nl,nc,nr,ncprev 
   integer(IN)    :: lsize
   logical        :: stopnow

   !--- formats ---
   character(*),parameter :: subName = '(check_myindex) '

!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------

!   call oasis_debug_enter(subname)
   check_myindex = .false.

   lsize = size(starti)
   if (lsize < 1) then
!     call oasis_debug_exit(subname)
      return
   endif

   nl = 0
   nr = lsize + 1
   nc = (nl+nr)/2
   stopnow = .false.
   do while (.not.stopnow)
      if (index < starti(nc)) then
         nr = nc
      elseif (index > (starti(nc) + counti(nc) - 1)) then
         nl = nc
      else
         check_myindex = .true.
!        call oasis_debug_exit(subname)
         return
      endif
      ncprev = nc
      nc = (nl + nr)/2
      if (nc == ncprev .or. nc < 1 .or. nc > lsize) stopnow = .true.
   enddo

   check_myindex = .false.

!   call oasis_debug_exit(subname)

end function check_myindex

!===============================================================================
END MODULE mod_oasis_coupler


