!-----------------------------------------------------------------------
! Copyright 2010, CERFACS, Toulouse, France.
! Copyright 2010, DKRZ, Hamburg, Germany.
! All rights reserved. Use is subject to OASIS4 license terms.
!-----------------------------------------------------------------------
!
! !DESCRIPTION:
!
! Module oasis_timer contains functionallity, which can be used to
! measure the time consumed in specific parts of the code.
!
! Available routines:
!  oasis_timer_init         allocates timers
!  oasis_timer_start        starts specific timer
!  oasis_timer_stop         stops specific timer and sums up measured time intervals
!  oasis_timer_print        root process prints all timers of all processes sharing
!                            the same mpi communicator provided to oasis_timer_init
!                            in addition it frees all memory allocated by timers
!
!
! !REVISION HISTORY:
!
!   Date      Programmer   Description
! ----------  ----------   -----------
! 03.01.11    M. Hanke     created (based on psmile_timer.F90 and
!                                   prismdrv_timer.F90 from SV and JL)
! 20.09.11    T. Craig     extended
! 16.04.13    T. Craig     use mpi comm from mod_oasis_data
!
!----------------------------------------------------------------------
!
!  $Id: oasis_timer.F90 2849 2011-01-05 08:14:13Z hanke $
!  $Author: hanke $
!
!----------------------------------------------------------------------

module mod_oasis_timer

   use mod_oasis_kinds
   use mod_oasis_data
   use mod_oasis_sys
   use mod_oasis_mpi

   implicit none
   private

   public oasis_timer_init
   public oasis_timer_start
   public oasis_timer_stop
   public oasis_timer_print

   ! name of the application
   character (len=ic_med) :: app_name

   ! name of the time statistics file
   character (len=ic_med) :: file_name

   type timer_details
      ! label of timer
      character (len=ic_med) :: label
      ! wall time values
      double precision :: start_wtime, end_wtime
      ! cpu time values
      double precision :: start_ctime, end_ctime
      ! is the timer running now
      character(len=1) :: runflag
   end type timer_details

   INTEGER :: mtimer 
   TYPE (timer_details), POINTER :: timer(:)
   DOUBLE PRECISION, POINTER     :: sum_ctime(:)       ! these values are not part of timer details
   DOUBLE PRECISION, POINTER     :: sum_wtime(:)       ! because they are later used in an mpi call
   INTEGER, POINTER              :: TIMER_COUNT(:)     ! number of calls

   integer :: ntimer

   integer :: output_unit = 901
   logical,save :: single_timer_header
   character(len=1),parameter :: t_stopped = ' '
   character(len=1),parameter :: t_running = '*'

   contains

! --------------------------------------------------------------------------------

      subroutine oasis_timer_init (app, file, nt)

         implicit none

         character (len=*), intent (in)   :: app
         character (len=*), intent (in)   :: file
         integer          , intent (in)   :: nt

         integer :: ierror,n
         character(len=*),parameter :: subname = 'oasis_timer_init'

         app_name  = trim (app)
         file_name = trim (file)

         mtimer = nt
         ALLOCATE(timer(mtimer))
         ALLOCATE(sum_ctime(mtimer))
         ALLOCATE(sum_wtime(mtimer))
         ALLOCATE(timer_count(mtimer))

         ntimer = 0
         do n = 1,mtimer
            timer(n)%label       = ' '
            timer(n)%start_wtime = 0
            timer(n)%end_wtime   = 0
            timer(n)%start_ctime = 0
            timer(n)%end_ctime   = 0
            timer(n)%runflag     = t_stopped

            sum_wtime(n)         = 0
            sum_ctime(n)         = 0
            timer_count(n)       = 0
         enddo

         IF ((TIMER_debug == 1) .AND. (mpi_rank_local == 0)) TIMER_Debug=2

         IF (TIMER_Debug >= 2) THEN

                 CALL oasis_unitget(output_unit)
                 WRITE(file_name,'(a,i4.4)') TRIM(file)//'_',mpi_rank_local

                 OPEN(output_unit, file=TRIM(file_name), form="FORMATTED", &
                    status="UNKNOWN")
                 WRITE(output_unit,*) ''
                 CLOSE(output_unit)

         ENDIF

         single_timer_header = .false.

      end subroutine oasis_timer_init

! --------------------------------------------------------------------------------

      subroutine oasis_timer_start (timer_label, barrier)

         implicit none

         character(len=*), intent (in) :: timer_label
         logical, intent (in), optional :: barrier

         integer :: ierr
         integer :: timer_id
         real :: cpu_time_arg
         character(len=*),parameter :: subname = 'oasis_timer_start'

         IF (TIMER_Debug >=1) THEN
         call oasis_timer_c2i(timer_label,timer_id)
         if (timer_id < 0) then
            ntimer = ntimer + 1
            timer_id = ntimer
            timer(timer_id)%label = trim(timer_label)
            IF (ntimer+1 > mtimer) THEN
                WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
                WRITE(nulprt,*) subname,' WARNING timer number exceeded' 
                WRITE(nulprt,*) subname,' Increase mtimer in mod_oasis_method'
                CALL oasis_flush(nulprt)
                CALL oasis_abort_noarg()
            ENDIF
         endif

         if (present(barrier)) then
            if (barrier .and. mpi_comm_local /= MPI_COMM_NULL) then
               call MPI_BARRIER(mpi_comm_local, ierr)
            endif
         endif

         timer(timer_id)%start_wtime = MPI_WTIME()
         call cpu_time(cpu_time_arg)
         timer(timer_id)%start_ctime = cpu_time_arg
         timer_count(timer_id) = timer_count(timer_id) + 1
         timer(timer_id)%runflag = t_running
         ENDIF

      end subroutine oasis_timer_start

! --------------------------------------------------------------------------------

      subroutine oasis_timer_stop (timer_label)

         character(len=*), intent (in) :: timer_label
         real :: cpu_time_arg
         integer :: timer_id
         character(len=*),parameter :: subname = 'oasis_timer_stop'

         IF (TIMER_Debug >=1) THEN
         call oasis_timer_c2i(timer_label,timer_id)
         if (timer_id < 0) then
             WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
             WRITE(nulprt,*) subname,' WARNING: timer_label does not exist ',&
                             TRIM(timer_label)
             CALL oasis_flush(nulprt)
             RETURN
         endif

         if (timer(timer_id)%runflag == t_stopped) then
             WRITE(nulprt,*) subname,' model :',compid,' proc :',mpi_rank_local
             WRITE(nulprt,*) subname,' WARNING timer_id ',timer_id,' not started'
             CALL oasis_flush(nulprt)
             RETURN
         endif

         timer(timer_id)%end_wtime = MPI_WTIME()
         call cpu_time(cpu_time_arg)
         timer(timer_id)%end_ctime = cpu_time_arg

         sum_wtime(timer_id) = sum_wtime(timer_id) + &
                               timer(timer_id)%end_wtime - &
                               timer(timer_id)%start_wtime
         sum_ctime(timer_id) = sum_ctime(timer_id) + &
                               timer(timer_id)%end_ctime - &
                               timer(timer_id)%start_ctime
         timer(timer_id)%runflag = t_stopped
         ENDIF

      end subroutine oasis_timer_stop

! --------------------------------------------------------------------------------

      subroutine oasis_timer_print(timer_label)

         implicit none

         character(len=*), optional, intent(in) :: timer_label

         integer :: timer_id
         real, allocatable             :: sum_ctime_global_tmp(:,:)
         double precision, allocatable :: sum_wtime_global_tmp(:,:)
         integer, allocatable          :: count_global_tmp(:,:)
         character(len=ic_med), allocatable :: label_global_tmp(:,:)
         real, allocatable             :: sum_ctime_global(:,:)
         double precision, allocatable :: sum_wtime_global(:,:)
         integer, allocatable          :: count_global(:,:)
         double precision, allocatable :: rarr(:)
         integer, allocatable          :: iarr(:)
         character(len=ic_med), allocatable :: carr(:)
         character(len=ic_med), allocatable :: label_list(:)
         double precision   :: rval
         integer            :: ival
         character(len=ic_med) :: cval
         logical            :: onetimer
         logical            :: found
         integer, parameter :: root = 0
         integer            :: k, n, m
         integer            :: nlabels
         integer            :: ierror
         integer            :: ntimermax
         integer            :: pe1,pe2
         integer            :: minpe,maxpe,mcnt
         double precision   :: mintime,maxtime,meantime
         character(len=*),parameter :: subname = 'oasis_timer_print'

         IF (TIMER_Debug < 1) then
            return
         ENDIF

         onetimer = .false.
         if (present(timer_label)) then
            onetimer = .true.
            call oasis_timer_c2i(timer_label,timer_id)
            if (timer_id < 1) then
                WRITE(nulprt,*) subname,' model :',compid,&
                                ' proc :',mpi_rank_local
                WRITE(nulprt,*) subname,' WARNING: invalid timer_label',&
                                TRIM(timer_label)
                CALL oasis_flush(nulprt)
                RETURN
            endif
         endif

!-----------------------------------------------------
! one timer output
!-----------------------------------------------------
         if (TIMER_Debug >= 2 .and. onetimer) then

            OPEN(output_unit, file=TRIM(file_name), form="FORMATTED", &
               status="UNKNOWN", position="APPEND")
            IF (.NOT.single_timer_header) THEN
               WRITE(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x))') &
                  ' wtime ','on pe','count',' ctime ','on pe','count'
               single_timer_header = .TRUE.
            ENDIF
            n = timer_id
            WRITE(output_unit,'(1x,i4,2x,a24,a1,1x,2(f10.4,i8,i12,4x))') &
               n, timer(n)%label, timer(n)%runflag, &
               sum_wtime(n), mpi_rank_local, TIMER_COUNT(n), &
               sum_ctime(n), mpi_rank_local, TIMER_COUNT(n)
            CLOSE(output_unit)
!----------
            return
!----------
         endif

!-----------------------------------------------------
! local output
!-----------------------------------------------------
         IF (TIMER_Debug >= 2) THEN
            OPEN(output_unit, file=TRIM(file_name), form="FORMATTED", &
               status="UNKNOWN", position="APPEND")

            WRITE(output_unit,*)''
            WRITE(output_unit,*)' =================================='
            WRITE(output_unit,*)' ', TRIM(app_name)
            WRITE(output_unit,*)' Local processor times '
            WRITE(output_unit,*)' =================================='
            WRITE(output_unit,*)''

            do n = 1,ntimer
               IF (.NOT.single_timer_header) THEN
                  WRITE(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x))') &
                     ' wtime ','on pe','count',' ctime ','on pe','count'
                  single_timer_header = .TRUE.
               ENDIF
               WRITE(output_unit,'(1x,i4,2x,a24,a1,1x,2(f10.4,i8,i12,4x))') &
                  n, timer(n)%label, timer(n)%runflag, &
                  sum_wtime(n), mpi_rank_local, TIMER_COUNT(n), &
                  sum_ctime(n), mpi_rank_local, TIMER_COUNT(n)
            enddo

            CLOSE(output_unit)
         ENDIF

!-----------------------------------------------------
! gather global output on mpi_comm_local pes
!-----------------------------------------------------
         if (mpi_size_local > 0) then

            call oasis_mpi_max(ntimer,ntimermax,mpi_comm_local,string='ntimer',all=.true.)

            allocate (sum_ctime_global_tmp(ntimermax, mpi_size_local), &
                      sum_wtime_global_tmp(ntimermax, mpi_size_local), stat=ierror)
            IF ( ierror /= 0 ) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error sum_global_tmp'
            allocate (count_global_tmp(ntimermax, mpi_size_local), stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error count_global_tmp'
            allocate (label_global_tmp(ntimermax, mpi_size_local), stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error label_global_tmp'

            sum_ctime_global_tmp = 0.0
            sum_wtime_global_tmp = 0.0
            count_global_tmp = 0
            label_global_tmp = ' '

            ! gathering of timer values on root process

! tcraig, causes memory failure on corail for some reason
!            call MPI_Gather(sum_ctime(1), ntimermax, MPI_DOUBLE_PRECISION, sum_ctime_global_tmp(1,1), &
!                            ntimermax, MPI_DOUBLE_PRECISION, root, mpi_comm_local, ierror)
!            call MPI_Gather(sum_wtime(1), ntimermax, MPI_DOUBLE_PRECISION, sum_wtime_global_tmp(1,1), &
!                            ntimermax, MPI_DOUBLE_PRECISION, root, mpi_comm_local, ierror)
!            call MPI_Gather(count(1), ntimermax, MPI_INTEGER, count_global_tmp(1,1), &
!                            ntimermax, MPI_INTEGER, root, mpi_comm_local, ierror)

! tcraig, this doesn't work either
!            allocate(rarr(ntimermax),stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: allocate error rarr'
!            rarr(1:ntimermax) = sum_ctime(1:ntimermax)
!            call MPI_Gather(rarr,ntimermax,MPI_DOUBLE_PRECISION,sum_ctime_global_tmp,ntimermax,MPI_DOUBLE_PRECISION,root,mpi_comm_local,ierror)
!            rarr(1:ntimermax) = sum_wtime(1:ntimermax)
!            call MPI_Gather(rarr,ntimermax,MPI_DOUBLE_PRECISION,sum_wtime_global_tmp,ntimermax,MPI_DOUBLE_PRECISION,root,mpi_comm_local,ierror)
!            deallocate(rarr,stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: deallocate error rarr'
!
!            allocate(iarr(ntimermax),stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: allocate error iarr'
!            iarr(1:ntimermax) = count(1:ntimermax)
!            call MPI_Gather(iarr,ntimermax,MPI_INTEGER,count_global_tmp,ntimermax,MPI_INTEGER,root,mpi_comm_local,ierror)
!            deallocate(iarr,stat=ierror)
!            if ( ierror /= 0 ) write(nulprt,*) subname,' WARNING: deallocate error iarr'

! tcraig this works but requires lots of gather calls, could be better
            allocate(rarr(mpi_size_local),iarr(mpi_size_local),carr(mpi_size_local),stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error rarr'
            do n = 1,ntimermax
               cval = timer(n)%label
               carr(:) = ' '
               call MPI_Gather(cval,len(cval),MPI_CHARACTER,carr(1),len(cval),&
                               MPI_CHARACTER,root,mpi_comm_local,ierror)
               if (mpi_rank_local == root) then
                  do m = 1,mpi_size_local
                     label_global_tmp(n,m) = trim(carr(m))
                  enddo
               endif

               rval = sum_ctime(n)
               call MPI_Gather(rval,1,MPI_DOUBLE_PRECISION,rarr(1),1,MPI_DOUBLE_PRECISION,&
                               root,mpi_comm_local,ierror)
               if (mpi_rank_local == root) then
                  sum_ctime_global_tmp(n,1:mpi_size_local) = rarr(1:mpi_size_local)
               endif

               rval = sum_wtime(n)
               call MPI_Gather(rval,1,MPI_DOUBLE_PRECISION,rarr(1),1,MPI_DOUBLE_PRECISION,&
                               root,mpi_comm_local,ierror)
               if (mpi_rank_local == root) then
                  sum_wtime_global_tmp(n,1:mpi_size_local) = rarr(1:mpi_size_local)
               endif

               ival = timer_count(n)
               call MPI_Gather(ival,1,MPI_INTEGER,iarr(1),1,MPI_INTEGER,root,&
                               mpi_comm_local,ierror)
               if (mpi_rank_local == root) then
                  count_global_tmp(n,1:mpi_size_local) = iarr(1:mpi_size_local)
               endif
            enddo
            deallocate(rarr,iarr,carr,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error rarr'

            ! now sort all the timers out by names

            allocate(carr(ntimermax*mpi_size_local),stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error carr'
            nlabels = 0
            do n = 1,ntimermax
            do m = 1,mpi_size_local
               found = .false.
               do k = 1,nlabels
                  if (trim(label_global_tmp(n,m)) == trim(carr(k))) found = .true.
                  if (trim(label_global_tmp(n,m)) == '') found = .false.
               enddo
               if (.not.found) then
                  nlabels = nlabels + 1
                  carr(nlabels) = trim(label_global_tmp(n,m))
               endif
            enddo
            enddo

            allocate(label_list(nlabels),stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error label_list'
            do k = 1,nlabels
               label_list(k) = trim(carr(k))
            enddo
            deallocate(carr,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error carr'
            allocate(sum_ctime_global(nlabels,mpi_size_local),stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error sum_ctime_global'
            allocate(sum_wtime_global(nlabels,mpi_size_local),stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error sum_wtime_global'
            allocate(count_global(nlabels,mpi_size_local),stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: allocate error count_global'

            sum_ctime_global = 0
            sum_wtime_global = 0
            count_global = 0

            do k = 1,nlabels
            do m = 1,ntimermax
            do n = 1,mpi_size_local
               if (trim(label_list(k)) == trim(label_global_tmp(m,n))) then
                  sum_ctime_global(k,n) = sum_ctime_global_tmp(m,n)
                  sum_wtime_global(k,n) = sum_wtime_global_tmp(m,n)
                  count_global(k,n) = count_global_tmp(m,n)
               endif
            enddo
            enddo
            enddo

            deallocate(label_global_tmp,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error label_global_tmp'
            deallocate(sum_ctime_global_tmp,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error sum_ctime_global_tmp'
            deallocate(sum_wtime_global_tmp,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error sum_wtime_global_tmp'
            deallocate(count_global_tmp,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error count_global'

         endif ! (mpi_size_local > 1)

!-----------------------------------------------------
! write global output on root of mpi_comm_local 
!-----------------------------------------------------
         if (TIMER_Debug >= 2 .and. mpi_rank_local == root) then
            OPEN(output_unit, file=TRIM(file_name), form="FORMATTED", &
               status="UNKNOWN", position="APPEND")

            if (onetimer) then
               IF (.NOT.single_timer_header) THEN
                  WRITE(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x))') &
                     'mintime','on pe','count','maxtime','on pe','count'
                  single_timer_header = .TRUE.
               ENDIF
               n = 0
               do k = 1,nlabels
                  if (trim(timer_label) == trim(label_list(k))) n = k
               enddo
               if (n < 1) then
                  write(nulprt,*) subname,' model :',compid,' proc :',&
                  mpi_rank_local,':',' WARNING: invalid timer_label',trim(timer_label)
                  CALL oasis_flush(nulprt)
                  return
               endif
               mintime = sum_ctime_global(n,1)
               minpe = 1
               maxtime = sum_ctime_global(n,1)
               maxpe = 1
               do k = 1,mpi_size_local
                  if (sum_ctime_global(n,k) < mintime) then
                     mintime = sum_ctime_global(n,k)
                     minpe = k
                  endif
                  if (sum_ctime_global(n,k) > maxtime) then
                     maxtime = sum_ctime_global(n,k)
                     maxpe = k
                  endif
               enddo
               WRITE(output_unit,'(1x,i4,2x,a24,a1,1x,2(f10.4,i8,i12,4x))') &
                  n, label_list(n), timer(n)%runflag, &
                  sum_ctime_global(n,minpe), minpe, count_global(n,minpe), &
                  sum_ctime_global(n,maxpe), maxpe, count_global(n,maxpe)

            else
               single_timer_header = .FALSE.

               WRITE(output_unit,*)''
               WRITE(output_unit,*)' =================================='
               WRITE(output_unit,*)' ', TRIM(app_name)
               WRITE(output_unit,*)' Overall Elapsed Min/Max statistics'
               WRITE(output_unit,*)' =================================='
               WRITE(output_unit,*)''
               WRITE(output_unit,'(32x,2(2x,a,5x,a,6x,a,4x),a,3x)') &
                  'mintime','on pe','count','maxtime','on pe','count','meantime'

               DO n = 1,nlabels
                  mintime = 1.0e36
                  minpe = -1
                  maxtime = -1.0e36
                  maxpe = -1
                  meantime = 0.0
                  mcnt = 0
                  do k = 1,mpi_size_local
                     if (count_global(n,k) > 0) then
                        meantime = meantime + sum_wtime_global(n,k)
                        mcnt = mcnt + 1
                        if (sum_wtime_global(n,k) < mintime) then
                           mintime = sum_wtime_global(n,k)
                           minpe = k
                        endif
                        if (sum_wtime_global(n,k) > maxtime) then
                           maxtime = sum_wtime_global(n,k)
                           maxpe = k
                        endif
                     endif
                  enddo
                  if (mcnt > 0) meantime = meantime / float(mcnt)
                  WRITE(output_unit,'(1x,i4,2x,a24,a1,1x,2(f10.4,i8,i12,4x),f10.4)') &
                     n, label_list(n), timer(n)%runflag, &
                     sum_wtime_global(n,minpe), minpe-1, count_global(n,minpe), &
                     sum_wtime_global(n,maxpe), maxpe-1, count_global(n,maxpe), &
                     meantime
               ENDDO

               IF (TIMER_Debug >= 3) THEN
                  WRITE(output_unit,*)''
                  WRITE(output_unit,*)' =================================='
                  WRITE(output_unit,*)' ', TRIM(app_name)
                  WRITE(output_unit,*)' Overall Count statistics'
                  WRITE(output_unit,*)' =================================='
                  WRITE(output_unit,*)''
                  DO k=1,mpi_size_local
                     WRITE(output_unit,'(a)',advance="NO") " P r o c e s s o r    ----------> "
                     WRITE(output_unit,'(3x,i8,5x)')(k-1)
                     DO n = 1, nlabels
                        WRITE(output_unit,'(1x,i8,2x,a24,a1,1x,(i10))') n, label_list(n), &
                                          timer(n)%runflag, (count_global(n,k))
                     ENDDO
                  ENDDO
                  WRITE(output_unit,*)''
                  WRITE(output_unit,*)' =================================='
                  WRITE(output_unit,*)' ', TRIM(app_name)
                  WRITE(output_unit,*)' Overall CPU time statistics'
                  WRITE(output_unit,*)' =================================='
                  WRITE(output_unit,*)''
                  DO k=1,mpi_size_local
                     WRITE(output_unit,'(a)',advance="NO") " P r o c e s s o r    ----------> "
                     WRITE(output_unit,'(3x,i8,5x)')(k-1)
                     DO n = 1, nlabels
                        WRITE(output_unit,'(1x,i8,2x,a24,a1,1x,(f10.4))') n, label_list(n), timer(n)%runflag, &
                                         (sum_ctime_global(n,k))
                     ENDDO
                  ENDDO
                  WRITE(output_unit,*)''
                  WRITE(output_unit,*)' ======================================'
                  WRITE(output_unit,*)' ', TRIM(app_name)
                  WRITE(output_unit,*)' Overall Elapsed time statistics'
                  WRITE(output_unit,*)' ======================================'
                  WRITE(output_unit,*)''
                  DO k=1,mpi_size_local
                     WRITE(output_unit,'(a)',advance="NO") " P r o c e s s o r    ----------> "
                     WRITE(output_unit,'(3x,i8,5x)')(k-1)
                     DO n = 1, nlabels
                        WRITE(output_unit,'(1x,i8,2x,a24,a1,1x,(f10.4))') n, label_list(n), timer(n)%runflag, &
                                           (sum_wtime_global(n,k))
                     ENDDO
                  ENDDO
                  WRITE(output_unit,*)''
                  WRITE(output_unit,*)' ======================================'
               ENDIF

            endif ! (onetimer)

            CLOSE(output_unit)

            deallocate (sum_ctime_global, stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error sum_ctime_global'
            deallocate (sum_wtime_global, stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error sum_wtime_global'
            deallocate (count_global,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error count_global'
            deallocate (label_list,stat=ierror)
            if ( ierror /= 0 ) write(nulprt,*) subname,' model :',compid,' proc :',&
               mpi_rank_local,':',' WARNING: deallocate error label_list'

         endif ! (mpi_rank_local == root)


      end subroutine oasis_timer_print

! --------------------------------------------------------------------------------
      subroutine oasis_timer_c2i(tname,tid)

         character(len=*),intent(in)  :: tname
         integer         ,intent(out) :: tid

         integer :: n

         tid = -1
         do n = 1,ntimer
            if (trim(tname) == trim(timer(n)%label)) tid = n
         enddo

      end subroutine oasis_timer_c2i

! --------------------------------------------------------------------------------
end module mod_oasis_timer
