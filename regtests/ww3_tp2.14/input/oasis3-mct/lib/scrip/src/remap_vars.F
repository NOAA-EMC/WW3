C****
C                    ************************
C                    *     OASIS MODULE     *
C                    *     ------------     *
C                    ************************
C****
C***********************************************************************
C     This module belongs to the SCRIP library. It is modified to run
C     within OASIS. 
C     Modifications:
C       - introduction of logical flags to allow multiple calls of SCRIP
C       - deallocation of arrays not needed any more
C       - added CASE for GAUSWGT
C
C     Modified by            V. Gayler,  M&D                  20.09.2001
C     Modified by            D. Declat,  CERFACS              27.06.2002
C***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this module contains necessary variables for remapping between
!     two grids.  also routines for resizing and initializing these
!     variables.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_vars.F 2826 2010-12-10 11:14:21Z valcke $
!
!     Copyright (c) 1997, 1998 the Regents of the University of 
!       California.
!
!     This software and ancillary information (herein called software) 
!     called SCRIP is made available under the terms described here.  
!     The software has been approved for release with associated 
!     LA-CC Number 98-45.
!
!     Unless otherwise indicated, this software has been authored
!     by an employee or employees of the University of California,
!     operator of the Los Alamos National Laboratory under Contract
!     No. W-7405-ENG-36 with the U.S. Department of Energy.  The U.S.
!     Government has rights to use, reproduce, and distribute this
!     software.  The public may copy and use this software without
!     charge, provided that this Notice and any statement of authorship
!     are reproduced on all copies.  Neither the Government nor the
!     University makes any warranty, express or implied, or assumes
!     any liability or responsibility for the use of this software.
!
!     If software is modified to produce derivative works, such modified
!     software should be clearly marked, so as not to confuse it with 
!     the version available from Los Alamos National Laboratory.
!
!***********************************************************************

      module remap_vars

      use kinds_mod
      use constants
      use grids
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     module variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), parameter ::
     &      norm_opt_none    = 1
     &,     norm_opt_dstarea = 2
     &,     norm_opt_frcarea = 3
     &,     norm_opt_nonorm  = 4

      integer (kind=int_kind), parameter ::
     &      map_type_conserv  = 1
     &,     map_type_bilinear = 2
     &,     map_type_bicubic  = 3
     &,     map_type_distwgt  = 4
     &,     map_type_gauswgt  = 5

      integer (kind=int_kind), save :: 
     &      max_links_map1  ! current size of link arrays
     &,     num_links_map1  ! actual number of links for remapping
     &,     max_links_map2  ! current size of link arrays
     &,     num_links_map2  ! actual number of links for remapping
     &,     num_maps        ! num of remappings for this grid pair
     &,     num_wts         ! num of weights used in remapping
     &,     map_type        ! identifier for remapping method
     &,     norm_opt        ! option for normalization (conserv only)
     &,     resize_increment ! default amount to increase array size

      integer (kind=int_kind), dimension(:), allocatable, save ::
     &      grid1_add_map1, ! grid1 address for each link in mapping 1
     &      grid2_add_map1, ! grid2 address for each link in mapping 1
     &      grid1_add_map2, ! grid1 address for each link in mapping 2
     &      grid2_add_map2  ! grid2 address for each link in mapping 2
#ifdef TREAT_OVERLAY
      INTEGER (kind=int_kind), dimension(:), allocatable, save ::
     &      grid1_add_repl1 ! grid1 address to use after overlap calculation
#endif TREAT_OVERLAY
      real (kind=dbl_kind), dimension(:,:), allocatable, save ::
     &      wts_map1, ! map weights for each link (num_wts,max_links)
     &      wts_map2  ! map weights for each link (num_wts,max_links)

      logical (kind=log_kind), save :: lfracnnei = .false.
 
      logical (kind=log_kind), save :: first_conserv = .true. ! flag to 
                                ! indicate, whether scrip is called from
                                ! oasis for the first time
      logical (kind=log_kind), save :: first_call = .true. ! flag used in
                                ! remap_conserve (store_link_cnsrv)

!***********************************************************************

      contains

!***********************************************************************

      subroutine init_remap_vars (id_scripvoi)

!-----------------------------------------------------------------------
!
!     this routine initializes some variables and provides an initial
!     allocation of arrays (fairly large so frequent resizing 
!     unnecessary).
!
!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      INTEGER (kind=int_kind)::
     &     id_scripvoi          ! number of neighbours for DISTWGT and GAUSWGT
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine init_remap_vars'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     determine the number of weights
!
!-----------------------------------------------------------------------

      select case (map_type)
      case(map_type_conserv)
        num_wts = 3
      case(map_type_bilinear)
        num_wts = 1
      case(map_type_bicubic)
          IF (restrict_type == 'REDUCED') THEN
              num_wts = 1
          ELSE
              num_wts = 4
          ENDIF
      case(map_type_distwgt)
        num_wts = 1
      case(map_type_gauswgt)
        num_wts = 1
      end select

!-----------------------------------------------------------------------
!
!     initialize num_links and set max_links to four times the largest 
!     of the destination grid sizes initially (can be changed later).
!     set a default resize increment to increase the size of link
!     arrays if the number of links exceeds the initial size
!   
!-----------------------------------------------------------------------

      num_links_map1 = 0
      select case (map_type)
      case(map_type_conserv)
          max_links_map1 = 4*grid2_size
      case(map_type_bilinear)
          max_links_map1 = 4*grid2_size
      case(map_type_bicubic)
          IF (restrict_type == 'REDUCED') THEN
              max_links_map1 = 16*grid2_size
          ELSE
              max_links_map1 = 4*grid2_size
          ENDIF
      case(map_type_distwgt)
          max_links_map1 = id_scripvoi*grid2_size
      case(map_type_gauswgt)
          max_links_map1 = id_scripvoi*grid2_size
      END select

      if (num_maps > 1) then
        num_links_map2 = 0
        max_links_map1 = max(4*grid1_size,4*grid2_size)
        max_links_map2 = max_links_map1
      endif

      resize_increment = 0.1*max(grid1_size,grid2_size)

!-----------------------------------------------------------------------
!
!     allocate address and weight arrays for mapping 1
!   
!-----------------------------------------------------------------------

      allocate (grid1_add_map1(max_links_map1),
     &          grid2_add_map1(max_links_map1),
     &          wts_map1(num_wts, max_links_map1))
#ifdef TREAT_OVERLAY
      allocate (grid1_add_repl1(grid1_size))
#endif TREAT_OVERLAY

!-----------------------------------------------------------------------
!
!     allocate address and weight arrays for mapping 2 if necessary 
!   
!-----------------------------------------------------------------------

      if (num_maps > 1) then
        allocate (grid1_add_map2(max_links_map2),
     &            grid2_add_map2(max_links_map2),
     &            wts_map2(num_wts, max_links_map2))
      endif
!-----------------------------------------------------------------------
!
!     initialize flag for routine store_link_cnsrv
!   
!-----------------------------------------------------------------------
      first_call = .true.

!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine init_remap_vars'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      end subroutine init_remap_vars

!***********************************************************************

      subroutine resize_remap_vars(nmap, increment)

!-----------------------------------------------------------------------
!
!     this routine resizes remapping arrays by increasing(decreasing)
!     the max_links by increment
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) ::
     &     nmap,      ! identifies which mapping array to resize
     &     increment  ! the number of links to add(subtract) to arrays

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) ::
     &   ierr,     ! error flag
     &   mxlinks   ! size of link arrays

      integer (kind=int_kind), dimension(:), allocatable ::
     &   add1_tmp, ! temp array for resizing address arrays
     &   add2_tmp  ! temp array for resizing address arrays

      real (kind=dbl_kind), dimension(:,:), allocatable ::
     &   wts_tmp   ! temp array for resizing weight arrays
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)
     &       'Entering routine resize_remap_vars'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     resize map 1 arrays if required.
!
!-----------------------------------------------------------------------

      select case (nmap)
      case(1)

        !***
        !*** allocate temporaries to hold original values
        !***

        mxlinks = size(grid1_add_map1)
        allocate (add1_tmp(mxlinks), add2_tmp(mxlinks), 
     &            wts_tmp(num_wts,mxlinks))

        add1_tmp = grid1_add_map1
        add2_tmp = grid2_add_map1
        wts_tmp  = wts_map1
        
        !***
        !*** deallocate originals and increment max_links then
        !*** reallocate arrays at new size
        !***

        deallocate (grid1_add_map1, grid2_add_map1, wts_map1)
        max_links_map1 = mxlinks + increment
        allocate (grid1_add_map1(max_links_map1),
     &            grid2_add_map1(max_links_map1),
     &            wts_map1(num_wts,max_links_map1))

        !***
        !*** restore original values from temp arrays and
        !*** deallocate temps
        !***

        mxlinks = min(mxlinks, max_links_map1)
        grid1_add_map1(1:mxlinks) = add1_tmp (1:mxlinks)
        grid2_add_map1(1:mxlinks) = add2_tmp (1:mxlinks)
        wts_map1    (:,1:mxlinks) = wts_tmp(:,1:mxlinks)
        deallocate(add1_tmp, add2_tmp, wts_tmp)

!-----------------------------------------------------------------------
!
!     resize map 2 arrays if required.
!
!-----------------------------------------------------------------------

      case(2)

        !***
        !*** allocate temporaries to hold original values
        !***

        mxlinks = size(grid1_add_map2)
        allocate (add1_tmp(mxlinks), add2_tmp(mxlinks), 
     &            wts_tmp(num_wts,mxlinks),stat=ierr)
        if (ierr .ne. 0) THEN
            WRITE(nulou,*) '   '
            WRITE(nulou,*)'error allocating temps in resize: ',ierr
            CALL OASIS_FLUSH_SCRIP(nulou)
            stop
        endif

        add1_tmp = grid1_add_map2
        add2_tmp = grid2_add_map2
        wts_tmp  = wts_map2
        
        !***
        !*** deallocate originals and increment max_links then
        !*** reallocate arrays at new size
        !***

        deallocate (grid1_add_map2, grid2_add_map2, wts_map2)
        max_links_map2 = mxlinks + increment
        allocate (grid1_add_map2(max_links_map2),
     &            grid2_add_map2(max_links_map2),
     &            wts_map2(num_wts,max_links_map2),stat=ierr)
        if (ierr .ne. 0) then
            WRITE(nulou,*) '   '
            WRITE(nulou,*)'error allocating new arrays in resize: ',ierr
            CALL OASIS_FLUSH_SCRIP(nulou)
            stop
        endif


        !***
        !*** restore original values from temp arrays and
        !*** deallocate temps
        !***

        mxlinks = min(mxlinks, max_links_map2)
        grid1_add_map2(1:mxlinks) = add1_tmp (1:mxlinks)
        grid2_add_map2(1:mxlinks) = add2_tmp (1:mxlinks)
        wts_map2    (:,1:mxlinks) = wts_tmp(:,1:mxlinks)
        deallocate(add1_tmp, add2_tmp, wts_tmp)

      end select
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)
     &       'Leaving routine resize_remap_vars'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------

      end subroutine resize_remap_vars
!----------------------------------------------------------------------- 
!----------------------------------------------------------------------- 
      subroutine free_remap_vars
!-----------------------------------------------------------------------
!
!     subroutine to deallocate allocated arrays
!
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine free_remap_vars'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      deallocate (grid1_add_map1, grid2_add_map1, wts_map1)
#ifdef TREAT_OVERLAY
      deallocate (grid1_add_repl1)
#endif TREAT_OVERLAY

      if (num_maps > 1) then
        deallocate (grid1_add_map2, grid2_add_map2, wts_map2)
      endif
      if (map_type == map_type_conserv) then
        first_call = .true.
        first_conserv = .false.
      endif
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine free_remap_vars'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------

      end subroutine free_remap_vars

!***********************************************************************

      end module remap_vars

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
