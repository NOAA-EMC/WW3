C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 1 *
C               * -------------     ------- *
C               *****************************
C****
C***********************************************************************
C     This routine belongs to the SCRIP library. It is modified to run
C     within OASIS.
C     Modifications:
C       - routine does not read namelist but gets parameters from the
C         calling routine scriprmp.f
C       - map-method and noralize-option are written in capital letters
C       - routine grid_init is not called from scrip any more but was
C         called earlier from scriprmp
C       - call of two extra routines: free_grids and free_remap_vars to 
C         allow multiple calls of SCRIP
C       - added case for GAUSWGT 
C       - added 'REDUCED' case for bilinear and bicubic.
C       - hard coded num_maps=1 for USE in OASIS
C       - added lextrapdone argument
C
C     Modified by            V. Gayler,  M&D                  20.09.2001
C     Modified by            D. Declat,  CERFACS              27.06.2002
C     Modified by            S. Valcke,  CERFACS              27.08.2002
C***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This routine is the driver for computing the addresses and weights 
!     for interpolating between two grids on a sphere.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: scrip.f 1831 2009-01-09 17:19:08Z valcke $
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

      subroutine scrip
     $           (interp_file1, map1_name, m_method, n_opt, 
     $            lextrapdone, rl_varmul, id_scripvoi)

!-----------------------------------------------------------------------

      use kinds_mod                  ! module defining data types
      use constants                  ! module for common constants
      use iounits                    ! I/O unit manager
      use timers                     ! CPU timers
      use grids                      ! module with grid information
      use remap_vars                 ! common remapping variables
      use remap_conservative         ! routines for conservative remap
      use remap_distance_weight      ! routines for dist-weight remap
      use remap_gaussian_weight      ! routines for gaus-weight remap
      use remap_bilinear             ! routines for bilinear interp
      use remap_bicubic              ! routines for bicubic  interp
      use remap_bilinear_reduced     ! routines for bilinear interp
      use remap_bicubic_reduced      ! routines for bicubic interp
      use remap_write                ! routines for remap output
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      character (char_len), intent(in) ::
     &           interp_file1, ! filename for output remap data (map1)
     &           map1_name     ! name for mapping from grid1 to grid2

      character*8, intent(in) ::
     &           m_method,     ! choice for mapping method
     &           n_opt         ! option for normalizing weights

      LOGICAL ::
     &           lextrapdone   ! logical, true if EXTRAP done on field

      REAL (kind=real_kind) ::
     &    rl_varmul             ! Gaussian variance (for GAUSWGT)

      INTEGER (kind=int_kind) ::
     &     id_scripvoi          ! number of neighbours for DISTWGT and GAUSWGT

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) ::
     &           n             ! dummy counter

      character (char_len) ::
     &           interp_file2, ! filename for output remap data (map2)
     &           map2_name,    ! name for mapping from grid2 to grid1
     &           output_opt,   ! option for output conventions
     &           map_method,   ! choice for mapping method
     &           normalize_opt ! option for normalizing weights
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine scrip'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     initialize timers
!
!-----------------------------------------------------------------------

      call timers_init
      do n=1,max_timers
        call timer_clear(n)
      end do

!-----------------------------------------------------------------------
!
!     initialize variables of former SCRIP namelist
!
!-----------------------------------------------------------------------

      interp_file2  = 'unknown'
      map2_name     = 'unknown'
      luse_grid1_area = .false.
      luse_grid2_area = .false.
      num_maps      = 1
      output_opt    = 'scrip'

      map_method = m_method
      normalize_opt = n_opt

      select case(map_method)
      case ('CONSERV')
        map_type = map_type_conserv
      case ('BILINEAR')
        map_type = map_type_bilinear
      case ('BICUBIC')
        map_type = map_type_bicubic
      case ('DISTWGT')
        map_type = map_type_distwgt
      case ('GAUSWGT')
        map_type = map_type_gauswgt
      case default
        stop 'unknown mapping method'
      end select

      SELECT CASE (normalize_opt)
      CASE ('FRACNNEI')
          lfracnnei = .true.
      END SELECT

      select case(normalize_opt(1:4))
      case ('NONE')
        norm_opt = norm_opt_none
      case ('FRAC')
        norm_opt = norm_opt_frcarea
      case ('DEST')
        norm_opt = norm_opt_dstarea
      CASE ('NONO')
        norm_opt = norm_opt_nonorm
      case default
        stop 'unknown normalization option'
      end select
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' Computing remappings between: '
     &        ,grid1_name
         WRITE (UNIT = nulou,FMT = *)'                          and  '
     &       ,grid2_name
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     initialize some remapping variables.
!
!-----------------------------------------------------------------------
      call init_remap_vars (id_scripvoi)
!-----------------------------------------------------------------------
!
!     call appropriate interpolation setup routine based on type of
!     remapping requested.
!
!-----------------------------------------------------------------------
      select case(map_type)
      case(map_type_conserv)
          call remap_conserv
      case(map_type_bilinear)
          IF (restrict_TYPE == 'REDUCED') then
              call remap_bilin_reduced (lextrapdone)
          ELSE
              call remap_bilin (lextrapdone)
          endif
      case(map_type_distwgt)
          call remap_distwgt (lextrapdone, id_scripvoi)
      case(map_type_gauswgt)
          call remap_gauswgt (lextrapdone, id_scripvoi, rl_varmul)
      case(map_type_bicubic)
          IF (restrict_TYPE == 'REDUCED') then
              call remap_bicub_reduced (lextrapdone)
          ELSE
              call remap_bicub (lextrapdone)
          endif
       case default
           stop 'Invalid Map Type'
      end select
      
      CALL sort_add(grid2_add_map1, grid1_add_map1, wts_map1,
     $   num_links_map1, num_wts)
      IF (lfracnnei) THEN
          CALL fracnnei(grid1_size, grid2_size, grid1_mask, grid2_mask,
     $        grid1_center_lon, grid1_center_lat,
     $        grid2_center_lon, grid2_center_lat,
     $        num_links_map1, num_wts,
     $        wts_map1, grid1_add_map1, grid2_add_map1)
          lfracnnei = .false.
      ENDIF
!
#ifdef TREAT_OVERLAY
!
! Change address if overlap point were found
      IF (map_type == 1) THEN
          DO n = 1, num_links_map1
            IF (grid1_add_map1(n) .ne. 0) THEN
                grid1_add_map1(n) = grid1_add_repl1(grid1_add_map1(n))
            ENDIF
          END DO
      ENDIF
!
#endif
!
!
!-----------------------------------------------------------------------
!
!     reduce size of remapping arrays and then write remapping info
!     to a file.
!
!-----------------------------------------------------------------------
      if (num_links_map1 /= max_links_map1) then
        call resize_remap_vars(1, num_links_map1-max_links_map1)
      endif
      if ((num_maps > 1) .and. (num_links_map2 /= max_links_map2)) then
        call resize_remap_vars(2, num_links_map2-max_links_map2)
      endif

      call write_remap(map1_name, map2_name, 
     &                 interp_file1, interp_file2, output_opt)

!-----------------------------------------------------------------------
!
!     deallocate allocatable arrays
!
!-----------------------------------------------------------------------

      call free_grids
      call free_remap_vars
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine scrip'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!-----------------------------------------------------------------------!
      end subroutine scrip
!
      subroutine sort_add(add1, add2, weights, num_links, num_wts)

!-----------------------------------------------------------------------
!
!     this routine sorts address and weight arrays based on the
!     destination address with the source address as a secondary
!     sorting criterion.  the method is a standard heap sort.
!
!-----------------------------------------------------------------------

      use kinds_mod     ! defines common data types
      use constants     ! defines common scalar constants
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     Input and Output arrays
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) :: num_links, num_wts
      integer (kind=int_kind), intent(inout), dimension(num_links) ::
     &        add1,       ! destination address array (num_links)
     &        add2        ! source      address array

      real (kind=dbl_kind), intent(inout),
     &    dimension(num_wts, num_links) ::
     &        weights     ! remapping weights (num_wts, num_links)


!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) ::
!     &          num_links,          ! num of links for this mapping
!     &          num_wts,            ! num of weights for this mapping
     &          add1_tmp, add2_tmp, ! temp for addresses during swap
     &          nwgt,
     &          lvl, final_lvl,     ! level indexes for heap sort levels
     &          chk_lvl1, chk_lvl2, max_lvl

      real (kind=dbl_kind), dimension(SIZE(weights,DIM=1)) ::
     &          wgttmp              ! temp for holding wts during swap
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine sort_add'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     determine total number of links to sort and number of weights
!
!-----------------------------------------------------------------------

!      num_links = SIZE(add1)
!      num_wts   = SIZE(weights, DIM=1)

!-----------------------------------------------------------------------
!
!     start at the lowest level (N/2) of the tree and sift lower 
!     values to the bottom of the tree, promoting the larger numbers
!
!-----------------------------------------------------------------------

      do lvl=num_links/2,1,-1

        final_lvl = lvl
        add1_tmp = add1(lvl)
        add2_tmp = add2(lvl)
        wgttmp(:) = weights(:,lvl)

        !***
        !*** loop until proper level is found for this link, or reach
        !*** bottom
        !***

        sift_loop1: do

          !***
          !*** find the largest of the two daughters
          !***

          chk_lvl1 = 2*final_lvl
          chk_lvl2 = 2*final_lvl+1
          if (chk_lvl1 .EQ. num_links) chk_lvl2 = chk_lvl1

          if ((add1(chk_lvl1) >  add1(chk_lvl2)) .OR.
     &       ((add1(chk_lvl1) == add1(chk_lvl2)) .AND.
     &        (add2(chk_lvl1) >  add2(chk_lvl2)))) then
            max_lvl = chk_lvl1
          else 
            max_lvl = chk_lvl2
          endif

          !***
          !*** if the parent is greater than both daughters,
          !*** the correct level has been found
          !***

          if ((add1_tmp .GT. add1(max_lvl)) .OR.
     &       ((add1_tmp .EQ. add1(max_lvl)) .AND.
     &        (add2_tmp .GT. add2(max_lvl)))) then
            add1(final_lvl) = add1_tmp
            add2(final_lvl) = add2_tmp
            weights(:,final_lvl) = wgttmp(:)
            exit sift_loop1

          !***
          !*** otherwise, promote the largest daughter and push
          !*** down one level in the tree.  if haven't reached
          !*** the end of the tree, repeat the process.  otherwise
          !*** store last values and exit the loop
          !***

          else 
            add1(final_lvl) = add1(max_lvl)
            add2(final_lvl) = add2(max_lvl)
            weights(:,final_lvl) = weights(:,max_lvl)

            final_lvl = max_lvl
            if (2*final_lvl > num_links) then
              add1(final_lvl) = add1_tmp
              add2(final_lvl) = add2_tmp
              weights(:,final_lvl) = wgttmp(:)
              exit sift_loop1
            endif
          endif
        end do sift_loop1
      end do

!-----------------------------------------------------------------------
!
!     now that the heap has been sorted, strip off the top (largest)
!     value and promote the values below
!
!-----------------------------------------------------------------------

      do lvl=num_links,3,-1

        !***
        !*** move the top value and insert it into the correct place
        !***

        add1_tmp = add1(lvl)
        add1(lvl) = add1(1)

        add2_tmp = add2(lvl)
        add2(lvl) = add2(1)

        wgttmp(:) = weights(:,lvl)
        weights(:,lvl) = weights(:,1)

        !***
        !*** as above this loop sifts the tmp values down until proper 
        !*** level is reached
        !***

        final_lvl = 1

        sift_loop2: do

          !***
          !*** find the largest of the two daughters
          !***

          chk_lvl1 = 2*final_lvl
          chk_lvl2 = 2*final_lvl+1
          if (chk_lvl2 >= lvl) chk_lvl2 = chk_lvl1

          if ((add1(chk_lvl1) >  add1(chk_lvl2)) .OR.
     &       ((add1(chk_lvl1) == add1(chk_lvl2)) .AND.
     &        (add2(chk_lvl1) >  add2(chk_lvl2)))) then
            max_lvl = chk_lvl1
          else 
            max_lvl = chk_lvl2
          endif

          !***
          !*** if the parent is greater than both daughters,
          !*** the correct level has been found
          !***

          if ((add1_tmp >  add1(max_lvl)) .OR.
     &       ((add1_tmp == add1(max_lvl)) .AND.
     &        (add2_tmp >  add2(max_lvl)))) then
            add1(final_lvl) = add1_tmp
            add2(final_lvl) = add2_tmp
            weights(:,final_lvl) = wgttmp(:)
            exit sift_loop2

          !***
          !*** otherwise, promote the largest daughter and push
          !*** down one level in the tree.  if haven't reached
          !*** the end of the tree, repeat the process.  otherwise
          !*** store last values and exit the loop
          !***

          else 
            add1(final_lvl) = add1(max_lvl)
            add2(final_lvl) = add2(max_lvl)
            weights(:,final_lvl) = weights(:,max_lvl)

            final_lvl = max_lvl
            if (2*final_lvl >= lvl) then
              add1(final_lvl) = add1_tmp
              add2(final_lvl) = add2_tmp
              weights(:,final_lvl) = wgttmp(:)
              exit sift_loop2
            endif
          endif
        end do sift_loop2
      end do

      !***
      !*** swap the last two entries
      !***


      add1_tmp = add1(2)
      add1(2)  = add1(1)
      add1(1)  = add1_tmp

      add2_tmp = add2(2)
      add2(2)  = add2(1)
      add2(1)  = add2_tmp

      wgttmp (:)   = weights(:,2)
      weights(:,2) = weights(:,1)
      weights(:,1) = wgttmp (:)
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine sort_add'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------

      end subroutine sort_add

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
