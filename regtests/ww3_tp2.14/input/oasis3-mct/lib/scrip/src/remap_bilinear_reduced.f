C****
C               *****************************
C               * OASIS MODULE  -  LEVEL ? *
C               * -------------     ------- *
C               *****************************
C
C**** remap_bilinear_reduced - calculate reduced grid bilinear remapping
C
C     Purpose:
C     -------
C     Adaptation of SCRIP 1.4 remap_bilinear module to calculate 
C     bilinear remapping for reduced grids.
C
C**   Interface:
C     ---------
C       *CALL*  *remap_bilin_reduced*
C
C     Input:
C     -----
C
C     Output:
C     ------
C
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       2.5       D. Declat      2002/07     created
C       2.5       S. Valcke      2002/09     modified
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this module contains necessary routines for performing an 
!     bilinear interpolation on reduced grids.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_bilinear_reduced.f 2826 2010-12-10 11:14:21Z valcke $
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

      module remap_bilinear_reduced

!-----------------------------------------------------------------------

      use kinds_mod     ! defines common data types
      use constants     ! defines common constants
      use grids         ! module containing grid info
      use remap_vars    ! module containing remap info
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------

      integer (kind=int_kind), parameter ::
     &    max_iter = 100   ! max iteration count for i,j iteration

      real (kind=dbl_kind), parameter ::
     &     converge = 1.e-10_dbl_kind  ! convergence criterion

!***********************************************************************

      contains

!***********************************************************************

      subroutine remap_bilin_reduced (lextrapdone)

!-----------------------------------------------------------------------
!
!     this routine computes the weights for a bilinear interpolation.
!
!-----------------------------------------------------------------------

      LOGICAL ::
     &           lextrapdone   ! logical, true if EXTRAP done on field
      LOGICAL :: ll_nnei        ! true (default) if extra search is done

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, icount, i,
     &     dst_add,        ! destination address
     &     iter,           ! iteration counter
     &     nmap           ! index of current map being computed

      integer (kind=int_kind), dimension(4) :: 
     &     src_add         ! address for the four source points

      real (kind=dbl_kind), dimension(4)  ::
     &     src_lats,       ! latitudes  of four bilinear corners
     &     src_lons,       ! longitudes of four bilinear corners
     &     wgts            ! bilinear weights for four corners

      real (kind=dbl_kind) ::
     &     plat, plon,       ! lat/lon coords of destination point
     &     iguess, jguess,   ! current guess for bilinear coordinate
     &     deli, delj,       ! corrections to i,j
     &     dth1, dth2, dth3, ! some latitude  differences
     &     dph1, dph2, dph3, ! some longitude differences
     &     dthp, dphp,       ! difference between point and sw corner
     &     mat1, mat2, mat3, mat4, ! matrix elements
     &     determinant,       ! matrix determinant
     &     sum_wgts          ! sum of weights for normalization

      real (kind=dbl_kind) ::  
     &      coslat_dst, sinlat_dst, coslon_dst, sinlon_dst,
     &      dist_min, distance, ! for computing dist-weighted avg
     &      src_latsnn, arg

      integer (kind=int_kind) :: min_add, max_add, srch_add, src_addnn

!-----------------------------------------------------------------------
!
!     compute mappings from grid1 to grid2
!
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)
     &       'Entering routine remap_bilin_reduced'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      ll_nnei = .true.
      nmap = 1
      if (grid1_rank /= 2) then
          stop 'Can not do bilinear interpolation when grid_rank /= 2'
      endif

      !***
      !*** loop over destination grid 
      !***

      grid_loop1: do dst_add = 1, grid2_size

         if (.not. grid2_mask(dst_add)) cycle grid_loop1

         plat = grid2_center_lat(dst_add)
         plon = grid2_center_lon(dst_add)

        !***
        !*** find nearest square of grid points on source grid
        !***

        call grid_search_bilin_rd(src_add, src_lats, src_lons, 
     &                            min_add, max_add,
     &                            plat, plon, grid1_dims,
     &                            grid1_center_lat, grid1_center_lon,
     &                            lextrapdone)
        if (src_add(1) > 0) THEN

        !***
        !*** if the 4 surrounding points have been found and are 
        !*** non-masked, do bilinear interpolation
        !***

          grid2_frac(dst_add) = one

          !***
          !*** iterate to find i,j for bilinear approximation
          !***

          dth1 = src_lats(2) - src_lats(1)
          dth2 = src_lats(4) - src_lats(1)
          dth3 = src_lats(3) - src_lats(2) - dth2

          dph1 = src_lons(2) - src_lons(1)
          dph2 = src_lons(4) - src_lons(1)
          dph3 = src_lons(3) - src_lons(2)

          if (dph1 >  three*pih) dph1 = dph1 - pi2
          if (dph2 >  three*pih) dph2 = dph2 - pi2
          if (dph3 >  three*pih) dph3 = dph3 - pi2
          if (dph1 < -three*pih) dph1 = dph1 + pi2
          if (dph2 < -three*pih) dph2 = dph2 + pi2
          if (dph3 < -three*pih) dph3 = dph3 + pi2

          dph3 = dph3 - dph2

          iguess = half
          jguess = half

          iter_loop1: do iter=1,max_iter

            dthp = plat - src_lats(1) - dth1*iguess -
     &                    dth2*jguess - dth3*iguess*jguess
            dphp = plon - src_lons(1)

            if (dphp >  three*pih) dphp = dphp - pi2
            if (dphp < -three*pih) dphp = dphp + pi2

            dphp = dphp - dph1*iguess - dph2*jguess - 
     &                    dph3*iguess*jguess

            mat1 = dth1 + dth3*jguess
            mat2 = dth2 + dth3*iguess
            mat3 = dph1 + dph3*jguess
            mat4 = dph2 + dph3*iguess

            determinant = mat1*mat4 - mat2*mat3

            deli = (dthp*mat4 - mat2*dphp)/determinant
            delj = (mat1*dphp - dthp*mat3)/determinant

            if (abs(deli) < converge .and. 
     &          abs(delj) < converge) exit iter_loop1

            iguess = iguess + deli
            jguess = jguess + delj

          end do iter_loop1

          if (iter <= max_iter) then

            !***
            !*** successfully found i,j - compute weights
            !***

            wgts(1) = (one-iguess)*(one-jguess)
            wgts(2) = iguess*(one-jguess)
            wgts(3) = iguess*jguess
            wgts(4) = (one-iguess)*jguess
            call store_link_bilin(dst_add, src_add, wgts, nmap)
          else
            write(nulou,*) 'Point coords: ',plat,plon
            write(nulou,*) 'Dest grid lats: ',src_lats
            write(nulou,*) 'Dest grid lons: ',src_lons
            write(nulou,*) 'Dest grid addresses: ',src_add
            write(nulou,*) 'Current i,j : ',iguess, jguess
            write(nulou,*) 
     &          'Iteration for i,j exceed max iteration count'
            stop 
          endif

        else if (src_add(1) < 0) THEN

          !***
          !*** We are in the first or last bin or at least one of the 4
          !*** neighbours was masked. Do distance-weighted average using
          !*** the non-masked points among the 4 closest ones. 

          IF (nlogprt .eq. 2) then
              WRITE(nulou,*) ' '
              WRITE(nulou,*) 
     &   'WARNING: Cannot make bilinear interpolation for target point'
     &            ,dst_add
              WRITE(nulou,*) 
     &    'Using non-masked points among 4 nearest neighbors.'
              CALL OASIS_FLUSH_SCRIP(nulou)
          ENDIF

          !***
          !*** Find the 4 closest points
          !***

          coslat_dst = cos(plat)
          sinlat_dst = sin(plat)
          coslon_dst = cos(plon)
          sinlon_dst = sin(plon)
          src_add = 0
          dist_min = bignum
          src_lats = bignum
          IF (min_add == 0) min_add = 1
          IF (max_add > grid1_size) max_add = grid1_size
          do srch_add = min_add,max_add
            arg = coslat_dst*cos(grid1_center_lat(srch_add))*
     &           (coslon_dst*cos(grid1_center_lon(srch_add)) +
     &            sinlon_dst*sin(grid1_center_lon(srch_add)))+
     &            sinlat_dst*sin(grid1_center_lat(srch_add))
            IF (arg < -1.0d0) THEN
                arg = -1.0d0
            ELSE IF (arg > 1.0d0) THEN
                arg = 1.0d0
            END IF    
            distance=acos(arg)

            if (distance < dist_min) then
                sort_loop: do n=1,4
                if (distance < src_lats(n)) then
                    do i=4,n+1,-1
                      src_add (i) = src_add (i-1)
                      src_lats(i) = src_lats(i-1)
                    end do
                    src_add (n) = srch_add
                    src_lats(n) = distance
                    dist_min = src_lats(4)
                    exit sort_loop
                endif
                end do sort_loop
            endif
          end do

          src_lons = one/(src_lats + tiny)
          distance = sum(src_lons)
          src_lats = src_lons/distance

          !***
          !*** Among 4 closest points, keep only the non-masked ones
          !***

          icount = 0
          do n=1,4
            if (grid1_mask(src_add(n)) .or.
     &          (.not. grid1_mask(src_add(n)) .and. lextrapdone)) then
                icount = icount + 1
            else
                src_lats(n) = zero
            endif
          end do

          if (icount > 0) then
              !*** renormalize weights
              sum_wgts = sum(src_lats)
              wgts(1) = src_lats(1)/sum_wgts
              wgts(2) = src_lats(2)/sum_wgts
              wgts(3) = src_lats(3)/sum_wgts
              wgts(4) = src_lats(4)/sum_wgts

              grid2_frac(dst_add) = one
              call store_link_bilin(dst_add, src_add, wgts, nmap)
          ELSE
              IF (ll_nnei .eqv. .true. ) then
              IF (nlogprt .ge. 2) THEN
                  WRITE(nulou,*) '  '
                  WRITE(nulou,*) 
     &                'All 4 surrounding source grid points are masked'
                  WRITE(nulou,*) 'for target point ',dst_add
                  WRITE(nulou,*) 'with longitude and latitude', 
     &                plon, plat
                  WRITE(nulou,*) 
     &                'Using the nearest non-masked neighbour.'
                  CALL OASIS_FLUSH_SCRIP(nulou)
              ENDIF
              src_latsnn = bignum
!cdir novector
              do srch_add = min_add,max_add
                if (grid1_mask(srch_add) .or.
     &          (.not. grid1_mask(srch_add) .and. lextrapdone)) THEN
                    arg = coslat_dst*cos(grid1_center_lat(srch_add))*
     &                  (coslon_dst*cos(grid1_center_lon(srch_add)) +
     &                  sinlon_dst*sin(grid1_center_lon(srch_add)))+
     &                  sinlat_dst*sin(grid1_center_lat(srch_add))
                    IF (arg < -1.0d0) THEN
                        arg = -1.0d0
                    ELSE IF (arg > 1.0d0) THEN
                        arg = 1.0d0
                    END IF
                    distance=acos(arg)

                    if (distance < src_latsnn) then
                        src_addnn = srch_add
                        src_latsnn = distance
                    endif
                endif
              end DO
              IF (nlogprt .ge. 2) THEN
                  WRITE(nulou,*) 
     &                'Nearest non masked neighbour is source point '
     &                ,src_addnn
                  WRITE(nulou,*) 'with longitude and latitude',
     &                grid1_center_lon(src_addnn), 
     &                grid1_center_lat(src_addnn) 
                  WRITE(nulou,*) '  '
                  CALL OASIS_FLUSH_SCRIP(nulou)
              ENDIF
              wgts(1) = 1.
              wgts(2) = 0.
              wgts(3) = 0.
              wgts(4) = 0.
              src_add(1) = src_addnn
              src_add(2) = 0
              src_add(3) = 0
              src_add(4) = 0

              grid2_frac(dst_add) = one
              call store_link_bilin(dst_add, src_add, wgts, nmap)
          endif
          ENDIF
      ENDIF
      end do grid_loop1
!
!-----------------------------------------------------------------------

      end subroutine remap_bilin_reduced

!***********************************************************************

      subroutine grid_search_bilin_rd(src_add, src_lats, src_lons,
     &                                min_add, max_add,
     &                                plat, plon, src_grid_dims,
     &                                src_center_lat, src_center_lon,
     &                                lextrapdone)

!-----------------------------------------------------------------------
!
!     this routine finds the location of the search point plat, plon
!     in the source grid and returns the corners needed for a bilinear
!     interpolation. The target grid is a reduced grid.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(4), intent(out) ::
     &        src_add  ! address of each corner point enclosing P

      real (kind=dbl_kind), dimension(4), intent(out) ::
     &        src_lats, ! latitudes  of the four corner points
     &        src_lons  ! longitudes of the four corner points

      integer (kind=int_kind) :: min_add, max_add

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      real (kind=dbl_kind), intent(in) ::
     &        plat,   ! latitude  of the search point
     &        plon    ! longitude of the search point

      integer (kind=int_kind), dimension(2), intent(in) ::
     &        src_grid_dims  ! size of each src grid dimension

      real (kind=dbl_kind), dimension(:), intent(in) ::
     &        src_center_lat, ! latitude  of each src grid center 
     &        src_center_lon  ! longitude of each src grid center

      LOGICAL ::
     &           lextrapdone   ! logical, true if EXTRAP done on field

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, next_n, srch_add, ni,  ! dummy indices
     &    nx, ny, ntotmask,           ! dimensions of src grid
     &    inter_add ! add for restricting search
!
      integer (kind=int_kind), DIMENSION(4) :: src_bid

!-----------------------------------------------------------------------
!
!     restrict search first using bins
!
!-----------------------------------------------------------------------

      nx = src_grid_dims(1)
      inter_add = 0

      src_add = 0

      min_add = size(src_center_lat) + 1
      max_add = 1
      if (plat >= bin_lats_r(1,1)) then
          min_add = 0
          max_add = bin_addr1_r(4,1)
          inter_add = bin_addr1_r(3,1)
      else if (plat <= bin_lats_r(1,num_srch_red)) then
          max_add = nx + 1
          min_add = bin_addr1_r(1,num_srch_red)
          inter_add = bin_addr1_r(3,num_srch_red)
      else
          srch_loopred: do n=1,num_srch_red
            if (plat <= bin_lats_r(1,n) 
     &        .and. plat >= bin_lats_r(2,n)) then
                min_add = bin_addr1_r(1,n)
                max_add = bin_addr1_r(4,n)
                inter_add = bin_addr1_r(3,n)
                exit srch_loopred
            endif
          end DO srch_loopred
      ENDIF

!-----------------------------------------------------------------------
!
!     now perform a more detailed search 
!
!-----------------------------------------------------------------------
      if (min_add .ne. 0 .and. max_add .ne. nx+1) THEN
         !*** The concerned bins are not the top north or south ones.
         !*** We should be able to find the four corners
         !*** for the bilinear interpolation.

          IF ( plon <= src_center_lon(min_add) ) THEN
              src_add(1) = inter_add-1
              src_add(2) = min_add
          ELSE IF ( plon > src_center_lon(inter_add-1) ) then
              src_add(1) = inter_add-1
              src_add(2) = min_add
          else
              srch_loop2: do srch_add = min_add, inter_add-2
                 if ( (plon > src_center_lon(srch_add)) .and.
     &            (plon <= src_center_lon(srch_add+1)) )then
                     src_add(1) = srch_add
                     src_add(2) = srch_add+1
                     exit srch_loop2
                 endif
               end do srch_loop2
           ENDIF
           IF ( plon <= src_center_lon(inter_add) ) THEN
               src_add(3) = max_add
               src_add(4) = inter_add
           ELSE IF ( plon >= src_center_lon(max_add) ) then
               src_add(3) = max_add
               src_add(4) = inter_add
           else
               srch_loop3: do srch_add = inter_add, max_add
                 if ( (plon >= src_center_lon(srch_add)) .and.
     &             (plon <= src_center_lon(srch_add+1)) )then
                      src_add(3) = srch_add
                      src_add(4) = srch_add+1
                      exit srch_loop3
                  endif
               enddo srch_loop3
           ENDIF
           src_lats(1) = src_center_lat(src_add(3))
           src_lats(2) = src_center_lat(src_add(4))
           src_lats(3) = src_center_lat(src_add(2))
           src_lats(4) = src_center_lat(src_add(1))
      
           src_lons(1) = src_center_lon(src_add(3))
           src_lons(2) = src_center_lon(src_add(4))
           src_lons(3) = src_center_lon(src_add(2))
           src_lons(4) = src_center_lon(src_add(1))

           src_bid=src_add

           src_add(1) = src_bid(3)
           src_add(2) = src_bid(4)
           src_add(3) = src_bid(2)
           src_add(4) = src_bid(1)
    
           ! Check if one point is masked; IF so, nearest-neighbour
           ! interpolation will be used 

           ntotmask = 0
           do ni=1,4
             if (.not. grid1_mask(src_add(ni)).and. 
     &           .not. lextrapdone) 
     &           ntotmask = ntotmask + 1 
           end DO
           IF (ntotmask .gt. 0) src_add(1) = -src_add(1) 
          
       ELSE 

           !*** We are in the first or last bin.  Put src_add = -1 so that
           !*** distance-weighted average of the 4 non-masked closest points
           !*** is done in calling routine. 

           src_add = -1

       ENDIF

!-----------------------------------------------------------------------

      end subroutine grid_search_bilin_rd 

!***********************************************************************

      subroutine store_link_bilin(dst_add, src_add, weights, nmap)

!-----------------------------------------------------------------------
!
!     this routine stores the address and weight for four links 
!     associated with one destination point in the appropriate address 
!     and weight arrays and resizes those arrays if necessary.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) ::
     &        dst_add,  ! address on destination grid
     &        nmap      ! identifies which direction for mapping

      integer (kind=int_kind), dimension(4), intent(in) ::
     &        src_add   ! addresses on source grid

      real (kind=dbl_kind), dimension(4), intent(in) ::
     &        weights ! array of remapping weights for these links

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, ! dummy index
     &       num_links_old          ! placeholder for old link number

!-----------------------------------------------------------------------
!
!     increment number of links and check to see if remap arrays need
!     to be increased to accomodate the new link.  then store the
!     link.
!
!-----------------------------------------------------------------------

      select case (nmap)
      case(1)

        num_links_old  = num_links_map1
        num_links_map1 = num_links_old + 4

        if (num_links_map1 > max_links_map1) 
     &     call resize_remap_vars(1,resize_increment)

        do n=1,4
          grid1_add_map1(num_links_old+n) = src_add(n)
          grid2_add_map1(num_links_old+n) = dst_add
          wts_map1    (1,num_links_old+n) = weights(n)
        end do

      case(2)

        num_links_old  = num_links_map2
        num_links_map2 = num_links_old + 4

        if (num_links_map2 > max_links_map2) 
     &     call resize_remap_vars(2,resize_increment)

        do n=1,4
          grid1_add_map2(num_links_old+n) = dst_add
          grid2_add_map2(num_links_old+n) = src_add(n)
          wts_map2    (1,num_links_old+n) = weights(n)
        end do

      end select

!-----------------------------------------------------------------------

      end subroutine store_link_bilin

!***********************************************************************

      end module remap_bilinear_reduced

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
