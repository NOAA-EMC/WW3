C****
C               *****************************
C               * OASIS MODULE  -  LEVEL ? *
C               * -------------     ------- *
C               *****************************
C
C**** remap_bilinear - calculate bilinear remapping
C
C     Purpose:
C     -------
C     Adaptation of SCRIP 1.4 remap_bicubic module to calculate 
C     bicubic remapping.
C
C**   Interface:
C     ---------
C       *CALL*  *remap_bicub**
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
C       2.5       S. Valcke      2002/09     Treatment for masked point
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this module contains necessary routines for performing an 
!     bicubic interpolation.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_bicubic.f 2826 2010-12-10 11:14:21Z valcke $
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

      module remap_bicubic

!-----------------------------------------------------------------------

      use kinds_mod     ! defines common data types
      use constants     ! defines common constants
      use grids         ! module containing grid info
      use remap_vars    ! module containing remap info
      USE mod_oasis_flush

      implicit NONE

!-----------------------------------------------------------------------

      integer (kind=int_kind), parameter ::
     &    max_iter = 100   ! max iteration count for i,j iteration

      real (kind=dbl_kind), parameter ::
     &     converge = 1.e-10_dbl_kind ! convergence criterion

!***********************************************************************

      contains

!***********************************************************************

      subroutine remap_bicub (lextrapdone)

!-----------------------------------------------------------------------
!
!     this routine computes the weights for a bicubic interpolation.
!
!-----------------------------------------------------------------------

      LOGICAL :: lextrapdone   ! logical, true if EXTRAP done on field
      LOGICAL :: ll_nnei       ! true (default) if extra search is done

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n,icount,i, 
     &     dst_add,        ! destination address
     &     iter,           ! iteration counter
     &     nmap            ! index of current map being computed

      integer (kind=int_kind), dimension(4) :: 
     &     src_add         ! address for the four source points

      real (kind=dbl_kind), dimension(4)  ::
     &     src_lats,       ! latitudes  of four  corners
     &     src_lons        ! longitudes of four  corners

      real (kind=dbl_kind), dimension(4,4)  ::
     &     wgts            ! bicubic weights for four corners

      real (kind=dbl_kind) ::
     &     plat, plon,       ! lat/lon coords of destination point
     &     iguess, jguess,   ! current guess for bicubic coordinate
     &     deli, delj,       ! corrections to i,j
     &     dth1, dth2, dth3, ! some latitude  differences
     &     dph1, dph2, dph3, ! some longitude differences
     &     dthp, dphp,       ! difference between point and sw corner
     &     mat1, mat2, mat3, mat4, ! matrix elements
     &     determinant,      ! matrix determinant
     &     sum_wgts         ! sum of weights for normalization

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
         WRITE (UNIT = nulou,FMT = *)'Entering routine remap_bicub'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      ll_nnei = .true.
      nmap = 1
      if (grid1_rank /= 2) then
        stop 'Can not do bicubic interpolation when grid_rank /= 2'
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

        call grid_search_bicub(src_add, src_lats, src_lons,
     &                         min_add, max_add,
     &                         plat, plon, grid1_dims,
     &                         grid1_center_lat, grid1_center_lon,
     &                         grid1_bound_box, bin_addr1, 
     &                         lextrapdone)

        if (src_add(1) > 0) then

          !***
          !*** if the 4 surrounding points have been found and are 
          !*** non-masked, do bicubic interpolation
          !***

          grid2_frac(dst_add) = one

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

            wgts(1,1) = (one - jguess**2*(three-two*jguess))*
     &                  (one - iguess**2*(three-two*iguess))
            wgts(1,2) = (one - jguess**2*(three-two*jguess))*
     &                         iguess**2*(three-two*iguess)
            wgts(1,3) =        jguess**2*(three-two*jguess)*
     &                         iguess**2*(three-two*iguess)
            wgts(1,4) =        jguess**2*(three-two*jguess)*
     &                  (one - iguess**2*(three-two*iguess))
            wgts(2,1) = (one - jguess**2*(three-two*jguess))*
     &                         iguess*(iguess-one)**2
            wgts(2,2) = (one - jguess**2*(three-two*jguess))*
     &                         iguess**2*(iguess-one)
            wgts(2,3) =        jguess**2*(three-two*jguess)*
     &                         iguess**2*(iguess-one)
            wgts(2,4) =        jguess**2*(three-two*jguess)*
     &                         iguess*(iguess-one)**2
            wgts(3,1) =        jguess*(jguess-one)**2*
     &                  (one - iguess**2*(three-two*iguess))
            wgts(3,2) =        jguess*(jguess-one)**2*
     &                         iguess**2*(three-two*iguess)
            wgts(3,3) =        jguess**2*(jguess-one)*
     &                         iguess**2*(three-two*iguess)
            wgts(3,4) =        jguess**2*(jguess-one)*
     &                  (one - iguess**2*(three-two*iguess))
            wgts(4,1) =        iguess*(iguess-one)**2*
     &                         jguess*(jguess-one)**2
            wgts(4,2) =        iguess**2*(iguess-one)*
     &                         jguess*(jguess-one)**2
            wgts(4,3) =        iguess**2*(iguess-one)*
     &                         jguess**2*(jguess-one)
            wgts(4,4) =        iguess*(iguess-one)**2*
     &                         jguess**2*(jguess-one)

            call store_link_bicub(dst_add, src_add, wgts, nmap)

          ELSE
              WRITE (UNIT = nulou,FMT = *)
     &            'Iteration for i,j exceed max iteration count'
              STOP
          endif

        else if (src_add(1) < 0) THEN

          !***
          !*** Search for bicubic failed or at least one of the 4
          !*** neighbours was masked. Do distance-weighted average using
          !*** the non-masked points among the 4 closest ones. 
          !***


          IF (nlogprt .eq. 2) then
              WRITE(nulou,*) ' '
              WRITE(nulou,*) 
     &  'WARNING: Cannot make bicubic interpolation for target point'
     &            ,dst_add
              WRITE(nulou,*) 
     &    'Using non-masked points among 4 nearest neighbors.'
              WRITE(nulou,*) ' '
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
              wgts(1,1) = src_lats(1)/sum_wgts
              wgts(1,2) = src_lats(2)/sum_wgts
              wgts(1,3) = src_lats(3)/sum_wgts
              wgts(1,4) = src_lats(4)/sum_wgts
              wgts(2,:) = 0.
              wgts(3,:) = 0.
              wgts(4,:) = 0.

              grid2_frac(dst_add) = one
              call store_link_bicub(dst_add, src_add, wgts, nmap)
          ELSE
              IF (ll_nnei .eqv. .true. ) then
              IF (NLOGPRT .GE. 2) THEN
                  WRITE(nulou,*) '  '
                  WRITE(nulou,*)' Using the nearest non-masked neighbour
     & as all 4 surrounding source points are masked for target point',
     &                dst_add
                  WRITE(nulou,*) 'with longitude and latitude', 
     &                plon, plat
              ENDIF
              src_latsnn = bignum
!cdir novector
              do srch_add = min_add,max_add
                if (grid1_mask(srch_add) .or.
     &          (.not. grid1_mask(srch_add) .and. lextrapdone)) THEN
                    arg = coslat_dst*cos(grid1_center_lat(srch_add))*
     &                   (coslon_dst*cos(grid1_center_lon(srch_add)) +
     &                    sinlon_dst*sin(grid1_center_lon(srch_add)))+
     &                    sinlat_dst*sin(grid1_center_lat(srch_add))
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
              IF (NLOGPRT .GE. 2) THEN
                  WRITE(nulou,*) '  '
                  WRITE(nulou,*) 
     &                'Nearest non masked neighbour is source point ',
     &                src_addnn
                  WRITE(nulou,*) 'with longitude and latitude',
     &          grid1_center_lon(src_addnn), grid1_center_lat(src_addnn)
              ENDIF
!
              wgts(1,1) = 1.
              wgts(1,2:4) = 0.
              wgts(2,:) = 0.
              wgts(3,:) = 0.
              wgts(4,:) = 0.
              src_add(1) = src_addnn
              src_add(2) = 0
              src_add(3) = 0
              src_add(4) = 0

              grid2_frac(dst_add) = one
              call store_link_bicub(dst_add, src_add, wgts, nmap)
          endif
       ENDIF
      ENDIF
      end do grid_loop1
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine remap_bicub'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      end subroutine remap_bicub

!***********************************************************************

      subroutine grid_search_bicub(src_add, src_lats, src_lons, 
     &                             min_add, max_add,
     &                             plat, plon, src_grid_dims,
     &                             src_center_lat, src_center_lon,
     &                             src_bound_box,
     &                             src_bin_add, lextrapdone)

!-----------------------------------------------------------------------
!
!     this routine finds the location of the search point plat, plon
!     in the source grid and returns the corners needed for a bicubic
!     interpolation.
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

      real (kind=dbl_kind), dimension(:,:), intent(in) ::
     &        src_bound_box   ! bounding box for src grid search

      integer (kind=int_kind), dimension(:,:), intent(in) ::
     &        src_bin_add    ! search bins for restricting

      LOGICAL ::
     &           lextrapdone   ! logical, true if EXTRAP done on field

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, next_n, srch_add, ni,  ! dummy indices
     &    nx, ny, ntotmask,           ! dimensions of src grid
     &    i, j, jp1, ip1, n_add, e_add, ne_add  ! addresses

      real (kind=dbl_kind) ::  ! vectors for cross-product check
     &      vec1_lat, vec1_lon,
     &      vec2_lat, vec2_lon, cross_product, cross_product_last

!-----------------------------------------------------------------------
!
!     restrict search first using search bins. 
!
!-----------------------------------------------------------------------

      src_add = 0

      min_add = size(src_center_lat)
      max_add = 1
      do n=1,num_srch_bins
        if (plat >= bin_lats(1,n) .and. plat <= bin_lats(2,n) .and.
     &      plon >= bin_lons(1,n) .and. plon <= bin_lons(2,n)) then
          min_add = min(min_add, src_bin_add(1,n))
          max_add = max(max_add, src_bin_add(2,n))
        endif
      end do
 
!-----------------------------------------------------------------------
!
!     now perform a more detailed search 
!
!-----------------------------------------------------------------------

      nx = src_grid_dims(1)
      ny = src_grid_dims(2)

      srch_loop: do srch_add = min_add,max_add

        !*** first check bounding box

        if (plat <= src_bound_box(2,srch_add) .and. 
     &    plat >= src_bound_box(1,srch_add) .and.
     &    plon <= src_bound_box(4,srch_add) .and. 
     &    plon >= src_bound_box(3,srch_add)) then

          !***
          !*** we are within bounding box so get really serious
          !***

          !*** find N,S and NE points to this grid point

          j = (srch_add - 1)/nx +1
          i = srch_add - (j-1)*nx
          
          !*** find ip1
          !*** Note: I do not want to take into account the number
          !*** of overlapping grid points, as the underlying cell
          !*** will be found in all cases if the grid overlaps.

          if (i < nx) then
              ip1 = i + 1
          else
              ip1 = 1
          endif

          if (j < ny) then
              jp1 = j+1
          else
              jp1 = 1
          endif

          n_add = (jp1 - 1)*nx + i
          e_add = (j - 1)*nx + ip1
          ne_add = (jp1 - 1)*nx + ip1

          src_lats(1) = src_center_lat(srch_add)
          src_lats(2) = src_center_lat(e_add)
          src_lats(3) = src_center_lat(ne_add)
          src_lats(4) = src_center_lat(n_add)

          src_lons(1) = src_center_lon(srch_add)
          src_lons(2) = src_center_lon(e_add)
          src_lons(3) = src_center_lon(ne_add)
          src_lons(4) = src_center_lon(n_add)

          !***
          !*** for consistency, we must make sure all lons are in
          !*** same 2pi interval
          !***

          vec1_lon = src_lons(1) - plon
          if (vec1_lon > pi) then
              src_lons(1) = src_lons(1) - pi2
          else if (vec1_lon < -pi) then
              src_lons(1) = src_lons(1) + pi2
          endif
          do n=2,4
            vec1_lon = src_lons(n) - src_lons(1)
            if (vec1_lon > pi) then
                src_lons(n) = src_lons(n) - pi2
            else if (vec1_lon < -pi) then
                src_lons(n) = src_lons(n) + pi2
            endif
          end do

          corner_loop: do n=1,4
            next_n = MOD(n,4) + 1

            !***
            !*** here we take the cross product of the vector making 
            !*** up each box side with the vector formed by the vertex
            !*** and search point.  if all the cross products are 
            !*** same sign, the point is contained in the box.
            !***

            vec1_lat = src_lats(next_n) - src_lats(n)
            vec1_lon = src_lons(next_n) - src_lons(n)
            vec2_lat = plat - src_lats(n)
            vec2_lon = plon - src_lons(n)

            !***
            !*** check for 0,2pi crossings
            !***

            if (vec1_lon >  three*pih) then
                vec1_lon = vec1_lon - pi2
            else if (vec1_lon < -three*pih) then
                vec1_lon = vec1_lon + pi2
            endif
            if (vec2_lon >  three*pih) then
                vec2_lon = vec2_lon - pi2
            else if (vec2_lon < -three*pih) then
                vec2_lon = vec2_lon + pi2
            endif

            cross_product = vec1_lon*vec2_lat - vec2_lon*vec1_lat

            !***
            !*** if cross product is less than zero, this cell
            !*** doesn't work
            !***

            if (n == 1) cross_product_last = cross_product
            if (cross_product*cross_product_last < zero) then
                exit corner_loop
            else
                cross_product_last = cross_product
            endif

          end do corner_loop

          !***
          !*** if cross products all positive, we found the location
          !***

          if (n > 4) then
              src_add(1) = srch_add
              src_add(2) = e_add
              src_add(3) = ne_add
              src_add(4) = n_add
              
              ! Check if one point is masked; IF so, nearest-neighbour
              ! interpolation will be used
  
              ntotmask = 0
              do ni=1,4
                if (.not. grid1_mask(src_add(ni)).and. 
     &              .not. lextrapdone) 
     &              ntotmask = ntotmask + 1 
              end DO
              IF (ntotmask .gt. 0) src_add(1) = -src_add(1)  
      
              return
          endif

          !***
          !*** otherwise move on to next cell
          !***

      endif                     !bounding box check
      end do srch_loop

      !***
      !*** if no cell found, point is likely either in a box that straddles
      !*** either pole or is outside the grid. Put src_add = -1 so that
      !*** distance-weighted average of the 4 non-masked closest points
      !*** is done in calling routine.
  
      src_add = -1

!-----------------------------------------------------------------------

      end subroutine grid_search_bicub 

!***********************************************************************

      subroutine store_link_bicub(dst_add, src_add, weights, nmap)

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

      real (kind=dbl_kind), dimension(4,4), intent(in) ::
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
          wts_map1    (:,num_links_old+n) = weights(:,n)
        end do

      case(2)

        num_links_old  = num_links_map2
        num_links_map2 = num_links_old + 4

        if (num_links_map2 > max_links_map2) 
     &     call resize_remap_vars(2,resize_increment)

        do n=1,4
          grid1_add_map2(num_links_old+n) = dst_add
          grid2_add_map2(num_links_old+n) = src_add(n)
          wts_map2    (:,num_links_old+n) = weights(:,n)
        end do

      end select

!-----------------------------------------------------------------------

      end subroutine store_link_bicub

!***********************************************************************

      end module remap_bicubic

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
