C****
C                    ************************
C                    *     OASIS MODULE     *
C                    *     ------------     *
C                    ************************
C**** 
C***********************************************************************
C     This module belongs to the SCRIP library. It is a modified version
C     of SCRIP 1.4 remap_ditswgt.f in order to weight a neighbour by
C     exp[-1/2 * d^^2/sigma^^2] where d is the distance between the source
C     and the target points and sigma^^2=VAR*dm^^2, where dm is the
C     average distance between the source grid points and VAR is a value 
C     given by the user.
C
C     Additional Modifications:
C       - restrict types are written in capital letters
C       - bug line 429: bin_lons(3,n) instead of bin_lons(2,n)
C
C     Modified by            D. Declat,  CERFACS              27.06.2002
C***********************************************************************
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     this module contains necessary routines for performing an 
!     interpolation using a distance-weighted average of n nearest
!     neighbors.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: remap_gauswgt.f 2826 2010-12-10 11:14:21Z valcke $
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

      module remap_gaussian_weight

!-----------------------------------------------------------------------

      use kinds_mod     ! defines common data types
      use constants     ! defines common constants
      use grids         ! module containing grid info
      use remap_vars    ! module containing remap info
      USE mod_oasis_flush

      implicit none

!-----------------------------------------------------------------------
!
!     module variables
!
!-----------------------------------------------------------------------

      real (kind=dbl_kind), dimension(:), allocatable, save ::
     &     coslat, sinlat, ! cosine, sine of grid lats (for distance)
     &     coslon, sinlon, ! cosine, sine of grid lons (for distance)
     &     wgtstmp         ! an array to hold the link weight

!***********************************************************************

      contains

!***********************************************************************

      subroutine remap_gauswgt (lextrapdone, num_neighbors, rl_varmul)

!-----------------------------------------------------------------------
!
!     this routine computes the inverse-distance weights for a
!     nearest-neighbor interpolation.
!
!-----------------------------------------------------------------------

      LOGICAL ::
     &           lextrapdone   ! logical, true if EXTRAP done on field

      REAL (kind=real_kind) ::
     $    rl_varmul             ! Gaussian variance

      INTEGER (kind=int_kind) ::
     &       num_neighbors     ! number of neighbours

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      logical (kind=log_kind), dimension(num_neighbors) ::
     &     nbr_mask        ! mask at nearest neighbors
      
      integer (kind=int_kind) :: n,
     &     dst_add,        ! destination address
     &     nmap            ! index of current map being computed

      integer (kind=int_kind), dimension(num_neighbors) ::
     &     nbr_add         ! source address at nearest neighbors

      real (kind=dbl_kind), dimension(num_neighbors) ::
     &     nbr_dist        ! angular distance four nearest neighbors

      real (kind=dbl_kind) ::
     &     coslat_dst,     ! cos(lat) of destination grid point
     &     coslon_dst,     ! cos(lon) of destination grid point
     &     sinlat_dst,     ! sin(lat) of destination grid point
     &     sinlon_dst,     ! sin(lon) of destination grid point
     &     dist_tot,       ! sum of neighbor distances (for normalizing)
     &     dist_average    ! average distance between the source points
      logical (kind=log_kind) :: ll_allmask
      logical (kind=log_kind), PARAMETER :: ll_nnei=.true.
      real (kind=dbl_kind) ::
     &        distance ,plat,plon,src_latsnn, arg   ! angular distance
      real (kind=dbl_kind), dimension (1) :: wgts_new
      integer (kind=int_kind) :: min_add_out, max_add_out, 
     &        srch_add, src_addnn
!-----------------------------------------------------------------------
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Entering routine remap_gauswgt'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!-----------------------------------------------------------------------
!
!     compute mappings from grid1 to grid2
!
!-----------------------------------------------------------------------

      nmap = 1

      !***
      !*** allocate wgtstmp to be consistent with store_link interface
      !***

      allocate (wgtstmp(num_wts))

      !***
      !*** compute cos, sin of lat/lon on source grid for distance
      !*** calculations
      !***

      allocate (coslat(grid1_size), coslon(grid1_size),
     &          sinlat(grid1_size), sinlon(grid1_size))

      coslat = cos(grid1_center_lat)
      coslon = cos(grid1_center_lon)
      sinlat = sin(grid1_center_lat)
      sinlon = sin(grid1_center_lon)

      !***
      !*** compute the average of the distances between the source points
      !*** 

      call grid_dist_average(grid1_size,
     &                       coslat, coslon,
     &                       sinlat, sinlon,
     &                       dist_average)
      !***
      !*** loop over destination grid 
      !***

      grid_loop1: do dst_add = 1, grid2_size

        if (.not. grid2_mask(dst_add)) cycle grid_loop1

        coslat_dst = cos(grid2_center_lat(dst_add))
        coslon_dst = cos(grid2_center_lon(dst_add))
        sinlat_dst = sin(grid2_center_lat(dst_add))
        sinlon_dst = sin(grid2_center_lon(dst_add))

        !***
        !*** find nearest grid points on source grid and
        !*** distances to each point
        !***

        call grid_search_nbrg(nbr_add, nbr_dist, 
     &                        min_add_out, max_add_out,
     &                        grid2_center_lat(dst_add),
     &                        grid2_center_lon(dst_add),
     &                        coslat_dst, coslon_dst, 
     &                        sinlat_dst, sinlon_dst,
     &                        bin_addr1, 
     &                        dist_average,  num_neighbors, rl_varmul)

        !***
        !*** compute weights based on inverse distance
        !*** if mask is false, eliminate those points
        !***
        src_addnn = zero
        dist_tot = zero
        do n=1,num_neighbors
          if ((grid1_mask(nbr_add(n))) .or.
     &        (.not. grid1_mask(nbr_add(n)) .and. lextrapdone)) THEN
            nbr_dist(n) = one/nbr_dist(n)
            dist_tot = dist_tot + nbr_dist(n)
            nbr_mask(n) = .true.
          else
            nbr_mask(n) = .false.
          endif
        end do

        !***
        !*** normalize weights and store the link
        !***

        do n=1,num_neighbors
          if (nbr_mask(n)) then
            wgtstmp(1) = nbr_dist(n)/dist_tot
            call store_link_nbr(nbr_add(n), dst_add, wgtstmp, nmap)
            grid2_frac(dst_add) = one
          endif
        end do
        IF (ll_nnei) THEN
            ll_allmask= .true.
            do n=1,num_neighbors
              if (nbr_mask(n)) then
                  ll_allmask=.false.
              endif
            end do
            if ( ll_allmask) THEN
                src_latsnn = bignum
                do srch_add = min_add_out,max_add_out
                  if (grid1_mask(srch_add) .or.
     &                (.not. grid1_mask(srch_add) 
     &                .and. lextrapdone)) THEN
                      arg = coslat_dst*cos(grid1_center_lat(srch_add))*
     &                (coslon_dst*cos(grid1_center_lon(srch_add)) +
     &                sinlon_dst*sin(grid1_center_lon(srch_add)))+
     &                sinlat_dst*sin(grid1_center_lat(srch_add))
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
                end do
                IF (nlogprt .GE. 2) THEN
                    WRITE(nulou,*)'ll_allmask=true and src_addnn= '
     &                  ,src_addnn
                    CALL OASIS_FLUSH_SCRIP(nulou)
                ENDIF
                wgts_new(1) = 1.
                grid2_frac(dst_add) = one
                call store_link_nbr(src_addnn,dst_add ,wgts_new, nmap)
            endif
        endif
            
      end do grid_loop1

      deallocate (coslat, coslon, sinlat, sinlon)

!-----------------------------------------------------------------------
!
!     compute mappings from grid2 to grid1 if necessary
!
!-----------------------------------------------------------------------

      if (num_maps > 1) then

      nmap = 2

      !***
      !*** compute cos, sin of lat/lon on source grid for distance
      !*** calculations
      !***

      allocate (coslat(grid2_size), coslon(grid2_size),
     &          sinlat(grid2_size), sinlon(grid2_size))

      coslat = cos(grid2_center_lat)
      coslon = cos(grid2_center_lon)
      sinlat = sin(grid2_center_lat)
      sinlon = sin(grid2_center_lon)

      !***
      !*** compute the average of the distances between the source points
      !*** 

      call grid_dist_average(grid2_size,
     &                       coslat, coslon,
     &                       sinlat, sinlon,
     &                       dist_average)

      !***
      !*** loop over destination grid 
      !***

      grid_loop2: do dst_add = 1, grid1_size

        if (.not. grid1_mask(dst_add)) cycle grid_loop2

        coslat_dst = cos(grid1_center_lat(dst_add))
        coslon_dst = cos(grid1_center_lon(dst_add))
        sinlat_dst = sin(grid1_center_lat(dst_add))
        sinlon_dst = sin(grid1_center_lon(dst_add))

        !***
        !*** find four nearest grid points on source grid and
        !*** distances to each point
        !***

        call grid_search_nbrg(nbr_add, nbr_dist, 
     &                        min_add_out, max_add_out,
     &                        grid1_center_lat(dst_add),
     &                        grid1_center_lon(dst_add),
     &                        coslat_dst, coslon_dst, 
     &                        sinlat_dst, sinlon_dst,
     &                        bin_addr2, 
     &                        dist_average, num_neighbors, rl_varmul)

        !***
        !*** compute weights based on inverse distance
        !*** if mask is false, eliminate those points
        !***

        dist_tot = zero
        do n=1,num_neighbors
          if (grid2_mask(nbr_add(n))) then
            nbr_dist(n) = one/nbr_dist(n)
            dist_tot = dist_tot + nbr_dist(n)
            nbr_mask(n) = .true.
          else
            nbr_mask(n) = .false.
          endif
        end do

        !***
        !*** normalize weights and store the link
        !***

        do n=1,num_neighbors
          if (nbr_mask(n)) then
            wgtstmp(1) = nbr_dist(n)/dist_tot
            call store_link_nbr(dst_add, nbr_add(n), wgtstmp, nmap)
            grid1_frac(dst_add) = one
          endif
        end do

        IF (ll_nnei) THEN
            ll_allmask= .true.
            do n=1,num_neighbors
              if (nbr_mask(n)) then
                  ll_allmask=.false.
              endif
            end do
            if ( ll_allmask) then
                PRINT*, 'll_allmask true',src_addnn   
                src_latsnn = bignum
                do srch_add = min_add_out,max_add_out
                  if (grid2_mask(srch_add) .or.
     &                (.not. grid2_mask(srch_add) 
     &                .and. lextrapdone)) THEN
                      arg = coslat_dst*cos(grid2_center_lat(srch_add))*
     &                (coslon_dst*cos(grid2_center_lon(srch_add)) +
     &                sinlon_dst*sin(grid2_center_lon(srch_add)))+
     &                sinlat_dst*sin(grid2_center_lat(srch_add))
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
                end do
                wgts_new = 1.
                grid1_frac(dst_add) = one
                call store_link_nbr(dst_add, src_addnn, wgts_new, nmap)
            endif
        endif
      end do grid_loop2

      deallocate (coslat, coslon, sinlat, sinlon)

      endif

      deallocate(wgtstmp)
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)'Leaving routine remap_gauswgt'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------

      end subroutine remap_gauswgt

!***********************************************************************

      subroutine grid_search_nbrg(nbr_add, nbr_dist, min_add, max_add, 
     &               plat, plon,  
     &               coslat_dst, coslon_dst, sinlat_dst, sinlon_dst,
     &               src_bin_add, dst_average,
     $               num_neighbors, rl_varmul)

!-----------------------------------------------------------------------
!
!     this routine finds the closest num_neighbor points to a search 
!     point and computes a distance to each of the neighbors.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(:,:), intent(in) ::
     &        src_bin_add ! search bins for restricting search

      real (kind=dbl_kind), intent(in) ::
     &        plat,         ! latitude  of the search point
     &        plon,         ! longitude of the search point
     &        coslat_dst,   ! cos(lat)  of the search point
     &        coslon_dst,   ! cos(lon)  of the search point
     &        sinlat_dst,   ! sin(lat)  of the search point
     &        sinlon_dst,   ! sin(lon)  of the search point
     &        dst_average   ! average distance between the source points

      REAL (kind=real_kind), intent(in) ::
     &        rl_varmul     ! Gaussian variance

      INTEGER (kind=int_kind) ::
     &       num_neighbors     ! number of neighbours

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), dimension(num_neighbors), intent(out) ::
     &        nbr_add  ! address of each of the closest points

      real (kind=dbl_kind), dimension(num_neighbors), intent(out) ::
     &        nbr_dist ! distance to each of the closest points

      integer (kind=int_kind),intent(out) :: min_add, max_add 

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: n, nmax, nadd, nchk, ! dummy indices
     &        nm1, np1, i, j, ip1, im1, jp1, jm1

      real (kind=dbl_kind) ::
     &        distance, arg      ! angular distance

      real (kind=dbl_kind) ::
     &        variance      ! variance for the gaussian FUNCTION


!-----------------------------------------------------------------------
!
!     loop over source grid and find nearest neighbors
!
!-----------------------------------------------------------------------

      !***
      !*** restrict the search using search bins
      !*** expand the bins to catch neighbors
      !***

      select case (restrict_type)
      case('LATITUDE')

        do n=1,num_srch_bins
          if (plat >= bin_lats(1,n) .and. plat <= bin_lats(2,n)) then
            min_add = src_bin_add(1,n)
            max_add = src_bin_add(2,n)

            nm1 = max(n-1,1)
            np1 = min(n+1,num_srch_bins)

            min_add = min(min_add,src_bin_add(1,nm1))
            max_add = max(max_add,src_bin_add(2,nm1))
            min_add = min(min_add,src_bin_add(1,np1))
            max_add = max(max_add,src_bin_add(2,np1))
          endif
        end do

      case('LATLON')

        n = 0
        nmax = nint(sqrt(real(num_srch_bins)))
        do j=1,nmax
        jp1 = min(j+1,nmax)
        jm1 = max(j-1,1)
        do i=1,nmax
          ip1 = min(i+1,nmax)
          im1 = max(i-1,1)

          n = n+1
          if (plat >= bin_lats(1,n) .and. plat <= bin_lats(2,n) .and.
     &        plon >= bin_lons(1,n) .and. plon <= bin_lons(2,n)) then
            min_add = src_bin_add(1,n)
            max_add = src_bin_add(2,n)

            nm1 = (jm1-1)*nmax + im1
            np1 = (jp1-1)*nmax + ip1
            nm1 = max(nm1,1)
            np1 = min(np1,num_srch_bins)

            min_add = min(min_add,src_bin_add(1,nm1))
            max_add = max(max_add,src_bin_add(2,nm1))
            min_add = min(min_add,src_bin_add(1,np1))
            max_add = max(max_add,src_bin_add(2,np1))
          endif
        end do
        end do

      end select

      !***
      !*** initialize distance and address arrays
      !***

      nbr_add = 0
      nbr_dist = bignum
      variance = rl_varmul*dst_average*dst_average
      do nadd=min_add,max_add

        !***
        !*** find distance to this point
        !***
        arg = sinlat_dst*sinlat(nadd) +
     &        coslat_dst*coslat(nadd)*
     &       (coslon_dst*coslon(nadd) +
     &        sinlon_dst*sinlon(nadd))
        IF (arg < -1.0d0) THEN
            arg = -1.0d0
        ELSE IF (arg > 1.0d0) THEN
            arg = 1.0d0
        END IF
        distance = acos(arg)
        !distance = min(500., distance) !ts-sv
        !distance = exp(.5*distance*distance/variance)
        !***
        !*** store the address and distance if this is one of the
        !*** smallest four so far
        !***

        check_loop: do nchk=1,num_neighbors
          if (distance .lt. nbr_dist(nchk)) THEN
            do n=num_neighbors,nchk+1,-1
              nbr_add(n) = nbr_add(n-1)
              nbr_dist(n) = nbr_dist(n-1)
            end do
            nbr_add(nchk) = nadd
            nbr_dist(nchk) = distance
            exit check_loop
          endif
        end do check_loop

      end do

      exp_loop: do nchk=1,num_neighbors
          nbr_dist(nchk) = 
     &     exp(.5*nbr_dist(nchk)*nbr_dist(nchk)/variance)
      end do exp_loop

!-----------------------------------------------------------------------

      end subroutine grid_search_nbrg 

!***********************************************************************

      subroutine store_link_nbr(add1, add2, weights, nmap)

!-----------------------------------------------------------------------
!
!     this routine stores the address and weight for this link in
!     the appropriate address and weight arrays and resizes those
!     arrays if necessary.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) ::
     &        add1,  ! address on grid1
     &        add2,  ! address on grid2
     &        nmap   ! identifies which direction for mapping

      real (kind=dbl_kind), dimension(:), intent(in) ::
     &        weights ! array of remapping weights for this link

!-----------------------------------------------------------------------
!
!     increment number of links and check to see if remap arrays need
!     to be increased to accomodate the new link.  then store the
!     link.
!
!-----------------------------------------------------------------------

      select case (nmap)
      case(1)

        num_links_map1  = num_links_map1 + 1

        if (num_links_map1 > max_links_map1) 
     &     call resize_remap_vars(1,resize_increment)

        grid1_add_map1(num_links_map1) = add1
        grid2_add_map1(num_links_map1) = add2
        wts_map1    (:,num_links_map1) = weights

      case(2)

        num_links_map2  = num_links_map2 + 1

        if (num_links_map2 > max_links_map2) 
     &     call resize_remap_vars(2,resize_increment)

        grid1_add_map2(num_links_map2) = add1
        grid2_add_map2(num_links_map2) = add2
        wts_map2    (:,num_links_map2) = weights

      end select

!-----------------------------------------------------------------------

      end subroutine store_link_nbr

!***********************************************************************

      subroutine grid_dist_average(grid_size,
     &                             coslat_grid, coslon_grid,
     &                             sinlat_grid, sinlon_grid,
     &                             dist_average)

!-----------------------------------------------------------------------
!
!     this routine computes the average distance between the points of a
!     grid.
!
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!
!     output variables
!
!-----------------------------------------------------------------------

      real (kind=dbl_kind), intent(out) ::
     &        dist_average ! distance to each of the closest points

!-----------------------------------------------------------------------
!
!     input variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind), intent(in) ::
     &        grid_size  

      real (kind=dbl_kind), dimension(:), intent(in) ::
     &        coslat_grid,   ! cos(lat)  of the grid points
     &        coslon_grid,   ! cos(lon)  of the grid points
     &        sinlat_grid,   ! sin(lat)  of the grid points
     &        sinlon_grid    ! sin(lon)  of the grid points
      REAL (kind=dbl_kind) :: arg

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (kind=int_kind) :: i
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)
     &       'Entering routine remap_dist_average'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     compute the distance over the grid and average
!
!-----------------------------------------------------------------------

      dist_average = 0.0
      DO i = 1, grid_size
        IF (i .eq. 1) THEN
            arg = sinlat_grid(grid_size)*sinlat_grid(i) +
     &          coslat_grid(grid_size)*coslat_grid(i)*
     &          (coslon_grid(grid_size)*coslon_grid(i) +
     &          sinlon_grid(grid_size)*sinlon_grid(i))
            IF (arg < -1.0d0) THEN
                arg = -1.0d0
            ELSE IF (arg > 1.0d0) THEN
                arg = 1.0d0
            END IF           
            dist_average = dist_average + acos(arg)

            arg = sinlat_grid(i)*sinlat_grid(i+1) +
     &          coslat_grid(i)*coslat_grid(i+1)*
     &          (coslon_grid(i)*coslon_grid(i+1) +
     &          sinlon_grid(i)*sinlon_grid(i+1))
            IF (arg < -1.0d0) THEN
                arg = -1.0d0
            ELSE IF (arg > 1.0d0) THEN
                arg = 1.0d0
            END IF 
            dist_average = dist_average + acos(arg)
             
        ELSE IF (i .eq. grid_size) THEN
            arg = sinlat_grid(i-1)*sinlat_grid(i) +
     &          coslat_grid(i-1)*coslat_grid(i)*
     &          (coslon_grid(i-1)*coslon_grid(i) +
     &          sinlon_grid(i-1)*sinlon_grid(i))
            IF (arg < -1.0d0) THEN
                arg = -1.0d0
            ELSE IF (arg > 1.0d0) THEN
                arg = 1.0d0
            END IF 
            dist_average = dist_average + acos(arg)

            arg = sinlat_grid(i)*sinlat_grid(1) +
     &          coslat_grid(i)*coslat_grid(1)*
     &          (coslon_grid(i)*coslon_grid(1) +
     &          sinlon_grid(i)*sinlon_grid(1))
            IF (arg < -1.0d0) THEN
                arg = -1.0d0
            ELSE IF (arg > 1.0d0) THEN
                arg = 1.0d0
            END IF 
            dist_average = dist_average + acos(arg)
            
        ELSE
            arg = sinlat_grid(i-1)*sinlat_grid(i) +
     &          coslat_grid(i-1)*coslat_grid(i)*
     &          (coslon_grid(i-1)*coslon_grid(i) +
     &          sinlon_grid(i-1)*sinlon_grid(i))
            IF (arg < -1.0d0) THEN
                arg = -1.0d0
            ELSE IF (arg > 1.0d0) THEN
                arg = 1.0d0
            END IF             
            dist_average = dist_average + acos(arg)

            arg = sinlat_grid(i)*sinlat_grid(i+1) +
     &          coslat_grid(i)*coslat_grid(i+1)*
     &          (coslon_grid(i)*coslon_grid(i+1) +
     &          sinlon_grid(i)*sinlon_grid(i+1))
            IF (arg < -1.0d0) THEN
                arg = -1.0d0
            ELSE IF (arg > 1.0d0) THEN
                arg = 1.0d0
            END IF              
            dist_average = dist_average + acos(arg)             
        END IF
      END DO
      dist_average = dist_average / (2*grid_size)
!
      IF (nlogprt .GE. 2) THEN
         WRITE (UNIT = nulou,FMT = *)' '
         WRITE (UNIT = nulou,FMT = *)
     &       'Leaving routine remap_dist_average'
         CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
!
      END subroutine grid_dist_average

!***********************************************************************

      end module remap_gaussian_weight

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

