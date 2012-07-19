!/ ------------------------------------------------------------------- /
      module scrip_interface

!  1. Original author :
!
!     Erick Rogers, NRL
!
!  2. Last update :
!
!     See revisions.
!
!  3. Revisions :
!
!     29-Apr-2011 : Origination                         ( version 4.01 )
!
!  4. Copyright :
!
!  5. Purpose :
!
!     Routines to provide interface between WMGHGH and SCRIP
!
!  6. Variables and types :
!
!  7. Subroutines and functions :
!
!  8. Subroutines and functions used :
!
!  9. Remarks :
!     On parallelization:
!         When WW3 is working on WMGHGH, each MPI task performs identical computations
!         and therefore creates its own copy of the an identical solution. Thus there
!         is no MPI parallelization of WMGHGH. Presumably, this was designed with the
!         expectation that WMGHGH computations are fairly quick. Unfortunately, SCRIP 
!         can be slow for large grids, making WMGHGH slow. It is possible to run SCRIP 
!         with OpenMP (with pgf90, it simply requires that we compile with the -mp flag). 
!         Unfortunately, this doesn't change the situation whereby each MPI task calls 
!         SCRIP and creates its own copy of an identical solution. Thus, if we want 
!         to use OpenMP to speed up SCRIP, it means that we have to leave some cores idle 
!         during the actual WW3 compuation. Example: using pgf90, we execute ww3_multi 
!         with mpirun -n 4. Thus, four MPI tasks are created. And say we compiled SCRIP 
!         using -mp and have nthreads=2 specified in scrip_interface.f. When multi gets 
!         to SCRIP, each MPI task will create 2 OpenMP threads, which means that we'd 
!         better have 8 threads available to do this, and also note that 4 threads will 
!         be idle after SCRIP is finished. This isn't an ideal solution. A better 
!         solution (though it would require some new coding) would be: WMGHGH, just 
!         prior to the SCRIP call, is instructed to only compute using one MPI thread. 
!         Then, SCRIP does its thing using OpenMPI and returns to WMGHGH. Then, WMGHGH
!         shares all the information with all the nodes and continues.
!
! 10. Switches :
!
! 11. Source code :

      use SCRIP_KindsMod    ! defines data types

!.....notes: since the calling subroutine (wmghgh) does not know a priori the size 
!............of the weights arrays, we manage the communication via this module.
      implicit none

      type weight_data
         integer (SCRIP_i4)              :: n    ! number of weights for dst cell
                                                 ! n is equivalent to NR1 and NLOC in original WMGHGH
                                                 ! NR1 is the counter of |MAPSTA|=1 (indicates sea point)
         integer (SCRIP_i4)              :: NR0  ! counter of MAPSTA=0 (indicates excluded point)
         integer (SCRIP_i4)              :: NR2  ! counter of |MAPSTA|=2 (indicates boundary point)
         integer (SCRIP_i4)              :: NRL  ! counter of MAPSTA=0 (indicates excluded point) and MAPST2=0 (indicates land)
         real    (SCRIP_r8), allocatable :: w(:) ! weights, sized by n, formerly wxwy(:,:)
         integer (SCRIP_i4), allocatable :: k(:) ! source grid cells, sized by n, formerly ksrc(:,:)
      end type weight_data

      type(weight_data), allocatable :: wgtdata(:)

      contains
!/ ------------------------------------------------------------------- /

!#######################################################################
      subroutine scrip_wrapper (xgrdin_src,ygrdin_src,dxdpin_src,dxdqin_src,dydpin_src,dydqin_src, &
                                xgrdin_dst,ygrdin_dst,dxdpin_dst,dxdqin_dst,dydpin_dst,dydqin_dst, &
                                mapsta_src,mapst2_src,flagll,gridshift)
!#######################################################################

!  1. Original author :
!
!     Erick Rogers, NRL
!
!  2. Last update :
!
!     See revisions.
!
!  3. Revisions :
!
!     29-Apr-2011 : Origination                         ( version 4.01 )
!
!  4. Copyright :
!
!  5. Purpose : 
!
!     Compute grid information required by SCRIP
!
!  6. Method :
!
!  7. Parameters, Variables and types :
!  
!  8. Called by : 
!
!     Subroutine WMGHGH
!
!  9. Subroutines and functions used :
!
!     Subroutine SCRIP
!
! 10. Error messages: 
!
! 11. Remarks :
!
! 12. Structure :
!
! 13. Switches :
!
! 14. Source code :

      use scrip_grids        ! module containing grid information
      use scrip_remap_vars   ! module containing remap information
      use scrip_constants    ! common constants
      use scrip_KindsMod     ! defines common data types

      implicit none
      real   (SCRIP_r8), intent(in)      :: xgrdin_src(:,:),ygrdin_src(:,:)
      real   (SCRIP_r8), intent(in)      :: dxdpin_src(:,:),dxdqin_src(:,:)
      real   (SCRIP_r8), intent(in)      :: dydpin_src(:,:),dydqin_src(:,:)
      real   (SCRIP_r8), intent(in)      :: xgrdin_dst(:,:),ygrdin_dst(:,:)
      real   (SCRIP_r8), intent(in)      :: dxdpin_dst(:,:),dxdqin_dst(:,:)
      real   (SCRIP_r8), intent(in)      :: dydpin_dst(:,:),dydqin_dst(:,:)
      real   (SCRIP_r8), intent(in)      :: gridshift
      integer(SCRIP_i4), intent(in)      :: mapsta_src(:,:)
      integer(SCRIP_i4), intent(in)      :: mapst2_src(:,:)
      logical(SCRIP_Logical), intent(in) :: flagll
!/ ------------------------------------------------------------------- /
!/ local variables
!/
      integer(SCRIP_i4)                 :: irec,i,j,ni,nj,idum,nk,k,ilink,iw,icorner
      integer(SCRIP_i4)                 :: isrc,jsrc,ksrc,ipnt,kdst,ni_src
      real   (SCRIP_r8)                 :: lat_conversion,offset
      real   (SCRIP_r8)                 :: conv_dx,conv_dy,weight
      real   (SCRIP_r8)                 :: wtsum
      real   (SCRIP_r8), allocatable    :: xgrd_src(:,:),ygrd_src(:,:)
      real   (SCRIP_r8), allocatable    :: dxdp_src(:,:),dxdq_src(:,:)
      real   (SCRIP_r8), allocatable    :: dydp_src(:,:),dydq_src(:,:)
      real   (SCRIP_r8), allocatable    :: xgrd_dst(:,:),ygrd_dst(:,:)
      real   (SCRIP_r8), allocatable    :: dxdp_dst(:,:),dxdq_dst(:,:)
      real   (SCRIP_r8), allocatable    :: dydp_dst(:,:),dydq_dst(:,:)

!#######################################################################
! "test output" for "input variables"
!#######################################################################

      write(*,*)'minval(xgrdin_src) = ',minval(xgrdin_src)
      write(*,*)'maxval(xgrdin_src) = ',maxval(xgrdin_src)

      write(*,*)'minval(xgrdin_dst) = ',minval(xgrdin_dst)
      write(*,*)'maxval(xgrdin_dst) = ',maxval(xgrdin_dst)

      write(*,*)'minval(dxdpin_src) = ',minval(dxdpin_src)
      write(*,*)'maxval(dxdpin_src) = ',maxval(dxdpin_src)

      write(*,*)'minval(dxdqin_src) = ',minval(dxdqin_src)
      write(*,*)'maxval(dxdqin_src) = ',maxval(dxdqin_src)

      write(*,*)'minval(dydpin_src) = ',minval(dydpin_src)
      write(*,*)'maxval(dydpin_src) = ',maxval(dydpin_src)

      write(*,*)'minval(dydqin_src) = ',minval(dydqin_src)
      write(*,*)'maxval(dydqin_src) = ',maxval(dydqin_src)

      write(*,*)'minval(dxdpin_dst) = ',minval(dxdpin_dst)
      write(*,*)'maxval(dxdpin_dst) = ',maxval(dxdpin_dst)

      write(*,*)'minval(dxdqin_dst) = ',minval(dxdqin_dst)
      write(*,*)'maxval(dxdqin_dst) = ',maxval(dxdqin_dst)

      write(*,*)'minval(dydpin_dst) = ',minval(dydpin_dst)
      write(*,*)'maxval(dydpin_dst) = ',maxval(dydpin_dst)

      write(*,*)'minval(dydqin_dst) = ',minval(dydqin_dst)
      write(*,*)'maxval(dydqin_dst) = ',maxval(dydqin_dst)

      write(*,*)'flagll = ',flagll
      write(*,*)'gridshift = ',gridshift

!#######################################################################
! START: universal settings
!#######################################################################

!.....Set variables for converting to degrees
!.....notes: SCRIP only operates on spherical coordinates, so for the case where
!............the problem is specified by the user as in a meters/cartesian coordinate
!............system, it is necessary to make a temporary conversion to a "fake"
!............spherical coordinate grid, to keep SCRIP happy. The good news here
!............is that multi-grid meters-grid simulations will be very rare:
!............we will probably only encounter them in the context of simple test cases.
!............Strictly speaking, this conversion does even need to be physically
!............correct, e.g. we could say that 1000 km is 1 deg....as long as we are
!............consistent between grids.
!............Potential future improvement: make conv_dy and offset such that dst grid 
!.................covers a specific longitude range, e.g. 1 deg east to 2 deg east
     
      if(flagll)then
         conv_dx=one
         conv_dy=one
         offset=zero
      else
         lat_conversion=zero ! lat_conversion : the latitude used for conversion everywhere in the grid (approximation) (in radians)
         !     conv_dy=92.6*1200.0 ! physical, =92.6/(3/3600)=111000 m = 111 km
         conv_dy=1.0e+6_SCRIP_r8 ! non-physical, 1e+6=1 deg
         conv_dx=cos(lat_conversion)*conv_dy
         !.....offset (in meters), is necessary so that our grid does not lie on the branch cut
         offset=75000.0_SCRIP_r8-min(minval(xgrdin_src),minval(xgrdin_dst))
      endif

!.....Set constants for thresholding weights:
      frac_lowest =1.e-3_SCRIP_r8
      frac_highest=one+1.e-3_SCRIP_r8
      wt_lowest   =zero
      wt_highest  =one+1.e-7_SCRIP_r8

!#######################################################################
! END: universal settings
!#######################################################################

!#######################################################################
! START: set up src grid
!#######################################################################

!notes....when we work out how to interface with an unstructured grid, 
!.........we will need to revisit this issue of how to set grid1_rank, etc.
      grid1_rank=2     ! hardwired: logically rectangular grid

!     strategy: declare variables in grid module, but allocate them here.
      allocate(grid1_dims(grid1_rank))

      grid1_dims(1)=size(xgrdin_src,2)
      ni=grid1_dims(1)

      grid1_dims(2)=size(xgrdin_src,1)
      nj=grid1_dims(2)

      grid1_size=ni*nj ! hardwired: logically rectangular grid
      grid1_corners=4  ! hardwired: each cell has 4 corners
      grid1_units='degrees' ! the other option is radians...we don't use this
      grid1_name='src' ! this is unnecessary, except for netcdf output

!.....notes: unfortunately, scrip only works for spherical coordinates. thus, if we want to have a multi-grid
!...............case in meters, we have to fake it. fortunately, it should be pretty rare to have a multi-grid
!...............case in meters.

      allocate(xgrd_src(nj,ni))
      allocate(ygrd_src(nj,ni))
      allocate(dxdp_src(nj,ni))
      allocate(dxdq_src(nj,ni))
      allocate(dydp_src(nj,ni))
      allocate(dydq_src(nj,ni))
      allocate(grid1_center_lon(ni*nj))
      allocate(grid1_center_lat(ni*nj))
      allocate(grid1_corner_lon(4,ni*nj))
      allocate(grid1_corner_lat(4,ni*nj))
      allocate(grid1_mask(ni*nj))

!.....manipulations associated with treating meters/cartestian grid as spherical coordinate grid
      xgrd_src=xgrdin_src+offset
      ygrd_src=ygrdin_src
      dxdp_src=dxdpin_src/conv_dx
      dxdq_src=dxdqin_src/conv_dx
      dydp_src=dydpin_src/conv_dy
      dydq_src=dydqin_src/conv_dy    

!.....notes: this "gridshift" variable is included because SCRIP sometimes has trouble when grids cell locations
!............are identical between the two grids. Thus we apply this to one of the two grids.
!.....Further notes about the problem that this addresses:
!............It did not occur when I was using SCRIP v1.4. However, use of this version is not an option since 
!............SCRIP v1.4 does not work for mww3_tp2.5.
!............Using SCRIP v1.5, the problem occurred only for test case mww3_test_05
!............I was able to fix the problem by modifying the conservative remapping routines, see /test_05a_grd2_grd3/v1.5/v5/
!............However, though this produced the proper solution, it also gave some worrisome warning messages, and my comprehension
!............of that code was incomplete, so this simpler approach of offsetting the grids is taken rather than modifying the
!............conservative remapping routines.
! Results running SCRIP outside of WW3, where "good" indicates that weights look good plotted in matlab:
! .....I'm pretty sure (can't remember) that "OK" means that the weights look good plotted in matlab, but there were some warnings from SCRIP
!     gridshift=tiny                 ! test2c : bad     test5a: good
!     gridshift=zero                 ! test2c : good    test5a: bad
!     gridshift=two*tiny             ! test2c : bad
!     gridshift=2.0_SCRIP_r8*tiny    ! test2c : bad
!     gridshift=1.e-6_SCRIP_r8       ! test2c : ok      test5a : ok
!     gridshift=1.e-9_SCRIP_r8       ! test2c : bad
!     gridshift=one/111.e+3_SCRIP_r8 ! test2c : good    test5a : good

!.....notes: The following block of code could be converted to a subroutine.
!............Since I call it twice, this would save a little space.
      irec=0
      do j=1,nj
         do i=1,ni
            irec=irec+1
            grid1_center_lon(irec)=xgrd_src(j,i)/conv_dx
            grid1_center_lat(irec)=ygrd_src(j,i)/conv_dy

! in WW3, it looks like this: IF(ABS(GRIDS(JF)%MAPSTA(JSRC,ISRC)).EQ.1) THEN ! sea point

            grid1_mask(irec)=.true.

!..notes: normally, we'd apply the mask like this:
!           if(abs(mapsta_src(j,i)).eq.1)then 
!              grid1_mask(irec)=.true.
!           else
!              grid1_mask(irec)=.false.
!           endif
!..but unfortunately, WMGHGH needs information about the overlaying high-res cells, even
!..............those that are masked, for calculating NRL, NR0, NR1, NR2. 

!...........corner 1 : halfway to i-1,j-1
            grid1_corner_lon(1,irec)=grid1_center_lon(irec)-half*dxdp_src(j,i)-half*dxdq_src(j,i)+gridshift
            grid1_corner_lat(1,irec)=grid1_center_lat(irec)-half*dydp_src(j,i)-half*dydq_src(j,i)+gridshift

!...........corner 2: halfway to i+1,j-1 
            grid1_corner_lon(2,irec)=grid1_center_lon(irec)+half*dxdp_src(j,i)-half*dxdq_src(j,i)+gridshift
            grid1_corner_lat(2,irec)=grid1_center_lat(irec)+half*dydp_src(j,i)-half*dydq_src(j,i)+gridshift

!...........corner 3: halfway to i+1,j+1
            grid1_corner_lon(3,irec)=grid1_center_lon(irec)+half*dxdp_src(j,i)+half*dxdq_src(j,i)+gridshift
            grid1_corner_lat(3,irec)=grid1_center_lat(irec)+half*dydp_src(j,i)+half*dydq_src(j,i)+gridshift

!...........corner 4: halfway to i-1,j+1
            grid1_corner_lon(4,irec)=grid1_center_lon(irec)-half*dxdp_src(j,i)+half*dxdq_src(j,i)+gridshift
            grid1_corner_lat(4,irec)=grid1_center_lat(irec)-half*dydp_src(j,i)+half*dydq_src(j,i)+gridshift

!..notes: here is a "simpler way" to compute the cell corner
!.......1) decide on cell center of diagonal "neighbor" e.g. (i-1,j-1) and
!.......2) go halfway there.
!....however, this requires if/then statements to work near boundary, so use of dxdp etc. is cleaner

!...........keep within 0 to 360
            if(grid1_center_lon(irec)>360.0)grid1_center_lon(irec)=grid1_center_lon(irec)-360.0
            if(grid1_center_lon(irec)<000.0)grid1_center_lon(irec)=grid1_center_lon(irec)+360.0
            do icorner=1,4
               if(grid1_corner_lon(icorner,irec)>360.0)grid1_corner_lon(icorner,irec)=grid1_corner_lon(icorner,irec)-360.0
               if(grid1_corner_lon(icorner,irec)<000.0)grid1_corner_lon(icorner,irec)=grid1_corner_lon(icorner,irec)+360.0
            end do

         end do
      end do

!#######################################################################
! END: set up src grid
!#######################################################################

!#######################################################################
! START: set up dst grid
!#######################################################################

      grid2_rank=2

      allocate(grid2_dims(grid2_rank))

      grid2_dims(1)=size(xgrdin_dst,2)
      ni=grid2_dims(1)

      grid2_dims(2)=size(xgrdin_dst,1)
      nj=grid2_dims(2)

      grid2_size=ni*nj
      grid2_corners=4
      grid2_units='degrees' ! the other option is radians...we don't use this
      grid2_name='dst'

      allocate(xgrd_dst(nj,ni))
      allocate(ygrd_dst(nj,ni))
      allocate(dxdp_dst(nj,ni))
      allocate(dxdq_dst(nj,ni))
      allocate(dydp_dst(nj,ni))
      allocate(dydq_dst(nj,ni))
      allocate(grid2_center_lon(ni*nj))
      allocate(grid2_center_lat(ni*nj))
      allocate(grid2_corner_lon(4,ni*nj))
      allocate(grid2_corner_lat(4,ni*nj))
      allocate(grid2_mask(ni*nj))

      xgrd_dst=xgrdin_dst+offset
      ygrd_dst=ygrdin_dst
      dxdp_dst=dxdpin_dst/conv_dx
      dxdq_dst=dxdqin_dst/conv_dx
      dydp_dst=dydpin_dst/conv_dy
      dydq_dst=dydqin_dst/conv_dy    

      irec=0
      do j=1,nj
         do i=1,ni
            irec=irec+1
            grid2_center_lon(irec)=xgrd_dst(j,i)/conv_dx
            grid2_center_lat(irec)=ygrd_dst(j,i)/conv_dy

            grid2_mask(irec)=.true.

!...........corner 1 : halfway to i-1,j-1
            grid2_corner_lon(1,irec)=grid2_center_lon(irec)-half*dxdp_dst(j,i)-half*dxdq_dst(j,i)
            grid2_corner_lat(1,irec)=grid2_center_lat(irec)-half*dydp_dst(j,i)-half*dydq_dst(j,i)

!...........corner 2: halfway to i+1,j-1 
            grid2_corner_lon(2,irec)=grid2_center_lon(irec)+half*dxdp_dst(j,i)-half*dxdq_dst(j,i)
            grid2_corner_lat(2,irec)=grid2_center_lat(irec)+half*dydp_dst(j,i)-half*dydq_dst(j,i)

!...........corner 3: halfway to i+1,j+1
            grid2_corner_lon(3,irec)=grid2_center_lon(irec)+half*dxdp_dst(j,i)+half*dxdq_dst(j,i)
            grid2_corner_lat(3,irec)=grid2_center_lat(irec)+half*dydp_dst(j,i)+half*dydq_dst(j,i)

!...........corner 4: halfway to i-1,j+1
            grid2_corner_lon(4,irec)=grid2_center_lon(irec)-half*dxdp_dst(j,i)+half*dxdq_dst(j,i)
            grid2_corner_lat(4,irec)=grid2_center_lat(irec)-half*dydp_dst(j,i)+half*dydq_dst(j,i)

!...........keep within 0 to 360
            if(grid2_center_lon(irec)>360.0)grid2_center_lon(irec)=grid2_center_lon(irec)-360.0
            if(grid2_center_lon(irec)<000.0)grid2_center_lon(irec)=grid2_center_lon(irec)+360.0
            do icorner=1,4
               if(grid2_corner_lon(icorner,irec)>360.0)grid2_corner_lon(icorner,irec)=grid2_corner_lon(icorner,irec)-360.0
               if(grid2_corner_lon(icorner,irec)<000.0)grid2_corner_lon(icorner,irec)=grid2_corner_lon(icorner,irec)+360.0
            end do

         end do
      end do

!#######################################################################
! END: set up dst grid
!#######################################################################

!.....notes: later, these write statements can be prepended with "!/T38"

      write(*,*)'offset = ',offset

      write(*,*)'minval(xgrd_src) = ',minval(xgrd_src)
      write(*,*)'maxval(xgrd_src) = ',maxval(xgrd_src)

      write(*,*)'minval(xgrd_dst) = ',minval(xgrd_dst)
      write(*,*)'maxval(xgrd_dst) = ',maxval(xgrd_dst)

      write(*,*)'minval(ygrd_src) = ',minval(ygrd_src)
      write(*,*)'maxval(ygrd_src) = ',maxval(ygrd_src)

      write(*,*)'minval(ygrd_dst) = ',minval(ygrd_dst)
      write(*,*)'maxval(ygrd_dst) = ',maxval(ygrd_dst)

      write(*,*)'minval(grid1_center_lon) = ',minval(grid1_center_lon)
      write(*,*)'maxval(grid1_center_lon) = ',maxval(grid1_center_lon)

      write(*,*)'minval(grid1_center_lat) = ',minval(grid1_center_lat)
      write(*,*)'maxval(grid1_center_lat) = ',maxval(grid1_center_lat)

      write(*,*)'minval(grid2_center_lon) = ',minval(grid2_center_lon)
      write(*,*)'maxval(grid2_center_lon) = ',maxval(grid2_center_lon)

      write(*,*)'minval(grid2_center_lat) = ',minval(grid2_center_lat)
      write(*,*)'maxval(grid2_center_lat) = ',maxval(grid2_center_lat)

      write(*,*)'minval(grid1_corner_lon) = ',minval(grid1_corner_lon)
      write(*,*)'maxval(grid1_corner_lon) = ',maxval(grid1_corner_lon)

      write(*,*)'minval(grid1_corner_lat) = ',minval(grid1_corner_lat)
      write(*,*)'maxval(grid1_corner_lat) = ',maxval(grid1_corner_lat)

      write(*,*)'minval(grid2_corner_lon) = ',minval(grid2_corner_lon)
      write(*,*)'maxval(grid2_corner_lon) = ',maxval(grid2_corner_lon)

      write(*,*)'minval(grid2_corner_lat) = ',minval(grid2_corner_lat)
      write(*,*)'maxval(grid2_corner_lat) = ',maxval(grid2_corner_lat)

      call scrip

!.....notes: at this point we have the following useful variables:
!........num_wts, e.g. num_wts=3....for first order conservative remapping, 
!........only the first one is relevant, the other two are for second-order remapping
!........max_links_map1, e.g. max_links_map1=1369, 
!......................equivalent to my netcdf/matlab variable num_links
!........grid2_size, e.g. grid2_size=1849, 
!......................equivalent to my netcdf/matlab variable ndst
!........wts_map1(num_wts,max_links_map1), e.g. wts_map1(3,1369), 
!......................equivalent to my netcdf/matlab variable remap_matrix(1369,3)
!........grid1_add_map1(max_links_map1),  e.g. grid1_add_map1(1369),
!......................equivalent to my netcdf/matlab variable src_address(1369)
!........grid2_add_map1(max_links_map1), e.g. grid2_add_map1(1369), 
!......................equivalent to my netcdf/matlab variable dst_address(1369)
!........grid2_frac(grid2_size), e.g. grid2_frac(1849), 
!......................equivalent to my netcdf/matlab variable dst_grid_frac(1849)

!#######################################################################
!.....check results (optional)
!#######################################################################

!.....note re: notation: I use k for the combined i/j array, similar to isea, 
!............but not necessarily the same as isea since some points may be land etc.
!/T38     do k=1,grid2_size
!/T38        write(403,*)grid2_frac(k)
!/T38     end do
!/T38     do ilink=1,max_links_map1
!/T38        write(405,'(999(1x,f20.7))')(wts_map1(iw,ilink),iw=1,num_wts)
!/T38     end do
!/T38     do ilink=1,max_links_map1
!/T38        write(406,'(i20)')grid1_add_map1(ilink) ! equivalent to my "src_address"
!/T38        write(407,'(i20)')grid2_add_map1(ilink) ! equivalent to my "dst_address"
!/T38     end do

!.....organize results and return to wmghgh.

! what I need, for purpose of feeding back to ww3, for each dst grid node,
!....... a) what is the set of src grid nodes, in terms of isea, for which weights are available?  
!...........isea_src_now in plot_destination_weights.m, from 
!...........isea_src_now=ksrc{isea_dst}; (ksrc is iseasrc in matlab script)
!....... b) what is the corresponding set of weights?  wxwy_now in plot_destination_weights.m, from 
!...........wxwy_now=wxwy{isea_dst};

      allocate(wgtdata(grid2_size))

!.....step 1: count up NR0, NR1, NR2, NRL, NLOC (NR1 and NLOC are denoted "n" here)
!.....It is especially important to determine how large npnts gets, so that we can allocate arrays properly
      wgtdata%NR0=0
      wgtdata%NR2=0
      wgtdata%NRL=0
      wgtdata%n=0

      ni_src=grid1_dims(1)

      do ilink=1,max_links_map1

         ksrc=grid1_add_map1(ilink)
         jsrc=INT((ksrc-1)/ni_src)+1
         isrc=ksrc-(jsrc-1)*ni_src

         if ((grid2_frac(grid2_add_map1(ilink))>frac_lowest) .and. (grid2_frac(grid2_add_map1(ilink))<frac_highest) .and. (wts_map1(1,ilink)>=wt_lowest) .and. (wts_map1(1,ilink)<=wt_highest))then
            if (MAPSTA_src(jsrc,isrc).eq.0) then ! excluded point
               wgtdata(grid2_add_map1(ilink))%NR0    =  wgtdata(grid2_add_map1(ilink))%NR0 + 1
               if (MAPST2_src(jsrc,isrc).eq.0)then
                  wgtdata(grid2_add_map1(ilink))%NRL =  wgtdata(grid2_add_map1(ilink))%NRL + 1
               endif
            else if (abs(MAPSTA_src(jsrc,isrc)).eq.1) then ! sea point
               wgtdata(grid2_add_map1(ilink))%n=wgtdata(grid2_add_map1(ilink))%n+1
            else if (abs(MAPSTA_src(jsrc,isrc)).eq.2) then ! bnd point
               wgtdata(grid2_add_map1(ilink))%NR2    =  wgtdata(grid2_add_map1(ilink))%NR2 + 1
            end if
         endif

      end do

! How this would look if we did this inside WMGHGH:
!                     do ipnt=1,WTS_NOMASK(GSRC)%wgtdata(kdst)%n
!                        ksrc=WTS_NOMASK(GSRC)%wgtdata(kdst)%k(ipnt)
!                        jsrc=INT((ksrc-1)/nisrc)+1
!                        isrc=ksrc-(jsrc-1)*nisrc
!                        IF (GRIDS(GSRC)%MAPSTA(JSRC,ISRC).EQ.0) THEN ! excluded point
!                           NR0    = NR0 + 1
!                           IF (GRIDS(GSRC)%MAPST2(JSRC,ISRC).EQ.0) NRL = NRL + 1
!                        ELSE IF (ABS(GRIDS(GSRC)%MAPSTA(JSRC,ISRC)).EQ.1) THEN ! sea point
!                           NR1    = NR1 + 1
!                           BDIST(JJ) = MIN ( BDIST(JJ) ,               &
!                                MDATAS(GSRC)%MAPBDI(JSRC,ISRC) )
!                        ELSE IF (ABS(GRIDS(GSRC)%MAPSTA(JSRC,ISRC)).EQ.2) THEN ! bnd point
!                           NR2    = NR2 + 1
!                        END IF
!                     end do

!.....step 2: allocate according to the size "n" determined above
      do kdst=1,grid2_size
         allocate(wgtdata(kdst)%w(wgtdata(kdst)%n))
         allocate(wgtdata(kdst)%k(wgtdata(kdst)%n))
         wgtdata(kdst)%n=0
      end do

!.....step 3: save weights
      do ilink=1,max_links_map1

         ksrc=grid1_add_map1(ilink)
         jsrc=INT((ksrc-1)/ni_src)+1
         isrc=ksrc-(jsrc-1)*ni_src

         if ((grid2_frac(grid2_add_map1(ilink))>frac_lowest) .and. (grid2_frac(grid2_add_map1(ilink))<frac_highest) .and. (wts_map1(1,ilink)>=wt_lowest) .and. (wts_map1(1,ilink)<=wt_highest))then
            if (abs(MAPSTA_src(jsrc,isrc)).eq.1) then ! sea point

!        if ((grid2_frac(grid2_add_map1(ilink))>0.001)       .and. (grid2_frac(grid2_add_map1(ilink))<1.001)        .and. (wts_map1(1,ilink)>=zero)      .and. (wts_map1(1,ilink)<=(one+1.e-7_SCRIP_r8)))then

!...........make a set of cells that are all referenced to the destination address
               wgtdata(grid2_add_map1(ilink))%n=wgtdata(grid2_add_map1(ilink))%n+1
               wgtdata(grid2_add_map1(ilink))%w(wgtdata(grid2_add_map1(ilink))%n)=wts_map1(1,ilink)
               wgtdata(grid2_add_map1(ilink))%k(wgtdata(grid2_add_map1(ilink))%n)=grid1_add_map1(ilink)
            endif
         endif
      end do

!.....step 4: re-normalize weights. This is necessary because we called scrip without the mask. Now that we have the mask in place, we need to re-normalize the weights.
      do kdst=1,grid2_size
         if (wgtdata(kdst)%n > 0) then
            wtsum=zero
            do ipnt=1,wgtdata(kdst)%n
               wtsum=wtsum+wgtdata(kdst)%w(ipnt)
            end do
            do ipnt=1,wgtdata(kdst)%n
               wgtdata(kdst)%w(ipnt)=wgtdata(kdst)%w(ipnt)/wtsum
            end do
         end if
      end do

!.....Call "destroy" / deallocation subroutine.
      call scrip_clear

!/ end of scrip_wrapper ----------------------------------------------------- /
!/
!#######################################################################
      end subroutine scrip_wrapper
!#######################################################################

!#######################################################################
      subroutine scrip_clear
!#######################################################################

!  1. Original author :
!
!     Erick Rogers, NRL
!
!  2. Last update :
!
!     See revisions.
!
!  3. Revisions :
!
!     5-May-2011 : Origination                         ( version 4.01 )
!
!  4. Copyright :
!
!  5. Purpose : 
!
!     "Clear" all variables declared at module level of SCRIP routines
!     (clear "common block" equivalent)
!
!  6. Method :
!
!     rules: 
!        - if not an array (scalar), set to zero or other start value
!        - if dimensioned array, set to zero
!        - if allocatable array, deallocate
!        - private variables: ignore for now, since we would need to 
!             make them public in order to clear them, which may do more
!             harm than good.
!
!  7. Parameters, Variables and types :
!  
!  8. Called by : 
!
!     Subroutine SCRIP_interface
!
!  9. Subroutines and functions used :
!
!     None
!
! 10. Error messages: 
!
! 11. Remarks :
!
!     We "clear" all variables with "save" attribute, 
!     both "module variables" and "subroutine variables"
!     including all variables that are initialized with a value
!     in the type declaration, e.g. "real :: x=5.0"
!
! 12. Structure :
!
! 13. Switches :
!
! 14. Source code :

      use SCRIP_KindsMod
      use scrip_timers
      use scrip_remap_vars
      use scrip_remap_conservative
      use scrip_iounitsmod
      use scrip_grids

      implicit none

      call timers_init ! takes care of all variables in timers.f
!     cycles_max=0 ! integer scalar, assumed taken care of by timers_init

!.....scrip_remap_write.f :
! src_mask_int and dst_mask_int are "private" and deallocated "in house"
!      if(allocated(src_mask_int))deallocate(src_mask_int)
!      if(allocated(dst_mask_int))deallocate(dst_mask_int)

!.....scrip_remap_vars.f :
      max_links_map1=0
      num_links_map1=0
      max_links_map2=0
      num_links_map2=0
      num_maps=0 
      num_wts=0   
      map_type=0  
      norm_opt=0
      resize_increment=0
      if(allocated(grid1_add_map1))deallocate(grid1_add_map1)
      if(allocated(grid2_add_map1))deallocate(grid2_add_map1)
      if(allocated(grid1_add_map2))deallocate(grid1_add_map2)
      if(allocated(grid2_add_map2))deallocate(grid2_add_map2)
      if(allocated(wts_map1))deallocate(wts_map1)
      if(allocated(wts_map2))deallocate(wts_map2)

!.....remap_conserv.f :
!.....scalars: 
      first_call_store_link_cnsrv = .true.
      first_call_locate_segstart= .true.
      first_call_locate_point= .true.
      first_call_get_srch_cells=.true.
      first_call_find_adj_cell=.true.
      avoid_pole_count = 0
      avoid_pole_offset = tiny
      last_cell_locate_segstart=0
      last_cell_grid_num_locate_segstart=0
      last_srch_grid_num_locate_segstart=0
      num_srch_cells_locate_segstart=0
      last_cell_locate_point=0
      last_cell_grid_num_locate_point=0
      last_srch_grid_num_locate_point=0
      num_srch_cell_locate_points=0
      srch_corners_locate_point=0
      srch_corners_find_adj_cell=0
      srch_corners_locate_segstart=0
      srch_corners_loc_get_srch_cells=0
      num_srch_cells_loc_get_srch_cells=0
      num_srch_cells_find_adj_cell=0
      last_cell_add_get_srch_cells=0
      last_cell_grid_num_get_srch_cells=0
      last_srch_grid_num_get_srch_cells=0
      last_cell_find_adj_cell=0
      last_cell_grid_num_find_adj_cell=0
!.....arrays :
      if(allocated(link_add1))deallocate(link_add1)
      if(allocated(link_add2))deallocate(link_add2)

      if(allocated(srch_add_loc_get_srch_cells))deallocate(srch_add_loc_get_srch_cells)
      if(allocated(srch_corner_lat_loc_get_srch_cells))deallocate(srch_corner_lat_loc_get_srch_cells)
      if(allocated(srch_corner_lon_loc_get_srch_cells))deallocate(srch_corner_lon_loc_get_srch_cells)
      if(allocated(srch_center_lat_loc_get_srch_cells))deallocate(srch_center_lat_loc_get_srch_cells)
      if(allocated(srch_center_lon_loc_get_srch_cells))deallocate(srch_center_lon_loc_get_srch_cells)

      if(allocated(srch_add_find_adj_cell))deallocate(srch_add_find_adj_cell)
      if(allocated(srch_corner_lat_find_adj_cell))deallocate(srch_corner_lat_find_adj_cell)
      if(allocated(srch_corner_lon_find_adj_cell))deallocate(srch_corner_lon_find_adj_cell)
      if(allocated(srch_center_lat_find_adj_cell))deallocate(srch_center_lat_find_adj_cell)
      if(allocated(srch_center_lon_find_adj_cell))deallocate(srch_center_lon_find_adj_cell)

      if(allocated(srch_add_locate_segstart))deallocate(srch_add_locate_segstart)
      if(allocated(srch_corner_lat_locate_segstart))deallocate(srch_corner_lat_locate_segstart)
      if(allocated(srch_corner_lon_locate_segstart))deallocate(srch_corner_lon_locate_segstart)
      if(allocated(srch_center_lat_locate_segstart))deallocate(srch_center_lat_locate_segstart)
      if(allocated(srch_center_lon_locate_segstart))deallocate(srch_center_lon_locate_segstart)

      if(allocated(srch_add_locate_point))deallocate(srch_add_locate_point)
      if(allocated(srch_corner_lat_locate_point))deallocate(srch_corner_lat_locate_point)
      if(allocated(srch_corner_lon_locate_point))deallocate(srch_corner_lon_locate_point)
      if(allocated(srch_center_lat_locate_point))deallocate(srch_center_lat_locate_point)
      if(allocated(srch_center_lon_locate_point))deallocate(srch_center_lon_locate_point)

!.....iounits.f :
!      unit_free = .true.
!      first_call_io = .true.

!.....scrip_grids.f :
      grid1_size=0
      grid2_size=0
      grid1_rank=0
      grid2_rank=0
      grid1_corners=0
      grid2_corners=0
      grid1_name=''
      grid2_name=''
      grid1_units=''
      grid2_units=''
      luse_grid_centers=.false.
      luse_grid1_area=.false.
      luse_grid2_area=.false.
      restrict_type=''
      num_srch_bins=0
      if(allocated(bin_addr1))deallocate(bin_addr1)
      if(allocated(bin_addr2))deallocate(bin_addr2)
      if(allocated(bin_lats))deallocate(bin_lats)
      if(allocated(bin_lons))deallocate(bin_lons)
      if(allocated(grid1_dims))deallocate(grid1_dims)
      if(allocated(grid2_dims))deallocate(grid2_dims)
      if(allocated(grid1_mask))deallocate(grid1_mask)
      if(allocated(grid2_mask))deallocate(grid2_mask)
      if(allocated(grid1_center_lat))deallocate(grid1_center_lat)
      if(allocated(grid1_center_lon))deallocate(grid1_center_lon)
      if(allocated(grid2_center_lat))deallocate(grid2_center_lat)
      if(allocated(grid2_center_lon))deallocate(grid2_center_lon)
      if(allocated(grid1_area))deallocate(grid1_area)
      if(allocated(grid2_area))deallocate(grid2_area)
      if(allocated(grid1_area_in))deallocate(grid1_area_in)
      if(allocated(grid2_area_in))deallocate(grid2_area_in)
      if(allocated(grid1_frac))deallocate(grid1_frac)
      if(allocated(grid2_frac))deallocate(grid2_frac)
      if(allocated(grid1_corner_lat))deallocate(grid1_corner_lat)
      if(allocated(grid1_corner_lon))deallocate(grid1_corner_lon)
      if(allocated(grid2_corner_lat))deallocate(grid2_corner_lat)
      if(allocated(grid2_corner_lon))deallocate(grid2_corner_lon)
      if(allocated(grid1_bound_box))deallocate(grid1_bound_box)
      if(allocated(grid2_bound_box))deallocate(grid2_bound_box)
      if(allocated(special_polar_cell1))deallocate(special_polar_cell1)
      if(allocated(special_polar_cell2))deallocate(special_polar_cell2)
      if(allocated(grid1_centroid_lat))deallocate(grid1_centroid_lat)
      if(allocated(grid1_centroid_lon))deallocate(grid1_centroid_lon)
      if(allocated(grid2_centroid_lat))deallocate(grid2_centroid_lat)
      if(allocated(grid2_centroid_lon))deallocate(grid2_centroid_lon)

!#######################################################################
      end subroutine scrip_clear
!#######################################################################

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     subroutine scrip:
!     This routine is the driver for computing the addresses and weights 
!     for interpolating between two grids on a sphere.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: scrip.f,v 1.6 2001/08/21 21:06:44 pwjones Exp $
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
!     This code has been modified from the version available from 
!     Los Alamos National Laboratory, for the purpose of running it
!     within WW3. Here is a list of modifications:
!     - changed from standalone program to a subroutine, to be called
!          by scrip_wrapper
!     - all modules are now prepended with "scrip_" to make them easier
!          to distinguish from WW3 code.
!     - code changed to free format style (e.g. continuation characters)
!     - print statements added
!     - initial values for settings are changed
!     - initial value for "map_method" added (is set to "conservative")
!     - read of settings from "scrip_in" is removed, and file "scrip_in"
!          is thus no longer used
!     - in context of scrip_wrapper, these initial values for the settings
!          are never changed. Thus, these settings are basically hardwired
!          in here.
!     - lines associated with remap_distance_weight, remap_bilinear, 
!          remap_bicubic are removed. Motivation: we do not need these
!          routines, so we opt to exclude them in our compile
!          (fewer .f files, fewer compiles).
!
!***********************************************************************

      subroutine scrip

!-----------------------------------------------------------------------

      use SCRIP_KindsMod                  ! module defining data types
      use scrip_constants                  ! module for common constants
      use scrip_iounitsmod                    ! I/O unit manager
      use scrip_timers                     ! CPU timers
      use scrip_grids                      ! module with grid information
      use scrip_remap_vars                 ! common remapping variables
      use scrip_remap_conservative         ! routines for conservative remap
!     use scrip_remap_distance_weight      ! routines for dist-weight remap
!     use scrip_remap_bilinear             ! routines for bilinear interp
!     use scrip_remap_bicubic              ! routines for bicubic  interp
      use scrip_remap_write                ! routines for remap output
      use scrip_errormod

      implicit none

!-----------------------------------------------------------------------
!
!     input variables formerly part of namelist
!
!-----------------------------------------------------------------------

      character (SCRIP_charLength) :: &
                 interp_file1,& ! filename for output remap data (map1)
                 interp_file2,& ! filename for output remap data (map2)
                 map1_name,   & ! name for mapping from grid1 to grid2
                 map2_name,   & ! name for mapping from grid2 to grid1
                 map_method,  & ! choice for mapping method
                 normalize_opt,&! option for normalizing weights
                 output_opt    ! option for output conventions

      integer (SCRIP_i4) :: &
                 nmap          ! number of mappings to compute (1 or 2)

!     namelist /remap_inputs/ grid1_file, grid2_file, &
!                             interp_file1, interp_file2, &
!                             map1_name, map2_name, num_maps, &
!                             luse_grid1_area, luse_grid2_area, &
!                             map_method, normalize_opt, output_opt, &
!                             restrict_type, num_srch_bins

!-----------------------------------------------------------------------
!
!     local variables
!
!-----------------------------------------------------------------------

      integer (SCRIP_i4) :: n, &   ! dummy counter
                                 iunit  ! unit number for namelist file

      integer (SCRIP_i4) :: &
           errorCode      ! error flag

      character (12), parameter :: &
           rtnName = 'SCRIP_driver'

!-----------------------------------------------------------------------
!
!     initialize timers and errorcode
!
!-----------------------------------------------------------------------

      write(*,*)'subroutine scrip'

      call timers_init
      do n=1,max_timers
        call timer_clear(n)
      end do

      errorCode = SCRIP_Success

!-----------------------------------------------------------------------
!
!     set variables that were previously read in as a namelist
!
!-----------------------------------------------------------------------

! reminder: later, we can remove the pointless parts, interp_file2 and map2_name
      num_maps = 1
      interp_file1 = 'rmp_source_to_destination_conserv.nc'
      interp_file2 = 'non-existent_file.nc'
      map1_name = 'source to destination Conservative Mapping'
      map2_name = 'non-existent map'
      map_method = 'conservative'
      normalize_opt = 'fracarea'
      output_opt = 'scrip'
      restrict_type = 'latitude'
      num_srch_bins = 90 
      luse_grid1_area = .false.
      luse_grid2_area = .false.
      npseg=11 ! or num_polar_segs
      north_thresh=1.5_SCRIP_r8 ! or npole_threshold
      south_thresh=-1.5_SCRIP_r8 ! or spole_threshold
      nthreads=2 ! or num_threads

!     call get_unit(iunit)
!     open(iunit, file='scrip_in', status='old', form='formatted')
!     read(iunit, nml=remap_inputs)
!     call release_unit(iunit)
      
      select case(map_method)
      case ('conservative')
         map_type = map_type_conserv
         luse_grid_centers = .false.
      case ('bilinear')
         map_type = map_type_bilinear
         luse_grid_centers = .true.
      case ('bicubic')
         map_type = map_type_bicubic
         luse_grid_centers = .true.
      case ('distwgt')
         map_type = map_type_distwgt
         luse_grid_centers = .true.
      case ('particle')
         map_type = map_type_particle
         luse_grid_centers = .false.
      case default
         call SCRIP_ErrorSet(errorCode, rtnName, 'unknown mapping method')
         call SCRIP_driverExit(errorCode, 'unknown mapping method')
      end select

      select case(normalize_opt(1:4))
      case ('none')
         norm_opt = norm_opt_none
      case ('frac')
         norm_opt = norm_opt_frcarea
      case ('dest')
         norm_opt = norm_opt_dstarea
      case default
         call SCRIP_ErrorSet(errorCode, rtnName, 'unknown normalization option')
         call SCRIP_driverExit(errorCode, 'unknown normalization option')
      end select

!-----------------------------------------------------------------------
!
!     initialize grid information for both grids
!
!-----------------------------------------------------------------------

      write(*,*)'calling grid_init'

      call grid_init( errorCode)

!       if (SCRIP_ErrorCheck(errorCode, rtnName, 'Error initializing grids')) &
!            call SCRIP_driverExit(errorCode, 'Error initializing grids')

      write(SCRIP_stdout, *) 'Computing remappings between: ',grid1_name
      write(SCRIP_stdout, *) '                         and  ',grid2_name

!-----------------------------------------------------------------------
!
!     initialize some remapping variables.
!
!-----------------------------------------------------------------------

      call init_remap_vars

!-----------------------------------------------------------------------
!
!     call appropriate interpolation setup routine based on type of
!     remapping requested.
!
!-----------------------------------------------------------------------

      select case(map_type)
      case(map_type_conserv)
         write(*,*)'calling remap_conserv'
         call remap_conserv
         write(*,*)'back from remap_conserv'
!     case(map_type_bilinear)
!        call remap_bilin
!     case(map_type_distwgt)
!        call remap_distwgt
!     case(map_type_bicubic)
!        call remap_bicub
!     case(map_type_particle)
!        call SCRIP_RemapParticleCreate(errorCode)
      case default
         call SCRIP_ErrorSet(errorCode, rtnName, 'Invalid Map Type')
         call SCRIP_driverExit(errorCode, 'Invalid Map Type')
      end select

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
      
      call write_remap(map1_name, map2_name, &
                       interp_file1, interp_file2, output_opt, errorCode)

!      if (SCRIP_ErrorCheck(errorCode, rtnName, 'error in write_remap')) &
!           call SCRIP_driverExit(errorCode, 'error in write_remap')

!-----------------------------------------------------------------------

    end subroutine scrip

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine SCRIP_driverExit(errorCode,errormsg)

! !DESCRIPTION:
!  This routine exits the SCRIP driver program. It first calls the 
!  SCRIP error print function to print any errors encountered and then
!  exits the message environment before stopping.
!
! !REVISION HISTORY:
!  SVN:$Id: $

! !USES:

   use SCRIP_KindsMod

! !INPUT PARAMETERS:

   integer (SCRIP_i4), intent(in) :: &
      errorCode        ! error flag to detect any errors encountered

   CHARACTER*(*), INTENT(IN)  :: errormsg
!BOC
!-----------------------------------------------------------------------
!
!  call SCRIP error print function to output any logged errors that
!  were encountered during execution.  Then stop.
!
!-----------------------------------------------------------------------

   write(*,*)'error encountered : ',errorcode
   write(*,*)errormsg

   stop

!-----------------------------------------------------------------------
!EOC

   end subroutine SCRIP_driverExit


!/
!/ End of module SCRIP_INTERFACE -------------------------------------------- /
!/
      END MODULE SCRIP_INTERFACE


