MODULE mod_oasis_grid
!-----------------------------------------------------------------------
! BOP
!
! !MODULE:  mod_prism_grid
! !REMARKS: 
!
! **********************
! THIS SHOULD BE CALLED BY A SINGLE PE ACCORDING TO THE OASIS3 
! STANDARD.  THE DATA IS GLOBAL.
! **********************
!
! !REVISION HISTORY:
! 
!
! !PUBLIC MEMBER FUNCTIONS:
! 
!      subroutine oasis_start_grids_writing(iwrite)
!             This subroutine initializes grid writing by receiving a 
!             starting command from OASIS.
!
!      subroutine oasis_write_grid(cgrid, nx, ny, lon, lat)
!	      This subroutine writes longitudes and latitudes for a model
!             grid.
!
!      subroutine oasis_write_corner(cgrid, nx, ny, nc, clon, clat)
!	      This subroutine writes the longitudes and latitudes of the
!             grid cell corners.
!
!      subroutine oasis_write_mask(cgrid, nx, ny, mask)
!	      This subroutine writes the mask for a model grid
!
!      subroutine oasis_write_area(cgrid, nx, ny, area)
!	      This subroutine writes the grid cell areas for a model grid.
!
!      subroutine oasis_terminate_grids_writing()
!             This subroutine terminates grid writing by sending a flag
!             to OASIS, stating the all needed grid information was written.
!       

! !USES:
  use mod_oasis_data
  use mod_oasis_io
  use mod_oasis_sys
  
  implicit none

  private

  public oasis_start_grids_writing
  public oasis_write_grid
  public oasis_write_angle
  public oasis_write_corner
  public oasis_write_mask
  public oasis_write_area   
  public oasis_terminate_grids_writing 
  public oasis_write2files

  interface oasis_write_grid
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_grid_r4
#endif
     module procedure oasis_write_grid_r8
  end interface

  interface oasis_write_angle
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_angle_r4
#endif
     module procedure oasis_write_angle_r8
  end interface

  interface oasis_write_corner
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_corner_r4
#endif
     module procedure oasis_write_corner_r8
  end interface

  interface oasis_write_area
#ifndef __NO_4BYTE_REALS
     module procedure oasis_write_area_r4
#endif
     module procedure oasis_write_area_r8
  end interface

  !--- datatypes ---
  public :: prism_grid_type

  integer(kind=ip_intwp_p),parameter :: mgrid = 100

  type prism_grid_type
     character(len=ic_med)  :: gridname
     integer(kind=ip_i4_p)  :: nx
     integer(kind=ip_i4_p)  :: ny
     integer(kind=ip_i4_p)  :: nc
     logical                :: grid_set
     logical                :: corner_set
     logical                :: angle_set
     logical                :: area_set
     logical                :: mask_set
     logical                :: written
     logical                :: terminated
     real(kind=ip_realwp_p),allocatable :: lon(:,:)     ! longitudes
     real(kind=ip_realwp_p),allocatable :: lat(:,:)     ! latitudes
     real(kind=ip_realwp_p),allocatable :: clon(:,:,:)  ! corner longitudes
     real(kind=ip_realwp_p),allocatable :: clat(:,:,:)  ! corner latitudes
     real(kind=ip_realwp_p),allocatable :: angle(:,:)   ! angle
     real(kind=ip_realwp_p),allocatable :: area(:,:)    ! area
     integer(kind=ip_i4_p) ,allocatable :: mask(:,:)    ! mask
  end type prism_grid_type

  integer(kind=ip_intwp_p),public,save :: prism_ngrid = 0
  type(prism_grid_type),public,save :: prism_grid(mgrid)


#ifdef use_netCDF
#include <netcdf.inc>
#endif

!---------------------------------------------------------------------------

CONTAINS

!--------------------------------------------------------------------------
    SUBROUTINE oasis_start_grids_writing(iwrite)

    !-------------------------------------------------
    ! Routine to start the grids writing. To syncronize access to the
    ! grids file all component models have to wait for the starting 
    ! message from OASIS (via MPI; see prism_init_comp_proto)
    !-------------------------------------------------

    implicit none
  
    integer(kind=ip_intwp_p), intent (OUT) :: iwrite ! flag to state whether
                                            ! grids file needs to be written
    !-------------------------------------------------
    character(len=*),parameter :: subname = 'oasis_start_grids_writing'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       write(nulprt,*) subname,' ERROR subroutine call by non root processor'
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif

    if (prism_ngrid == 0) then  ! first call
       prism_grid(:)%grid_set   = .false.
       prism_grid(:)%corner_set = .false.
       prism_grid(:)%angle_set  = .false.
       prism_grid(:)%area_set   = .false.
       prism_grid(:)%mask_set   = .false.
       prism_grid(:)%written    = .false.
    endif
    iwrite = 1   ! just set grids are needed always

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_start_grids_writing

!--------------------------------------------------------------------------

    SUBROUTINE oasis_write_grid_r8(cgrid, nx, ny, lon, lat)

    !-------------------------------------------------
    ! Routine to create a new grids file or to add a grid description to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid      ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx         ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny         ! number of latitudes
    real(kind=ip_double_p),   intent (in) :: lon(nx,ny) ! longitudes
    real(kind=ip_double_p),   intent (in) :: lat(nx,ny) ! latitudes
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_grid_r8'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       write(nulprt,*) subname,' ERROR subroutine call by non root processor'
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%lon(nx,ny),stat=ierror)
    IF (ierror /= 0) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lon alloc'
    allocate(prism_grid(gridID)%lat(nx,ny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lat alloc'
    prism_grid(gridID)%lon = lon
    prism_grid(gridID)%lat = lat
    prism_grid(gridID)%grid_set = .true.

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_grid_r8

!--------------------------------------------------------------------------

    SUBROUTINE oasis_write_grid_r4(cgrid, nx, ny, lon, lat)

    !-------------------------------------------------
    ! Routine to create a new grids file or to add a grid description to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid      ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx         ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny         ! number of latitudes
    real(kind=ip_single_p),   intent (in) :: lon(nx,ny) ! longitudes
    real(kind=ip_single_p),   intent (in) :: lat(nx,ny) ! latitudes
    !-------------------------------------------------
    real(kind=ip_double_p), allocatable :: lon8(:,:)
    real(kind=ip_double_p), allocatable :: lat8(:,:)
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_grid_r4'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    allocate(lon8(nx,ny),stat=ierror)
    IF (ierror /= 0) WRITE(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lon alloc'
    allocate(lat8(nx,ny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING lat alloc'

    lon8 = lon
    lat8 = lat
    call oasis_write_grid_r8(cgrid,nx,ny,lon8,lat8)
    deallocate(lon8)
    deallocate(lat8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_grid_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_angle_r8(cgrid, nx, ny, angle)

    !-------------------------------------------------
    ! Routine to add angles to an existing grid file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_double_p),   intent (in) :: angle(nx,ny) ! angles
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_angle_r8'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       write(nulprt,*) subname,' ERROR subroutine call by non root processor'
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%angle(nx,ny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING angle alloc'
    prism_grid(gridID)%angle = angle
    prism_grid(gridID)%angle_set = .true.

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_angle_r8

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_angle_r4(cgrid, nx, ny, angle)

    !-------------------------------------------------
    ! Routine to add angles to an existing grid file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_single_p),   intent (in) :: angle(nx,ny) ! angles
    !-------------------------------------------------
    real(kind=ip_double_p),allocatable :: angle8(:,:)
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_angle_r4'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    allocate(angle8(nx,ny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING angle8 alloc'

    angle8 = angle
    call oasis_write_angle_r8(cgrid,nx,ny,angle8)

    deallocate(angle8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_angle_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_corner_r8(cgrid, nx, ny, nc, clon, clat)

    !-------------------------------------------------
    ! Routine to add longitudes and latitudes of grid cell corners to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid  ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx     ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny     ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: nc     ! number of corners per cell
    real(kind=ip_double_p),   intent (in) :: clon(nx,ny,nc) ! longitudes
    real(kind=ip_double_p),   intent (in) :: clat(nx,ny,nc) ! latitudes
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_corner_r8'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       write(nulprt,*) subname,' ERROR subroutine call by non root processor'
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       call oasis_abort_noarg()
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%clon(nx,ny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clon alloc'
    allocate(prism_grid(gridID)%clat(nx,ny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clat alloc'
    prism_grid(gridID)%nc = nc
    prism_grid(gridID)%clon = clon
    prism_grid(gridID)%clat = clat
    prism_grid(gridID)%corner_set = .true.

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_corner_r8

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_corner_r4(cgrid, nx, ny, nc, clon, clat)

    !-------------------------------------------------
    ! Routine to add longitudes and latitudes of grid cell corners to an
    ! existing grids file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid  ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx     ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny     ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: nc     ! number of corners per cell
    real(kind=ip_single_p),   intent (in) :: clon(nx,ny,nc) ! longitudes
    real(kind=ip_single_p),   intent (in) :: clat(nx,ny,nc) ! latitudes
    !-------------------------------------------------
    real(kind=ip_double_p), allocatable :: clon8(:,:,:),clat8(:,:,:)
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_corner_r4'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    allocate(clon8(nx,ny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clon8 alloc'
    allocate(clat8(nx,ny,nc),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING clat8 alloc'

    clon8 = clon
    clat8 = clat
    call oasis_write_corner_r8(cgrid,nx,ny,nc,clon8,clat8)

    deallocate(clon8)
    deallocate(clat8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_corner_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_mask(cgrid, nx, ny, mask)

    !-------------------------------------------------
    ! Routine to create a new masks file or to add a land see mask to an
    ! existing masks file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    integer(kind=ip_intwp_p), intent (in) :: mask(nx,ny) ! mask
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_mask'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       write(nulprt,*) subname,' ERROR subroutine call by non root processor'
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%mask(nx,ny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING mask alloc'
    prism_grid(gridID)%mask = mask
    prism_grid(gridID)%mask_set = .true.

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_mask

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_area_r8(cgrid, nx, ny, area)

    !-------------------------------------------------
    ! Routine to create a new areas file or to add areas of a grid to an
    ! existing areas file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_double_p),   intent (in) :: area(nx,ny) ! areas
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: GRIDID
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_area_r8'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       write(nulprt,*) subname,' ERROR subroutine call by non root processor'
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif

    call oasis_findgrid(cgrid,nx,ny,gridID)

    allocate(prism_grid(gridID)%area(nx,ny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING area alloc'
    prism_grid(gridID)%area = area
    prism_grid(gridID)%area_set = .true.

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_area_r8

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write_area_r4(cgrid, nx, ny, area)

    !-------------------------------------------------
    ! Routine to create a new areas file or to add areas of a grid to an
    ! existing areas file.
    !-------------------------------------------------

    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    real(kind=ip_single_p),   intent (in) :: area(nx,ny) ! areas
    !-------------------------------------------------
    real(kind=ip_double_p), allocatable :: area8(:,:)
    integer(kind=ip_intwp_p) :: ierror
    character(len=*),parameter :: subname = 'oasis_write_area_r4'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    allocate(area8(nx,ny),stat=ierror)
    if (ierror /= 0) write(nulprt,*) subname,' model :',compid,' proc :',&
                                     mpi_rank_local,' WARNING area8 alloc'

    area8 = area
    call oasis_write_area_r8(cgrid,nx,ny,area8)

    deallocate(area8)

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write_area_r4

!--------------------------------------------------------------------------
    SUBROUTINE oasis_terminate_grids_writing()
    !-------------------------------------------------
    ! Routine to terminate the grids writing.
    !-------------------------------------------------

    implicit none
    integer(kind=ip_i4_p) :: n
    character(len=*),parameter :: subname = 'oasis_terminate_grids_writing'

    call oasis_debug_enter(subname)

    if (mpi_rank_local /= mpi_root_local) then
       write(nulprt,*) subname,' ERROR subroutine call by non root processor'
       WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
       CALL oasis_flush(nulprt)
       call oasis_abort_noarg()
    endif

    do n = 1,prism_ngrid
       prism_grid(n)%terminated = .true.
    enddo

! moved to prism_method_enddef for synchronization
!    call oasis_write2files()

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_terminate_grids_writing

!--------------------------------------------------------------------------
    SUBROUTINE oasis_write2files()

    !-------------------------------------------------
    ! Write fields to grid files.
    ! Only write fields that have been buffered and
    ! if prism_grid_terminate_grids_writing has been called
    !-------------------------------------------------

    implicit none

    !-------------------------------------------------
    character(len=ic_med) :: filename  ! grid filename
    character(len=ic_med) :: fldname   ! full field name
    character(len=ic_med) :: cgrid     ! grid name
    logical :: exists                  ! check if file exists
    integer(kind=ip_i4_p) :: n         ! counter
    integer(kind=ip_i4_p) :: nx,ny,nc  ! grid size
    character(len=*),parameter :: subname = 'oasis_write2files'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    do n = 1,prism_ngrid
    if (prism_grid(n)%terminated) then
       cgrid = trim(prism_grid(n)%gridname)
       prism_grid(n)%written = .true.

       nx = prism_grid(n)%nx
       ny = prism_grid(n)%ny
       nc = prism_grid(n)%nc

       if (prism_grid(n)%grid_set) then
          filename = 'grids.nc'
          fldname  = trim(cgrid)//'.lon'
          call oasis_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%lon,nx,ny)
          fldname  = trim(cgrid)//'.lat'
          call oasis_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%lat,nx,ny)
       endif

       if (prism_grid(n)%corner_set) then
          filename = 'grids.nc'
          fldname  = trim(cgrid)//'.clo'
          call oasis_io_write_3dgridfld_fromroot(filename,fldname,prism_grid(n)%clon,nx,ny,nc)
          fldname  = trim(cgrid)//'.cla'
          call oasis_io_write_3dgridfld_fromroot(filename,fldname,prism_grid(n)%clat,nx,ny,nc)
       endif

       if (prism_grid(n)%area_set) then
          filename = 'areas.nc'
          fldname  = trim(cgrid)//'.srf'
          call oasis_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%area,nx,ny)
       endif

       if (prism_grid(n)%angle_set) then
          filename = 'grids.nc'
          fldname  = trim(cgrid)//'.ang'
          call oasis_io_write_2dgridfld_fromroot(filename,fldname,prism_grid(n)%angle,nx,ny)
       endif

       if (prism_grid(n)%mask_set) then
          filename = 'masks.nc'
          fldname  = trim(cgrid)//'.msk'
          call oasis_io_write_2dgridint_fromroot(filename,fldname,prism_grid(n)%mask,nx,ny)
       endif

    endif  ! terminated
    enddo

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_write2files
!--------------------------------------------------------------------------

    SUBROUTINE oasis_findgrid(cgrid,nx,ny,gridID)
    !-------------------------------------------------
    ! Routine that sets gridID, identifies existing
    ! grid with cgrid name or starts a new one
    !-------------------------------------------------
    implicit none

    character(len=*),         intent (in) :: cgrid       ! grid acronym
    integer(kind=ip_intwp_p), intent (in) :: nx          ! number of longitudes
    integer(kind=ip_intwp_p), intent (in) :: ny          ! number of latitudes
    integer(kind=ip_intwp_p), intent(out) :: gridID      ! gridID matching cgrid
    !-------------------------------------------------
    integer(kind=ip_intwp_p) :: n
    character(len=*),parameter :: subname = 'oasis_findgrid'
    !-------------------------------------------------

    call oasis_debug_enter(subname)

    gridID = -1
    do n = 1,prism_ngrid
       if (trim(cgrid) == trim(prism_grid(n)%gridname)) then
          gridID = n
          ! since grid is defined before, make sure nx,ny match
          if (nx /= prism_grid(gridID)%nx .or. ny /= prism_grid(gridID)%ny) then
             write(nulprt,*) subname,' ERROR in predefined grid size',nx,ny, &
                prism_grid(gridID)%nx,prism_grid(gridID)%ny
             WRITE(nulprt,*) subname,' abort by model :',compid,' proc :',mpi_rank_local
             CALL oasis_flush(nulprt)
             call oasis_abort_noarg()
          endif
       endif
    enddo

    if (gridID < 1) then
       prism_ngrid = prism_ngrid+1
       gridID = prism_ngrid
    endif

    prism_grid(gridID)%gridname = trim(cgrid)
    prism_grid(gridID)%nx = nx
    prism_grid(gridID)%ny = ny

    call oasis_debug_exit(subname)

  END SUBROUTINE oasis_findgrid
!--------------------------------------------------------------------------
END MODULE mod_oasis_grid
!--------------------------------------------------------------------------


     
