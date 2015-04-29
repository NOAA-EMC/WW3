!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This module defines the F90 kind parameter for common data types.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: kinds_mod.f 818 2006-03-10 17:18:31Z valcke $
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

      module kinds_mod

!-----------------------------------------------------------------------

      implicit none
      save

!-----------------------------------------------------------------------

      integer, parameter :: char_len  = 80
      integer, parameter :: int_kind  = SELECTED_INT_KIND(9)
      integer, parameter :: log_kind  = kind(.true.)
      integer, parameter :: real_kind = SELECTED_REAL_KIND(12,307)
      integer, parameter :: dbl_kind  = SELECTED_REAL_KIND(12,307)
!-----------------------------------------------------------------------
! hardwire for now, tcraig

      INTEGER           :: nlogprt = 2
      INTEGER           :: nulou = 6
      logical,parameter :: ll_single = .false.   ! single reals
      logical,parameter :: lncdfgrd = .true.     ! grid files netcdf
      integer,parameter :: jpeight = 8
      character(len=*),parameter :: cgrdnam = 'grids'
      character(len=*),parameter :: cglonsuf  = '.lon'
      character(len=*),parameter :: cglatsuf  = '.lat'
      character(len=*),parameter :: crnlonsuf = '.clo'
      character(len=*),parameter :: crnlatsuf = '.cla'

      INTEGER, DIMENSION (:), ALLOCATABLE :: snum_links, snum_wgts
      LOGICAL, DIMENSION (:), ALLOCATABLE :: sweight_flag

      TYPE wp
         REAL(kind=dbl_kind), POINTER :: warray(:,:)
      END TYPE wp
      type(wp), allocatable :: sweigth(:)

      TYPE sp
        INTEGER, POINTER :: srcarray(:)
      END TYPE sp
      type(sp), allocatable :: ssrc_addr(:)

      TYPE dp
        INTEGER, POINTER :: dstarray(:)
      END TYPE dp
      type(dp), allocatable :: sdst_addr(:)

      end module kinds_mod

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
