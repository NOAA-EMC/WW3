!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!     This module defines common constants used in many routines.
!
!-----------------------------------------------------------------------
!
!     CVS:$Id: constants.f 818 2006-03-10 17:18:31Z valcke $
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

      module constants

!-----------------------------------------------------------------------

      use kinds_mod  ! defines common data types

      implicit none

      save

!-----------------------------------------------------------------------

      real (kind = dbl_kind), parameter :: 
     &                        zero   = 0.0_dbl_kind,
     &                        one    = 1.0_dbl_kind,
     &                        two    = 2.0_dbl_kind,
     &                        three  = 3.0_dbl_kind,
     &                        four   = 4.0_dbl_kind,
     &                        five   = 5.0_dbl_kind,
     &                        half   = 0.5_dbl_kind,
     &                        quart  = 0.25_dbl_kind,
     &                        bignum = 1.e+20_dbl_kind,
     &                        tiny   = 1.e-14_dbl_kind,
     &                        pi     = 3.14159265358979323846_dbl_kind,
     &                        pi2    = two*pi,
     &                        pih    = half*pi

!-----------------------------------------------------------------------

      end module constants

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
