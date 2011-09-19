!/ ------------------------------------------------------------------- /
      MODULE CONSTANTS
!/
!/    Copyright 2009-2010 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  iDistributed as part of WAVEWATCH III. WAVEWATCH III
!/       is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!$$$  MODULE DOCUMENTATION BLOCK
!                .      .    .                                       .
! MODULE:        CONSTANTS   Define some constants globally
!   PRGMMR: H.L. Tolman      ORG: W/NP21      DATE: 1999-11-30
!
! ABSTRACT: Define some constants globally
!
! PROGRAM HISTORY LOG:
! 1999-11-30  Tolman      Origination.
!
! PUBLIC VARIABLES :      See documentation below.
! PUBLIC TYPES :          See documentation below.
! PUBLIC SUBPROGRAMS :    See documentation below.
!
! REMARKS:                None.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Portable code
!
!$$$
!/                  +-----------------------------------+
!/                  | WAVEWATCH-III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         11-Nov-1999 |
!/                  +-----------------------------------+
!/
!  1. Purpose :
!
!     Define some much-used constants for global use (all defined
!     as PARAMETER).
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      GRAV      Real  Global   Acc. of gravity                 (m/s2)
!      DWAT      Real  Global   Density of water               (kg/m3)
!      DAIR      Real  Global   Density of air                 (kg/m3)
!      PI        Real  Global   pi.
!      TPI       Real  Global   2pi.
!      HPI       Real  Global   0.5pi.
!      TPIINV    Real  Global   1/2pi.
!      HPIINV    Real  Global   2/pi.
!      RADE      Real  Global   Conv. factor from radians to degrees.
!      DERA      Real  Global   Conv. factor from degrees to radians.
!      RADIUS    Real  Global   Radius of the earth.             (m)
!     ----------------------------------------------------------------
!
!/ ------------------------------------------------------------------- /
!/
      REAL, PARAMETER         :: GRAV   =    9.806
      REAL, PARAMETER         :: DWAT   = 1000.
      REAL, PARAMETER         :: DAIR   =    1.225
!
      REAL, PARAMETER         :: PI     = 3.1415927
      REAL, PARAMETER         :: TPI    = 2.0 * PI
      REAL, PARAMETER         :: HPI    = 0.5 * PI
      REAL, PARAMETER         :: TPIINV = 1. / TPI
      REAL, PARAMETER         :: HPIINV = 1. / HPI
      REAL, PARAMETER         :: RADE   = 180. / PI
      REAL, PARAMETER         :: DERA   = PI / 180.
!
      REAL, PARAMETER         :: RADIUS = 4.E7 * TPIINV
!
      REAL, PARAMETER         :: G2PI3I = 1. / ( GRAV**2 * TPI**3 )
      REAL, PARAMETER         :: G1PI1I = 1. / ( GRAV * TPI )
!/
!/ End of module CONSTANTS ------------------------------------------- /
!/
      END MODULE CONSTANTS
