!> @file
!> @brief Defines MPI communicator id as constants for global use.
!>
!> @author H. L. Tolman  @date 05-Jun-2018
!>
#include "w3macros.h"

!>
!> @brief Define some mpi constants for global use
!>
!> @author  J. M. Sexton  @date 19-Jul-2024
!>
!
!
!/ ------------------------------------------------------------------- /
MODULE MPICOMM
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           H. L. Tolman            |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         19-Jul-2024 |
  !/                  +-----------------------------------+
  !/
  !/    19-Jul-2024 : Add MPI SubCommunicator variable    ( version 7.xx )
  !/
  !/    Copyright 2009-2024 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Define some mpi constants for global use
  !
  !  2. Variables and types :
  !
  !      Name                     Type  Scope    Description
  !     ----------------------------------------------------------------
  !      MPI_COMM_WW3             Int   Global   Value for mpi (sub)communicator for WW3
  !      IS_EXTERNAL_COMPONENT    Bool  Global   General logical similar to IS_ESMF_COMPONENT
  !     ----------------------------------------------------------------
  !/ ------------------------------------------------------------------- /
  !/
  !

  INTEGER :: MPI_COMM_WW3=0 !< MPI_COMM_WW3
  !
  ! Parameters in support of running as ESMF component
  !
  ! --- Flag indicating whether or not the model has been invoked as an
  !     external Component.  This flag is set to true in the external
  !     module during initialization.
  LOGICAL :: IS_EXTERNAL_COMPONENT = .FALSE. !< IS_EXTERNAL_COMPONENT Flag for model invoked via external executable.
  !
  CONTAINS

  !/
  !/ End of module MPICOMM ------------------------------------------- /
  !/
END MODULE MPICOMM
