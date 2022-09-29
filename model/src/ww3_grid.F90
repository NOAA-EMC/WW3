!> @file
!> @brief Contains the "Grid" preprocessing program.
!>
!> @author No author listed @date 27-May-2021

#include "w3macros.h"

!> @brief "Grid" preprocessing program, which writes a model definition
!>  file containing the model parameter settigs and grid data.
!>
!> @details Information is read from the file ww3_grid.inp/nml.
!>  A model definition file mod_def.ww3 is then produced by W3IOGR.
!>  Note that the name of the model definition file is set in W3IOGR.
!>  This is now all done in the subroutine W3GRID.
!>
!> @author No author listed @date 27-May-2021
!/ ------------------------------------------------------------------- /
PROGRAM WW3GRID
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |                                   |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         27-May-2021 |
  !/                  +-----------------------------------+
  !/    27-May-2021 : Seperated subroutine to w3grid   ( version 7.13 )
  !/
  !/    Copyright 2009-2013 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     "Grid" preprocessing program, which writes a model definition
  !     file containing the model parameter settigs and grid data.
  !
  !  2. Method :
  !
  !     Information is read from the file ww3_grid.inp/nml.
  !     A model definition file mod_def.ww3 is then produced by W3IOGR.
  !     Note that the name of the model definition file is set in W3IOGR.
  !     This is now all done in the subroutine W3GRID
  !
  !  3. Parameters :
  !      none
  !
  !  4. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3GRID    Subr. W3GRIDMD creates mod_def file
  !
  !  5. Called by :
  !
  !     None, stand-alone program.
  !
  !  6. Error messages :
  !
  !  7. Remarks :
  !        See w3gridmd.ftn for details
  !
  !  8. Structure :
  !        Call subroutine W3GRID
  !
  !  9. Switches :
  !        none
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /

  USE W3GRIDMD, ONLY: W3GRID
  IMPLICIT NONE

  CALL W3GRID

END PROGRAM WW3GRID
