!*********************************************************************************
SUBROUTINE hdlerr(istatus, line, current_file)
  !*********************************************************************************
  use netcdf
  implicit none
  !
  INCLUDE 'mpif.h'
  !
  ! Check for error message from NetCDF call
  !
  integer, intent(in) :: istatus, line
  character(len=*), intent(in) :: current_file
  integer             :: ierror
  !
  IF (istatus .NE. NF90_NOERR) THEN
      write ( * , * ) 'NetCDF problem at line', line, 'in ', current_file
      write ( * , * ) 'Stopped '
      call MPI_Abort ( MPI_COMM_WORLD, 1, ierror )
  ENDIF
  !
  RETURN
END SUBROUTINE hdlerr
