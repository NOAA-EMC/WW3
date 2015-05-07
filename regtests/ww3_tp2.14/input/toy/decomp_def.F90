!****************************************************************************************
SUBROUTINE decomp_def(id_paral,id_size,id_im,id_jm,id_rank,id_npes,id_unit)
  !
  IMPLICIT NONE
  INTEGER, DIMENSION(id_size), INTENT(out) :: id_paral(id_size)    
  INTEGER, INTENT(in)  :: id_size
  INTEGER, INTENT(in)  :: id_im       ! Grid dimension in i
  INTEGER, INTENT(in)  :: id_jm       ! Grid dimension in j
  INTEGER, INTENT(in)  :: id_rank     ! Rank of process
  INTEGER, INTENT(in)  :: id_npes     ! Number of processes involved in the coupling
  INTEGER, INTENT(in)  :: id_unit     ! Unit of log file
  INTEGER              :: il_imjm, il_partj
  !
  il_imjm = id_im*id_jm
  il_partj = id_jm/id_npes  ! Nbr of latitude circles in the partition
  !
#ifdef DECOMP_APPLE
  ! Each process is responsible for a part of field defined by
  ! the number of grid points and the offset of the first point
  !
  WRITE (id_unit,*) 'APPLE partitioning'
  !
  IF (id_rank .LT. (id_npes-1)) THEN
      id_paral (1) = 1
      id_paral (2) = id_rank*(il_partj * id_im)
      id_paral (3) = il_partj * id_im
  ELSE
      id_paral (1) = 1
      id_paral (2) = id_rank*(il_partj * id_im)
      id_paral (3) = il_imjm-(id_rank*(il_partj * id_im))
  ENDIF
  !
#elif defined DECOMP_BOX
  !
  WRITE (id_unit,*) 'BOX partitioning'
  !
  ! Each process is responsible for a rectangular box of 32x48 points
  !
  id_paral (1) = 2
  id_paral (5) = id_im
  id_paral (2) = id_rank*il_partj*id_im
  id_paral (3) = id_im
  IF (id_rank .LT. (id_npes-1)) THEN
      id_paral (4) = il_partj
  ELSE
      id_paral (4) = id_jm-(id_rank*il_partj)
  ENDIF
  !
#endif
  !
END SUBROUTINE decomp_def

