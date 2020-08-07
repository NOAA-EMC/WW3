!****************************************************************************************
SUBROUTINE read_dimgrid (nlon,nlat,data_filename,w_unit,krank)
  !**************************************************************************************
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER                  :: i,w_unit,krank
  !
  INTEGER                  :: il_file_id,il_lon_id, &
     il_lat_id,il_indice_id, &
     lon_dims,lat_dims,imask_dims
  !
  INTEGER, DIMENSION(NF90_MAX_VAR_DIMS) :: lon_dims_ids,lat_dims_ids,&
     imask_dims_ids,lon_dims_len,&
     lat_dims_len,imask_dims_len  
  !               
  INTEGER, INTENT(out)     :: nlon,nlat
  !
  CHARACTER(len=30)        :: data_filename
  !
  ! Dimensions
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__,__FILE__ )
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'longitude' ,  il_lon_id),    __LINE__,__FILE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'latitude' ,  il_lat_id),    __LINE__,__FILE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'imask_t', il_indice_id), __LINE__,__FILE__ )
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(il_file_id, varid=il_lon_id, ndims=lon_dims, dimids=lon_dims_ids), __LINE__,__FILE__ )
  !
  ! The value lon_dims_len(i) is obtained thanks to the lon_dims_ids ID already obtained from the file
  DO i=1,lon_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=lon_dims_ids(i),len=lon_dims_len(i)), __LINE__,__FILE__ )
  ENDDO
  !
  nlon=lon_dims_len(1)
  nlat=lon_dims_len(2)
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_lat_id, ndims=lat_dims, dimids=lat_dims_ids), __LINE__,__FILE__ )
  !
  ! The value lat_dims_len(i) is obtained thanks to the lat_dims_ids ID already obtained from the file
  DO i=1,lat_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=lat_dims_ids(i),len=lat_dims_len(i)), __LINE__,__FILE__ )
  ENDDO
  !
  IF ( (lat_dims_len(1) .NE. lon_dims_len(1)).OR.(lat_dims_len(2) .NE. lon_dims_len(2)) ) THEN
    IF(krank.EQ.0) THEN
      WRITE(w_unit,*) 'Problem model1 in read_dimgrid'
      WRITE(w_unit,*) 'Dimensions of the latitude are not the same as the ones of the longitude'
      CALL flush(w_unit)
      STOP
    ENDIF
  ENDIF
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  CALL hdlerr( NF90_INQUIRE_VARIABLE(ncid=il_file_id, varid=il_indice_id, ndims=imask_dims, dimids=imask_dims_ids), __LINE__,__FILE__ )
  !
  ! The value imask_dims_len(i) is obtained thanks to the imask_dims_ids ID already obtained from the file
  DO i=1,imask_dims
    CALL hdlerr( NF90_INQUIRE_DIMENSION(ncid=il_file_id,dimid=imask_dims_ids(i),len=imask_dims_len(i)), __LINE__,__FILE__ )
  ENDDO
  !
  CALL hdlerr(NF90_CLOSE(il_file_id), __LINE__,__FILE__ )
  !
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'Reading input file ',data_filename
    WRITE(w_unit,*) 'Global dimensions nlon=',nlon,' nlat=',nlat
    CALL flush(w_unit)
  ENDIF
  !
  !
END SUBROUTINE read_dimgrid
