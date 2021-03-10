  !****************************************************************************************
  SUBROUTINE read_grid (nlon,nlat,corners_ij_lus, &
                                       data_filename, w_unit, krank, &
                                       gridlon,gridlat, &
                                       gridclo,gridcla, &
                                       gridsrf, &
                                       indice_mask)
  !**************************************************************************************
  !
  USE netcdf
  IMPLICIT NONE
  !
  INTEGER                  :: w_unit, krank
  !
  INTEGER                  :: il_file_id,il_lon_id, &
                              il_lat_id,il_clo_id,il_cla_id,il_srf_id,il_indice_id
  !
  INTEGER, INTENT(in)     :: nlon,nlat,corners_ij_lus
  !
  CHARACTER(len=30)        :: data_filename
  !
  INTEGER,  DIMENSION(3)   :: ila_dim
  INTEGER,  DIMENSION(3)   :: ila_corners,ila_what
  !
  REAL, DIMENSION(nlon,nlat)                :: gridlon,gridlat,gridsrf
  REAL, DIMENSION(nlon,nlat,corners_ij_lus) :: gridclo,gridcla
  INTEGER, DIMENSION(nlon,nlat)                      :: indice_mask
  !
  !
  ! Dimensions
  !
  CALL hdlerr(NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__,__FILE__ )
  !
  !
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'longitude' , il_lon_id), __LINE__,__FILE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'latitude' , il_lat_id), __LINE__,__FILE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'clo_f' , il_clo_id), __LINE__,__FILE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'cla_f' , il_cla_id), __LINE__,__FILE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'srf' , il_srf_id), __LINE__,__FILE__ )
  CALL hdlerr( NF90_INQ_VARID(il_file_id, 'imask_t' , il_indice_id), __LINE__,__FILE__ )
  !
  ila_what(:)=1
  !
  ila_dim(1)=nlon
  ila_dim(2)=nlat
  ila_dim(3)=1
  !
  ila_corners(1)=nlon
  ila_corners(2)=nlat
  ila_corners(3)=corners_ij_lus
  !
  ! Data
  !
  CALL hdlerr( NF90_OPEN(data_filename, NF90_NOWRITE, il_file_id), __LINE__,__FILE__ )
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lon_id, gridlon, &
     ila_what(1:2), ila_dim(1:2)), __LINE__,__FILE__ )
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'Global grid longitudes reading done'
    CALL flush(w_unit)
  ENDIF
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_lat_id, gridlat, &
     ila_what(1:2), ila_dim(1:2)), __LINE__,__FILE__ )
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'Global grid latitudes reading done'
    CALL flush(w_unit)
  ENDIF
  !
  CALL hdlerr( NF90_GET_VAR(il_file_id, il_clo_id, gridclo, &
     ila_what, ila_corners), __LINE__,__FILE__ )
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'Global grid longitude corners reading done'
    CALL flush(w_unit)
  ENDIF
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_cla_id, gridcla, &
     ila_what, ila_corners), __LINE__,__FILE__ )
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'Global grid latitude corners reading done'
    CALL flush(w_unit)
  ENDIF
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_srf_id, gridsrf, &
     ila_what(1:2), ila_dim(1:2)), __LINE__,__FILE__ )
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'Global grid surfaces reading done'
    CALL flush(w_unit)
  ENDIF
  !
  CALL hdlerr( NF90_GET_VAR (il_file_id, il_indice_id, indice_mask, &
     ila_what, ila_dim), __LINE__,__FILE__ )
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'Global grid mask reading done'
    CALL flush(w_unit)
  ENDIF
  !
  CALL hdlerr( NF90_CLOSE(il_file_id), __LINE__,__FILE__ )
  !
  IF(krank.EQ.0) THEN
    WRITE(w_unit,*) 'End of routine read_grid'
    CALL flush(w_unit)
  ENDIF
  !
END SUBROUTINE read_grid
