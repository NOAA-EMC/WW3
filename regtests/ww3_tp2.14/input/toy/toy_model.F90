!!##############################################################################
PROGRAM TOY_MODEL
!!##############################################################################
!!
!!---------------------------------------------------------------------
!!
!! ** PURPOSE : Toy model for coupling
!!
!! ** AUTHOR :
!!        ORIGINAL: tuto, CERFACS
!!
!!------------------------------------------------------------------------
!! Copyright 2010, CERFACS, Toulouse, France.
!! All rights reserved. Use is subject to OASIS3 license terms.
!=============================================================================
!!
!!        2014-03 : J. PIANEZZE (LPO) : Modification
!!        2015-02                       Update of routines
!!        2015-04                       Add namelist file
!!
!!---------------------------------------------------------------------
!!
!
USE NETCDF
USE MOD_OASIS ! Use for OASIS communication library
!
IMPLICIT NONE
!
INCLUDE 'mpif.h'
!
#ifdef NO_USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: WP = SELECTED_REAL_KIND(6,37)   ! real
#elif USE_DOUBLE_PRECISION
  INTEGER, PARAMETER :: WP = SELECTED_REAL_KIND(12,307) ! double
#endif
!
CHARACTER(LEN=6) :: CMODEL_NAME = 'toyexe' ! Component name (6 characters) same as in the namcouple
INTEGER :: ICOMP_ID
INTEGER :: KLOCALCOMM, KSIZE, KRANK
INTEGER :: IERR
INTEGER :: IOUTDIAG_UNIT=51
!
! Global grid parameters for grid definition
! ----------------------------------------------------- 
INTEGER :: NLON, NLAT     ! dimensions in the 2 directions of space
INTEGER :: NTOT           ! total dimension
INTEGER :: NC ! number of corners in the (i,j) plan
INTEGER :: INDI_BEG, INDI_END, INDJ_BEG, INDJ_END
INTEGER :: IL_FLAG          ! Flag for grid writing
!
REAL, DIMENSION(:,:), POINTER :: GLOBALGRID_LON,GLOBALGRID_LAT
REAL, DIMENSION(:,:,:), POINTER :: GLOBALGRID_CLO,GLOBALGRID_CLA
REAL, DIMENSION(:,:), POINTER :: GLOBALGRID_SRF
INTEGER, DIMENSION(:,:), POINTER :: INDICE_MASK ! mask, 0 == valid point, 1 == masked point 
!
CHARACTER(LEN=30) :: DATA_FILENAME
!
CHARACTER(LEN=5) :: CTYPE_FCT
REAL :: VALUE
CHARACTER(LEN=30) :: CNAME_FILE
!
! Global definition parition parameters
! -------------------------------------
INTEGER :: PART_ID, IL_SIZE
INTEGER, DIMENSION(:), ALLOCATABLE :: IL_PARAL ! Decomposition for each proc
!
! Global parameters for OASIS_DEF_VAR
! -----------------------------------
!
INTEGER :: NB_RECV_FIELDS
INTEGER, PARAMETER :: NB_RECV_FIELDS_MAX=50
CHARACTER(LEN=8), DIMENSION(NB_RECV_FIELDS_MAX) :: CRCVFIELDS='        '
!
INTEGER :: NB_SEND_FIELDS
INTEGER, PARAMETER :: NB_SEND_FIELDS_MAX=50
CHARACTER(LEN=8), DIMENSION(NB_SEND_FIELDS_MAX) :: CSNDFIELDS='        '
!
! Used in oasis_def_var
INTEGER, DIMENSION(NB_RECV_FIELDS_MAX+NB_SEND_FIELDS_MAX) :: VAR_ID 
INTEGER :: VAR_NODIMS(2) 
INTEGER :: VAR_TYPE
INTEGER :: VAR_ACTUAL_SHAPE(4) ! local dimensions of the arrays to the pe
                               ! 2 x field rank (= 4 because fields are of rank = 2)
!
REAL (KIND=WP), PARAMETER :: FIELD_INI = -1. ! initialisation of received fields
!
INTEGER :: IB, IND
INTEGER :: IL_NB_TIME_STEPS ! number of time steps
INTEGER :: DELTA_T          ! time step
INTEGER :: ITAP_SEC         ! Time
!
! Exchanged local fields arrays
! used in routines oasis_put and oasis_get
REAL(KIND=WP), POINTER :: FIELD_RECV(:,:)
REAL(KIND=WP), POINTER :: FIELD_SEND(:,:,:)
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Open a flog file
OPEN(UNIT=IOUTDIAG_UNIT,FILE='OUTPUT_TOY.txt')
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
WRITE(IOUTDIAG_UNIT,*) '   INITIALISATION'
WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
!
WRITE(IOUTDIAG_UNIT,*) '----- CALL OASIS_INIT_COMP'
CALL OASIS_INIT_COMP(ICOMP_ID,CMODEL_NAME,IERR)
IF(IERR /= 0) THEN
  CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'ERROR')
ENDIF
!
WRITE(IOUTDIAG_UNIT,*) '----- CALL OASIS_GET_LOCALCOMM'
CALL OASIS_GET_LOCALCOMM(KLOCALCOMM,IERR)
IF(IERR /= 0) THEN
  CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'ERROR')
ENDIF
!
! Get MPI size and rank
! ---------------------
CALL MPI_COMM_SIZE (KLOCALCOMM, KSIZE, IERR )
WRITE(IOUTDIAG_UNIT,*) 'KSIZE=', KSIZE
IF (IERR /= 0) THEN
  WRITE(IOUTDIAG_UNIT,*) 'MPI_COMM_SIZE ABORT BY TOY MODEL COMPID ',ICOMP_ID
  CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'PROBLEM DURING MPI_COMM_SIZE')
ENDIF
!
CALL MPI_COMM_RANK (KLOCALCOMM, KRANK, IERR )
IF (IERR /= 0) THEN
  WRITE (IOUTDIAG_UNIT,*) 'MPI_COMM_RANK ABORT BY TOY MODEL COMPID ',ICOMP_ID
  CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'PROBLEM DURING MPI_COMM_RANK')
ENDIF
!
IF(KRANK .EQ. 0) THEN
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  WRITE(IOUTDIAG_UNIT,*) '    READING THE NAMELIST'
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
ENDIF
!
CALL READ_NAMELIST(IOUTDIAG_UNIT,KRANK,IL_NB_TIME_STEPS,DELTA_T, &
                   DATA_FILENAME, &
                   CTYPE_FCT, VALUE, CNAME_FILE, &
                   NB_RECV_FIELDS, CRCVFIELDS, &
                   NB_SEND_FIELDS, CSNDFIELDS)
!
IF(KRANK .EQ. 0) THEN
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  WRITE(IOUTDIAG_UNIT,*) '    GRID DEFINITION'
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
ENDIF
!
! Reading netcdf file with pre-defined variable names
! ---------------------------------------------------
! Reading dimensions of the grid
CALL READ_DIMGRID(NLON,NLAT,DATA_FILENAME,IOUTDIAG_UNIT,KRANK)
NC=4
!
! Allocation
ALLOCATE(GLOBALGRID_LON(NLON,NLAT), STAT=IERR )
IF ( IERR /= 0 .AND. KRANK .EQ. 0 ) WRITE(IOUTDIAG_UNIT,*) 'Error allocating GLOBALGRID_LON'
ALLOCATE(GLOBALGRID_LAT(NLON,NLAT), STAT=IERR )
IF ( IERR /= 0 .AND. KRANK .EQ. 0 ) WRITE(IOUTDIAG_UNIT,*) 'Error allocating GLOBALGRID_LAT'
ALLOCATE(GLOBALGRID_CLO(NLON,NLAT,nc), STAT=IERR )
IF ( IERR /= 0 .AND. KRANK .EQ. 0 ) WRITE(IOUTDIAG_UNIT,*) 'Error allocating GLOBALGRID_CLO'
ALLOCATE(GLOBALGRID_CLA(NLON,NLAT,nc), STAT=IERR )
IF ( IERR /= 0 .AND. KRANK .EQ. 0 ) WRITE(IOUTDIAG_UNIT,*) 'Error allocating GLOBALGRID_CLA'
ALLOCATE(GLOBALGRID_SRF(NLON,NLAT), STAT=IERR )
IF ( IERR /= 0 .AND. KRANK .EQ. 0 ) WRITE(IOUTDIAG_UNIT,*) 'Error allocating GLOBALGRID_SRF'
ALLOCATE(INDICE_MASK(NLON,NLAT), STAT=IERR )
IF ( IERR /= 0 .AND. KRANK .EQ. 0 ) WRITE(IOUTDIAG_UNIT,*) 'Error allocating INDICE_MASK'
!
! Reading of the longitudes, latitudes, longitude and latitudes of the corners, mask of the grid
CALL READ_GRID(NLON,NLAT,NC,DATA_FILENAME,IOUTDIAG_UNIT, KRANK, &
               GLOBALGRID_LON,GLOBALGRID_LAT, &
               GLOBALGRID_CLO,GLOBALGRID_CLA, &
               GLOBALGRID_SRF, &
               INDICE_MASK)
!
! (Global) grid definition for OASIS3
! Writing of the file grids.nc and masks.nc by the processor 0 from the grid read in 
!
IF (KRANK == 0) THEN
!
! Mask inversion to follow (historical) OASIS3 convention (0=not masked;1=masked)
  WHERE(INDICE_MASK == 1) 
    INDICE_MASK=0
  ELSEWHERE
    INDICE_MASK=1
  END WHERE
  !
  CALL OASIS_START_GRIDS_WRITING(IL_FLAG)
  CALL OASIS_WRITE_GRID('toyt', NLON, NLAT, GLOBALGRID_LON, GLOBALGRID_LAT)
  CALL OASIS_WRITE_CORNER('toyt', NLON, NLAT, 4, GLOBALGRID_CLO, GLOBALGRID_CLA)
  CALL OASIS_WRITE_AREA('toyt', NLON, NLAT, GLOBALGRID_SRF)
  CALL OASIS_WRITE_MASK('toyt', NLON, NLAT, INDICE_MASK(:,:))
  CALL OASIS_TERMINATE_GRIDS_WRITING()
ENDIF
!
IF(KRANK.EQ.0) THEN
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  WRITE(IOUTDIAG_UNIT,*) '    PARTITION DEFINITION'
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
ENDIF
!
! Definition of the partition of the grid (calling oasis_def_partition)
NTOT=NLON*NLAT
!
#ifdef DECOMP_APPLE
  IL_SIZE = 3
#elif defined DECOMP_BOX
  IL_SIZE = 5
#endif
!
ALLOCATE(IL_PARAL(IL_SIZE))
IF(KRANK.EQ.0) WRITE(IOUTDIAG_UNIT,*) 'After allocate il_paral, il_size', IL_SIZE
!
CALL DECOMP_DEF(IL_PARAL,IL_SIZE,NLON,NLAT,KRANK,KSIZE,IOUTDIAG_UNIT)
IF(KRANK.EQ.0) WRITE(IOUTDIAG_UNIT,*) 'After decomp_def, il_paral = ', IL_PARAL(:)
!
CALL OASIS_DEF_PARTITION(PART_ID, IL_PARAL, IERR)
!
IF(KRANK.EQ.0) THEN
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  WRITE(IOUTDIAG_UNIT,*) '    DEFINITION OF THE LOCAL FIELDS'
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
ENDIF
!
! Define transient variables
!
VAR_NODIMS(1) = 2    ! Rank of the field array is 2
VAR_NODIMS(2) = 1    ! Bundles always 1 for OASIS3
VAR_TYPE = OASIS_Real
!
VAR_ACTUAL_SHAPE(1) = 1
VAR_ACTUAL_SHAPE(2) = IL_PARAL(3)
VAR_ACTUAL_SHAPE(3) = 1 
!
#ifdef DECOMP_APPLE
  VAR_ACTUAL_SHAPE(4) = 1
#elif defined DECOMP_BOX
  VAR_ACTUAL_SHAPE(4) = IL_PARAL(4)
#endif
!
! Declaration of the field associated with the partition of the grid
DO IND=1, NB_RECV_FIELDS 
  CALL OASIS_DEF_VAR(VAR_ID(IND),CRCVFIELDS(IND), PART_ID, &
                   VAR_NODIMS, OASIS_IN, VAR_ACTUAL_SHAPE, VAR_TYPE, IERR)
  IF (IERR /= 0) CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'ERROR DURING DEFINITION OF RECV VAR')
ENDDO
!
DO IND=1, NB_SEND_FIELDS
  CALL OASIS_DEF_VAR(VAR_ID(IND+NB_RECV_FIELDS),CSNDFIELDS(IND), PART_ID, &
                   VAR_NODIMS, OASIS_OUT, VAR_ACTUAL_SHAPE, VAR_TYPE, IERR)
  IF (IERR /= 0) CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'ERROR DURING DEFINITION OF SEND VAR')
ENDDO 
!
IF(KRANK.EQ.0) THEN
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  WRITE(IOUTDIAG_UNIT,*) '    TERMINATION OF DEFINITION PHASE'
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
ENDIF
!
CALL OASIS_ENDDEF(IERR)
IF(IERR /= 0) CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'ERROR')
!
IF(KRANK.EQ.0) THEN
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  WRITE(IOUTDIAG_UNIT,*) '    SEND AND RECEIVE ARRAYS'
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
ENDIF
!
ALLOCATE(FIELD_RECV(VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4)), STAT=IERR)
IF(KRANK.EQ.0 .AND. IERR /= 0 ) WRITE(IOUTDIAG_UNIT,*) 'ERROR ALLOCATING FIELD_RECV'
!
ALLOCATE(FIELD_SEND(VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4),NB_SEND_FIELDS),STAT=IERR)
IF(KRANK.EQ.0 .AND. IERR /= 0 ) WRITE(IOUTDIAG_UNIT,*) 'ERROR ALLOCATING FIELD_SEND'
!
DEALLOCATE(IL_PARAL)
!
!
INDI_BEG=1
INDI_END=NLON
INDJ_BEG=((NLAT/KSIZE)*KRANK)+1 
!
IF (KRANK .LT. KSIZE - 1) THEN
  INDJ_END = (NLAT/KSIZE)*(KRANK+1)
ELSE
  INDJ_END = NLAT 
ENDIF
!
DO IB=1, IL_NB_TIME_STEPS
  !
  ITAP_SEC = DELTA_T * (IB-1) ! Time
  !
  IF(KRANK.EQ.0) THEN
    WRITE(IOUTDIAG_UNIT,*) 'CURRENT TIME : ', ITAP_SEC
  ENDIF
  !
  ! Get the field from coupled model (atmosphere/wave/ocean)
  ! -------------------------------------------------------
  DO IND=1, NB_RECV_FIELDS
    FIELD_RECV=FIELD_INI
    CALL OASIS_GET(VAR_ID(IND),ITAP_SEC, FIELD_RECV, IERR)
    IF(KRANK.EQ.0) THEN
      WRITE(IOUTDIAG_UNIT,*) 'RECEIVE FIELD : ', CRCVFIELDS(IND) , ' => ', ITAP_SEC, MINVAL(FIELD_RECV), MAXVAL(FIELD_RECV)
    ENDIF
    IF ( IERR .NE. OASIS_Ok .AND. IERR .LT. OASIS_Recvd) THEN
      IF(KRANK.EQ.0) THEN
        WRITE (IOUTDIAG_UNIT,*) 'OASIS_GET ABORT BY TOY MODEL COMPID ',ICOMP_ID
      ENDIF
      CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'PROBLEM DURING OASIS_GET')
    ENDIF
  ENDDO
  !
  ! Send the field to coupled model (atmosphere/wave/ocean)
  ! -------------------------------------------------------
  !
  CALL FUNCTION_SENT(IOUTDIAG_UNIT,KRANK,INDI_BEG,INDI_END,INDJ_BEG,INDJ_END, &
                     VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4), NB_SEND_FIELDS, &
                     RESHAPE(GLOBALGRID_LON(INDI_BEG:INDI_END,INDJ_BEG:INDJ_END),&
                     (/ VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4) /)), &
                     RESHAPE(GLOBALGRID_LAT(INDI_BEG:INDI_END,INDJ_BEG:INDJ_END),&
                     (/ VAR_ACTUAL_SHAPE(2), VAR_ACTUAL_SHAPE(4) /)), &
                     FIELD_SEND,IB, &
                     CTYPE_FCT, VALUE, CNAME_FILE, CSNDFIELDS)
  !		     
  DO IND=1, NB_SEND_FIELDS 
    IF(KRANK.EQ.0) THEN
      WRITE(IOUTDIAG_UNIT,*) 'SEND FIELD : ', CSNDFIELDS(IND), ' => ', ITAP_SEC, MINVAL(FIELD_SEND), MAXVAL(FIELD_SEND)
    ENDIF
    CALL OASIS_PUT(VAR_ID(IND+NB_RECV_FIELDS),ITAP_SEC, FIELD_SEND(:,:,IND), IERR)
    IF ( IERR .NE. OASIS_Ok .AND. IERR .LT. OASIS_Sent) THEN
      IF(KRANK.EQ.0) THEN
        WRITE (IOUTDIAG_UNIT,*) 'OASIS_PUT ABORT BY TOY MODEL COMPID ',ICOMP_ID
      ENDIF
      CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'PROBLEM DURING OASIS_PUT')
    ENDIF
  ENDDO
  !
ENDDO
!
IF(KRANK.EQ.0) THEN
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  WRITE(IOUTDIAG_UNIT,*) '   TERMINATION'
  WRITE(IOUTDIAG_UNIT,*) '==========================================================================='
  !
  WRITE(IOUTDIAG_UNIT,*) '----- CALL OASIS_TERMINATE'
ENDIF
CALL OASIS_TERMINATE(IERR)
IF(IERR /= 0) THEN
  CALL OASIS_ABORT(ICOMP_ID,CMODEL_NAME,'ERROR DURING OASIS_TERMINATE')
ENDIF
!
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Close the log file
CLOSE(UNIT=IOUTDIAG_UNIT)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!############################################################################
END PROGRAM TOY_MODEL
!############################################################################
