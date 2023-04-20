#include "w3macros.h"
!/ ------------------------------------------------------------------- /
MODULE W3OACPMD
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |           A. Thevenin             |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :         22-Mar-2021 |
  !/                  +-----------------------------------+
  !/
  !/      July-2013 : Origination.                         ( version 4.18 )
  !/                    For upgrades see subroutines.
  !/     April-2016 : Add comments (J. Pianezze)           ( version 5.07 )
  !/    25-Sep-2020 : Coupling at T+0 support              ( version 7.10 )
  !/    22-Mar-2021 : Adds extra coupling fields           ( version 7.13 )
  !/
  !/    Copyright 2009-2012 National Weather Service (NWS),
  !/       National Oceanic and Atmospheric Administration.  All rights
  !/       reserved.  WAVEWATCH III is a trademark of the NWS.
  !/       No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Generic Module used for coupling applications with OASIS3-MCT
  !
  !  2. Variables and types :
  !
  !  3. Subroutines and functions :
  !
  !      Name                Type  Scope    Description
  !     ----------------------------------------------------------------
  !      CPL_OASIS_INIT      Subr. Public   Initialize the coupling
  !      CPL_OASIS_GRID      Subr. Public   Grids defintion
  !      CPL_OASIS_DEFINE    Subr. Public   Partition definition
  !      CPL_OASIS_SND       Subr. Public   Send fields to ocean/atmos model
  !      CPL_OASIS_RCV       Subr. Public   Receive fields from ocean/atmos model
  !      CPL_OASIS_FINALIZE  Subr. Public   Finalize the coupling
  !     ----------------------------------------------------------------
  !
  !  4. Subroutines and functions used :
  !
  !      Name                 Type    Module    Description
  !     --------------------------------------------------------------------
  !      GET_LIST_EXCH_FIELD  Subr.   W3OACPMD  List of the exchanged fields
  !      STRSPLIT             Subr.   W3SERVMD  Splits string into words
  !     --------------------------------------------------------------------
  !
  !  5. Remarks
  !
  !     Module adapted from WRF-OASIS routine implemented by
  !     Sebastien Masson (IPSL), Guillaume Samson (Legos) and Eric Maisonnave (Cerfacs)
  !
  !  6. Switches :
  !  7. Source code :
  !
  !/ ------------------------------------------------------------------- /
  !
  USE MOD_OASIS                                      ! OASIS3-MCT module
  !
  IMPLICIT NONE
  PRIVATE
  !
  INTEGER               :: IL_COMPID                 ! Component model ID returned by oasis_init_comp
  CHARACTER(LEN=6)      :: CL_MODEL_NAME = 'wwatch'  ! Model name (same as in namcouple)
  INTEGER               :: IL_ERR                    ! Return error code
  INTEGER, PUBLIC       :: IL_NB_RCV, IL_NB_SND      ! Number of coupling fields
  INTEGER, PARAMETER    :: IP_MAXFLD=50              ! Maximum number of coupling fields
  INTEGER               :: NNODES                    ! Total numbers of cell in the grid
  !
  TYPE, PUBLIC          :: CPL_FIELD                 ! Type for coupling field information
    CHARACTER(LEN = 8) :: CL_FIELD_NAME             ! Name of the coupling field
    INTEGER            :: IL_FIELD_ID               ! Field ID
  END TYPE CPL_FIELD
  !
  TYPE(CPL_FIELD), DIMENSION(IP_MAXFLD), PUBLIC :: RCV_FLD, SND_FLD   ! Coupling fields
  !
  INTEGER, PUBLIC       :: ID_OASIS_TIME=0           ! time counter for coupling exchanges
  !
  LOGICAL, PUBLIC       :: CPLT0                     ! Flag for coupling at T+0
  !
  ! * Accessibility
  PUBLIC CPL_OASIS_INIT
  PUBLIC CPL_OASIS_GRID
  PUBLIC CPL_OASIS_DEFINE
  PUBLIC CPL_OASIS_SND
  PUBLIC CPL_OASIS_RCV
  PUBLIC CPL_OASIS_FINALIZE
  !
CONTAINS
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CPL_OASIS_INIT(ID_LCOMM)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Thevenin             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          April-2016 |
    !/                  +-----------------------------------+
    !/
    !/     Jul-2013 : Origination.                    ( version 4.18 )
    !/     April-2016 : Add comments (J. Pianezze)    ( version 5.07 )
    !/
    !  1. Purpose :
    !
    !     Initialize the coupling
    !
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ID_LCOMM     Int.     O    MPI communicator
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !      Name                Type     Module    Description
    !     ----------------------------------------------------------------
    !      WW3_SHEL            Prog.    -         Main program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks :
    !  8. Structure :
    !  9. Switches :
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    ! * Argument
    INTEGER, INTENT(OUT) :: ID_LCOMM                   ! Model local communicator
    !
    !----------------------------------------------------------------------
    ! * Executable part
    !
    !! Initialize the coupling
    CALL OASIS_INIT_COMP(IL_COMPID, CL_MODEL_NAME, IL_ERR)
    IF (IL_ERR /= 0) THEN
      CALL OASIS_ABORT(IL_COMPID, 'CPL_OASIS_INIT', 'Problem during oasis_init_comp')
    ENDIF
    !
    !! Get the value of a local MPI communicator to be used by WW3 for its internal parallelisation
    CALL OASIS_GET_LOCALCOMM(ID_LCOMM, IL_ERR)
    IF (IL_ERR /= 0) THEN
      CALL OASIS_ABORT(IL_COMPID, 'CPL_OASIS_INIT', 'Problem during oasis_get_localcomm')
    ENDIF
    !
    !/ ------------------------------------------------------------------- /
  END SUBROUTINE CPL_OASIS_INIT
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CPL_OASIS_GRID(LD_MASTER,ID_LCOMM)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Thevenin             |
    !/                  |           V. Garnier              |
    !/                  |           M. Accensi              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          April-2016 |
    !/                  +-----------------------------------+
    !/
    !/    Jul-2013 : Origination.                    ( version 4.18 )
    !/    April-2016 : Add comments (J. Pianezze)    ( version 5.07 )
    !/    Sept-2016 : Correct bug MPI (J. Pianezze)  ( version 5.12 )
    !/
    !  1. Purpose :
    !
    !     Grid data file definition
    !
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     LD_MASTER         Bool.    I     Flag to know the master process
    !     ID_LCOMM          Int.     I     MPI communicator
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !  5. Called by :
    !
    !      Name                Type     Module    Description
    !     ----------------------------------------------------------------
    !      WW3_SHEL            Prog.    -         Main program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks :
    !  8. Structure :
    !  9. Switches :
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    USE CONSTANTS, ONLY: RADIUS, DERA
    USE W3GDATMD,  ONLY: NX, NY, FLAGLL, XGRD, YGRD, MAPSTA, &
         & HPFAC, HQFAC, GTYPE, &
         & UNGTYPE, RLGTYPE, CLGTYPE, SMCTYPE
#ifdef W3_SMC
    USE W3GDATMD,  ONLY: NSEA, X0, Y0, MRFct, SX, SY, IJKCel
#endif
#ifdef W3_MPI
    INCLUDE "mpif.h"
#endif
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    LOGICAL, INTENT(IN) :: LD_MASTER    ! MASTER process or not
    INTEGER, INTENT(IN) :: ID_LCOMM     ! Model local communicator
    !
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER, ALLOCATABLE :: MASK(:,:)
    INTEGER              :: I, IX, IY, NXW, NXE, NYS, NYN, INODE, IERR_MPI
    REAL, ALLOCATABLE    :: LON(:,:),LAT(:,:),AREA(:,:),      &
         CORLON(:,:,:),CORLAT(:,:,:)
    REAL                 :: FACTOR
#ifdef W3_SMC
    REAL                 :: DLON, DLAT
#endif
    !/ ------------------------------------------------------------------- /
    !
    IF (LD_MASTER) THEN
      !
      !
      ! 0. Create grids file
      ! --------------------------------
      CALL OASIS_START_GRIDS_WRITING(IERR_MPI)
      !
      ! 1. Get the lat/lon/corners,areas and masks
      ! -------------------------------------------
      IF (GTYPE .EQ. RLGTYPE .OR. GTYPE .EQ. CLGTYPE) THEN
        !
        IF (FLAGLL) THEN
          FACTOR = 1.
        ELSE
          FACTOR = 1. / (RADIUS * DERA)
        END IF
        !
        ! 1.1. regular and curvilinear grids
        ! ----------------------------------
        NNODES = NX*NY
        NXW=1
        NXE=NX
        NYS=1
        NYN=NY
        !
        ! lat/lon
        ALLOCATE ( LON(NNODES,1), LAT(NNODES,1) )
        I = 0
        DO IY = NYS, NYN
          DO IX = NXW, NXE
            I = I+1
            LON(I,1)=XGRD(IY,IX)*FACTOR
            LAT(I,1)=YGRD(IY,IX)*FACTOR
          END DO
        END DO
        !
        ! areas, corners
        ALLOCATE ( AREA(NNODES,1), CORLON(NNODES,1,4), CORLAT(NNODES,1,4) )
        I = 0
        DO IY = NYS, NYN
          DO IX = NXW, NXE
            I = I+1
            CORLON(I,1,1)=LON(I,1)+HPFAC(IY,IX)/2.*FACTOR
            CORLON(I,1,2)=LON(I,1)-HPFAC(IY,IX)/2.*FACTOR
            CORLON(I,1,3)=LON(I,1)-HPFAC(IY,IX)/2.*FACTOR
            CORLON(I,1,4)=LON(I,1)+HPFAC(IY,IX)/2.*FACTOR
            CORLAT(I,1,1)=LAT(I,1)+HQFAC(IY,IX)/2.*FACTOR
            CORLAT(I,1,2)=LAT(I,1)+HQFAC(IY,IX)/2.*FACTOR
            CORLAT(I,1,3)=LAT(I,1)-HQFAC(IY,IX)/2.*FACTOR
            CORLAT(I,1,4)=LAT(I,1)-HQFAC(IY,IX)/2.*FACTOR
            AREA(I,1)=HPFAC(IY,IX)*HQFAC(IY,IX)
          END DO
        END DO
        !
        ! Model grid mask
        ALLOCATE ( MASK(NNODES,1) )
        I = 0
        DO IY = NYS, NYN
          DO IX = NXW, NXE
            I = I+1
            ! Get the mask : 0 - sea  / 1 - open boundary cells (the land is already excluded)
            IF ((MAPSTA(IY,IX) .EQ. 1)) THEN
              MASK(I,1) = 0
            ELSE
              MASK(I,1) = 1
            END IF
          END DO
        END DO
#ifdef W3_SMC
      ELSE IF( GTYPE .EQ. SMCTYPE ) THEN
        !
        ! 1.2. SMC grids
        ! ----------------------------------
        NNODES = NSEA
        !
        ! Calculate the smallest grid cell increments depending on the number of SMC levels
        DLON = SX / MRFct
        DLAT = SY / MRFct
        !
        ALLOCATE ( LON(NNODES,1), LAT(NNODES,1) )
        ALLOCATE ( AREA(NNODES,1), CORLON(NNODES,1,4), CORLAT(NNODES,1,4) )
        ALLOCATE ( MASK(NNODES,1) )
        DO I=1, NNODES
          ! lat/lon
          LON(I,1) = X0 + (IJKCel(1,I) + IJKCel(3,I)*0.5)*DLON
          LAT(I,1) = Y0 + (IJKCel(2,I) + IJKCel(4,I)*0.5)*DLAT
          ! corners
          CORLON(I,1,1) = X0 + IJKCel(1,I)*DLON
          CORLON(I,1,2) = X0 + (IJKCel(1,I) + IJKCel(3,I))*DLON
          CORLON(I,1,3) = CORLON(I,1,2)
          CORLON(I,1,4) = CORLON(I,1,1)
          CORLAT(I,1,1) = Y0 + IJKCel(2,I)*DLAT
          CORLAT(I,1,2)=CORLAT(I,1,1)
          CORLAT(I,1,3) = Y0 + (IJKCel(2,I) + IJKCel(4,I))*DLAT
          CORLAT(I,1,4)=CORLAT(I,1,3)
          ! areas
          AREA(I,1) = 0.25 * IJKCEL(3,I)*DLON * IJKCEL(4,I)*DLAT
          ! Model grid mask
          MASK(I,1) = 1
        ENDDO
#endif
        !
      ELSE
        !
        ! 1.3. Unstructured grids
        ! ----------------------------------
        WRITE(*,*) 'TO BE IMPLEMENT FOR UNSTRUCTURED GRIDS'
        STOP
      END IF
      !
      CALL OASIS_WRITE_GRID('ww3t',NNODES,1,LON,LAT)
      CALL OASIS_WRITE_CORNER('ww3t',NNODES,1,4,CORLON,CORLAT)
      CALL OASIS_WRITE_AREA('ww3t',NNODES,1,AREA)
      CALL OASIS_WRITE_MASK('ww3t',NNODES,1,MASK)
      !
      ! 2. Terminate grid writing
      ! -------------------------
      CALL OASIS_TERMINATE_GRIDS_WRITING()
      !
      DEALLOCATE(LON)
      DEALLOCATE(LAT)
      DEALLOCATE(CORLON)
      DEALLOCATE(CORLAT)
      DEALLOCATE(AREA)
      DEALLOCATE(MASK)
      !
    ENDIF
    !
#ifdef W3_MPI
    CALL MPI_BCAST(NNODES,1,MPI_INTEGER,0,ID_LCOMM,IERR_MPI)
#endif
    !
    !/ ------------------------------------------------------------------- /
  END SUBROUTINE CPL_OASIS_GRID
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CPL_OASIS_DEFINE(NDSO,RCV_STR,SND_STR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Thevenin             |
    !/                  |           V. Garnier              |
    !/                  |           M. Accensi              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         08-Jun-2018 |
    !/                  +-----------------------------------+
    !/
    !/      Jul-2013 : Origination.                         ( version 4.18 )
    !/    April-2016 : Add coupling for unstructured grids  ( version 5.07 )
    !/                   (R. Baraille & J. Pianezze)
    !/    April-2016 : Add comments (J. Pianezze)           ( version 5.07 )
    !/    08-Jun-2018 : use INIT_GET_ISEA                   ( version 6.04 )
    !/
    !  1. Purpose :
    !
    !     Partition definition and coupling fields declaration
    !
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     NDSO             Int.      I      Id. of the output file
    !     RCV_STR          Char.     I      Name of receive fields
    !     SND_STR          Char.     I      Name of send fields
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name                 Type    Module    Description
    !     ----------------------------------------------------------------
    !      GET_LIST_EXCH_FIELD  Subr.   W3OACPMD  List of the exchanged fields
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name                Type     Module    Description
    !     ----------------------------------------------------------------
    !      WW3_SHEL            Prog.    -         Main program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks :
    !  8. Structure :
    !  9. Switches :
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    USE W3GDATMD, ONLY: NSEAL,NSEA, NX, NY, MAPSTA, MAPSF, GTYPE, &
         & UNGTYPE, RLGTYPE, CLGTYPE, SMCTYPE
    USE W3ODATMD, ONLY: NAPROC, IAPROC
    USE W3PARALL, ONLY : INIT_GET_ISEA
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)                          :: NDSO
    CHARACTER(LEN=1024), INTENT(IN)              :: RCV_STR,SND_STR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: IB_I,I
    INTEGER                 :: IL_PART_ID      ! PartitionID
    INTEGER, ALLOCATABLE, DIMENSION(:)   :: ILA_PARAL       ! Description of the local partition in the global index space
    INTEGER, DIMENSION(4)   :: ILA_SHAPE       ! Vector giving the min & max index for each dim of the fields
    INTEGER, DIMENSION(2)   :: ILA_VAR_NODIMS  ! rank of fields & number of bundles (1 with OASIS3-MCT)
    INTEGER                 :: ISEA, JSEA, IX, IY
    INTEGER                 :: NHXW, NHXE, NHYS, NHYN  ! size of the halo at the western, eastern, southern, northern boundaries
    LOGICAL                 :: LL_MPI_FILE     ! to check if there an mpi.txt file for domain decompasition
    !/
    !/ ------------------------------------------------------------------- /
    !/ Executable part
    !/
    !
    IF (GTYPE .EQ. RLGTYPE .OR. GTYPE .EQ. CLGTYPE) THEN
      !
      ! 1.1. regular and curvilinear grids
      ! ----------------------------------
      NHXW = 1 ; NHXE = NX ; NHYS = 1 ; NHYN = NY
      NHXW = NHXW - 1
      NHXE = NX - NHXE
      NHYS = NHYS - 1
      NHYN = NY - NHYN
      !
      ALLOCATE(ILA_PARAL(2+NSEAL*2))
      !
      ! * Define the partition : OASIS ORANGE partition
      ILA_PARAL(1) = 3
      !
      ! * total number of segments of the global domain
      ILA_PARAL(2) = NSEAL
      !
      DO JSEA=1, NSEAL
        CALL INIT_GET_ISEA(ISEA,JSEA)

        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
        ILA_PARAL(JSEA*2+1) = (IY - NHYN -1)*(NX - NHXE - NHXW) + (IX - NHXW - 1)
        ILA_PARAL(JSEA*2+2) = 1
      END DO
#ifdef W3_SMC
    ELSE IF( GTYPE .EQ. SMCTYPE ) THEN
      !
      ! 1.2. SMC grids
      ! ----------------------------------
      ALLOCATE(ILA_PARAL(2+NSEAL))
      !
      ! * Define the partition : OASIS POINTS partition
      ILA_PARAL(1) = 4
      !
      ! * total number of segments of the global domain
      ILA_PARAL(2) = NSEAL
      !
      DO JSEA=1, NSEAL
        ILA_PARAL(JSEA+2) = IAPROC + (JSEA-1)*NAPROC
      ENDDO
#endif
      !
    ELSE
      !
      ! 1.3. Unstructured grids
      ! ----------------------------------
      WRITE(*,*) 'TO BE VERIFIED FOR UNSTRUCTURED GRIDS'
      STOP
      !
      DO JSEA=1,NSEAL
        ILA_PARAL(JSEA*2+1) = (IAPROC-1) + (JSEA-1)*NAPROC
        ILA_PARAL(JSEA*2+2) = 1
      END DO
      !
    ENDIF
    !
    ! 2. Partition definition
    ! ----------------------------------
    CALL OASIS_DEF_PARTITION(IL_PART_ID, ILA_PARAL,IL_ERR,NNODES)
    IF(IL_ERR /= 0) THEN
      CALL OASIS_ABORT(IL_COMPID, 'CPL_OASIS_DEFINE', 'Problem during oasis_def_partition')
    ENDIF
    !
    ! 3. Coupling fields declaration
    ! ----------------------------------
    ILA_SHAPE(:) = (/1, NSEAL, 1, 1 /)
    !
    ILA_VAR_NODIMS(1) = 2    ! rank of fields array
    ILA_VAR_NODIMS(2) = 1    ! always 1 with OASIS3-MCT 2.0
    !
    CALL GET_LIST_EXCH_FIELD(NDSO, RCV_FLD, SND_FLD, IL_NB_RCV, IL_NB_SND, RCV_STR, SND_STR)
    !
    ! 3.1 Send coupling fields
    ! ----------------------------------
    DO IB_I = 1, IL_NB_SND
      CALL OASIS_DEF_VAR (SND_FLD(IB_I)%IL_FIELD_ID     &
           &            , SND_FLD(IB_I)%CL_FIELD_NAME   &
           &            , IL_PART_ID                    &
           &            , ILA_VAR_NODIMS                &
           &            , OASIS_OUT                     &
           &            , ILA_SHAPE                     &
           &            , OASIS_REAL                    &
           &            , IL_ERR )

      IF (IL_ERR /= 0) THEN
        CALL OASIS_ABORT(IL_COMPID, 'CPL_OASIS_DEFINE', 'Problem during oasis_def_var')
      ENDIF
    ENDDO
    !
    ! 3.2 Received coupling fields
    ! ----------------------------------
    DO IB_I = 1, IL_NB_RCV
      CALL OASIS_DEF_VAR (RCV_FLD(IB_I)%IL_FIELD_ID    &
           &            , RCV_FLD(IB_I)%CL_FIELD_NAME  &
           &            , IL_PART_ID                   &
           &            , ILA_VAR_NODIMS               &
           &            , OASIS_IN                     &
           &            , ILA_SHAPE                    &
           &            , OASIS_REAL                   &
           &            , IL_ERR )
      !
      IF (IL_ERR /= 0) THEN
        CALL OASIS_ABORT(IL_COMPID, 'CPL_OASIS_DEFINE', 'Problem during oasis_def_var')
      ENDIF
    ENDDO
    !
    ! 4. End of definition phase
    ! ----------------------------------
    CALL OASIS_ENDDEF(IL_ERR)

    IF (IL_ERR /= 0) THEN
      CALL OASIS_ABORT(IL_COMPID, 'CPL_OASIS_DEFINE', 'Problem during oasis_enddef')
    ENDIF
    !
    !/ ------------------------------------------------------------------- /
  END SUBROUTINE CPL_OASIS_DEFINE
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CPL_OASIS_SND(ID_NB, ID_TIME, RDA_FIELD, LD_ACTION)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Thevenin             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          April-2016 |
    !/                  +-----------------------------------+
    !/
    !/    Jul-2013 : Origination.                    ( version 4.18 )
    !/    April-2016 : Add comments (J. Pianezze)    ( version 5.07 )
    !/
    !  1. Purpose :
    !
    !     In the model time step loop, each process sends its parts of the coupling field
    !
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ID_NB           Int.      I       Number of the field to be send
    !     ID_TIME         Int.      I       Atmosphere time-step in seconds
    !     RDA_FIELD       Real      I       Coupling field array to be send
    !     LD_ACTION       Bool.     O       Action performed
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !  5. Called by :
    !
    !      Name                  Type    Module     Description
    !     ----------------------------------------------------------------
    !      SND_FIELDS_TO_ATMOS   Subr.   W3AGCMMD   Send fields to atmos. model
    !      SND_FIELDS_TO_OCEAN   Subr.   W3OGCMMD   Send fields to ocean model
    !      SND_FIELDS_TO_ICE     Subr.   W3IGCMMD   Send fields to ice model
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks :
    !  8. Structure :
    !  9. Switches :
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)   :: ID_NB                         ! Number of the field to be send
    INTEGER, INTENT(IN)   :: ID_TIME                       ! Atmosphere time-step in seconds
    REAL(KIND=8), DIMENSION(:,:), INTENT(IN) :: RDA_FIELD  ! Coupling field array to be send
    LOGICAL, INTENT(OUT)  :: LD_ACTION                     ! Action performed
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IL_INFO                                     ! OASIS3-MCT info argument
    !/
    !/ ------------------------------------------------------------------- /
    !/ Executable part
    !/
    CALL OASIS_PUT ( SND_FLD(ID_NB)%IL_FIELD_ID &
         &              , ID_TIME                    &
         &              , RDA_FIELD                  &
         &              , IL_INFO                    &
         &                )

    LD_ACTION = IL_INFO == OASIS_SENT     .OR. IL_INFO == OASIS_TOREST .OR.   &
         &           IL_INFO == OASIS_SENTOUT  .OR. IL_INFO == OASIS_TORESTOUT

    !/ ------------------------------------------------------------------- /
  END SUBROUTINE CPL_OASIS_SND
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CPL_OASIS_RCV(ID_NB, ID_TIME, RDA_FIELD, LD_ACTION)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Thevenin             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          April-2016 |
    !/                  +-----------------------------------+
    !/
    !/    Jul-2013 : Origination.                    ( version 4.18 )
    !/    April-2016 : Add comments (J. Pianezze)    ( version 5.07 )
    !/
    !  1. Purpose :
    !
    !     In the model time step loop, each process receives its parts of the coupling field
    !
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ID_NB           Int.      I       Number of the field to be received
    !     ID_TIME         Int.      I       Ocean time-step in seconds
    !     RDA_FIELD       Real      I       Coupling field array to be received
    !     LD_ACTION       Bool.     O       Action performed
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !  5. Called by :
    !
    !      Name                  Type    Module     Description
    !     ----------------------------------------------------------------
    !      RCV_FIELDS_FROM_ATMOS   Subr.   W3AGCMMD   Receive fields from atmos. model
    !      RCV_FIELDS_FROM_OCEAN   Subr.   W3OGCMMD   Receive fields from ocean model
    !      RCV_FIELDS_FROM_ICE     Subr.   W3IGCMMD   Receive fields from ice model
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks :
    !  8. Structure :
    !  9. Switches :
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)   :: ID_NB                          ! Number of the field to be received
    INTEGER, INTENT(IN)   :: ID_TIME                        ! Ocean time-step in seconds
    REAL(KIND=8), DIMENSION(:,:), INTENT(OUT) :: RDA_FIELD    ! Coupling field array to be received
    LOGICAL, INTENT(OUT)  :: LD_ACTION                      ! Action performed
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER :: IL_INFO                                      ! OASIS3-MCT info argument
    !/
    !/ ------------------------------------------------------------------- /
    !/ Executable part
    !/
    CALL OASIS_GET ( RCV_fld(ID_NB)%IL_FIELD_ID &
         &              , ID_TIME                    &
         &              , RDA_FIELD                  &
         &              , IL_INFO                    &
         &                )
    !
    LD_ACTION = IL_INFO == OASIS_RECVD   .OR. IL_INFO == OASIS_FROMREST .OR.   &
         &           IL_INFO == OASIS_RECVOUT .OR. IL_INFO == OASIS_FROMRESTOUT
    !
    !/ ------------------------------------------------------------------- /
  END SUBROUTINE CPL_OASIS_RCV
  !/ ------------------------------------------------------------------- /
  SUBROUTINE CPL_OASIS_FINALIZE
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Thevenin             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          April-2016 |
    !/                  +-----------------------------------+
    !/
    !/    Jul-2013 : Origination.                    ( version 4.18 )
    !/  April-2016 : Add comments (J. Pianezze)      ( version 5.07 )
    !/
    !  1. Purpose :
    !
    !     Terminate the coupling
    !
    !  2. Method :
    !  3. Parameters :
    !  4. Subroutines used :
    !  5. Called by :
    !
    !      Name                Type     Module    Description
    !     ----------------------------------------------------------------
    !      WW3_SHEL            Prog.    -         Main program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks :
    !  8. Structure :
    !  9. Switches :
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !/ Executable part
    !/
    CALL OASIS_TERMINATE(IL_ERR)
    !
    IF (IL_ERR /= 0) THEN
      CALL OASIS_ABORT(IL_COMPID, 'CPL_OASIS_FINALIZE', 'Problem during oasis_terminate')
    ENDIF
    !
    !/ ------------------------------------------------------------------- /
  END SUBROUTINE CPL_OASIS_FINALIZE
  !/ ------------------------------------------------------------------- /
  SUBROUTINE GET_LIST_EXCH_FIELD(NDSO, RCV, SND, ID_NB_RCV, ID_NB_SND, RCV_STR, SND_STR)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Thevenin             |
    !/                  |           V. Garnier              |
    !/                  |           A.C. Bennis             |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         22-Mar-2021 |
    !/                  +-----------------------------------+
    !/
    !/    Jul-2013 : Origination.                               ( version 4.18 )
    !/    Mar-2014 : J. Pianezze (LPO) : Add atmospheric fields ( version 5.07 )
    !/    Apr-2015 : M. Accensi  (LPO) : Add fields selection   ( version 5.07 )
    !/    Apr-2016 : Add comments (J. Pianezze)                 ( version 5.07 )
    !/ 22-Mar-2021 : Adds extra coupling fields                 ( version 7.13 )
    !/
    !  1. Purpose :
    !
    !     Provides the list of coupling fields
    !
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     NDSO         Int.      I       Id. of the output file
    !     RCV          Type     I/O      Received variables
    !     SND          Type     I/O      Send variables
    !     ID_NB_RCV    Int.     I/O      Number of received variables
    !     ID_NB_SND    Int.     I/O      Number of send variables
    !     RCV_STR      Char.     I       Name of the received variables
    !     SND_STR      Char      I       Name of the send variables
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name                Type    Module     Description
    !     ----------------------------------------------------------------
    !      STRSPLIT            Subr.   W3SERVMD   Splits string into words
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name                Type    Module     Description
    !     ----------------------------------------------------------------
    !      CPL_OASIS_DEFINE    Subr.   W3OACPMD   Partition definition
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks :
    !  8. Structure :
    !  9. Switches :
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    !
    USE W3SERVMD, ONLY: STRSPLIT
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    TYPE(CPL_FIELD), DIMENSION(IP_MAXFLD), INTENT (INOUT)   :: RCV, SND
    INTEGER, INTENT(INOUT)                                  :: ID_NB_RCV, ID_NB_SND
    INTEGER, INTENT(IN)                                     :: NDSO
    CHARACTER(LEN=1024), INTENT(IN)                         :: RCV_STR, SND_STR
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    CHARACTER(LEN=100)                                      :: OUT_NAMES(50), TESTSTR
    INTEGER                                                 :: IOUT
    !/
    !/ ------------------------------------------------------------------- /
    !/ Executable part
    !/
    !
    ! 1. Coupling fields received by WW3
    ! ----------------------------------
    ID_NB_RCV = 0
    !
    OUT_NAMES(:)=''
    CALL STRSPLIT(RCV_STR,OUT_NAMES)
    IOUT=0
    DO WHILE (LEN_TRIM(OUT_NAMES(IOUT+1)).NE.0)
      TESTSTR=OUT_NAMES(IOUT+1)
      SELECT CASE(TRIM(TESTSTR(1:6)))
        !
        !
        ! OCEAN MODEL VARIABLES
        !
#ifdef W3_OASOCM
      CASE('DRY')
        ! wet-drying at the middle of the cell
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_OWDH'
        !
        ! wet-drying at u-location
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_OWDU'
        !
        ! wet-drying at v-location
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_OWDV'
        !
      CASE('SSH')
        ! ssh : sea surface height (m)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3__SSH'
        !
      CASE('CUR')
        ! uz : sea surface zonal currents (m.s-1)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_OSSU'
        !
        ! vz : sea surface meridional currents (m.s-1)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_OSSV'
#endif
        !

        !
        ! ATMOSPHERE MODEL VARIABLES
        !
#ifdef W3_OASACM
      CASE('WND')
        ! U10 : 10m u-wind speed (m.s-1)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3__U10'
        !
        ! V10 : 10m v-wind speed (m.s-1)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3__V10'
        !
      CASE('TAU')
        ! UTAUA : u-momentum (m2.s-2)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_UTAU'
        !
        ! V10 : v-momentum speed (m2.s-2)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_VTAU'
        !
      CASE('RHO')
        ! rhoa : air density (kg.m-3)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3_RHOA'
#endif
        !

        !
        ! ICE MODEL VARIABLES
        !
#ifdef W3_OASICM
      CASE('IC1')
        ! IC1 : ice thickness (m)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3__IC1'
        !
      CASE('IC5')
        ! ICEF : ice floe diameters (m)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3__IC5'
        !
      CASE('ICE')
        ! ICE : ice concentration (n.d)
        ID_NB_RCV=ID_NB_RCV+1
        RCV(ID_NB_RCV)%CL_FIELD_NAME='WW3__ICE'
#endif
        !

      CASE DEFAULT
        WRITE (NDSO,1001) TRIM(TESTSTR(1:6))
      END SELECT
      IOUT=IOUT+1
    END DO
    !
    ! 2. Coupling fields sent by WW3
    ! ----------------------------------
    ID_NB_SND = 0
    !
    OUT_NAMES(:)=''
    CALL STRSPLIT(SND_STR,OUT_NAMES)
    IOUT=0
    DO WHILE (LEN_TRIM(OUT_NAMES(IOUT+1)).NE.0)
      TESTSTR=OUT_NAMES(IOUT+1)
      SELECT CASE(TRIM(TESTSTR(1:6)))
        !
        !
        ! OCEAN MODEL VARIABLES
        !
#ifdef W3_OASOCM
      CASE('OHS')
        ! Significant wave height (m)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__OHS'
        !
      CASE('DRY')
        ! mask to manage wet-drying
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_ODRY'
        !
      CASE('T0M1')
        ! T0M1 / wave_t0m1 : mean period (s)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_T0M1'
        !
      CASE('T01')
        ! T01 / wave_t01 : mean period (s)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__T01'
        !
      CASE('DIR')
        ! THM / wave_thm : cosinus of mean direction (n/a)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_CDIR'
        !
        ! THM / wave_thm : sinus of mean direction (n/a)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_SDIR'
        !
      CASE('THM')
        ! THM / wave_thm : mean direction (n/a)
        ! exchange the mean direction instead of cos/sin projection
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__DIR'
        !
      CASE('BHD')
        ! BHD / wave_bhd : wave-induced Bernoulli head pressure (bhd in N.m-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__BHD'
        !
      CASE('TWO')
        ! tauox / wave_tauox : x-component of the wave-ocean momentum flux (tauox in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TWOX'
        !
        ! tauoy / wave_tauoy : y-component of the wave-ocean momentum flux (tauox in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TWOY'
        !
      CASE('TOC')
        ! tauocx / wave_tauocx : x-component of the total wave-ocean momentum flux (tauocx in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TOCX'
        !
        ! tauocy / wave_tauocy : y-component of the total wave-ocean momentum flux (tauocx in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TOCY'
        !
      CASE('FOC')
        ! phioc / wave_phioc : Wave-to-ocean TKE flux (phioc in W.m-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__FOC'
        !
      CASE('TBB')
        ! Momentum flux due to bottom friction, u component (m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TBBX'
        !
        ! Momentum flux due to bottom friction, v component (m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TBBY'
        !
      CASE('FBB')
        ! phibbl / wave_phibbl : Energy flux due to bottom friction (phioc in W.m-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__FBB'
        !
      CASE('UBR')
        ! uba / wave_ubrx : x component of the rms amplitude of orbital velocity of the waves (m/s)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_UBRX'
        !
        ! uba / wave_ubry : y component of the rms amplitude of orbital velocity of the waves (m/s)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_UBRY'
        !
      CASE('TAW')
        ! tauwix / wave_tauwix : Net wave-supported stress, u component (tauwix in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TAWX'
        !
        ! tauwiy / wave_tauwiy : ! Net wave-supported stress, v component (tauwix in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TAWY'
        !
      CASE('LM')
        ! wlm / wave_wlm : mean length wave (m)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3___LM'
        !
      CASE('WNM')
        ! wnmean / wave_wnmean : mean wave number (m-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__WNM'
        !
      CASE('TUS')
        ! Volume transport associated to Stokes drift, u component (m2.s-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TUSX'
        !
        ! Volume transport associated to Stokes drift, v component (m2.s-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TUSY'
        !
      CASE('USS')
        ! Surface Stokes drift, u component (m.s-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_USSX'
        !
        ! Surface Stokes drift, v component (m.s-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_USSY'
        !
      CASE('OCHA')
        ! Charnock Coefficient (-)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_OCHA'
#endif

        !
        ! ATMOSPHERE MODEL VARIABLES
        !
#ifdef W3_OASACM
      CASE('AHS')
        ! Significant wave height (m)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__AHS'
        !
      CASE('CUR')
        ! Ocean sea surface current (m.s-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_WSSU'
        !
        ! Ocean sea surface current (m.s-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_WSSV'
        !
      CASE('ACHA')
        ! Charnock Coefficient (-)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_ACHA'
        !
      CASE('FP')
        ! Peak frequency (s-1)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3___FP'
        !
      CASE('TP')
        ! Peak period (s)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3___TP'
        !
      CASE('FWS')
        ! Wind_sea_mean_period_T0M1 (s)
        ID_NB_SND=ID_NB_SND+1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3__FWS'
#endif
        !

        !
        ! ICE MODEL VARIABLES
        !
#ifdef W3_OASICM
      CASE('IC5')
        ! Ice floe diameters (m)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_ICEF'
        !
      CASE('TWI')
        ! TWIX : x stress to ice , u component (twix in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TWIX'
        ! TWIY : y stress to ice , v component (twiy in m2.s-2)
        ID_NB_SND = ID_NB_SND +1
        SND(ID_NB_SND)%CL_FIELD_NAME='WW3_TWIY'
#endif
        !
      CASE DEFAULT
        WRITE (NDSO,1002) TRIM(TESTSTR(1:6))
      END SELECT
      IOUT=IOUT+1
    END DO
    !
    ! Formats
    !
1001 FORMAT (/' *** WAVEWATCH III WARNING IN W3OACPMD : '/                       &
         '     REQUESTED COUPLING RECEIVED FIELD ',A,' WAS NOT RECOGNIZED.'/)
    !
1002 FORMAT (/' *** WAVEWATCH III WARNING IN W3OACPMD : '/                       &
         '     REQUESTED COUPLING SENT FIELD ',A,' WAS NOT RECOGNIZED.'/)
    !/
    !/ ------------------------------------------------------------------- /
  END SUBROUTINE GET_LIST_EXCH_FIELD
  !/ ------------------------------------------------------------------- /
  !/
END MODULE W3OACPMD
!/
!/ ------------------------------------------------------------------- /
