!> @file
!> @brief Reads triangle and unstructured grid information.
!>
!> @author F. Ardhuin
!> @author A. Roland
!> @date   26-Jan-2014
!>

#include "w3macros.h"
!/ ------------------------------------------------------------------- /

!>
!> @brief Reads triangle and unstructured grid information.
!>
!> @details Look for namelist with name NAME in unit NDS and read if found.
!>
!> @author F. Ardhuin
!> @author A. Roland
!> @date   26-Jan-2014
!>
MODULE W3TRIAMD
  !/ -------------------------------------------------------------------
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |       F. Ardhuin and A. Roland    |
  !/                  |                        FORTRAN 90 |
  !/                  | Last update :          26-Jan-2014|
  !/                  +-----------------------------------+
  !/
  !/    15-Mar-2007 : Origination.                        ( version 3.13 )
  !/    25-Aug-2011 : Modification of boundary treatment  ( version 4.04 )
  !/    30-Aug-2012 : Automatic detection of open BC      ( version 4.08 )
  !/    02-Sep-2012 : Clean up of open BC for UG grids    ( version 4.08 )
  !/    14-Oct-2013 : Correction  of latitude factor      ( version 4.12 )
  !/    26-Jan-2014 : Correction  interpolation weights   ( version 4.18 )
  !/    21-Apr-2016 : New algorithm to detect boundary    ( version 5.12 )
  !/
  !
  !  1. Purpose :
  !
  !      Reads triangle and unstructured grid information
  !
  !  2. Method :
  !
  !     Look for namelist with name NAME in unit NDS and read if found.
  !
  !  3. Parameters :
  !
  !
  !  4. Subroutines used :
  !
  !      Name               Type  Module   Description
  !     ------------------------------------------------------------------------------------
  !      READTRI            Subr. Internal Read unstructured grid data from .grd .tri formatted files.
  !      READMSH            Subr.   Id.    Read unstructured grid data from MSH format
  !      COUNT              Subr. Internal Count connection.
  !      SPATIAL_GRID       Subr.   Id.    Calculate surfaces.
  !      NVECTRI            Subr.   Id.    Define cell normals and angles and edge length
  !      COORDMAX           Subr.   Id.    Calculate  useful grid elements
  !      AREA_SI            Subr.   Id.    Define Connections
  !     ------------------------------------------------------------------------------------
  !
  !
  !  5. Called by :
  !
  !     Program in which it is contained.
  !
  !  6. Error messages :
  !
  !  7. Remarks :
  !     The only point index which is needed is IX and NX stands for the total number of grid point.
  !     IY and NY are not needed anymore, they are set to 1 in the unstructured case
  !     Some noticeable arrays are:
  !                     TRIGP  : give the vertices of each triangle
  !  8. Structure :
  !
  !  9. Switches :
  !       !/PR3   : Enables unstructured meshes (temporary, will be replace by Unstructured switch)
  ! 10. Source code :
  !
  !/ ------------------------------------------------------------------- /
  PUBLIC
  !      USE CONSTANTS
  !      USE W3GDATMD, ONLY: W3NMOD, W3SETG
  !      USE W3ODATMD, ONLY: W3NOUT, W3SETO, W3DMO5
  !      USE W3IOGRMD, ONLY: W3IOGR
  !     USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
  !      USE W3ARRYMD, ONLY: INA2R, INA2I
  !      USE W3DISPMD, ONLY: DISTAB
  !      USE W3GDATMD
  !      USE W3ODATMD, ONLY: NDSE, NDST, NDSO
  !      USE W3ODATMD, ONLY: NBI, NBI2, NFBPO, NBO, NBO2, FLBPI, FLBPO,  &
  !                         IPBPO, ISBPO, XBPO, YBPO, RDBPO, FNMPRE
  !---------------------------------------------------------------------
  !
  !C
  !        integer :: node_num
  !        integer :: dim_num
  !        integer :: triangle_order
  !        integer :: triangle_num
  !        integer :: bound_edge_num
  !        integer :: bound_num
  !C
  !        logical,save, allocatable :: edge_boundary(:)
  !        logical,save, allocatable :: node_boundary(:)
  !        integer,save, allocatable :: edge_nums(:)
  !        integer,save, allocatable :: boundary_node_index(:)
  !C
  !        integer,save, allocatable :: triangle_node(:,:)
  !        integer,save, allocatable :: edge(:,:)
  !        integer,save, allocatable :: edge_index(:,:)
  !        integer,save, allocatable :: iobp_aux(:)

  INTEGER, SAVE               :: N_OUTSIDE_BOUNDARY
  INTEGER, SAVE, ALLOCATABLE  :: OUTSIDE_BOUNDARY(:)

CONTAINS
  !/ -------------------------------------------------------------------/

  !>
  !> @brief Reads triangle and unstructured grid information from GMSH files.
  !>
  !> @details Calls the subroutines needed to compute grid connectivity.
  !>  Look for namelist with name NAME in unit NDS and read if found.
  !>
  !> @param[in] NDS    Data set number used for search.
  !> @param[in] FNAME  Name of namelist.
  !>
  !> @author F. Ardhuin
  !> @author A. Roland
  !> @date   06-Jun-2018
  !>
  SUBROUTINE READMSH(NDS,FNAME)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |           A. Roland               |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          06-Jun-2018|
    !/                  +-----------------------------------+
    !/
    !/    15-Feb-2008 : Origination.                        ( version 3.13 )
    !/    25-Aug-2011 : Change of method for IOBPD          ( version 4.04 )
    !/    06-Jun-2018 : Add DEBUGINIT/PDLIB/DEBUGSTP/DEBUGSETIOBP
    !/                                                      ( version 6.04 )
    !/
    !
    !  1. Purpose :
    !
    !      Reads triangle and unstructured grid information from GMSH files
    !      Calls the subroutines needed to compute grid connectivity
    !
    !  2. Method :
    !
    !     Look for namelist with name NAME in unit NDS and read if found.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDS     Int.   I   Data set number used for search.
    !       NAME    C*4    I   Name of namelist.
    !       STATUS  C*20   O   Status at end of routine,
    !                            '(default values)  ' if no namelist found.
    !                            '(user def. values)' if namelist read.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name               Type  Module   Description
    !     ------------------------------------------------------------------------------------
    !      NEXTLN             Subr.
    !      COUNT              Subr. Internal Count connection.
    !      SPATIAL_GRID       Subr.   Id.    Calculate surfaces.
    !      NVECTRI            Subr.   Id.    Define cell normals and angles and edge length
    !      COORDMAX           Subr.   Id.    Calculate  useful grid elements
    !      AREA_SI            Subr.   Id.    Define Connections
    !     ----------------------------------------------------------------
    !
    !
    !
    !  5. Called by :
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3GRID    Prog.          Model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !     The only point index which is needed is IX and NX stands for the total number of grid point.
    !     IY and NY are not needed anymore, they are set to 1 in the unstructured case
    !     Some noticeable arrays are:
    !                     TRIGP  : give the vertices of each triangle
    !     GMSH file gives too much information that is not necessarily required so data processing is needed (data sort and nesting).
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3ODATMD, ONLY: NDSE, NDST, NDSO
    USE W3GDATMD, ONLY: ZB, XGRD, YGRD, NTRI, NX, COUNTOT, TRIGP, NNZ, W3DIMUG
    USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
    USE CONSTANTS, only: LPDLIB
    USE W3ODATMD, ONLY: IAPROC
    !
    IMPLICIT NONE
    !/
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)                :: NDS
    CHARACTER(60), INTENT(IN)          :: FNAME
    !/
    !/ local parameters
    !/
    INTEGER                            :: i,j,k, NODES, NELTS, ID, KID
    INTEGER                            :: ID1, ID2, KID1, ITMP(3)
    INTEGER                            :: I1, I2, I3
    INTEGER(KIND=4)                    :: Ind,eltype,ntag, INode
    CHARACTER                          :: COMSTR*1, SPACE*1 = ' ', CELS*64
    REAL, ALLOCATABLE                  :: TAGS(:)
    CHARACTER(LEN=64), ALLOCATABLE     :: ELS(:)
    CHARACTER(LEN=120)                 :: LINE
    CHARACTER(LEN=50)                  :: CHTMP
    CHARACTER(LEN=10)                  :: A, B, C
    INTEGER,ALLOCATABLE                :: NELS(:), TRIGPTMP1(:,:), TRIGPTMP2(:,:)
    INTEGER(KIND=4),ALLOCATABLE        :: IFOUND(:), VERTEX(:), BOUNDTMP(:)
    DOUBLE PRECISION, ALLOCATABLE      :: XYBTMP1(:,:),XYBTMP2(:,:)
    REAL                               :: z

    OPEN(NDS,FILE = FNAME,STATUS='old')
    READ (NDS,'(A)') COMSTR
    IF (COMSTR.EQ.' ') COMSTR = '$'
    CALL NEXTLN(COMSTR, NDS, NDSE)
    READ(NDS,*) i,j,k
    CALL NEXTLN(COMSTR, NDS, NDSE)
    LPDLIB = .FALSE.
#ifdef W3_PDLIB
    LPDLIB = .TRUE.
#endif
    !
    ! read number of nodes and nodes from Gmsh files
    !
    READ(NDS,*) NODES
    ALLOCATE(XYBTMP1(3,NODES))
    DO I= 1, NODES
      READ(NDS,*) j, XYBTMP1(1,I), XYBTMP1(2,I), XYBTMP1(3,I)
    END DO
    !
    ! read number of elements and elements from Gmsh files
    !
    ALLOCATE(BOUNDTMP(NODES))
    N_OUTSIDE_BOUNDARY = 0
    CALL NEXTLN(COMSTR, NDS, NDSE)
    READ(NDS,*) NELTS
    ALLOCATE(TRIGPTMP1(3,NELTS))
    J = 0
    DO I= 1, NELTS
      READ(NDS,'(A100)') LINE
      READ(LINE,*) Ind,eltype,ntag
      ALLOCATE(TAGS(ntag))
      SELECT CASE (eltype)
        !
        ! eltype = 15 : boundary points  (this is used to make the difference
        !                                between the outside polygon and islands)
        !
      CASE(15)
        READ(LINE,*) Ind,eltype,ntag,TAGS,INODE
        N_OUTSIDE_BOUNDARY = N_OUTSIDE_BOUNDARY +1
        BOUNDTMP(N_OUTSIDE_BOUNDARY)=INODE
        !
        ! eltype = 2 : triangles
        !
      CASE (2)
        J = J + 1
        READ(LINE,*)  Ind,eltype,ntag,tags,ITMP
        TRIGPTMP1(1:3,J) = ITMP
      END SELECT

      DEALLOCATE(TAGS)
    END DO
    !
    ! organizes the grid data structure
    !
    ALLOCATE(OUTSIDE_BOUNDARY(N_OUTSIDE_BOUNDARY))
    OUTSIDE_BOUNDARY(:) = BOUNDTMP(1:N_OUTSIDE_BOUNDARY)
    NTRI = J

    ALLOCATE(IFOUND(NODES))

    IFOUND = 0
    !
    ! Verifies that the nodes are used in at least one triangle
    !
    DO K = 1, NTRI
      I1 = TRIGPTMP1(1,K)
      I2 = TRIGPTMP1(2,K)
      I3 = TRIGPTMP1(3,K)

      IFOUND(I1)= IFOUND(I1) + 1
      IFOUND(I2)= IFOUND(I2) + 1
      IFOUND(I3)= IFOUND(I3) + 1
    END DO

    J = 0

    ALLOCATE(TRIGPTMP2(3,NTRI),VERTEX(NODES),XYBTMP2(3,NODES))
    VERTEX(:)=0
    XYBTMP2 = 0

    DO I = 1, NODES
      IF( IFOUND(I) .GT. 0) THEN
        J = J+1
        XYBTMP2(:,J) = XYBTMP1(:,I)
        VERTEX(I) = J
      END IF
    END DO
    !
    ! Number of nodes after clean up
    !
    NX = J
    !
    DO I = 1, NTRI
      I1 = TRIGPTMP1(1,I)
      I2 = TRIGPTMP1(2,I)
      I3 = TRIGPTMP1(3,I)
      TRIGPTMP2(1,I) = VERTEX(I1)
      TRIGPTMP2(2,I) = VERTEX(I2)
      TRIGPTMP2(3,I) = VERTEX(I3)
    END DO
    !
    DEALLOCATE(XYBTMP1,IFOUND,TRIGPTMP1)
    DEALLOCATE(VERTEX)
    !
    !count points connections to allocate array in W3DIMUG
    !
    CALL COUNT(TRIGPTMP2)
    CALL W3DIMUG ( 1, NTRI, NX, COUNTOT, NNZ, NDSE, NDST )
    !
    ! fills arrays
    !
    DO I = 1, NX
      XGRD(1,I) = XYBTMP2(1,I)
      YGRD(1,I) = XYBTMP2(2,I)
      ZB(I)     = XYBTMP2(3,I)
    END DO
    !
    DO I=1, NTRI
      ITMP = TRIGPTMP2(:,I)
      TRIGP(:,I) = ITMP
    END DO
    !
    DEALLOCATE(TRIGPTMP2,XYBTMP2)
    !
    ! call the various routines which define the point spotting strategy
    !
    CALL SPATIAL_GRID
    CALL NVECTRI
    CALL COORDMAX

#ifdef W3_PDLIB
    IF(.false.) THEN
#endif
      CALL AREA_SI(1)
#ifdef W3_PDLIB
    ENDIF
#endif
    !
    CLOSE(NDS)
  END SUBROUTINE READMSH
  !/--------------------------------------------------------------------/

  !>
  !> @brief Reads triangle and unstructured grid information from GMSH files.
  !>
  !> @details Calls the subroutines needed to compute grid connectivity.
  !>  Look for namelist with name NAME in unit NDS and read if found.
  !>
  !> @param[in] NDS    Data set number used for search.
  !> @param[in] FNAME  Name of namelist.
  !>
  !> @author
  !> @date
  !>
  SUBROUTINE READMSH_IOBP(NDS,FNAME)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Roland               |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          06-Jun-2018|
    !/                  +-----------------------------------+
    !/
    !/    15-Feb-2008 : Origination.                        ( version 3.13 )
    !/    25-Aug-2011 : Change of method for IOBPD          ( version 4.04 )
    !/    06-Jun-2018 : Add DEBUGINIT/PDLIB/DEBUGSTP/DEBUGSETIOBP
    !/                                                      ( version 6.04 )
    !/
    !
    !  1. Purpose :
    !
    !      Reads triangle and unstructured grid information from GMSH files
    !      Calls the subroutines needed to compute grid connectivity
    !
    !  2. Method :
    !
    !     Look for namelist with name NAME in unit NDS and read if found.
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDS     Int.   I   Data set number used for search.
    !       NAME    C*4    I   Name of namelist.
    !       STATUS  C*20   O   Status at end of routine,
    !                            '(default values)  ' if no namelist found.
    !                            '(user def. values)' if namelist read.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name               Type  Module   Description
    !     ------------------------------------------------------------------------------------
    !      NEXTLN             Subr.
    !      COUNT              Subr. Internal Count connection.
    !      SPATIAL_GRID       Subr.   Id.    Calculate surfaces.
    !      NVECTRI            Subr.   Id.    Define cell normals and angles and edge length
    !      COORDMAX           Subr.   Id.    Calculate  useful grid elements
    !      AREA_SI            Subr.   Id.    Define Connections
    !     ----------------------------------------------------------------
    !
    !
    !
    !  5. Called by :
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3GRID    Prog.          Model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !     The only point index which is needed is IX and NX stands for the total number of grid point.
    !     IY and NY are not needed anymore, they are set to 1 in the unstructured case
    !     Some noticeable arrays are:
    !                     TRIGP  : give the vertices of each triangle
    !     GMSH file gives too much information that is not necessarily required so data processing is needed (data sort and nesting).
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3ODATMD, ONLY: NDSE, NDST, NDSO
    USE W3GDATMD
    USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
    USE CONSTANTS, only: LPDLIB
    USE W3ODATMD, ONLY: IAPROC
    !
    IMPLICIT NONE
    !/
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)                :: NDS
    CHARACTER(60), INTENT(IN)          :: FNAME
    !/
    !/ local parameters
    !/
    INTEGER                            :: i,j,k, NODES
    LOGICAL                            :: lfile_exists
    CHARACTER                          :: COMSTR*1, SPACE*1 = ' ', CELS*64
    DOUBLE PRECISION, ALLOCATABLE      :: XYBTMP1(:,:)

    INQUIRE(FILE=FNAME, EXIST=lfile_exists)
    IF (.NOT. lfile_exists) RETURN
    OPEN(NDS,FILE = FNAME,STATUS='old')
    READ (NDS,'(A)') COMSTR
    IF (COMSTR.EQ.' ') COMSTR = '$'
    CALL NEXTLN(COMSTR, NDS, NDSE)
    READ(NDS,*) i,j,k
    CALL NEXTLN(COMSTR, NDS, NDSE)
    !
    ! read number of nodes and nodes from Gmsh files
    !
    READ(NDS,*) NODES
    ALLOCATE(XYBTMP1(3,NODES))
    DO I= 1, NODES
      READ(NDS,*) j, XYBTMP1(1,I), XYBTMP1(2,I), XYBTMP1(3,I)
      IF (INT(XYBTMP1(3,I)) .EQ. 3) IOBP(I) = 3
    END DO
    !
    CLOSE(NDS)
  END SUBROUTINE READMSH_IOBP
  !/--------------------------------------------------------------------/

  !>
  !> @brief Boundary status (code duplication).
  !>
  !> @param[out] STATUS
  !>
  !> @author Mathieu Dutour-Sikiric
  !> @author Aron Roland
  !> @date   01-May-2018
  !>
  SUBROUTINE GET_BOUNDARY_STATUS(STATUS)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         01-Mai-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-Mai-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : boundary status (code duplication)
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !

#ifdef W3_PDLIB
    use yowElementpool, only: ne_global
    use yowNodepool, only: np_global
#endif
    USE W3GDATMD, ONLY : TRIGP, NTRI, NX
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
    !
    integer*2, intent(out) :: STATUS(NX)
    INTEGER :: COLLECTED(NX), NEXTVERT(NX), PREVVERT(NX)
    INTEGER :: ISFINISHED, INEXT, IPREV
    INTEGER :: IPNEXT, IPPREV, ZNEXT, IP, I, IE
#ifdef W3_S
    CALL STRACE (IENT, 'VA_SETUP_IOBPD')
#endif
    STATUS(:) = 0
    DO IE=1,NTRI
      DO I=1,3
        IF (I.EQ.1) THEN
          IPREV=3
        ELSE
          IPREV=I-1
        END IF
        IF (I.EQ.3) THEN
          INEXT=1
        ELSE
          INEXT=I+1
        END IF
        IP=TRIGP(I,IE)
        IPNEXT=TRIGP(INEXT,IE)
        IPPREV=TRIGP(IPREV,IE)
        IF (STATUS(IP).EQ.0) THEN
          STATUS(IP)=1
          PREVVERT(IP)=IPPREV
          NEXTVERT(IP)=IPNEXT
        END IF
      END DO
    END DO
    STATUS(:)=0
    DO
      COLLECTED(:)=0
      DO IE=1,NTRI
        DO I=1,3
          IF (I.EQ.1) THEN
            IPREV=3
          ELSE
            IPREV=I-1
          END IF
          IF (I.EQ.3) THEN
            INEXT=1
          ELSE
            INEXT=I+1
          END IF
          IP=TRIGP(I,IE)
          IPNEXT=TRIGP(INEXT,IE)
          IPPREV=TRIGP(IPREV,IE)
          IF (STATUS(IP).eq.0) THEN
            ZNEXT=NEXTVERT(IP)
            IF (ZNEXT.eq.IPPREV) THEN
              COLLECTED(IP)=1
              NEXTVERT(IP)=IPNEXT
              IF (NEXTVERT(IP).eq.PREVVERT(IP)) THEN
                STATUS(IP)=1
              END IF
            END IF
          END IF
        END DO
      END DO
      ISFINISHED=1
      DO IP=1,NX
        IF ((COLLECTED(IP).eq.0).and.(STATUS(IP).eq.0)) THEN
          STATUS(IP)=-1
        END IF
        IF (STATUS(IP).eq.0) THEN
          ISFINISHED=0
        END IF
      END DO
      IF (ISFINISHED.eq.1) THEN
        EXIT
      END IF
    END DO
  END SUBROUTINE GET_BOUNDARY_STATUS

  !/ -------------------------------------------------------------------/

  !>
  !> @brief Reads open boundary information for UNST grids following
  !>  GMESH type format.
  !>
  !> @param[in]    NDS      Data set number used for search.
  !> @param[in]    FNAME    File name.
  !> @param[inout] TMPSTA   Status map to be updated (for OBC,TMPSTA = 2).
  !> @param[out]   UGOBCOK  Flag for proper reading of OBC file.
  !>
  !> @author F. Ardhuin
  !> @date   14-Mar-2018
  !>
  SUBROUTINE READMSHOBC(NDS, FNAME, TMPSTA, UGOBCOK)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          14-Mar-2018|
    !/                  +-----------------------------------+
    !/
    !/    14-Mar-2018 : Origination.                        ( version 6.02 )
    !/
    !
    !  1. Purpose :
    !
    !      Reads open boundary information for UNST grids following GMESH type format
    !
    !  2. Method :
    !
    !     Reads an ASCII file
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NDS     Int.   I   Data set number used for search.
    !       FNAME   Char*60 I  File name
    !       TMPSTA  Char*60 I/O status map to be updated (for OBC, TMPSTA = 2)
    !       UGOBCOK Logical O   flag for proper reading of OBC file
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used : NONE
    !
    !  5. Called by :
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3GRID    Prog.          Model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD, ONLY: NX, NY, CCON , COUNTCON
    USE W3ODATMD, ONLY: NDSE, NDST, NDSO
    USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)                :: NDS
    CHARACTER(60), INTENT(IN) :: FNAME
    INTEGER, INTENT(INOUT)    :: TMPSTA(NY,NX)
    LOGICAL, INTENT(OUT)      :: UGOBCOK
    !/
    !/ local parameters
    !/
    INTEGER                            :: I, IERR
    INTEGER(KIND=4)                    :: Ind,ntag, INode
    CHARACTER                          :: COMSTR*1, SPACE*1 = ' ', CELS*64
    REAL, ALLOCATABLE                  :: TAGS(:)
    CHARACTER(LEN=120)                 :: LINE

    UGOBCOK=.FALSE.

    OPEN(NDS,FILE = FNAME,STATUS='old')
    READ (NDS,'(A)') COMSTR
    IF (COMSTR.EQ.' ') COMSTR = '$'
    CALL NEXTLN(COMSTR, NDS, NDSE)
    IERR = 0
    DO WHILE (IERR.EQ.0)
      READ (NDS,'(A100)',END=2001,ERR=2002,IOSTAT=IERR) LINE
      READ(LINE,*,IOSTAT=IERR) Ind,ntag
      IF (IERR.EQ.0) THEN
        ALLOCATE(TAGS(ntag))
        READ(LINE,*,IOSTAT=IERR) Ind,ntag,TAGS,INODE
        IF (IERR.EQ.0) THEN
          TMPSTA(1,INODE)=2
          DEALLOCATE(TAGS)
        ELSE
          GOTO 2001
        END IF
      END IF
    END DO
    CLOSE(NDS)
    UGOBCOK=.TRUE.
    RETURN
    !
2001 CONTINUE
    WRITE (NDSE,1001)
    CALL EXTCDE ( 61 )
    !
2002 CONTINUE
    WRITE (NDSE,1002) IERR
    CALL EXTCDE ( 62 )
1001 FORMAT (/' *** WAVEWATCH III ERROR IN READMSHOBC : '/          &
         '     PREMATURE END OF FILE IN READING ',A/)
1002 FORMAT (/' *** WAVEWATCH III ERROR IN READMSHOBC : '/          &
         '     ERROR IN READING ',A,'  IOSTAT =',I8/)

  END SUBROUTINE READMSHOBC
  !/ ------------------------------------------------------------------- /


  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Defines open boundary points based on depth.
  !>
  !> @details A boundary node has more node around it than triangles.
  !>
  !> @param[inout] TMPSTA  Status map to be updated (for OBC, TMPSTA = 2).
  !> @param[in]    ZBIN
  !> @param[in]    ZLIM
  !>
  !> @author F. Ardhuin
  !> @date   30-Aug-2012
  !>
  SUBROUTINE UG_GETOPENBOUNDARY(TMPSTA,ZBIN,ZLIM)
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          30-Aug-2012|
    !/                  +-----------------------------------+
    !/
    !/    30-Aug-2012 : Adpatation from SHOM-Ifremer program( version 4.07 )
    !/
    !
    !  1. purpose: defines open boundary points based on depth
    !  2. Method : a boundary node has more node around it than triangles
    !
    !
    !  3. Parameters :
    !     TMPSTA: status map to be updated (for OBC, TMPSTA = 2)
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      w3GRID    Prog.          Model configuration program
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !

    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    USE W3GDATMD, ONLY: NX, NY, CCON, COUNTCON, IOBP

#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(INOUT)  :: TMPSTA(NY,NX)
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    REAL   , INTENT(IN)     :: ZBIN(NY,NX)
    REAL   , INTENT(IN)     :: ZLIM
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 ::  IBC, IX
    INTEGER                 ::  MASK(NX)
    INTEGER*2               ::  STATUS(NX)
    !
    MASK(:)=1
    CALL SET_IOBP (MASK, STATUS)
    !
#ifdef W3_S
    CALL STRACE (IENT, 'UG_GETOPENBOUNDARY')
#endif
    DO IBC = 1, N_OUTSIDE_BOUNDARY
      IX = OUTSIDE_BOUNDARY(IBC)
      !write(*,*) 'TEST1', IX, TMPSTA(1,IX), CCON(IX), COUNTCON(IX), ZBIN(1,IX), ZLIM
      ! OUTSIDE_BOUNDARY(IBC) is defined over the full nodes NODES indexes
      ! whereas TMPSTA and ZBIN are defined over the clean up list of nodes NX
      IF ((IX.NE.0).AND.(IX.LE.NX)) THEN
        IF ( (TMPSTA(1,IX).EQ.1) .AND. (STATUS(IX).EQ.0) .AND. (ZBIN(1,IX) .LT. ZLIM)) TMPSTA(1,IX) = 2
      END IF
    END DO
    !
  END SUBROUTINE UG_GETOPENBOUNDARY
  !/ ------------------------------------------------------------------- /


  !/----------------------------------------------------------------------

  !>
  !> @brief Calculates triangle areas and reorders the triangles to have
  !>  them oriented counterclockwise.
  !>
  !> @details The triangle surface calculation is based on cross product.
  !>
  !> @author A. Roland
  !> @author F. Ardhuin
  !> @date   31-Aug-2011
  !>
  SUBROUTINE SPATIAL_GRID
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |      A. Roland  (BGS IT&E GbmH)   |
    !/                  |      F. Ardhuin (IFREMER)         |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          31-Aug-2011|
    !/                  +-----------------------------------+
    !/
    !/    15-May-2007 : Origination: adjustment from the WWM code       ( version 3.13 )
    !/    31-Aug-2011 : Simplfies the cross products                    ( version 4.05 )
    !/
    !
    !  1. Purpose :
    !
    !      Calculates triangle areas and reorders the triangles to have them
    !      oriented counterclockwise
    !
    !  2. Method :
    !
    !     The triangle surface calculation is based on cross product.
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      READTRI    Subr. Internal  Unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     This part of code is adapted from the WWM wave model develop at the Darmstadt University
    !     (Aaron Roland)
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3ODATMD, ONLY: NDSE

    IMPLICIT NONE
    !
    !local parameters
    !
    REAL              :: TL1, TL2, TL3, TMPTRIGP
    INTEGER           :: I1, I2, I3
    INTEGER           :: K
    REAL*8            :: PT(3,2)

#ifdef W3_S
    INTEGER                      ::  IENT = 0
#endif
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    CALL STRACE (IENT, 'SPATIAL_GRID')
#endif

    DO K = 1, NTRI

      I1 = TRIGP(1,K)
      I2 = TRIGP(2,K)
      I3 = TRIGP(3,K)

!AR: todo call this only for global grid 
      CALL FIX_PERIODCITY(I1,I2,I3,XGRD,YGRD,PT)
      !
      ! cross product of edge-vector  (orientated anticlockwise)
      !
      TRIA(K) = REAL( (PT(2,2)-PT(1,2)) & 
           *(PT(1,1)-PT(3,1))      &    
           +(PT(3,2)-PT(1,2))      &    
           *(PT(2,1)-PT(1,1))      )*0.5
      !
      ! test on negative triangle area, which means that the orientiation is not as assumed to be anticw.
      ! therefore we swap the nodes !!!
      !
      IF (TRIA(K) .lt. TINY(1.)) THEN
        TMPTRIGP = TRIGP(2,K)
        TRIGP(2,K) = TRIGP(3,K)
        TRIGP(3,K) = TMPTRIGP
        I2 = TRIGP(2,K)
        I3 = TRIGP(3,K)
        TRIA(K) = -1.d0*TRIA(K)
      END IF
    END DO
  END SUBROUTINE SPATIAL_GRID
  !/--------------------------------------------------------------------/
  !
  !/--------------------------------------------------------------------/

  !>
  !> @brief Calculate cell tools: inward normal, angles and length of edges.
  !>
  !> @details To get inward pointing normals, triangle are glanced through
  !>  anti-clockwisely.
  !>
  !> @author A. Roland
  !> @date   15-May-2008
  !>
  SUBROUTINE NVECTRI
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Roland               |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          15-May-2008|
    !/                  +-----------------------------------+
    !/
    !/    15-May-2007 : Origination: adjustment from the WWM code       ( version 3.13 )
    !/
    !
    !  1. Purpose :
    !
    !      Calculate cell tools: inward normal, angles and length of edges.
    !
    !  2. Method :
    !      To get inward pointing normals, triangle are glanced through anti-clockwisely
    !
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      READTRI    Subr. Internal  Unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE CONSTANTS

    IMPLICIT NONE
    !
    !local parameter
    !
    INTEGER :: IP, IE
    INTEGER :: I1, I2, I3, I11, I22, I33
    !
    REAL*8    :: P1(2), P2(2), P3(2)
    REAL*8    :: R1(2), R2(2), R3(2)
    REAL*8    :: N1(2), N2(2), N3(2)
    REAL*8    :: TMP(3)
    REAL*8    :: TMPINV(3)
    REAL*8    :: PT(3,2)
#ifdef W3_S
    INTEGER                      ::  IENT = 0
#endif
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    CALL STRACE (IENT, 'NVECTRI')
#endif
    !
    DO IE = 1, NTRI
      !
      ! vertices
      !
      I1 = TRIGP(1,IE)
      I2 = TRIGP(2,IE)
      I3 = TRIGP(3,IE)

      CALL FIX_PERIODCITY(I1,I2,I3,XGRD,YGRD,PT)

      P1(1) = PT(1,1)
      P1(2) = PT(1,2)
      P2(1) = PT(2,1)
      P2(2) = PT(2,2)
      P3(1) = PT(3,1)
      P3(2) = PT(3,2)
      !
      ! I1 -> I2, I2 -> I3, I3 -> I1 (anticlockwise orientation is preserved)
      !
      R1 = P3-P2
      R2 = P1-P3
      R3 = P2-P1

      N1(1) = (-R1(2))
      N1(2) = ( R1(1))
      N2(1) = (-R2(2))
      N2(2) = ( R2(1))
      N3(1) = (-R3(2))
      N3(2) = ( R3(1))
      !
      ! edges length
      !
      LEN(IE,1) = DSQRT(R1(1)**2+R1(2)**2)
      LEN(IE,2) = DSQRT(R2(1)**2+R2(2)**2)
      LEN(IE,3) = DSQRT(R3(1)**2+R3(2)**2)
      !
      ! inward normal used for propagation (not normalized)
      !
      IEN(IE,1) = N1(1)
      IEN(IE,2) = N1(2)
      IEN(IE,3) = N2(1)
      IEN(IE,4) = N2(2)
      IEN(IE,5) = N3(1)
      IEN(IE,6) = N3(2)

    END DO

  END SUBROUTINE NVECTRI
  !/---------------------------------------------------------------------------

  !/------------------------------------------------------------------------

  !>
  !> @brief Calculate global and maximum number of connection for array
  !>  allocations.
  !>
  !> @param[in] TRIGPTEMP  Temporary array of triangle vertices.
  !>
  !> @author A. Roland
  !> @author F. Ardhuin
  !> @date   15-May-2008
  !>
  SUBROUTINE COUNT(TRIGPTEMP)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Roland               |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          15-May-2008|
    !/                  +-----------------------------------+
    !/
    !/    15-May-2007 : Origination.                        ( version 3.13 )
    !/
    !
    !  1. Purpose :
    !
    !      Calculate global and maximum number of connection for array allocations .
    !
    !  2. Method :
    !
    !  3. Parameters :
    !     Parameter list
    !     ----------------------------------------------------------------
    !       NTRI         Int.   I   Total number of triangle.
    !       TRIGPTEMP    Int    I   Temporary array of triangle vertices
    !       COUNTRI      Int    O   Maximum number of connected triangle
    !                               for a given points
    !       COUNTOT      Int    O   Global number of triangle connection
    !                               for the whole grid.
    !     ----------------------------------------------------------------
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      READTRI    Subr. Internal  Unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    IMPLICIT NONE


    !/ parameter list

    INTEGER,INTENT(IN) :: TRIGPTEMP(:,:)
    !/ ------------------------------------------------------------------- /
    !/ local parameter

    INTEGER               :: CONN(NX)
    INTEGER               :: COUNTER, IP, IE, I, J, N(3)
#ifdef W3_S
    INTEGER                      ::  IENT = 0
#endif
    !/------------------------------------------------------------------------

#ifdef W3_S
    CALL STRACE (IENT, 'COUNT')
#endif

    COUNTRI=0
    COUNTOT=0
    CONN(:)= 0

    !
    !calculate the number of connected triangles for a given point.
    !

    DO IE = 1,NTRI
      N(:) = 0.
      N(1) = TRIGPTEMP(1,IE)
      N(2) = TRIGPTEMP(2,IE)
      N(3) = TRIGPTEMP(3,IE)
      CONN(N(1)) = CONN(N(1)) + 1
      CONN(N(2)) = CONN(N(2)) + 1
      CONN(N(3)) = CONN(N(3)) + 1
    ENDDO

    COUNTRI = MAXVAL(CONN)
    !
    ! calculate the global number of connections available through the mesh
    !
    J=0
    DO  IP=1,NX
      DO I=1,CONN(IP)
        J=J+1
      ENDDO
    ENDDO
    COUNTOT=J

  END SUBROUTINE COUNT

  !/----------------------------------------------------------------------------

  !>
  !> @brief Calculate first point and last point coordinates, and minimum and
  !>  maximum edge length.
  !>
  !> @author F. Ardhuin
  !> @date   15-May-2008
  !>
  SUBROUTINE COORDMAX
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          15-May-2008|
    !/                  +-----------------------------------+
    !/
    !/    15-May-2007 : Origination.                        ( version 3.13 )
    !/
    !  1. Purpose :
    !
    !      Calculate first point and last point coordinates, and minimum and maximum edge length.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      READTRI    Subr. Internal  Unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    IMPLICIT NONE
#ifdef W3_S
    INTEGER                      ::  IENT = 0
#endif


#ifdef W3_S
    CALL STRACE (IENT, 'COORDMAX')
#endif
    !
    ! maximum of coordinates s
    !
    MAXX = MAXVAL(XGRD(1,:))
    MAXY = MAXVAL(YGRD(1,:))
    !
    ! minimum of coordinates
    !
    X0 = MINVAL(XGRD(1,:))
    Y0 = MINVAL(YGRD(1,:))
    !
    !maximum and minimum length of edges
    !
    DXYMAX = MAXVAL(LEN(:,:))
    SX = MINVAL(LEN(:,:))
    SY = SX
    !
  END SUBROUTINE COORDMAX
  !-------------------------------------------------------------------------

  !>
  !> @brief Define optimized connection arrays (points and triangles) for
  !>  spatial propagation schemes.
  !>
  !> @details The storage is optimize especially considering the iterative solver used.
  !>  The schemes used are vertex-centered, a point has to be considered within its
  !>  median dual cell. For a given point, the surface of the dual cell is one third
  !>  of the sum of the surface of connected triangles.
  !>
  !>  This routine is from WWM developed in Darmstadt(Aaron Roland).
  !>
  !> @param[in] IMOD  Model number to point to.
  !>
  !> @author A. Roland
  !> @date   23-Aug-2011
  !>
  SUBROUTINE AREA_SI(IMOD)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           A. Roland               |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          23-Aug-2011|
    !/                  +-----------------------------------+
    !/
    !/    15-May-2007 : Origination: adjustment from the WWM code       ( version 3.13 )
    !/    23-Aug-2011 : Removes double entries in VNEIGH                ( version 4.04 )
    !/
    !
    !  1. Purpose :
    !
    !      Define optimized connection arrays (points and triangles) for spatial propagation schemes.
    !
    !  2. Method :
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      READTRI    Subr. Internal  Unstructured mesh definition.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !     The storage is optimize especially considering the iterative solver used.
    !     The schemes used are vertex-centered, a point has to be considered within its
    !     median dual cell. For a given point, the surface of the dual cell is one third
    !     of the sum of the surface of connected triangles.
    !     This routine is from WWM developped in Darmstadt(Aaron Roland)
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /

    USE W3GDATMD
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    IMPLICIT NONE
    !/ input

    INTEGER, INTENT(IN) :: IMOD

    !/ local parameters

    INTEGER :: COUNTER,ifound,alreadyfound
    INTEGER :: I, J, K, II
    INTEGER :: IP, IE, POS, POS_I, POS_J, POS_K, IP_I, IP_J, IP_K
    INTEGER :: I1, I2, I3, IP2, CHILF(NX)
    INTEGER :: TMP(NX), CELLVERTEX(NX,COUNTRI,2)
    INTEGER :: COUNT_MAX
    DOUBLE PRECISION   :: TRIA03
    INTEGER, ALLOCATABLE :: PTABLE(:,:)

#ifdef W3_S
    INTEGER                      ::  IENT = 0
#endif
    !/ ------------------------------------------------------------------- /

#ifdef W3_S
    CALL STRACE (IENT, 'AREA_SI')
#endif

    SI(:) = 0.D0
    !
    CCON(:) = 0     ! Number of connected Elements
    DO IE = 1, NTRI
      I1 = TRIGP(1,IE)
      I2 = TRIGP(2,IE)
      I3 = TRIGP(3,IE)
      CCON(I1) = CCON(I1) + 1
      CCON(I2) = CCON(I2) + 1
      CCON(I3) = CCON(I3) + 1
      TRIA03 = 1./3. * TRIA(IE)
      SI(I1) = SI(I1) + TRIA03
      SI(I2) = SI(I2) + TRIA03
      SI(I3) = SI(I3) + TRIA03
    ENDDO

    CELLVERTEX(:,:,:) = 0 ! Stores for each node the Elementnumbers of the connected Elements
    ! and the Position of the Node in the Element Index

    CHILF = 0

    DO IE = 1, NTRI
      DO J=1,3
        I = TRIGP(J,IE)!INE(J,IE)
        CHILF(I) = CHILF(I)+1
        CELLVERTEX(I,CHILF(I),1) = IE
        CELLVERTEX(I,CHILF(I),2) = J
      END DO
    ENDDO
    !
    ! Second step in storage, the initial 3D array CELLVERTEX, is transformed in a 1D array
    ! the global index is J . From now, all the computation step based on these arrays must
    ! abide by the conservation of the 2 loop algorithm (points + connected triangles)
    !
    INDEX_CELL(1)=1
    J = 0
    DO IP = 1, NX
      DO I = 1, CCON(IP)
        J = J + 1
        IE_CELL(J)  = CELLVERTEX(IP,I,1)
        POS_CELL(J) = CELLVERTEX(IP,I,2)
      END DO
      INDEX_CELL(IP+1)=J+1
    END DO

    IF (.NOT. FSNIMP) RETURN

    J = 0
    DO IP = 1, NX
      DO I = 1, CCON(IP)
        J = J + 1
      END DO
    END DO

    COUNT_MAX = J

    ALLOCATE(PTABLE(COUNT_MAX,7))

    J = 0
    PTABLE(:,:) = 0.
    DO IP = 1, NX
      DO I = 1, CCON(IP)
        J = J + 1
        IE    = IE_CELL(J)
        POS   = POS_CELL(J)
        I1 = TRIGP(1,IE)
        I2 = TRIGP(2,IE)
        I3 = TRIGP(3,IE)
        IF (POS == 1) THEN
          POS_J = 2
          POS_K = 3
        ELSE IF (POS == 2) THEN
          POS_J = 3
          POS_K = 1
        ELSE
          POS_J = 1
          POS_K = 2
        END IF
        IP_I = IP
        IP_J = TRIGP(POS_J,IE)
        IP_K = TRIGP(POS_K,IE)
        PTABLE(J,1) = IP_I
        PTABLE(J,2) = IP_J
        PTABLE(J,3) = IP_K
        PTABLE(J,4) = POS
        PTABLE(J,5) = POS_J
        PTABLE(J,6) = POS_K
        PTABLE(J,7) = IE
      END DO
    END DO

    !        WRITE(*,'("+TRACE......",A)') 'SET UP SPARSE MATRIX POINTER ... COUNT NONZERO ENTRY'

    J = 0
    K = 0
    DO IP = 1, NX
      TMP(:) = 0
      DO I = 1, CCON(IP)
        J = J + 1
        IP_J  = PTABLE(J,2)
        IP_K  = PTABLE(J,3)
        POS   = PTABLE(J,4)
        TMP(IP)   = 1
        TMP(IP_J) = 1
        TMP(IP_K) = 1
      END DO
      K = K + SUM(TMP)
    END DO

    NNZ => GRIDS(IMOD)%NNZ
    NNZ = K

    !          WRITE(*,'("+TRACE......",A)') 'SET UP SPARSE MATRIX POINTER ... SETUP POINTER'

    ALLOCATE (GRIDS(IMOD)%JAA(NNZ))
    ALLOCATE (GRIDS(IMOD)%IAA(NX+1))
    ALLOCATE (GRIDS(IMOD)%POSI(3,COUNT_MAX))
    JAA   => GRIDS(IMOD)%JAA
    IAA   => GRIDS(IMOD)%IAA
    POSI  => GRIDS(IMOD)%POSI

    J = 0
    K = 0
    IAA(1) = 1
    JAA    = 0
    DO IP = 1, NX ! Run through all rows
      TMP = 0
      DO I = 1, CCON(IP)         ! Check how many entries there are ...
        J = J + 1                ! this is the same J index as in IE_CELL
        IP_J  = PTABLE(J,2)
        IP_K  = PTABLE(J,3)
        TMP(IP)   = 1
        TMP(IP_J) = 1
        TMP(IP_K) = 1
      END DO
      DO I = 1, NX               ! Run through all columns
        IF (TMP(I) .GT. 0) THEN  ! this is true only for the connected points
          K = K + 1
          JAA(K) = I
        END IF
      END DO
      IAA(IP + 1) = K + 1
    END DO

    POSI = 0
    J = 0
    DO IP = 1, NX
      DO I = 1, CCON(IP)
        J = J + 1
        IP_J  = PTABLE(J,2)
        IP_K  = PTABLE(J,3)
        DO K = IAA(IP), IAA(IP+1) - 1
          IF (IP   == JAA(K)) POSI(1,J)  = K
          IF (IP_J == JAA(K)) POSI(2,J)  = K
          IF (IP_K == JAA(K)) POSI(3,J)  = K
          IF (K == 0) THEN
            WRITE(*,*) 'ERROR IN AREA_SI K .EQ. 0'
            STOP
          END IF
        END DO
      END DO
    END DO

    DEALLOCATE(PTABLE)

  END SUBROUTINE AREA_SI

  !>
  !> @brief Determine whether a point is inside or outside an
  !>  unstructured grid, and returns index of triangle and
  !>  interpolation weights.
  !>
  !> @details This is the analogue for triangles of the FUNCTION W3GRMP.
  !>
  !>  Using barycentric coordinates defined as the ratio of triangle
  !>  algebric areas which are positive or negative.  Computes the 3
  !>  interpolation weights for each triangle until they are all positive.
  !>
  !> @param[in]  IMOD   Model number to point to.
  !> @param[in]  XTIN   X-coordinate of target point.
  !> @param[in]  YTIN   Y-coordinate of target point.
  !> @param[out] ITOUT  Model number to point to.
  !> @param[out] IS     I indices of vertices of enclosing grid cell.
  !> @param[out] JS     J indices of vertices of enclosing grid cell.
  !> @param[out] RW     Array of interpolation weights.
  !>
  !> @author Mathieu Dutour Sikiric, IRB
  !> @author Aron Roland, Z&P
  !> @author Fabrice Ardhuin
  !> @date   26-Jan-2014
  !>
  SUBROUTINE IS_IN_UNGRID(IMOD, XTIN, YTIN, ITOUT, IS, JS, RW)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |      Mathieu Dutour Sikiric, IRB  |
    !/                  |                 Aron Roland, Z&P  |
    !/                  |             Fabrice Ardhuin       |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          26-Jan-2014|
    !/                  +-----------------------------------+
    !/
    !/ Adapted from other subroutine
    !/    15-Oct-2007 : Origination.                        ( version 3.13 )
    !/    21-Sep-2012 : Uses same interpolation as regular  ( version 4.08 )
    !/    26-Jan-2014 : Correcting bug in RW                ( version 4.18 )
    !/
    !  1. Purpose :
    !
    !      Determine whether a point is inside or outside an unstructured grid,
    !      and returns index of triangle and interpolation weights
    !      This is the analogue for triangles of the FUNCTION W3GRMP
    !
    !  2. Method :
    !
    !     Using barycentric coordinates defined as the ratio of triangle algebric areas
    !     which are positive or negative.
    !     Computes the 3 interpolation weights for each triangle until they are all positive
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number to point to.
    !       XTIN    Real   I   X-coordinate of target point.
    !       YTIN    Real   I   Y-coordinate of target point.
    !       ITOUT    Int.   I   Model number to point to.
    !       IS,JS   I.A.   O   (I,J) indices of vertices of enclosing grid cell.
    !       RW      R.A.   O   Array of interpolation weights.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     None
    !
    !  5. Called by :
    !
    !     WMGLOW, W3IOPP, WMIOPP, WW3_GINT
    !
    !  6. Error messages :
    !
    !     - Error checks on previous setting of variable.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/S    Enable subroutine tracing.
    !     !/T    Enable test output
    !
    ! 10. Source code :
    !
    !  2. Method :
    !
    !     Using barycentric coordinates. Each coefficient depends on the mass of its related point in the interpolation.
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3IOPP    Subr. Internal  Preprocessing of point output.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !      This subroutine is adjusted from CREST code (Fabrice Ardhuin)
    !      For a given output point, the algorithm enable to glance through all the triangles
    !      to find the one the point belong to, and then make interpolation.
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !       !/LLG   Spherical grid.
    !       !/XYG   Carthesian grid.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3ODATMD, ONLY: NDSE
    IMPLICIT NONE

    !/ ------------------------------------------------------------------- /
    ! Parameter list

    INTEGER, INTENT(IN)            :: IMOD
    DOUBLE PRECISION, INTENT(IN)   :: XTIN, YTIN
    INTEGER, INTENT(OUT)           :: itout
    INTEGER, INTENT(OUT)           :: IS(4), JS(4)
    REAL, INTENT(OUT)              :: RW(4)
    !/ ------------------------------------------------------------------- /
    !local parameters

    DOUBLE PRECISION             :: x1, x2, x3
    DOUBLE PRECISION             :: y1, y2, y3
    DOUBLE PRECISION             :: s1, s2, s3, sg1, sg2, sg3
    REAL*8                       :: PT(3,2)
    INTEGER                      :: ITRI
    INTEGER                      :: I1, I2, I3
    INTEGER                      :: nbFound
#ifdef W3_S
    INTEGER                      ::  IENT = 0
    CALL STRACE (IENT, 'IS_IN_UNGRID')
#endif

    !
    itout = 0
    nbFound=0
    ITRI = 0
    DO WHILE (nbFound.EQ.0.AND.ITRI.LT.GRIDS(IMOD)%NTRI)
      ITRI = ITRI +1
      I1=GRIDS(IMOD)%TRIGP(1,ITRI)
      I2=GRIDS(IMOD)%TRIGP(2,ITRI)
      I3=GRIDS(IMOD)%TRIGP(3,ITRI)

      CALL FIX_PERIODCITY(I1,I2,I3,GRIDS(IMOD)%XGRD,GRIDS(IMOD)%YGRD,PT)
      ! coordinates of the first vertex A
      x1 = PT(1,1)
      y1 = PT(1,2)
      ! coordinates of the 2nd vertex B
      x2 = PT(2,1)
      y2 = PT(2,2)
      !coordinates of the 3rd vertex C
      x3 = PT(3,1)
      y3 = PT(3,2)
      !with M = (XTIN,YTIN) the target point ...
      !vector product of AB and AC
      sg3=(y3-y1)*(x2-x1)-(x3-x1)*(y2-y1)
      !vector product of AB and AM
      s3=(YTIN-y1)*(x2-x1)-(XTIN-x1)*(y2-y1)
      !vector product of BC and BA
      sg1=(y1-y2)*(x3-x2)-(x1-x2)*(y3-y2)
      !vector product of BC and BM
      s1=(YTIN-y2)*(x3-x2)-(XTIN-x2)*(y3-y2)
      !vector product of CA and CB
      sg2=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3)
      !vector product of CA and CM
      s2=(YTIN-y3)*(x1-x3)-(XTIN-x3)*(y1-y3)
      IF ((s1*sg1.GE.0).AND.(s2*sg2.GE.0).AND.(s3*sg3.GE.0)) THEN
        itout=ITRI
        nbFound=nbFound+1
        IS(1)=I1
        IS(2)=I2
        IS(3)=I3
        IS(4)=1
        JS(:)=1
        RW(1)=s1/sg1
        RW(2)=s2/sg2
        RW(3)=1.-RW(1)-RW(2)  !s3/sg3
        RW(4)=0.
      END IF
    ENDDO
  END SUBROUTINE IS_IN_UNGRID
  !/ -------------------------------------------------------------------

  !>
  !> @brief Determine whether a point is inside or outside an
  !>  unstructured grid, and returns index of triangle and
  !>  interpolation weights.
  !>
  !> @details This is the analogue for triangles of the FUNCTION W3GRMP.
  !>
  !>  Using barycentric coordinates defined as the ratio of triangle
  !>  algebric areas which are positive or negative.  Computes the 3
  !>  interpolation weights for each triangle until they are all positive.
  !>
  !> @param[in]  IMOD   Model number to point to.
  !> @param[in]  XTIN   X-coordinate of target point.
  !> @param[in]  YTIN   Y-coordinate of target point.
  !> @param[in]  FORCE
  !> @param[out] ITOUT  Model number to point to.
  !> @param[out] IS     I indices of vertices of enclosing grid cell.
  !> @param[out] JS     J indices of vertices of enclosing grid cell.
  !> @param[out] RW     Array of interpolation weights.
  !>
  !> @author Mathieu Dutour Sikiric, IRB
  !> @author Aron Roland, Z&P
  !> @author Fabrice Ardhuin
  !> @date   26-Jan-2014
  !>
  SUBROUTINE IS_IN_UNGRID2(IMOD, XTIN, YTIN, FORCE, ITOUT, IS, JS, RW)
    !/ -------------------------------------------------------------------
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |      Mathieu Dutour Sikiric, IRB  |
    !/                  |                 Aron Roland, Z&P  |
    !/                  |             Fabrice Ardhuin       |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          26-Jan-2014|
    !/                  +-----------------------------------+
    !/
    !/ Adapted from other subroutine
    !/    15-Oct-2007 : Origination.                        ( version 3.13 )
    !/    21-Sep-2012 : Uses same interpolation as regular  ( version 4.08 )
    !/    26-Jan-2014 : Correcting bug in RW                ( version 4.18 )
    !/
    !  1. Purpose :
    !
    !      Determine whether a point is inside or outside an unstructured grid,
    !      and returns index of triangle and interpolation weights
    !      This is the analogue for triangles of the FUNCTION W3GRMP
    !
    !  2. Method :
    !
    !     Using barycentric coordinates defined as the ratio of triangle algebric areas
    !     which are positive or negative.
    !     Computes the 3 interpolation weights for each triangle until they are all positive
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !       IMOD    Int.   I   Model number to point to.
    !       XTIN    Real   I   X-coordinate of target point.
    !       YTIN    Real   I   Y-coordinate of target point.
    !       ITOUT    Int.   I   Model number to point to.
    !       IS,JS   I.A.   O   (I,J) indices of vertices of enclosing grid cell.
    !       RW      R.A.   O   Array of interpolation weights.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     None
    !
    !  5. Called by :
    !
    !     WMGLOW, W3IOPP, WMIOPP, WW3_GINT
    !
    !  6. Error messages :
    !
    !     - Error checks on previous setting of variable.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/S    Enable subroutine tracing.
    !     !/T    Enable test output
    !
    ! 10. Source code :
    !


    !  2. Method :
    !
    !     Using barycentric coordinates. Each coefficient depends on the mass of its related point in the interpolation.
    !
    !  3. Parameters :
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3IOPP    Subr. Internal  Preprocessing of point output.
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !      This subroutine is adjusted from CREST code (Fabrice Ardhuin)
    !      For a given output point, the algorithm enable to glance through all the triangles
    !      to find the one the point belong to, and then make interpolation.
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !       !/LLG   Spherical grid.
    !       !/XYG   Carthesian grid.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
    USE W3GDATMD
    USE W3SERVMD, ONLY: EXTCDE
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    USE W3ODATMD, ONLY: NDSE
    IMPLICIT NONE

    !/ ------------------------------------------------------------------- /
    ! Parameter list

    INTEGER, INTENT(IN)            :: IMOD, FORCE
    DOUBLE PRECISION, INTENT(IN)            :: XTIN, YTIN
    INTEGER, INTENT(OUT)           :: itout
    INTEGER, INTENT(OUT)           :: IS(4), JS(4)
    REAL, INTENT(OUT)              :: RW(4)
    !/ ------------------------------------------------------------------- /
    !local parameters

    DOUBLE PRECISION             :: x1, x2, x3, D1, D2, D3, DISTMIN, DDMIN
    DOUBLE PRECISION             :: s1, s2, s3, sg1, sg2, sg3, smin, ssum
    DOUBLE PRECISION             :: y1, y2, y3
    INTEGER                      :: ITRI, ITRIS
    INTEGER                      :: I1, I2, I3
    INTEGER                      :: nbFound
    LOGICAL                      :: MAPSTAOK
#ifdef W3_S
    INTEGER                      ::  IENT = 0
    CALL STRACE (IENT, 'IS_IN_UNGRID2')
#endif

    !
    itout = 0
    nbFound=0
    ITRI = 0
    ITRIS = 1
    ssum = 0
    smin = 0
    DO WHILE (nbFound.EQ.0.AND.ITRI.LT.GRIDS(IMOD)%NTRI)
      ITRI = ITRI +1
      I1=GRIDS(IMOD)%TRIGP(1,ITRI)
      I2=GRIDS(IMOD)%TRIGP(2,ITRI)
      I3=GRIDS(IMOD)%TRIGP(3,ITRI)
      ! coordinates of the first vertex A
      x1=GRIDS(IMOD)%XGRD(1,I1)
      y1=GRIDS(IMOD)%YGRD(1,I1)
      ! coordinates of the 2nd vertex B
      x2=GRIDS(IMOD)%XGRD(1,I2)
      y2=GRIDS(IMOD)%XGRD(1,I2)
      !coordinates of the 3rd vertex C
      x3=GRIDS(IMOD)%XGRD(1,I3)
      y3=GRIDS(IMOD)%YGRD(1,I3)
      !with M = (XTIN,YTIN) the target point ...
      !vector product of AB and AC
      sg3=(y3-y1)*(x2-x1)-(x3-x1)*(y2-y1)
      !vector product of AB and AM
      s3=(YTIN-y1)*(x2-x1)-(XTIN-x1)*(y2-y1)
      !vector product of BC and BA
      sg1=(y1-y2)*(x3-x2)-(x1-x2)*(y3-y2)
      !vector product of BC and BM
      s1=(YTIN-y2)*(x3-x2)-(XTIN-x2)*(y3-y2)
      !vector product of CA and CB
      sg2=(y2-y3)*(x1-x3)-(x2-x3)*(y1-y3)
      !vector product of CA and CM
      s2=(YTIN-y3)*(x1-x3)-(XTIN-x3)*(y1-y3)
      !     ssum = ABS(s1*sg1)+ABS(s2*sg2)+ABS(s3*sg3)
      MAPSTAOK = ((GRIDS(IMOD)%MAPSTA(1,I1).GE.1).AND. &
           (GRIDS(IMOD)%MAPSTA(1,I2).GE.1).AND.(GRIDS(IMOD)%MAPSTA(1,I3).GE.1))
      IF (FORCE.LT.2) MAPSTAOK =.TRUE.
      ssum = (XTIN-(x1+x2+x3)/3.)**2+(YTIN-(y1+y2+y2)/3.)**2
      IF (smin.EQ.0.AND. MAPSTAOK ) smin=ssum
      !WRITE(6,*) 'ssum',ITRI,MAPSTAOK,ssum,smin
      IF (ssum.LT.smin .AND.  MAPSTAOK  ) THEN
        smin=ssum
        ITRIS=ITRI
      ENDIF
      IF ((s1*sg1.GE.0).AND.(s2*sg2.GE.0).AND.(s3*sg3.GE.0)) THEN
        itout=ITRI
        nbFound=nbFound+1
        IS(1)=I1
        IS(2)=I2
        IS(3)=I3
        IS(4)=1
        JS(:)=1
        RW(1)=s1/sg1
        RW(2)=s2/sg2
        RW(3)=1.-RW(1)-RW(2)  !s3/sg3
        RW(4)=0.
      END IF
    ENDDO
    IF (itout.EQ.0.AND.FORCE.GT.0) THEN
      ITRI=ITRIS
      I1=GRIDS(IMOD)%TRIGP(1,ITRI)
      I2=GRIDS(IMOD)%TRIGP(2,ITRI)
      I3=GRIDS(IMOD)%TRIGP(3,ITRI)
      ! coordinates of the first vertex A
      x1=GRIDS(IMOD)%XGRD(1,I1)
      y1=GRIDS(IMOD)%YGRD(1,I1)
      ! coordinates of the 2nd vertex B
      x2=GRIDS(IMOD)%XGRD(1,I2)
      y2=GRIDS(IMOD)%YGRD(1,I2)
      !coordinates of the 3rd vertex C
      x3=GRIDS(IMOD)%XGRD(1,I3)
      y3=GRIDS(IMOD)%YGRD(1,I3)
      D1=(XTIN-X1)**2+(YTIN-Y1)**2
      D2=(XTIN-X2)**2+(YTIN-Y2)**2
      D3=(XTIN-X3)**2+(YTIN-Y3)**2
      IF (D1.LE.D2.AND.D1.LE.D3) IS(1)=I1
      IF (D2.LE.D1.AND.D2.LE.D3) IS(1)=I2
      IF (D3.LE.D2.AND.D3.LE.D1) IS(1)=I3
      IS(2:4)=1
      JS(:)=1
      RW(1)=1
      RW(2:4)=0.
      ITOUT=ITRI

    ENDIF
  END SUBROUTINE IS_IN_UNGRID2
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Calculate gradients at a point via its connection.
  !>
  !> @details Using linear shape function this is a basis on which
  !>  all advection schemes in Roland (2008) are checked.
  !>
  !> @param[in]  PARAM  Depth or current field (indices 0 to NSEA).
  !> @param[out] DIFFX  X gradient (indices 1 to NX).
  !> @param[out] DIFFY  Y gradient (indices 1 to NY).
  !>
  !> @author F. Ardhuin
  !> @author A. Roland
  !> @date   14-Oct-2013
  !>
  SUBROUTINE UG_GRADIENTS (PARAM, DIFFX, DIFFY)
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |           F. Ardhuin              |
    !/                  |           A. Roland               |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :          14-Oct-2013|
    !/                  +-----------------------------------+
    !/
    !/    15-Nov-2007 : Origination.                        ( version 3.13 )
    !/    31-Oct-2010 : Merging of 4.03 with 3.14-Ifremer   ( version 4.04 )
    !/    08-Nov-2011 : Correction for zero grad. on contour( version 4.04 )
    !/    14-Oct-2013 : Correction  of latitude factor      ( version 4.12 )
    !/    01-Mai-2018 : Using linear shape function for gradients [ version 6.04)
    !/
    !
    !  1. purpose: calculate gradients at a point via its connection.
    !  2. Method : using linear shape function this is a basis on which
    !              all advection schemes in Roland (2008) are checked.
    !
    !  3. Parameters :
    !     PARAM : depth or current field (indices 0 to NSEA)
    !     DIFFX :  x gradient            (indices 1 to NX)
    !     DIFFY :  y gradient            (indices 1 to NX)
    !
    !  4. Subroutines used :
    !
    !  5. Called by :
    !
    !       Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      W3WAVE    Subr.          Actual wind wave routine
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !      This subroutine is adjusted from WWM code (Aaron Roland)
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    USE CONSTANTS
    USE W3GDATMD, ONLY : TRIGP, NTRI, NX, NSEA, MAPFS, CLATIS, &
         MAPSTA, ANGLE, FLAGLL,  IOBP, IEN, TRIA, NSEAL, NTRI
    USE W3ADATMD, ONLY : NSEALM
#ifdef W3_PDLIB
    USE yowElementpool
    use yowNodepool,    only: PDLIB_IEN, PDLIB_TRIA, NPA
    USE yowExchangeModule, only : PDLIB_exchange1Dreal
#endif

    IMPLICIT NONE


    REAL, INTENT(IN)     :: PARAM(0:NSEA)
    REAL, INTENT(OUT)  :: DIFFX(:,:), DIFFY(:,:)

    ! local parameters

    INTEGER              :: VERTICES(3), NI(3), NI_GL(3)
    REAL                 :: TMP1(3), TMP2(3)
    INTEGER              :: I, IX, IE, IE_GL
    REAL                 :: VAR(3), FACT, LATMEAN
    REAL                 :: DIFFXTMP, DIFFYTMP
    REAL                 :: DEDX(3), DEDY(3)
    REAL                 :: DVDXIE, DVDYIE
    REAL                 :: WEI(NX), WEI_LOCAL(NSEAL)
    REAL*8               :: RTMP(NSEAL)

    DIFFX = 0.
    DIFFY = 0.
    !
    IF (FLAGLL) THEN
      FACT=1./(DERA*RADIUS)
    ELSE
      FACT=1.
    END IF

#ifdef W3_PDLIB
    IF (.NOT. LPDLIB) THEN
#endif
      WEI = 0.
      DO IE = 1, NTRI
        NI = TRIGP(:,IE)
        LATMEAN = 1./3. * SUM(CLATIS(MAPFS(1,NI)))
        WEI(NI) = WEI(NI) + 2.*TRIA(IE)
        DEDX(1)     = IEN(IE,1)
        DEDX(2)     = IEN(IE,3)
        DEDX(3)     = IEN(IE,5)
        DEDY(1)     = IEN(IE,2)
        DEDY(2)     = IEN(IE,4)
        DEDY(3)     = IEN(IE,6)
        VAR         = PARAM(MAPFS(1,NI)) * FACT
        DVDXIE      = DOT_PRODUCT( VAR,DEDX)
        DVDYIE      = DOT_PRODUCT( VAR,DEDY)
        DIFFX(1,NI) = DIFFX(1,NI) + DVDXIE * LATMEAN
        DIFFY(1,NI) = DIFFY(1,NI) + DVDYIE
      END DO
      DIFFX(1,:) = DIFFX(1,:)/WEI
      DIFFY(1,:) = DIFFY(1,:)/WEI
#ifdef W3_PDLIB
    ELSE
      WEI_LOCAL = 0.
      DO IE = 1, NE
        NI      = INE(:,IE)
        IE_GL   = IELG(IE)
        NI_GL   = TRIGP(:,IE_GL)
        LATMEAN = 1./3. * SUM(CLATIS(MAPFS(1,NI_GL)))
        WEI_LOCAL(NI) = WEI_LOCAL(NI) + 2.*PDLIB_TRIA(IE)
        DEDX(1)     = PDLIB_IEN(1,IE)
        DEDX(2)     = PDLIB_IEN(3,IE)
        DEDX(3)     = PDLIB_IEN(5,IE)
        DEDY(1)     = PDLIB_IEN(2,IE)
        DEDY(2)     = PDLIB_IEN(4,IE)
        DEDY(3)     = PDLIB_IEN(6,IE)
        VAR         = PARAM(MAPFS(1,NI_GL)) * FACT
        DVDXIE      = DOT_PRODUCT(VAR,DEDX)
        DVDYIE      = DOT_PRODUCT(VAR,DEDY)
        DIFFX(1,NI) = DIFFX(1,NI) + DVDXIE * LATMEAN
        DIFFY(1,NI) = DIFFY(1,NI) + DVDYIE
      END DO
      DIFFX(1,:) = DIFFX(1,:)/WEI_LOCAL
      DIFFY(1,:) = DIFFY(1,:)/WEI_LOCAL
    ENDIF
    CALL PDLIB_exchange1Dreal(DIFFX(1,:))
    CALL PDLIB_exchange1Dreal(DIFFY(1,:))
#endif
    !
  END SUBROUTINE UG_GRADIENTS
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief UGTYPE nesting initialization.
  !>
  !> @param[in]    DISTMIN
  !> @param[inout] FLOK
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE W3NESTUG(DISTMIN,FLOK)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : UGTYPE nesting initialization
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    USE W3ODATMD, ONLY: NBI, NDSE, ISBPI, XBPI, YBPI
    USE W3GDATMD, ONLY: NX, XGRD, YGRD, MAPSTA, MAPFS, MAPSF


    REAL, INTENT(IN)         :: DISTMIN
    LOGICAL, INTENT(INOUT)         :: FLOK

    INTEGER                   :: I, J, JMEMO, IS, IX,  N, IX1(NBI)
    REAL                      :: DIST, DIST0
    !
    N = 0
    !
    !1. look for input boundary point index
    ! warning: if land points are included as boundary points to abide by the nest
    ! file, their status should be -2.
    !
    IX1 = 0
    ISBPI = 1
    DO IX = 1, NX
      IF (ABS(MAPSTA (1,IX)) .EQ. 2) THEN
        N = N + 1
        IF (N.GT.NBI) THEN
          WRITE(NDSE,*) 'Error: boundary node index > NBI ... nest.ww3 file is not consistent with mod_def.ww3'
          STOP
        ENDIF
        IX1(N) = IX
#ifdef W3_T
        WRITE(NDSE ,*)'ADDING BOUNDARY POINT:',N,IX
#endif
      END IF
    END DO
    !
    !2. Matches the model grid points (where MAPSTA = 2) with the points in nest.ww3
    !   For this, we use the nearest point in the nest file.
    !
    DO I = 1, NBI
      DIST0 = HUGE(1.)
      IS = 1
      DO J = 1, N
        DIST = (XBPI(I) - XGRD(1,IX1(J)))**2 + (YBPI(I) - YGRD(1,IX1(J)))**2
        IF (DIST.LT.DIST0) THEN
          IS = MAPFS(1,IX1(J))
          DIST0 = DIST
          JMEMO = J
        END IF
      END DO

      DIST0 = SQRT(DIST0)
      IF (DIST0.LE.DISTMIN) THEN
        ISBPI(I) = IS
#ifdef W3_T
        WRITE(NDSE ,'(A,I6,A,I7,A,I6)') 'MATCHED BOUNDARY POINT:',I,'GRID POINT:', &
             MAPSF(IS,1),'INDEX IN nest.ww3:', JMEMO
#endif
      ELSE
        FLOK=.TRUE.
      END IF

    END DO

    IF ( N .NE. NBI) THEN
      WRITE(NDSE ,900) N, NBI
      DO J=1,N
        WRITE(6,*) 'THIS POINT HAS MAPSTA=2:',ISBPI(J)
      END DO
      ISBPI(N+1:NBI)=ISBPI(1)
    END IF

900 FORMAT (/' *** WAVEWATCH III ERROR IN W3IOBC : '/                &
         '     NUMBER OF MAPSTA=2 DIFFERS FROM NUMBER IN nest.ww3    '/                &
         '     CHECK nest.ww3 AND ww3_grid.inp ',2I8/)
  END SUBROUTINE W3NESTUG


  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Setup boundary pointer.
  !>
  !> @param[inout] MASK
  !> @param[inout] STATUS
  !>
  !> @author Aron Roland
  !> @author Mathiew Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE SET_IOBP (MASK, STATUS)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : setup boundary pointer
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    !/
    !
    USE CONSTANTS
    !
    !
    USE W3GDATMD, ONLY: NX, NTRI, TRIGP
    USE W3ODATMD, ONLY: IAPROC


    IMPLICIT NONE

    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    INTEGER, INTENT(IN)   :: MASK(NX)
    INTEGER*2, INTENT(OUT)  :: STATUS(NX)
    !
    INTEGER :: COLLECTED(NX), NEXTVERT(NX), PREVVERT(NX)
    INTEGER          :: ISFINISHED !, INEXT, IPREV
    INTEGER :: INEXT(3), IPREV(3)
    INTEGER          :: ZNEXT, IP, I, IE, IPNEXT, IPPREV, COUNT
    integer nb0, nb1, nbM1
    STATUS = -1
    INEXT=(/ 2, 3, 1 /) !IPREV=1+MOD(I+1,3)
    IPREV=(/ 3, 1, 2 /) !INEXT=1+MOD(I,3)
    DO IE=1,NTRI
      ! If one of the points of the triangle is masked out (land) then do as if triangle does not exist...
      !        IF ((MASK(TRIGP(1,IE)).GT.0).AND.(MASK(TRIGP(2,IE)).GT.0).AND.(MASK(TRIGP(3,IE)).GT.0)) THEN
      DO I=1,3
        IP=TRIGP(I,IE)
        CALL TRIANG_INDEXES(I, IPNEXT, IPPREV)
        !IPNEXT=TRIGP(INEXT(I),IE)
        !IPPREV=TRIGP(IPREV(I),IE)
        IF (STATUS(IP).EQ.-1) THEN
          STATUS(IP)=1
          PREVVERT(IP)=IPPREV
          NEXTVERT(IP)=IPNEXT
        END IF
      END DO
      !        ENDIF
    END DO
    STATUS(:)=-1
    !
    COUNT = 0
    DO
      COUNT = COUNT + 1
      COLLECTED(:)=0
      DO IE=1,NTRI
        !        IF ((MASK(TRIGP(1,IE)).GT.0).AND.(MASK(TRIGP(2,IE)).GT.0).AND.(MASK(TRIGP(3,IE)).GT.0)) THEN
        DO I=1,3
          IP=TRIGP(I,IE)
          CALL TRIANG_INDEXES(I, IPNEXT, IPPREV)
          !IPNEXT=TRIGP(INEXT(I),IE)
          !IPPREV=TRIGP(IPREV(I),IE)
          IF (STATUS(IP).EQ.-1) THEN
            ZNEXT=NEXTVERT(IP)
            IF (ZNEXT.EQ.IPPREV) THEN
              COLLECTED(IP)=1
              NEXTVERT(IP)=IPNEXT
              IF (NEXTVERT(IP).EQ.PREVVERT(IP)) THEN
                STATUS(IP)=1
              END IF
            END IF
          END IF
        END DO
        !            END IF ! end of test on MASK
      END DO
      !
      ! Checks that all nodes have been treated ...
      !
      ISFINISHED=1
      DO IP=1,NX
        IF (MASK(IP).LE.0) THEN
          STATUS(IP)=0
        ELSE
          IF ((COLLECTED(IP).EQ.0).AND.(STATUS(IP).EQ.-1)) THEN
            STATUS(IP)=0
          END IF
          IF (STATUS(IP).eq.-1) THEN
            ISFINISHED=0
          END IF
        ENDIF
      END DO
      IF (ISFINISHED.EQ.1) THEN
        EXIT
      END IF
    END DO

    STATUS = 1
    CALL GET_BOUNDARY(NX, NTRI, TRIGP, STATUS, PREVVERT, NEXTVERT)

    !#ifdef MPI_PARALL_GRID
    !      CALL exchange_p2di(STATUS)
    !#endif
  END SUBROUTINE SET_IOBP
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Find boundary points.
  !>
  !> @param[in]    MNP
  !> @param[in]    MNE
  !> @param[in]    TRIGP
  !> @param[inout] IOBP
  !> @param[inout] NEIGHBOR_PREV
  !> @param[inout] NEIGHBOR_NEXT
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE GET_BOUNDARY(MNP, MNE, TRIGP, IOBP, NEIGHBOR_PREV,       &
       &   NEIGHBOR_NEXT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : find boundary points
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    !
    USE W3SERVMD, ONLY: EXTCDE
    IMPLICIT NONE
    !/
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif

    INTEGER, INTENT(IN)             :: MNP, MNE, TRIGP(3,MNE)
    INTEGER*2, INTENT(INOUT)        :: IOBP(MNP)
    INTEGER, INTENT(INOUT)          :: NEIGHBOR_PREV(MNP)
    INTEGER, INTENT(INOUT)          :: NEIGHBOR_NEXT(MNP)

    INTEGER, POINTER :: STATUS(:)
    INTEGER, POINTER :: COLLECTED(:)
    INTEGER, POINTER :: NEXTVERT(:)
    INTEGER, POINTER :: PREVVERT(:)

    INTEGER :: IE, I, IP, IP2, IP3
    INTEGER :: ISFINISHED, INEXT, IPREV, ISTAT
    INTEGER :: IPNEXT, IPPREV, ZNEXT, ZPREV
#ifdef W3_S
    CALL STRACE (IENT, 'GET_BOUNDARY')
#endif
    ALLOCATE(STATUS(MNP), STAT=ISTAT)
    CHECK_ALLOC_STATUS ( ISTAT )
    ALLOCATE(COLLECTED(MNP), STAT=ISTAT)
    CHECK_ALLOC_STATUS ( ISTAT )
    ALLOCATE(PREVVERT(MNP), STAT=ISTAT)
    CHECK_ALLOC_STATUS ( ISTAT )
    ALLOCATE(NEXTVERT(MNP), STAT=ISTAT)
    CHECK_ALLOC_STATUS ( ISTAT )
    NEIGHBOR_NEXT = 0
    NEIGHBOR_PREV = 0
    !  Now computing the next items
    STATUS = 0
    NEXTVERT = 0
    PREVVERT = 0

    DO IE=1,MNE
      DO I=1,3
        CALL TRIANG_INDEXES(I, INEXT, IPREV)
        IP=TRIGP(I,IE)
        IPNEXT=TRIGP(INEXT,IE)
        IPPREV=TRIGP(IPREV,IE)
        IF (STATUS(IP).EQ.0) THEN
          STATUS(IP)=1
          PREVVERT(IP)=IPPREV
          NEXTVERT(IP)=IPNEXT
        END IF
      END DO
    END DO
    STATUS(:)=0
    DO
      COLLECTED(:)=0
      DO IE=1,MNE
        DO I=1,3
          CALL TRIANG_INDEXES(I, INEXT, IPREV)
          IP=TRIGP(I,IE)
          IPNEXT=TRIGP(INEXT,IE)
          IPPREV=TRIGP(IPREV,IE)
          IF (STATUS(IP).EQ.0) THEN
            ZNEXT=NEXTVERT(IP)
            IF (ZNEXT.EQ.IPPREV) THEN
              COLLECTED(IP)=1
              NEXTVERT(IP)=IPNEXT
              IF (NEXTVERT(IP).EQ.PREVVERT(IP)) THEN
                STATUS(IP)=1
              END IF
            END IF
          END IF
        END DO
      END DO

      ISFINISHED=1
      DO IP=1,MNP
        IF ((COLLECTED(IP).EQ.0).AND.(STATUS(IP).EQ.0)) THEN
          STATUS(IP)=-1
          NEIGHBOR_NEXT(IP)=NEXTVERT(IP)
        END IF
        IF (STATUS(IP).EQ.0) THEN
          ISFINISHED=0
        END IF
      END DO
      IF (ISFINISHED.EQ.1) THEN
        EXIT
      END IF
    END DO

    !  Now computing the prev items
    STATUS = 0
    NEXTVERT = 0
    PREVVERT = 0
    DO IE=1,MNE
      DO I=1,3
        CALL TRIANG_INDEXES(I, INEXT, IPREV)
        IP=TRIGP(I,IE)
        IPNEXT=TRIGP(INEXT,IE)
        IPPREV=TRIGP(IPREV,IE)
        IF (STATUS(IP).EQ.0) THEN
          STATUS(IP)=1
          PREVVERT(IP)=IPPREV
          NEXTVERT(IP)=IPNEXT
        END IF
      END DO
    END DO
    STATUS(:)=0
    DO
      COLLECTED(:)=0
      DO IE=1,MNE
        DO I=1,3
          CALL TRIANG_INDEXES(I, INEXT, IPREV)
          IP=TRIGP(I,IE)
          IPNEXT=TRIGP(INEXT,IE)
          IPPREV=TRIGP(IPREV,IE)
          IF (STATUS(IP).EQ.0) THEN
            ZPREV=PREVVERT(IP)
            IF (ZPREV.EQ.IPNEXT) THEN
              COLLECTED(IP)=1
              PREVVERT(IP)=IPPREV
              IF (PREVVERT(IP).EQ.NEXTVERT(IP)) THEN
                STATUS(IP)=1
              END IF
            END IF
          END IF
        END DO
      END DO

      ISFINISHED=1
      DO IP=1,MNP
        IF ((COLLECTED(IP).EQ.0).AND.(STATUS(IP).EQ.0)) THEN
          STATUS(IP)=-1
          NEIGHBOR_PREV(IP)=PREVVERT(IP)     ! new code
        END IF
        IF (STATUS(IP).EQ.0) THEN
          ISFINISHED=0
        END IF
      END DO
      IF (ISFINISHED.EQ.1) THEN
        EXIT
      END IF
    END DO
    !  Now making checks
    DO IP=1,MNP
      IP2=NEIGHBOR_NEXT(IP)
      IF (IP2.GT.0) THEN
        IP3=NEIGHBOR_PREV(IP2)
        IF (ABS(IP3 - IP).GT.0) THEN
          WRITE(*,*) 'IP=', IP, ' IP2=', IP2, ' IP3=', IP3
          WRITE(*,*) 'We have a dramatic inconsistency'
          STOP
        END IF
      END IF
    END DO
    !   Now assigning the boundary IOBP array
    DO IP=1,MNP
      IF (STATUS(IP).EQ.-1 .AND. IOBP(IP) .EQ. 1) THEN
        IOBP(IP)=0
      END IF
    END DO

    DEALLOCATE(STATUS, STAT=ISTAT)
    CHECK_DEALLOC_STATUS ( ISTAT )
    DEALLOCATE(COLLECTED, STAT=ISTAT)
    CHECK_DEALLOC_STATUS ( ISTAT )
    DEALLOCATE(NEXTVERT, STAT=ISTAT)
    CHECK_DEALLOC_STATUS ( ISTAT )
    DEALLOCATE(PREVVERT, STAT=ISTAT)
    CHECK_DEALLOC_STATUS ( ISTAT )

  END SUBROUTINE GET_BOUNDARY

  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Set indices of the triangle.
  !>
  !> @param[in]  I
  !> @param[out] INEXT
  !> @param[out] IPREV
  !>
  !> @author Aron Roland
  !> @author Mathieu Dutour-Sikiric
  !> @date   01-Jun-2018
  !>
  SUBROUTINE TRIANG_INDEXES(I, INEXT, IPREV)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |                                   |
    !/                  | Aron Roland (BGS IT&E GmbH)       |
    !/                  | Mathieu Dutour-Sikiric (IRB)      |
    !/                  |                                   |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :        01-June-2018 |
    !/                  +-----------------------------------+
    !/
    !/    01-June-2018 : Origination.                        ( version 6.04 )
    !/
    !  1. Purpose : set indices of the triangle
    !  2. Method :
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      STRACE    Subr. W3SERVMD Subroutine tracing.
    !     ----------------------------------------------------------------
    !
    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !  7. Remarks
    !  8. Structure :
    !  9. Switches :
    !
    !     !/S  Enable subroutine tracing.
    !
    ! 10. Source code :
    !
    !/ ------------------------------------------------------------------- /
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif
    IMPLICIT NONE
    !
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local PARAMETERs
    !/
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !/
    INTEGER, INTENT(IN)  :: I
    INTEGER, INTENT(OUT) :: INEXT, IPREV
#ifdef W3_S
    CALL STRACE (IENT, 'TRIANG_INDEXES')
#endif
    IF (I.EQ.1) THEN
      INEXT=3
    ELSE
      INEXT=I-1
    END IF
    IF (I.EQ.3) THEN
      IPREV=1
    ELSE
      IPREV=I+1
    END IF
  END SUBROUTINE TRIANG_INDEXES

  !/ ------------------------------------------------------------------- /
  
  !>
  !> @brief Redefines the values of the boundary points and angle pointers
  !>  based on the MAPSTA array.
  !>
  !> @details Adapted boundary detection from A. Roland and M. Dutour (WWM code).
  !>
  !> @author Fabrice Ardhuin
  !> @author Aron Roland
  !> @date   17-Apr-2016
  !>
  SUBROUTINE SET_UG_IOBP()
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        Fabrice Ardhuin            |
    !/                  |        Aron Roland                |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         17-Apr-2016 |
    !/                  +-----------------------------------+
    !/
    !/    23-Aug-2011 : Origination.                        ( version 4.04 )
    !/    17-Apr-2016 : Uses optimized boundary detection   ( version 5.10 )
    !/
    !  1. Purpose :
    !
    !     Redefines the values of the boundary points and angle pointers
    !     based on the MAPSTA array
    !
    !  2. Method :
    !
    !     Adapted boundary detection from A. Roland and M. Dutour (WWM code)
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !     Local variables.
    !     ----------------------------------------------------------------
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !

    !  5. Called by :
    !
    !      Name      Type  Module   Description
    !     ----------------------------------------------------------------
    !      WW3_GRID  Prog. WW3_GRID Grid preprocessor
    !      W3ULEV    Subr. W3UPDTMD Water level update
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !
    !  9. Switches :
    !
    !       !/S     Enable subroutine tracing.
    !
    !
    ! 10. Source code :
    !/ ------------------------------------------------------------------- /
    !/
    !
    USE CONSTANTS
    !
    !
    USE W3GDATMD, ONLY: NX, NY, NSEA, MAPFS,                        &
         NK, NTH, DTH, XFR, MAPSTA, COUNTRI,         &
         ECOS, ESIN, IEN, NTRI, TRIGP,               &
         IOBP,IOBPD, IOBPA,                          &
#ifdef W3_REF1
         REFPARS, REFLC, REFLD,                      &
#endif
         ANGLE0, ANGLE

    USE W3ODATMD, ONLY: TBPI0, TBPIN, FLBPI
    USE W3ADATMD, ONLY: CG, CX, CY, ATRNX, ATRNY, ITIME, CFLXYMAX
    USE W3IDATMD, ONLY: FLCUR
    USE W3ODATMD, only : IAPROC
#ifdef W3_S
    USE W3SERVMD, ONLY: STRACE
#endif

    IMPLICIT NONE
    !/ ------------------------------------------------------------------- /
    !/ Parameter list
    !/
    !/
    !/ ------------------------------------------------------------------- /
    !/ Local parameters
    !/
    INTEGER                 :: ITH, IX, I, J, IP, IE, NDIRSUM
    REAL (KIND = 8)         :: COSSUM, SINSUM
    REAL (KIND = 8)         :: DIRMIN, DIRMAX, SHIFT, TEMPO, DIRCOAST
    REAL (KIND = 8)         :: X1, X2, Y1, Y2, DXP1, DXP2, DXP3
    REAL (KIND = 8)         :: DYP1, DYP2, DYP3, eDet1, eDet2, EVX, EVY
    REAL(KIND=8), PARAMETER :: THR    = TINY(1.)
    INTEGER                 :: I1, I2, I3
    INTEGER                 :: ITMP(NX), NEXTVERT(NX), PREVVERT(NX)
    CHARACTER(60) :: FNAME
#ifdef W3_S
    INTEGER, SAVE           :: IENT = 0
#endif
    !/ ------------------------------------------------------------------- /
    !
    ! 1.  Preparations --------------------------------------------------- *
    ! 1.a Set constants
    !
#ifdef W3_S
    CALL STRACE (IENT, 'SETUGIOBP')
#endif
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 2.  Searches for boundary points
    !
    ITMP = MAPSTA(1,:)
    CALL SET_IOBP(ITMP, IOBP)
    FNAME = 'meshbnd.msh'
    CALL READMSH_IOBP(23456,FNAME)
    !
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3. Defines directions pointing into land or sea
    !
    IOBPD(:,:) = 0
    IOBPA(:)   = 0
    !
    DO IP=1,NX
      IF (MAPSTA(1,IP).EQ.2) THEN
        IOBPA(IP) = 1
        IOBP(IP)  = 2
      ENDIF
    END DO

    DO IE = 1,NTRI
      I1   =   TRIGP(1,IE)
      I2   =   TRIGP(2,IE)
      I3   =   TRIGP(3,IE)
      DXP1 =   IEN(IE,6)
      DYP1 = - IEN(IE,5)
      DXP2 =   IEN(IE,2)
      DYP2 = - IEN(IE,1)
      DXP3 =   IEN(IE,4)
      DYP3 = - IEN(IE,3)
      DO ITH=1,NTH
        EVX=ECOS(ITH)
        EVY=ESIN(ITH)
        DO I=1,3
          IF (I.eq.1) THEN
            x1=   DXP1
            y1=   DYP1
            x2= - DXP3
            y2= - DYP3
            IP=   I1
          END IF
          IF (I.eq.2) THEN
            x1 =   DXP2
            y1 =   DYP2
            x2 = - DXP1
            y2 = - DYP1
            IP =   I2
          END IF
          IF (I.eq.3) THEN
            x1 =   DXP3
            y1 =   DYP3
            x2 = - DXP2
            y2 = - DYP2
            IP =   I3
          END IF
          IF (IOBP(IP) .eq. 0) THEN ! physical boundary
            eDet1 = THR-x1*EVY+y1*EVX
            eDet2 = THR+x2*EVY-y2*EVX
            IF ((eDet1.gt.0.).and.(eDet2.gt.0.)) THEN
              ! this is the case of waves going towards the boundary ...
              IOBPD(ITH,IP)=1
            ENDIF
          ELSE ! water ...
            IOBPD(ITH,IP)=1
          END IF
        END DO
      END DO
    END DO
    DO IP = 1, NX
      IF ( IOBPA(IP) .eq. 1 .OR. IOBP(IP) .eq. 3 .OR. IOBP(IP) .eq. 4) IOBPD(:,IP) = 1
    END DO
    !2do: recode for mpi
    !        IF (LBCWA .OR. LBCSP) THEN
    !          IF (.NOT. ANY(IOBP .EQ. 2)) THEN
    !            CALL WWM_ABORT('YOU IMPOSED BOUNDARY CONDITIONS BUT IN THE BOUNDARY FILE ARE NO NODES WITH FLAG = 2')
    !          ENDIF
    !        ENDIF
    !#ifdef MPI_PARALL_GRID
    !      CALL exchange_p2di(IOBWB)
    !      DO ID = 1, MDC
    !        iwild = IOBPD(ID,:)
    !        CALL exchange_p2di(iwild)
    !        IOBPD(ID,:) = iwild
    !      ENDDO
    !#endif
    !--- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ! 3. Updates the reflection direction and sharp / flat shoreline angle

#ifdef W3_REF1
    !
    ! Finds the shoreline direction from IOBPD
    !
    REFLC(1,:)= 0.
    REFLD(:,:)= 1
    DO IP=1,NX
      IF (IOBP(IP).EQ.0.AND.MAPSTA(1,IP).EQ.1) THEN
        COSSUM=0.
        SINSUM=0.
        NDIRSUM=0.
        DO ITH=1,NTH
          COSSUM=COSSUM+IOBPD(ITH,IP)*ECOS(ITH)
          SINSUM=SINSUM+IOBPD(ITH,IP)*ESIN(ITH)
          NDIRSUM=NDIRSUM+IOBPD(ITH,IP)
        END DO
        DIRCOAST=ATAN2(SINSUM, COSSUM)
        REFLD(1,MAPFS(1,IP)) = 1+MOD(NTH+NINT(DIRCOAST/DTH),NTH)
        REFLD(2,MAPFS(1,IP)) = 4-MAX(2,NINT(4.*REAL(NDIRSUM)/REAL(NTH)))
        REFLC(1,MAPFS(1,IP))= REFPARS(1)
      END IF
    END DO
#endif
    !
    ! Recomputes the angles used in the gradients estimation
    !
    !
    RETURN
  END SUBROUTINE SET_UG_IOBP
  !/ ------------------------------------------------------------------- /

  !>
  !> @brief Adjust element longitude coordinates for elements straddling the
  !>  dateline with distance of ~360 degrees.
  !>
  !> @details Detect if element has nodes on both sides of dateline and adjust
  !>  coordinates so that all nodes have the same sign.
  !>
  !> @param[in]  I1
  !> @param[in]  I2
  !> @param[in]  I3
  !> @param[in]  XGRD
  !> @param[in]  YGRD
  !> @param[out] PT
  !>
  !> @author Steven Brus
  !> @author Ali Abdolali
  !> @date   21-May-2020
  !>
  SUBROUTINE FIX_PERIODCITY(I1,I2,I3,XGRD,YGRD,PT)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |        Steven Brus                |
    !/                  |        Ali Abdolali               |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         21-May-2020 |
    !/                  +-----------------------------------+
    !/
    !/    21-May-2020 : Origination.                        ( version 6.07 )
    !/
    !/
    !  1. Purpose :
    !
    !     Adjust element longitude coordinates for elements straddling the
    !     dateline with distance of ~360 degrees
    !
    !  2. Method :
    !
    !     Detect if element has nodes on both sides of dateline and adjust
    !     coordinates so that all nodes have the same sign
    !
    !  3. Parameters :
    !
    !     Parameter list
    !     ----------------------------------------------------------------
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: I1, I2, I3
    DOUBLE PRECISION, INTENT(IN) :: XGRD(:,:), YGRD(:,:)
    REAL*8, INTENT(OUT) :: PT(3,2)
    !     ----------------------------------------------------------------
    !
    !     Local variables.
    !     ----------------------------------------------------------------
    INTEGER :: I
    INTEGER :: R1GT180, R2GT180, R3GT180
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !

    !  5. Called by :
    !
    !      Name         Type  Module   Description
    !     ----------------------------------------------------------------
    !      SPATIAL_GRID Subr. W3TRIAM  Triangle area calculation
    !      NVECTRI      Subr. W3TRIAM  Edge length, angle, normal calcuation
    !      IS_IN_UNGRID Subr. W3TRIAM  Point in element calculation
    !     ----------------------------------------------------------------
    !
    !  6. Error messages :
    !
    !       None.
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    ! 10. Source code :
    !/ ------------------------------------------------------------------- /

    PT(1,1) = XGRD(1,I1)
    PT(1,2) = YGRD(1,I1)
    PT(2,1) = XGRD(1,I2)
    PT(2,2) = YGRD(1,I2)
    PT(3,1) = XGRD(1,I3)
    PT(3,2) = YGRD(1,I3)


    R1GT180 = MERGE(1, 0, ABS(PT(3,1)-PT(2,1)).GT.180)
    R2GT180 = MERGE(1, 0, ABS(PT(1,1)-PT(3,1)).GT.180)
    R3GT180 = MERGE(1, 0, ABS(PT(2,1)-PT(1,1)).GT.180)
    ! if R1GT180+R2GT180+R3GT180 .eq. 0 the element does not cross the dateline
    ! if R1GT180+R2GT180+R3GT180 .eq. 1 the element contains the pole
    ! if R1GT180+R2GT180+R3GT180 .eq. 2 the element crosses the dateline


    IF ( R1GT180 + R2GT180 == 2 ) THEN
      PT(3,1)=PT(3,1)-SIGN(360.0d0,(PT(3,1)-PT(2,1)))
    ELSE IF ( R2GT180 + R3GT180 == 2 ) THEN
      PT(1,1)=PT(1,1)-SIGN(360.0d0,(PT(1,1)-PT(2,1)))
    ELSE IF ( R1GT180 + R3GT180 == 2 ) THEN
      PT(2,1)=PT(2,1)-SIGN(360.0d0,(PT(2,1)-PT(3,1)))
    ENDIF

    RETURN
  END SUBROUTINE FIX_PERIODCITY
END MODULE W3TRIAMD
