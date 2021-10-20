#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3IGCMMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           G. Boutin               |
!/                  |                        FORTRAN 90 |
!/                  | Last update :          Aug-2016   |
!/                  +-----------------------------------+
!/
!/        Aug-2016 : Origination (G. Boutin)         ( version 5.10 )
!/
!/    Copyright 2009-2012 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS. 
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Module used for coupling applications between ice model and WW3 with OASIS3-MCT 
!
!  2. Variables and types :
!
!  3. Subroutines and functions :
!
!      Name                   Type   Scope    Description
!     ----------------------------------------------------------------
!      SND_FIELDS_TO_ICE    Subr.  Public   Send fields to ice model
!      RCV_FIELDS_FROM_ICE Subr.  Public   Receive fields from ice model
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name                 Type    Module     Description
!     ----------------------------------------------------------------
!      CPL_OASIS_SEND       Subr.   W3OACPMD   Send fields
!      CPL_OASIS_RECV       Subr.   W3OACPMD   Receive fields
!     ----------------------------------------------------------------
!
!  5. Remarks
!  6. Switches :
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
!
      IMPLICIT NONE
!
      INCLUDE "mpif.h"
!
      PRIVATE
!
! * Accessibility
      PUBLIC SND_FIELDS_TO_ICE 
      PUBLIC RCV_FIELDS_FROM_ICE
!
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE SND_FIELDS_TO_ICE()
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           G. Boutin               |
!/                  |                        FORTRAN 90 |
!/                  | Last update :            Aug-2016 |
!/                  +-----------------------------------+
!/
!/    Aug-2016 : Origination (G. Boutin)    ( version 5.10 )
!/
!  1. Purpose :
!
!     Send coupling fields to ice model
!
!  2. Method :
!  3. Parameters :
!  4. Subroutines used :
!
!     Name             Type    Module     Description
!     -------------------------------------------------------------------
!     CPL_OASIS_SND    Subr.   W3OACPMD   Send field to ice/ocean model
!     -------------------------------------------------------------------
!
!  5. Called by :
!
!     Name            Type    Module     Description
!     ------------------------------------------------------------------
!     W3WAVE          Subr.   W3WAVEMD   Wave model 
!     ------------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks :
!  8. Structure :
!  9. Switches :
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3OACPMD,  ONLY: ID_OASIS_TIME, IL_NB_SND, SND_FLD, CPL_OASIS_SND
      USE W3GDATMD,  ONLY: NSEAL, NSEA 
      USE W3WDATMD,  ONLY: ICEF
      USE W3ADATMD,  ONLY: TAUICE
      USE W3ODATMD,  ONLY: UNDEF, NAPROC, IAPROC
!
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      REAL(kind=8), DIMENSION(NSEAL,1) :: RLA_OASIS_SND
      INTEGER                          :: IB_DO, NDSO
      LOGICAL                          :: LL_ACTION   
      REAL(kind=8), DIMENSION(NSEAL)   :: TMP
      INTEGER                          :: JSEA, ISEA
!
!----------------------------------------------------------------------
! * Executable part
!
      DO IB_DO = 1, IL_NB_SND

        SELECT CASE(SND_FLD(IB_DO)%CL_FIELD_NAME)

         !
         ! Ice floe diameters (m)
         ! ---------------------------------------------------------------------     
         CASE ('WW3_ICEF')
            TMP(1:NSEAL) = 0.0 
            DO JSEA=1, NSEAL
               ISEA=IAPROC+(JSEA-1)*NAPROC
               IF(ICEF(ISEA) /= UNDEF) TMP(JSEA)=ICEF(ISEA)
            END DO
            RLA_OASIS_SND(:,1) = DBLE(TMP(1:NSEAL))
            CALL CPL_OASIS_SND(IB_DO, ID_OASIS_TIME, RLA_OASIS_SND, LL_ACTION)

         CASE ('WW3_TWIX')
            TMP(1:NSEAL) = 0.0 
            WHERE(TAUICE(1:NSEAL,1) /= UNDEF) TMP(1:NSEAL)=TAUICE(1:NSEAL,1)
            RLA_OASIS_SND(:,1) = DBLE(TMP(1:NSEAL))
            CALL CPL_OASIS_SND(IB_DO, ID_OASIS_TIME, RLA_OASIS_SND, LL_ACTION)

         CASE ('WW3_TWIY')
            TMP(1:NSEAL) = 0.0 
            WHERE(TAUICE(1:NSEAL,2) /= UNDEF) TMP(1:NSEAL)=TAUICE(1:NSEAL,2)
            RLA_OASIS_SND(:,1) = DBLE(TMP(1:NSEAL))
            CALL CPL_OASIS_SND(IB_DO, ID_OASIS_TIME, RLA_OASIS_SND, LL_ACTION)

         END SELECT


      ENDDO
!
!/ ------------------------------------------------------------------- /
      END SUBROUTINE SND_FIELDS_TO_ICE
!/ ------------------------------------------------------------------- /
      SUBROUTINE RCV_FIELDS_FROM_ICE(ID_LCOMM, IDFLD, FXN, FYN, FAN)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           G. Boutin             |
!/                  |                        FORTRAN 90 |
!/                  | Last update :          April-2016 |
!/                  +-----------------------------------+
!/
!/    Aug-2016 : Origination (G. Boutin)  ( version 5.10 )
!/
!  1. Purpose :
!
!     Receive coupling fields from ice model
!
!  2. Method :
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ID_LCOMM          Char.     I     MPI communicator
!     IDFLD             Int.      I     Name of the exchange fields    
!     FXN               Int.     I/O    First exchange field
!     FYN               Int.     I/O    Second exchange field
!     FAN               Int.     I/O    Third exchange field
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!     Name             Type    Module     Description
!     -------------------------------------------------------------------
!     CPL_OASIS_RCV    Subr.   W3OACPMD   Receive fields from ice/ocean model
!     W3S2XY           Subr.   W3SERVMD   Convert from storage (NSEA) to spatial grid (NX, NY)
!     -------------------------------------------------------------------
!
!  5. Called by :
!
!     Name            Type    Module     Description
!     ------------------------------------------------------------------
!     W3FLDG          Subr.   W3FLDSMD   Manage input fields of depth,
!                                        current, wind and ice concentration
!     ------------------------------------------------------------------
!
!  6. Error messages :
!  7. Remarks :
!  8. Structure :
!  9. Switches :
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
      USE W3OACPMD, ONLY: ID_OASIS_TIME, IL_NB_RCV, RCV_FLD, CPL_OASIS_RCV
      USE W3GDATMD, ONLY: NX, NY, NSEAL, NSEA, MAPSF
      USE W3ODATMD, ONLY: NAPROC, IAPROC
      USE W3SERVMD, ONLY: W3S2XY
!
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)              :: ID_LCOMM
      CHARACTER(LEN=3), INTENT(IN)     :: IDFLD
      REAL, INTENT(INOUT)              :: FXN(:,:), FYN(:,:), FAN(:,:)
!
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      LOGICAL                          :: LL_ACTION   
      INTEGER                          :: IB_DO, IB_I, IB_J, IL_ERR, NDSO
      REAL(kind=8), DIMENSION(NSEAL,1) :: RLA_OASIS_RCV
      REAL(kind=8), DIMENSION(NSEAL)   :: TMP
      REAL, DIMENSION(1:NSEA)          :: SND_BUFF,RCV_BUFF
!
!----------------------------------------------------------------------
! * Executable part
!
      RLA_OASIS_RCV(:,:) = 0.0
!
      DO IB_DO = 1, IL_NB_RCV
        IF (IDFLD == 'IC5') THEN
           SELECT CASE (RCV_FLD(IB_DO)%CL_FIELD_NAME)
            !
            ! Ice floe diameters (m)
            ! ----------------------------------------------------------------------
           CASE ('WW3__IC5')

            CALL CPL_OASIS_RCV(IB_DO, ID_OASIS_TIME, RLA_OASIS_RCV, LL_ACTION)
            IF (LL_ACTION) THEN
              TMP(1:NSEAL) = RLA_OASIS_RCV(1:NSEAL,1)
              SND_BUFF(1:NSEA) = 0.0
              DO IB_I = 1, NSEAL
                IB_J = IAPROC + (IB_I-1)*NAPROC
                SND_BUFF(IB_J) = TMP(IB_I)
              END DO
              !
              CALL MPI_ALLREDUCE(SND_BUFF(1:NSEA), &
                                 RCV_BUFF(1:NSEA), &
                                 NSEA,     &
                                 MPI_REAL, &
                                 MPI_SUM,  &
                                 ID_LCOMM, &
                                 IL_ERR)
              !
              ! Convert from storage (NSEA) to spatial grid (NX, NY)
              CALL W3S2XY(NSEA,NSEA,NX,NY,RCV_BUFF(1:NSEA),MAPSF,FAN)
              !
            END IF
           END SELECT   
           !
           ! Ice Concentration 
           ! ----------------------------------------------------------------------
        ELSE IF (IDFLD == 'ICE') THEN  
           SELECT CASE (RCV_FLD(IB_DO)%CL_FIELD_NAME)            
           CASE ('WW3__ICE')
            CALL CPL_OASIS_RCV(IB_DO, ID_OASIS_TIME, RLA_OASIS_RCV, LL_ACTION)
            IF (LL_ACTION) THEN
              TMP(1:NSEAL) = RLA_OASIS_RCV(1:NSEAL,1)
              SND_BUFF(1:NSEA) = 0.0
              DO IB_I = 1, NSEAL
                IB_J = IAPROC + (IB_I-1)*NAPROC
                SND_BUFF(IB_J) = TMP(IB_I)
              END DO
              ! 
              !
              CALL MPI_ALLREDUCE(SND_BUFF(1:NSEA), &
                                 RCV_BUFF(1:NSEA), &
                                 NSEA,     &
                                 MPI_REAL, &
                                 MPI_SUM,  &
                                 ID_LCOMM, &
                                 IL_ERR)
              !
              ! Convert from storage (NSEA) to spatial grid (NX, NY)
              CALL W3S2XY(NSEA,NSEA,NX,NY,RCV_BUFF(1:NSEA),MAPSF,FAN)
              !
            END IF
          END SELECT  
          ! Ice Thickness
          ! ----------------------------------------------------------------------
          ELSE IF (IDFLD == 'IC1') THEN  
           SELECT CASE (RCV_FLD(IB_DO)%CL_FIELD_NAME)
           CASE ('WW3__IC1')
            CALL CPL_OASIS_RCV(IB_DO, ID_OASIS_TIME, RLA_OASIS_RCV, LL_ACTION)
            IF (LL_ACTION) THEN
              TMP(1:NSEAL) = RLA_OASIS_RCV(1:NSEAL,1)
              SND_BUFF(1:NSEA) = 0.0
              DO IB_I = 1, NSEAL
                IB_J = IAPROC + (IB_I-1)*NAPROC
                SND_BUFF(IB_J) = TMP(IB_I)
              END DO

              CALL MPI_ALLREDUCE(SND_BUFF(1:NSEA), &
                                 RCV_BUFF(1:NSEA), &
                                 NSEA,     &
                                 MPI_REAL, &
                                 MPI_SUM,  &
                                 ID_LCOMM, &
                                 IL_ERR)
              !
              ! Convert from storage (NSEA) to spatial grid (NX, NY)
              CALL W3S2XY(NSEA,NSEA,NX,NY,RCV_BUFF(1:NSEA),MAPSF,FAN)
            ENDIF
           !
           END SELECT    
       
        END IF
      END DO
!

!/ ------------------------------------------------------------------- /
      END SUBROUTINE RCV_FIELDS_FROM_ICE
!/ ------------------------------------------------------------------- /
!/
      END MODULE W3IGCMMD
!/
!/ ------------------------------------------------------------------- /

