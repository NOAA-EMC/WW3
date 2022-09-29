!> @file
!> @brief Contains the program W3UPRSTR.
!>
!> @author Stelios Flampouris @date 16-Feb-2017

#include "w3macros.h"
!/ ------------------------------------------------------------------- /
!>
!> @brief Update restart files based on Hs from DA.
!>
!> @details Update the WAVEWATCH III restart files based on the significant
!>  wave height analysis from any data assimilation system.
!>
!>  The W3UPRSTR is the intermediator between the background WW3
!>  and the analysis of the wave field, it modifies the original restart
!>  file according to the analysis.
!>  For the wave modeling and DA, the ww3_uprstr program applies the
!>  operator from the diagnostic to the prognostic variable.
!>
!> @author Stelios Flampouris @date 16-Feb-2017
!>
PROGRAM W3UPRSTR
  !/
  !/                  +-----------------------------------+
  !/                  | WAVEWATCH III           NOAA/NCEP |
  !/                  |         Stelios Flampouris        |
  !/                  |                        FORTRAN 90 |
  !/                  | First version:        16-Feb-2017 |
  !/                  +-----------------------------------+
  !/
  !/    16-Feb-2017 :  Original Code                       ( version 6.03 )
  !/    07-Jun-2017 :  Change of the Core
  !/    07-Jul-2017 :  Clean the code, add some flexibility, etc
  !/    04-Sep-2017 :  Simplified the code, take out a significant part of the
  !/                   flexibility (The code is still available at SVN/UpRest)
  !/    15-Sep-2017 :  Version 0.65                        ( version 6.03 )
  !/    01-Oct-2018 :  Fixes to preserve spectral energy correctly
  !/                   (Andy Saulter)                      ( version 6.06 )
  !/    17-Oct-2018 :  Version 0.95                        ( version 6.06 )
  !/                   Simplified the code, remove some user unfriendly
  !/                   options, add reg test ta1, add logical checks,
  !/                   unified the operator, add/update the documentation.
  !/    05-Oct-2019 :  Added UPD5 and UPD6 options, plus logic for running
  !/                   with SMC grids (Andy Saulter)       ( version 6.07 )
  !/    01-Nov-2019 :  UPD5 and UPD6 use wind data either from anl.XXX file
  !/                   or from restart under WRST switch (Andy Saulter)
  !/    06-Oct-2020 :  Added namelist input options        ( version 7.11 )
  !/    06-May-2021 :  Use SMCTYPE and FSWND for SMC grid. ( version 7.13 )
  !/
  !/    Copyright 2010 National Weather Service (NWS),
  !/    National Oceanic and Atmospheric Administration.  All rights
  !/    reserved.  WAVEWATCH III is a trademark of the NWS.
  !/    No unauthorized use without permission.
  !/
  !  1. Purpose :
  !
  !     Update the WAVEWATCH III restart files based on the significant
  !     wave height analysis from any data assimilation system.
  !
  !  2. Method :
  !
  !  2.1. General:
  !     The W3UPRSTR is the intermediator between the background WW3
  !     and the analysis of the wave field, it modifies the original restart
  !     file according to the analysis.
  !     For the wave modeling and DA, the ww3_uprstr program applies the
  !     operator from the diagnostic to the prognostic variable.
  !
  !     See the chart below:
  !
  !                       +-------------------+
  !                       | WW3 Background Run|
  !                       +-------------------+
  !    +--------------+              |                   +-----+
  !    | Restart File | <------------|-----------------> |  Hs |
  !    +--------------+                                  +-----+
  !            |                                            |
  !            |                                            |
  !            |               +---------------+         +-----+
  !            |               |Hs Observations|-------> | D.A.|
  !            |               +---------------+         +-----+
  !            |                                            |
  !            |                  +----------+              |
  !            +----------------> | W3UPRSTR |<-/Analysis/--+
  !                               +----------+
  !                                     |
  !                          +----------------------+
  !                          | Updated Restart File |
  !                          +----------------------+
  !
  !     A. The WW3 Background Run has to provide two files:
  !         i. The field of Hs (for NCEP at grib2 format) and
  !        ii. the restart.ww3, at the WW3 format for restart files.
  !     Both of them, at the moment of the assimilation (Nevertheless, the WW3
  !     restart reader will fail when the timestamps are not identical).
  !
  !     B. The DA module produces the analysis and/or the difference (%) of
  !     the analysis from the first guess of Hs in the space of model and
  !     exports the results.
  !
  !     C. The algorithm
  !     The Hs correction is redistributed to each frequency and direction.
  !
  !     1. The W3UPRSTR imports: i. the restart.ww3,
  !                             ii. the analysis file and
  !                            iii. the input file: ww3_uprstr.inp, details at 2.2.2.i.
  !
  !     2. The W3UPRSTR updates the restart file according to the option at
  !     ww3_uprstr UPD[N]
  !     Note: With the version 6.06 some options have been removed, but the naming
  !     is consistent with the original version.
  !
  !     3. W3UPRSTR exports the updated spectrum in the same format as the
  !     restart.ww3. The name of the output file is: restart001.ww3 and it has to
  !     be renamed "restart.ww3" for the initialization of the next prediction
  !     cycle.
  !
  !     E. The user runs WW3 with the analysis restart file.
  !
  !     2.2. How to use ww3_uprstr
  !     The ww3_uperstr is one of the WW3 auxilary programs, therefore it works in
  !     a very similar way as the other auxilary programs.
  !
  !     A. To compile:
  !
  !     ww3_uprstr is included in the make_makefile.sh, to compile:
  !     $ ./w3_make ww3_uprstr
  !     or
  !     $ ./w3_make
  !
  !     And the executable "ww3_uprstr" will appear at [...]/model/exe/
  !
  !     B. To run:
  !     At the computational path:
  !        > ${EXE}/ww3_uprstr
  !     And it should run if the input files are at ./
  !
  !     C. Input Files:
  !
  !        i. ww3_uprstr.inp
  !     It includes some limited information for running the program:
  !
  ! -------------------------------------------------------------------- $
  ! WAVEWATCH III Update Restart input file                              $
  ! -------------------------------------------------------------------- $
  !
  ! Time of Assimilation ----------------------------------------------- $
  ! - Starting time in yyyymmdd hhmmss format.
  !
  ! This is the assimilation starting time and has to be the same with
  ! the time at the restart.ww3.
  !
  !   19680607 120000
  !
  ! Choose algorithm to update restart file
  !  UPDN for the Nth approach
  !  The UPDN*, with N<2 the same correction factor is applied at all the grid points
  !   UPD0C:: ELIMINATED
  !   UPDOF:: Option 0F  All the spectra are updated with a constant
  !           fac=HsAnl/HsBckg.
  !           Expected input: PRCNTG, as defined at fac
  !   UPD1 :: ELIMINATED
  !   UPDN, with N>1 each gridpoint has its own update factor.
  !   UPD2 :: Option 2   The fac(x,y,frq,theta), is calculated at each grid point
  !           according to the ratio of HsBckg and HsAnl (squared to preseve energy)
  !           Expected input: the Analysis field, grbtxt format
  !   UPD3 :: Option 3   The update factor is a surface with the shape of
  !           the background spectrum.
  !           Expected input: the Analysis field, grbtxt format
  !   UPD4 :: [NOT INCLUDED in this Version, Just keeping the spot]
  !           Option 4  The generalization of the UPD3. The update factor
  !           is the sum of surfaces which are applied on the background
  !           spectrum.
  !           The algorithm requires the mapping of each partition on the
  !           individual spectra; the map is used to determine the weighting
  !           surfaces.
  !           Expected input: the Analysis field, grbtxt format and the
  !           functions(frq,theta) of the update to be applied.
  !   UPD5 :: Option 5   Corrections are calculated as per UPD2 but are
  !           applied to wind-sea parts of the spectrum only when wind-sea
  !           is the dominant component, otherwise the whole spectrum is
  !           corrected
  !           Expected input: the Analysis Hs field plus background wind speed
  !                           and direction
  !   UPD6 :: Option 6   Corrections are calculated as per UPD5 but wind-sea
  !           components are also shifted in frequency space using Toba (1973)
  !           Expected input: the Analysis Hs field plus background wind speed
  !                           and direction
  !
  ! PRCNTG is input for option UPD0F and is the correction factor
  ! applied  to all the gridpoints (e.g. 1.)
  !
  !   0.475
  !
  ! PRCNTG_CAP is global input for option UPD2 and UPD3 and it is a cap on
  ! the maximum SWH correction factor applied to all the gridpoints, as
  ! both a multiple or divisor (e.g. cap at 5.0 means SWHANL/SWHBKG<=5.0
  ! and SWHANL/SWHBKG>=0.2). The value given should not be less than 1.0
  !
  !  5.0
  !
  ! Name of the file with the SWH analysis from the DA system            $
  ! suffix .grbtxt for text out of grib2 file.                           $
  !
  !   anl.grbtxt
  !
  ! -------------------------------------------------------------------- $
  ! WAVEWATCH III EoF ww3_uprstr.inp
  ! -------------------------------------------------------------------- $
  !
  !   ii. Data files anl.XXX
  !
  ! FOR UPD2,3 and UPD5,6 with WRST switch
  ! USE THE grbtxt FORMAT, See Format E.
  !
  ! Format E.
  !        Text file created by wgrib2. This format is tested more extensively
  !        and currently the only format supported for anl.grbtxt.
  !
  !        NX NY
  !        VAL0001
  !        VAL0002
  !        ...
  !        VALNX*NY
  !
  ! IMPORTANT : All the regtests are with the format E. strongly recommended.
  ! The order of the values in .grbtxt, is assumed the same by
  ! default as the order of spectral data in the restart file.
  !
  ! NOTE: It is recommended to use UPD5,6 with the WRST switch enabled and
  ! using SWH analysis data only as per Format E. However, the code includes
  ! an option to run using a text file in which case:
  ! USE THE grbtxtws format below
  !
  ! Text file with following lines:
  !        NX NY
  !        SWH0001 WSPD0001 WDIR0001
  !        SWH0002 WSPD0002 WDIR0002
  !        ...
  !        SWHNX*NY WSPDNX*NY WDIRNX*NY
  !
  ! The order of the values in .grbtxt, is assumed the same by
  ! default as the order of spectral data in the restart file.
  ! Wind speeds and directions in the anl.XXX file are assumed to be
  ! in CARTESIAN (GRID U,V) CONVENTION
  !
  ! NOTE About Format: if you prefer a different format; there are several
  ! I/O subroutines ready, not included in the current version of the code,
  ! contact the prgmr to get access to the source code.
  !
  !        iii. restart.ww3
  ! The restart file as came out of the background run, the name has to be
  ! restart.ww3, but the name of the output depends on the mod_def.ww3, the
  ! ww3_uprstr follows its content (be careful with ovewriting).
  !
  !  3. Example
  !     Use the regression tests ww3_ta1
  !
  !  4. Parameters :
  !
  !     Local parameters.
  !     ----------------------------------------------------------------
  !
  !     ----------------------------------------------------------------
  !
  !  5. Subroutines used :
  !
  !      Name      Type  Module   Description
  !     ----------------------------------------------------------------
  !      W3NMOD    Subr. W3GDATMD Set number of model.
  !      W3SETG    Subr.   Id.    Point to selected model.
  !      W3NDAT    Subr. W3WDATMD Set number of model for wave data.
  !      W3SETW    Subr.   Id.    Point to selected model for wave data.
  !      W3NINP    Subr. W3IDATMD Set number of grids/models.
  !      W3SETI    Subr.   Id.    Point to data structure.
  !      W3DIMI    Subr.   Id.    Set array sizes in data structure.
  !      W2NAUX    Subr. W3ADATMD Set number of model for aux data.
  !      W3SETA    Subr.   Id.    Point to selected model for aux data.
  !      ITRACE    Subr. W3SERVMD Subroutine tracing initialization.
  !      NEXTLN    Subr.   Id.    Get next line from input file.
  !      EXTCDE    Subr.   Id.    Abort program as graceful as possible.
  !      W3IOGR    Subr. W3IOGRMD Reading/writing model definition file.
  !      WAVNU1    Subr. W3DISPMD

  !     ----------------------------------------------------------------
  !  Internal Subroutines:
  !
  !   READ_GRBTXT
  !   WORKER
  !   SWH_RSRT_1p
  !   WRITEMATRIX
  !
  !  6. Called by :
  !
  !     None, stand-alone program.
  !
  !  7. Error messages :
  !
  !  8. Remarks:
  !
  !     7.1 Use the grbtxt format for correction and analysis files.
  !
  !     7.2 There are some variables not used but declared, it's for future
  !      development.
  !
  !  9. Structure :
  !
  !     ----------------------------------------------------
  !     1.    Set up data structures.
  !     2.    Read model defintion file with base model ( W3IOGR )
  !     3.    Import restart file                       ( W3IORS )
  !     4.    Import correction percentage              (  )
  !        OR Import the analysis field                 (  )
  !     5.    Apply correction to the restart file      (  )
  !     6.    Export the updated restart file           ( W3IORS )
  !     ----------------------------------------------------
  !
  !  10. Switches :
  !
  !     !/SHRD  Switch for shared / distributed memory architecture.
  !     !/T
  !     !/S     Enable subroutine tracing.
  !
  ! 11. Known Bugs
  !
  !     1. Fix the format for the output (NSDO) of non strings, e.g. for
  !     TIME.
  !
  ! 12. Source code :
  !
  !/
  USE W3GDATMD, ONLY: W3NMOD, W3SETG
  USE W3WDATMD, ONLY: W3NDAT, W3SETW
  USE W3ADATMD, ONLY: W3NAUX, W3SETA
  USE W3ODATMD, ONLY: W3NOUT, W3SETO
  USE W3IORSMD, ONLY: W3IORS
  USE W3SERVMD, ONLY: ITRACE, NEXTLN, EXTCDE
  USE W3IOGRMD, ONLY: W3IOGR
  USE W3DISPMD, ONLY: WAVNU1
  !
  USE W3GDATMD, ONLY: GNAME, NX, NY, MAPSTA, SIG, NK, NTH, NSEA,  &
       NSEAL, MAPSF, DMIN, ZB, DSIP, DTH, RSTYPE,  &
       GTYPE, SMCTYPE
#ifdef W3_SMC
  USE W3GDATMD, ONLY: FSWND
#endif
  USE W3WDATMD, ONLY: VA, TIME
  USE W3ADATMD, ONLY: NSEALM
  USE W3ODATMD, ONLY: IAPROC, NAPERR, NAPLOG, NDS, NAPOUT
  USE W3ODATMD, ONLY: NDSE, NDSO, NDST, IDOUT, FNMPRE
#ifdef W3_WRST
  USE W3IDATMD
#endif
  !
  USE W3NMLUPRSTRMD
  !
  IMPLICIT NONE
  !/
  !/ ------------------------------------------------------------------- /
  !  Local variables
  !/
  INTEGER                 :: NDSI, NDSM, NDSTRC, NTRACE, IERR, I, J
  CHARACTER               :: COMSTR*1
  !
  TYPE(NML_RESTART_T)     :: NML_RESTART
  TYPE(NML_UPDATE_T)      :: NML_UPDATE
  !
  !      REAL, ALLOCATABLE       :: BETAW(:)
  !      LOGICAL, ALLOCATABLE    :: MASK(:)
  LOGICAL                 :: anl_exists, CORWSEA, FLGNML
  INTEGER                 :: IMOD,  NDSEN, IX, IY, IK, ITH, &
       IXW, IYW
  REAL, ALLOCATABLE       :: UPDPRCNT(:,:),VATMP(:), HSIG(:,:),     &
       A(:), HS_ANAL(:,:), gues(:,:),         &
       HS_DIF(:,:),SWHANL(:,:), SWHBCKG(:,:), &
       SWHUPRSTR(:,:),VATMP_NORM(:),          &
       WSBCKG(:,:),WDRBCKG(:,:)
  INTEGER, ALLOCATABLE    :: VAMAPWS(:)
  REAL                    :: PRCNTG, PRCNTG_CAP, THRWSEA
  INTEGER                 :: ROWS, COLS, ISEA
  CHARACTER(128)          :: FLNMCOR, FLNMANL
  CHARACTER(16)           :: UPDPROC
  !     for howv
  REAL                    :: SWHTMP,SWHBCKG_1, SWHANL_1,            &
       DEPTH, WN, CG, ETOT, E1I,              &
       SWHTMP1,SUMVATMP, SWHBCKG_W, SWHBCKG_S
  REAL                    :: K
  CHARACTER(8), PARAMETER :: MYNAME='W3UPRSTR'
  LOGICAL                 :: SMCGRD = .FALSE.
  LOGICAL                 :: SMCWND = .FALSE.
  LOGICAL                 :: WRSTON = .FALSE.
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !  1.  IO set-up.
  CALL W3NMOD ( 1, 6, 6 )
  CALL W3SETG ( 1, 6, 6 )
  CALL W3NDAT (    6, 6 )
  CALL W3SETW ( 1, 6, 6 )
  CALL W3NAUX (    6, 6 )
  CALL W3SETA ( 1, 6, 6 )
  CALL W3NOUT (    6, 6 )
  CALL W3SETO ( 1, 6, 6 )
#ifdef W3_WRST
  CALL W3NINP (    6, 6 )
  CALL W3SETI ( 1, 6, 6 )
#endif
  !
  NDSE   = 6
  NDSI   = 10
  NDSM   = 20
  !
  IAPROC = 1
  NAPOUT = 1
  NAPERR = 1
  IMOD   = 1
  NAPLOG = 1
  !
  NDSTRC =  6
  NTRACE = 10
  CALL ITRACE ( NDSTRC, NTRACE )
  !
  IF ( IAPROC .EQ. NAPERR ) THEN
    NDSEN  = NDSE
  ELSE
    NDSEN  = -1
  END IF
  !
  WRITE (NDSO,900)
  !
#ifdef W3_WRST
  !Compiling with WRST will allow access to options UPD5/6
  WRSTON = .TRUE.
  WRITE (NDSO,*) '*** UPRSTR will read wind from restart files'
#endif
  !/
  !/ ------------------------------------------------------------------- /
  !  2. Read the ww3_uprstr input data
  !
  ! process ww3_uprstr namelist
  !
  INQUIRE(FILE=TRIM(FNMPRE)//"ww3_uprstr.nml", EXIST=FLGNML)
  IF (FLGNML) THEN
    ! Read namelist
    CALL W3NMLUPRSTR (NDSI, TRIM(FNMPRE)//'ww3_uprstr.nml', NML_RESTART, &
         NML_UPDATE, IERR)
    READ(NML_RESTART%RESTARTTIME, *) TIME
    UPDPROC = NML_UPDATE%UPDPROC
    PRCNTG = NML_UPDATE%PRCNTG
    PRCNTG_CAP = NML_UPDATE%PRCNTGCAP
    THRWSEA = NML_UPDATE%THRWSEA
    FLNMANL = NML_UPDATE%FILE
  END IF
  !/
  ! otherwise read from the .inp file
  IF (.NOT. FLGNML) THEN
    J      = LEN_TRIM(FNMPRE)
    OPEN (NDSI,FILE=FNMPRE(:J)//'ww3_uprstr.inp',STATUS='OLD',       &
         ERR=800,IOSTAT=IERR)
    READ (NDSI,'(A)',END=801,ERR=802) COMSTR
    IF (COMSTR.EQ.' ') COMSTR = '$'
    WRITE (NDSO,901) COMSTR
    !
    CALL NEXTLN ( COMSTR , NDSI , NDSEN )
    READ (NDSI,*,END=2001,ERR=2002) TIME
    CALL NEXTLN ( COMSTR , NDSI , NDSEN )
    READ (NDSI,*,END=2001,ERR=2002) UPDPROC
    CALL NEXTLN ( COMSTR , NDSI , NDSEN )
    IF (UPDPROC .EQ. 'UPD0F') THEN
      READ (NDSI,*,END=2001,ERR=2002) PRCNTG
    ELSE
      IF ((UPDPROC .EQ. 'UPD2') .OR. (UPDPROC .EQ. 'UPD3')) THEN
        !         CALL NEXTLN ( COMSTR , NDSI , NDSEN )
        READ (NDSI,*,END=2001,ERR=2002) PRCNTG_CAP
#ifdef W3_F
        CALL NEXTLN ( COMSTR , NDSI , NDSEN )
        READ (NDSI,*,END=2001,ERR=2002) FLNMCOR
#endif
      ELSE
        READ (NDSI,*,END=2001,ERR=2002) PRCNTG_CAP, THRWSEA
      END IF
      CALL NEXTLN ( COMSTR , NDSI , NDSEN )
      READ (NDSI,*,END=2001,ERR=2002) FLNMANL
    END IF
  ENDIF
#ifdef W3_T
  WRITE (NDSO,*)' TIME: ',TIME
#endif
  !/
  !/ ------------------------------------------------------------------- /
  !  3.  Read model definition file.
  !/
  CALL W3IOGR ( 'READ', NDSM )
  NSEAL  = NSEA
  WRITE (NDSO,920) GNAME
  !/
#ifdef W3_SMC
  !! SMC grid option is activated if GTYPE .EQ. SMCTYPE.  JGLi06May2021
  IF( GTYPE .EQ. SMCTYPE ) SMCGRD = .TRUE.
  !! SMC sea-point wind option is activated if FSWND=.TRUE.  JGLi06May2021
  IF( FSWND )              SMCWND = .TRUE.
#endif
#ifdef W3_WRST
  ! Override SMCWND - at present restarts only store wind on
  ! a regular grid
  SMCWND = .FALSE.
#endif
#ifdef W3_SMC
  WRITE (NDSO,*) '*** UPRSTR set to work with SMC grid model'
#endif
  !/
  !/ ------------------------------------------------------------------- /
  !  4. Read restart file
  !/
#ifdef W3_WRST
  ! Set the wind flag to true when reading restart wind
  INFLAGS1(3) = .TRUE.
  CALL W3DIMI ( 1, 6, 6 ) !Needs to be called after w3iogr to have correct dimensions?
#endif
  CALL W3IORS ( 'READ', NDS(6), SIG(NK), IMOD )!
  IF ( IAPROC .EQ. NAPLOG ) THEN
    IF (RSTYPE.EQ.0.OR.RSTYPE.EQ.1.OR.RSTYPE.EQ.4) THEN
      WRITE (NDSO,1004) 'Terminating ww3_uprstr: The restart ' // &
           'file is not read'
      CALL EXTCDE ( 1 )
    ELSE
      WRITE (NDSO,1004) '  Updating Restart File'
      WRITE (NDSO,*) ' TIME: ',TIME
    END IF
  END IF
#ifdef W3_T
  WRITE (NDST,*), MYNAME,' : Exporting VA as imported to VA01.txt'
  CALL writeMatrix('VA01.txt', REAL(VA))
#endif
  !/
  !/ ------------------------------------------------------------------- /
  ! 5. Update restart spectra array according to the selected option
  !/
  SELECT CASE (UPDPROC)
    !/
    !/ ------------------------------------------------------------------- /
    ! UPD0F
    !/
  CASE ('UPD0F')
    WRITE (NDSO,902) 'UPD0F'
    WRITE (NDSO,1005) ' PRCNTG = ',PRCNTG
#ifdef W3_T
    ALLOCATE( VATMP  (SIZE(VA    ,1)                ))
    ALLOCATE( SWHANL (SIZE(MAPSTA,1), SIZE(MAPSTA,2)))
    ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)))
#endif
    DO ISEA=1, NSEA, 1
#ifdef W3_T
      IX = MAPSF(ISEA,1)
      IY = MAPSF(ISEA,2)
      VATMP = VA(:,ISEA)
      CALL SWH_RSRT_1p (VATMP, ISEA, SWHBCKG_1)
      SWHBCKG(IY,IX)=SWHBCKG_1
#endif
      CALL UPDATE_VA(PRCNTG, VA(:,ISEA))
#ifdef W3_T
      VATMP = VA(:,ISEA)
      CALL SWH_RSRT_1p (VATMP, ISEA, SWHANL_1)
      SWHANL(IY,IX)=SWHANL_1
      WRITE (NDSO,*) ' =========== UPD0F Output ==========='
      WRITE (NDSO,*)'ISEA = ', ISEA,' PRCNTG = ',PRCNTG,    &
           ' SWHBCKG = ',SWHBCKG(IY,IX),           &
           ' SWHANL= ', SWHANL(IY,IX)
#endif
    END DO
#ifdef W3_T
    CALL writeMatrix('SWHBCKG_UPD0F.txt', REAL(SWHBCKG))
    CALL writeMatrix('SWHANL_UPD0F.txt' , REAL(SWHANL ))
    CALL writeMatrix('SWHRSTR_UPD0F.txt', REAL(SWHANL ))

    DEALLOCATE ( VATMP, SWHBCKG, SWHANL )
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! UPD2
    ! Apply a bulk correction to the wave spectrum at each grid cell based
    ! on the ratio of HsBckg and HsAnl
    !/
  CASE ('UPD2')
    WRITE (NDSO,902) 'UPD2'
    WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
    WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
    !
    !        Array allocation
    ALLOCATE ( VATMP(SIZE(VA,1)))
    IF (.NOT. SMCGRD) THEN
      ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
      ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
#ifdef W3_SMC
    ELSE
      ALLOCATE( SWHBCKG(NSEA,1) )
      ALLOCATE( SWHANL(NSEA,1) )
#endif
    ENDIF
#ifdef W3_T
    IF (.NOT. SMCGRD) THEN
      ALLOCATE( SWHUPRSTR(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
    ELSE
      ALLOCATE( SWHUPRSTR(NSEA,1) )
    ENDIF
#endif
    !
    !        Read additional Input: Analysis Field
    INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
    IF (anl_exists) THEN
#ifdef W3_T
      WRITE (NDSO,*) 'shape(SWHANL)', shape(SWHANL)
#endif
      CALL READ_GRBTXT(SWHANL, FLNMANL, SMCGRD)
#ifdef W3_T
      CALL writeMatrix('SWHANL_IN.txt',SWHANL)
#endif
    ELSE
      WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
      DEALLOCATE( SWHANL,VATMP,SWHBCKG )
#ifdef W3_T
      DEALLOCATE( SWHUPRSTR )
#endif
      STOP
    END IF
    !
    !        Calculation
    DO ISEA=1, NSEA, 1
      IF (.NOT. SMCGRD) THEN
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
#ifdef W3_SMC
      ELSE
        IX = 1
        IY = ISEA
#endif
      ENDIF
      VATMP = VA(:,ISEA)
      CALL SWH_RSRT_1p (VATMP, ISEA, SWHBCKG_1)
      SWHBCKG(IY,IX)=SWHBCKG_1
      !
      IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
        PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
#ifdef W3_T
        WRITE (NDSO,*) 'ISEA = ', ISEA,' IX = ',IX,' IY = ', IY,         &
             ' PRCNTG = ',PRCNTG,' SWHBCKG = ',SWHBCKG(IY,IX), &
             ' SWHANL = ', SWHANL(IY,IX)
#endif
        CALL CHECK_PRCNTG (PRCNTG,PRCNTG_CAP)
        CALL UPDATE_VA(PRCNTG, VA(:,ISEA))
#ifdef W3_T
        CALL SWH_RSRT_1p (VA(:,ISEA), ISEA, SWHUPRSTR(IY,IX))
        WRITE (NDSO,*) ' =========== UPD2 Output ==========='
        WRITE (NDSO,*)'ISEA = ',ISEA,                            &
             'SWH_BCKG = ', SWHBCKG(IY,IX),             &
             'SWH_ANL = ', SWHANL(IY,IX),               &
             'PRCNTG = ', PRCNTG,                       &
             'SWH_RSTR = ',SWHUPRSTR(IY,IX)
#endif
      END IF
    END DO
#ifdef W3_T
    CALL writeMatrix('SWHBCKG_UPD2.txt', REAL(SWHBCKG  ))
    CALL writeMatrix('SWHANL_UPD2.txt' , REAL(SWHANL   ))
    CALL writeMatrix('SWHRSTR_UPD2.txt', REAL(SWHUPRSTR))
#endif
    !
    DEALLOCATE( SWHANL,VATMP,SWHBCKG )
#ifdef W3_T
    DEALLOCATE( SWHUPRSTR )
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! UPD3
    ! As per UPD2, but the update factor is a surface with the shape of the
    ! background spectrum
    !/
  CASE ('UPD3')
    WRITE (NDSO,902) 'UPD3'
    WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
    WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
    !
    !        Array allocation
    ALLOCATE ( VATMP(SIZE(VA,1)))
    ALLOCATE ( VATMP_NORM(SIZE(VA,1)))
    ALLOCATE ( A(SIZE(VA,1)))
    IF (.NOT. SMCGRD) THEN
      ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
      ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
#ifdef W3_SMC
    ELSE
      ALLOCATE( SWHBCKG(NSEA,1) )
      ALLOCATE( SWHANL(NSEA,1) )
#endif
    ENDIF
#ifdef W3_T
    IF (.NOT. SMCGRD) THEN
      ALLOCATE( SWHUPRSTR(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
    ELSE
      ALLOCATE( SWHUPRSTR(NSEA,1) )
    ENDIF
#endif
    !
    !        Read additional Input: Analysis Field
    INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
    IF (anl_exists) THEN
#ifdef W3_T
      WRITE (NDSO,*) 'shape(SWHANL)', shape(SWHANL)
#endif
      CALL READ_GRBTXT(SWHANL, FLNMANL, SMCGRD)
#ifdef W3_T
      CALL writeMatrix('SWHANL_IN.txt',SWHANL)
#endif
    ELSE
      WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
      DEALLOCATE( SWHANL,VATMP,SWHBCKG,VATMP_NORM,A )
#ifdef W3_T
      DEALLOCATE( SWHUPRSTR )
#endif
      STOP
    END IF
    !
    !        Calculation
    DO ISEA=1, NSEA, 1
      IF (.NOT. SMCGRD) THEN
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
#ifdef W3_SMC
      ELSE
        IX = 1
        IY = ISEA
#endif
      ENDIF
      VATMP = VA(:,ISEA)
      CALL SWH_RSRT_1p (VATMP, ISEA, SWHBCKG_1)
      SWHBCKG(IY,IX)=SWHBCKG_1
      !
      IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
        !Step 1.
        PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
#ifdef W3_T
        WRITE (NDSO,*) ' =========== Step 1. ==========='
        WRITE (NDSO,*) ' ISEA = ', ISEA,' IX = ',IX,' IY = ', IY,         &
             ' PRCNTG = ',PRCNTG,' SWHBCKG = ',SWHBCKG(IY,IX), &
             ' SWHANL = ', SWHANL(IY,IX)
#endif
        CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
        VATMP_NORM=VATMP/SUM(VATMP)
#ifdef W3_T
        WRITE (NDSO,*)' ISEA =', ISEA,' IX = ',IX,' IY = ', IY, &
             ' PRCNTG = ',PRCNTG,   &
             ' SWHBCKG = ',SWHBCKG(IY,IX), ' SWHANL = ', SWHANL(IY,IX)
#endif
        IF (PRCNTG > 1.) THEN
          A=PRCNTG**2*(1 + VATMP_NORM)
        ELSE
          A=PRCNTG**2*(1 - VATMP_NORM)
        END IF
        VATMP=A*VATMP
        CALL SWH_RSRT_1p (VATMP, ISEA, SWHTMP)
        PRCNTG=(SWHANL(IY,IX)/SWHTMP)
#ifdef W3_T
        SWHUPRSTR(IY,IX)=SWHTMP
        WRITE (NDSO,*) ' =========== Step 2. ==========='
        WRITE (NDSO,*)'ISEA = ', ISEA, ' PRCNTG = ',PRCNTG,        &
             ' SWHANL= ', SWHANL(IY,IX),                  &
             ' SWHUPRSTR(IY,IX) = ', SWHUPRSTR(IY,IX)
#endif
        CALL CHECK_PRCNTG (PRCNTG,PRCNTG_CAP)
        CALL UPDATE_VA(PRCNTG, VATMP)
        VA(:,ISEA)=VATMP
#ifdef W3_T
        CALL SWH_RSRT_1p (VATMP, ISEA, SWHTMP)
        SWHUPRSTR(IY,IX)=SWHTMP
        WRITE (NDSO,*) ' =========== UPD3 Output ==========='
        WRITE (NDSO,*)'ISEA = ',ISEA,'SWH_BCKG = ', SWHBCKG(IY,IX), &
             'SWH_ANL = ', SWHANL(IY,IX),                 &
             'SWH_RSTR = ',SWHUPRSTR(IY,IX)
#endif
      END IF
    END DO
#ifdef W3_T
    CALL writeMatrix('SWHBCKG_UPD3.txt', REAL(SWHBCKG))
    CALL writeMatrix('SWHANL_UPD3.txt' , REAL(SWHANL ))
    CALL writeMatrix('SWHRSTR_UPD3.txt', REAL(SWHUPRSTR))
#endif
    !
    DEALLOCATE( SWHANL,VATMP,SWHBCKG,VATMP_NORM,A )
#ifdef W3_T
    DEALLOCATE( SWHUPRSTR )
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! UPD5
    ! Corrects wind-sea only in wind dominated conditions - bulk correction
    ! The fac(x,y,frq,theta), is calculated at each grid point according to
    ! HsBckg and HsAnl
    !/
  CASE ('UPD5')
    WRITE (NDSO,902) 'UPD5'
    WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
    WRITE (NDSO,1005) ' THRWSEA = ',THRWSEA
    WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
    ! Presently set hardwired THRWSEA energy threshold here
    ! not user defined in input file
    ! THRWSEA = 0.7
    !
    !        Array allocation
    ALLOCATE ( VATMP(SIZE(VA,1)))
    ALLOCATE ( VAMAPWS(SIZE(VA,1)))
    IF (.NOT. SMCGRD) THEN
      ! SWH arrays allocated using Y,X convention as per wgrib write
      ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
      ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
      ! Wind arrays allocated using X,Y convention as in w3idatmd
      ALLOCATE( WSBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
      ALLOCATE( WDRBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
#ifdef W3_SMC
    ELSE
      ALLOCATE( SWHBCKG(NSEA,1) )
      ALLOCATE( SWHANL(NSEA,1) )
      ! Use SMCWND to determine if reading a seapoint aray for wind
      IF( SMCWND ) THEN
        ALLOCATE( WSBCKG(NSEA,1) )
        ALLOCATE( WDRBCKG(NSEA,1) )
      ELSE
        ALLOCATE(WSBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)))
        ALLOCATE(WDRBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)))
      ENDIF
#endif
    ENDIF
#ifdef W3_T
    IF (.NOT. SMCGRD) THEN
      ALLOCATE( SWHUPRSTR(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
    ELSE
      ALLOCATE( SWHUPRSTR(NSEA,1) )
    ENDIF
#endif
    !
    !        Read additional Input: Analysis Field
    INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
    IF (anl_exists) THEN
#ifdef W3_T
      WRITE (NDSO,*) 'shape(SWHANL)', shape(SWHANL)
#endif
#ifdef W3_WRST
      ! For WRST switch read only corrected SWH
      ! Wind will have been read from the restart
      IF (WRSTON) THEN
        CALL READ_GRBTXT(SWHANL, FLNMANL, SMCGRD)
      ELSE
#endif
        CALL READ_GRBTXTWS(SWHANL,WSBCKG,WDRBCKG,FLNMANL,SMCGRD)
#ifdef W3_WRST
      ENDIF
#endif
#ifdef W3_T
      CALL writeMatrix('SWHANL_IN.txt',SWHANL)
#endif
    ELSE
      WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
      DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
#ifdef W3_T
      DEALLOCATE( SWHUPRSTR )
#endif
      STOP
    END IF
    !
#ifdef W3_WRST
    !Calculate wind speed and direction values from u,v..
    !..using cartesian direction convention
    !At present assume only needed for data read from restart
    CALL UVTOCART(WXNwrst,WYNwrst,WSBCKG,WDRBCKG,SMCWND)
#endif
    !
    !        Calculation
    DO ISEA=1, NSEA, 1
      IF (.NOT. SMCGRD) THEN
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
        IXW = IX
        IYW = IY
#ifdef W3_SMC
      ELSE
        IX = 1
        IY = ISEA
        IF( SMCWND ) THEN
          ! Wind arrays allocated using (X,Y) convention for regular grids
          ! but overriding here for the SMC grid which are always defined
          ! as (NSEA,1) by switching the IY and IX dimension values around
          IXW = IY
          IYW = IX
        ELSE
          IXW = MAPSF(ISEA,1)
          IYW= MAPSF(ISEA,2)
        ENDIF
#endif
      ENDIF
      VATMP = VA(:,ISEA)
      CALL SWH_RSRT_1pw (VATMP, WSBCKG(IXW,IYW), WDRBCKG(IXW,IYW), ISEA, &
           SWHBCKG_1, SWHBCKG_W, SWHBCKG_S, VAMAPWS)
      SWHBCKG(IY,IX)=SWHBCKG_1
      !
      IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
        ! If wind-sea is dominant energy component apply correction to
        ! wind-sea part only
        IF ( (SWHBCKG_W / SWHBCKG_1)**2.0 > THRWSEA ) THEN
          ! Apply spectrum updates to wind-sea bins only
          PRCNTG=SQRT((SWHANL(IY,IX)**2.0-SWHBCKG_S**2.0)/SWHBCKG_W**2.0)
          CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
          CALL UPDTWSPEC(VATMP, PRCNTG, VAMAPWS)
          ! else correct the whole spectrum as for UPD2
        ELSE
          PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
          CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
          CALL UPDATE_VA(PRCNTG,VATMP)
        END IF
#ifdef W3_T
        WRITE (NDSO,*) 'ISEA = ', ISEA,' IX = ',IX,' IY = ', IY,         &
             ' PRCNTG = ',PRCNTG,' SWHBCKG = ',SWHBCKG(IY,IX), &
             ' SWHANL = ', SWHANL(IY,IX)
#endif
        VA(:,ISEA)=VATMP
#ifdef W3_T
        CALL SWH_RSRT_1p (VATMP, ISEA, SWHTMP)
        SWHUPRSTR(IY,IX)=SWHTMP
        WRITE (NDSO,*) ' =========== UPD5 Output ==========='
        WRITE (NDSO,*)'ISEA = ',ISEA,'SWH_BCKG = ', SWHBCKG(IY,IX), &
             'SWH_ANL = ', SWHANL(IY,IX),                 &
             'SWH_RSTR = ',SWHUPRSTR(IY,IX)
#endif
      END IF
    END DO
#ifdef W3_T
    CALL writeMatrix('SWHBCKG_UPD5.txt', REAL(SWHBCKG  ))
    CALL writeMatrix('SWHANL_UPD5.txt' , REAL(SWHANL   ))
    CALL writeMatrix('SWHRSTR_UPD5.txt', REAL(SWHUPRSTR))
#endif
    !
    DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
#ifdef W3_T
    DEALLOCATE( SWHUPRSTR )
#endif
    !/
    !/ ------------------------------------------------------------------- /
    ! UPD6
    ! Hybrid of Lionello et al. and Kohno methods
    ! Corrects wind-sea only in wind dominated conditions - including fp shift
    ! The fac(x,y,frq,theta), is calculated at each grid point according to
    ! HsBckg and HsAnl
    !/
  CASE ('UPD6')
    WRITE (NDSO,902) 'UPD6'
    WRITE (NDSO,1005) ' PRCNTG_CAP = ',PRCNTG_CAP
    WRITE (NDSO,1005) ' THRWSEA = ',THRWSEA
    WRITE (NDSO,1006) ' Reading updated SWH from: ',trim(FLNMANL)
    ! Presently set hardwired CORWSEA logical and THRWSEA energy
    ! thresholds here, not user defined in input file
    CORWSEA = .FALSE.
    !THRWSEA = 0.7
    !
    !        Array allocation
    ALLOCATE ( VATMP(SIZE(VA,1)))
    ALLOCATE ( VAMAPWS(SIZE(VA,1)))
    IF (.NOT. SMCGRD) THEN
      ! SWH arrays allocated using Y,X convention as per wgrib write
      ALLOCATE( SWHBCKG(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
      ALLOCATE( SWHANL(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
      ! Wind arrays allocated using X,Y convention as in w3idatmd
      ALLOCATE( WSBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
      ALLOCATE( WDRBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)) )
#ifdef W3_SMC
    ELSE
      ALLOCATE( SWHBCKG(NSEA,1) )
      ALLOCATE( SWHANL(NSEA,1) )
      ! Use SMCWND to determine if reading a seapoint aray for wind
      IF( SMCWND ) THEN
        ALLOCATE( WSBCKG(NSEA,1) )
        ALLOCATE( WDRBCKG(NSEA,1) )
      ELSE
        ALLOCATE(WSBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)))
        ALLOCATE(WDRBCKG(SIZE(MAPSTA,2), SIZE(MAPSTA,1)))
      ENDIF
#endif
    ENDIF
#ifdef W3_T
    IF (.NOT. SMCGRD) THEN
      ALLOCATE( SWHUPRSTR(SIZE(MAPSTA,1), SIZE(MAPSTA,2)) )
    ELSE
      ALLOCATE( SWHUPRSTR(NSEA,1) )
    ENDIF
#endif
    !
    !        Read additional Input: Analysis Field
    INQUIRE(FILE=FLNMANL, EXIST=anl_exists)
    IF (anl_exists) THEN
#ifdef W3_T
      WRITE (NDSO,*) 'shape(SWHANL)', shape(SWHANL)
#endif
#ifdef W3_WRST
      ! For WRST switch read only corrected SWH
      ! Wind will have been read from the restart
      IF (WRSTON) THEN
        CALL READ_GRBTXT(SWHANL, FLNMANL, SMCGRD)
      ELSE
#endif
        CALL READ_GRBTXTWS(SWHANL,WSBCKG,WDRBCKG,FLNMANL,SMCGRD)
#ifdef W3_WRST
      ENDIF
#endif
#ifdef W3_T
      CALL writeMatrix('SWHANL_IN.txt',SWHANL)
#endif
    ELSE
      WRITE (NDSO,*) trim(FLNMANL), ' does not exist, stopping...'
      DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
#ifdef W3_T
      DEALLOCATE( SWHUPRSTR )
#endif
      STOP
    END IF
    !
#ifdef W3_WRST
    !Calculate wind speed and direction values from u,v..
    !..using cartesian direction convention
    !At present assume only needed for data read from restart
    CALL UVTOCART(WXNwrst,WYNwrst,WSBCKG,WDRBCKG,SMCWND)
#endif
    !
    !        Calculation
    DO ISEA=1, NSEA, 1
      IF (.NOT. SMCGRD) THEN
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
        IXW = IX
        IYW = IY
#ifdef W3_SMC
      ELSE
        IX = 1
        IY = ISEA
        IF( SMCWND ) THEN
          ! Wind arrays allocated using (X,Y) convention for regular grids
          ! but overriding here for the SMC grid which are always defined
          ! as (NSEA,1) by switching the IY and IX dimension values around
          IXW = IY
          IYW = IX
        ELSE
          IXW = MAPSF(ISEA,1)
          IYW = MAPSF(ISEA,2)
        ENDIF
#endif
      ENDIF
      VATMP = VA(:,ISEA)
      CALL SWH_RSRT_1pw (VATMP, WSBCKG(IXW,IYW), WDRBCKG(IXW,IYW), ISEA, &
           SWHBCKG_1, SWHBCKG_W, SWHBCKG_S, VAMAPWS)
      SWHBCKG(IY,IX)=SWHBCKG_1
      !/
      IF ( SWHBCKG(IY,IX) > 0.01 .AND. SWHANL(IY,IX) > 0.01 ) THEN
        ! If wind-sea is dominant energy component apply correction to
        ! wind-sea part only
        IF ( (SWHBCKG_W / SWHBCKG_1)**2.0 > THRWSEA ) THEN
          ! Apply spectrum updates to wind-sea bins only
          PRCNTG=SQRT((SWHANL(IY,IX)**2.0-SWHBCKG_S**2.0)/SWHBCKG_W**2.0)
          CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
          CALL UPDTWSPECF(VATMP, PRCNTG, VAMAPWS, ISEA, .FALSE.)
          ! else correct the whole spectrum
        ELSE
          PRCNTG=(SWHANL(IY,IX)/SWHBCKG_1)
          CALL CHECK_PRCNTG(PRCNTG,PRCNTG_CAP)
          IF (CORWSEA) THEN
            ! Include frequency shifts in wind-sea update
            CALL UPDTWSPECF(VATMP, PRCNTG, VAMAPWS, ISEA, .TRUE.)
          ELSE
            ! bulk correction only, as per UPD2
            CALL UPDATE_VA(PRCNTG,VATMP)
          END IF
        END IF
#ifdef W3_T
        WRITE (NDSO,*) 'ISEA = ', ISEA,' IX = ',IX,' IY = ', IY,         &
             ' PRCNTG = ',PRCNTG,' SWHBCKG = ',SWHBCKG(IY,IX), &
             ' SWHANL = ', SWHANL(IY,IX)
#endif
        VA(:,ISEA)=VATMP
#ifdef W3_T
        CALL SWH_RSRT_1p (VATMP, ISEA, SWHTMP)
        SWHUPRSTR(IY,IX)=SWHTMP
        WRITE (NDSO,*) ' =========== UPD6 Output ==========='
        WRITE (NDSO,*)'ISEA = ',ISEA,'SWH_BCKG = ', SWHBCKG(IY,IX), &
             'SWH_ANL = ', SWHANL(IY,IX),                 &
             'SWH_RSTR = ',SWHUPRSTR(IY,IX)
#endif
      END IF
    END DO
#ifdef W3_T
    CALL writeMatrix('SWHBCKG_UPD6.txt', REAL(SWHBCKG  ))
    CALL writeMatrix('SWHANL_UPD6.txt' , REAL(SWHANL   ))
    CALL writeMatrix('SWHRSTR_UPD6.txt', REAL(SWHUPRSTR))
#endif
    !
    DEALLOCATE( SWHANL,VATMP,SWHBCKG,VAMAPWS,WSBCKG,WDRBCKG )
#ifdef W3_T
    DEALLOCATE( SWHUPRSTR )
#endif
    !/
    !/ ------------------------------------------------------------------- /
    !     End of update options
    !/
  END SELECT
  !/
  !/ ------------------------------------------------------------------- /
  ! 6. Write updated restart file
  !/
#ifdef W3_WRST
  ! Copy read wind values from restart for write out
  WXN = WXNwrst
  WYN = WYNwrst
#endif
  WRITE (NDSO,903)
  RSTYPE = 3
  CALL W3IORS ( 'HOT', NDS(6), SIG(NK), 1 )
#ifdef W3_T
  WRITE (NDST,*), MYNAME,' : Exporting VA at the end of the re-analysis'
  CALL writeMatrix('VA02.txt', REAL(VA))
#endif
  !
  !/
  !/ ------------------------------------------------------------------- /
  ! Escape locations read errors 08k:
  !/
  GOTO 888
  !
800 CONTINUE
  WRITE (NDSE,1000) IERR
  CALL EXTCDE ( 10 )
  !
801 CONTINUE
  WRITE (NDSE,1001)
  CALL EXTCDE ( 11 )
  !
802 CONTINUE
  WRITE (NDSE,1002) IERR
  CALL EXTCDE ( 12 )
  !
888 CONTINUE
  WRITE (NDSO,999)
  !/
  !/ ------------------------------------------------------------------- /
  ! Escape locations read errors 2k:
  !/
  GOTO 2222
  !
2001 CONTINUE
  IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1001)
  GOTO 2222
  !
2002 CONTINUE
  IF ( IAPROC .EQ. NAPERR ) WRITE (NDSE,1002) IERR
  GOTO 2222
  !
2222 CONTINUE
  !/
  !/ ------------------------------------------------------------------- /
  !  Formats
  !/
900 FORMAT (/15X,'   *** WAVEWATCH III ww3_uprstr Initializing ***   '/ &
       15X,'  ==============================================='/)
901 FORMAT ( '  Comment character is ''',A,''''/)
  !
902 FORMAT ( '  The Option ''',A,''' is used.'/)
  !
903 FORMAT ( '  Exporting the Updated Restart file to "restart001.ww3"'/)
  !
920 FORMAT ( '  Grid name : ',A/)
  !
930 FORMAT (/'  Time interval : '/                                  &
       ' --------------------------------------------------')
  !
931 FORMAT ( '       Starting time : ',A)
  !
932 FORMAT ( '       Ending time   : ',A/)
  !
999 FORMAT (/'  End of program '/                                   &
       ' ========================================='/          &
       '         WAVEWATCH III ww3_uprstr         '/)
  !
1000 FORMAT (/' *** WAVEWATCH III ERROR IN W3UPRSTR : '/              &
       '     ERROR IN OPENING INPUT FILE'/                    &
       '     IOSTAT =',I5/)
  !
1001 FORMAT (/' *** WAVEWATCH III ERROR IN W3UPRSTR : '/               &
       '     PREMATURE END OF INPUT FILE'/)

1002 FORMAT (/' *** WAVEWATCH III ERROR IN W3UPRSTR : '/              &
       '     ERROR IN READING FROM INPUT FILE'/               &
       '     IOSTAT =',I5/)
1004 FORMAT (/' '/,A/)
1005 FORMAT (' ',A, F6.3/)
1006 FORMAT (' ',A, A/)
  !
  !/
CONTAINS
  !/
  !/ ------------------------------------------------------------------- /
  !>
  !> @brief Apply correction to the spectrum.
  !>
  !> @details The factor is (swh_anal/swh_bkg)**2 as applying to wave energy.
  !>
  !> @param[in] PRCNTG
  !> @param[inout] VATMP
  !> @author Stelios Flampouris  @date 16-Oct-2018
  !>
  SUBROUTINE UPDATE_VA (PRCNTG, VATMP)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Stelios Flampouris        |
    !/                  |                        FORTRAN 90 |
    !/                  | Created       :       16-Oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    16-Oct-2018 : Original Code                       ( version 6.06 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Apply correction to the spectrum
    !
    !  2. Method :
    !     The factor is (swh_anal/swh_bkg)**2 as applying to wave energy
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, INTENT(IN) :: PRCNTG
    REAL, DIMENSION(:), INTENT(INOUT)  :: VATMP
    !
    VATMP = (PRCNTG**2)*VATMP
    !
  END SUBROUTINE UPDATE_VA
  !/
  !/ ---------------------------------------------------------------------
  !/
  !>
  !> @brief Last sanity check before the update of the spectrum.
  !>
  !> @details The percentage of change is compared against a user defined cap.
  !>
  !> @param[inout] PRCNTG
  !> @param[inout] PRCNTG_CAP
  !> @author Stelios Flampouris  @date 16-Oct-2018
  !>
  SUBROUTINE CHECK_PRCNTG (PRCNTG,PRCNTG_CAP)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Stelios Flampouris        |
    !/                  |                        FORTRAN 90 |
    !/                  | Created       :       16-Oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    16-Oct-2018 : Original Code                       ( version 6.06 )
    !/    24-Oct-2018 : Update by Andy Saulter              ( version 7.14 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Last sanity check before the update of the spectrum
    !  2. Method :
    !     The percentage of change is compared against a user defined cap.
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, INTENT(INOUT)  ::    PRCNTG
    REAL, INTENT(IN   )  ::    PRCNTG_CAP
    ! local
    CHARACTER(12), PARAMETER :: MYNAME='CHECK_PRCNTG'
#ifdef W3_T

    WRITE (NDSO,*) trim(MYNAME)," The original correction is ",PRCNTG
    WRITE (NDSO,*) trim(MYNAME)," The cap is ",PRCNTG_CAP
#endif
    IF ( PRCNTG_CAP < 1. ) THEN
      WRITE (NDSO,*) trim(MYNAME)," WARNING: PRCNTG_CAP set < 1."
      WRITE (NDSO,*) trim(MYNAME),"          This may introduce spurious corrections"
    END IF
#ifdef W3_T
    WRITE (NDSO,*) trim(MYNAME)," The cap is ",PRCNTG_CAP
#endif
    IF ( PRCNTG > 1. ) THEN
#ifdef W3_T
      WRITE (NDSO,*) trim(MYNAME)," PRCNTG > 1."
#endif
      PRCNTG = MIN(PRCNTG, 1. * PRCNTG_CAP)
    ELSE IF ( PRCNTG < 1. ) THEN
#ifdef W3_T
      WRITE (NDSO,*) trim(MYNAME)," PRCNTG < 1."
#endif
      PRCNTG = MAX(PRCNTG, 1. / PRCNTG_CAP)
#ifdef W3_T

#endif
    END IF
#ifdef W3_T
    WRITE (NDSO,*) trim(MYNAME)," The updated correction is ",PRCNTG
#endif
    !
  END SUBROUTINE CHECK_PRCNTG
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> @brief Read gribtxt files.
  !>
  !> @param[inout] UPDPRCNT
  !> @param[inout] FLNMCOR
  !> @param[inout] SMCGRD
  !> @author Stelios Flampouris @date 16-Oct-2018
  !>
  SUBROUTINE READ_GRBTXT(UPDPRCNT,FLNMCOR,SMCGRD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Stelios Flampouris        |
    !/                  |                        FORTRAN 90 |
    !/                  | Created       :       15-Mar-2017 |
    !/                  | Last Update   :       16-Oct-2018 |
    !/                  +-----------------------------------+
    !/
    !/    15-Mar-2017 : Original Code                       ( version 6.04 )
    !/    16-Oct-2018 : Generalization of the reader        ( version 6.06 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Read gribtxt files
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, DIMENSION(:,:), INTENT(OUT) :: UPDPRCNT
    CHARACTER(*), INTENT(IN) :: FLNMCOR
    LOGICAL, INTENT(IN) :: SMCGRD
    ! Local Variables
    INTEGER            :: I, J, IERR
    INTEGER            :: K, L, M, N
    REAL :: A
    INTEGER, PARAMETER :: IP_FID = 123
    CHARACTER(25), PARAMETER::myname='read_grbtxt'
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' starts'
#endif
    J     = LEN_TRIM(FNMPRE)
    OPEN (IP_FID,FILE=FNMPRE(:J)//TRIM(FLNMCOR),STATUS='OLD' &
         ,ACTION='read',IOSTAT=IERR)
    !
    ! Read text header and check dimensions match expected values
    IF (.NOT. SMCGRD) THEN
      READ( IP_FID, *) M,N
      IF (( SIZE(UPDPRCNT,1) /= N) .OR. ( SIZE(UPDPRCNT,2) /= M )) THEN
        WRITE (NDSO,*) trim(myname),': These are not the grid ' // &
             'dimensions: M=',M,' N=',N
        STOP
      END IF
#ifdef W3_SMC
    ELSE
      READ( IP_FID, *) N
      IF ( SIZE(UPDPRCNT,1) /= N ) THEN
        WRITE (NDSO,*) trim(myname),': These are not the grid ' // &
             'dimensions: N=',N
        STOP
      END IF
#endif
    END IF
    UPDPRCNT=0
    !
    ! Read the data into its allocated array
    IF (.NOT. SMCGRD) THEN
      DO L=1,N
        DO K=1,M
          A=0.
          READ(IP_FID,*)A
          UPDPRCNT(N+1-L,K)=A
        END DO
      END DO
#ifdef W3_SMC
    ELSE
      DO L=1,N
        A=0.
        READ(IP_FID,*)A
        UPDPRCNT(L,1)=A
      END DO
#endif
    END IF
    !
    CLOSE(IP_FID)
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' ends'
#endif
  END SUBROUTINE READ_GRBTXT
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !>
  !> @brief Read txt files that include wind data.
  !>
  !> @param[out] UPDPRCNT
  !> @param[out] WSPD
  !> @param[out] WDIR
  !> @param[in] FLNMCOR
  !> @param[in] SMCGRD
  !>
  !> @author Andy Saulter @date 24-Oct-2018
  !>
  SUBROUTINE READ_GRBTXTWS(UPDPRCNT,WSPD,WDIR,FLNMCOR,SMCGRD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Andy Saulter              |
    !/                  |                        FORTRAN 90 |
    !/                  | Original code :       24-Oct-2018 |
    !/                  | Last update :         05-Oct-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Oct-2018 : Original Code                       ( version 6.07 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Read txt files that include wind data
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, DIMENSION(:,:), INTENT(OUT) :: UPDPRCNT, WSPD, WDIR
    CHARACTER(*), INTENT(IN) :: FLNMCOR
    LOGICAL, INTENT(IN) :: SMCGRD
    ! Local Variables
    INTEGER            :: I, J, IERR
    INTEGER            :: K, L, M, N
    REAL :: A, WS, WD
    INTEGER, PARAMETER :: IP_FID = 123
    CHARACTER(25), PARAMETER::myname='read_grbtxt'
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' starts'
#endif
    J     = LEN_TRIM(FNMPRE)
    OPEN (IP_FID,FILE=FNMPRE(:J)//TRIM(FLNMCOR),STATUS='OLD' &
         ,ACTION='read',IOSTAT=IERR)
    !
    ! Read text header and check dimensions match expected values
    IF (.NOT. SMCGRD) THEN
      READ( IP_FID, *) M,N
      IF (( SIZE(UPDPRCNT,1) /= N) .OR. ( SIZE(UPDPRCNT,2) /= M )) THEN
        WRITE (NDSO,*) trim(myname),': These are not the grid ' // &
             'dimensions: M=',M,' N=',N
        STOP
      END IF
#ifdef W3_SMC
    ELSE
      READ( IP_FID, *) N
      IF ( SIZE(UPDPRCNT,1) /= N ) THEN
        WRITE (NDSO,*) trim(myname),': These are not the grid ' // &
             'dimensions: N=',N
        STOP
      END IF
#endif
    END IF
    UPDPRCNT=0
    WSPD=0.
    WDIR=0.
    !
    ! Read the data into allocated arrays
    IF (.NOT. SMCGRD) THEN
      DO L=1,N
        DO K=1,M
          A=0.
          WS=0.
          WD=0.
          READ(IP_FID,*)A, WS, WD
          !SWH data read onto Y,X grid
          UPDPRCNT(N+1-L,K)=A
          !Wind data read onto X,Y grid
          WSPD(K,N+1-L)=WS
          WDIR(K,N+1-L)=WD
        END DO
      END DO
#ifdef W3_SMC
    ELSE
      DO L=1,N
        A=0.
        READ(IP_FID,*)A, WS, WD
        UPDPRCNT(L,1)=A
        WSPD(L,1)=WS
        WDIR(L,1)=WD
      END DO
#endif
    ENDIF
    !
    CLOSE(IP_FID)
    !
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' ends'
#endif
  END SUBROUTINE READ_GRBTXTWS
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> @brief Calculate the significant wave height from the restart file for 1 point.
  !>
  !> @param[in] VA1p
  !> @param[in] ISEA1p
  !> @param[out] HSIG1p
  !> @author Stelios Flampouris  @date 15-May-2017
  !>
  SUBROUTINE SWH_RSRT_1p (VA1p, ISEA1p, HSIG1p )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Stelios Flampouris        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-Mar-2017 |
    !/                  +-----------------------------------+
    !/
    !/    15-Mar-2017 : Original Code                       ( version 6.04 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Calculate the significant wave height from the restart file for 1 point
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, INTENT(OUT) :: HSIG1p
    INTEGER, INTENT(IN) :: ISEA1p
    REAL, DIMENSION(:), INTENT(IN)  :: VA1p
    CHARACTER(25),PARAMETER :: myname='SWH_RSRT_1p'
    !
#ifdef W3_FT
    WRITE (NDSO,*)' '
    WRITE (NDSO,*) trim(myname), ' starts'
#endif
    HSIG1p = 0.
    DEPTH  = MAX ( DMIN , -ZB(ISEA1p) )
    ETOT   = 0.
    !
    DO IK=1, NK
      CALL WAVNU1 ( SIG(IK), DEPTH, WN, CG )
      E1I    = 0.
      DO ITH=1, NTH
        E1I    = E1I + VA1p(ITH+(IK-1)*NTH)  *  SIG(IK) / CG
      END DO
      ETOT   = ETOT + E1I*DSIP(IK)
    END DO
    !
    HSIG1p = 4. * SQRT ( ETOT * DTH )
    !
#ifdef W3_FT
    WRITE (NDSO,*) ' ', trim(myname), ' ends'
    WRITE (NDSO,*)' '
#endif
  END SUBROUTINE SWH_RSRT_1p
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> @brief Calculate Hs from restart for 1 point.
  !>
  !> @details Calculate the significant wave height for total, wind sea and
  !>  swell components from the restart file for 1 point
  !>
  !> @param[in] VA1p
  !> @param[in] WS
  !> @param[in] WD
  !> @param[in] ISEA1p
  !> @param[out] HSIG1p
  !> @param[out] HSIGwp
  !> @param[out] HSIGsp
  !> @param[out] VAMAPWS
  !>
  !> @author Andy Saulter  @date 05-0ct-2019
  !>
  SUBROUTINE SWH_RSRT_1pw (VA1p, WS, WD, ISEA1p, HSIG1p, HSIGwp, HSIGsp, VAMAPWS )
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Andy Saulter              |
    !/                  |                        FORTRAN 90 |
    !/                  | Original code :       24-Oct-2018 |
    !/                  | Last update :         05-Oct-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Oct-2018 : Original Code                       ( version 6.07 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Calculate the significant wave height for total, wind sea and
    !     swell components from the restart file for 1 point
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    USE W3GDATMD, ONLY: TH
    USE W3ODATMD, ONLY: WSMULT !same wind sea cut-off factor for sea/swell outputs
    !
    REAL, INTENT(OUT) :: HSIG1p, HSIGwp, HSIGsp
    INTEGER, INTENT(IN) :: ISEA1p
    REAL, INTENT(IN) :: WS, WD
    REAL, DIMENSION(:), INTENT(IN)  :: VA1p
    INTEGER, DIMENSION(:), INTENT(OUT) :: VAMAPWS ! Wind-sea id for spectral bins
    REAL :: RELWS, ETOTw, ETOTs, EwI, EsI
    CHARACTER(25),PARAMETER :: myname='SWH_RSRT_1pw'
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' starts'
#endif
    HSIG1p = 0.
    HSIGwp = 0.
    HSIGsp = 0.
    DEPTH  = MAX ( DMIN , -ZB(ISEA1p) )
    ETOT   = 0.
    ETOTw  = 0.
    ETOTs  = 0.
    !
    DO IK=1, NK
      CALL WAVNU1 ( SIG(IK), DEPTH, WN, CG )
      E1I    = 0.
      EwI    = 0.
      EsI    = 0.
      DO ITH=1, NTH
        ! Relative wind-sea calc assumes input with in direction toward
        ! i.e. same as for the wave spectrum
        RELWS = WSMULT * WS * MAX(0.0, COS(WD - TH(ITH)))
        E1I    = E1I + VA1p(ITH+(IK-1)*NTH) * SIG(IK) / CG
        IF ( RELWS > (SIG(IK)/WN) ) THEN
          EwI = EwI + VA1p(ITH+(IK-1)*NTH) * SIG(IK) / CG
          VAMAPWS(ITH+(IK-1)*NTH) = 1
        ELSE
          EsI = EsI + VA1p(ITH+(IK-1)*NTH) * SIG(IK) / CG
          VAMAPWS(ITH+(IK-1)*NTH) = 0
        END IF
      END DO
      ETOT   = ETOT + E1I*DSIP(IK)
      ETOTw  = ETOTw + EwI*DSIP(IK)
      ETOTs  = ETOTs + EsI*DSIP(IK)
    END DO
    !
    HSIG1p = 4. * SQRT ( ETOT * DTH )
    HSIGwp = 4. * SQRT ( ETOTw * DTH )
    HSIGsp = 4. * SQRT ( ETOTs * DTH )
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' ends'
#endif
  END SUBROUTINE SWH_RSRT_1pw
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> @brief Calculate speed and cartesian convention directions from u,v
  !>  input vectors.
  !>
  !> @param[in] UVEC
  !> @param[in] VVEC
  !> @param[out] SPD
  !> @param[out] DCART
  !> @param[in] SMCGRD
  !>
  !> @author Andy Saulter  @date 05-Oct-2019
  !>
  SUBROUTINE UVTOCART (UVEC, VVEC, SPD, DCART, SMCGRD)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Andy Saulter              |
    !/                  |                        FORTRAN 90 |
    !/                  | Original code :       05-Oct-2019 |
    !/                  +-----------------------------------+
    !/
    !/    05-Oct-2019 : Original Code                       ( version 6.07 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Calculate speed and cartesian convention directions from u,v
    !     input vectors
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    USE CONSTANTS, ONLY: TPI
    !
    REAL, DIMENSION(:,:), INTENT(OUT) :: SPD, DCART
    REAL, DIMENSION(:,:), INTENT(IN) :: UVEC, VVEC
    LOGICAL, INTENT(IN) :: SMCGRD
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' starts'
#endif
    !
    DO ISEA=1, NSEA, 1
      IF (.NOT. SMCGRD) THEN
        IX = MAPSF(ISEA,1)
        IY = MAPSF(ISEA,2)
#ifdef W3_SMC
      ELSE
        IX = 1
        IY = ISEA
#endif
      ENDIF
      !
      SPD(IY,IX) = SQRT( UVEC(IY,IX)**2 + VVEC(IY,IX)**2 )
      IF( SPD(IY,IX) .GT. 1.E-7) THEN
        DCART = MOD( TPI+ATAN2(UVEC(IY,IX),VVEC(IY,IX)) , TPI )
      ELSE
        DCART = 0
      END IF
      SPD(IY,IX) = MAX( SPD(IY,IX) , 0.001 )
    END DO
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' ends'
#endif
  END SUBROUTINE UVTOCART
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> @brief Updates the wind-sea part of the wave spectrum only.
  !>
  !> @param[inout] VATMP
  !> @param[in] PRCNTG
  !> @param[in] VAMAPWS
  !>
  !> @author Andy Saulter  @date 05-Oct-2019
  !>
  !>
  SUBROUTINE UPDTWSPEC(VATMP, PRCNTG, VAMAPWS)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Andy Saulter              |
    !/                  |                        FORTRAN 90 |
    !/                  | Original code :       24-Oct-2018 |
    !/                  | Last update :         05-Oct-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Oct-2018 : Original Code                       ( version 6.07 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Updates the wind-sea part of the wave spectrum only
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, DIMENSION(:), INTENT(INOUT)  :: VATMP
    INTEGER, DIMENSION(:), INTENT(IN) :: VAMAPWS
    REAL, INTENT(IN) :: PRCNTG
    CHARACTER(25),PARAMETER :: myname='UPDTWSPEC'
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' starts'
#endif
    DO IK=1, NK
      DO ITH=1, NTH
        IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
          VATMP(ITH+(IK-1)*NTH) = VATMP(ITH+(IK-1)*NTH) * PRCNTG**2
        END IF
      END DO
    END DO
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' ends'
#endif
  END SUBROUTINE UPDTWSPEC
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> @brief Updates the wind-sea part of the wave spectrum and shifts
  !>  in frequency space.
  !>
  !> @param[inout] VATMP
  !> @param[in] PRCNTG
  !> @param[in] VAMAPWS
  !> @param[in] ISEA1p
  !> @param[in] ADJALL
  !>
  !> @author Andy Saulter @date 05-Oct-2019
  !>
  SUBROUTINE UPDTWSPECF(VATMP, PRCNTG, VAMAPWS, ISEA1p, ADJALL)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Andy Saulter              |
    !/                  |                        FORTRAN 90 |
    !/                  | Original code :       24-Oct-2018 |
    !/                  | Last update :         05-Oct-2019 |
    !/                  +-----------------------------------+
    !/
    !/    24-Oct-2018 : Original Code                       ( version 6.07 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Updates the wind-sea part of the wave spectrum and shifts in frequency
    !     space
    !  2. Method :
    !
    !  3. Parameters :
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, DIMENSION(:), INTENT(INOUT)  :: VATMP
    INTEGER, DIMENSION(:), INTENT(IN) :: VAMAPWS
    REAL, INTENT(IN) :: PRCNTG
    INTEGER, INTENT(IN) :: ISEA1p
    LOGICAL, INTENT(IN) :: ADJALL
    CHARACTER(25),PARAMETER :: myname='UPDTWSPECF'
    REAL :: FFAC, SIGSHFT, FDM1, FDM2, WN1, CG1, WN2, CG2
    INTEGER :: LPF, M1, M2
    REAL, ALLOCATABLE :: VASHFT(:)
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' starts'
#endif
    DEPTH  = MAX( DMIN , -ZB(ISEA1p))
    ALLOCATE(VASHFT(SIZE(VATMP)))
    VASHFT(:) = 0.0
    !
    ! 1st iteration shifts wind-sea energy in freq space
    FFAC = (1. / PRCNTG**2)**(1.0/3.0) ! uses Toba's relationship
    DO IK=1, NK
      CALL WAVNU1(SIG(IK), DEPTH, WN, CG)
      SIGSHFT = FFAC * SIG(IK)
      DO ITH=1, NTH
        IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
          ! Interpolate frequency bin according to f-shift
          LPF = 1
          DO WHILE (LPF < NK)
            IF (SIG(LPF) >= SIGSHFT) THEN
              IF (LPF .EQ. 1) THEN
                CALL WAVNU1(SIG(LPF), DEPTH, WN1, CG1)
                VASHFT(ITH+(LPF-1)*NTH) = VASHFT(ITH+(LPF-1)*NTH) + &
                     VATMP(ITH+(IK-1)*NTH) *  &
                     (DSIP(IK)*SIG(IK)/CG) /  &
                     (DSIP(LPF)*SIG(LPF)/CG1)
              ELSE
                M2 = LPF
                M1 = LPF - 1
                FDM1 = SIGSHFT - SIG(M1)
                FDM2 = SIG(M2) - SIG(M1)
                CALL WAVNU1(SIG(M1), DEPTH, WN1, CG1)
                CALL WAVNU1(SIG(M2), DEPTH, WN2, CG2)
                VASHFT(ITH+(M1-1)*NTH) = VASHFT(ITH+(M1-1)*NTH) + &
                     (FDM1 / FDM2) *         &
                     VATMP(ITH+(IK-1)*NTH) * &
                     (DSIP(IK)*SIG(IK)/CG) / &
                     (DSIP(M1)*SIG(M1)/CG1)
                VASHFT(ITH+(M2-1)*NTH) = VASHFT(ITH+(M2-1)*NTH) + &
                     (1.0 - FDM1 / FDM2) *   &
                     VATMP(ITH+(IK-1)*NTH) * &
                     (DSIP(IK)*SIG(IK)/CG) / &
                     (DSIP(M2)*SIG(M2)/CG2)
              END IF
              LPF = NK + 1
            ENDIF
            LPF = LPF + 1
          END DO
          IF (LPF .EQ. NK) THEN
            CALL WAVNU1(SIG(LPF), DEPTH, WN1, CG1)
            VASHFT(ITH+(LPF-1)*NTH) = VASHFT(ITH+(LPF-1)*NTH) + &
                 VATMP(ITH+(IK-1)*NTH) *  &
                 (DSIP(IK)*SIG(IK)/CG) /  &
                 (DSIP(LPF)*SIG(LPF)/CG1)
          END IF
        END IF
      END DO
    END DO
    ! 2nd iteration scales wind-sea energy
    DO IK=1, NK
      DO ITH=1, NTH
        IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
          VASHFT(ITH+(IK-1)*NTH) = VASHFT(ITH+(IK-1)*NTH) * PRCNTG**2
        END IF
      END DO
    END DO
    ! 3rd iteration combines wind-sea and swell energy
    DO IK=1, NK
      DO ITH=1, NTH
        IF ( VAMAPWS(ITH+(IK-1)*NTH) .EQ. 1 ) THEN
          VATMP(ITH+(IK-1)*NTH) = VASHFT(ITH+(IK-1)*NTH)
        ELSE
          IF ( ADJALL ) THEN
            ! Swell components are also re-scaled
            VATMP(ITH+(IK-1)*NTH) = VATMP(ITH+(IK-1)*NTH) * &
                 PRCNTG**2            + &
                 VASHFT(ITH+(IK-1)*NTH)
          ELSE
            ! Re-scaling wind-sea only
            VATMP(ITH+(IK-1)*NTH) = VATMP(ITH+(IK-1)*NTH) + &
                 VASHFT(ITH+(IK-1)*NTH)
          END IF
        END IF
      END DO
    END DO
    !
    DEALLOCATE(VASHFT)
    !
#ifdef W3_T
    WRITE (NDSO,*) trim(myname), ' ends'
#endif
  END SUBROUTINE UPDTWSPECF
  !/
  !/ ------------------------------------------------------------------- /
  !/
  !> @brief Writes a 2D array to text file, column by column.
  !>
  !> @param[inout] FILENAME Path to the output file.
  !> @param[inout] RDA_A 2D array to write.
  !>
  !> @author Stelios Flampouris  @date 15-Mar-2017
  !>
  SUBROUTINE WRITEMATRIX(FILENAME, RDA_A)
    !/
    !/                  +-----------------------------------+
    !/                  | WAVEWATCH III           NOAA/NCEP |
    !/                  |         Stelios Flampouris        |
    !/                  |                        FORTRAN 90 |
    !/                  | Last update :         15-Mar-2017 |
    !/                  +-----------------------------------+
    !/
    !/    15-Mar-2017 : Original Code                       ( version 6.04 )
    !/
    !/    Copyright 2010 National Weather Service (NWS),
    !/    National Oceanic and Atmospheric Administration.  All rights
    !/    reserved.  WAVEWATCH III is a trademark of the NWS.
    !/    No unauthorized use without permission.
    !/
    !  1. Purpose :
    !     Writes a 2D array to text file, column by column
    !  2. Method :
    !
    !  3. Parameters :
    !     fileName path to the output file
    !     rda_A 2D array to write
    !
    !     Local parameters.
    !     ----------------------------------------------------------------
    !
    !  4. Subroutines used :
    !
    !     ----------------------------------------------------------------
    !  Internal Subroutines:
    !
    !  5. Called by :
    !  Any routine that has to write 2d arrays !?!
    !
    !  6. Error messages :
    !
    !  7. Remarks :
    !
    !  8. Structure :
    !
    !  9. Switches :
    !
    !     !/T
    !
    ! 10. Source code :
    !
    !/
    REAL, DIMENSION(:, :), INTENT(IN) :: RDA_A
    CHARACTER(*)         , INTENT(IN) :: FILENAME
    INTEGER IB_I, IB_J, IL_IOS
    INTEGER, PARAMETER :: IP_FID = 123
    !
    OPEN( UNIT = IP_FID, FILE = FILENAME, STATUS = 'REPLACE', &
         FORM = 'FORMATTED', IOSTAT = IL_IOS)
    IF (IL_IOS /= 0) PRINT*,'In writeMatrix : Error creating file'//FILENAME
    DO IB_J = 1, SIZE(RDA_A,2)
      DO IB_I = 1, SIZE(RDA_A,1)
        !               write(unit=ip_fid, fmt='(I, $)') rda_A(ib_i,ib_j)
        WRITE(UNIT=IP_FID, FMT='(E18.8, $)') RDA_A(IB_I,IB_J)
      END DO
      WRITE(UNIT=IP_FID, FMT=*)''
    END DO
    CLOSE(IP_FID)
    !
  END SUBROUTINE WRITEMATRIX
  !/
  !/ ------------------------------------------------------------------- /
  !/
END PROGRAM W3UPRSTR
