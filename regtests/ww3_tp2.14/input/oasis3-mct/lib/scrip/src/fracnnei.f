      subroutine fracnnei (src_size, dst_size,
     $    ld_srcmask, ld_dstmask,
     $    src_lon, src_lat, dst_lon, dst_lat,
     $    num_links, num_wgts,
     $    weights, src_addr, dst_addr)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 4 *
C               * -------------     ------- *
C               *****************************
C
C**** *fracnnei* - SCRIP remapping
C
C     Purpose:
C     -------
C     Treatment of the tricky points in an interpolation
C
C     Interface:
C     ---------
C       *CALL*  *
C
C     Called from:
C     -----------
C     scriprmp
C
C     Input:
C     -----
C             src_size    : source grid size (integer)
C             dst_size    : target grid size (integer)
C             ld_srcmask  : mask of the source grid
C             ld_dstmask  : mask of the target grid
C             src_lon     : longitudes of the source grid
C             src_lat     : latitudes of the source grid
C             dst_lon     : longitudes of the target grid
C             dst_lat     : latitudes of the target grid
C             num_links   : total number of links
C             num_wgts    : number of weights for each link
C     InOut
C     -----
C             weights     : remapping weights
C             src_addr    : remapping source addresses
C             dst_addr    : remapping target addresses
C
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       2.5       D.Declat       2002/08/20  adapted from S. Valcke ptmsq
C       3.0       S. Valcke      2002/10/30  test and corrections
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C* ---------------------------- Modules used ----------------------------
C
      use kinds_mod     ! defines common data types
      use constants     ! defines common constants
      use grids         ! module containing grid information
      use remap_vars    ! module containing remap information
      USE mod_oasis_flush
C
C* ---------------------------- Implicit --------------------------------
C
      implicit none
C
C* ---------------------------- Include files ---------------------------
C

C      INCLUDE 'netcdf.inc'
C
C* ---------------------------- Intent In -------------------------------
C
      INTEGER (kind=int_kind) ::
     $    src_size,             ! size of the source grid
     $    dst_size              ! size of the destination grid
C
      REAL (kind=dbl_kind) ::
     $    src_lat(src_size), src_lon(src_size),
     $    dst_lat(dst_size), dst_lon(dst_size)

C
      LOGICAL ::
     $    ld_srcmask(src_size),   ! source grid mask
     $    ld_dstmask(dst_size)    ! target grid mask
C
      INTEGER (kind=int_kind) ::
     $    num_links,      ! number of links between src and tgt
     $    num_wgts        ! number of weights

C
C* ---------------------------- Intent InOut ------------------------------
C
      REAL (kind=dbl_kind) ::
     $    weights(num_wgts, num_links) ! remapping weights
C
      INTEGER (kind=int_kind) ::
     $    src_addr(num_links), ! remapping source addresses
     $    dst_addr(num_links)  ! remapping target addresses
C
C* ---------------------------- Local declarations ----------------------
C
C
      INTEGER (kind=int_kind) :: 
     $    ila_nneiadd           ! Nearest-neighbor address
C
      INTEGER (kind=int_kind) ::
     $    ib_dst,               ! INDEX loop for the distance grid
     $    ib_src,               ! INDEX loop for the source grid
     $    ib_neigh,             ! INDEX loop on the corresponding src pts
     $    ib_links              ! INDEX loop for the links      
C
      INTEGER (kind=int_kind) ::
     $    nb_Vmm,               ! number of Mtt points
     $    ntottotland,          ! number of land points
     $    ntotland,             ! number of land points
     $    ntotoce,              ! number of oceanic points
     $    ntotngh               ! number of corresponding source points
C
      INTEGER (kind=int_kind) ::
     $    beg_links,            ! begining of the serie of links
     $    nb_links              ! number of links
C
      REAL (kind=dbl_kind) ::
     $    coslat,               ! cosinus of the latitude
     $    sinlat,               ! sinus of the latitude
     $    coslon,               ! cosinus of the longitude
     $    sinlon,               ! sinus of the longitude
     $    distance, 
     $    dist_min,
     $    arg
C
      INTEGER (kind=int_kind) :: n, il
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        '   Entering ROUTINE fracnnei  -  Level 4'
          WRITE (UNIT = nulou,FMT = *) 
     $        '           ****************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Treating the tricky points of the remapping'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF
C
C *----------------------------------------------------------------------
C
C*    2. Treating Vmm points   V
C        -------------------  m m
C     The target point is a non-masked Valid point while the source points 
C         are all masked points. Use of the non-masked nearest neighbours.
C
      nb_Vmm = 0
      ntottotland = 0
      beg_links = 1

C* -- Loop all other the target points
      DO ib_dst = 1, dst_size
C* -- If the point is a sea point
        IF (ld_dstmask(ib_dst)) THEN

            beg_links = 0

            DO ib_links = 1, num_links
              IF (dst_addr(ib_links) .eq. ib_dst) THEN
                  beg_links = ib_links
                  exit
              ENDIF
            END DO 

            IF (beg_links .ne. 0) THEN

                ntotland = 0
                ntotoce = 0
                ntotngh = 0

C* -- Find the number of associated src points to the non-masked tgt point
                nb_links = 1
                DO il = beg_links+1, num_links
                  IF (dst_addr(il) .eq. dst_addr(beg_links)) 
     $                nb_links = nb_links + 1
                END DO
                
C* -- For each point on the src grid associated to the non-masked tgt point
                DO ib_neigh = 1, nb_links
C
                  ntotngh = ntotngh + 1

C* -- Check IF the point is a masked or non-masked point and treat it
                  IF (.not. ld_srcmask(src_addr(beg_links+ib_neigh-1)))
     $                THEN
                      ntotland = ntotland + 1
                      ntottotland = ntottotland + 1
                  ELSE IF (ld_srcmask(src_addr(beg_links+ib_neigh-1))) 
     $                    THEN
                      ntotoce = ntotoce + 1
                  ELSE
                      WRITE (nulou, *) 'Pb with ocean mask with Mtt 1' 
                  END IF

                END DO
 
C* -- If all the src points are land, treat it !
                IF (ntotland .EQ. ntotngh) THEN
                    nb_Vmm = nb_Vmm + 1
                    WRITE(nulou,*) 
     $               '************ Doing FRACNNEI for point', ib_dst  

C* -- Find the nearest neighbours and change weights and address 

                    coslat = cos(dst_lat(ib_dst))
                    sinlat = sin(dst_lat(ib_dst))
                    coslon = cos(dst_lon(ib_dst))
                    sinlon = sin(dst_lon(ib_dst))

                    dist_min = bignum
                    ila_nneiadd = 0
                    DO ib_src = 1, src_size
                      IF (ld_srcmask(ib_src)) THEN
                          arg = 
     &                        coslat*cos(src_lat(ib_src))*
     &                       (coslon*cos(src_lon(ib_src)) +
     &                        sinlon*sin(src_lon(ib_src)))+
     &                        sinlat*sin(src_lat(ib_src))
                          IF (arg < -1.0d0) THEN
                              arg = -1.0d0
                          ELSE IF (arg > 1.0d0) THEN
                              arg = 1.0d0
                          END IF
                          distance = acos(arg)
                          IF (distance < dist_min) THEN
                              ila_nneiadd = ib_src
                              dist_min = distance
                          ENDIF
                      ENDIF
                    END DO
                    src_addr(beg_links) = ila_nneiadd
                    weights(1,beg_links) = 1.0
                    WRITE(nulou,*) 
     $               '*************** Nearest source neighbour is ', 
     $                  ila_nneiadd                    
                    IF (nb_links > 1) then
                        DO n=2, nb_links
                          src_addr(beg_links+n-1)=0
                          weights(1,beg_links+n-1)=0.0
                        END DO
                    END IF
                END IF
            END IF
        ENDIF
      END DO
C
C
C *----------------------------------------------------------------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        '   Leaving ROUTINE fracnnei  -  Level 4'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL OASIS_FLUSH_SCRIP(nulou)
      ENDIF

      END SUBROUTINE fracnnei

!***********************************************************************

  
