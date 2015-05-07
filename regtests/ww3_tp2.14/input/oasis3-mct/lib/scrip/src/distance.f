       FUNCTION distance (lon1, lat1, lon2, lat2)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL ? *
C               * -------------     ------- *
C               *****************************
C
C**** *distance*  - calculate the distance between two points on a sphere
C
C     Purpose:
C     -------
C     Calculation of the distance between two points on a sphere
C       1. Transformation to x,y,z-coordinates
C       2. Calculating the distance
C       3. Calculating the distance on the sphere
C
C**   Interface:
C     ---------
C       *CALL*  *distance*(lon1, lat1, lon2, lat2)
C
C     Input:
C     -----
C          lon1              : longitude of first point (rad)
C          lat1              : latitude of first point (rad)
C          lon2              : longitude of second point (rad)
C          lat2              : latitude of second point (rad)
C
C     Output:
C     ------
C          distance          : distance
CC
C     History:
C     -------
C       Version   Programmer     Date        Description
C       -------   ----------     ----        -----------  
C       2.5       V. Gayler      2001/09/20  created
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      USE constants
      USE kinds_mod

      IMPLICIT NONE
!-----------------------------------------------------------------------
!     INTENT(IN)
!-----------------------------------------------------------------------
      REAL (kind=real_kind), INTENT(IN) :: 
     $     lon1,                ! longitude of first point (rad)
     $     lon2,                ! longitude of second point (rad)
     $     lat1,                ! latitude of first point (rad)
     $     lat2                 ! latitude of second point (rad)

!-----------------------------------------------------------------------
!     LOKAL VARIABLES
!-----------------------------------------------------------------------
      REAL (kind=real_kind) ::
     $     x1, y1, z1,          ! coordinates of the first point
     $     x2, y2, z2,          ! coordinates of the second point
     $     distance             ! distance between the points (rad)

!-----------------------------------------------------------------------

!     Transformation to x,y,z-coordinates
!     -----------------------------------
      x1 = cos(lat1)*cos(lon1)
      y1 = cos(lat1)*sin(lon1)
      z1 = sin(lat1)

      x2 = cos(lat2)*cos(lon2)
      y2 = cos(lat2)*sin(lon2)
      z2 = sin(lat2)

!     Calculation of the distance
!     ---------------------------
!     direct distance:
      distance = SQRT((x2-x1)**2 + (y2-y1)**2 + (z2-z1)**2)

!     distance along the surface:
      distance = 2*ASIN(distance/2)

!-----------------------------------------------------------------------
      RETURN 
      END FUNCTION distance

