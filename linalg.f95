MODULE linalg
      IMPLICIT NONE
CONTAINS
!********************************************
!FUNCTION cross
! gets the cross porduct of vector
! Args:
!       u: input vector
!Returns:
!       normal vector to it
!********************************************
FUNCTION cross(u,v)
        REAL, DIMENSION(3)         :: u,v
        REAL, DIMENSION(3)         :: cross 
        
        cross(1) = u(2) * v(3) - v(3) * u(2) 
        cross(2) = u(3) * v(1) - u(1) * v(3)
        cross(3) = u(1) * v(2) - v(2) * u(1) 

END FUNCTION 
!********************************************
!FUNCTION mag
! gets the magnetude of u
! Args:
!       u: input vector
!Returns:
!       magnetude of vector u
!********************************************

FUNCTION mag(u)
        REAL, DIMENSION(3)   :: u
        REAL                 :: mag

        mag = sqrt(u(1)**2 + u(2)**2 + u(3)**2)
END FUNCTION

END MODULE linalg

