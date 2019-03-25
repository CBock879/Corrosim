PROGRAM main
SUBROUTINE  RK4(x,t,dt,l)
      !x = x(t+dt)

      IMPLICIT NONE
      !size of the state matrix
      INTEGER              :: l 
      REAL                 :: dt, t
      REAL, DIMENSION (l)  :: x,k1,k2,k3,k4

      k1 = dt * funct(x,t)
      k2 = dt * funct(x+k1/2,t+dt/2,l)
      k3 = dt * funct(x+k2/2,t+dt/2,l)
      k4 = dt * funct(x+k4/2,t+dtl)
      x = x+ ((k1 + 2* k2 + 2*k3 + k4)/6,l)
      t = t + dt

END SUBROUTINE
!********************************************
!FUNCTION funct
! gets the state of the system
! Args:
!       state: current state of the system`
!       t:     time
!       l:     length of state vector
!Returns:
!       funct: dstate/dt 
!********************************************
FUNCTION funct(state,t,l)
        IMPLICIT NONE
        REAL, DIMENSION (2) :: funct,  u
        REAL                      :: t
        REAL, PARAMETER           :: m = 3 
        REAL, PARAMETER           :: k = 10 
        REAL, PARAMETER           :: c = 0.5477 
        REAL, PARAMETER           :: f0 = 2.5
        REAL                      :: force

END FUNCTION
!********************************************
!FUNCTION element val
! Gets the value of the partial at the element
! Args:
!       ua: `u(i-1)
!       ub:  u(i)
!       uc:  u(i+1)
!
!Returns:
!       value of the partial at that element
!********************************************
FUNCTION elementval(ua,ub,uc)
        IMPLICIT NONE 
        REAL, DIMENSION(3)        :: ua,ub,uc,v,area, unorm
        REAL, DIMENSION(3)        :: elementval
        REAL, PARAMETER           :: alpha
        
        area = mag(ub - ua) + mag(uc - ua)

        unorm = norm(uc - ua)

        elementval =  area * alpha * unorm
        
END FUNCTION
!********************************************
!FUNCTION element val
! gets the normal of a vector using LAPACK
! Args:
!       u: input vector
!       l: length of vector
!Returns:
!       normal vector to it
!********************************************
FUNCTION norm(u)
        REAL, DIMENSION(3)         :: u
        REAL, DIMENSION(3)         :: norm 

        
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
!*********************************************
!FUNCTION
! Args:
! Returns:
!*********************************************

END PROGRAM main

