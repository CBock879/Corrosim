!**********************************************
! CORROSIM
! (c) Christopher Bock
!
! An attempt to simulate corrosion in 2d
!
!       
!
!**********************************************
PROGRAM main
        USE linalg
        IMPLICIT NONE

        INTEGER                         :: l , i
        REAL,DIMENSION(:,:),ALLOCATABLE     :: surface
        REAL                            :: t
        l = 4 
        
        ALLOCATE(surface(l,3))


        !testing conditions
        surface(1,:) = (/1,0,0/)
        surface(2,:) = (/0,1,0/)
        surface(3,:) = (/-1,0,0/)
        surface(4,:) = (/0,-1,0/)

        CALL RK4(surface,t,0.1,l)

        WRITE(*,*) surface

        `
CONTAINS
SUBROUTINE  RK4(x,t,dt,l)
      !x = x(t+dt)

      IMPLICIT NONE
      !size of the state matrix
      INTEGER                           :: l 
      REAL                              :: dt, t
      REAL, ALLOCATABLE, DIMENSION (:,:)  :: x,k1,k2,k3,k4

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
        REAL, ALLOCATABLE ,DIMENSION (:,:) :: funct, state  
        REAL                  :: t
        INTEGER               :: l,i
        REAL,DIMENSION(3)     :: orienation

        orientation  = (/0,0,1/) 


        !first element (Assumes closed region)
        funct(1,:) = state(1,:) + elementval(state(l,:),state(1,:),state(2,:),orientation)        

        DO i = 2, l-1, 1
                funct(i,:) = state(i,:) + elementval(state(i-1,:),state(i,:),state(i+1,:),orientation)        
        END DO
        funct(l,:) = state(l) + elementval(state(l-1,:),state(l,:),state(1,:),orientation)


END FUNCTION
!********************************************
!FUNCTION element val
! Gets the value of the partial at the element
! Args:
!       ua: `u(i-1)
!       ub:  u(i)
!       uc:  u(i+1)
!       orientation: surface inside or out
!
!Returns:
!       value of the partial at that element
!********************************************
FUNCTION elementval(ua,ub,uc,orientation)
        IMPLICIT NONE 
        REAL, DIMENSION(3)        :: ua,ub,uc,
        REAL, DIMENSION(3)        :: orientation
        REAL, DIMENSION(3)        :: elementval 
        REAL                      :: area
        REAL, PARAMETER           :: alpha = 0.5
        
        area = mag(ub - ua) + mag(uc - ua)

        unorm = norm(uc - ua)

        elementval =  area * alpha * unorm
        
END FUNCTION
!comment style
!*********************************************
!FUNCTION
! Args:
! Returns:
!*********************************************

END PROGRAM main

