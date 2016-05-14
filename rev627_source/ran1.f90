REAL*8 FUNCTION ran1(idum)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02


!  THIS SUBROUTINE IS FROM "NUMERICAL RECIPES" BY PRESS ET AL.

INTEGER, INTENT(IN)                      :: idum
IMPLICIT REAL*8 (a-h,o-z)
DIMENSION rqq(97)
INTEGER, PARAMETER :: m1 = 259200
INTEGER, PARAMETER :: ia1 = 7141
INTEGER, PARAMETER :: ic1 = 54773
REAL, PARAMETER :: rm1 =3.8580247E-6
INTEGER, PARAMETER :: m2 = 134456
INTEGER, PARAMETER :: ia2 = 8121
INTEGER, PARAMETER :: ic2 = 28411
REAL, PARAMETER :: rm2 =7.4373773E-6
INTEGER, PARAMETER :: m3 = 243000
INTEGER, PARAMETER :: ia3 = 4561
INTEGER, PARAMETER :: ic3 = 51349
SAVE
DATA iff / 0 /

IF ((idum < 0) .OR. (iff == 0)) THEN
  iff = 1
  ix1 = MOD(ic1 - idum,m1)
  ix1 = MOD((ia1 * ix1) + ic1,m1)
  ix2 = MOD(ix1,m2)
  ix1 = MOD((ia1 * ix1) + ic1,m1)
  ix3 = MOD(ix1,m3)
  DO  j = 1, 97
    ix1 = MOD((ia1 * ix1) + ic1,m1)
    ix2 = MOD((ia2 * ix2) + ic2,m2)
    rqq(j) = (DBLE(ix1) + (DBLE(ix2) * rm2)) * rm1
  END DO
!!    idum = 1
END IF
ix1 = MOD((ia1 * ix1) + ic1,m1)
ix2 = MOD((ia2 * ix2) + ic2,m2)
ix3 = MOD((ia3 * ix3) + ic3,m3)
j = 1 + ((97 * ix3) / m3)
ran1 = rqq(j)
rqq(j) = (DBLE(ix1) + (DBLE(ix2) * rm2)) * rm1

!  END OF SUBROUTINE RAN1
RETURN
END FUNCTION ran1
