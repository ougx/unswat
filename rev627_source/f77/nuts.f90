SUBROUTINE nuts(u1,u2,uu)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function calculates the plant stress factor caused by limited
!!    supply of nitrogen or phosphorus

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    u1          |kg/ha         |actual amount of element in plant
!!    u2          |kg/ha         |optimal amount of element in plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    uu          |none          |fraction of optimal plant growth achieved
!!                               |where reduction is caused by plant element
!!                               |deficiency
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


REAL, INTENT(INOUT)                      :: u1
REAL, INTENT(INOUT)                      :: u2
REAL, INTENT(OUT)                        :: uu



uu = 0.

uu = 200. * (u1 / (u2 + .0001) - .5)

IF (uu <= 0.) THEN
  uu = 0.
ELSE
  IF (uu < 99.) THEN
    uu = uu / (uu + EXP(3.535 - .02597 * uu))
  ELSE
    uu = 1.
  END IF
END IF

IF (u2 <= 1.e-6) uu = 1.

RETURN
END SUBROUTINE nuts
