SUBROUTINE dailycn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Calculates curve number for the day in the HRU

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    sci(:)      |none          |retention coefficient for cn method based on
!!                               |plant ET
!!    smx(:)      |none          |retention coefficient for cn method based on
!!                               |soil moisture
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on
!!                               |any given day
!!    sol_tmp(2,:)|deg C         |daily average temperature of second soil layer
!!    wrt(1,:)    |none          |1st shape parameter for calculation of
!!                               |water retention
!!    wrt(2,:)    |none          |2nd shape parameter for calculation of
!!                               |water retention
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnday(:)    |none          |curve number for current day, HRU and at
!!                               |current soil moisture
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icn         |none          |CN method flag:
!!                               |(for testing alternative methods)
!!                               |0 use traditional SWAT method which bases
!!                               |  CN on soil moisture
!!                               |1 use alternative method which bases CN on
!!                               |  plant ET
!!                  Daniel 1/2012
!!                               |2 use tradtional SWAT method which bases
!!                               |  CN on soil moisture but rention is adjusted
!!                               |  for mildly-sloped tiled-drained watersheds
!!                  Daniel 1/2012
!!    j           |none          |HRU number
!!    r2          |none          |retention parameter in CN equation
!!    xx          |none          |variable used to store intermediate
!!                               |calculation result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

INTEGER :: j

REAL :: xx, r2

j = 0
j = ihru


xx = 0.
r2 = 0.
xx = wrt(1,j) - wrt(2,j) * sol_sw(j)
IF (xx < -20.) xx = -20.
IF (xx > 20.) xx = 20.

IF (icn <= 0) THEN
!! traditional CN method (function of soil water)
  IF ((sol_sw(j) + EXP(xx)) > 0.001) THEN
    r2 = r2adj(j) * smx(j) * (1. - sol_sw(j)/(sol_sw(j)+EXP(xx)))
  END IF
ELSE
!! alternative CN method (function of plant ET)
  r2 = AMAX1(3., sci(j))
END IF

IF (sol_tmp(2,j) <= 0.) r2 = smx(j) * (1. - EXP(- cn_froz * r2))
r2 = AMAX1(3.,r2)

cnday(j) = 25400. / (r2 + 254.)
sol_cnsw(j) = sol_sw(j)

RETURN
END SUBROUTINE dailycn
