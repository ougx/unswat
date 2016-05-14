SUBROUTINE vbl(evx,spx,pp,qin,ox,vx1,vy,yi,yo,ysx,vf,vyf,aha)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:05

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the water and sediment balance for ponds
!!    and reservoirs at the end of a simulation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aha         |ha            |area draining into water body
!!    evx         |m^3 H2O       |evaporation from water body
!!    ox          |m^3 H2O       |water leaving water body
!!    pp          |m^3 H2O       |precipitation on water body
!!    qin         |m^3 H2O       |water entering water body
!!    spx         |m^3 H2O       |seepage from water body
!!    vf          |m^3 H2O       |volume of water in water body at
!!                               |end of simulation
!!    vx1         |m^3 H2O       |volume of water in water body at
!!                               |beginning of simulation
!!    vy          |metric tons   |sediment in water body at beginning
!!                               |of simulation
!!    vyf         |metric tons   |sediment in water body at end of
!!                               |simulation
!!    yi          |metric tons   |sediment entering water body
!!    yo          |metric tons   |sediment leaving water body
!!    ysx         |metric tons   |change in sediment level in water body
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    vx1         |mm H2O        |dfw expressed as depth over drainage
!!                               |area
!!    vy          |metric tons/ha|dfy expressed as loading per unit area
!!                               |for drainage area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dfw         |m^3 H2O       |difference between mass balance
!!                               |calculated from watershed averages
!!                               |and actual volume of water in water
!!                               |body at end of simulation
!!    dfy         |metric tons   |difference between mass balance
!!                               |calculated from watershed averages
!!                               |and actual sediment in water body
!!                               |at end of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


REAL, INTENT(IN)                         :: evx
REAL, INTENT(IN)                         :: spx
REAL, INTENT(IN)                         :: pp
REAL, INTENT(IN)                         :: qin
REAL, INTENT(IN)                         :: ox
REAL, INTENT(INOUT)                      :: vx1
REAL, INTENT(INOUT)                      :: vy
REAL, INTENT(IN)                         :: yi
REAL, INTENT(IN)                         :: yo
REAL, INTENT(IN)                         :: ysx
REAL, INTENT(IN)                         :: vf
REAL, INTENT(IN)                         :: vyf
REAL, INTENT(IN)                         :: aha



REAL :: dfw, dfy

!! ysx undefined for reservoirs

dfw = 0.
dfy = 0.
dfw = vx1 - evx - spx + qin + pp - ox - vf
dfy = vy + yi - yo - ysx - vyf
vx1 = 0.
vy = 0.
vx1 = .1 * dfw / aha
vy = dfy / aha

RETURN
END SUBROUTINE vbl
