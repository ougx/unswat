SUBROUTINE resbact
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine models bacteria in reservoirs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1            |none        |reservoir number
!!    inum2            |none        |inflow hydrograph storage location number
!!    res_bactlp(:)    |# cfu/100ml |less persistent bacteria stored in
!!                                  |reservoir
!!    res_bactp(:)     |# cfu/100ml |persistent bacteria stored in reservoir
!!    reswtr           |m^3 H2O     |initial reservoir volume
!!    thbact           |none        |temperature adjustment factor for bacteria
!!                                  |die-off/growth
!!    tmpav(:)         |deg C       |average air temperature on current day
!!    varoute(2,:)     |m^3 H2O     |water flowing into reservoir on day
!!    varoute(18,:)    |# cfu/100ml |persistent bacteria
!!    varoute(19,:)    |# cfu/100ml |less persistent bacteria
!!    wdlpres          |1/day       |Die-off factor for less persistent bacteria
!!                                  |in reservoirs
!!    wdpres           |1/day       |Die-off factor for persistent bacteria in
!!                                  |reservoirs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    res_bactlp(:)|# cfu/100ml  |less persistent bacteria in reservoir/outflow
!!                               |at end of day
!!    res_bactp(:) |# cfu/100ml  |persistent bacteria in reservoir/outflow at
!!                               |end of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jres        |none          |reservoir number
!!    netwtr      |m^3 H2O       |net amount of water in reservoir during
!!                               |time step
!!    totbactlp   |10^4 cfu      |mass less persistent bacteria
!!    totbactp    |10^4 cfu      |mass persistent bacteria
!!    wtmp        |deg C         |temperature of water in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
IMPLICIT NONE

REAL, EXTERNAL :: theta

INTEGER :: jres
REAL :: totbactp, totbactlp, netwtr
REAL :: wtmp

jres = 0
jres = inum1

!! calculate temperature in stream
!! Stefan and Preudhomme. 1993.  Stream temperature estimation
!! from air temperature.  Water Res. Bull. p. 27-45
!! SWAT manual equation 2.3.13
wtmp = 0.
wtmp = 5.0 + 0.75 * tmpav(jres)
IF (wtmp <= 0.) wtmp = 0.1


!! daily mass balance
!! total bacteria mass in reservoir
totbactp = 0.
totbactlp = 0.
totbactp = varoute(18,inum2) * varoute(2,inum2) + res_bactp(jres) * reswtr
totbactlp = varoute(19,inum2) * varoute(2,inum2) + res_bactlp(jres) * reswtr

!! compute bacteria die-off
totbactp = totbactp * EXP(-theta(wdpres,thbact,wtmp))
totbactp = MAX(0., totbactp)
totbactlp = totbactlp * EXP(-theta(wdlpres,thbact,wtmp))
totbactlp = MAX(0., totbactlp)

!! new concentration
netwtr = 0.
netwtr = varoute(2,inum2) + reswtr
IF (netwtr >= 1.) THEN
  res_bactp(jres) = totbactp / netwtr
  res_bactlp(jres) = totbactlp / netwtr
ELSE
  res_bactp(jres) = 0.
  res_bactlp(jres) = 0.
END IF

RETURN
END SUBROUTINE resbact
