SUBROUTINE resnut
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes soluble nitrogen and soluble phosphorus through reservoirs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlar(:)    |none          |chlorophyll-a production coefficient for
!!                               |reservoir
!!    inum1       |none          |reservoir number
!!    inum2       |none          |inflow hydrograph storage location number
!!    ires1(:)    |none          |beginning of mid-year nutrient settling
!!                               |"season"
!!    ires2(:)    |none          |end of mid-year nutrient settling "season"
!!    i_mo        |none          |current month of simulation
!!    nsetlr(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlr(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    psetlr(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlr(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    res_nh3(:)  |kg N          |amount of ammonia in reservoir
!!    res_no2(:)  |kg N          |amount of nitrite in reservoir
!!    res_no3(:)  |kg N          |amount of nitrate in reservoir
!!    res_orgn(:) |kg N          |amount of organic N in reservoir
!!    res_orgp(:) |kg P          |amount of organic P in reservoir
!!    res_solp(:) |kg P          |amount of soluble P in reservoir
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    resflwo     |m^3 H2O       |water leaving reservoir on day
!!    ressa       |ha            |surface area of reservoir on day
!!    seccir(:)   |none          |water clarity coefficient for reservoir
!!    varoute(4,:)|kg N          |organic nitrogen
!!    varoute(5,:)|kg P          |organic posphorus
!!    varoute(6,:)|kg N          |nitrate
!!    varoute(7,:)|kg P          |soluble phosphorus
!!    varoute(14,:)|kg N         |ammonia
!!    varoute(15,:)|kg N         |nitrite
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    res_chla(:) |kg chl-a      |amount of chlorophyll-a in reservoir
!!    res_nh3(:)  |kg N          |amount of ammonia in reservoir
!!    res_no2(:)  |kg N          |amount of nitrite in reservoir
!!    res_no3(:)  |kg N          |amount of nitrate in reservoir
!!    res_orgn(:) |kg N          |amount of organic N in reservoir
!!    res_orgp(:) |kg P          |amount of organic P in reservoir
!!    res_seci(:) |m             |secchi-disk depth
!!    res_solp(:) |kg P          |amount of soluble P in reservior
!!    reschlao    |kg chl-a      |amount of chlorophyll-a leaving reaservoir
!!                               |on day
!!    resnh3o     |kg N          |amount of ammonia leaving reservoir on day
!!    resno2o     |kg N          |amount of nitrite leaving reservoir on day
!!    resno3o     |kg N          |amount of nitrate leaving reservoir on day
!!    resorgno    |kg N          |amount of organic N leaving reservoir on day
!!    resorgpo    |kg P          |amount of organic P leaving reservoir on day
!!    ressolpo    |kg P          |amount of soluble P leaving reservoir on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlaco      |ppb (ug/L)    |chlorophyll-a concentration
!!    iseas       |none          |nutrient settling rate season
!!    jres        |none          |reservior number
!!    nitrok      |none          |fraction of nitrogen in reservoir removed by
!!                               |settling
!!    phosk       |none          |fraction of phosphorus in reservoir removed
!!                               |by settling
!!    tpco        |ppb (ug/L)    |concentration of phosphorus in water
!!                               |on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: jres, iseas
REAL :: nitrok, phosk, tpco, chlaco

jres = 0
jres = inum1

!! if reservoir volume less than 1 m^3, set all nutrient levels to
!! zero and perform no nutrient calculations
IF (res_vol(jres) < 1.) THEN
  res_orgn(jres) = 0.
  res_orgp(jres) = 0.
  res_no3(jres) = 0.
  res_nh3(jres) = 0.
  res_no2(jres) = 0.
  res_solp(jres) = 0.
  res_chla(jres) = 0.
  res_seci(jres) = 0.
END IF
IF (res_vol(jres) < 1.) RETURN

!! if reservoir volume greater than 1 m^3, perform nutrient calculations
IF (i_mo >= ires1(jres) .AND. i_mo <= ires2(jres)) THEN
  iseas = 1
ELSE
  iseas = 2
END IF

!! add incoming nutrients to those in reservoir
!! equation 29.1.1 in SWAT manual
res_orgn(jres) = res_orgn(jres) + varoute(4,inum2)
res_orgp(jres) = res_orgp(jres) + varoute(5,inum2)
res_no3(jres) = res_no3(jres) + varoute(6,inum2)
res_nh3(jres) = res_nh3(jres) + varoute(14,inum2)
res_no2(jres) = res_no2(jres) + varoute(15,inum2)
res_solp(jres) = res_solp(jres) + varoute(7,inum2)

!! settling rate/mean depth
!! part of equation 29.1.3 in SWAT manual
phosk = 0.
nitrok = 0.
phosk = psetlr(iseas,jres) * ressa * 10000. / (res_vol(jres) + resflwo)
phosk = MIN(phosk, 1.)
nitrok = nsetlr(iseas,jres) * ressa * 10000. / (res_vol(jres) + resflwo)
nitrok = MIN(nitrok, 1.)

!! remove nutrients from reservoir by settling
!! other part of equation 29.1.3 in SWAT manual
res_solp(jres) = res_solp(jres) * (1. - phosk)
res_orgp(jres) = res_orgp(jres) * (1. - phosk)
res_orgn(jres) = res_orgn(jres) * (1. - nitrok)
res_no3(jres) = res_no3(jres) * (1. - nitrok)
res_nh3(jres) = res_nh3(jres) * (1. - nitrok)
res_no2(jres) = res_no2(jres) * (1. - nitrok)

!! calculate chlorophyll-a and water clarity
tpco = 0.
chlaco = 0.
res_chla(jres) = 0.
res_seci(jres) = 0.
tpco = 1.e+6 * (res_solp(jres) + res_orgp(jres)) / (res_vol(jres) + resflwo)
IF (tpco > 1.e-4) THEN
!! equation 29.1.6 in SWAT manual
  chlaco = chlar(jres) * 0.551 * (tpco**0.76)
  res_chla(jres) = chlaco * (res_vol(jres) + resflwo) * 1.e-6
END IF
IF (chlaco > 1.e-4) THEN
!! equation 29.1.8 in SWAT manual
  res_seci(jres) = seccir(jres) * 6.35 * (chlaco**(-0.473))
END IF

!! calculate amount of nutrients leaving reservoir
IF (res_no3(jres) < 1.e-4) res_no3(jres) = 0.0
IF (res_orgn(jres) < 1.e-4) res_orgn(jres) = 0.0
IF (res_orgp(jres) < 1.e-4) res_orgp(jres) = 0.0
IF (res_solp(jres) < 1.e-4) res_solp(jres) = 0.0
IF (res_chla(jres) < 1.e-4) res_chla(jres) = 0.0
IF (res_nh3(jres) < 1.e-4) res_nh3(jres) = 0.0
IF (res_no2(jres) < 1.e-4) res_no2(jres) = 0.0
resno3o = res_no3(jres) * resflwo / (res_vol(jres) + resflwo)
resorgno = res_orgn(jres) * resflwo / (res_vol(jres) + resflwo)
resorgpo = res_orgp(jres) * resflwo / (res_vol(jres) + resflwo)
ressolpo = res_solp(jres) * resflwo / (res_vol(jres) + resflwo)
reschlao = res_chla(jres) * resflwo / (res_vol(jres) + resflwo)
resnh3o = res_nh3(jres) * resflwo / (res_vol(jres) + resflwo)
resno2o = res_no2(jres) * resflwo / (res_vol(jres) + resflwo)
res_orgn(jres) = res_orgn(jres) - resorgno
res_orgp(jres) = res_orgp(jres) - resorgpo
res_no3(jres) = res_no3(jres) - resno3o
res_nh3(jres) = res_nh3(jres) - resnh3o
res_no2(jres) = res_no2(jres) - resno2o
res_solp(jres) = res_solp(jres) - ressolpo
res_chla(jres) = res_chla(jres) - reschlao

RETURN
END SUBROUTINE resnut
