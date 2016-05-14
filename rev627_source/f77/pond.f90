SUBROUTINE pond(k)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through ponds
!!    and computes evaporation and seepage from the ponds

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bp1(:)      |none          |1st shape parameter for pond surface area
!!                               |equation
!!    bp2(:)      |none          |2nd shape parameter for the pond surface area
!!                               |equation
!!    chlap(:)    |none          |chlorophyll-a production coefficient for pond
!!    hru_sub(:)  |none          |subbasin in which HRU/reach is located
!!    iflod1(:)   |none          |beginning month of non-flood season
!!    iflod2(:)   |none          |ending month of non-flood season
!!    ipnd1(:)    |none          |beginning month of 2nd "season" of nutrient
!!                               |settling
!!    ipnd2(:)    |none          |ending month of 2nd "season" of nutrient
!!                               |settling
!!    i_mo        |none          |current month of simulation
!!    ndtarg(:)   |none          |number of days required to reach target
!!                               |storage from current pond storage
!!    nsetlp(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlp(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    pnd_evol(:) |m^3 H2O       |volume of water required to fill pond
!!                               |to the emergency spillway
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into ponds
!!    pnd_k(:)    |mm/hr         |hydraulic conductivity through bottom of
!!                               |ponds
!!    pnd_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in pond at beginning of day
!!    pnd_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in pond at beginning of day
!!    pnd_nsed(:) |kg/L          |normal ratio of sediment to water in pond
!!    pnd_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in pond at beginning of day
!!    pnd_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in pond at beginning of day
!!    pnd_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in pond at
!!                               |beginning of day
!!    pnd_pvol(:) |m^3 H2O       |volume of water required to fill pond
!!                               |to the principal spillway
!!    pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
!!    pnd_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in pond at beginning of day
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndsedin    |metric tons   |sediment entering pond during day
!!    psetlp(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlp(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    seccip(:)   |none          |water clarity coefficient for pond
!!    sed_stl(:)  |kg/kg         |fraction of sediment remaining suspended in
!!                               |impoundment after settling for one day
!!    sol_sumfc(:)|mm H2O        |amount of water held in the soil profile
!!                               |at field capacity
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on any given day
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pnd_chla(:) |kg chl_a      |amount of chlorophyll-a in pond at end of day
!!    pnd_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in pond at end of day
!!    pnd_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in pond at end of day
!!    pnd_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in pond at end of day
!!    pnd_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in pond at end of day
!!    pnd_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in pond at end of day
!!    pnd_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in pond at
!!                               |end of day
!!    pnd_seci(:) |m             |secchi-disk depth of pond
!!    pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
!!    pnd_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in pond at end of day
!!    pnd_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in pond at end of day
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    pndev       |m^3 H2O       |evaporation from pond on day
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp      |m^3 H2O       |precipitation on pond during day
!!    pndsedc     |metric tons   |net change in sediment in pond during day
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlaco      |ppb (ug/L)    |concentration of chlorophyll-a in pond
!!    iseas       |none          |nutrient settling rate season
!!    k           |none          |HRU or reach number
!!    nitrok      |none          |fraction of nitrogen in pond removed by
!!                               |settling
!!    phosk       |none          |fraction of phosphorus in pond removed by
!!                               |settling
!!    pndsa       |ha            |surface area of pond on current day
!!    sed         |kg/L          |sediment concentration in pond at beginning of
!!                               |day
!!    targ        |m^3 H2O       |target storage level in pond
!!    tpco        |ppb (ug/L)    |concentration of phosphorus in pond water
!!                               |on day
!!    vol         |m^3 H2O       |volume of water in pond at beginning of day
!!    xx          |none          |variable to hold intermediate calc result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: k

REAL :: vol, sed, pndsa, xx, targ, tpco, phosk, nitrok, chlaco
INTEGER :: iseas
REAL :: san, sil, cla, sag, lag, inised, finsed,setsed,remsetsed


!! store initial values
vol = 0.
sed = 0.
san = 0.
sil = 0.
cla = 0.
sag = 0.
lag = 0.
inised = 0.
finsed = 0.
setsed = 0.
remsetsed = 0.
vol = pnd_vol(k)
sed = pnd_sed(k)
san = pnd_san(k)
sil = pnd_sil(k)
cla = pnd_cla(k)
sag = pnd_sag(k)
lag = pnd_lag(k)

!! calculate water balance for day
pndsa = 0.
pndsa = hru_fr(k) * bp1(k) * pnd_vol(k) ** bp2(k)
pndev = 10. * evpnd(k) * pet_day * pndsa
pndsep = pnd_k(k) * pndsa * 240.
pndpcp = subp(k) * pndsa * 10.

!! new water volume for day
pnd_vol(k) = pnd_vol(k) - pndsep - pndev + pndpcp + pndflwi

IF (pnd_vol(k) < 0.001) THEN
!! if volume deficit in pond reduce seepage
!! so that the pond volume is zero
  pndsep = pndsep + pnd_vol(k)
  pnd_vol(k) = 0.
!! if seepage is less than the volume deficit, take the remainder
!! from evaporation
  IF (pndsep < 0.) THEN
    pndev = pndev + pndsep
    pndsep = 0.
  END IF
  pnd_sed(k) = 0.
  pnd_san(k) = 0.
  pnd_sil(k) = 0.
  pnd_cla(k) = 0.
  pnd_sag(k) = 0.
  pnd_lag(k) = 0.
  pnd_solp(k) = 0.
  pnd_psed(k) = 0.
  pnd_orgp(k) = 0.
  pnd_solpg(k) = 0.
  pnd_orgn(k) = 0.
  pnd_no3(k) = 0.
  pnd_no3s(k) = 0.
  pnd_no3g(k) = 0.
  pnd_chla(k) = 0.
  pnd_seci(k) = 0.
  
ELSE
  
!! compute outflow
  IF (pnd_evol(k) <= 0.) THEN
!! all storage over principle is outflow
    IF (pnd_vol(k) <= pnd_pvol(k)) THEN
      pndflwo = 0.
    ELSE
      pndflwo = pnd_vol(k) - pnd_pvol(k)
    END IF
  ELSE IF (pnd_vol(k) > pnd_evol(k)) THEN
!! if emergency level is defined, anytime pond volume
!! exceeds this level, all excess is released
    pndflwo = pnd_vol(k) - pnd_evol(k)
  ELSE
!! target storage based on flood season and soil water
    xx = 0.
    targ = 0.
    IF (iflod2(k) > iflod1(k)) THEN
      IF (i_mo > iflod1(k) .AND. i_mo < iflod2(k)) THEN
        targ = pnd_evol(k)
      ELSE
        xx = MIN(sol_sw(k) / sol_sumfc(k),1.)
        targ = pnd_pvol(k) + .5 * (1. - xx) * (pnd_evol(k) - pnd_pvol(k))
      END IF
    ELSE
      IF (i_mo > iflod1(k) .OR. i_mo < iflod2(k)) THEN
        targ = pnd_evol(k)
      ELSE
        xx = MIN(sol_sw(k) / sol_sumfc(k),1.)
        targ = pnd_pvol(k) + .5 * (1. - xx) * (pnd_evol(k) - pnd_pvol(k))
      END IF
    END IF
    IF (pnd_vol(k) > targ) THEN
      pndflwo = (pnd_vol(k) - targ) / ndtarg(k)
    ELSE
      pndflwo = 0.
    END IF
  END IF
  
!! compute new sediment concentration
  IF (pndsedin < 1.e-6) pndsedin = 0.
  IF (pndsa == 0.) pndsa = 0.001    !!MJW added line of code 040811
  velofl = (pndflwo / pndsa) / 10000.
  IF (velofl > 1.e-6) THEN
    trappnd = velsetlp(k) / velofl
    IF (trappnd > 1.) trappnd = 1.
    susp = 1. - trappnd
  ELSE
    susp = 0.
  END IF
  
  pnd_sed(k) = (sed * vol + susp * pndsedin) / pnd_vol(k)
  pnd_san(k) = (san * vol + pndsanin) / pnd_vol(k)
  pnd_sil(k) = (sil * vol + pndsilin) / pnd_vol(k)
  pnd_cla(k) = (cla * vol + pndclain) / pnd_vol(k)
  pnd_sag(k) = (sag * vol + pndsagin) / pnd_vol(k)
  pnd_lag(k) = (lag * vol + pndlagin) / pnd_vol(k)
  
!! compute final pond volume
  pnd_vol(k) = pnd_vol(k) - pndflwo
  IF (pnd_vol(k) < 0.) THEN
    pndflwo = pndflwo + pnd_vol(k)
    pnd_vol(k) = 0.
  END IF
  
!! compute change in sediment concentration due to settling
  IF (sed_stl(k) < 1.e-6) sed_stl(k) = 0.0
  IF (pnd_sed(k) > pnd_nsed(k)) THEN
    inised = pnd_sed(k)
    pnd_sed(k) = (pnd_sed(k) - pnd_nsed(k)) * sed_stl(k) + pnd_nsed(k)
    finsed = pnd_sed(k)
    setsed = inised - finsed
    
    IF (pnd_lag(k) >= setsed) THEN
      pnd_lag(k) = pnd_lag(k) - setsed
      remsetsed = 0.
    ELSE
      remsetsed = setsed - pnd_lag(k)
      pnd_lag(k) = 0.
      IF (pnd_san(k) >= remsetsed) THEN
        pnd_san(k) = pnd_san(k) - remsetsed
        remsetsed = 0.
      ELSE
        remsetsed = remsetsed - pnd_san(k)
        pnd_san(k) = 0.
        IF (pnd_sag(k) >= remsetsed) THEN
          pnd_sag(k) = pnd_sag(k) - remsetsed
          remsetsed = 0.
        ELSE
          remsetsed = remsetsed - pnd_sag(k)
          pnd_sag(k) = 0.
          IF (pnd_sil(k) >= remsetsed) THEN
            pnd_sil(k) = pnd_sil(k) - remsetsed
            remsetsed = 0.
          ELSE
            remsetsed = remsetsed - pnd_sil(k)
            pnd_sil(k) = 0.
            IF (pnd_cla(k) >= remsetsed) THEN
              pnd_cla(k) = pnd_cla(k) - remsetsed
              remsetsed = 0.
            ELSE
              remsetsed = remsetsed - pnd_cla(k)
              pnd_cla(k) = 0.
            END IF
          END IF
        END IF
      END IF
    END IF
    
  END IF
!! compute sediment leaving pond
  pndsedo = pnd_sed(k) * pndflwo
  pndsano = pnd_san(k) * pndflwo
  pndsilo = pnd_sil(k) * pndflwo
  pndclao = pnd_cla(k) * pndflwo
  pndsago = pnd_sag(k) * pndflwo
  pndlago = pnd_lag(k) * pndflwo
  
!! net change in amount of sediment in pond for day
  pndsedc = vol * sed + pndsedin - pndsedo - pnd_sed(k) * pnd_vol(k)
  
!! determine settling rate
!! part of equation 29.1.3 in SWAT manual
  IF (i_mo >= ipnd1(k) .AND. i_mo <= ipnd2(k)) THEN
    iseas = 1
  ELSE
    iseas = 2
  END IF
  phosk = 0.
  nitrok = 0.
  phosk = psetlp(iseas,k) * pndsa * 10000. / pnd_vol(k)  !setl/mean depth
  phosk = MIN(phosk, 1.)
  nitrok = nsetlp(iseas,k) * pndsa * 10000. / pnd_vol(k) !setl/mean depth
  nitrok = MIN(nitrok, 1.)
  
!! remove nutrients by settling
!! other part of equation 29.1.3 in SWAT manual
  pnd_solp(k) = pnd_solp(k) * (1. - phosk)
  pnd_psed(k) = pnd_psed(k) * (1. - phosk)
  pnd_orgp(k) = pnd_orgp(k) * (1. - phosk)
  pnd_solpg(k) = pnd_solpg(k) * (1. - phosk)
  pnd_orgn(k) = pnd_orgn(k) * (1. - nitrok)
  pnd_no3(k) = pnd_no3(k) * (1. - nitrok)
  pnd_no3s(k) = pnd_no3s(k) * (1. - nitrok)
  pnd_no3g(k) = pnd_no3g(k) * (1. - nitrok)
  
  tpco = 0.
  IF (pnd_vol(k) + pndflwo > 0.1) THEN
    tpco = 1.e+6 * (pnd_solp(k) + pnd_orgp(k) + pnd_psed(k) +  &
        pnd_solpg(k)) / (pnd_vol(k) + pndflwo)
  ELSE
    tpco = 0.
  END IF
  chlaco = 0.
  pnd_chla(k) = 0.
  pnd_seci(k) = 0.
  IF (tpco > 1.e-4) THEN
!! equation 29.1.6 in SWAT manual
    chlaco = chlap(k) * 0.551 * (tpco**0.76)
    pnd_chla(k) = chlaco * (pnd_vol(k) + pndflwo) * 1.e-6
  END IF
  IF (chlaco > 1.e-4) THEN
!! equation 29.1.8 in SWAT manual
    pnd_seci(k) = seccip(k) * 6.35 * (chlaco**(-0.473))
  END IF
END IF

RETURN
END SUBROUTINE pond
