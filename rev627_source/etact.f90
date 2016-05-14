SUBROUTINE etact
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates potential plant transpiration for Priestley-
!!    Taylor and Hargreaves ET methods, and potential and actual soil
!!    evaporation. NO3 movement into surface soil layer due to evaporation
!!    is also calculated.


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canstor(:)   |mm H2O        |amount of water held in canopy storage
!!    elevb(:,:)   |m             |elevation at center of band in subbasin
!!    elevb_fr(:,:)|none          |fraction of subbasin area within elevation
!!                                |band
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU
!!    esco(:)      |none          |soil evaporation compensation factor
!!    ihru         |none          |HRU number
!!    ipet         |none          |code for potential ET method
!!                                |0 Priestley-Taylor method
!!                                |1 Penman/Monteith method
!!                                |2 Hargreaves method
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    pet_day      |mm H2O        |potential evapotranspiration on current day
!!                                |in HRU
!!    pot_vol(:)   |m**3 H2O      |current volume of water stored in the
!!                                |depression/impounded area
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    snoeb(:,:)   |mm H2O        |snow water content in elevation band on
!!                                |current day
!!    sol_cov(:)   |kg/ha         |amount of residue on soil surface
!!    sol_fc(:,:)  |mm H2O        |amount of water available to plants in soil
!!                                |layer at field capacity (fc - wp water)
!!    sol_nly(:)   |none          |number of soil layers in profile
!!    sol_no3(:,:) |kg N/ha       |amount of nitrogen stored in the nitrate
!!                                |pool
!!    sol_st(:,:)  |mm H2O        |amount of water stored in the soil layer on
!!                                |current day
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
!!    tavband(:,:) |deg C         |average temperature for the day in band in HRU
!!    tmpav(:)     |deg C         |average air temperature on current day for
!!                                |HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canev        |mm H2O        |amount of water evaporated from canopy
!!                                |storage
!!    ep_max       |mm H2O        |maximum amount of transpiration (plant et)
!!                                |that can occur on current day in HRU
!!    es_day       |mm H2O        |actual amount of evaporation (soil et) that
!!                                |occurs on day in HRU
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    sno3up       |kg N/ha       |amount of nitrate moving upward in the soil
!!                                |profile in watershed
!!    snoeb(:,:)   |mm H2O        |snow water content in elevation band on
!!                                |current day
!!    snoev        |mm H2O        |amount of water in snow lost through
!!                                |sublimation on current day
!!    sol_st(:,:)  |mm H2O        |amount of water stored in the soil layer on
!!                                |current day
!!    sol_sw(:)    |mm H2O        |amount of water stored in the soil profile
!!                                |on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cej          |
!!    dep          |mm            |soil depth from which evaporation will occur
!!                                |in current soil layer
!!    eaj          |none          |weighting factor to adjust PET for impact of
!!                                |plant cover
!!    effnup       |
!!    eos1         |none          |variable to hold intermediate calculation
!!                                |result
!!    eosl         |mm H2O        |maximum amount of evaporation that can occur
!!                                |from soil profile
!!    es_max       |mm H2O        |maximum amount of evaporation (soil et)
!!                                |that can occur on current day in HRU
!!    esd          |mm            |maximum soil depth from which evaporation
!!                                |is allowed to occur
!!    esleft       |mm H2O        |potenial soil evap that is still available
!!    etco         |
!!    evz          |
!!    evzp         |
!!    ib           |none          |counter
!!    j            |none          |HRU number
!!    ly           |none          |counter
!!    no3up        |kg N/ha       |amount of nitrate moving upward in profile
!!    pet          |mm H2O        |amount of PET remaining after water stored
!!                                |in canopy is evaporated
!!    sev          |mm H2O        |amount of evaporation from soil layer
!!    sumsnoeb     |mm H2O        |amount of snow in elevation bands whose air
!!                                |temperature is greater than 0 degrees C
!!    xx           |none          |variable to hold intermediate calculation
!!                                |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Expo

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
use rossmod

INTEGER :: j, ib, ly
!!    real, parameter :: esd = 500., etco = 0.80, effnup = 0.1
REAL :: esd, etco, effnup
REAL :: no3up, es_max, eos1, xx, cej, eaj, pet, esleft
REAL :: sumsnoeb, evzp, eosl, dep, evz, sev

j = 0
j = ihru
pet = 0.
pet = pet_day
!!    added statements for test of real statement above
esd = 500.
etco = 0.80
effnup = 0.1

!! evaporate canopy storage first
!! canopy storage is calculated by the model only if the Green & Ampt
!! method is used to calculate surface runoff. The curve number methods
!! take canopy effects into account in the equations. For either of the
!! CN methods, canstor will always equal zero.
pet = pet - canstor(j)
IF (pet < 0.) THEN
  canstor(j) = -pet
  canev = pet_day
  pet = 0.
  ep_max = 0.
  es_max = 0.
ELSE
  canev = canstor(j)
  canstor(j) = 0.
END IF

IF (pet > 1.0E-6) THEN
  
!! compute potential plant evap for methods other that Penman-Monteith
  IF (ipet /= 1) THEN
    IF (laiday(j) <= 3.0) THEN
      ep_max = laiday(j) * pet / 3.
    ELSE
      ep_max = pet
    END IF
    IF (ep_max < 0.) ep_max = 0.
  END IF
  
!! compute potential soil evaporation
  cej = -5.e-5
  eaj = 0.
  es_max = 0.
  eos1 = 0.
  IF (sno_hru(j) >= 0.5) THEN
    eaj = 0.5
  ELSE
    eaj = EXP(cej * (sol_cov(j)+ 0.1))
  END IF
  es_max = pet * eaj
  eos1 = pet / (es_max + ep_max + 1.e-10)
  eos1 = es_max * eos1
  es_max = MIN(es_max, eos1)
  es_max = MAX(es_max, 0.)
!        if (pot_vol(j) > 1.e-4) es_max = 0.
  
!! make sure maximum plant and soil ET doesn't exceed potential ET
!!if (pet_day < es_max + ep_max) then
!!es_max = pet_day - ep_max
  IF (pet < es_max + ep_max) THEN
    es_max = pet * es_max / (es_max + ep_max)
    ep_max = pet * ep_max / (es_max + ep_max)
  END IF
  IF (pet < es_max + ep_max) THEN
    es_max = pet - ep_max - 1.0E-6
  END IF
!!end if
  
!! initialize soil evaporation variables
  esleft = 0.
  esleft = es_max
  
!! compute sublimation
  IF (elevb_fr(1,hru_sub(j)) <= 0.) THEN
!! compute sublimation without elevation bands
    IF (tmpav(j) > 0.) THEN
      IF (sno_hru(j) >= esleft) THEN
!! take all soil evap from snow cover
        sno_hru(j) = sno_hru(j) - esleft
        snoev = snoev + esleft
        esleft = 0.
      ELSE
!! take all soil evap from snow cover then start taking from soil
        esleft = esleft - sno_hru(j)
        snoev = snoev + sno_hru(j)
        sno_hru(j) = 0.
      END IF
    END IF
  ELSE
!! elevation bands
    sumsnoeb = 0.
!! calculate air temp in elevation bands and sum snow
!! for elevation bands with temp > 0 deg C
    DO ib = 1, 10
      IF (elevb_fr(ib,hru_sub(j)) <= 0.) EXIT
      IF (tavband(ib,j) > 0.) THEN
        sumsnoeb = sumsnoeb + snoeb(ib,j) * elevb_fr(ib,hru_sub(j))
      END IF
    END DO
    
!! compute sublimation from elevation bands
    IF (sumsnoeb >= esleft .AND. sumsnoeb > 0.01) THEN
      DO ib = 1, 10
        IF (elevb_fr(ib,hru_sub(j)) <= 0.) EXIT
        IF (tavband(ib,j) > 0.) THEN
          snoev = snoev + snoeb(ib,j) * (esleft / sumsnoeb) *  &
              elevb_fr(ib,hru_sub(j))
          snoeb(ib,j) = snoeb(ib,j) - snoeb(ib,j) * (esleft / sumsnoeb)
        END IF
      END DO
    ELSE
      DO ib = 1, 10
        IF (elevb_fr(ib,hru_sub(j)) <= 0.) EXIT
        IF (tavband(ib,j) > 0.) THEN
          snoev = snoev + snoeb(ib,j) * elevb_fr(ib,hru_sub(j))
          snoeb(ib,j) = 0.
        END IF
      END DO
    END IF
    esleft = esleft - snoev
    sno_hru(j) = sno_hru(j) - snoev
  END IF
  
!! take soil evap from each soil layer
  evzp = 0.
  eosl = 0.
  eosl = esleft
  
!!-------------------OGXinSWAT Begin------------------------------
!!  Soil evaporation and transpiration
  IF (ievent>0) THEN
    solcol(j)%esmax = esleft/24.
    solcol(j)%epmax = ep_max/24.
    RETURN
  END IF
!!-------------------------End------------------------------------
  
  
  DO ly = 1, sol_nly(j)
    
!! depth exceeds max depth for soil evap (esd)
    dep = 0.
    IF (ly == 1) THEN
      dep = sol_z(1,j)
    ELSE
      dep = sol_z(ly-1,j)
    END IF
    
    IF (dep < esd) THEN
!! calculate evaporation from soil layer
      evz = 0.
      sev = 0.
      xx = 0.
      evz = eosl * sol_z(ly,j) / (sol_z(ly,j) + EXP(2.374 -  &
          .00713 * sol_z(ly,j)))
      sev = evz - evzp * esco(j)
      evzp = evz
      IF (sol_st(ly,j) < sol_fc(ly,j)) THEN
        xx =  2.5 * (sol_st(ly,j) - sol_fc(ly,j)) / sol_fc(ly,j)
        sev = sev * expo(xx)
      END IF
      sev = MIN(sev, sol_st(ly,j) * etco)
      
      IF (sev < 0.) sev = 0.
      IF (sev > esleft) sev = esleft
      
!! adjust soil storage, potential evap
      IF (sol_st(ly,j) > sev) THEN
        esleft = esleft - sev
        sol_st(ly,j) = MAX(1.e-6, sol_st(ly,j) - sev)
      ELSE
        esleft = esleft - sol_st(ly,j)
        sol_st(ly,j) = 0.
      END IF
    END IF
    
!! compute no3 flux from layer 2 to 1 by soil evaporation
    IF (ly == 2) THEN
      no3up = 0.
      no3up = effnup * sev * sol_no3(2,j) / (sol_st(2,j) + 1.e-6)
      no3up = MIN(no3up, sol_no3(2,j))
      sno3up = sno3up + no3up * hru_dafr(j)
      sol_no3(2,j) = sol_no3(2,j) - no3up
      sol_no3(1,j) = sol_no3(1,j) + no3up
    END IF
    
  END DO
  
!! update total soil water content
  sol_sw(j) = 0.
  DO ly = 1, sol_nly(j)
    sol_sw(j) = sol_sw(j) + sol_st(ly,j)
  END DO
  
!! calculate actual amount of evaporation from soil
  es_day = es_max - esleft
  IF (es_day < 0.) es_day = 0.
  
END IF

RETURN
END SUBROUTINE etact
