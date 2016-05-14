SUBROUTINE potholehr()
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates depressional areas that do not drain to the
!!    stream network (potholes) and impounded areas such as rice paddies at sub-daily time step

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr          |none          |current year of simulation
!!    evlai          |none          |leaf area index at which no evaporation
!!                                  |occurs from the water surface. This
!!                                  |variable is used in ponded HRUs (eg rice)
!!                                  |where evaporation from the water surface
!!                                  |is restricted by the plant canopy cover.
!!                                  |Evaporation from the water surface equals
!!                                  |potential ET when LAI = 0 an decreases
!!                                  |linearly to O when LAI = EVLAI
!!    hru_dafr(:)    |none          |fraction of watershed area in HRU
!!    hru_ha(:)      |ha            |area of HRU in hectares
!!    hru_slp(:)     |m/m           |average slope steepness
!!    iida           |julian date   |day being simulated (current julian date)
!!    ihru           |none          |HRU number
!!    ipot(:)        |none          |number of HRU (in subbasin) that is ponding
!!                                  |water--the HRU that the surface runoff from
!!                                  |current HRU drains into. This variable is
!!                                  |used only for rice paddys or closed
!!                                  |depressional areas
!!    imp_trig(:,:,:)|none          |release/impound action code:
!!                                  |0 begin impounding water
!!                                  |1 release impounded water
!!    irelease(:,:,:)|julian date   |date of impound/release operation
!!    laiday(:)      |none          |leaf area index
!!    nrelease(:)    |none          |sequence number of impound/release
!!                                  |operation within the year
!!    nro(:)         |none          |sequence number of year in rotation
!!    nyskip         |none          |number of years to skip output
!!                                  |summarization/printing
!!    pet_day        |mm H2O        |potential evapotranspiration on current day
!!                                  |in HRU
!!    pot_fr(:)      |km2/km2       |fraction of HRU area that drains into
!!                                  |pothole
!!    pot_no3(:)     |kg N          |amount of nitrate in pothole water body
!!    pot_no3l(:)    |1/day         |nitrate decay rate in impounded area
!!    pot_nsed(:)    |mg/L          |normal sediment concentration in impounded
!!                                  |water (needed only if current HRU is IPOT)
!!    pot_sed(:)     |metric tons   |amount of sediment in pothole water body
!!    pot_tile(:)    |m3/d          |average daily outflow to main channel from
!!                                  |tile flow if drainage tiles are installed
!!                                  |in pothole (needed only if current HRU is
!!                                  |IPOT)
!!    pot_vol(:)     |m**3 H2O      |current volume of water stored in the
!!                                  |depression/impounded area
!!    pot_volx(:)    |m**3 H2O      |maximum volume of water stored in the
!!                                  |depression/impounded area
!!    qday           |mm H2O        |surface runoff loading to main channel from
!!                                  |HRU for day
!!    rainsub(:,:)  |mm H2O        |precipitation for the time step during the
!!         |day in HRU
!!    sed_stl(:)     |kg/kg         |fraction of sediment remaining suspended in
!!                                  |impoundment after settling for one day
!!    sedyld(:)      |metric tons   |daily soil loss caused by water erosion
!!                                  |in HRU
!!    sol_k(:,:)     |mm/hr         |saturated hydraulic conductivity of soil
!!                                  |layer
!!    sol_nly(:)     |none          |number of layers in soil profile
!!    sol_por(:,:)   |none          |total porosity of soil layer expressed as
!!                                  |a fraction of the total volume
!!    sol_st(:,:)    |mm H2O        |amount of water stored in the soil layer
!!                                  |on any given day
!!    sol_sumfc(:)   |mm H2O        |amount of water held in the soil profile
!!                                  |at field capacity
!!    sol_sw(:)      |mm H2O        |amount of water stored in soil profile on
!!                                  |current day
!!    sol_z(:,:)     |mm            |depth to bottom of soil layer
!!    spadyev        |mm H2O        |average annual amount of water removed
!!                                  |from potholes by evaporation in watershed
!!    spadyo         |mm H2O        |average annual amount of water released to
!!                                  |main channel from potholes in watershed
!!    spadyrfv       |mm H2O        |average annual amount of precipitation on
!!                                  |potholes in watershed
!!    spadysp        |mm H2O        |average annual amount of water removed
!!                                  |from potholes by seepage in watershed
!!    subp(:)        |mm H2O        |precipitation for the day in HRU
!!    surqno3(:)     |kg N/ha       |amount of NO3-N in surface runoff in HRU
!!                                  |for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pot_no3(:)     |kg N          |amount of nitrate in pothole water body
!!    pot_solp(:)     |kg N          |amount of soluble p in pothole water body
!!    pot_orgn(:)     |kg N          |amount of organic N in pothole water body
!!    pot_orgp(:)     |kg N          |amount of organic P in pothole water body
!!    pot_mpa(:)     |kg N          |amount of active mineral pool P in pothole water body
!!    pot_mps(:)     |kg N          |amount of stable mineral pool P in pothole water body
!!    pot_sed(:)     |metric tons   |amount of sediment in pothole water body
!!    pot_vol(:)     |m**3 H2O      |current volume of water stored in the
!!                                  |depression/impounded area
!!    potevmm        |mm H2O        |volume of water evaporated from pothole
!!                                  |expressed as depth over HRU
!!    potflwi(:)     |m^3 H2O       |water entering pothole on day
!!    potflwo        |mm H2O        |discharge from pothole expressed as depth
!!                                  |over HRU
!!    potpcpmm       |mm H2O        |precipitation falling on pothole water body
!!                                  |expressed as depth over HRU
!!    potsa(:)       |ha            |surface area of impounded water body
!!    potsedi(:)     |metric tons   |sediment entering pothole on day
!!    potsedo        |metric tons   |sediment leaving pothole on day
!!    potsepmm       |mm H2O        |seepage from pothole expressed as depth over
!!                                  |HRU
!!    qday           |mm H2O        |surface runoff loading to main channel from
!!                                  |HRU for day
!!    sedyld(:)      |metric tons   |daily soil loss caused by water erosion
!!                                  |in HRU
!!    sol_st(:,:)    |mm H2O        |amount of water stored in the soil layer
!!                                  |on any given day
!!    sol_sw(:)      |mm H2O        |amount of water stored in soil profile on
!!                                  |current day
!!    spadyev        |mm H2O        |average annual amount of water removed
!!                                  |from potholes by evaporation in watershed
!!    spadyo         |mm H2O        |average annual amount of water released to
!!                                  |main channel from potholes in watershed
!!    spadyrfv       |mm H2O        |average annual amount of precipitation on
!!                                  |potholes in watershed
!!    spadysp        |mm H2O        |average annual amount of water removed
!!                                  |from potholes by seepage in watershed
!!    surqno3(:)     |kg N/ha       |amount of NO3-N in surface runoff in HRU
!!                                  |for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    dg          |mm            |depth of soil layer
!!    excess      |mm H2O        |amount of water moving into soil that exceeds
!!                               |storage of layer
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layers)
!!    no3loss     |kg N          |amount of nitrate lost from water body
!!    pi          |none          |pi
!!    potev       |m^3 H2O       |evaporation from impouned water body
!!    potmm       |mm H2O        |volume of water in pothole expressed as depth
!!                               |over HRU
!!    potpcp      |m^3 H2O       |precipitation falling on water body
!!    potsep      |m^3 H2O       |seepage from impounded water body
!!    sedloss     |metric tons   |amount of sediment lost from water body
!!    sedsetl     |metric tons   |amount of sediment settling out of water
!!                               |during day
!!    spillo      |m^3 H2O       |amount of water released to main channel from
!!                               |impounded water body due to spill-over
!!    stmax       |mm H2O        |maximum water storage in soil layer
!!    sumo        |m^3 H2O       |sum of all releases from water body on
!!                               |current day
!!    tileo       |m^3 H2O       |amount of water released to the main channel
!!                               |from the water body by drainage tiles
!!    yy          |none          |fraction of maximum seepage allowed to occur
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs, Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

REAL, PARAMETER :: pi = 3.1416
INTEGER :: j, ly,kk,ll,k
REAL :: potsep, sumo, potev, cnv, potpcp, spillo,no3in
REAL :: sedloss, no3loss, yy, dg, excess, stmax, sedsetl
REAL :: sanloss, silloss, claloss, sagloss, lagloss, xx
REAL :: potmm,minpsloss,minpaloss, solploss, orgnloss, orgploss
j = 0
j = ihru

!! initialize variables
potev = 0.
spillo = 0.
potpcp = 0.
potsep = 0.
sumo = 0.
potpcpmm = 0.
potevmm = 0.
potsepmm = 0.
potflwo = 0.
spillo = 0.
potsedo = 0.
potsano = 0.
potsilo = 0.
potclao = 0.
potsago = 0.
potlago = 0.
potno3o = 0.
potsolpo = 0.
potorgno = 0.
potorgpo = 0.
potmpso = 0.
potmpao = 0.
sedloss = 0.
no3loss = 0.
solploss = 0.
orgnloss = 0.
orgploss = 0.
minpsloss = 0.
minpaloss = 0.




no3in = surqno3(j) + latno3(j)    ! + gwno3(j) - don't include groundwater no3

!! conversion factors
cnv = 0.
cnv = 10. * hru_ha(j)
rto = 1.

!! iterate for time step calculation using 1hr rainfall values, hhsubp(mhru,24)
DO k=1,nstep
  qin = hhqday(k) + (latq(j) + gw_q(j)) / nstep    !inflow = water yield (surf+lat+gw)
!! when water is impounding
!        if (imp_trig(nro(j),nrelease(j),ipot(j)) == 0) then
  
!       compute surface area assuming a cone shape (m^2)
  potsa(j) = pi * (3. * pot_vol(j) / (pi * hru_slp(j)))**.6666
  potsa(j) = potsa(j) / 10000.                  !convert to ha
  IF (potsa(j) <= 0.000001) THEN
    potsa(j) = 0.001
  END IF
  IF (potsa(j) > hru_ha(j)) THEN
    potsa(j) = hru_ha(j)
  END IF
   !! u date volume of water in pothole
!       pot_fr is now the fraction of the hru draining into the pothole
!       the remainder (1-pot_fr) goes directly to runoff
  pot_vol(j) = pot_vol(j) + qin * pot_fr(j) * cnv
  potflwi(j) = qin * pot_fr(j) * cnv
  hhqday(k) = hhqday(k) * (1. - pot_fr(j))
  latq(j) = latq(j) * (1. - 1. / nstep * pot_fr(j))
  gw_q(j) = gw_q(j) * (1. - 1. / nstep * pot_fr(j))
  
!! update sediment in pothole
  pot_sed(j) = pot_sed(j) + hhsedy(j,k) * ABS(pot_fr(j))
  potsedi(j) = potsedi(j) + hhsedy(j,k) *  ! incoming sediment cumulative for the day, tons  &
      ABS(pot_fr(j))
  
  pot_san(j) = pot_san(j) + sanyld(j) / nstep * pot_fr(j)
  potsani(j) = pot_san(j)
  pot_sil(j) = pot_sil(j) + silyld(j) / nstep * pot_fr(j)
  potsili(j) = pot_sil(j)
  pot_cla(j) = pot_cla(j) + clayld(j) / nstep * pot_fr(j)
  potclai(j) = pot_cla(j)
  pot_sag(j) = pot_sag(j) + sagyld(j) / nstep * pot_fr(j)
  potsagi(j) = pot_sag(j)
  pot_lag(j) = pot_lag(j) + lagyld(j) / nstep * pot_fr(j)
  potlagi(j) = pot_lag(j)
  
  yy = 1. - pot_fr(j)
  xx = 1. - 1. / nstep * pot_fr(j)
  hhsedy(j,k) = hhsedy(j,k) * yy
  sanyld(j) = sanyld(j) * xx
  silyld(j) = silyld(j) * xx
  clayld(j) = clayld(j) * xx
  sagyld(j) = sagyld(j) * xx
  lagyld(j) = lagyld(j) * xx
  
!       update forms of N and P in pothole
  xx = pot_fr(j) * hru_ha(j) / nstep
  pot_no3(j) = pot_no3(j) + no3in * xx
  pot_solp(j) = pot_solp(j) + surqsolp(j) * xx
  pot_orgn(j) = pot_orgn(j) + sedorgn(j) * xx
  pot_orgp(j) = pot_orgp(j) + sedorgp(j) * xx
  pot_mps(j) = pot_mps(j) + sedminps(j) * xx
  pot_mpa(j) = pot_mpa(j) + sedminpa(j) * xx
!       track incoming loads
  pot_sedin(j)= hhsedy(j,k) * pot_fr(j) ! incoming sediment during this time step, tons
  pot_no3i(j) = pot_no3i(j) + no3in * xx
  pot_solpi(j) = pot_solpi(j) + surqsolp(j) * xx
  pot_orgni(j) = pot_orgni(j) + sedorgn(j) * xx
  pot_orgpi(j) = pot_orgpi(j) + sedorgp(j) * xx
  pot_mpsi(j) = pot_mpsi(j) + sedminps(j) * xx
  pot_mpai(j) = pot_mpai(j) + sedminpa(j) * xx
  
  
  
!       update forms of N and P in surface runoff
  yy = 1. - pot_fr(j) / nstep
  surqno3(j) = surqno3(j) * yy
  latno3(j) = latno3(j) * yy
!        gwno3(j) = gwno3(j) * yy
  surqsolp(j) = surqsolp(j) * yy
  sedorgn(j) = sedorgn(j) * yy
  sedorgp(j) = sedorgp(j) * yy
  sedminps(j) = sedminps(j) * yy
  sedminpa(j) = sedminpa (j) * yy
  
!       if overflow, then send the overflow to the HRU surface flow
  IF (pot_vol(j) > pot_volx(j)) THEN
    hhqday(k) = hhqday(k) + (pot_vol(j)- pot_volx(j)) / cnv
    spillo = pot_vol(j)- pot_volx(j)
    pot_vol(j) = pot_volx(j)
    xx = spillo / (spillo + pot_volx(j))
    potsedo = potsedo + pot_sed(j) * xx
    potsano = potsano + pot_san(j) * xx
    potsilo = potsilo + pot_sil(j) * xx
    potclao = potclao + pot_cla(j) * xx
    potsago = potsago + pot_sag(j) * xx
    potlago = potlago + pot_lag(j) * xx
    potno3o = potno3o + pot_no3(j) * xx
    potsolpo = potsolpo + pot_solp(j) * xx
    potorgno = potorgno + pot_orgn(j) * xx
    potorgpo = potorgpo + pot_orgp(j) * xx
    potmpso = potmpso + pot_mps(j) * xx
    potmpao = potmpao + pot_mpa(j) * xx
    
    pot_sed(j) = pot_sed(j) - potsedo
    pot_san(j) = pot_san(j) - potsano
    pot_sil(j) = pot_sil(j) - potsilo
    pot_cla(j) = pot_cla(j) - potclao
    pot_sag(j) = pot_sag(j) - potsago
    pot_lag(j) = pot_lag(j) - potlago
    
    pot_no3(j) = pot_no3(j) - potno3o
    pot_solp(j) = pot_solp(j) - potsolpo
    pot_orgn(j) = pot_orgn(j) - potorgno
    pot_orgp(j) = pot_orgp(j) - potorgpo
    pot_mps(j) = pot_mps(j) - potmpso
    pot_mpa(j) = pot_mpa(j) - potmpao
    
    hhsedy(j,k) = hhsedy(j,k) + potsedo
    sanyld(j) = sanyld(j) + potsano
    silyld(j) = silyld(j) + potsilo
    clayld(j) = clayld(j) + potclao
    pot_sag(j) = sagyld(j) + potsago
    lagyld(j) = lagyld(j) + potlago
    
    
    
    
    surqno3(j) = surqno3(j) + potno3o / cnv
    surqsolp(j) = surqsolp(j) + potsolpo / cnv
    sedorgn(j) = sedorgn(j) + potorgno / cnv
    sedorgp(j) = sedorgp(j) + potorgpo / cnv
    sedminps(j) = sedminps(j) + potmpso / cnv
    sedminpa(j) = sedminpa(j) + potmpao / cnv
  END IF       !! if overflow
  
!      If no overflow, compute settling and losses, surface inlet tile
!      flow, evap, seepage, and redistribute soil water
  IF (pot_vol(j) > 1.e-6) THEN
!        compute settling -clay and silt based on fall velocity (v=411*d2) d=mm, v=m/hr
    pot_depth = pot_vol(j) / potsa(j) / 10.         !m3/ha/10 = mm
    IF (pot_depth > 10./nstep) THEN        !assume clay v(fall)= 10 mm/d = 10 / nstep mm/dt
      drcla = 1. - .5 * 10. / nstep / pot_depth
    ELSE
      drcla = .5 * pot_depth / (10. / nstep)
    END IF
    pot_cla(j) = drcla * pot_cla(j)
    
    IF (pot_depth > 1000./nstep) THEN    !assume silt v(fall)= 1000 mm/d = 1000/nstep mm/dt
      drsil = 1. - .5 * (1000. / nstep) / pot_depth
    ELSE
      drsil = .5 * pot_depth / (1000. / nstep)
    END IF
    pot_sil(j) = drsil * pot_sil(j)
!        assume complete settlling of all other sizes (dr = 0)
    pot_san(j) = 0.
    pot_sag(j) = 0.
    pot_lag(j) = 0.
    
!        compute total delivery ratio for pot_sed
    drtot = (pot_cla(j) + pot_sil(j) + pot_san(j) + pot_sag(j) +  &
        pot_lag(j)) / (potclai(j) + potsili(j) + potsani(j) +  &
        potsagi(j) + potlagi(j))
    pot_sed(j) = drtot * pot_sed(j)
    
!        compute organic settling assuming an enrichment ratio of 3 on clay (0.75)
!        delivery of organics is 0.75*dr(clay)- assuming dr on all non-clay = 1
    pot_orgn(j) = .75 * drcla * pot_orgn(j)
    pot_orgp(j) = .75 * drcla * pot_orgp(j)
    pot_mps(j) = .75 * drcla * pot_mps(j)
    pot_mpa(j) = .75 * drcla * pot_mpa(j)
    
    pot_no3(j) = pot_no3(j) * (1. - pot_no3l(j) / nstep)
    pot_solp(j) = pot_solp(j) * (1. - pot_no3l(j) / nstep)
!         hlife_pot = 20.    !!assume half life of 20 days
!         pot_no3(j) = Exp(-.693 / hlife_pot) * pot_no3(j)
!         pot_solp(j) = Exp(-.693 / hlife_pot) * pot_solp(j)
    
!! limit seepage into soil if profile is near field capacity
    yy = 0.
    IF (sol_sw(j) / sol_sumfc(j) < .5) THEN
      yy = 1.
    ELSE IF (sol_sw(j) / sol_sumfc(j) < 1.) THEN
      yy = 1. - sol_sw(j) / sol_sumfc(j)
    END IF
    
!        calculate seepage into soil
    potsep = yy * sol_k(1,j) * potsa(j) * 240./nstep
    potsep = MIN(potsep, pot_vol(j))
    pot_vol(j) = pot_vol(j) - potsep
    pot_seep(j)= pot_seep(j)+ potsep
    
    sol_st(1,j) = sol_st(1,j) + potsep / hru_ha(j) / 10.
    
!        redistribute water so that no layer exceeds maximum storage
    DO ly = 1, sol_nly(j)
      dg = 0.
      stmax = 0.
      excess = 0.
      IF (ly == 1) THEN
        dg = sol_z(ly,j)
      ELSE
        dg = sol_z(ly,j) - sol_z(ly-1,j)
      END IF
      stmax = sol_por(ly,j) * dg
      IF (sol_st(ly,j) <= stmax) EXIT
      excess = sol_st(ly,j) - stmax
      sol_st(ly,j) = stmax
      IF (ly + 1 <= sol_nly(j)) THEN
        sol_st(ly+1,j) = sol_st(ly+1,j) + excess
      END IF
    END DO
    
!         recompute total soil water
    sol_sw(j) = 0.
    DO ly = 1, sol_nly(j)
      sol_sw(j) = sol_sw(j) + sol_st(ly,j)
    END DO
    
!       compute evaporation from water surface  NUBZ - I moved this before the if(pot_vol
    IF (laiday(j) < evlai) THEN
      potev = (1. - laiday(j) / evlai) * pet_day * evpot(j)/ nstep
      potev = 10. * potev * potsa(j)       !!units mm => m^3
      potev = MIN(potev, pot_vol(j))
      pot_vol(j) = pot_vol(j) - potev
      
      pot_evap(j)= pot_evap(j) + potev
    END IF
    
    IF (iprint==3) THEN
      WRITE (125,2000)i,j,curyr,k,pot_vol(j),potsa(j),spillo,potsep,  &
          potev,sol_sw(j),potpcpmm,potflwi(j) / cnv,  &
          potsedi(j) / hru_ha(j),potflow,potsedo / hru_ha(j)
    END IF
    
    IF (pot_vol(j) > 1.e-6) THEN
!         compute flow from surface inlet tile
      tileo = MIN(pot_tile(j)/nstep, pot_vol(j))
      sumo = sumo + tileo
      tile_out(j) = tile_out(j) + tileo
!          if (tile_out(j) > 1.e-6) then    !! Srin 12/15/2011
      sedloss = pot_sed(j) * tileo / pot_vol(j)
      sedloss = MIN(sedloss, pot_sed(j))
      
      pot_sed(j) = pot_sed(j) - sedloss
      potsedo = potsedo + sedloss
      hhsedy(j,k) = hhsedy(j,k) + sedloss
      no3loss = pot_no3(j) *  tileo / pot_vol(j)
      no3loss = MIN(no3loss, pot_no3(j))
      pot_no3(j) = pot_no3(j) - no3loss
      surqno3(j) = surqno3(j) + no3loss / hru_ha(j)
      
      solploss = pot_solp(j) *  tileo / pot_vol(j)
      solploss = MIN(solploss, pot_solp(j))
      pot_solp(j) = pot_solp(j) - solploss
      surqsolp(j) = surqsolp(j) + solploss / hru_ha(j)
      orgnloss = pot_orgn(j) *  tileo / pot_vol(j)
      orgnloss = MIN(orgnloss, pot_orgn(j))
      pot_orgn(j) = pot_orgn(j) - orgnloss
      sedorgn(j) = sedorgn(j) + orgnloss / hru_ha(j)
      orgploss = pot_orgp(j) *  tileo / pot_vol(j)
      orgploss = MIN(orgploss, pot_orgp(j))
      pot_orgp(j) = pot_orgp(j) - orgploss
      sedorgp(j) = sedorgp(j) + orgploss / hru_ha(j)
      
      minpsloss = pot_mps(j) *  tileo / pot_vol(j)
      minpsloss = MIN(minpsloss, pot_mps(j))
      pot_mps(j) = pot_mps(j) - minpsloss
      sedminps(j) = sedminps(j) + minpsloss / hru_ha(j)
      
      minpaloss = pot_mpa(j) *  tileo / pot_vol(j)
      minpaloss = MIN(minpaloss, pot_mpa(j))
      pot_mpa(j) = pot_mpa(j) - minpaloss
      sedminpa(j) = sedminpa(j) + minpaloss / hru_ha(j)
      
      sanloss = pot_san(j) *  tileo / pot_vol(j)
      pot_san(j) = pot_san(j) - sanloss
      potsano = potsano + sanloss
      sanyld(j) = sanyld(j) + sanloss
      
      silloss = pot_sil(j) *  tileo / pot_vol(j)
      pot_sil(j) = pot_sil(j) - silloss
      
      potsilo = potsilo + silloss
      silyld(j) = silyld(j) + silloss
      
      claloss = pot_cla(j) *  tileo / pot_vol(j)
      pot_cla(j) = pot_cla(j) - claloss
      potclao = potclao + claloss
      clayld(j) = clayld(j) + claloss
      
      sagloss = pot_sag(j) *  tileo / pot_vol(j)
      pot_sag(j) = pot_sag(j) - sagloss
      potsago = potsago + sagloss
      sagyld(j) = sagyld(j) + sagloss
      
      lagloss = pot_lag(j) *  tileo / pot_vol(j)
      pot_lag(j) = pot_lag(j) - lagloss
      potlago = potlago + lagloss
      lagyld(j) = lagyld(j) + lagloss
!         end if                           !!!!srin 12/15/2011
!         track loadings removed via tile flow
      tile_sedo(j)= tile_sedo(j)+ sedloss
      tile_no3o(j)= tile_no3o(j)+ no3loss
      tile_solpo(j)= tile_solpo(j)+ solploss
      tile_orgno(j)= tile_orgno(j)+ orgnloss
      tile_orgpo(j)= tile_orgpo(j)+ orgploss
      tile_minpso(j)= tile_minpso(j)+ minpsloss
      tile_minpao(j)= tile_minpao(j)+ minpaloss
      
      IF (pot_vol(j) > 0. .AND. potsa(j) > 0.0) THEN
        potpcpmm = rainsub(j,k)
        potevmm = potev / potsa(j) / 10.
        potsepmm = potsep / potsa(j) / 10.
        potflwo = sumo / potsa(j) / 10.
      END IF
      
      pot_vol(j) = pot_vol(j) - tileo
      hhqday(k) = hhqday(k) + tileo / cnv
    END IF
  END IF
END DO

!       summary calculations
IF (curyr > nyskip) THEN
  potmm = 0.
  IF (pot_vol(j) > 0. .AND. potsa(j) > 0.0) THEN
    potmm = pot_vol(j) / potsa(j) / 10.
    
  END IF
  spadyo = spadyo + potflwo * hru_dafr(j)
  spadyev = spadyev + potevmm * hru_dafr(j)
  spadysp = spadysp + potsepmm * hru_dafr(j)
  spadyrfv = spadyrfv + potpcpmm * hru_dafr(j)
  
  
END IF

RETURN
1000    FORMAT (1X,i4,2X,9(f8.2,2X))
2000    FORMAT (4I4,11F10.2)
END SUBROUTINE potholehr
