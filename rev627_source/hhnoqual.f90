SUBROUTINE hhnoqual
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient calculations. No trans-
!!    formations are calculated

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0              |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    algae(:)         |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:)      |mg N/L        |ammonia concentration in reach
!!    chlora(:)        |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)       |mg P/L        |dissolved P concentration in reach
!!    hhvaroute(2,:,:) |m^3 H2O       |water
!!    hhvaroute(4,:,:) |kg N          |organic nitrogen
!!    hhvaroute(5,:,:) |kg P          |organic posphorus
!!    hhvaroute(6,:,:) |kg N          |nitrate
!!    hhvaroute(7,:,:) |kg P          |soluble phosphorus
!!    hhvaroute(13,:,:)|kg            |chlorophyll-a
!!    hhvaroute(14,:,:)|kg N          |ammonium
!!    hhvaroute(15,:,:)|kg N          |nitrite
!!    hhvaroute(16,:,:)|kg            |carbonaceous biological oxygen demand
!!    hhvaroute(17,:,:)|kg O2         |dissolved oxygen
!!    hrchwtr(ii)      |m^3 H2O       |water stored in reach at beginning of day
!!    hrtwtr(:)        |m^3 H2O       |flow out of reach
!!    inum1            |none          |reach number
!!    inum2            |none          |inflow hydrograph storage location number
!!    nitraten(:)      |mg N/L        |nitrate concentration in reach
!!    nitriten(:)      |mg N/L        |nitrite concentration in reach
!!    organicn(:)      |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:)      |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:)      |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                    |reach
!!    rch_dox(:)       |mg O2/L       |dissolved oxygen concentration in reach
!!    rnum1            |none          |fraction of overland flow
!!    tmpav(:)         |deg C         |average air temperature on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algae(:)    |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:) |mg N/L        |ammonia concentration in reach
!!    chlora(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)  |mg P/L        |dissolved phosphorus concentration in reach
!!    nitraten(:) |mg N/L        |nitrate concentration in reach
!!    nitriten(:) |mg N/L        |nitrite concentration in reach
!!    organicn(:) |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:) |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:) |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                               |reach
!!    rch_dox(:)  |mg O2/L       |dissolved oxygen concentration in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    jrch        |none          |reach number
!!    nh3con      |mg N/L        |initial ammonia concentration in reach
!!    nitratin    |mg N/L        |nitrate concentration in inflow
!!    nitritin    |mg N/L        |nitrite concentration in inflow
!!    no2con      |mg N/L        |initial nitrite concentration in reach
!!    no3con      |mg N/L        |initial nitrate concentration in reach
!!    o2con       |mg O2/L       |initial dissolved oxygen concentration in
!!                               |reach
!!    orgncon     |mg N/L        |initial organic N concentration in reach
!!    orgnin      |mg N/L        |organic N concentration in inflow
!!    orgpcon     |mg P/L        |initial organic P concentration in reach
!!    orgpin      |mg P/L        |organic P concentration in inflow
!!    solpcon     |mg P/L        |initial soluble P concentration in reach
!!    wtmp        |deg C         |temperature of water in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: jrch, ii
REAL :: wtrin, chlin, algin, orgnin, ammoin, nitratin, nitritin
REAL :: orgpin, dispin, cbodin, disoxin, wtmp
REAL :: orgpcon, solpcon, cbodcon, o2con, wtrtot
REAL :: algcon, orgncon, nh3con, no2con, no3con

jrch = 0
jrch = inum1

!! hourly loop
DO ii = 1, nstep
!! initialize water flowing into reach
  wtrin = 0.
  wtrin = hhvaroute(2,inum2,ii) * (1. - rnum1)
  
  IF (hrtwtr(ii) / (idt * 60.) > 0.01 .AND. wtrin > 0.01) THEN
!! concentrations
!! initialize inflow concentrations
    chlin = 0.
    algin = 0.
    orgnin = 0.
    ammoin = 0.
    nitritin = 0.
    nitratin = 0.
    orgpin = 0.
    dispin = 0.
    cbodin = 0.
    disoxin = 0.
    IF (wtrin > 0.001) THEN
      chlin = 1000. * hhvaroute(13,inum2,ii) * (1. - rnum1) / wtrin
      algin = 1000. * chlin / ai0        !! QUAL2E equation III-1
      orgnin = 1000. * hhvaroute(4,inum2,ii) * (1. - rnum1) / wtrin
      ammoin = 1000. * hhvaroute(14,inum2,ii) * (1. - rnum1) / wtrin
      nitritin = 1000. * hhvaroute(15,inum2,ii) * (1. - rnum1) / wtrin
      nitratin = 1000. * hhvaroute(6,inum2,ii) * (1. - rnum1) / wtrin
      orgpin = 1000. * hhvaroute(5,inum2,ii) * (1. - rnum1) / wtrin
      dispin = 1000. * hhvaroute(7,inum2,ii) * (1. - rnum1) / wtrin
      cbodin = 1000. * hhvaroute(16,inum2,ii) * (1. - rnum1) / wtrin
      disoxin= 1000. * hhvaroute(17,inum2,ii) * (1. - rnum1) / wtrin
    END IF
    
    IF (chlin < 1.e-6) chlin = 0.0
    IF (algin < 1.e-6) algin = 0.0
    IF (orgnin < 1.e-6) orgnin = 0.0
    IF (ammoin < 1.e-6) ammoin = 0.0
    IF (nitritin < 1.e-6) nitritin = 0.0
    IF (nitratin < 1.e-6) nitratin = 0.0
    IF (orgpin < 1.e-6) orgpin = 0.0
    IF (dispin < 1.e-6) dispin = 0.0
    IF (cbodin < 1.e-6) cbodin = 0.0
    IF (disoxin < 1.e-6) disoxin = 0.0
    
!! initialize concentration of nutrient in reach
    wtrtot = 0.
    algcon = 0.
    orgncon = 0.
    nh3con = 0.
    no2con = 0.
    no3con = 0.
    orgpcon = 0.
    solpcon = 0.
    cbodcon = 0.
    o2con = 0.
    wtrtot = wtrin + hrchwtr(ii)
    IF (ii == 1) THEN
      algcon = (algin * wtrin + algae(jrch) * hrchwtr(ii)) / wtrtot
      orgncon = (orgnin * wtrin + organicn(jrch) * hrchwtr(ii)) / wtrtot
      nh3con = (ammoin * wtrin + ammonian(jrch) * hrchwtr(ii)) / wtrtot
      no2con = (nitritin * wtrin + nitriten(jrch) * hrchwtr(ii)) / wtrtot
      no3con = (nitratin * wtrin + nitraten(jrch) * hrchwtr(ii)) / wtrtot
      orgpcon = (orgpin * wtrin + organicp(jrch) * hrchwtr(ii)) / wtrtot
      solpcon = (dispin * wtrin + disolvp(jrch) * hrchwtr(ii)) / wtrtot
      cbodcon = (cbodin * wtrin + rch_cbod(jrch) * hrchwtr(ii)) / wtrtot
      o2con = (disoxin * wtrin + rch_dox(jrch) * hrchwtr(ii)) / wtrtot
    ELSE
      algcon = (algin * wtrin + halgae(ii-1) * hrchwtr(ii)) / wtrtot
      orgncon = (orgnin * wtrin + horgn(ii-1) * hrchwtr(ii)) / wtrtot
      nh3con = (ammoin * wtrin + hnh4(ii-1) * hrchwtr(ii)) / wtrtot
      no2con = (nitritin * wtrin + hno2(ii-1) * hrchwtr(ii)) / wtrtot
      no3con = (nitratin * wtrin + hno3(ii-1) * hrchwtr(ii)) / wtrtot
      orgpcon = (orgpin * wtrin + horgp(ii-1) * hrchwtr(ii)) / wtrtot
      solpcon = (dispin * wtrin + hsolp(ii-1) * hrchwtr(ii)) / wtrtot
      cbodcon = (cbodin * wtrin + hbod(ii-1) * hrchwtr(ii)) / wtrtot
      o2con = (disoxin * wtrin + hdisox(ii-1) * hrchwtr(ii)) / wtrtot
    END IF
    
    IF (algcon < 1.e-6) algon = 0.0
    IF (orgncon < 1.e-6) orgncon = 0.0
    IF (nh3con < 1.e-6) nh3con = 0.0
    IF (no2con < 1.e-6) no2con = 0.0
    IF (no3con < 1.e-6) no3con = 0.0
    IF (orgpcon < 1.e-6) orgpcon = 0.0
    IF (solpcon < 1.e-6) solpcon = 0.0
    IF (cbodcon < 1.e-6) cbodcon = 0.0
    IF (o2con < 1.e-6) o2con = 0.0
    
    
!! calculate temperature in stream
!! Stefan and Preudhomme. 1993.  Stream temperature estimation
!! from air temperature.  Water Res. Bull. p. 27-45
!! SWAT manual equation 2.3.13
    wtmp = 0.
    
    wtmp = 5.0 + 0.75 * tmpav(jrch)
    IF (wtmp <= 0.) wtmp = 0.1
    
!! end initialize concentrations
    
!! calculate algal biomass concentration at end of hour
!! (phytoplanktonic algae)
    halgae(ii) = 0.
    halgae(ii) = algcon
    IF (halgae(ii) < 0.) halgae(ii) = 0.
    
!! calculate chlorophyll-a concentration at end of hour
    hchla(ii) = 0.
    hchla(ii) = halgae(ii) * ai0 / 1000.
    
!! oxygen calculations
!! calculate carbonaceous biological oxygen demand at end of hour
    hbod(ii) = 0.
    hbod(ii) = cbodcon
    IF (hbod(ii) < 0.) hbod(ii) = 0.
    
!! calculate dissolved oxygen concentration if reach at
!! end of hour
    hdisox(ii) = 0.
    hdisox(ii) = o2con
    IF (hdisox(ii) < 0.) hdisox(ii) = 0.
!! end oxygen calculations
    
!! nitrogen calculations
!! calculate organic N concentration at end of hour
    horgn(ii) = 0.
    horgn(ii) = orgncon
    IF (horgn(ii) < 0.) horgn(ii) = 0.
    
!! calculate ammonia nitrogen concentration at end of hour
    hnh4(ii) = 0.
    hnh4(ii) = nh3con
    IF (hnh4(ii) < 0.) hnh4(ii) = 0.
    
!! calculate concentration of nitrite at end of hour
    hno2(ii) = 0.
    hno2(ii) = no2con
    IF (hno2(ii) < 0.) hno2(ii) = 0.
    
!! calculate nitrate concentration at end of hour
    hno3(ii) = 0.
    hno3(ii) = no3con
    IF (hno3(ii) < 0.) hno3(ii) = 0.
!! end nitrogen calculations
    
!! phosphorus calculations
!! calculate organic phosphorus concentration at end of hour
    horgp(ii) = 0.
    horgp(ii) = orgpcon
    IF (horgp(ii) < 0.) horgp(ii) = 0.
    
!! calculate dissolved phosphorus concentration at end of hour
    hsolp(ii) = 0.
    hsolp(ii) = solpcon
    IF (hsolp(ii) < 0.) hsolp(ii) = 0.
!! end phosphorus calculations
    
  ELSE
!! all water quality variables set to zero when no flow
    algin = 0.0
    chlin = 0.0
    orgnin = 0.0
    ammoin = 0.0
    nitritin = 0.0
    nitratin = 0.0
    orgpin = 0.0
    dispin = 0.0
    cbodin = 0.0
    disoxin = 0.0
    halgae(ii) = 0.0
    hchla(ii) = 0.0
    horgn(ii) = 0.0
    hnh4(ii) = 0.0
    hno2(ii) = 0.0
    hno3(ii) = 0.0
    horgp(ii) = 0.0
    hsolp(ii) = 0.0
    hbod(ii) = 0.0
    hdisox(ii) = 0.0
  END IF
  IF (halgae(ii) < 1.e-6) halgae(ii) = 0.0
  IF (hchla(ii) < 1.e-6) hchla(ii) = 0.0
  IF (horgn(ii) < 1.e-6) horgn(ii) = 0.0
  IF (hnh4(ii) < 1.e-6) hnh4(ii) = 0.0
  IF (hno2(ii) < 1.e-6) hno2(ii) = 0.0
  IF (hno3(ii) < 1.e-6) hno3(ii) = 0.0
  IF (horgp(ii) < 1.e-6) horgp(ii) = 0.0
  IF (hsolp(ii) < 1.e-6) hsolp(ii) = 0.0
  IF (hbod(ii) < 1.e-6) hbod(ii) = 0.0
  IF (hdisox(ii) < 1.e-6) hdisox(ii) = 0.0
END DO
!! end hourly loop

!! set end of day concentrations
algae(jrch) = halgae(nstep)
chlora(jrch) = hchla(nstep)
organicn(jrch) = horgn(nstep)
ammonian(jrch) = hnh4(nstep)
nitriten(jrch) = hno2(nstep)
nitraten(jrch) = hno3(nstep)
organicp(jrch) = horgp(nstep)
disolvp(jrch) = hsolp(nstep)
rch_cbod(jrch) = hbod(nstep)
rch_dox(jrch) = hdisox(nstep)

IF (algae(jrch) < 1.e-6) algae(jrch) = 0.0
IF (chlora(jrch) < 1.e-6) chlora(jrch) = 0.0
IF (organicn(jrch) < 1.e-6) organicn(jrch) = 0.0
IF (ammonian(jrch) < 1.e-6) ammonian(jrch) = 0.0
IF (nitriten(jrch) < 1.e-6) nitriten(jrch) = 0.0
IF (nitraten(jrch) < 1.e-6) nitraten(jrch) = 0.0
IF (organicp(jrch) < 1.e-6) organicp(jrch) = 0.0
IF (disolvp(jrch) < 1.e-6) disolvp(jrch) = 0.0
IF (rch_cbod(jrch) < 1.e-6) rch_cbod(jrch) = 0.0
IF (rch_dox(jrch) < 1.e-6) rch_dox(jrch) = 0.0

RETURN
END SUBROUTINE hhnoqual
