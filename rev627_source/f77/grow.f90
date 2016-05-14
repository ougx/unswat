SUBROUTINE grow
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adjusts plant biomass, leaf area index, and canopy height
!!    taking into account the effect of water, temperature and nutrient stresses
!!    on the plant

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blai(:)     |none             |maximum (potential) leaf area index
!!    auto_nstrs(:) |none           |nitrogen stress factor which triggers
!!                                  |auto fertilization
!!    bio_e(:)    |(kg/ha)/(MJ/m**2)|biomass-energy ratio
!!                                  |The potential (unstressed) growth rate per
!!                                  |unit of intercepted photosynthetically
!!                                  |active radiation.
!!    bio_ms(:)   |kg/ha            |land cover/crop biomass (dry weight)
!!    bio_targ(:,:,:)|kg/ha          |biomass target
!!    chtmx(:)    |m                |maximum canopy height
!!    co2(:)      |ppmv             |CO2 concentration
!!    curyr       |none             |current year of simulation
!!    dlai(:)     |none             |fraction of growing season when leaf
!!                                  |area declines
!!    ep_day      |mm H2O           |actual amount of transpiration that occurs
!!                                  |on day in HRU
!!    es_day      |mm H2O           |actual amount of evaporation (soil et) that
!!                                  |occurs on day in HRU
!!    hru_dafr(:) |km**2/km**2      |fraction of watershed area in HRU
!!    hru_ra(:)   |MJ/m^2           |solar radiation for the day in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)  |harvest index: crop yield/aboveground
!!                                  |biomass
!!    icr(:)      |none             |sequence number of crop grown within the
!!                                  |current year
!!    idc(:)      |none             |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
!!    idorm(:)    |none             |dormancy status code:
!!                                  |0 land cover growing (not dormant)
!!                                  |1 land cover dormant
!!    idplt(:)    |none             |land cover code from crop.dat
!!    igro(:)     |none             |land cover status code:
!!                                  |0 no land cover currently growing
!!                                  |1 land cover growing
!!    ihru        |none             |HRU number
!!    lai_yrmx(:) |none             |maximum leaf area index for the year in the
!!                                  |HRU
!!    laiday(:)   |m**2/m**2        |leaf area index
!!    laimxfr(:)  |
!!    leaf1(:)    |none             |1st shape parameter for leaf area
!!                                  |development equation.
!!    leaf2(:)    |none             |2nd shape parameter for leaf area
!!                                  |development equation.
!!    nro(:)      |none             |sequence number of year in rotation
!!    nyskip      |none             |number of years output summarization
!!                                  |and printing is skipped
!!    olai(:)     |
!!    pet_day     |mm H2O           |potential evapotranspiration on current day
!!                                  |in HRU
!!    phu_plt(:)  |heat units       |total number of heat units to bring plant
!!                                  |to maturity
!!    phuacc(:)   |none             |fraction of plant heat units accumulated
!!    plt_et(:)   |mm H2O           |actual ET simulated during life of plant
!!    plt_pet(:)  |mm H2O           |potential ET simulated during life of plant
!!    strsn(:)    |none             |fraction of potential plant growth achieved
!!                                  |on the day where the reduction is caused by
!!                                  |nitrogen stress
!!    strsp(:)    |none             |fraction of potential plant growth achieved
!!                                  |on the day where the reduction is caused by
!!                                  |phosphorus stress
!!    strstmp(:)  |none             |fraction of potential plant growth achieved
!!                                  |on the day in HRU where the reduction is
!!                                  |caused by temperature stress
!!    strsw(:)    |none             |fraction of potential plant growth achieved
!!                                  |on the day where the reduction is caused by
!!                                  |water stress
!!    t_base(:)   |deg C            |minimum temperature for plant growth
!!    tmpav(:)    |deg C            |average air temperature on current day in
!!                                  |HRU
!!    vpd         |kPa              |vapor pressure deficit
!!    wac21(:)    |none             |1st shape parameter for radiation use
!!                                  |efficiency equation.
!!    wac22(:)    |none             |2nd shape parameter for radiation use
!!                                  |efficiency equation.
!!    wavp(:)     |none             |Rate of decline in radiation use efficiency
!!                                  |as a function of vapor pressure deficit
!!    wshd_nstrs  |stress units     |average annual number of nitrogen stress
!!                                  |units in watershed
!!    wshd_pstrs  |stress units     |average annual number of phosphorus stress
!!                                  |units in watershed
!!    wshd_tstrs  |stress units     |average annual number of temperature stress
!!                                  |units in watershed
!!    wshd_wstrs  |stress units     |average annual number of water stress units
!!                                  |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    bioday      |kg            |biomass generated on current day in HRU
!!    cht(:)      |m             |canopy height
!!    hvstiadj(:) |none          |harvest index adjusted for water stress
!!    lai_yrmx(:) |none          |maximum leaf area index for the year in the
!!                               |HRU
!!    laimxfr(:)  |
!!    olai(:)     |
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plt_et(:)   |mm H2O        |actual ET simulated during life of plant
!!    plt_pet(:)  |mm H2O        |potential ET simulated during life of plant
!!    rsr1c(:)    |              |initial root to shoot ratio at beg of growing season
!!    rsr2c(:)    |              |root to shoot ratio at end of growing season
!!    rwt(:)      |none          |fraction of total plant biomass that is
!!                               |in roots
!!    wshd_nstrs  |stress units  |average annual number of nitrogen stress
!!                               |units in watershed
!!    wshd_pstrs  |stress units  |average annual number of phosphorus stress
!!                               |units in watershed
!!    wshd_tstrs  |stress units  |average annual number of temperature stress
!!                               |units in watershed
!!    wshd_wstrs  |stress units  |average annual number of water stress units
!!                               |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    beadj       |(kg/ha)/(MJ/m**2)|radiation-use efficiency for a given CO2
!!                                  |concentration
!!    delg        |
!!    deltalai    |
!!    f           |none             |fraction of plant's maximum leaf area index
!!                                  |corresponding to a given fraction of
!!                                  |potential heat units for plant
!!    ff          |
!!    j           |none             |HRU number
!!    laimax      |none             |maximum leaf area index
!!    par         |MJ/m^2           |photosynthetically active radiation
!!    reg         |none             |stress factor that most limits plant growth
!!                                  |on current day
!!    ruedecl     |none             |decline in radiation use efficiency for the
!!                                  |plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min, Sqrt
!!    SWAT: tstr, nup, npup, anfert

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j
REAL :: delg, par, ruedecl, beadj, reg, f, ff, deltalai
REAL :: laimax, rto

j = 0
j = ihru
rto = 1.

!! plant will not undergo stress if dormant
IF (idorm(j) == 1) RETURN
idp = idplt(j)

!! update accumulated heat units for the plant
delg = 0.
IF (phu_plt(j) > 0.1) THEN
  delg = (tmpav(j) - t_base(idp)) / phu_plt(j)
END IF
IF (delg < 0.) delg = 0.
phuacc(j) = phuacc(j) + delg


!! if plant hasn't reached maturity
IF (phuacc(j) <= 1.) THEN
  
!! compute temperature stress - strstmp(j)
  CALL tstr
  
!! calculate optimal biomass
  
!! calculate photosynthetically active radiation
  par = 0.
  par = .5 * hru_ra(j) * (1. - EXP(-ext_coef(idp) * (laiday(j) + .05)))
  
!! adjust radiation-use efficiency for CO2
  beadj = 0.
  IF (co2(hru_sub(j)) > 330.) THEN
    beadj = 100. * co2(hru_sub(j)) / (co2(hru_sub(j)) +  &
        EXP(wac21(idp) - co2(hru_sub(j)) * wac22(idp)))
  ELSE
    beadj = bio_e(idp)
  END IF
  
!! adjust radiation-use efficiency for vapor pressure deficit
!!assumes vapor pressure threshold of 1.0 kPa
  IF (vpd > 1.0) THEN
    ruedecl = 0.
    ruedecl = vpd - 1.0
    beadj = beadj - wavp(idp) * ruedecl
    beadj = MAX(beadj, 0.27 * bio_e(idp))
  END IF
  
  bioday = beadj * par
  IF (bioday < 0.) bioday = 0.
  
!! calculate plant uptake of nitrogen and phosphorus changed by cibin 02/15/12
!! to make sure no plant N and P uptake under, temperature, water and aeration stress.
  reg = 0.
  reg = MIN(strsw(j), strstmp(j), strsa(j))
  IF (reg < 0.) reg = 0.
  
  IF (reg > 0.) THEN
    CALL nup
    CALL npup
  ELSE
    strsn(j) = 1.
    strsp(j) = 1.
  END IF
  
!! auto fertilization-nitrogen demand (non-legumes only)
  select case (idc(idp))
  case (4, 5, 6, 7)
  IF (auto_nstrs(j) > 0.) CALL anfert
END select

!! reduce predicted biomass due to stress on plant
reg = 0.
reg = MIN(strsw(j), strstmp(j), strsn(j), strsp(j), strsa(j))
IF (reg < 0.) reg = 0.
IF (reg > 1.) reg = 1.

IF (bio_targ(j) > 1.e-2) THEN
  bioday = bioday * (bio_targ(j) - bio_ms(j)) / bio_targ(j)
  reg = 1.
END IF

bio_ms(j) = bio_ms(j) + bioday * reg
IF (idc(idp) == 7 .AND. igrotree(j) == 0) THEN
  IF (mat_yrs(idp) > 0) THEN
    rto = FLOAT(curyr_mat(j)) / FLOAT(mat_yrs(idp))
    biomxyr = rto * bmx_trees(idp)
    bio_ms(j) = MIN (bio_ms(j), biomxyr)
  ELSE
    rto = 1.
  END IF
END IF

bio_ms(j) = MAX(bio_ms(j),0.)

!!add by zhang
!!============
IF (cswat == 2) THEN
  nppc_d(j) = nppc_d(j) + bioday * reg* 0.42
END IF
!!add by zhang
!!============

!! calculate fraction of total biomass that is in the roots
rwt(j) = rsr1(idp) -(rsr1(idp) - rsr2(idp)) * phuacc(j)

f = 0.
ff = 0.
f = phuacc(j) / (phuacc(j) + EXP(leaf1(idp) - leaf2(idp) * phuacc(j)))
ff = f - laimxfr(j)
laimxfr(j) = f

!! calculate new canopy height
IF (idc(idp) == 7) THEN
  cht(j) = rto * chtmx(idp)
ELSE
  cht(j) = chtmx(idp) * SQRT(f)
END IF

!! calculate new leaf area index
IF (phuacc(j) <= dlai(idp)) THEN
  laimax = 0.
  deltalai = 0.
  IF (idc(idp) == 7) THEN
    laimax = rto * blai(idp)
  ELSE
    laimax = blai(idp)
  END IF
  
  IF (laiday(j) > laimax) laiday(j) = laimax
  deltalai = ff * laimax * (1.0 - EXP(5.0 * (laiday(j) - laimax))) * SQRT(reg)
  laiday(j) = laiday(j) + deltalai
  IF (laiday(j) > laimax) laiday(j) = laimax
  olai(j) = laiday(j)
  IF (laiday(j) > lai_yrmx(j)) lai_yrmx(j) = laiday(j)
ELSE
  laiday(j) = olai(j) * (1. - phuacc(j)) / (1. - dlai(idp))
END IF
IF (laiday(j) < alai_min(idplt(j))) THEN   !Sue White dormancy
  laiday(j) = alai_min(idplt(j))
END IF

!! calculate plant ET values
IF (phuacc(j) > 0.5 .AND. phuacc(j) < dlai(idp)) THEN
  plt_et(j) = plt_et(j) + ep_day + es_day
  plt_pet(j) = plt_pet(j) + pet_day
END IF

hvstiadj(j) = hvsti(idp) * 100. * phuacc(j)  &
    / (100. * phuacc(j) + EXP(11.1 - 10. * phuacc(j)))

!!  added per JGA for Srini by gsm 9/8/2011
strsw_sum(j) = strsw_sum(j) + (1. - strsw(j))
strstmp_sum(j) = strstmp_sum(j) + (1. - strstmp(j))
strsn_sum(j) = strsn_sum(j) + (1. - strsn(j))
strsp_sum(j) = strsp_sum(j) + (1. - strsp(j))
strsa_sum(j) = strsa_sum(j) + (1. - strsa(j))

!! summary calculations
IF (curyr > nyskip) THEN
  wshd_wstrs = wshd_wstrs + (1.-strsw(j)) * hru_dafr(j)
  wshd_tstrs = wshd_tstrs + (1.-strstmp(j)) * hru_dafr(j)
  wshd_nstrs = wshd_nstrs + (1.-strsn(j)) * hru_dafr(j)
  wshd_pstrs = wshd_pstrs + (1.-strsp(j)) * hru_dafr(j)
  wshd_astrs = wshd_astrs + (1.-strsa(j)) * hru_dafr(j)
END IF
ELSE                                                                             !! modified by cibin TO INCLUDE dlai>1
IF (dlai(idp) > 1.) THEN
  IF (phuacc(j) > dlai(idp)) THEN
    laiday(j) = olai(j) * (1. - (phuacc(j) - dlai(idp)) /                       !! Modified by cibin TO INCLUDE dlai>1  &
        (1.2 - dlai(idp)))           !! Modified by Cibin to include DLAI>1
  END IF
END IF
IF (laiday(j) < 0.) laiday(j) = 0.              !! Modified by Cibin to include dlai>1
END IF

RETURN
END SUBROUTINE grow
