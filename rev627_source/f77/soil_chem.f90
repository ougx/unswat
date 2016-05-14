SUBROUTINE soil_chem
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil chemical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    i             |none          |HRU number
!!    nactfr        |none          |nitrogen active pool fraction. The fraction
!!                                 |of organic nitrogen in the active pool.
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    psp           |none          |Phosphorus availability index. The fraction
!!                                 |of fertilizer P remaining in labile pool
!!                                 |after initial rapid phase of P sorption.
!!    skoc(:)       |(mg/kg)/(mg/L)|soil adsorption coefficient normalized
!!                                 |for soil organic carbon content
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_nly(:)    |none          |number of soil layers
!!    sol_no3(:,:)  |mg N/kg soil  |nitrate concentration in soil layer
!!    sol_orgn(:,:) |mg/kg         |organic N concentration in soil layer
!!    sol_orgp(:,:) |mg/kg         |organic P concentration in soil layer
!!    sol_pst(:,:,1)|kg/ha         |initial amount of pesticide in first layer
!!                                 |read in from .chm file
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil layer
!!                                 |classified as residue
!!    sol_solp(:,:) |mg/kg         |solution P concentration in soil layer
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    basminpi      |kg P/ha       |average amount of phosphorus initially in
!!                                 |the mineral P pool in watershed soil
!!    basno3i       |kg N/ha       |average amount of nitrogen initially in the
!!                                 |nitrate pool in watershed soil
!!    basorgni      |kg N/ha       |average amount of nitrogen initially in
!!                                 |the organic N pool in watershed soil
!!    basorgpi      |kg P/ha       |average amount of phosphorus initially in
!!                                 |the organic P pool in watershed soil
!!    conv_wt(:,:)  |none          |factor which converts kg/kg soil to kg/ha
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_cov(:)    |kg/ha         |amount of residue on soil surface
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_hum(:,:)  |kg humus/ha   |amount of organic matter in the soil layer
!!                                 |classified as humic substances
!!    sol_kp(:,:,:) |(mg/kg)/(mg/L)|pesticide sorption coefficient, Kp; the
!!                                 |ratio of the concentration in the solid
!!                                 |phase to the concentration in solution
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool. This variable is read in as
!!                                 |a concentration and converted to kg/ha.
!!                                 |(this value is read from the .sol file in
!!                                 |units of mg/kg)
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool NOTE UNIT CHANGE!
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool NOTE UNIT CHANGE!
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer NOTE UNIT
!!                                 |CHANGE!
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!                                 |NOTE UNIT CHANGE!
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus
!!                                 |pool
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dg          |mm            |depth of layer
!!    j           |none          |counter
!!    jj          |none          |dummy variable to hold value
!!    n           |none          |counter
!!    nly         |none          |number of soil layers
!!    soldepth    |mm            |depth from bottom of 1st soil layer to
!!                               |the bottom of the layer of interest
!!    solpst      |mg/kg         |concentration of pesticide in soil
!!    summinp     |kg P/ha       |amount of phosphorus stored in the mineral P
!!                               |pool in the profile
!!    sumno3      |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in the soil profile
!!    sumorgn     |kg N/ha       |amount of nitrogen stored in the organic N
!!                               |pools in the profile
!!    sumorgp     |kg P/ha       |amount of phosphorus stored in the organic P
!!                               |pools in the profile
!!    wt1         |none          |converts mg/kg (ppm) to kg/ha
!!    xx          |none          |variable to hold value
!!    zdst        |none          |variable to hold value
!!    labfrac     |none          |fraction of total soil mineral P which is labile
!!    soil_TP   |kg/ha         |Total Soil Mineral P
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: nly, j, jj, n
REAL :: xx, dg, wt1, zdst, soldepth, sumno3, sumorgn, summinp
REAL :: sumorgp, solpst, soil_tp, labfrac,solp

!!by zhang
!!=============
REAL :: sol_mass
REAL :: fbm, fhp, rtno, fhs, x1, rto, sol_min_n
sol_mass = 0.
dg = 0.
fbm = 0.
fhp = 0.
rtno = 0.
fhs = 0.
x1 = 0.
rto = 0.
!!by zhang
!!=============


nly = 0
solpst = 0.
sumno3 = 0.
sumorgn = 0.
summinp = 0.
sumorgp = 0.
nly = sol_nly(i)

!!    calculate sol_cbn for lower layers if only have upper layer
IF (nly >= 3 .AND. sol_cbn(3,i) <= 0) THEN
  DO j = 3, nly
    IF (sol_cbn(j,i) == 0.) THEN
      soldepth = 0
      soldepth = sol_z(j,i) - sol_z(2,i)
      sol_cbn(j,i) = sol_cbn(j-1,i) * EXP(-.001 * soldepth)
    END IF
  END DO
END IF

cmup_kgh = 0.
cmtot_kgh = 0.
DO j = 1, nly
  IF (j == 1) THEN
    sol_thick = sol_z(j,i)
  ELSE
    sol_thick = sol_z(j,i) - sol_z(j-1,i)
  END IF
  
!! soil carbon and nitrogen
  sol_mass = (sol_thick / 1000.) * 10000. * sol_bd(j,i)  &
      * 1000. * (1 - sol_rock(j,i) / 100.)
  sol_cmass = sol_mass * (sol_cbn(j,i) / 100.)
  
  IF (j == 1) cmup_kgh(i) = sol_cmass
  cmtot_kgh(i) = cmtot_kgh(i) + sol_cmass
END DO


!!    calculate sol_kp as function of koc and sol_cbn
!!    and set initial pesticide in all layers equal to value given for
!!    upper layer
IF (hrupest(i) == 1) THEN
  DO j = 1, npmx
    jj = 0
    jj = npno(j)
    IF (jj > 0) THEN
      solpst = 0.
      solpst = sol_pst(j,i,1)  !!concentration of pesticide in soil
      xx = 0.
      DO n = 1, nly
        dg = 0.
        wt1 = 0.
        dg = (sol_z(n,i) - xx)
        xx = sol_z(n,i)
        wt1 = sol_bd(n,i) * dg / 100.              !! mg/kg => kg/ha
        sol_kp(j,i,n) = skoc(jj) * sol_cbn(n,i) / 100.
        sol_pst(j,i,n) = solpst * wt1
      END DO
    END IF
  END DO
END IF


!!    calculate initial nutrient contents of layers, profile and
!!    average in soil for the entire watershed
!!    convert mg/kg (ppm) to kg/ha
xx = 0.
sol_fop(1,i) = sol_rsd(1,i) * .0010 !! was 0.0003 Armen January 2009
sol_fon(1,i) = sol_rsd(1,i) * .0055 !! was 0.0015 Armen January 2009
sol_cov(i) = sol_rsd(1,i)
DO j = 1, nly
  dg = 0.
  wt1 = 0.
  dg = (sol_z(j,i) - xx)
  wt1 = sol_bd(j,i) * dg / 100.              !! mg/kg => kg/ha
  conv_wt(j,i) = 1.e6 * wt1                  !! kg/kg => kg/ha
  
  IF (sol_no3(j,i) <= 0.) THEN
    zdst = 0.
    zdst = EXP(-sol_z(j,i) / 1000.)
    sol_no3(j,i) = 10. * zdst * .7
  END IF
  sol_no3(j,i) = sol_no3(j,i) * wt1          !! mg/kg => kg/ha
  sumno3 = sumno3 + sol_no3(j,i)
  
  IF (sol_orgn(j,i) > 0.0001) THEN
    sol_orgn(j,i) = sol_orgn(j,i) * wt1      !! mg/kg => kg/ha
  ELSE
!! assume C:N ratio of 10:1
    sol_orgn(j,i) = 10000. * (sol_cbn(j,i) / 14.) * wt1  !! CN ratio changed back to 14 cibin 03022012
  END IF
  sol_aorgn(j,i) = sol_orgn(j,i) * nactfr
  sol_orgn(j,i) = sol_orgn(j,i) * (1. - nactfr)
  sumorgn = sumorgn + sol_aorgn(j,i) + sol_orgn(j,i) + sol_fon(j,i)
  
  IF (sol_orgp(j,i) > 0.0001) THEN
    sol_orgp(j,i) = sol_orgp(j,i) * wt1      !! mg/kg => kg/ha
  ELSE
!! assume N:P ratio of 8:1
    sol_orgp(j,i) = .125 * sol_orgn(j,i)
  END IF
  
  IF (sol_solp(j,i) > 0.0001) THEN
    sol_solp(j,i) = sol_solp(j,i) * wt1      !! mg/kg => kg/ha
  ELSE
!! assume initial concentration of 5 mg/kg
    sol_solp(j,i) = 5. * wt1
  END IF
  
!! Set active pool based on dynamic PSP MJW
  
  IF (sol_p_model == 0) THEN
!! Allow Dynamic PSP Ratio
!! convert to concentration
    solp = sol_solp(j,i) / conv_wt(j,i) * 1000000.
!! PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
    IF (sol_clay(j,i) > 0.) THEN
      psp = -0.045 * LOG(sol_clay(j,i))+ (0.001 * solp)
      psp = psp - (0.035  * sol_cbn(j,i)) + 0.43
    ELSE
      psp = 0.4
    END IF
!! Limit PSP range
    IF (psp <.05) THEN
      psp = 0.05
    ELSE IF (psp > 0.9) THEN
      psp = 0.9
    END IF
  END IF
  
  sol_actp(j,i) = sol_solp(j,i) * (1. - psp) / psp
  
!! Set Stable pool based on dynamic coefficant
  IF (sol_p_model == 0) THEN  !! From White et al 2009
!! convert to concentration for ssp calculation
    actp = sol_actp(j,i) / conv_wt(j,i) * 1000000.
    solp = sol_solp(j,i) / conv_wt(j,i) * 1000000.
!! estimate Total Mineral P in this soil based on data from sharpley 2004
    ssp = 25.044 * (actp + solp)** -0.3833
!!limit SSP Range
    IF (ssp > 7.) ssp = 7.
    IF (ssp < 1.) ssp = 1.
    sol_stap(j,i) = ssp * (sol_actp(j,i) + sol_solp(j,i))!define stableP
  ELSE
!! The original code
    sol_stap(j,i) = 4. * sol_actp(j,i)
  END IF
  
  sol_hum(j,i) = sol_cbn(j,i) * wt1 * 17200.
  xx = sol_z(j,i)
  summinp = summinp + sol_solp(j,i) + sol_actp(j,i) + sol_stap(j,i)
  sumorgp = sumorgp + sol_orgp(j,i) + sol_fop(j,i)
END DO

basno3i = basno3i + sumno3 * hru_km(i) / da_km
basorgni = basorgni + sumorgn * hru_km(i) / da_km
basminpi = basminpi + summinp * hru_km(i) / da_km
basorgpi = basorgpi + sumorgp * hru_km(i) / da_km

!! By Zhang for C/N cycling
!!===============================
IF (cswat == 2) THEN
  IF (rsdin(i) > 0.) sol_rsd(1,i) = rsdin(i)
  DO j = 1, nly
!!kg/ha sol mass in each layer
    IF (j == 1) THEN
      sol_mass = (sol_z(j,i)) / 1000.
!&      10000. * sol_bd(j,ihru)* 1000. *
!&       (1- sol_rock(j,ihru) / 100.)
      sol_mass = sol_mass * 10000. * sol_bd(j,i)* 1000.
      sol_mass = sol_mass * (1- sol_rock(j,i) / 100.)
      
    ELSE
      sol_mass = (sol_z(j,i) - sol_z(j-1,i)) / 1000.
!&      10000. * sol_bd(j,ihru)* 1000. *
!&       (1- sol_rock(j,ihru) / 100.)
      sol_mass = sol_mass * 10000. * sol_bd(j,i)* 1000.
      sol_mass = sol_mass * (1- sol_rock(j,i) / 100.)
    END IF
!!kg/ha mineral nitrogen
    sol_min_n = sol_no3(j,i)+sol_nh3(j,i)
    
!XCB = 0.2
!mm
    IF (j == 1) THEN
!DG = 10
      dg = sol_z(j,i)
    ELSE
      dg = (sol_z(j,i) - sol_z(j-1,i))
    END IF
    
!if(sol_WOC(j,ihru)<1.E-5) sol_WOC(j,ihru)=XCB*exp(-.001*DG)
    
!XCB=sol_WOC(j,ihru)
!XZ=sol_WOC(j,ihru) *.0172
!ZZ=1.-XZ
!sol_BDM(j,ihru)=ZZ/(1./sol_BD(j,ihru)-XZ/.224)
!if(sol_BDM(j,ihru)<1.)then
!    sol_BDM(j,ihru)=1.
!    sol_BD(j,ihru)=1./(ZZ+XZ/.224)
!end if
    
    
!ton/ha
!WT = sol_mass/1000.
    
!WT1 = WT/1000.
!X1 = 10. * sol_cbn(j,ihru) * WT
!WT(J)=BD(J)*DG*10.
!DG1=DG
!WT1=WT(J)/1000.
!X1=10.*WOC(J)*WT(J)
!WOC(J)=X1
!kg/ha
!sol_WOC(j,ihru)=X1
    sol_woc(j,i) = sol_mass * sol_cbn(j,i)/100
!if(sol_WON(j,ihru)>0.)then
!      sol_WON(j,ihru)=WT1*sol_WON(j,ihru)
!      KK=0
!else
    sol_won(j,i) = sol_aorgn(j,i)+  sol_orgn(j,i)!0.1 * sol_WOC(j,i)
!      KK=1
!end if
    
!Frction of Mirobial Biomass, Humus Passive C pools
    fbm = 0.0
    fhp = 0.0
    IF(fbm<1.e-10)fbm=.04
    rtn0 = 100.
    IF(fhp<1.e-10)fhp=.7-.4*EXP(-.0277*100)
    fhs = 1 - fbm - fhp
!From DSSAT
!FBM = 0.02
!FHS = 0.54
!FHP = 0.44
    
!NCC = 0
!IF(NCC==0)THEN
!sol_WBM(j,ihru)=FBM*X1
    sol_bm(j,i)=fbm*sol_woc(j,i)
    sol_bmc(j,i)=sol_bm(j,i)
!IF(KK==0)THEN
    rto=sol_won(j,i)/sol_woc(j,i)
!ELSE
!      RTO=.1
!END IF
    sol_bmn(j,i)=rto*sol_bmc(j,i)
!sol_HP(j,ihru)=FHP*(X1-sol_BM(j,ihru))
    sol_hp(j,i)=fhp*(sol_woc(j,i)-sol_bm(j,i))
    sol_hs(j,i)=sol_woc(j,i)-sol_bm(j,i)-sol_hp(j,i)
!sol_HP(j,i)=sol_WOC(j,i)-sol_BM(j,i)-sol_HP(j,i)
    sol_hsc(j,i)=sol_hs(j,i)
    sol_hsn(j,i)= rto*sol_hsc(j,i)  !sol_aorgn(j,i)
    sol_hpc(j,i)=sol_hp(j,i)
    sol_hpn(j,i)= rto*sol_hpc(j,i)  !sol_orgn(j,i)
    
    
    x1=sol_rsd(j,i) /1000.
!!skip std in SWAT
!IF(j==1)X1=X1+STD(j)/1000.
    
    sol_lm(j,i)=500.*x1
    sol_ls(j,i)=sol_lm(j,i)
    sol_lsl(j,i)=.8*sol_ls(j,i)
    sol_lmc(j,i)=.42*sol_lm(j,i)
    
    sol_lmn(j,i)=.1*sol_lmc(j,i)
    sol_lsc(j,i)=.42*sol_ls(j,i)
    sol_lslc(j,i)=.8*sol_lsc(j,i)
    sol_lslnc(j,i)=.2*sol_lsc(j,i)
    sol_lsn(j,i)=sol_lsc(j,i)/150.
!sol_WOC(j,ihru)=sol_WOC(j,ihru)+sol_LSC(j,ihru)+sol_WLMC(j,ihru)
    sol_woc(j,i)=sol_woc(j,i)+sol_lsc(j,i)+sol_lmc(j,i)
!sol_WON(j,ihru)=sol_WON(j,ihru)+sol_LSN(j,ihru)+sol_WLMN(j,ihru)
    sol_won(j,i)=sol_won(j,i)+sol_lsn(j,i)+sol_lmn(j,i)
!END IF
    
!if (sol_orgn(j,i) > 0.0001) then
!  sol_orgn(j,i) = sol_orgn(j,i) * wt1      !! mg/kg => kg/ha
!else
!! assume C:N ratio of 10:1
!  sol_orgn(j,i) = 10000. * (sol_cbn(j,i) / 11.) * wt1  !! CN ratio was 14 before 01-22-09 Armen
!end if
    sol_orgn(j,i) = sol_hpn(j,i)
    sol_aorgn(j,i) = sol_hsn(j,i)
    sol_fon(1,i) = sol_lmn(j,i) + sol_lsn(j,i)
!sol_aorgn(j,i) = sol_orgn(j,i) * nactfr
!sol_orgn(j,i) = sol_orgn(j,i) * (1. - nactfr)
    sumorgn = sumorgn + sol_aorgn(j,i) + sol_orgn(j,i) +  &
        sol_fon(j,i) + sol_bmn(j,i)
    
    
  END DO
  
END IF
!! By Zhang for C/N cycling
!!===============================


!!May need to think about moving the following lines which appear before in this module to the end of this module,
!!because orgn has been re-calculated.
!!============================
!basno3i = basno3i + sumno3 * hru_km(i) / da_km
!basorgni = basorgni + sumorgn * hru_km(i) / da_km
!basminpi = basminpi + summinp * hru_km(i) / da_km
!basorgpi = basorgpi + sumorgp * hru_km(i) / da_km

RETURN
END SUBROUTINE soil_chem
