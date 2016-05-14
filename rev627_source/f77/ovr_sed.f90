SUBROUTINE ovr_sed()
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes splash erosion by raindrop impact and flow erosion by overland flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cht(:)      |m             |canopy height
!!    fimp(:)     |fraction      |fraction of HRU area that is
!!                               |impervious (both directly and
!!                               |indirectly connected)
!!    hhqday(:)   |mm H2O        |surface runoff generated each timestep
!!                               |of day in HRU
!!    hru_km(:)   |km2           |area of HRU in square kilometers
!!    idt         |minutes       |length of time step used to report
!!    inum1       |none          |subbasin number
!!    laiday(:)   |m2/m2         |leaf area index
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    eros_spl   |none          |coefficient of splash erosion varing 0.9-3.1
!!    urblu(:)    |none          |urban land type identification number from
!!                               |urban.dat
!!    usle_k(:)   |              |USLE equation soil erodibility (K) factor
!!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhsedy(:,:)|tons           |sediment yield from HRU drung a time step
!!                               |applied to HRU

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!   bed_shear  |N/m2     |shear stress b/w stream bed and flow
!!   erod_k  |g/J     |soil detachability value
!!    jj   |none          |HRU number
!!    kk   |none          |time step of the day
!!   ke_direct  |J/m2/mm    |rainfall kinetic energy of direct throughfall
!!   ke_leaf  |J/m2/mm    |rainfall kinetic energy of leaf drainage
!!   ke_total  |J/m2       |total kinetic energy of rainfall
!!   percent_clay |percent    |percent clay
!!   percent_sand |percent    |percent sand
!!   percent_silt |percent    |percent silt
!!   pheff      |m      |effective plant height
!!   rdepth_direct |mm      |rainfall depth of direct throughfall
!!   rdepth_leaf |mm      |rainfall depth of leaf drainage
!!   rdepth_tot |mm      |total rainfall depth
!!    rintnsty     |mm/hr         |rainfall intensity
!!   sedspl  |tons     |sediment yield by rainfall impact during time step
!!   sedov   |tons     |sediment yield by overland flow during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: log10, Exp, Real
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!  Splash erosion model is adopted from EUROSEM model developed by Morgan (2001).
!! Rill/interill erosion model is adoped from Modified ANSWERS model by Park et al.(1982)
!!  Code developed by J. Jeong and N. Kannan, BRC.

use parm

INTEGER :: k, j
REAL :: percent_clay, percent_silt, percent_sand, erod_k
REAL :: ke_direct, ke_leaf, ke_total,pheff, c
REAL :: rdepth_direct, rdepth_leaf, rdepth_tot, canopy_cover
REAL :: bed_shear, sedov, sedspl, rain_d50, rintnsty

j = ihru

!! Fraction of sand
percent_clay = sol_clay(1,j)
percent_silt = sol_silt(1,j)
percent_sand = 100. - percent_clay - percent_silt

!! Soil detachability values adopted from EUROSEM User Guide (Table 1)
IF ((percent_clay>=40.) .AND. (percent_sand>=20.) .AND.  &
      (percent_sand<=45.)) THEN
  erod_k = 2.0 !clay
ELSE IF ((percent_clay>=27.) .AND. (percent_sand>=20.) .AND.  &
      (percent_sand<=45.)) THEN
  erod_k = 1.7 !Clay loam
ELSE IF ((percent_silt<=40.).AND.(percent_sand<=20.)) THEN
  erod_k = 2.0 !Clay
ELSE IF ((percent_silt>40.).AND.(percent_clay>=40.)) THEN
  erod_k = 1.6 !Silty clay
ELSE IF ((percent_clay>=35.).AND.(percent_sand>=45.)) THEN
  erod_k = 1.9 !Sandy clay
ELSE IF ((percent_clay>=27.).AND.(percent_sand<20.)) THEN
  erod_k = 1.6 !Silty clay loam
ELSE IF ((percent_clay<=10.).AND.(percent_silt>=80.)) THEN
  erod_k = 1.2 !Silt
ELSE IF (percent_silt>=50.) THEN
  erod_k = 1.5 !Silt loam
ELSE IF ((percent_clay>=7.) .AND. (percent_sand<=52.) .AND.  &
      (percent_silt>=28.)) THEN
  erod_k = 2.0 !Loam
ELSE IF (percent_clay>=20.) THEN
  erod_k = 2.1 !Sandy clay loam
ELSE IF (percent_clay>=percent_sand-70.) THEN
  erod_k = 2.6 !Sandy loam
ELSE IF (percent_clay>=(2. * percent_sand) - 170.) THEN
  erod_k = 3.0 !Loamy sand
ELSE
  erod_k = 1.9 !Sand
END IF
 !! c nopy cover based on leaf area index
!! canopy cover is assumed to be 100% if LAI>=1
IF(laiday(j)>=1.) THEN
  canopy_cover = 1.
ELSE
  canopy_cover = laiday(j)
END IF

DO k=1,nstep
  rintnsty = 60. * rainsub(j,k) / REAL(idt)
  rain_d50 = 0.188 * rintnsty ** 0.182
  
  IF(rintnsty>0) THEN
    
!! Rainfall kinetic energy generated by direct throughfall (J/m^2/mm)
    ke_direct = 8.95 + 8.44 * LOG10(rintnsty)
    IF(ke_direct<0.) ke_direct = 0.
!! Rainfall kinetic energy generated by leaf drainage (J/m^2)
    pheff = 0.5 * cht(j)
    ke_leaf = 15.8 * pheff ** 0.5 - 5.87
    IF (ke_leaf<0) ke_leaf = 0.
    
!! Depth of rainfall
    rdepth_tot = rainsub(j,k) / (idt * 60.)
    rdepth_leaf = rdepth_tot * canopy_cover
    rdepth_direct = rdepth_tot - rdepth_leaf
  ELSE
    ke_direct = 0.
    ke_leaf = 0.
    rdepth_tot = 0.
    rdepth_leaf = 0.
    rdepth_direct = 0.
  END IF
  
!! total kinetic energy by rainfall (J/m^2)
  ke_total = 0.001 * (rdepth_direct * ke_direct + rdepth_leaf * ke_leaf)
  
!! total soil detachment by raindrop impact
  sedspl = erod_k * ke_total * EXP(-eros_spl * hhqday(k) / 1000.) *  &
      hru_km(j) ! tons
  
!! Impervious area of HRU
  IF(urblu(j)>0) sedspl = sedspl * (1.- fimp(urblu(j)))
  
!! maximum water depth that allows splash erosion
  IF(hhqday(k)>=3.* rain_d50.OR.hhqday(k)<=1.e-3) sedspl = 0.
  
  
!! Overland flow erosion  &
!  cover and management factor used in usle equation (ysed.f)
  c = EXP((-.2231 - cvm(idplt(j))) *  &
      EXP(-.00115 * sol_cov(j)) + cvm(idplt(j)))
!! specific weight of water at 5 centigrate =9807N/m3
  bed_shear = 9807 * (hhqday(k) / 1000.) * hru_slp(j) ! N/m2
  sedov = 11.02 * rill_mult * usle_k(j) * c_factor * c *  &
      bed_shear ** eros_expo ! kg/hour/m2
  IF(ievent>=2) THEN
    sedov = 16.667 * sedov * hru_km(j) * idt ! tons per time step
  ELSE
    sedov = 24000. * sedov * hru_km(j) ! tons per day
  END IF
  
!! Impervious area of HRU
  IF(urblu(j)>0) sedov = sedov * (1.- fimp(urblu(j)))
  
  hhsedy(j,k) = dratio(inum1) * (sedspl + sedov)
  IF (hhsedy(j,k) < 1.e-10) hhsedy(j,k) = 0.
  
END DO
RETURN
END SUBROUTINE ovr_sed
