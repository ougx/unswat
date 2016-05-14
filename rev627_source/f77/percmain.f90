SUBROUTINE percmain
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drainmod tile equations   08/2006
!! dep_imp(:) |mm            |depth to impervious layer
!!    drainmod tile equations   08/2006

!!    icrk        |none          |crack flow code
!!                               |1 simulate crack flow in watershed
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    drainmod tile equations   01/2006
!!    itdrn       |none          |tile drainage equations flag/code
!!                               |1 simulate tile flow using subroutine drains(wt_shall)
!!                               |0 simulate tile flow using subroutine origtile(wt_shall,d)
!!    iwtdn       |none          |water table depth algorithms flag/code
!!                               |1 simulate wt_shall using subroutine new water table depth routine
!!                               |0 simulate wt_shall using subroutine original water table depth routine
!!    ismax       |none          |maximum depressional storage selection flag/code
!!                               |1 dynamic stmaxd computed as a function of random roughness and rain intensity
!!                               |by depstor.f
!!                               |0 static stmaxd read from .bsn for the global value or .sdr for specific hrus
!!    drainmod tile equations   01/2006
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp)
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    wat_tbl(:)  |mm            |water table based on depth from soil surface
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    new water table depth  equations   01/2009
!! c   |none     |a factor used to convert airvol to wtd
!! deep_p      |mm      |total thickness of soil profile in HRU
!!    dg          |mm      |soil layer thickness in HRU
!!    new water table depth  equations   01/2009
!!    flat(:,:)   |mm H2O        |lateral flow storage array
!!    latlyr      |mm H2O        |lateral flow in soil layer for the day
!!    latq(:)     |mm H2O        |total lateral flow in soil profile for the
!!                               |day in HRU
!!    lyrtile     |mm H2O        |drainage tile flow in soil layer for day
!!    new water table depth  equations   01/2009
!! ne_p  |mm/hr     |effective porosity in HRU for all soil profile layers
!! ne_w  |mm/hr     |effective porosity in HRU for soil layers above wtd
!!    new water table depth  equations   01/2009
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sepday      |mm H2O        |micropore percolation from soil layer
!!    sepbtm(:)   |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on current day
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    new water table depth  equations   01/2009
!!    wat      |mm H2O        |shallow water table depth below the soil surface to up to impervious layer
!!    new water table depth  equations   01/2009
!!    wt_shall    |mm H2O        |shallow water table depth above the impervious layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!! w2   |mm      |
!! y1   |mm      |dummy variable for wat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro, drains(wt_shall), origtile(wt_shall,d)

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, j1, nn, k, sb

j = 0
j = ihru
sb = inum1

!! initialize water entering first soil layer

IF (icrk == 1) THEN
  sepday = MAX(0., inflpcp - voltot)
ELSE
  sepday = inflpcp
END IF

!!  add irrigation water
IF (aird(j)>0) THEN
  j=j
END IF
sepday = inflpcp + aird(j) + pot_seep(j)
pot_seep(j) = 0.

!! if unlimted, or groundwater source reset aird here (otherwise in virtual)
!!  change per JGA 10/12/11 irrigation problem with reach
!! if (irrsc(j) > 2)  aird(j) = 0.
!!      aird(j) = 0.

!! calculate crack flow
IF (icrk == 1) THEN
  CALL percmacro
  sepday = sepday - sepcrktot
END IF

DO j1 = 1, sol_nly(j)
!! add water moving into soil layer from overlying layer
  sol_st(j1,j) = sol_st(j1,j) + sepday
  
!! septic tank inflow to biozone layer  J.Jeong
! STE added to the biozone layer if soil temp is above zero.
  IF(j1==i_sep(j).AND.sol_tmp(j1,j) > 0. .AND. isep_opt(j) /= 0) THEN
    sol_st(j1,j) = sol_st(j1,j) + qstemm(j)  ! in mm
    qvol = qstemm(j) * hru_ha(j) * 10.
    xx = qvol / hru_ha(j) / 1000.
    sol_no3(j1,j) = sol_no3(j1,j) + xx *(sptno3concs(isp) + sptno2concs(isp))
    sol_nh3(j1,j) = sol_nh3(j1,j) + xx * sptnh4concs(isp)
    sol_orgn(j1,j) = sol_orgn(j1,j) + xx * sptorgnconcs(isp)*0.5
    sol_fon(j1,j) = sol_fon(j1,j) + xx * sptorgnconcs(isp) * 0.5
    sol_orgp(j1,j) = sol_orgp(j1,j) + xx * sptorgps(isp) * 0.5
    sol_fop(j1,j) = sol_fop(j1,j) + xx * sptorgps(isp) * 0.5
    sol_solp(j1,j) = sol_solp(j1,j) + xx * sptminps(isp)
  END IF
  
!! determine gravity drained water in layer
  sw_excess = 0.
  sw_excess = sol_st(j1,j) - sol_fc(j1,j)
  
!! initialize variables for current layer
  sepday = 0.
  latlyr = 0.
  lyrtile = 0.
  lyrtilex = 0.
  
  IF (sw_excess > 1.e-5) THEN
!! calculate tile flow (lyrtile), lateral flow (latlyr) and
!! percolation (sepday)
    CALL percmicro(j1)
    
    sol_st(j1,j) = sol_st(j1,j) - sepday - latlyr - lyrtile
    sol_st(j1,j) = MAX(1.e-6,sol_st(j1,j))
    
!! redistribute soil water if above field capacity (high water table)
    CALL sat_excess(j1)
!         sol_st(j1,j) = sol_st(j1,j) - lyrtilex
!         sol_st(j1,j) = Max(1.e-6,sol_st(j1,j))
  END IF
  
!! summary calculations
  IF (j1 == sol_nly(j)) THEN
    sepbtm(j) = sepbtm(j) + sepday
  END IF
  latq(j) = latq(j) + latlyr
  qtile = qtile + lyrtile
  flat(j1,j) = latlyr + lyrtile
  sol_prk(j1,j) = sol_prk(j1,j) + sepday
  IF (latq(j) < 1.e-6) latq(j) = 0.
  IF (qtile < 1.e-6) qtile = 0.
  IF (flat(j1,j) < 1.e-6) flat(j1,j) = 0.
END DO

!! seepage contribution by urban distributed bmps
IF (ievent >= 2) THEN
  sepbtm(j) = sepbtm(j) + bmp_recharge(sb)
END IF

!! update soil profile water
sol_sw(j) = 0.
DO j1 = 1, sol_nly(j)
  sol_sw(j) = sol_sw(j) + sol_st(j1,j)
  
END DO

!! compute shallow water table depth and tile flow
qtile = 0.
wt_shall = 0.    !CB 8/24/09
wt_shall = dep_imp(j)
!! drainmod tile equations   08/11/2006
IF (sol_tmp(2,j) > 0.) THEN   !Daniel 1/29/09
  por_air = 0.5
  d = dep_imp(j) - ddrain(j)
!! drainmod wt_shall equations   10/23/2006
  IF (iwtdn == 0) THEN !compute wt_shall using original eq-Daniel 10/23/06
    IF (sol_sw(j) > sol_sumfc(j)) THEN
      yy = sol_sumul(j) * por_air
      IF (yy < 1.1 * sol_sumfc(j)) THEN
        yy = 1.1 * sol_sumfc(j)
      END IF
      xx = (sol_sw(j) - sol_sumfc(j)) / (yy - sol_sumfc(j))
      IF (xx > 1.) xx = 1.
      wt_shall = xx * dep_imp(j)
      wat = dep_imp(j) - wt_shall
      IF(wat > dep_imp(j)) wat = dep_imp(j)
    END IF
  ELSE
!compute water table depth using Daniel's modifications
!       Updated water table depth D.Moriasi 4/8/2014
    swst_del = 0.
    sw_del = 0.
    wt_del = 0.
    wtst_del = 0.
    DO j1 = 1, sol_nly(j)
!            if (wat_tbl(j) < sol_z(j1,j)) then
      swst_del = sol_stpwt(j1,j) - sol_st(j1,j)
      sw_del = sol_swpwt(j) - sol_sw(j)
      wt_del = sw_del * vwt(j1,j)
      wtst_del = swst_del * vwt(j1,j)
!            wat_tbl(j) = wat_tbl(j) + wt_del
      wat_tbl(j) = wat_tbl(j) + wtst_del
      IF(wat_tbl(j) < 0.0) wat_tbl(j) = 0.0
      IF(wat_tbl(j) > dep_imp(j)) wat_tbl(j) = dep_imp(j)
      wt_shall = dep_imp(j) - wat_tbl(j)
      sol_swpwt(j) = sol_sw(j)
      sol_stpwt(j1,j) = sol_st(j1,j)
!         exit
!       end if
!       Updated water table depth D.Moriasi 4/8/2014
    END DO
  END IF
!! drainmod wt_shall equations   10/23/2006
  
  IF (ddrain(j) > 0.) THEN
    IF (wt_shall <= d) THEN
      qtile = 0.
    ELSE
!! Start Daniel's tile equations modifications  01/2006
      IF (itdrn == 1) THEN
        CALL drains     ! compute tile flow using drainmod tile equations
!! drainmod tile equations   01/2006
      ELSE !! compute tile flow using existing tile equations
        CALL origtile(d)! existing tile equations
        IF(qtile < 0.) qtile=0.
      END IF
    END IF
  END IF
END IF
!! End Daniel's tile equations modifications  01/2006

IF (qtile > 0.) THEN
!! update soil profile water after tile drainage
  sumqtile = qtile
  DO j1 = 1, sol_nly(j)
    xx = sol_st(j1,j) - sol_fc(j1,j)
    IF (xx > 0.) THEN
      IF (xx > sumqtile) THEN
        sol_st(j1,j) = sol_st(j1,j) - sumqtile
        sumqtile = 0.
      ELSE
        sumqtile = sumqtile - xx
        sol_st(j1,j) = sol_fc(j1,j)
      END IF
    END IF
  END DO
  IF (sumqtile > 0.) THEN
    qtile = qtile - sumqtile
    qtile = AMAX1(0., qtile)
  END IF
END IF

!! update soil profile water
sol_sw(j) = 0.
DO j1 = 1, sol_nly(j)
  sol_sw(j) = sol_sw(j) + sol_st(j1,j)
END DO

RETURN
END SUBROUTINE percmain
