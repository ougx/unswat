SUBROUTINE newtillmix(jj,bmix)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients during tillage and
!!    biological mixing
!!    New version developed by Armen R. Kemanian in collaboration with Stefan Julich and Cole Rossi
!!    Mixing was extended to all layers
!!    A subroutine to simulate stimulation of organic matter decomposition was added
!!    March 2009: testing has been minimal and further adjustments are expected
!!    use with caution

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil
!!                                 |particles
!!    cnop          |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    curyr         |none          |current year of simulation
!!    deptil(:)     |mm            |depth of mixing caused by tillage
!!                                 |operation
!!    effmix(:)     |none          |mixing efficiency of tillage operation
!!    sol_nly(jj)          |none          |maximum number of soil layers
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    nro(:)        |none          |sequence number of year in rotation
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    nyskip        |none          |number of years to skip output printing/
!!                                 |summarization
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_nly(:)    |none          |number of soil layers
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool.
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus pool
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil
!!                                 |particles
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool.
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus pool
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    min_res(:) |kg/ha     |Min residue allowed due to implementation of
!!                                 |residue managment in the OPS file.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bmix        |none          |biological mixing efficiency: this
!!                               |number is zero for tillage operations
!!    dg          |mm            |depth of soil layer
!!    dtil        |mm            |depth of mixing
!!    emix        |none          |mixing efficiency
!!    jj          |none          |HRU number
!!    k           |none          |counter
!!    l           |none          |counter
!!    nl          |none          |number of layers being mixed
!!    smix(:)     |varies        |amount of substance in soil profile
!!                               |that is being redistributed between
!!                               |mixed layers
!!    thtill(:)   |none          |fraction of soil layer that is mixed
!!    sol_msm      | sol_mass mixed
!!    sol_msn      | sol_mass not mixed
!!    maxmix      |none          | maximum mixing eff to preserve specified minimum residue cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: jj
REAL, INTENT(IN)                         :: bmix


!$$$$$$       integer :: l, k, nl, a
INTEGER :: l, k              !CB 12/2/09 nl and a are not used.
REAL :: emix, dtil, xx, ww1, ww2, ww3, ww4, maxmix
!$$$$$$       real :: thtill(sol_nly(jj)), smix(20+npmx)
!!by zhang
!!=============
REAL :: smix(22+npmx+12)        !CB 12/2/09 thtill is not used. mjw rev 490
!!changed the dimension from 22 + npmx to 22 + npmx + 12
!!by zhang
!!=============
REAL :: sol_mass(sol_nly(jj))
REAL :: sol_thick(sol_nly(jj)), sol_msm(sol_nly(jj))
REAL :: sol_msn(sol_nly(jj))


xx = 0.
ww1 = 0.
ww2 = 0.
ww3 = 0.
ww4 = 0.
emix = 0.
dtil = 0.
IF (bmix > 1.e-6) THEN
!! biological mixing
  emix = bmix !bmix MJW (rev 412)
  dtil = MIN(sol_z(sol_nly(jj),jj), 50.) ! it was 300.  MJW (rev 412)
ELSE
!! tillage operation
  emix = effmix(idtill)
  dtil = deptil(idtill)
END IF
! -------------------------------- Original D. Moriasi code replaced by code below
! Drainmod  07/2006
!      if(itill(jj) == 1) then
!   cumei(jj) = 0.
!   cumeira(jj) = 0.
!   cumrt(jj) = 0.
!        cumrai(jj) = 0.
!   ranrns_hru(jj) = ranrns(idtill)
!      end if
!!    Drainmod 7/2006
! --------------------------------------------------------------------
IF (idtill >= 1) THEN ! Updated dynamic depressional storage D.Moriasi 4/8/2014
  cumei(jj)   = 0.
  cumeira(jj) = 0.
  cumrt(jj)   = 0.
  cumrai(jj)  = 0.
  ranrns_hru(jj) = ranrns(idtill)
END IF
! --------------------------------------------------------------------

!!by zhang DSSAT tillage
!!=======================
!!    deptil(:)   |mm  |depth of mixing caused by tillage operation
!jj is hru number
IF (cswat == 2) THEN
  tillage_days(jj) = 0
  tillage_depth(jj) = dtil
  tillage_switch(jj) = 1
END IF
!!by zhang DSSAT tillage
!!=======================


smix = 0.
sol_mass = 0.
sol_thick = 0.
sol_msm = 0.
sol_msn = 0.

!! incorporate bacteria - no mixing - lost from transport
IF (dtil > 10.) THEN
  bactpq(jj) = bactpq(jj) * (1. - emix)
  bactps(jj) = bactps(jj) * (1. - emix)
  bactlpq(jj) = bactlpq(jj) * (1. - emix)
  bactlps(jj) = bactlps(jj) * (1. - emix)
END IF

!! calculate max mixing to preserve target surface residue MJW rev 490
!! Assume residue in all other layers is negligible to simplify calculation and remove depth dependency
IF (min_res(jj) > 1. .AND. bmix < 0.001) THEN
  maxmix = 1 - min_res(jj)/sol_rsd(1,jj)
  IF (maxmix <0.05)  maxmix = 0.05
  IF (emix > maxmix)  emix = maxmix
END IF


DO l=1, sol_nly(jj)
  IF ( l == 1) THEN
    sol_thick(l) = sol_z(l,jj)
  ELSE
    sol_thick(l) = sol_z(l,jj) - sol_z(l-1,jj)
  END IF
  
  sol_mass(l) = (sol_thick(l) / 1000.) * 10000. *  &
      sol_bd(l,jj) * 1000. * (1.- sol_rock(l,jj) / 100.)
  
END DO

!       do l=1,20+npmx
!         smix(l)=0.
!       end do
smix = 0.

IF (dtil > 0.) THEN
!!!  added by Armen 09/10/2010 next line only
  IF (dtil < 10.0) dtil = 11.0
  DO l=1, sol_nly(jj)
    
    IF (sol_z(l,jj) <= dtil) THEN
!! msm = mass of soil mixed for the layer
!! msn = mass of soil not mixed for the layer
      sol_msm(l) = emix * sol_mass(l)
      sol_msn(l) = sol_mass(l) - sol_msm(l)
    ELSE IF (sol_z(l,jj) > dtil.AND.sol_z(l-1,jj) < dtil) THEN
      sol_msm(l) = emix * sol_mass(l) * (dtil - sol_z(l-1,jj)) / sol_thick(l)
      sol_msn(l) =  sol_mass(l) -  sol_msm(l)
    ELSE
      sol_msm(l) = 0.
      sol_msn(l) = sol_mass(l)
    END IF
    
!! calculate the mass or concentration of each mixed element
!! mass based mixing
    ww1 = sol_msm(l)/(sol_msm(l) + sol_msn(l))
    smix(1) = smix(1) + sol_no3(l,jj) * ww1
    smix(2) = smix(2) + sol_orgn(l,jj) * ww1
    smix(3) = smix(3) + sol_nh3(l,jj) * ww1
    smix(4) = smix(4) + sol_solp(l,jj) * ww1
    smix(5) = smix(5) + sol_orgp(l,jj) * ww1
    smix(6) = smix(6) + sol_aorgn(l,jj) * ww1
    smix(7) = smix(7) + sol_actp(l,jj) * ww1
    smix(8) = smix(8) + sol_fon(l,jj) * ww1
    smix(9) = smix(9) + sol_fop(l,jj) * ww1
    smix(10) = smix(10) + sol_stap(l,jj) * ww1
    smix(11) = smix(11) + sol_rsd(l,jj) * ww1
    smix(12) = smix(12) + sol_mc(l,jj) * ww1
    smix(13) = smix(13) + sol_mn(l,jj) * ww1
    smix(14) = smix(14) + sol_mp(l,jj) * ww1
    
!! concentration based mixing
    ww2 = xx + sol_msm(l)
    smix(15) = (xx * smix(15) + sol_cbn(l,jj) * sol_msm(l)) /ww2
    smix(16) = (xx * smix(16) + sol_n(l,jj) * sol_msm(l)) /ww2
    smix(17) = (xx * smix(17) + sol_clay(l,jj) * sol_msm(l)) /ww2
    smix(18) = (xx * smix(18) + sol_silt(l,jj) * sol_msm(l)) /ww2
    smix(19) = (xx * smix(19) + sol_sand(l,jj) * sol_msm(l)) /ww2
!          smix(20) = (XX * smix(20) + sol_rock(l,jj) * sol_msm(l)) / WW2
!          smix(21) = (XX * smix(21) + sol_ph(l,jj) * sol_msm(l)) /WW2 !! mjw rev490
!          smix(22) = (XX * smix(22) + sol_cal(l,jj) * sol_msm(l)) /WW2 !! mjw rev490
!! mass based distribution
    DO k = 1, npmx
      smix(20+k) = smix(20+k) + sol_pst(k,jj,l) * ww1
    END DO
    
!!by zhang
!!==============
    IF (cswat == 2) THEN
      smix(20+npmx+1) = smix(20+npmx+1) +sol_lsc(l,jj)* ww1
      smix(20+npmx+2) = smix(20+npmx+2) +sol_lslc(l,jj)* ww1
      smix(20+npmx+3) = smix(20+npmx+3) +sol_lslnc(l,jj)* ww1
      smix(20+npmx+4) = smix(20+npmx+4) +sol_lmc(l,jj)* ww1
      smix(20+npmx+5) = smix(20+npmx+5) +sol_lm(l,jj)* ww1
      smix(20+npmx+6) = smix(20+npmx+6) +sol_lsl(l,jj)* ww1
      smix(20+npmx+7) = smix(20+npmx+7) +sol_ls(l,jj)* ww1
      
      smix(20+npmx+8) = smix(20+npmx+8) +sol_lsn(l,jj)* ww1
      smix(20+npmx+9) = smix(20+npmx+9) +sol_lmn(l,jj)* ww1
      smix(20+npmx+10) = smix(20+npmx+10) +sol_bmn(l,jj)* ww1
      smix(20+npmx+11) = smix(20+npmx+11) +sol_hsn(l,jj)* ww1
      smix(20+npmx+12) = smix(20+npmx+12) +sol_hpn(l,jj)* ww1
    END IF
!!by zhang
!!=============
    
    xx = xx + sol_msm(l)
  END DO
  
  DO l=1, sol_nly(jj)
    
! reconstitute each soil layer
    ww3 = sol_msn(l) / sol_mass(l)
    ww4 = sol_msm(l) / xx
    
    sol_no3(l,jj) = sol_no3(l,jj) * ww3 + smix(1) * ww4
    sol_orgn(l,jj) = sol_orgn(l,jj) * ww3 + smix(2) * ww4
    sol_nh3(l,jj) = sol_nh3(l,jj) * ww3 + smix(3) * ww4
    sol_solp(l,jj) = sol_solp(l,jj) * ww3 + smix(4) * ww4
    sol_orgp(l,jj) = sol_orgp(l,jj) * ww3 + smix(5) * ww4
    sol_aorgn(l,jj) = sol_aorgn(l,jj) * ww3 + smix(6) * ww4
    sol_actp(l,jj) = sol_actp(l,jj) * ww3 + smix(7) * ww4
    sol_fon(l,jj) = sol_fon(l,jj) * ww3 + smix(8) * ww4
    sol_fop(l,jj) = sol_fop(l,jj) * ww3 + smix(9) * ww4
    sol_stap(l,jj) = sol_stap(l,jj) * ww3 + smix(10) * ww4
    sol_rsd(l,jj) = sol_rsd(l,jj) * ww3 + smix(11) * ww4
    IF (sol_rsd(l,jj) < 1.e-10) sol_rsd(l,jj) = 1.e-10
    sol_mc(l,jj) = sol_mc(l,jj) * ww3 + smix(12) * ww4
    sol_mn(l,jj) = sol_mn(l,jj) * ww3 + smix(13) * ww4
    sol_mp(l,jj) = sol_mp(l,jj) * ww3 + smix(14) * ww4
    
    sol_cbn(l,jj) = (sol_cbn(l,jj) * sol_msn(l) + smix(15)  &
        * sol_msm(l)) / sol_mass(l)
    sol_n(l,jj) = (sol_n(l,jj) * sol_msn(l) + smix(16)  &
        * sol_msm(l)) / sol_mass(l)
    sol_clay(l,jj) = (sol_clay(l,jj) * sol_msn(l) + smix(17)  &
        * sol_msm(l)) / sol_mass(l)
    sol_silt(l,jj) = (sol_silt(l,jj) * sol_msn(l) + smix(18)  &
        * sol_msm(l)) / sol_mass(l)
    sol_sand(l,jj) = (sol_sand(l,jj) * sol_msn(l) + smix(19)  &
        * sol_msm(l)) / sol_mass(l)
!  sol_rock(l,jj) = (sol_rock(l,jj) * sol_msn(l) + smix(20) * sol_msm(l)) / sol_mass(l)
!            sol_ph(l,jj) = (sol_ph(l,jj) * sol_msn(l) + smix(21)        &
!     &           * sol_msm(l)) / sol_mass(l) !! mjw rev 490 simplified, PH not linear
!            sol_cal(l,jj) = (sol_cal(l,jj) * sol_msn(l) + smix(22)      &
!     &           * sol_msm(l)) / sol_mass(l) !! mjw rev 490
    
    
    
    DO k = 1, npmx
      sol_pst(k,jj,l) = sol_pst(k,jj,l) * ww3 + smix(20+k) * ww4
    END DO
    
!!by zhang
!!=============
    IF (cswat == 2) THEN
      sol_lsc(l,jj) = sol_lsc(l,jj)*ww3+smix(20+npmx+1)* ww4
      sol_lslc(l,jj) = sol_lslc(l,jj)*ww3+smix(20+npmx+2)* ww4
      sol_lslnc(l,jj) = sol_lslnc(l,jj)*ww3+smix(20+npmx+3)* ww4
      sol_lmc(l,jj) = sol_lmc(l,jj)*ww3 + smix(20+npmx+4)* ww4
      sol_lm(l,jj) = sol_lm(l,jj)*ww3 + smix(20+npmx+5)* ww4
      sol_lsl(l,jj) = sol_lsl(l,jj)*ww3 + smix(20+npmx+6)* ww4
      sol_ls(l,jj) = sol_ls(l,jj)*ww3 + smix(20+npmx+7)* ww4
      sol_lsn(l,jj) = sol_lsn(l,jj)*ww3 + smix(20+npmx+8)* ww4
      sol_lmn(l,jj) = sol_lmn(l,jj)*ww3 + smix(20+npmx+9)* ww4
      sol_bmn(l,jj) = sol_bmn(l,jj)*ww3 + smix(20+npmx+10)* ww4
      sol_hsn(l,jj) = sol_hsn(l,jj)*ww3 + smix(20+npmx+11)* ww4
      sol_hpn(l,jj) = sol_hpn(l,jj)*ww3 + smix(20+npmx+12)* ww4
    END IF
!!by zhang
!!==============
    
  END DO
  
  IF (cswat == 1) THEN
    CALL tillfactor(jj,bmix,emix,dtil,sol_thick)
  END IF
  
!! summary calculations
  IF (curyr > nyskip) THEN
    sumix(jj) = sumix(jj) + emix
  END IF
  
END IF

!! perform final calculations for tillage operation

!! count the tillage only if it is a scheduled operation biomix does not count MJW Rev 490
IF (bmix <= 1.e-6) THEN
  ntil(jj) = ntil(jj) + 1
END IF
IF (cnop > 1.e-4) CALL curno(cnop,jj)

!ntil(jj) = ntil(jj) + 1 ' orig code

RETURN
END SUBROUTINE newtillmix
