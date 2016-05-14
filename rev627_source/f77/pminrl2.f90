SUBROUTINE pminrl2
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes p flux between the labile, active mineral
!!    and stable mineral p pools.
!!    this is the alternate phosphorus model described in Vadas and White (2010)


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr        |none          |current year of simulation
!!    hru_dafr(:)  |km**2/km**2   |fraction of watershed area in HRU
!!    ihru         |none          |HRU number
!!    nyskip       |none          |number of years to skip output summarization
!!                                |and printing
!!    psp          |none          |Phosphorus availability index. The fraction
!!                                |of fertilizer P remaining in labile pool
!!                                |after initial rapid phase of P sorption
!!    sol_actp(:,:)|kg P/ha       |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_nly(:)   |none          |number of layers in soil profile
!!    sol_solp(:,:)|kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:)|kg P/ha       |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    wshd_pal     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from labile mineral to active mineral pool
!!                                |in watershed
!!    wshd_pas     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from active mineral to stable mineral pool
!!                                |in watershed
!!    sol_clay(:,:) |%            |percent clay content in soil material
!!    sol_cbn(:,:)  |%            |percent organic carbon in soil layer

!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp)
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer
!!                               |on any given day (less wp water)


!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rmp1tl       |kg P/ha       |amount of phosphorus moving from the labile
!!                                |mineral pool to the active mineral pool in
!!                                |the soil profile on the current day in the
!!                                |HRU
!!    roctl        |kg P/ha       |amount of phosphorus moving from the active
!!                                |mineral pool to the stable mineral pool
!!                                |in the soil profile on the current day in
!!                                |the HRU
!!    sol_actp(:,:)|kg P/ha       |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_solp(:,:)|kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:)|kg P/ha       |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    wshd_pal     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from labile mineral to active mineral pool
!!                                |in watershed
!!    wshd_pas     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from active mineral to stable mineral pool
!!                                |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    solp(:)  |mg/kg        |Solution pool phosphorous content
!!    actp(:)  |mg/kg        |Active pool phosphorous content
!!    stap(:)  |mg/kg        |Stable pool phosphorous content
!!    vara            |Intermediate Variable
!!    varb            |Intermediate Variable
!!    varc            |Intermediate Variable
!!    arate             |Intermediate Variable      |
!!    j           |none            |HRU number
!!    l           |none            |counter (soil layer)
!!    rmn1        |kg P/ha         |amount of phosphorus moving from the solution
!!                                 |mineral to the active mineral pool in the
!!                                 |soil layer
!!    roc         |kg P/ha         |amount of phosphorus moving from the active
!!                                 |mineral to the stable mineral pool in the
!!                                 |soil layer
!!    rto         |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
INTEGER :: j, l
REAL :: rto, rmn1, roc, wetness, base vara,varb,varc,as_p_coeff
REAL*8  solp(mlyr),actp(mlyr),stap(mlyr) !! locals for concentation based data

j = 0
j = ihru

DO l = 1, sol_nly(j) !! loop through soil layers in this HRU
!! make sure that no zero or negative pool values come in
  IF (sol_solp(l,j) <= 1.e-6) sol_solp(l,j) = 1.e-6
  IF (sol_actp(l,j) <= 1.e-6) sol_actp(l,j) = 1.e-6
  IF (sol_stap(l,j) <= 1.e-6) sol_stap(l,j) = 1.e-6
  
!! Convert kg/ha to ppm so that it is more meaningful to compare between soil layers
  solp(l) = sol_solp(l,j) / conv_wt(l,j) * 1000000.
  actp(l) = sol_actp(l,j) / conv_wt(l,j) * 1000000.
  stap(l) = sol_stap(l,j)/ conv_wt(l,j) * 1000000.
  
  
!! ***************Soluble - Active Transformations***************
  
!! Dynamic PSP Ratio
!!PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
  IF (sol_clay(l,j) > 0.) THEN
    psp = -0.045 * LOG(sol_clay(l,j))+ (0.001 * solp(l))
    psp = psp - (0.035  * sol_cbn(l,j)) + 0.43
  ELSE
    psp = 0.4
  END IF
!! Limit PSP range
  IF (psp <.1)  psp = 0.1 ! limits on PSP
  IF (psp > 0.7)  psp = 0.7
  
!! Calculate smoothed PSP average
  IF (psp_store(l,j) > 0.) THEN
    psp = (psp_store(l,j) * 29. + psp * 1.)/30
  END IF
!! Store PSP for tomarrows smoothing calculation
  psp_store(l,j) = psp
  
!!***************Dynamic Active/Soluble Transformation Coeff******************
  
!! on day 1 just set to a value of zero
  IF ((iida == 1) .AND. (curyr == 1)) THEN
    a_days(l,j) = 0 !! days since P Application
    b_days(l,j) = 0 !! days since P deficit
  END IF
  
!! Calculate P balance
  rto = 0.
  rto = psp / (1.-psp)
  rmn1 = 0.
  rmn1 = sol_solp(l,j) - sol_actp(l,j) * rto !! P imbalance
  
!! Move P between the soluble and active pools based on vadas et al., 2006
  IF (rmn1 >= 0.) THEN !! Net movement from soluble to active
    rmn1 = MAX(rmn1, (-1 * sol_solp(l,j)))
!! Calculate Dynamic Coefficant
    vara = 0.918 * (EXP(-4.603 * psp))
    varb = (-0.238 * ALOG(vara)) - 1.126
    IF (a_days(l,j) >0) THEN
      arate = vara * (a_days(l,j) ** varb)
    ELSE
      arate = vara * (1) ** varb
    END IF
!! limit rate coeff from 0.05 to .5 helps on day 1 when a_days is zero
    IF (arate > 0.5) arate  = 0.5
    IF (arate < 0.1) arate  = 0.1
    rmn1 = (arate) * rmn1
    a_days(l,j) = a_days(l,j)  + 1 !! add a day to the imbalance counter
    b_days(l,j) = 0
  END IF
  
  IF (rmn1 < 0.) THEN !! Net movement from Active to Soluble
    rmn1 = MIN(rmn1, sol_actp(l,j))
!! Calculate Dynamic Coefficant
    base = (-1.08 * psp) + 0.79
    varc = base * (EXP (-0.29))
!! limit varc from 0.1 to 1
    IF (varc > 1.0) varc  = 1.0
    IF (varc < 0.1) varc  = 0.1
    rmn1 = rmn1 * varc
    a_days(l,j) = 0
    b_days(l,j) = b_days(l,j)  + 1 !! add a day to the imbalance counter
  END IF
  
!!*************** Active - Stable Transformations ******************
!! Estimate active stable transformation rate coeff
!! original value was .0006
!! based on linear regression rate coeff = 0.005 @ 0% CaCo3 0.05 @ 20% CaCo3
  as_p_coeff = 0.0023 * sol_cal(l,j) + 0.005
  IF (as_p_coeff > 0.05) as_p_coeff = 0.05
  IF (as_p_coeff < 0.002) as_p_coeff = 0.002
!! Estimate active/stable pool ratio
!! Generated from sharpley 2003
  
  xx = actp(l) + (actp(l) * rto)
  IF (xx > 1.e-6) THEN
    ssp = 25.044 * xx ** -0.3833
  END IF
  
! limit ssp to range in measured data
  IF (ssp > 10.) ssp = 10.
  IF (ssp < 0.7) ssp = 0.7
  
! Smooth ssp, no rapid changes
  IF (ssp_store(l,j) > 0.) THEN
    ssp = (ssp + ssp_store(l,j) * 99.)/100.
  END IF
  
  roc = 0.
  roc = ssp * (sol_actp(l,j) + sol_actp(l,j) * rto)
  roc = roc - sol_stap(l,j)
  roc = as_p_coeff * roc
!! Store todays ssp for tomarrows calculation
  ssp_store(l,j) = ssp
  
!! **************** Account for Soil Water content, do not allow movement in dry soil************
  wetness = (sol_st(l,j)/sol_fc(l,j)) !! range from 0-1 1 = field cap
  IF (wetness >1.)  wetness = 1.
  IF (wetness <0.25)  wetness = 0.25
  rmn1 = rmn1 * wetness
  roc  = roc  * wetness
  
!! If total P is greater than 10,000 mg/kg do not allow transformations at all
  IF ((solp(l) + actp(l) + stap(l)) < 10000.) THEN
!! Allow P Transformations
    sol_stap(l,j) = sol_stap(l,j) + roc
    IF (sol_stap(l,j) < 0.) sol_stap(l,j) = 0.
    sol_actp(l,j) = sol_actp(l,j) - roc + rmn1
    IF (sol_actp(l,j) < 0.) sol_actp(l,j) = 0.
    sol_solp(l,j) = sol_solp(l,j) - rmn1
    IF (sol_solp(l,j) < 0.) sol_solp(l,j) = 0.
  END IF
  
!! Add water soluble P pool assume 1:5 ratio based on sharpley 2005 et al
  sol_watp(l,j) = sol_solp(l,j) / 5
  
  IF (curyr > nyskip) THEN
    wshd_pas = wshd_pas + roc * hru_dafr(j)
    wshd_pal = wshd_pal + rmn1 * hru_dafr(j)
    roctl = roctl + roc
    rmp1tl = rmp1tl + rmn1
  END IF
END DO
RETURN
END SUBROUTINE pminrl2
