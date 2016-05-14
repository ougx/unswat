SUBROUTINE percmicro(ly1)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes percolation and lateral subsurface flow
!!    from a soil layer when field capacity is exceeded

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_slp(:)   |m/m           |average slope steepness
!!    ihru         |none          |HRU number
!!    iwatable     |none          |high water table code:
!!                                |0 no high water table
!!                                |1 high water table
!!    ldrain(:)    |none          |soil layer where drainage tile is located
!!    slsoil(:)    |m             |slope length for lateral subsurface flow
!!    sol_fc(:,:)  |mm H2O        |amount of water available to plants in soil
!!                                |layer at field capacity (fc - wp water)
!!    sol_hk(:,:)  |none          |beta coefficient to calculate hydraulic
!!                                |conductivity
!!    sol_k(:,:)   |mm/hr         |saturated hydraulic conductivity of soil
!!                                |layer
!!    sol_nly(:)   |none          |number of soil layers in HRU
!!    sol_st(:,:)  |mm H2O        |amount of water stored in the soil layer
!!                                |on any given day
!!    sol_sumfc(:) |mm H2O        |amount of water held in the soil profile
!!                                |at field capacity
!!    sol_sw(:)    |mm H2O        |amount of water stored in the soil profile
!!                                |on any given day
!!    sol_tmp(:,:) |deg C         |daily average temperature of soil layer
!!    sol_ul(:,:)  |mm H2O        |amount of water held in the soil layer at
!!                                |saturation (sat - wp water)
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
!!    sw_excess    |mm H2O        |amount of water in soil that exceeds field
!!                                |capacity (gravity drained water)
!!    tdrain(:)    |hrs           |time to drain soil to field capacity
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latlyr       |mm H2O        |lateral subsurface flow in layer
!!    lyrtile      |mm H2O        |drainage tile flow in layer for day in HRU
!!    sepday       |mm H2O        |percolation from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adjf         |none          |adjustment factor for lateral flow
!!    dg           |mm            |depth of soil layer
!!    ho           |none          |variable to hold intermediate calculation
!!                                |result
!!    j            |none          |HRU number
!!    ly1          |none          |soil layer number
!!    ratio        |none          |ratio of seepage to (latq + sepday)
!!    yy           |mm            |depth to top of soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: ly1

INTEGER :: j
REAL :: adjf, yy, dg, ho, ratio, sol_k_sep

j = 0
j = ihru

adjf = 1.

!! if temperature of layer is 0 degrees C or below
!! there is no water flow
IF (sol_tmp(ly1,j) <= 0.) THEN
  sepday = 0.
  RETURN
END IF

!! COMPUTE LATERAL FLOW USING HILLSLOPE STORAGE METHOD
IF (ly1 == 1) THEN
  yy = 0.
ELSE
  yy = 0.
  yy = sol_z(ly1-1,j)
END IF

dg = 0.
ho = 0.
latlyr = 0.
dg = sol_z(ly1,j) - yy
IF (sol_ul(ly1,j) - sol_fc(ly1,j)==0.) THEN
  ho=0.
ELSE
  ho = 2. * sw_excess / ((sol_ul(ly1,j) - sol_fc(ly1,j)) /  dg)
END IF
latlyr = adjf * ho * sol_k(ly1,j) * hru_slp(j) / slsoil(j) * .024

IF (latlyr < 0.) latlyr = 0.
IF (latlyr > sw_excess) latlyr = sw_excess

sol_hk(ly1,j) = (sol_ul(ly1,j) - sol_fc(ly1,j)) / sol_k(ly1,j)

!!  septic changes 1/28/09
IF (ly1 == i_sep(j)) THEN
  IF (isep_opt(j) == 1) THEN !active system
    sol_k_sep = sol_k(ly1,j)* (sol_st(ly1,j) - sol_fc(ly1,j))/  &
        (sol_ul(ly1,j) - sol_fc(ly1,j))
    sol_k_sep = MAX(1.e-6, sol_k_sep)
    sol_k_sep = MIN(sol_k(ly1,j), sol_k_sep)
    
    sol_hk(ly1,j) = (sol_ul(ly1,j) - sol_fc(ly1,j)) / sol_k_sep
    
  ELSE IF (isep_opt(j) == 2) THEN !failing system
    sol_hk(ly1,j) = 1.e10
  END IF
END IF
!!  septic changes 1/28/09

sol_hk(ly1,j) = MAX(2., sol_hk(ly1,j))

!! compute seepage to the next layer
sepday = 0.
sepday = sw_excess * (1. - EXP(-24. / sol_hk(ly1,j)))

!! limit maximum seepage from biozone layer below potential perc amount
IF(ly1 == i_sep(j).AND.isep_opt(j)==1) THEN
  sepday = MIN(sepday,sol_k_sep *24.)
  bz_perc(j) = sepday
END IF

!! restrict seepage if next layer is saturated
IF (ly1 == sol_nly(j)) THEN
  xx = (dep_imp(j) - sol_z(ly1,j)) / 1000.
  IF (xx < 1.e-4) THEN
    sepday = 0.
  ELSE
    sepday = sepday * xx / (xx + EXP(8.833 - 2.598 * xx))
  END IF
END IF

!! check mass balance
IF (sepday + latlyr > sw_excess) THEN
  ratio = 0.
  ratio = sepday / (latlyr + sepday)
  sepday = 0.
  latlyr = 0.
  sepday = sw_excess * ratio
  latlyr = sw_excess * (1. - ratio)
END IF
IF (sepday + lyrtile > sw_excess) THEN
  sepday = 0.
  sepday = sw_excess - lyrtile
END IF

RETURN
END SUBROUTINE percmicro
