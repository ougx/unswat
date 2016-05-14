SUBROUTINE swu
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes potential plant evaporation through
!!    the root zone and calculates actual plant water use based on soil
!!    water availability. Also estimates water stress factor.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et)
!!                               |that can occur on current day in HRU
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    idc(:)      |none          |crop/landcover category:
!!                               |1 warm season annual legume
!!                               |2 cold season annual legume
!!                               |3 perennial legume
!!                               |4 warm season annual
!!                               |5 cold season annual
!!                               |6 perennial
!!                               |7 trees
!!    idplt(:)    |none          |land cover code from crop.dat
!!    ihru        |none          |HRU number
!!    iwatable    |none          |high water table code:
!!                               |0 no high water table
!!                               |1 high water table
!!    nro(:)      |none          |sequence number of year in rotation
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp water)
!!    sol_nly(:)  |none          |number of soil layers in profile
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |current day
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    sol_z(:,:)  |mm            |depth to bottom of soil layer
!!    sol_zmx(:)  |mm            |maximum rooting depth
!!    stsol_rd(:) |mm            |storing last soil root depth for use in harvestkillop/killop
!!    ubw         |none          |water uptake distribution parameter
!!                               |This parameter controls the amount of
!!                               |water removed from the different soil layers
!!                               |by the plant. In particular, this parameter
!!                               |allows the amount of water removed from
!!                               |the surface layer via plant uptake to be
!!                               |controlled. While the relationship between
!!                               |UBW and H2O removed from the surface layer is
!!                               |affected by the depth of the soil profile, in
!!                               |general, as UBW increases the amount of water
!!                               |removed from the surface layer relative to the
!!                               |amount removed from the entire profile
!!                               |increases
!!    uobw        |none          |water uptake normalization parameter
!!                               |This variable normalizes the water uptake so
!!                               |that the model can easily verify that uptake
!!                               |from the different soil layers sums to 1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs
!!                               |on day in HRU
!!    sol_rd      |mm            |current rooting depth
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |current day
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on
!!                               |current day
!!    strsw(:)    |none          |fraction of potential plant growth achieved
!!                               |on the day where the reduction is caused by
!!                               |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |
!!    ir          |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    reduc       |none          |fraction of water uptake by plants achieved
!!                               |where the reduction is caused by low water
!!                               |content
!!    sum         |
!!    sump        |
!!    wuse(:)     |mm H2O        |water uptake by plants in each soil layer
!!    xx          |mm H2O        |water uptake by plants from all layers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
use rossmod

INTEGER :: j, k, ir
REAL, DIMENSION(mlyr) :: wuse
REAL :: sum, xx, gx, reduc, sump

j = 0
j = ihru

select case (idc(idplt(j)))
case (1, 2, 4, 5)
sol_rd = 2.5 * phuacc(j) * sol_zmx(j)
IF (sol_rd > sol_zmx(j)) sol_rd = sol_zmx(j)
IF (sol_rd < 10.) sol_rd = 10.
case default
sol_rd = sol_zmx(j)
END select

stsol_rd(j) = sol_rd ! cole armen 26 Feb

!!-------------------OGXinSWAT: Root depth
IF (ievent>0) solcol(j)%deprot=sol_rd

IF (ep_max <= 0.01) THEN
  strsw(j) = 1.
  RETURN
ELSE
!! initialize variables
  gx = 0.
  ir = 0
  sump = 0.
  wuse = 0.
  xx = 0.
  
!!  compute aeration stress
  IF (sol_sw(j) > sol_sumfc(j)) THEN
    satco = (sol_sw(j) - sol_sumfc(j)) / (sol_sumul(j) - sol_sumfc(j))
    pl_aerfac = .85
    scparm = 100. * (satco - pl_aerfac) / (1.0001 - pl_aerfac)
    IF (scparm > 0.) THEN
      strsa(j) = 1. - (scparm / (scparm + EXP(2.9014 - .03867 * scparm)))
    ELSE
      strsa(j) = 1.
    END IF
  END IF
  
  
  DO k = 1, sol_nly(j)
    IF (ir > 0) EXIT
    
    IF (sol_rd <= sol_z(k,j)) THEN
      gx = sol_rd
      ir = k
    ELSE
      gx = sol_z(k,j)
    END IF
    
    sum = 0.
    IF (sol_rd <= 0.01) THEN
      sum = ep_max / uobw
    ELSE
      sum = ep_max * (1. - EXP(-ubw * gx / sol_rd)) / uobw
    END IF
    
!! don't allow compensation for aeration stress
!          if (strsa(j) > .99) then
!           yy = 0.
!          else
!            yy= sump - xx
!          end if
    wuse(k) = sum - sump + 1. * epco(j)
    wuse(k) = sum - sump + (sump - xx) * epco(j)
    sump = sum
    
!!! commented aeration stress out !!!
!! adjust uptake if sw is greater than 90% of plant available water
!! aeration stress
!         yy = air_str(idplt(j))
!         satco = 100. * (sol_st(k,j) / sol_ul(k,j) - yy) / (1. - yy)
!         if (satco > 0.) then
!           strsa(j) = 1. - (1. - (satco / (satco + Exp(5.1 - .082 *
!    &                                                      satco))))
!         else
!           strsa(j) = 1.
!         end if
!         wuse(k) = strsa(j) * wuse(k)
!         if (iwatable(j) > 0) then
!           yy = sol_sumfc(j) + .08 * (sol_sumul(j) - sol_sumfc(j))
!           yy = sol_fc(k,j) + .01 * (sol_ul(k,j) - sol_fc(k,j))
!           if (sol_sw(j) > yy) then
!             wuse(k) = 0.
!           endif
!         endif
    
!! adjust uptake if sw is less than 25% of plant available water
    reduc = 0.
    IF (sol_st(k,j) < sol_fc(k,j)/4.) THEN
      reduc = EXP(5. * (4. * sol_st(k,j) / sol_fc(k,j) - 1.))
    ELSE
      reduc = 1.
    END IF
    reduc = 1.
    wuse(k) = wuse(k) * reduc
    
    IF (sol_st(k,j) < wuse(k)) THEN
      wuse(k) = sol_st(k,j)
    END IF
    
    sol_st(k,j) = MAX(1.e-6, sol_st(k,j) - wuse(k))
    xx = xx + wuse(k)
  END DO
  
!! update total soil water in profile
  sol_sw(j) = 0.
  DO k = 1, sol_nly(j)
    sol_sw(j) = sol_sw(j) + sol_st(k,j)
  END DO
  
  strsw(j) = xx / ep_max
  ep_day = xx
END IF

RETURN
END SUBROUTINE swu
