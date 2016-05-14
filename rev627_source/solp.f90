SUBROUTINE solp
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of phosphorus lost from the soil
!!    profile in runoff and the movement of soluble phosphorus from the first
!!    to the second layer via percolation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conv_wt(:,:)  |none         |factor which converts kg/kg soil to kg/ha
!!    curyr         |none         |current year of simulation
!!    hru_dafr(:)   |none         |fraction of watershed area located in HRU
!!    ihru          |none         |HRU number
!!    nyskip        |none         |number of years to skip output summarization
!!                                |and printing
!!    phoskd        |none         |Phosphorus soil partitioning coefficient
!!                                |Ratio of phosphorus attached to sediment to
!!                                |phosphorus dissolved in soil water
!!    pperco        |none         |phosphorus percolation coefficient (0-1)
!!                                |0:concentration of soluble P in surface
!!                                |  runoff is zero
!!                                |1:percolate has same concentration of soluble
!!                                |  P as surface runoff
!!    sol_bd(:,:)   |Mg/m**3      |bulk density of the soil
!!    sol_nly(:)    |none         |number of layers in soil profile
!!    sol_prk(:,:)  |mm H2O       |percolation from soil layer on current day
!!    sol_solp(:,:) |kg P/ha      |amount of phosohorus stored in solution
!!    sol_z(:,:)    |mm           |depth to bottom of soil layer
!!    surfq(:)      |mm H2O       |surface runoff generated on day in HRU
!!    wshd_plch     |kg P/ha      |average annual amount of phosphorus leached
!!                                |into second soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    wshd_plch     |kg P/ha       |average annual amount of phosphorus leached
!!                                 |into second soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    vap         |kg P/ha       |amount of P leached from soil layer
!!    xx          |none          |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j
REAL :: xx, vap, vap_tile

j = 0
j = ihru
vap_tile = 0

!! compute soluble P lost in surface runoff
xx = 0.
xx = sol_bd(1,j) * sol_z(1,j) * phoskd
surqsolp(j) = sol_solp(1,j) * surfq(j) / xx
!!units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
!     if (surfq(j) > 0.001) then
!     write (17,77) i, iyr, sol_bd(1,j), sol_z(1,j), phoskd, surfq(j),  &
!    &              sol_solp(1,j), surqsolp(j)
!     end if
! 77  format(2i6,6f10.3)
surqsolp(j) = MIN(surqsolp(j), sol_solp(1,j))
surqsolp(j) = MAX(surqsolp(j), 0.)
sol_solp(1,j) = sol_solp(1,j) - surqsolp(j)


!! compute soluble P leaching
vap = 0.
vap = sol_solp(1,j) * sol_prk(1,j) / ((conv_wt(1,j) / 1000.) * pperco)
vap = MIN(vap, .5 * sol_solp(1,j))
sol_solp(1,j) = sol_solp(1,j) - vap

!! estimate soluble p in tiles due to crack flow
IF (ldrain(j) > 0) THEN
  xx = MIN(1., sol_crk(j) / 3.0)
  vap_tile = xx * vap
  vap = vap - vap_tile
END IF

IF (sol_nly(j) >= 2) THEN
  sol_solp(2,j) = sol_solp(2,j) + vap
END IF

DO ii = 2, sol_nly(j)
  vap = 0.
  IF (ii /= i_sep(j)) THEN
    vap = sol_solp(ii,j) * sol_prk(ii,j) / ((conv_wt(ii,j)  &
        / 1000.) * pperco_sub(ii,j))
    vap = MIN(vap, .2 * sol_solp(ii,j))
    sol_solp(ii,j) = sol_solp(ii,j) - vap
    IF (ii == sol_nly(j)) THEN
      sol_solp(ii+1,j) = sol_solp(ii+1,j) + vap
    END IF
!         if (ii == ldrain(j)) then
!           vap = sol_solp(ii,j) * qtile / (conv_wt(ii,j) / 1000.
!     *                                         * pperco_sub(ii,j))
!           sol_solp(ii,j) = sol_solp(ii,j) - vap
!           tilep = vap
!         endif
  END IF
END DO
percp(j) = vap

!! summary calculation
IF (curyr > nyskip) THEN
  wshd_plch = wshd_plch + vap * hru_dafr(j)
  wshd_ptile = wshd_ptile + vap_tile * hru_dafr(j)
END IF

RETURN
END SUBROUTINE solp
