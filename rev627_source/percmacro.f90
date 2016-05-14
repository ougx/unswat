SUBROUTINE percmacro
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine computes percolation by crack flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc-wp)
!!    sol_nly(:)  |none          |numer of layers in soil profile
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |any given day (less wilting point water)
!!    sol_z(:,:)  |mm            |depth to bottom of soil layer
!!    volcr(:,:)  |mm            |crack volume for soil layer
!!    volcrmin    |mm            |minimum soil volume in profile
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    crk         |mm H2O        |percolation due to crack flow
!!    sepbtm(:)   |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    crklch      |none          |
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layer)
!!    sepcrk      |mm H2O        |water entering cracks in soil
!!    xx          |mm H2O        |water deficiency in soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, ly
REAL :: crklch = 0.5, xx

j = 0
j = ihru

sepcrk = 0.
sepcrk = MIN(voltot, inflpcp)
sepcrktot = sepcrk
IF (sepcrk > 1.e-4) THEN
  DO ly = sol_nly(j), 1, -1
    crk = 0.
    xx = 0.
    IF (ly == sol_nly(j)) THEN
      crk = crklch * (volcr(ly,j) / (sol_z(ly,j) - sol_z(ly-1,j))  &
          * voltot - volcrmin)
      IF (crk < sepcrk) THEN
        sepcrk = sepcrk - crk
        sepbtm(j) = sepbtm(j) + crk
        sol_prk(ly,j) = sol_prk(ly,j) + crk
      ELSE
        sepbtm(j) = sepbtm(j) + sepcrk
        sol_prk(ly,j) = sol_prk(ly,j) + sepcrk
        sepcrk = 0.
      END IF
    END IF
    xx = sol_fc(ly,j) - sol_st(ly,j)
    IF (xx > 0.) THEN
      crk = MIN(sepcrk, xx)
      sol_st(ly,j) = sol_st(ly,j) + crk
      sepcrk = sepcrk - crk
      IF (ly /= 1) sol_prk(ly-1,j) = sol_prk(ly-1,j) + crk
    END IF
    IF (sepcrk < 1.e-6) EXIT
  END DO
  
!! if soil layers filled and there is still water attributed to
!! crack flow, it is assumed to percolate out of bottom of profile
  IF (sepcrk > 1.e-4) THEN
    sepbtm(j) = sepbtm(j) + sepcrk
    sol_prk(sol_nly(j),j) = sol_prk(sol_nly(j),j) + sepcrk
  END IF
END IF

RETURN
END SUBROUTINE percmacro
