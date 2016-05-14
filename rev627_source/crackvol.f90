SUBROUTINE crackvol
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this surboutine computes total crack volume for the soil profile and
!!    modifies surface runoff to account for crack flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp)
!!    sol_nly(:)  |none          |number of soil layers in HRU
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |any given day (less wp water)
!!    sol_sumfc(:)|mm H2O        |amount of water held in soil profile at
!!                               |field capacity
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on
!!                               |any given day
!!    crdep(:,:)  |mm            |maximum or potential crack volume
!!    volcr(:,:)  |mm            |crack volume for soil layer
!!    volcrmin    |mm            |minimum crack volume allowed in any soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    volcr(:,:)  |mm            |crack volume for soil layer
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    crlag       |none          |lag factor for day
!!    crlagdry    |none          |lag in crack development when soil is dry
!!    crlagwet    |none          |lag in crack development when soil is wet
!!    j           |none          |HRU number
!!    l           |none          |counter
!!    volcrnew    |mm            |crack volume for soil layer based on new
!!                               |moisture conditions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: l, j
REAL :: volcrnew, crlag, crlagdry = .99, crlagwet = 0.

j = 0
j = ihru
voltot = 0.

!! calculate volume of cracks in soil
DO l = 1, sol_nly(j)
  volcrnew = 0.
  crlag = 0.
  volcrnew = crdep(l,j) * (sol_fc(l,j) - sol_st(l,j)) / (sol_fc(l,j))
  IF (sol_sw(j) < .90 * sol_sumfc(j)) THEN
    IF (volcrnew > volcr(l,j)) THEN
      crlag = crlagdry
    ELSE
      crlag = crlagwet
    END IF
  ELSE
    crlag = crlagwet
  END IF
  volcr(l,j) = crlag * volcr(l,j) + (1. - crlag) * volcrnew
  IF (volcr(l,j) < 0.) volcr(l,j) = 0.
  voltot = voltot + volcr(l,j) + volcrmin
END DO

RETURN
END SUBROUTINE crackvol
