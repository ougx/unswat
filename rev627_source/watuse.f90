SUBROUTINE watuse
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:05

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine removes water from appropriate source (pond,
!!    shallow aquifer, and/or deep aquifer) for consumptive water use

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    ihru        |none          |HRU number
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    wudeep(:,:) |10^4 m^3/day  |average daily water removal from the deep
!!                               |aquifer for the month for the HRU within the
!!                               |subbasin
!!    wupnd(:,:)  |10^4 m^3/day  |average daily water removal from the pond
!!                               |for the month for the HRU within the subbasin
!!    wushal(:,:) |10^4 m^3/day  |average daily water removal from the shallow
!!                               |aquifer for the month for the HRU within the
!!                               |subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j
REAL :: cnv

j = 0
j = ihru

cnv = 0.
sub_ha = da_ha * sub_fr(hru_sub(j))
cnv = sub_ha * 10.

pnd_vol(j) = pnd_vol(j) - wupnd(i_mo,j) * 10000.
IF (pnd_vol(j) < 0.) pnd_vol(j) = 0.

rchrg_src(j) = 0.
IF (wushal(i_mo,j) < 0.) THEN
  rchrg_src(j) = -1. * wushal(i_mo,j) * 10000. / cnv
ELSE
  shallst(j) = shallst(j) - wushal(i_mo,j) * 10000. / cnv
  IF (shallst(j) < 0.) shallst(j) = 0.
END IF

deepst(j) = deepst(j) - wudeep(i_mo,j) * 10000. / cnv
IF (deepst(j) < 0.) deepst(j) = 0.

RETURN
END SUBROUTINE watuse
