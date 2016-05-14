SUBROUTINE enrsb(iwave)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the enrichment ratio for nutrient and
!!    pesticide transport with runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha       |ha            |area of watershed in hectares
!!    ihru        |none          |HRU number
!!    iwave       |none          |flag to differentiate calculation of HRU and
!!                               |subbasin sediment calculation
!!                               |iwave = 0 for HRU
!!                               |iwave = subbasin # for subbasin
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    sub_surfq(:)|nn H2O        |surface runoff generated on day in subbasin
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio     |none          |enrichment ratio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cy          |
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: iwave

INTEGER :: j
REAL :: cy

j = 0
j = ihru

IF (sedyld(j) < 1.e-4) THEN
  sedyld(j) = 0.0
  sanyld(j) = 0.0
  silyld(j) = 0.0
  clayld(j) = 0.0
  sagyld(j) = 0.0
  lagyld(j) = 0.0
END IF

!! CREAMS method for calculating enrichment ratio
cy = 0.
IF (iwave > 0) THEN
!! subbasin sediment calculations
  cy = .1 * sedyld(j) / (da_ha * sub_fr(iwave) * sub_surfq(iwave) + 1.e-6)
ELSE
!! HRU sediment calculations
  cy = .1 * sedyld(j) / (hru_ha(j) * surfq(j) + 1.e-6)
END IF

IF (cy > 1.e-6) THEN
  enratio = .78 * cy ** (-.2468)
ELSE
  enratio = 0.
END IF

IF (enratio > 3.5) enratio = 3.5

RETURN
END SUBROUTINE enrsb
