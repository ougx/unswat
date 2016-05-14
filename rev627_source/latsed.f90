SUBROUTINE latsed
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the sediment load contributed in lateral flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_km(:)   |km^2          |area of HRU in square kilometers
!!    ihru        |none          |HRU number
!!    lat_sed(:)  |g/L           |sediment concentration in lateral flow
!!    latq(:)     |mm H2O        |total lateral flow in soil profile for the
!!                               |day in HRU
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j

j = 0
j = ihru

!! update sediment yield for sediment in lateral flow
sedyld(j) = sedyld(j) + (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j)

sanyld(j) = sanyld(j) +  &
    (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_san(j)
silyld(j) = silyld(j) +  &
    (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_sil(j)
clayld(j) = clayld(j) +  &
    (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_cla(j)
sagyld(j) = sagyld(j) +  &
    (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_sag(j)
lagyld(j) = lagyld(j) +  &
    (latq(j) + gw_q(j)) * hru_km(j) * lat_sed(j) * det_lag(j)

!! organic n and p in the lateral flow     - by J.Jeong BREC 2011
!mm*mg/L*1000L/m3*kg/1000000mg*m3/10ha-mm=kg/ha
sedorgn(j) = sedorgn(j) + (latq(j) + gw_q(j)) * lat_orgn(j) / 10000.
sedorgp(j) = sedorgp(j) + (latq(j) + gw_q(j)) * lat_orgp(j) / 10000.

IF (sedyld(j) < 0.) sedyld(j) = 0.
IF (sanyld(j) < 0.) sanyld(j) = 0.0
IF (silyld(j) < 0.) silyld(j) = 0.0
IF (clayld(j) < 0.) clayld(j) = 0.0
IF (sagyld(j) < 0.) sagyld(j) = 0.0
IF (lagyld(j) < 0.) lagyld(j) = 0.0
IF (sedorgn(j) < 0.) sedorgn(j) = 0.0
IF (sedorgp(j) < 0.) sedorgp(j) = 0.0

RETURN
END SUBROUTINE latsed
