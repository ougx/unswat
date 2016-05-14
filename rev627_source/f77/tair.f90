FUNCTION tair(hr,jj)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function approximates hourly air temperature from daily max and
!!    min temperatures as documented by Campbell (1985)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmp_hi(:)   |deg C         |last maximum temperature in HRU
!!    tmp_lo(:)   |deg C         |last minimum temperature in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tair        |deg C         |air temperature for hour in HRU
!!    tmp_hi(:)   |deg C         |last maximum temperature in HRU
!!    tmp_lo(:)   |deg C         |last minimum temperature in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hr          |none          |hour if the day
!!    jj          |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Cos

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!    subroutine modified by SLN

use parm


REAL, INTENT(INOUT)                      :: hr
INTEGER, INTENT(INOUT)                   :: jj


REAL :: tair

!! update hi or lo temperature depending on hour of day
IF (hr == 3) tmp_lo(jj) = tmn(jj)
IF (hr == 15) tmp_hi(jj) = tmx(jj)

!! SWAT manual equation 2.3.1
tair = 0.
tair = 0.5 * (tmp_hi(jj) + tmp_lo(jj) + (tmp_hi(jj) - tmp_lo(jj)  &
    * COS(0.2618 * REAL(hr - 15))))

RETURN
END FUNCTION tair
