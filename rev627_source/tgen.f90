SUBROUTINE tgen(j)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates temperature data when the user chooses to
!!    simulate or when data is missing for particular days in the
!!    weather file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    i_mo        |none          |month being simulated
!!    pr_w(3,:,:) |none          |proportion of wet days in month
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    tmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
!!                               |temperature
!!    tmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
!!                               |temperature
!!    wgncur(1,:) |none          |parameter which predicts impact of precip on
!!                               |daily maximum air temperature
!!    wgncur(2,:) |none          |parameter which predicts impact of precip on
!!                               |daily minimum air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    tamp        |deg C         |difference between mean monthly air temperature
!!                               |and monthly max or min temperature
!!    tmng        |deg C         |generated minimum temperature for the day
!!    tmxg        |deg C         |generated maximum temperature for the day
!!    txxm        |deg C         |modified monthly maximum temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: j


REAL :: tmxg, tmng, tamp, txxm


tamp = 0.
txxm = 0.
tmng = 0.
tmxg = 0.
tamp = (tmpmx(i_mo,hru_sub(j)) - tmpmn(i_mo,hru_sub(j))) / 2
txxm = tmpmx(i_mo,hru_sub(j)) + tamp * pr_w(3,i_mo,hru_sub(j))

IF (subp(j) > 0.0) txxm = txxm - tamp

tmxg = txxm + tmpstdmx(i_mo,hru_sub(j)) * wgncur(1,j)
tmng = tmpmn(i_mo,hru_sub(j)) + tmpstdmn(i_mo,hru_sub(j)) * wgncur(2,j)
IF (tmng > tmxg) tmng = tmxg - .2 * ABS(tmxg)

tmx(j) = tmxg
tmn(j) = tmng

RETURN
END SUBROUTINE tgen
