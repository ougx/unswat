SUBROUTINE rhgen(j)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates weather relative humidity, solar
!!    radiation, and wind speed.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dewpt(:,:)  |deg C         |average dew point temperature for the month
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    j           |none          |HRU number
!!    irelh       |none          |irelh = 0 (dewpoint)
!!                               |      = 1 (relative humidity)
!!                               |note:  inputs > 1.0 (dewpoint)
!!                                       inputs < 1.0 (relative humidity)
!!    i_mo        |none          |month being simulated
!!    pr_w(3,:,:) |none          |proportion of wet days in a month
!!    rndseed(:,:)|none          |random number seeds
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    blm         |none          |lowest relative humidity value allowed for
!!                               |any day in month
!!    rhm         |none          |mean monthly relative humidity adjusted for
!!                               |wet or dry condiditions
!!    rhmo        |none          |mean monthly relative humidity
!!    tmpmean     |deg C         |average temperature for the month in HRU
!!    uplm        |none          |highest relative humidity value allowed for
!!                               |any day in month
!!    vv          |none          |variable to hold intermediate calculation
!!    yy          |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp
!!    SWAT: Atri, Ee

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: j


REAL :: vv, rhm, yy, uplm, blm
REAL :: rhmo, tmpmean

!! Climate Paramenters required for Penman-Monteith !!

!! Generate relative humidity !!
rhmo = 0.
yy = 0.
rhm = 0.
vv = 0.
uplm = 0.
blm = 0.
tmpmean = 0.
tmpmean = (tmpmx(i_mo,hru_sub(j)) + tmpmn(i_mo,hru_sub(j))) / 2.

!!   dewpoint or relative humidity --
IF (irelh(hru_sub(j)) == 1) THEN
  rhmo = dewpt(i_mo,hru_sub(j))
ELSE
  rhmo = ee(dewpt(i_mo,hru_sub(j))) / ee(tmpmean)
END IF

yy = 0.9 * pr_w(3,i_mo,hru_sub(j))
rhm = (rhmo - yy) / (1.0 - yy)
IF (rhm < 0.05) rhm = 0.5 * rhmo
IF (subp(j) > 0.0) rhm = rhm * 0.1 + 0.9
vv = rhm - 1.
uplm = rhm - vv * EXP(vv)
blm = rhm * (1.0 - EXP(-rhm))
rhd(j) = atri(blm,rhm,uplm,rndseed(idg(7),j))


RETURN
END SUBROUTINE rhgen
