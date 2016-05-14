SUBROUTINE hmeas
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads in relative humidity data from file and
!!    assigns the data to the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    id1         |julian date   |first day of simulation in current year
!!    ifirsth     |none          |relative humidity data search code
!!                               |0 first day of relative humidity data located
!!                               |  in file
!!                               |1 first day of relative humidity data not
!!                               |  located in file
!!    ihgage(:)   |none          |HRU relative humidity data code (gage # for
!!                               |relative humidity data used in HRU)
!!    iyr         |none          |beginning year of simulation
!!    mrg         |none          |maximum number of rainfall/temp gages
!!    nhru        |none          |number of HRUs in watershed
!!    nhtot       |none          |total number of relative humidity records
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ifirsth     |none          |relative humidity data search code
!!                               |0 first day of relative humidity data located
!!                               |  in file
!!                               |1 first day of relative humidity data not
!!                               |  located in file
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idap        |julian date   |julian date of measured weather data
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |last 2 digits of year measured weather data
!!    k           |none          |counter
!!    l           |none          |counter
!!    rhdbsb      |none          |generated relative humidity for subbasin
!!    rhmeas(:)   |none          |relative humidity read in from file (fraction)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: rhgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: k, iyp, idap, l, inum3sprev
REAL :: rhdbsb
REAL, DIMENSION (mrg) :: rhmeas

!! initialize variables for the day
rhmeas = 0.

!! read relative humidity data from file
IF (ifirsth == 0) THEN
  READ (138,5200) (rhmeas(l), l = 1, nhtot)
ELSE
  ifirsth = 0
  DO
    iyp = 0
    idap = 0
    READ (138,5300) iyp, idap, (rhmeas(l), l = 1, nhtot)
    IF (iyp + idap <= 0) EXIT
    IF (iyp == iyr .AND. idap == id1) EXIT
  END DO
END IF


!! assign relative humidity data to HRUs
inum3sprev = 0
DO k = 1, nhru
!! generate values to replace missing data
  IF (rhmeas(ihgage(hru_sub(k))) <  -97.) THEN
!! use same generated data for all HRUs in a subbasin
    IF (hru_sub(k) == inum3sprev .AND. hru_sub(k) /= 0) THEN
      rhd(k) = rhdbsb
    ELSE
      CALL rhgen(k)
!! set subbasin generated values
      inum3sprev = 0
      rhdbsb = 0.
      inum3sprev = hru_sub(k)
      rhdbsb = rhd(k)
    END IF
  ELSE
!!       if (i == 1) then
    IF (rhmeas(ihgage(hru_sub(k))) < 1. .AND.  &
          rhmeas(ihgage(hru_sub(k))) > 0.) THEN
      rhd(k) = rhmeas(ihgage(hru_sub(k)))
    ELSE
      tmpmean=(tmpmx(i_mo,hru_sub(k))+tmpmn(i_mo,hru_sub(k)))/2.
      rhd(k) = ee(rhmeas(ihgage(hru_sub(k)))) / ee(tmpmean)
    END IF
  END IF
END DO

RETURN
! 5200 format (7x,300f8.3)
! 5300 format (i4,i3,300f8.3)
5200 FORMAT (7X,1800F8.3)
5300 FORMAT (i4,i3,1800F8.3)
END SUBROUTINE hmeas
