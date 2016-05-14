SUBROUTINE wmeas
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:05

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads in wind speed data from file and assigns the
!!    data to HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    id1         |julian date   |first day of simulation in current year
!!    ifirstw     |none          |wind speed data search code
!!                               |0 first day of wind speed data located in
!!                               |  file
!!                               |1 first day of wind speed data not
!!                               |  located in file
!!    iwgage(:)   |none          |HRU wind speed gage data code (gage # for
!!                               |wind speed data used in HRU)
!!    iyr         |none          |beginning year of simulation
!!    mrg         |none          |maximum number of rainfall/temp gages
!!    nhru        |none          |number of HRUs in watershed
!!    nwtot       |none          |total number of wind speed records
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ifirstw     |none          |wind speed data search code
!!                               |0 first day of wind speed data located in
!!                               |  file
!!                               |1 first day of wind speed data not
!!                               |  located in file
!!    u10(:)      |m/s           |wind speed for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idap        |julian date   |julian date of measured weather data
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |last 2 digits of year measured weather data
!!    k           |none          |counter
!!    l           |none          |counter
!!    u10bsb      |m/s           |generated wind speed for subbasin
!!    wndmeas(:)  |m/s           |wind speed read from file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: wndgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: k, iyp, idap, l, inum3sprev
REAL :: u10bsb
REAL, DIMENSION (mrg) :: wndmeas

!! initialize variables for the day
wndmeas = 0.


!! read wind speed data from file
IF (ifirstw == 0) THEN
  READ (139,5200) (wndmeas(l), l = 1, nwtot)
ELSE
  ifirstw = 0
  DO
    iyp = 0
    idap = 0
    READ (139,5300) iyp, idap, (wndmeas(l), l = 1, nwtot)
    IF (iyp + idap <= 0) EXIT
    IF (iyp == iyr .AND. idap == id1) EXIT
  END DO
END IF

!! assign wind speed data to HRUs
inum3sprev = 0
DO k = 1, nhru
  u10(k) = wndmeas(iwgage(hru_sub(k)))
!! generate values to replace missing data
  IF (u10(k) <  -97.) THEN
!! use same generated data for all HRUs in a subbasin
    IF (hru_sub(k) == inum3sprev .AND. hru_sub(k) /= 0) THEN
      u10(k) = u10bsb
    ELSE
      CALL wndgen(k)
!! set subbasin generated values
      inum3sprev = 0
      u10bsb = 0.
      inum3sprev = hru_sub(k)
      u10bsb = u10(k)
    END IF
  END IF
END DO

RETURN
! 5200 format (7x,300f8.3)
! 5300 format (i4,i3,300f8.3)
5200 FORMAT (7X,1800F8.3)
5300 FORMAT (i4,i3,1800F8.3)
END SUBROUTINE wmeas
