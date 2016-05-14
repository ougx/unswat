SUBROUTINE tmeas
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads in temperature data and assigns it to the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    id1         |julian date   |first day of simulation in current year
!!    ifirstt(:)  |none          |temperature data search code
!!                               |0 first day of temperature data located in
!!                               |  file
!!                               |1 first day of temperature data not located in
!!                               |  file
!!    itgage(:)   |none          |HRU temperature gage data code (gage # for
!!                               |temperature data used in HRU)
!!    iyr         |none          |beginning year of simulation
!!    mrg         |none          |maximum number of rainfall/temp gages
!!    nhru        |none          |number of HRUs in watershed
!!    ntgage      |none          |number of temperature gage files
!!    ntgfil      |none          |number of temperature gages per file
!!    nttot       |none          |total number of temperature gages
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ifirstt(:)  |none          |temperature data search code
!!                               |0 first day of temperature data located in
!!                               |  file
!!                               |1 first day of temperature data not located in
!!                               |  file
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idap        |julian date   |julian date of measured weather data
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |last 2 digits of year measured weather data
!!    k           |none          |counter
!!    kk1         |none          |gage code for first dataset in weather file
!!    kk2         |none          |gage code for last dataset in weather file
!!    l           |none          |counter
!!    tmnbsb      |deg C         |generated minimum temperature for subbasin
!!    tmxbsb      |deg C         |generated maximum temperature for subbasin
!!    tnmeas(:)   |deg C         |minimum temperature read in from file
!!    txmeas(:)   |deg C         |maximum temperature read in from file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT subroutines: tgen, weatgn

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: k, kk1, kk2, iyp, idap, l, inum3sprev
REAL :: tmxbsb, tmnbsb
REAL, DIMENSION (mrg) :: txmeas, tnmeas

!! initialize variables for the day
txmeas = 0.
tnmeas = 0.


!! read temperature data from files
DO k = 1, ntgage
!! calculate gage id codes for first and last dataset in file
  kk1 = 0
  kk2 = 0
  kk1 = ntgfil * (k - 1) + 1
  IF (k == ntgage) THEN
    kk2 = nttot
  ELSE
    kk2 = kk1 + (ntgfil - 1)
  END IF
  
!! read in date from files
  IF (ifirstt(k) == 0) THEN
    READ (118+k,5000) (txmeas(l), tnmeas(l), l = kk1, kk2)
  ELSE
    ifirstt(k) = 0
    DO
      iyp = 0
      idap = 0
      READ (118+k,5100) iyp, idap, (txmeas(l), tnmeas(l), l = kk1, kk2)
      IF (iyp + idap <= 0) EXIT
      IF (iyp == iyr .AND. idap == id1) EXIT
    END DO
  END IF
END DO

!! assign temperature data to HRUs
inum3sprev = 0
DO k = 1, nhru
  CALL weatgn(k)
  tmx(k) = txmeas(itgage(hru_sub(k)))
  tmn(k) = tnmeas(itgage(hru_sub(k)))
!! generate values to replace missing data
  IF (tmx(k) <  -97. .OR. tmn(k) < -97. .OR. tmx(k) + tmn(k) == 0.) THEN
!! use same generated data for all HRUs in a subbasin
    IF (hru_sub(k) == inum3sprev .AND. hru_sub(k) /= 0) THEN
      tmx(k) = tmxbsb
      tmn(k) = tmnbsb
    ELSE
      CALL tgen(k)
!! set subbasin generated values
      inum3sprev = 0
      tmxbsb = 0.
      tmnbsb = 0.
      inum3sprev = hru_sub(k)
      tmxbsb = tmx(k)
      tmnbsb = tmn(k)
    END IF
  END IF
END DO

RETURN !!   000 format (7x,600f5.1)  &
!!   100 format (i4,i3,600f5.1)
!5000  format (7x,900f5.1)
!5100  format (i4,i3,900f5.1)
5000  FORMAT (7X,3600F5.1)
5100  FORMAT (i4,i3,3600F5.1)
END SUBROUTINE tmeas
