SUBROUTINE openwth
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine opens the precipitation, temperature, solar radiation,
!!    relative humidity and wind speed files for simulations using measured
!!    weather data

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nrgage      |none          |number of raingage files
!!    nrgfil      |none          |number of rain gage per file
!!    nrtot       |none          |total number of rain gages
!!    ntgage      |none          |number of temperature gage files
!!    ntgfil      |none          |number of temperature gages per file
!!    nttot       |none          |total number of temperature gages
!!    petfile     |NA            |potential ET file name (.pet)
!!    rfile(:)    |NA            |rainfall file name (.pcp)
!!    rhfile      |NA            |relative humidity file name (.hmd)
!!    slrfile     |NA            |solar radiation file name (.slr)
!!    tfile(:)    |NA            |temperature file name (.tmp)
!!    wndfile     |NA            |wind speed file name (.wnd)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    elevp(:)    |m             |elevation of precipitation gage station
!!    elevt(:)    |m             |elevation of temperature gage station
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none           |counter
!!    k           |none           |counter
!!    kk1         |none           |gage code for first dataset in weather file
!!    kk2         |none           |gage code for last dataset in weather file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, kk1, kk2, k
CHARACTER (LEN=80) :: titldum

!! open precip files and read elevation
DO j = 1, nrgage
!! calculate gage id codes for first and last dataset in file
  kk1 = 0
  kk2 = 0
  kk1 = nrgfil * (j - 1) + 1
  IF (j == nrgage) THEN
    kk2 = nrtot
  ELSE
    
    kk2 = kk1 + (nrgfil - 1)
  END IF
  IF (rfile(j) /= '             ') THEN  &
!     open (100+j,file=rfile(j),recl=800)
    OPEN (100+j,FILE=rfile(j),RECL=1850)
    READ (100+j,5000) titldum
    READ (100+j,5000) titldum
    READ (100+j,5000) titldum
    IF (ievent < 2) THEN   !daily records
      READ (100+j,5001) (elevp(k), k = kk1, kk2)
    ELSE                   !subdaily records
      READ (100+j,5003) (elevp(k), k = kk1, kk2)
    END IF
  END IF
END DO

DO j = 1, ntgage
!! calculate gage id codes for first and last dataset in file
  kk1 = 0
  kk2 = 0
  kk1 = ntgfil * (j - 1) + 1
  IF (j == ntgage) THEN
    kk2 = nttot
  ELSE
    kk2 = kk1 + (ntgfil - 1)
  END IF
  IF (tfile(j) /= '             ') THEN
!!        open (118+j,file=tfile(j),recl=800)
    OPEN (118+j,FILE=tfile(j),RECL=20000)
    READ (118+j,5000) titldum
    READ (118+j,5000) titldum
    READ (118+j,5000) titldum
    READ (118+j,5002) (elevt(k), k = kk1, kk2)
  END IF
END DO

IF (slrfile /= '             ') THEN
!!       open (137,file=slrfile,recl=800)
  OPEN (137,FILE=slrfile,RECL=15000)
  READ (137,5000) titldum
END IF

IF (rhfile /= '             ') THEN
!!        open (138,file=rhfile,recl=800)
  OPEN (138,FILE=rhfile,RECL=15000)
  READ (138,5000) titldum
END IF

IF (wndfile /= '             ') THEN
!!       open (139,file=wndfile,recl=800)
  OPEN (139,FILE=wndfile,RECL=15000)
  READ (139,5000) titldum
END IF

IF (petfile /= '             ') THEN
  OPEN (140,FILE=petfile)
  READ (140,5000) titldum
END IF

RETURN
5000 FORMAT (a80)
5001 FORMAT (7X,1800I5)
5002 FORMAT (7X,1800I10)
! 5002 format (7x,287i10)
5003 FORMAT (12X,1800I5)
END SUBROUTINE openwth
