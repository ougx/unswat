SUBROUTINE pmeas
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads in precipitation data and assigns it to the
!!    proper subbasins

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |number of subbasin in which HRU is located
!!    i           |julian date   |current day of simulation
!!    id1         |julian date   |first day of simulation in current year
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ifirstpcp(:)|none          |precipitation data search code
!!                               |0 first day of precipitation data located in
!!                               |  file
!!                               |1 first day of precipitation data not located
!!                               |  in file
!!    irgage(:)   |none          |HRU rain gage data code (gage # for rainfall
!!                               |data used in HRU)
!!    iyr         |none          |beginning year of simulation
!!    mrg         |none          |maximum number of rainfall/temp gages
!!    nhru        |none          |number of HRUs in watershed
!!    nrgage      |none          |number of raingage files
!!    nrgfil      |none          |number of rain gage per file
!!    nrtot       |none          |total number of rain gages
!!    nstep       |none          |number of lines of rainfall data for each
!!                               |day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ifirstpcp(:)|none          |precipitation data search code
!!                               |0 first day of precipitation data located in
!!                               |  file
!!                               |1 first day of precipitation data not located
!!                               |  in file
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a           |NA            |check for hour/min ":" on first time step of
!!                               |day
!!    flag        |none          |flag to locate first day of simulation
!!                               |in precipitation file
!!    hrmeas(:,:) |mm H2O        |precipitation falling in hour on day of
!!                               |simulation
!!    idap        |julian date   |julian date of measured weather data
!!    ihour       |none          |hour of measured weather data (0 - 23)
!!    ii          |none          |counter
!!    imin        |none          |minute of measured weather data within hour
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |last 2 digits of year measured weather data
!!    k           |none          |counter
!!    kk1         |none          |gage code for first dataset in weather file
!!    kk2         |none          |gage code for last dataset in weather file
!!    l           |none          |counter
!!    rainsb(:,:) |mm H2O        |precipitation falling in time increment
!!                               |defined by IDT
!!    rbsb        |mm H2O        |generated precipitation for subbasin
!!    rmeas(:)    |mm H2O        |precipitation read in from file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT subroutines: pgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

CHARACTER (LEN=1) :: a
INTEGER :: k, kk1, kk2, iyp, idap, l, inum3sprev, ii
INTEGER :: ihour, imin, flag
REAL :: rbsb
REAL, DIMENSION (mrg) :: rmeas
REAL, DIMENSION (:,:), allocatable :: rainsb
!     real, dimension (:), allocatable :: rhrbsb, rstpbsb
IF (nstep > 0) THEN
  allocate (rainsb(mrg,nstep))
!       allocate (rstpbsb(nstep))
!        allocate (hrmeas(mrg,25))
!       allocate (rhrbsb(24))
END IF

inum3sprev = 0

!! initialize variables for the day
rmeas = 0.
IF (nstep > 0) THEN
  rainsb = 0.
!       hrmeas = 0.
END IF


select case (ievent)
case (0, 1)                       !!daily rainfall

!! read precipitation data from files
DO k = 1, nrgage
!! calculate gage id codes for first and last dataset in file
  kk1 = 0
  kk2 = 0
  kk1 = nrgfil * (k - 1) + 1
  IF (k == nrgage) THEN
    kk2 = nrtot
  ELSE
    kk2 = kk1 + (nrgfil - 1)
  END IF
  
!! read data from file
  IF (ifirstpcp(k) == 0) THEN
    READ (100+k,5000) (rmeas(l), l = kk1, kk2)
  ELSE
    ifirstpcp(k) = 0
    DO
      iyp = 0
      idap = 0
      READ (100+k,5100) iyp, idap, (rmeas(l), l = kk1, kk2)
      IF (iyp + idap <= 0) EXIT
      IF (iyp == iyr .AND. idap == id1) EXIT
    END DO
  END IF
END DO

!! assign precipitation data to HRUsoutput.std

inum3sprev = 0
DO k = 1, nhru
  subp(k) = rmeas(irgage(hru_sub(k)))
!! generate data to replace missing values
  IF (subp(k) < -97.) THEN
!! use same generated data for all HRUs in a subbasin
    IF (hru_sub(k) == inum3sprev .AND. hru_sub(k) /= 0) THEN
      subp(k) = rbsb
      IF (ievent == 1) THEN
        DO l = 1, nstep
          rainsub(k,l) = rstpbsb(l)
        END DO
      END IF
    ELSE
      CALL pgen(k)
!! set subbasin generated values
      inum3sprev = 0
      rbsb = 0.
      inum3sprev = hru_sub(k)
      rbsb = subp(k)
      IF (ievent == 1) THEN
        rstpbsb(:) = 0.
        DO l = 1, nstep
          rstpbsb(l) = rainsub(k,l)
        END DO
      END IF
    END IF
  ELSE
!if (ievent == 1 .and. subp(k) >= 0.01) call pgenhr(k)   !test the
    IF (ievent==1 .AND. subp(k) >= 0.01) CALL pgenhr_pulse(k)   !test the impact of precipitation intensity on runoff generation
  END IF
END DO

case default                   !! subdaily precipitation

!! read precipitation data from files
DO k = 1, nrgage
!! calculate gage id codes for first and last dataset in file
  kk1 = 0
  kk2 = 0
  kk1 = nrgfil * (k - 1) + 1
  IF (k == nrgage) THEN
    kk2 = nrtot
  ELSE
    kk2 = kk1 + (nrgfil - 1)
  END IF
  
!! read data from file
  IF (ifirstpcp(k) == 0) THEN
    READ (100+k,5300) a
    BACKSPACE (100+k)
    IF (a /= " ") THEN               !subdaily precip on day
      DO ii = 1, nstep
        flag = 0
        ihour = 0
        imin = 0
        a = ""
        READ (100+k,5200) iyp, idap, ihour, imin, (rainsb(l,ii), l = kk1, kk2)
        IF (iyp /= iyr .OR. idap /= i) flag = 1
        IF (flag == 1) THEN
          WRITE (24,5400) iyr, i
!!              stop
        END IF
        DO l = kk1, kk2
          rmeas(l) = rmeas(l) + rainsb(l,ii)
        END DO
      END DO
    ELSE                                 !no precip on day
      READ (100+k,5201) iyp, idap, (rmeas(l), l = kk1, kk2)
      IF (iyp /= iyr .OR. idap /= i) flag = 1
      IF (flag == 1) THEN
        WRITE (24,5400) iyr, i !               stop
      END IF
      DO l = kk1, kk2
        DO ii = 1, nstep
          rainsb(l,ii) = 0.
        END DO
        rmeas(l) = 0.
      END DO
    END IF
  ELSE
    ifirstpcp(k) = 0
    flag = 0
    DO
      iyp = 0
      idap = 0
      READ (100+k,5202) iyp, idap, ihour, a, imin, (rainsb(l,1), l = kk1, kk2)
      IF (iyp == iyr .AND. idap == i) flag = 1
      IF (flag == 1) THEN
        IF (a /= " ") THEN
          DO l = kk1, kk2
            IF (rainsb(l,1)<-97) THEN
              CALL pgen(k)
              rainsb(l,1) = subp(k) / nstep
            END IF
            rmeas(l) = rmeas(l) + rainsb(l,1)
          END DO
          DO ii = 2, nstep
            ihour = 0
            imin = 0
            READ (100+k,5200) iyp, idap, ihour, imin,  &
                (rainsb(l,ii), l = kk1, kk2)
            DO l = kk1, kk2
              IF (rainsb(l,1)<-97) THEN
                CALL pgen(k)
                rainsb(l,1) = subp(k) / nstep
              END IF
              rmeas(l) = rmeas(l) + rainsb(l,ii)
            END DO
          END DO
        ELSE
          DO l = kk1, kk2
            rmeas(l) = rainsb(l,1)
            DO ii = 1, nstep
              rainsb(l,ii) = 0.
            END DO
          END DO
        END IF
      END IF
      IF (flag == 1) EXIT
    END DO
  END IF
END DO

!! assign precipitation data to HRUs
!! missing precipitation data cannot be generated for
!! sub-daily simulation
DO k = 1, nhru
  subp(k) = rmeas(irgage(hru_sub(k)))
  DO ii = 1, nstep
    rainsub(k,ii) = rainsb(irgage(hru_sub(k)),ii)
  END DO
!! generate data to replace missing values
  IF (subp(k) < -97.) THEN
!! use same generated data for all HRUs in a subbasin
    IF (hru_sub(k) == inum3sprev .AND. hru_sub(k) /= 0) THEN
      subp(k) = rbsb
      IF (ievent == 1) THEN
        DO l = 1, nstep
          rainsub(k,l) = rstpbsb(l)
        END DO
      END IF
    ELSE
      CALL pgen(k)
!! set subbasin generated values
      inum3sprev = 0
      rbsb = 0.
      inum3sprev = hru_sub(k)
      rbsb = subp(k)
      rstpbsb(:) = 0.
      DO l = 1, nstep
        rstpbsb(l) = rainsub(k,l)
      END DO
    END IF
  END IF
END DO

END select

IF (nstep > 0) THEN
  deallocate (rainsb)
END IF


RETURN
5000 FORMAT (7X,1800F5.1)
5100 FORMAT (i4,i3,1800F5.1)
5200 FORMAT (i4,i3,i2,1X,i2,300F6.2)
5201 FORMAT (i4,i3,5X,300F5.1)
5202 FORMAT (i4,i3,i2,a1,i2,300F6.2)
5300 FORMAT (9X,a1)
5400 FORMAT (10X,"ERROR: Precipitation data dates do not match for",  &
    " simulation year: ",i4," and julian date: ",i3)
END SUBROUTINE pmeas
