SUBROUTINE alph(iwave)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes alpha, a dimensionless parameter that
!!    expresses the fraction of total rainfall that occurs during 0.5h

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    amp_r(:,:)  |none        |alpha factor for rain(mo max 0.5h rain)
!!    idg(:)      |none        |array location of random number seed
!!                             |used for a given process
!!    idt         |minutes     |length of time step used to report
!!                             |precipitation data for sub-daily modeling
!!    ievent      |none        |rainfall/runoff code
!!                             |0 daily rainfall/curve number technique
!!                             |1 daily rainfall/Green&Ampt technique/daily
!!                             |  routing
!!                             |2 sub-daily rainfall/Green&Ampt technique/
!!                             |  daily routing
!!                             |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihru        |none        |HRU number
!!    iwave       |none        |flag to differentiate calculation of HRU and
!!                             |subbasin sediment calculation
!!                             |iwave = 0 for HRU
!!                                      MUSLE(sedyld) each hru is calculated
!!                             |        independently using hru area and
!!                             |        adjusted channel length
!!                             |iwave = 1 subbasin # for subbasin
!!                                      MUSLE is computed for entire subbasin
!!                             |        using hru weighted KLSCP
!!    i_mo        |none        |month being simulated
!!    nstep       |none        |number of lines of rainfall data for each
!!                             |day
!!    ovrlnd(:)   |mm H2O      |overland flow onto HRU from upstream
!!                             |routing unit
!!    precipday   |mm H2O      |amount of water reaching soil surface in HRU
!!    precipdt(:) |mm H2O      |precipitation in time step for HRU
!!    rndseed(:,:)|none        |random number generator seed
!!    snomlt      |mm H2O      |amount of snow melt in HRU on current day
!!    sub_precip(:)|mm H2O      |amount of water reaching soil surface in
!!                             |subbasin
!!    sub_snom(:) |mm H2O      |amount of snow melt in subbasin on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    al5         |none        |fraction of total rainfall on day that occurs
!!                             |during 0.5h highest intensity rainfall
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ab          |mm H2O      |lowest value al5 can have
!!    ajp         |mm H2O      |highest value al5 can have
!!    j           |none        |HRU number
!!    jj          |none        |counter
!!    k           |none        |number of time steps equivalent to 30 minutes
!!    kk          |none        |counter
!!    preceff     |mm H2O      |amount of rainfall on day in HRU
!!    rainsum     |mm H2O      |sum of rainfall during 30 min period
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Expo, Atri

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: iwave

INTEGER :: j, k, kk, jj
REAL :: ab, ajp, preceff, rainsum

j = 0
j = ihru
ab = 0.02083

select case (ievent)
case(0, 1)                !! daily rainfall, estimate al5
preceff = 0.
IF (iwave > 0) THEN
!! subbasin sediment calculations
  IF (sub_precip(iwave) > sub_snom(iwave)) THEN
    preceff = sub_precip(iwave) - sub_snom(iwave)
  ELSE
    preceff = 0.
  END IF
ELSE
!! HRU sediment calculations
  IF (precipday > snomlt) THEN
    preceff = precipday - snomlt
  ELSE
    preceff = 0.
  END IF
  IF (preceff > ovrlnd(j)) THEN
    preceff = preceff - ovrlnd(j)
  ELSE
    preceff = 0.
  END IF
END IF

ajp = 0.
ajp = 1. - expo(-125. / (preceff + 5.))
IF (ised_det == 0) THEN
  al5 = atri(ab, amp_r(i_mo,hru_sub(j)), ajp, rndseed(idg(6),j))
ELSE
  al5 = amp_r(i_mo,hru_sub(j))
END IF

case default            !! subdaily rainfall, get from pcp data
IF (idt <= 30) THEN
  k = 0
  k = 30 / idt
  k = k - 1
  DO kk = 1, nstep+1-k
    rainsum = 0.
    DO jj = 0, k
      IF (precipdt(kk+jj) > (snomlt+ovrlnd(j))/nstep) THEN
        rainsum = rainsum + precipdt(kk+jj) - (snomlt + ovrlnd(j)) / nstep
      END IF
    END DO
    al5 = MAX(al5,rainsum)
  END DO
  IF (subp(j) > 0.01) THEN
    al5 = al5 / subp(j)
    al5 = MIN(al5,.99)
  ELSE
    al5 = ab
  END IF
ELSE
  preceff = 0.
  IF (iwave > 0) THEN
!! subbasin sediment calculations
    IF (sub_precip(iwave) > sub_snom(iwave)) THEN
      preceff = sub_precip(iwave) - sub_snom(iwave)
    ELSE
      preceff = 0.
    END IF
  ELSE
!! HRU sediment calculations
    IF (precipday > snomlt) THEN
      preceff = precipday - snomlt
    ELSE
      preceff = 0.
    END IF
    IF (preceff > ovrlnd(j)) THEN
      preceff = preceff - ovrlnd(j)
    ELSE
      preceff = 0.
    END IF
  END IF
  
  ajp = 0.
  ajp = 1. - expo(-125. / (preceff + 5.))
  IF (ised_det == 0) THEN
    al5 = atri(ab, amp_r(i_mo,hru_sub(j)), ajp, rndseed(idg(6),j))
  ELSE
    al5 = amp_r(i_mo,hru_sub(j))
  END IF
END IF

END select

RETURN
END SUBROUTINE alph
