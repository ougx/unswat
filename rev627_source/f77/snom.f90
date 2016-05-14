SUBROUTINE snom
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily snom melt when the average air
!!    temperature exceeds 0 degrees Celcius

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    elevb(:,:)   |m             |elevation at center of band
!!    elevb_fr(:,:)|none          |fraction of subbasin area within elevation
!!                                |band
!!    iida         |julian date   |day being simulated (current julian date)
!!    ihru         |none          |HRU number
!!    pcpband(:,:) |mm H2O        |precipitation for the day in band in HRU
!!    precipday    |mm H2O        |precipitation on the current day in the HRU
!!    sub_sftmp    |deg C         |Snowfall temperature
!!                                |Mean air temperature at which precipitation
!!                                |is equally likely to be rain as snow/freezing
!!                                |rain.
!!    sub_smfmn    |mm/deg C/day  |Minimum melt rate for snow during year (Dec.
!!                                |21) where deg C refers to the air temperature
!!    sub_smfmx    |mm/deg C/day  |Maximum melt rate for snow during year (June
!!                                |21) where deg C refers to the air temperature
!!                                |SMFMX and SMFMN allow the rate of snow melt
!!                                |to vary through the year. These parameters
!!                                |are accounting for the impact of soil
!!                                |temperature on snow melt.
!!    sub_smtmp    |deg C         |Snow melt base temperature
!!                                |Mean air temperature at which snow melt will
!!                                |occur.
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    snocov1      |none          |1st shape parameter for snow cover equation
!!                                |This parameter is determined by solving the
!!                                |equation for 50% snow cover
!!    snocov2      |none          |2nd shape parameter for snow cover equation
!!                                |This parameter is determined by solving the
!!                                |equation for 95% snow cover
!!    snocovmx     |mm H2O        |Minimum snow water content that corresponds
!!                                |to 100% snow cover. If the snow water content
!!                                |is less than SNOCOVMX, then a certain
!!                                |percentage of the ground will be bare.
!!    snoeb(:,:)   |mm H2O        |snow water content in elevation band on
!!                                |current day
!!    snotmp(:)    |deg C         |temperature of snow pack in HRU
!!    snotmpeb(:,:)|deg C         |temperature of snow pack in elevation band
!!    tavband(:,:) |deg C         |average temperature for the day in band in HRU
!!    sub_timp     |none          |Snow pack temperature lag factor (0-1)
!!                                |1 = no lag (snow pack temp=current day air
!!                                |temp) as the lag factor goes to zero, the
!!                                |snow pack's temperature will be less
!!                                |influenced by the current day's air
!!                                |temperature
!!    tmpav(:)     |deg C         |average air temperature on current day for
!!                                |HRU
!!    tmx(:)       |deg C         |maximum air temperature on current day for
!!                                |HRU
!!    tmxband(:,:) |deg C         |maximum temperature for the day in band in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    precipday    |mm H2O        |amount of water in effective precipitation
!!                                |in HRU
!!    precipdt(:)  |mm H2O        |precipitation for the time step during day
!!    sno_hru(:)   |mm H2O        |amount of water in snow in HRU on current day
!!    snoeb(:,:)   |mm H2O        |snow water content in elevation band on
!!                                |current day
!!    snofall      |mm H2O        |amount of precipitation falling as freezing
!!                                |rain/snow on day in HRU
!!    snomlt       |mm H2O        |amount of water in snow melt for the day in
!!                                |HRU
!!    snotmp(:)    |deg C         |temperature of snow pack in HRU
!!    snotmpeb(:,:)|deg C         |temperature of snow pack in elevation band
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ib          |none          |counter
!!    j           |none          |HRU number
!!    smfac       |
!!    smleb       |mm H2O        |amount of snow melt in elevation band on
!!                               |current day
!!    smp         |mm H2O        |precipitation on current day for HRU
!!    snocov      |none          |fraction of HRU area covered with snow
!!    sum         |mm H2O        |snow water content in HRU on current day
!!    xx          |none          |ratio of amount of current day's snow water
!!                               |content to the minimum amount needed to
!!                               |cover ground completely
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Sin, Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

INTEGER :: j, ib
REAL :: sum, smp, smfac, smleb
REAL :: xx, snocov

j = 0
j = ihru
sum =0.
smp =0.
isub = hru_sub(j)

IF (elevb(1,isub) > 0. .AND. elevb_fr(1,isub) > 0.) THEN
!! elevation bands
!! compute snow fall and melt for each elevation band
  DO ib = 1, 10
    IF (elevb_fr(ib,isub) < 0.) EXIT
    snotmpeb(ib,j) = snotmpeb(ib,j) * (1.-sub_timp(ib,isub)) +  &
        tavband(ib,j) * sub_timp(ib,isub)
    
    IF (tavband(ib,j) < sub_sftmp(ib,isub)) THEN
      
!! compute snow fall if temperature is below sftmp
      snoeb(ib,j) = snoeb(ib,j) + pcpband(ib,j)
      snofall = snofall + pcpband(ib,j) * elevb_fr(ib,isub)
      
    ELSE
      
!! compute snow melt if temperature is above smtmp
      IF (tmxband(ib,j) > sub_smtmp(ib,isub)) THEN
        smfac = 0.
        smleb = 0.
        smfac = (sub_smfmx(ib,isub) + sub_smfmn(ib,isub)) / 2. +  &
            SIN((iida - 81) / 58.09) *  &
            (sub_smfmx(ib,isub) - sub_smfmn(ib,isub)) / 2.    !! 365/2pi = 58.09
        smleb = smfac * (((snotmpeb(ib,j) + tmxband(ib,j)) / 2.)  &
            - sub_smtmp(ib,isub))
        
!! adjust for areal extent of snow cover
        IF (snoeb(ib,j) < snocovmx) THEN
          xx = 0.
          snocov = 0.
          xx = snoeb(ib,j) / snocovmx
          snocov = xx / (xx + EXP(snocov1 - snocov2 * xx))
        ELSE
          snocov = 1.
        END IF
        smleb = smleb * snocov
        IF (smleb < 0.) smleb = 0.
        IF (smleb > snoeb(ib,j)) smleb = snoeb(ib,j)
        snoeb(ib,j) = snoeb(ib,j) - smleb
        snomlt = snomlt + smleb * elevb_fr(ib,isub)
      END IF
    END IF
    sum = sum + snoeb(ib,j) * elevb_fr(ib,isub)
    smp = smp + pcpband(ib,j) * elevb_fr(ib,isub)
    
  END DO
  
  
  
!! add/sub aggregate snow fall and melt from effective precip
!! and snow cover
  precipday = smp + snomlt - snofall
  IF (precipday < 0.) precipday = 0.
  IF (nstep > 0) THEN
    DO ii = 1, nstep
      precipdt(ii+1) = precipdt(ii+1) + (snomlt - snofall) / nstep
      IF (precipdt(ii+1) < 0.) precipdt(ii+1) = 0.
    END DO
  END IF
  sno_hru(j) = sum
  
ELSE
!! no elevation bands
  
  ib = 1
  
!! estimate snow pack temperature
  snotmp(j)=snotmp(j) * (1. - sub_timp(ib,isub)) + tmpav(j) *  &
      sub_timp(ib,isub)
  
  IF (tmpav(j) <= sub_sftmp(ib,isub)) THEN
!! calculate snow fall
    sno_hru(j) = sno_hru(j) + precipday
    snofall = precipday
    precipday = 0.
    precipdt = 0.
  END IF
  
  IF (tmx(j) > sub_smtmp(ib,isub) .AND. sno_hru(j) > 0.) THEN
!! adjust melt factor for time of year
    smfac = 0.
    snomlt = 0.
    smfac = (sub_smfmx(ib,isub) + sub_smfmn(ib,isub)) / 2. +  &
        SIN((iida - 81) / 58.09) *  &
        (sub_smfmx(ib,isub) - sub_smfmn(ib,isub)) / 2.    !! 365/2pi = 58.09
    snomlt = smfac * (((snotmp(j)+tmx(j))/2.)-sub_smtmp(ib,isub))
    
!! adjust for areal extent of snow cover
    IF (sno_hru(j) < snocovmx) THEN
      xx = 0.
      xx = sno_hru(j) / snocovmx
      snocov = xx / (xx + EXP(snocov1 - snocov2 * xx))
    ELSE
      snocov = 1.
    END IF
    snomlt = snomlt * snocov
    IF (snomlt < 0.) snomlt = 0.
    IF (snomlt > sno_hru(j)) snomlt = sno_hru(j)
    sno_hru(j) = sno_hru(j) - snomlt
    precipday = precipday + snomlt
    IF (nstep > 0) THEN
      DO ii = 1, nstep
        precipdt(ii+1) = precipdt(ii+1) + snomlt / nstep
      END DO
    END IF
    IF (precipday < 0.) precipday = 0.
  ELSE
    snomlt = 0.
  END IF
END IF
RETURN
END SUBROUTINE snom
