SUBROUTINE rtpest
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine computes the daily stream pesticide balance
!!     (soluble and sorbed)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_l2(:)      |km            |length of main channel
!!    ch_w(2,:)     |m             |average width of main channel
!!    chpst_conc(:) |mg/(m**3)     |initial pesticide concentration in reach
!!    chpst_koc(:)  |m**3/g        |pesticide partition coefficient between
!!                                 |water and sediment in reach
!!    chpst_mix(:)  |m/day         |mixing velocity (diffusion/dispersion) for
!!                                 |pesticide in reach
!!    chpst_rea(:)  |1/day         |pesticide reaction coefficient in reach
!!    chpst_rsp(:)  |m/day         |resuspension velocity in reach for pesticide
!!                                 |sorbed to sediment
!!    chpst_stl(:)  |m/day         |settling velocity in reach for pesticide
!!                                 |sorbed to sediment
!!    chpst_vol(:)  |m/day         |pesticide volatilization coefficient in
!!                                 |reach
!!    drift(:)      |kg            |amount of pesticide drifting onto main
!!                                 |channel in subbasin
!!    hru_sub(:)    |none          |subbasin number where reach is located
!!    inum1         |none          |reach number
!!    inum2         |none          |inflow hydrograph storage location number
!!    rchdep        |m             |depth of flow on day
!!    rchwtr        |m^3 H2O       |water stored in reach at beginning of day
!!    rnum1         |none          |fraction of overland flow
!!    rtwtr         |m^3 H2O       |water leaving reach on day
!!    sedpst_act(:) |m             |depth of active sediment layer in reach for
!!                                 |pesticide
!!    sedpst_bry(:) |m/day         |pesticide burial velocity in river bed
!!                                 |sediment
!!    sedpst_conc(:)|mg/(m**3)     |inital pesticide concentration in river bed
!!                                 |sediment
!!    sedpst_rea(:) |1/day         |pesticide reaction coefficient in river bed
!!                                 |sediment
!!    varoute(11,:) |mg pst        |pesticide in solution
!!    varoute(12,:) |mg pst        |pesticide sorbed to sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    difus       |mg pst        |diffusion of pesticide from sediment to reach
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    reactw      |mg pst        |amount of pesticide in reach that is lost
!!                               |through reactions
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |reach due to resuspension
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    solpesto    |mg pst/m^3    |soluble pesticide concentration in outflow
!!                               |on day
!!    sorpesto    |mg pst/m^3    |sorbed pesticide concentration in outflow
!!                               |on day
!!    volatpst    |mg pst        |amount of pesticide in reach lost by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bedvol      |m^3           |volume of river bed sediment
!!    chpstmass   |mg pst        |mass of pesticide in reach
!!    depth       |m             |depth of water in reach
!!    fd2         |
!!    frsol       |none          |fraction of pesticide in reach that is soluble
!!    frsrb       |none          |fraction of pesticide in reach that is sorbed
!!    jrch        |none          |reach number
!!    pstin       |mg pst        |total pesticide transported into reach
!!                               |during time step
!!    sedcon      |g/m^3         |sediment concentration
!!    sedpstmass  |mg pst        |mass of pesticide in bed sediment
!!    solpstin    |mg pst        |soluble pesticide entering reach during
!!                               |time step
!!    sorpstin    |mg pst        |sorbed pesticide entering reach during
!!                               |time step
!!    tday        |days          |flow duration
!!    wtrin       |m^3 H2O       |volume of water entering reach during time
!!                               |step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: jrch
REAL :: solpstin, sorpstin, pstin, depth, chpstmass, frsol, frsrb
REAL :: sedpstmass, bedvol, fd2, wtrin, solmax, sedcon, tday

jrch = 0
jrch = inum1

!! initialize depth of water for pesticide calculations
depth = 0.
IF (rchdep < 0.1) THEN
  depth = .1
ELSE
  depth = rchdep
END IF

!! calculate volume of active river bed sediment layer
bedvol = 0.
bedvol = ch_w(2,jrch) * ch_l2(jrch) * 1000. * sedpst_act(jrch)

!! calculate volume of water entering reach
wtrin = 0.
wtrin = varoute(2,inum2) * (1. - rnum1)

!! pesticide transported into reach during day
solpstin = 0.
sorpstin = 0.
pstin = 0.
solpstin = varoute(11,inum2) * (1. - rnum1)
sorpstin = varoute(12,inum2) * (1. - rnum1)
pstin = solpstin + sorpstin

!! add pesticide drifting from HRUs in subbasin to reach
!      if (rtwtr > 0.) then
!        pstin = pstin + (drift(jrch) * 1.e6)
!      else
!        sedpst_conc(jrch) = sedpst_conc(jrch) + drift(jrch) * 1.e6 /    &
!     &                                                            bedvol
!      endif

!! calculate mass of pesticide in reach
chpstmass = 0.
chpstmass = pstin + chpst_conc(jrch) * rchwtr

!! calculate mass of pesticide in bed sediment
sedpstmass = 0.
sedpstmass = sedpst_conc(jrch) * bedvol

IF (chpstmass + sedpstmass < 1.e-6) THEN
  chpst_conc(jrch) = 0.
  sedpst_conc(jrch) = 0.
END IF
IF (chpstmass + sedpstmass < 1.e-6) RETURN

!!in-stream processes
IF (rtwtr / 86400. > 0.002) THEN
!! calculated sediment concentration
  sedcon = 0.
  sedcon = sedrch / rtwtr * 1.e6
  
!! calculate fraction of soluble and sorbed pesticide
  frsol = 0.
  frsrb = 0.
  IF (solpstin + sorpstin > 1.e-6) THEN
    IF (chpst_koc(jrch) > 0.) THEN
      frsol = 1. / (1. + chpst_koc(jrch)* sedcon)
    ELSE
      frsol = solpstin / (solpstin + sorpstin)
    END IF
    frsrb = 1. - frsol
  ELSE
!!drifting pesticide is only pesticide entering
!!and none is sorbed
    frsol = 1.
    frsrb = 0.
  END IF
  
!! ASSUME POR=0.5; DENSITY=2.6E6; KD2=KD1
  fd2 = 1. / (.5 + chpst_koc(jrch))
  
!! calculate flow duration
  tday = 0.
  tday = rttime / 24.0
  IF (tday > 1.0) tday = 1.0
  tday = 1.0
  
!! calculate amount of pesticide that undergoes chemical or
!! biological degradation on day in reach
!! MFW, 3/12/12: modify decay to be 1st order
!! reactw = chpst_rea(jrch) * chpstmass * tday
  reactw = chpstmass - (chpstmass * EXP(-1. * chpst_rea(jrch) * tday))
  chpstmass = chpstmass - reactw
  
!! calculate amount of pesticide that volatilizes from reach
  volatpst = chpst_vol(jrch) * frsol * chpstmass * tday / depth
  IF (volatpst > frsol * chpstmass) THEN
    volatpst = frsol * chpstmass
    chpstmass = chpstmass - volatpst
  ELSE
    chpstmass = chpstmass - volatpst
  END IF
  
!! calculate amount of pesticide removed from reach by
!! settling
  setlpst = chpst_stl(jrch) * frsrb * chpstmass * tday / depth
  IF (setlpst >  frsrb * chpstmass) THEN
    setlpst = frsrb * chpstmass
    chpstmass = chpstmass - setlpst
  ELSE
    chpstmass = chpstmass - setlpst
  END IF
  sedpstmass = sedpstmass + setlpst
  
!! calculate resuspension of pesticide in reach
  resuspst = chpst_rsp(jrch) * sedpstmass * tday / depth
  IF (resuspst > sedpstmass) THEN
    resuspst = sedpstmass
    sedpstmass = 0.
  ELSE
    sedpstmass = sedpstmass - resuspst
  END IF
  chpstmass = chpstmass + resuspst
  
!! calculate diffusion of pesticide between reach and sediment
  difus = chpst_mix(jrch) * (fd2 * sedpstmass - frsol *  &
      chpstmass) * tday / depth
  IF (difus > 0.) THEN
    IF (difus > sedpstmass) THEN
      difus = sedpstmass
      sedpstmass = 0.
    ELSE
      sedpstmass = sedpstmass - ABS(difus)
    END IF
    chpstmass = chpstmass + ABS(difus)
  ELSE
    IF (ABS(difus) > chpstmass) THEN
      difus = -chpstmass
      chpstmass = 0.
    ELSE
      chpstmass = chpstmass - ABS(difus)
    END IF
    sedpstmass = sedpstmass + ABS(difus)
  END IF
  
!! calculate removal of pesticide from active sediment layer
!! by burial
  bury = sedpst_bry(jrch) * sedpstmass / sedpst_act(jrch)
  IF (bury > sedpstmass) THEN
    bury = sedpstmass
    sedpstmass = 0.
  ELSE
    sedpstmass = sedpstmass - bury
  END IF
  
!! verify that water concentration is at or below solubility
  solmax = 0.
  solmax = pest_sol * (rchwtr + wtrin)
  IF (solmax < chpstmass * frsol) THEN
    sedpstmass = sedpstmass + (chpstmass * frsol - solmax)
    chpstmass = chpstmass - (chpstmass * frsol - solmax)
  END IF
  
ELSE
!!insignificant flow
  sedpstmass = sedpstmass + chpstmass
  chpstmass = 0.
END IF

!! sediment processes
!! calculate loss of pesticide from bed sediments by reaction
reactb = sedpst_rea(jrch) * sedpstmass
IF (reactb > sedpstmass) THEN
  reactb = sedpstmass
  sedpstmass = 0.
ELSE
  sedpstmass = sedpstmass - reactb
END IF

!! calculate pesticide concentrations at end of day
chpst_conc(jrch) = 0.
sedpst_conc(jrch) = 0.
IF (rchwtr + wtrin > 1.e-6) THEN
  chpst_conc(jrch) = chpstmass / (rchwtr + wtrin)
ELSE
  sedpstmass = sedpstmass + chpstmass
END IF
sedpst_conc(jrch) = sedpstmass / bedvol

!! calculate amount of pesticide transported out of reach
IF (rtwtr / 86400. > 0.002) THEN             !Claire, corrected to match line 151
  solpesto = chpst_conc(jrch) * frsol
  sorpesto = chpst_conc(jrch) * frsrb
ELSE
  solpesto = 0.
  sorpesto = 0.
END IF

RETURN
END SUBROUTINE rtpest
