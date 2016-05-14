SUBROUTINE rtdt
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes flow at any required time step through the reach
!!    using a constant storage coefficient
!! Routing method: Variable Storage routing

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)         |m             |average depth of main channel
!!    ch_k(2,:)       |mm/hr         |effective hydraulic conductivity of
!!                                   |main channel alluvium
!!    ch_l2(:)        |km            |length of main channel
!!    ch_n(2,:)       |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)       |m/m           |average slope of main channel
!!    ch_w(2,:)       |m             |average width of main channel
!!    chside(:)       |none          |change in horizontal distance per unit
!!                                   |change in vertical distance on channel
!!                                   |side slopes; always set to 2 (slope=1/2)
!!    evrch           |none          |Reach evaporation adjustment factor.
!!                                   |Evaporation from the reach is multiplied
!!                                   |by EVRCH. This variable was created to
!!                                   |limit the evaporation predicted in arid
!!                                   |regions.
!!    hhvaroute(2,:,:)|m^3 H2O       |water
!!    idt             |min           |model operational time step
!!    inum1           |none          |reach number
!!    inum2           |none          |inflow hydrograph storage location number
!!    pet_day         |mm H2O        |potential evapotranspiration
!!    phi(1,:)        |m^2           |cross-sectional area of flow in channel at
!!                                   |bankfull depth
!!    phi(6,:)        |m             |bottom width of main channel
!!    rchstor(:)      |m^3 H2O       |water stored in reach
!!    rnum1           |none          |fraction of overland flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hdepth(:)   |m             |depth of flow during time step
!!    hharea(:)   |m^2           |cross-sectional area of flow
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of time step
!!    hhtime(:)   |hr            |flow travel time for time step
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of time step
!!    hrtevp(:)   |m^3 H2O       |evaporation losses for hour
!!    hrttlc(:)    |m^3 H2O       |transmission losses for hour
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rhy(:)          |m H2O         |main channel hydraulic radius
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    scoef       |none          |storage coefficient
!!    nstep       |none          |No. of steps in a day (depends on model operational time step)
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!! Modified by N.Kannan, Blackland Research, Temple, USA

use parm

INTEGER :: jrch, ii, inhyd
REAL :: wtrin, c, p, scoef
REAL :: vol, topw


jrch = 0
jrch = inum1

inhyd = 0
inhyd = inum2

! nstep = int(1440 / idt) !! nstep is a global variable

!!     start of sub-daily loop

DO ii = 1, nstep
  
!! water entering reach during time step
  
  wtrin = 0.
  wtrin = hhvaroute(2,inhyd,ii) * (1. - rnum1)
  
!! calculate volume of water in reach
  
  vol = 0.
  IF (ii == 1) THEN
    hrchwtr(ii) = rchstor(jrch)
    vol = wtrin + rchstor(jrch)
  ELSE
    hrchwtr(ii) = hhstor(ii-1)
    vol = wtrin + hhstor(ii-1)
  END IF
  vol = MAX(vol,1.e-14) ! changed from e-4 to e-14 for urban modeing by J.Jeong 4/21/2008
  
!! calculate cross-sectional area of flow
  
  hharea(ii) = vol / (ch_l2(jrch) * 1000.)
  
!! calculate depth of flow
  
  c = 0.
  c = chside(jrch)
  IF (hharea(ii) <= phi(1,jrch)) THEN
    hdepth(ii) = SQRT(hharea(ii) / c + phi(6,jrch) * phi(6,jrch) /  &
        (4. * c * c)) - phi(6,jrch) / (2. * c)
    IF (hdepth(ii) < 0.) hdepth(ii) = 0.
  ELSE
    hdepth(ii) = SQRT((hharea(ii) - phi(1,jrch)) / 4. + 25. *  &
        ch_w(2,jrch) * ch_w(2,jrch) / 64.) - 5.* ch_w(2,jrch) / 8.
    IF (hdepth(ii) < 0.) hdepth(ii) = 0.
    hdepth(ii) = hdepth(ii) + ch_d(jrch)
  END IF
  
!! calculate wetted perimeter
  
  p = 0.
  IF (hdepth(ii) <= ch_d(jrch)) THEN
    p = phi(6,jrch) + 2. * hdepth(ii) * SQRT(1. + c * c)
  ELSE
    p = phi(6,jrch) + 2. * ch_d(jrch) * SQRT(1. + c * c) + 4. *  &
        ch_w(2,jrch) + 2. * (hdepth(ii) - ch_d(jrch)) * SQRT(17.)
  END IF
  
!! calculate hydraulic radius
  
  rhy(ii) = 0.
  IF (p > 0.01) THEN
    rhy(ii) = hharea(ii) / p
  ELSE
    rhy(ii) = 0.
  END IF
  
!! calculate rate of flow in reach
  
  hsdti(ii) = qman(hharea(ii),rhy(ii),ch_n(2,jrch),ch_s(2,jrch))
  
  IF (hsdti(ii) > 0.) THEN
    
!! calculate travel time
    
    hhtime(ii) = ch_l2(jrch) * hharea(ii) / (3.6 * hsdti(ii))
    IF (hhtime(ii) < 1.) THEN
      rttime = rttime + hhtime(ii)
    ELSE
      rttime = rttime + 1.
    END IF
    
!! calculate volume of water leaving reach on day
    
    scoef = 0.
    scoef = 2. / (2. * hhtime(ii) + 1.)
    IF (scoef > 1.) scoef = 1.
    hrtwtr(ii) = scoef * vol
    IF (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.
    
    
!! calculate transmission losses
    
    hrttlc = 0.
    IF (hhtime(ii) < 1.) THEN
      hrttlc(ii) = ch_k(2,jrch) * ch_l2(jrch) * p * hhtime(ii)
    ELSE
      hrttlc(ii) = ch_k(2,jrch) * ch_l2(jrch) * p
    END IF
    hrttlc(ii) = MIN(hrtwtr(ii),hrttlc(ii))
    hrtwtr(ii) = hrtwtr(ii) - hrttlc(ii)
    rttlc = rttlc + hrttlc(ii)
    
    hrtevp = 0.
    IF (hrtwtr(ii) > 0.) THEN
      
!! calculate width of channel at water level
      
      topw = 0.
      IF (hdepth(ii) <= ch_d(jrch)) THEN
        topw = phi(6,jrch) + 2. * hdepth(ii) * chside(jrch)
      ELSE
        topw = 5. * ch_w(2,jrch) + 8. * (hdepth(ii) - ch_d(jrch))
      END IF
      
!! calculate evaporation
      
      IF (hhtime(ii) < 1.) THEN
        hrtevp(ii) = evrch * pet_day/nstep * ch_l2(jrch) * topw * hhtime(ii)
      ELSE
        hrtevp(ii) = evrch * pet_day/nstep * ch_l2(jrch) * topw
      END IF
      IF (hrtevp(ii) < 0.) hrtevp(ii) = 0.
      hrtevp(ii) = MIN(hrtwtr(ii),hrtevp(ii))
      hrtwtr(ii) = hrtwtr(ii) - hrtevp(ii)
      rtevp = rtevp + hrtevp(ii)
    END IF
    
!! set volume of water in channel at end of hour
    
    IF (ii == 1) THEN
      hhstor(ii) = rchstor(jrch) + wtrin - hrtwtr(ii) -  &
          hrtevp(ii) - hrttlc(ii)
    ELSE
      hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii) - hrtevp(ii) - hrttlc(ii)
    END IF
    IF (hhstor(ii) < 0.) THEN
      hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
      hhstor(ii) = 0.
      IF (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.
    END IF
  END IF
  
END DO                     !! end of sub-daily loop

!! calculate amount of water in channel at end of day

!      if (hhstor(nstep) < 0.1.and.hrtwtr(ii-1)>0.) then
!        hrtwtr(nstep) = hrtwtr(nstep) + hhstor(nstep)
!        hhstor(nstep) = 0.
!      end if
IF (hrtwtr(nstep) < 0.) hrtwtr(nstep) = 0.

!! calculation of daily average values

!! set volume of water in reach at end of day
rchstor(jrch) = hhstor(nstep)
!! calculate total amount of water leaving reach
rtwtr = sum(hrtwtr)
!! calculate average flow area
rcharea = sum (hharea) / nstep
!! calculate average flow depth
rchdep = sum(hdepth) / nstep
!! calculate average flow rate
sdti = sum(hsdti) / nstep

RETURN
END SUBROUTINE rtdt
