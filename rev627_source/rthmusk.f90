SUBROUTINE rthmusk
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes flow through a reach using the
!!    Muskingum method at a given time step

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_k(2,:)   |mm/hr         |effective hydraulic conductivity of
!!                               |main channel alluvium
!!    ch_l2(:)    |km            |length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    chside(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    curyr       |none          |current year of simulation (consecutive)
!!    evrch       |none          |Reach evaporation adjustment factor.
!!                               |Evaporation from the reach is multiplied by
!!                               |EVRCH. This variable was created to limit the
!!                               |evaporation predicted in arid regions.
!!    flwin(:)    |m^3 H2O       |flow into reach on previous day
!!    flwout(:)   |m^3 H2O       |flow out of reach on previous day
!!    i           |none          |current day of simulation
!!    id1         |none          |first day of simulation in year
!!    idt         |minutes       |operational time step
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    msk_co1     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at bankfull depth (phi(10,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_co2     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at 0.1 bankfull depth (phi(13,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_x       |none          |weighting factor controlling relative
!!                               |importance of inflow rate and outflow rate
!!                               |in determining storage on reach
!!    pet_day     |mm H2O        |potential evapotranspiration for the day
!!    phi(1,:)    |m^2           |cross-sectional area of flow in channel at
!!                               |bankfull depth
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    phi(6,:)    |m             |bottom width of main channel
!!    phi(10,:)   |hr            |storage time constant for reach at
!!                               |bankfull depth (ratio of storage to
!!                               |discharge)
!!    phi(13,:)   |hr            |storage time constant for reach at
!!                               |0.1 bankfull depth (low flow) (ratio
!!                               |of storage to discharge)
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rnum1       |none          |fraction of overland flow
!!    varoute(2,:)|m^3 H2O       |water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flwin(:)    |m^3 H2O       |flow into reach on current day
!!    flwout(:)   |m^3 H2O       |flow out of reach on current day
!!    hdepth(:)   |m             |depth of flow during time step
!!    hharea(:)   |m^2           |cross-sectional area of flow for time step
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of time step
!!    hhtime(:)   |hr            |flow travel time for time step
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of time step
!!    hrtevp(:)   |m^3 H2O       |evaporation from reach during time step
!!    hrttlc(:)   |m^3 H2O       |transmission losses from reach during time step
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rhy(:)      |m H2O         |main channel hydraulic radius
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    c1          |
!!    c2          |
!!    c3          |
!!    c4          |m^3 H2O       |
!!    det         |hr            |time step
!!    ii          |none          |counter (Number of operational step during day)
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    tbase       |none          |flow duration (fraction of 1 hr)
!!    nstep       |none          |number of steps in a day
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of day
!!    wtrin       |m^3 H2O       |water entering reach on day
!!    xkm         |hr            |storage time constant for the reach on
!!                               |current day
!!    yy          |none          |variable to hold intermediate calculation
!!                               |value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    code provided by Dr. Valentina Krysanova, Pottsdam Institute for
!!    Climate Impact Research, Germany
!! Modified by N.Kannan, Blackland Research Center, Temple, USA

use parm

INTEGER :: jrch, ii
REAL :: xkm, det, yy, c1, c2, c3, c4, wtrin, p, vol, c
REAL :: tbase, topw

jrch = 0
jrch = inum1
 !! C mpute storage time constant for reach
xkm = 0.
xkm = phi(10,jrch) * msk_co1 + phi(13,jrch) * msk_co2

det = idt / 60.
 !! C mpute coefficients
yy = 0.
c1 = 0.
c2 = 0.
c3 = 0.
c4 = 0.
yy = 2. * xkm * (1. - msk_x) + det
c1 = (det - 2. * xkm * msk_x) / yy
c2 = (det + 2. * xkm * msk_x) / yy
c3 = (2. * xkm * (1. - msk_x) - det) / yy
c4 = phi(5,jrch) * ch_l2(jrch) * det / yy

DO ii = 1, nstep   !! begin time step loop
!! Water entering reach on day
  wtrin = 0.
  wtrin = hhvaroute(2,inum2,ii) * (1. - rnum1)
  
!! Compute water leaving reach at the end of time step
  IF (curyr == 1 .AND. i == id1 .AND. ii == 1) THEN
    hrtwtr(ii) = c1 * wtrin + c2 * rchstor(jrch) + c3 * rchstor(jrch) + c4
  ELSE
    hrtwtr(ii) = c1 * wtrin + c2 * flwin(jrch) + c3 * flwout(jrch)
  END IF
  IF (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.
  
  
!! define flow parameters for current time step
  flwin(jrch) = 0.
  flwout(jrch) = 0.
  flwin(jrch) = wtrin
  flwout(jrch) = hrtwtr(ii)
!! calculate volume of water in reach
  vol = 0.
  IF (ii == 1) THEN
    hrchwtr(ii) = rchstor(jrch)
    vol = wtrin + rchstor(jrch)
  ELSE
    hrchwtr = hhstor(ii-1)
    vol = wtrin + hhstor(ii-1)
  END IF
  vol = MAX(vol,1.e-14) ! changed from e-4 to e-14 for urban modeing by J.Jeong 4/21/2008
  
!! calculate cross-sectional area of flow
  hharea(ii) = vol / (ch_l2(jrch) * 1000.)
  
!! calculate depth of flow
  c = 0.
  c = chside(jrch)
  IF (hharea(ii) <= phi(1,jrch)) THEN
    hdepth(ii) = SQRT(hharea(ii) / c + phi(6,jrch) * phi(6,jrch)  &
        / (4. * c * c)) - phi(6,jrch) / (2. * c)
    IF (hdepth(ii) < 0.) hdepth(ii) = 0.
  ELSE
    hdepth(ii) = SQRT((hharea(ii) - phi(1,jrch)) / 4. + 25. *  &
        ch_w(2,jrch) * ch_w(2,jrch) / 64.) - 5. * ch_w(2,jrch) / 8.
    IF (hdepth(ii) < 0.) hdepth(ii) = 0.
    hdepth(ii) = hdepth(ii) + ch_d(jrch)
  END IF
  
!! calculate wetted perimeter
  p = 0.
  IF (hdepth(ii) <= ch_d(jrch)) THEN
    p = phi(6,jrch) + 2. * hdepth(ii) * SQRT(1. + c * c)
  ELSE
    p = phi(6,jrch) + 2. * ch_d(jrch) * SQRT(1. + c * c) +  &
        4. * ch_w(2,jrch) + 2. * (hdepth(ii) - ch_d(jrch)) * SQRT(17.)
  END IF
  
!! calculate hydraulic radius
  rhy(ii) = 0.
  IF (p > 0.01) THEN
    rhy(ii) = hharea(ii) / p
  ELSE
    rhy(ii) = 0.
  END IF
  
!! calculate flow in reach [m3/s]
  hsdti(ii) = qman(hharea(ii), rhy(ii), ch_n(2,jrch),ch_s(2,jrch))
  
!! calculate travel time[hour]
  IF (hsdti(ii) > 1.e-4) THEN
    hhtime(ii) = ch_l2(jrch) * hharea(ii) / (3.6 * hsdti(ii))
    IF (hhtime(ii) < 1.) THEN
      rttime = rttime + hhtime(ii)
    ELSE
      rttime = rttime + 1.
    END IF
  END IF
  
!! calculate transmission losses
!! transmission losses are ignored if ch_k(2,jrch) is set to zero
!! in .rte file
  IF (hhtime(ii) < 1.) THEN
    hrttlc(ii) = ch_k(2,jrch) * ch_l2(jrch) * p * hhtime(ii)
  ELSE
    hrttlc(ii) = ch_k(2,jrch) * ch_l2(jrch) * p
  END IF
  hrttlc(ii) = MIN(hrtwtr(ii),hrttlc(ii))
  hrtwtr(ii) = hrtwtr(ii) - hrttlc(ii)
  rttlc = rttlc + hrttlc(ii)
  
  IF (hrtwtr(ii) > 0.) THEN
!! calculate flow duration
    tbase = 0.
    tbase = hhtime(ii)
    IF (tbase > 1.) tbase = 1.
    
!! calculate width of channel at water level
    topw = 0.
    IF (hdepth(ii) <= ch_d(jrch)) THEN
      topw = phi(6,jrch) + 2. * hdepth(ii) * chside(jrch)
    ELSE
      topw = 5. * ch_w(2,jrch) + 2. * (hdepth(ii)-ch_d(jrch)) * 4.
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
    hhstor(ii) = rchstor(jrch) + wtrin - hrtwtr(ii) - hrtevp(ii) - hrttlc(ii)
  ELSE
    hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii) - hrtevp(ii) - hrttlc(ii)
  END IF
  IF (hhstor(ii) < 0.) THEN
    hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
    hhstor(ii) = 0.
    IF (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
  END IF
  
END DO                   !! end time step loop

!! calculate amount of water in channel at end of day
IF (hhstor(nstep) < 10.) THEN
  hrtwtr(nstep) = hrtwtr(nstep)+hhstor(nstep)
  hhstor(nstep) = 0.
END IF
IF (hrtwtr(nstep) < 0.) hrtwtr(nstep) = 0.

!! daily average values
!! set volume of water in reach at end of day
rchstor(jrch) = hhstor(nstep)
!! calculate total amount of water leaving reach
rtwtr = sum(hrtwtr)
!! calculate average flow cross-sectional area
rcharea = sum(hharea) / nstep
!! calculate average flow depth
rchdep = sum(hdepth) / nstep
!! calculate average flow rate
sdti = sum(hsdti) / nstep

RETURN
END SUBROUTINE rthmusk
