SUBROUTINE rtmusk
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes a daily flow through a reach using the
!!    Muskingum method

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
!!    pet_day     |mm H2O        |potential evapotranspiration
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
!!    rnum1       |none          |fraction of overland flow
!!    rchstor(:)   |m^3 H2O       |water stored in reach
!!    varoute(2,:)|m^3 H2O       |water flowing into reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flwin(:)    |m^3 H2O       |flow into reach on current day
!!    flwout(:)   |m^3 H2O       |flow out of reach on current day
!!    rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    rchstor(:)   |m^3 H2O       |water stored in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    c1          |
!!    c2          |
!!    c3          |
!!    c4          |m^3 H2O       |
!!    det         |hr            |time step (24 hours)
!!    jrch        |none          |reach number
!!    nn          |              |number of subdaily computation points for stable
!!                               |routing in the muskingum routing method
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    tbase       |none          |flow duration (fraction of 24 hr)
!!    topw        |m             |top width of main channel
!!    vol         |m^3 H2O       |volume of water in reach at beginning of
!!                               |day
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
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University

use parm

INTEGER :: jrch,nn,ii
REAL :: xkm, det, yy, c1, c2, c3, c4, wtrin, p, vol, c, rh
REAL :: topw,msk1,msk2,detmax,detmin,qinday,qoutday
REAL :: volrt, maxrt, adddep, addp, addarea
REAL :: rttlc1, rttlc2, rtevp1, rtevp2

jrch = 0
jrch = inum1
qinday = 0; qoutday = 0

det = 24.


!! Water entering reach on day
wtrin = 0.
wtrin = varoute(2,inum2) * (1. - rnum1)

!! Compute storage time constant for reach (msk_co1 + msk_co2 = 1.)
msk1 = msk_co1 / (msk_co1 + msk_co2)
msk2 = msk_co2 / (msk_co1 + msk_co2)
msk_co1 = msk1
msk_co2 = msk2
xkm = 0.
xkm = phi(10,jrch) * msk_co1 + phi(13,jrch) * msk_co2

!! Muskingum numerical stability -Jaehak Jeong, 2011
!! Check numerical stability
detmax = 2.* xkm * (1.- msk_x)
detmin = 2.* xkm * msk_x

!! Discretize time interval to meet the stability criterion
IF (det>detmax) THEN
  IF (det/2.<=detmax) THEN
    det = 12; nn = 2
  ELSE IF (det/4.<=detmax) THEN
    det = 6; nn = 4
  ELSE
    det = 1; nn = 24
  END IF
ELSE
  det = 24; nn = 1
END IF
 !! I flow during a sub time interval
wtrin = wtrin / nn

!! Iterate for the day
DO ii=1,nn
   !! c lculate volume of water in reach
  vol = 0.
  vol = wtrin + rchstor(jrch)
  
!! Find average flowrate in a sub time interval
  volrt = vol / (86400. / nn)
  
!! Find maximum flow capacity of the channel at bank full
  c = 0.
  c = chside(jrch)
  p = phi(6,jrch) + 2. * ch_d(jrch) * SQRT(1. + c * c)
  rh = phi(1,jrch) / p
  maxrt = qman(phi(1,jrch), rh, ch_n(2,jrch), ch_s(2,jrch))
  
  sdti = 0.
  rchdep = 0.
  p = 0.
  rh = 0.
  vc = 0.
  
!! If average flowrate is greater than than the channel capacity at bank full
!! then simulate flood plain flow else simulate the regular channel flow
  IF (volrt > maxrt) THEN
    rcharea = phi(1,jrch)
    rchdep = ch_d(jrch)
    p = phi(6,jrch) + 2. * ch_d(jrch) * SQRT(1. + c * c)
    rh = phi(1,jrch) / p
    sdti = maxrt
    adddep = 0
!! find the crossectional area and depth for volrt
!! by iteration method at 1cm interval depth
!! find the depth until the discharge rate is equal to volrt
    DO WHILE (sdti < volrt)
      adddep = adddep + 0.01
      addarea = rcharea + ((ch_w(2,jrch) * 5) + 4 * adddep) * adddep
      addp = p + (ch_w(2,jrch) * 4) + 2. * adddep * SQRT(1. + 4 * 4)
      rh = addarea / addp
      sdti = qman(addarea, rh, ch_n(2,jrch), ch_s(2,jrch))
    END DO
    rcharea = addarea
    rchdep = ch_d(jrch) + adddep
    p = addp
    sdti = volrt
  ELSE
!! find the crossectional area and depth for volrt
!! by iteration method at 1cm interval depth
!! find the depth until the discharge rate is equal to volrt
    DO WHILE (sdti < volrt)
      rchdep = rchdep + 0.01
      rcharea = (phi(6,jrch) + c * rchdep) * rchdep
      p = phi(6,jrch) + 2. * rchdep * SQRT(1. + c * c)
      rh = rcharea / p
      sdti = qman(rcharea, rh, ch_n(2,jrch), ch_s(2,jrch))
    END DO
    sdti = volrt
  END IF
  
!! calculate top width of channel at water level
  topw = 0.
  IF (rchdep <= ch_d(jrch)) THEN
    topw = phi(6,jrch) + 2. * rchdep * c
  ELSE
    topw = 5 * ch_w(2,jrch) + 2. * (rchdep - ch_d(jrch)) * 4.
  END IF
  
  IF (sdti > 0) THEN
    
!! calculate velocity and travel time
    vc = sdti / rcharea
    vel_chan(jrch) = vc
    rttime = ch_l2(jrch) * 1000. / (3600. * vc)
    
!! Compute coefficients
    yy = 0.
    c1 = 0.
    c2 = 0.
    c3 = 0.
    c4 = 0.
    yy = 2. * xkm * (1. - msk_x) + det
    c1 = (det - 2. * xkm * msk_x) / yy
    c2 = (det + 2. * xkm * msk_x) / yy
    c3 = (2. * xkm * (1. - msk_x) - det) / yy
    
!! Compute water leaving reach on day
    IF (curyr == 1 .AND. i == id1) THEN
      flwin(jrch) = rchstor(jrch)
      flwout(jrch) = rchstor(jrch)
    END IF
    
    rtwtr = c1 * wtrin + c2 * flwin(jrch) + c3 * flwout(jrch)
    IF (rtwtr < 0.) rtwtr = 0.
    
    rtwtr = MIN(rtwtr, (wtrin + rchstor(jrch)))
    
!! calculate amount of water in channel at end of day
    rchstor(jrch) = rchstor(jrch) + wtrin - rtwtr
!! Add if statement to keep rchstor from becoming negative
    IF (rchstor(jrch) < 0.0) rchstor(jrch) = 0.0
    
!! transmission and evaporation losses are proportionally taken from the
!! channel storage and from volume flowing out
    
!! calculate transmission losses
    rttlc = 0.
    
    IF (rtwtr > 0.) THEN
      
!!  Total time in hours to clear the water
      
      rttlc = det * ch_k(2,jrch) * ch_l2(jrch) * p
      rttlc2 = rttlc * rchstor(jrch) / (rtwtr + rchstor(jrch))
      
      IF (rchstor(jrch) <= rttlc2) THEN
        rttlc2 = MIN(rttlc2, rchstor(jrch))
        rchstor(jrch) = rchstor(jrch) - rttlc2
        rttlc1 = rttlc - rttlc2
        IF (rtwtr <= rttlc1) THEN
          rttlc1 = MIN(rttlc1, rtwtr)
          rtwtr = rtwtr - rttlc1
        ELSE
          rtwtr = rtwtr - rttlc1
        END IF
      ELSE
        rchstor(jrch) = rchstor(jrch) - rttlc2
        rttlc1 = rttlc - rttlc2
        IF (rtwtr <= rttlc1) THEN
          rttlc1 = MIN(rttlc1, rtwtr)
          rtwtr = rtwtr - rttlc1
        ELSE
          rtwtr = rtwtr - rttlc1
        END IF
      END IF
      rttlc = rttlc1 + rttlc2
    END IF
    
    
!! calculate evaporation
    rtevp = 0.
    IF (rtwtr > 0.) THEN
      
      aaa = evrch * pet_day / 1000.
      
      IF (rchdep <= ch_d(jrch)) THEN
        rtevp = aaa * ch_l2(jrch) * 1000. * topw
      ELSE
        IF (aaa <=  (rchdep - ch_d(jrch))) THEN
          rtevp = aaa * ch_l2(jrch) * 1000. * topw
        ELSE
          rtevp = (rchdep - ch_d(jrch))
          rtevp = rtevp + (aaa - (rchdep - ch_d(jrch)))
          topw = phi(6,jrch) + 2. * ch_d(jrch) * c
          rtevp = rtevp * ch_l2(jrch) * 1000. * topw
        END IF
      END IF
      
      rtevp2 = rtevp * rchstor(jrch) / (rtwtr + rchstor(jrch))
      
      IF (rchstor(jrch) <= rtevp2) THEN
        rtevp2 = MIN(rtevp2, rchstor(jrch))
        rchstor(jrch) = rchstor(jrch) - rtevp2
        rtevp1 = rtevp - rtevp2
        IF (rtwtr <= rtevp1) THEN
          rtevp1 = MIN(rtevp1, rtwtr)
          rtwtr = rtwtr - rtevp1
        ELSE
          rtwtr = rtwtr - rtevp1
        END IF
      ELSE
        rchstor(jrch) = rchstor(jrch) - rtevp2
        rtevp1 = rtevp - rtevp2
        IF (rtwtr <= rtevp1) THEN
          rtevp1 = MIN(rtevp1, rtwtr)
          rtwtr = rtwtr - rtevp1
        ELSE
          rtwtr = rtwtr - rtevp1
        END IF
      END IF
      rtevp = rtevp1 + rtevp2
    END IF
    
!! define flow parameters for current iteration
    flwin(jrch) = 0.
    flwout(jrch) = 0.
    flwin(jrch) = wtrin
    flwout(jrch) = rtwtr
    
!! define flow parameters for current day
    qinday = qinday + wtrin
    qoutday = qoutday + rtwtr
    
    
!! total outflow for the day
    rtwtr = qoutday
    
  ELSE
    rtwtr = 0.
    sdti = 0.
    rchstor(jrch) = 0.
    vel_chan(jrch) = 0.
    flwin(jrch) = 0.
    flwout(jrch) = 0.
  END IF
  
END DO

!! precipitation on reach is not calculated because area of HRUs
!! in subbasin sums up to entire subbasin area (including channel
!! area) so precipitation is accounted for in subbasin loop

!!      volinprev(jrch) = wtrin
!! qoutprev(jrch) = rtwtr

IF (rtwtr < 0.) rtwtr = 0.
IF (rchstor(jrch) < 0.) rchstor(jrch) = 0.

IF (rchstor(jrch) < 10.) THEN
  rtwtr = rtwtr + rchstor(jrch)
  rchstor(jrch) = 0.
END IF

RETURN
END SUBROUTINE rtmusk
