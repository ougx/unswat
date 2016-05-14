SUBROUTINE rtsed
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    deposition is based on fall velocity and degradation on stream

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_cov1(:)  |none          |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_cov2(:)  |none          |channel cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_di(:)    |m             |initial depth of main channel
!!    ch_li(:)    |km            |initial length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_si(:)    |m/m           |initial slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    ch_wdr(:)   |m/m           |channel width to depth ratio
!!    ideg        |none          |channel degredation code
!!                               |0: do not compute channel degradation
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    prf(:)      |none          |Reach peak rate adjustment factor for sediment
!!                               |routing in the channel. Allows impact of
!!                               |peak flow rate on sediment routing and
!!                               |channel reshaping to be taken into account
!!    rchdep      |m             |depth of flow on day
!!    rnum1       |none          |fraction of overland flow
!!    sdti        |m^3/s         |average flow on day in reach
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    spcon       |none          |linear parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    spexp       |none          |exponent parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    varoute(3,:)|metric tons   |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    peakr       |m^3/s         |peak runoff rate in channel
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    sedrch      |metric tons   |sediment transported out of channel
!!                               |during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dat2        |m             |change in channel depth during time step
!!    deg         |metric tons   |sediment reentrained in water by channel
!!                               |degradation
!!    dep         |metric tons   |sediment deposited on river bottom
!!    depdeg      |m             |depth of degradation/deposition from original
!!    depnet      |metric tons   |
!!    dot         |
!!    jrch        |none          |reach number
!!    qdin        |m^3 H2O       |water in reach during time step
!!    vc          |m/s           |flow velocity in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: jrch
REAL :: qdin, sedin, vc, cyin, cych, depnet, deg1, deg2, dep
REAL :: depdeg, dot, outfract, deg

jrch = 0
jrch = inum1
sedin = 0.0

IF (rtwtr > 0. .AND. rchdep > 0.) THEN
  
!! initialize water in reach during time step
  qdin = 0.
  qdin = rtwtr + rchstor(jrch)
  
!! do not perform sediment routing if no water in reach
  IF (qdin > 0.01) THEN
    
!! initialize sediment in reach during time step
    sedin = 0.
    sedin = varoute(3,inum2) * (1. - rnum1) + sedst(jrch)
    sedinorg = sedin
!! initialize reach peak runoff rate
    peakr = prf(jrch) * sdti
    
!! calculate flow velocity
    vc = 0.
    IF (rchdep < .010) THEN
      vc = 0.01
    ELSE
      vc = peakr / rcharea
    END IF
    IF (vc > 5.) vc = 5.
    
    tbase = ch_l2(jrch) * 1000. / (3600. * 24. * vc)
    tbase = prf(jrch)
    
    IF (tbase > 1.) tbase = 1.
    
    
!! JIMMY'S NEW IMPROVED METHOD for sediment transport
    cyin = 0.
    cych = 0.
    depnet = 0.
    deg = 0.
    deg1 = 0.
    deg2 = 0.
    dep = 0.
    cyin = sedin / qdin
    cych = spcon * vc ** spexp
    depnet = qdin * (cych - cyin)
    IF(ABS(depnet) < 1.e-6) depnet = 0.
    IF (vc < vcrit) depnet = 0.
    
!!  tbase is multiplied so that erosion is proportional to the traveltime,
!!  which is directly related to the length of the channel
!!  Otherwise for the same discharge rate and sediment deficit
!!  the model will erode more sediment per unit length of channel
!!  from a small channel than a larger channel. Modification made by Balaji Narasimhan
    
    IF (depnet > 1.e-6) THEN
      deg = depnet * tbase
!! First the deposited material will be degraded before channel bed
      IF (deg >= depch(jrch)) THEN
        deg1 = depch(jrch)
        deg2 = (deg - deg1) * ch_erodmo(jrch,i_mo)*ch_cov2(jrch)
      ELSE
        deg1 = deg
        deg2 = 0.
      END IF
      dep = 0.
    ELSE
      dep = -depnet ! * tbase
      deg = 0.
      deg1 = 0.
      deg2 = 0.
    END IF
    
    depch(jrch) = depch(jrch) + dep - deg1
    IF (depch(jrch) < 1.e-6) depch(jrch) = 0.
    
    sedin = sedin + deg1 + deg2 - dep
    IF (sedin < 1.e-6) sedin = 0.
    
    outfract = rtwtr / qdin
    IF (outfract > 1.) outfract = 1.
    
    sedrch = sedin * outfract
    IF (sedrch < 1.e-6) sedrch = 0.
    
    sedst(jrch) = sedin - sedrch
    IF (sedst(jrch) < 1.e-6) sedst(jrch) = 0.
    
!!    Mass balance tests
!! ambalsed = sedinorg + deg1 + deg2 - dep - sedrch - sedst(jrch)
!! if (ambalsed .gt. 1e-3) write (*,*) iida, jrch, ambalsed
    
!!  In this default sediment routing sediment is not tracked by particle size
    rch_san = 0.
    rch_sil = sedrch  !! As particles are not tracked by size, the sediments
    rch_cla = 0.      !! in reach is assumed to be silt for mass conservation
    rch_sag = 0.
    rch_lag = 0.
    rch_gra = 0.
    
!!    Bank erosion
    rchdy(55,jrch) = 0.
!!    Channel Degredation
    rchdy(56,jrch) = deg2
!!    Channel Deposition
    rchdy(57,jrch) = dep
!!    Floodplain Deposition
    rchdy(58,jrch) = 0.
!!    Total suspended sediments
    rchdy(59,jrch) = sedrch / rtwtr * 1.e6
    
!!    Organic nitrogen and Organic Phosphorus contribution from channel erosion
!!    ch_orgn(jrch) = deg2 * ch_onco(jrch) * 1000.
!!    ch_orgp(jrch) = deg2 * ch_opco(jrch) * 1000.
    
    ch_orgn(jrch) = deg2 * ch_onco(jrch) / 1000.
    ch_orgp(jrch) = deg2 * ch_opco(jrch) / 1000.
    
!! compute changes in channel dimensions
    IF (ideg == 1) THEN
      depdeg = 0.
      depdeg = ch_d(jrch) - ch_di(jrch)
      IF (depdeg < ch_si(jrch) * ch_li(jrch) * 1000.) THEN
        IF (qdin > 1400000.) THEN
          dot = 0.
          dot = 358.6 * rchdep * ch_s(2,jrch) * ch_cov1(jrch)
          dat2 = 1.
          dat2 =  dat2 * dot
          ch_d(jrch) = ch_d(jrch) + dat2
          ch_w(2,jrch) = ch_wdr(jrch) * ch_d(jrch)
          ch_s(2,jrch) = ch_s(2,jrch) - dat2 / (ch_l2(jrch) * 1000.)
          ch_s(2,jrch) = MAX(.0001, ch_s(2,jrch))
          CALL ttcoef(jrch)
        END IF
      END IF
    END IF
    
  ELSE
    sedrch = 0.
    rch_san = 0.
    rch_sil = 0.
    rch_cla = 0.
    rch_sag = 0.
    rch_lag = 0.
    rch_gra = 0.
    sedst(jrch) = sedin
!!    Bank erosion
    rchdy(55,jrch) = 0.
!!    Channel Degredation
    rchdy(56,jrch) = 0.
!!    Channel Deposition
    rchdy(57,jrch) = 0.
!!    Floodplain Deposition
    rchdy(58,jrch) = 0.
!!    Total suspended sediments
    rchdy(59,jrch) = 0.
    
  END IF !! end of qdin > 0.01 loop
  
END IF  !! end of rtwtr and rchdep > 0 loop

RETURN
END SUBROUTINE rtsed
