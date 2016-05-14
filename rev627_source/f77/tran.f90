SUBROUTINE tran
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes tributary channel transmission losses

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_k(1,j)   |mm/hr         |effective hydraulic conductivity of tributary
!!                               |channel alluvium
!!    ch_w(1,j)   |(m)           |average main channel width
!!    ch_l1(j)    |(km)          |main channel length
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    ihru        |none          |HRU number
!!    peakr       |m^3/s         |peak runoff rate
!!    qday        |mm H2O        |surface runoff loading to main channel
!!                               |from HRU on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    peakr       |m^3/s         |peak runoff rate
!!    qday        |mm H2O        |amount of surface runoff loading to main
!!                               |channel from HRU on current day
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer in HRU
!!    tloss       |mm H2O        |amount of water removed from surface runoff
!!                               |via transmission losses on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a           |m^3           |regression intercept for unit channel
!!    axw         |m^3           |regression intercept for unit channel with
!!                               |a flow of duration DUR
!!    b           |none          |regression slope for unit channel
!!    bxw         |none          |regression slope for a channel of length L
!!                               |and width W
!!    dur         |hr            |length of time runoff occurs if traveling
!!                               |at peak runoff rate
!!    j           |none          |HRU number
!!    k           |1/(m-km)      |decay factor for unit channel with a flow
!!                               |duration DUR and volume VOL
!!    pr1         |m^3/s         |peak runoff rate prior to accounting for
!!                               |transmission losses
!!    pxw         |m^3           |threshold volume for a channel reach of
!!                               |length L and width W
!!    qinit       |mm H2O        |amount of water in surface runoff loading
!!                               |prior to accounting for transmission losses
!!    vo          |m**3 H2O      |volume of water in surface runoff loading
!!                               |to main channel on day
!!    xx          |none          |variable to hold intermediate calculation
!!    zz          |none          |variable to hold intermediate calculation

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j
REAL :: qinit, vo, dur, k, b, zz, bxw, pr1, a, xx, axw, pxw

!! initialize variables
j = 0
j = ihru

IF (ch_k(1,hru_sub(j)) <= 0.) RETURN

!! save runoff amount prior to transmission losses
qinit = 0.
pr1 = 0.
qinit = qday
pr1 = peakr

!! calculate incoming volume of water
vo = 0.
vo = qday * hru_km(j) * 1000.   !!volume incoming: m^3

!! calculate flow duration
dur = 0.
dur = vo / (peakr * 3600.)      !!duration: hr
IF (dur > 24.) dur = 24.

!! flagged by pdw, should be after xx condition
!!!! zero surface runoff/peak rate
!!      qday = 0.
!!      peakr = 0.

xx = 0.
xx = 2.6466 * ch_k(1,hru_sub(j)) * dur / vo
IF (xx < 1.) THEN
  
!moved by pdw
!! zero surface runoff/peak rate
  qday = 0.
  peakr = 0.
! end move pdw
  
  k = 0.
  k = -2.22 * LOG(1. - xx)
  b = 0.
  b = EXP(-0.4905 * k)
  
! bug fix by pdw
! old code: if ((1. - b) > 1.e-20) then
! new code:
  IF ((1. - b) >= 0.) THEN
! end fix pdw
    
    zz = 0.
    zz = - k * ch_w(1,hru_sub(j)) * ch_l1(j)
    IF (zz >= -30.) THEN
      bxw = 0.
      bxw = EXP(zz)
      a = 0.
      a = -.2258 * ch_k(1,hru_sub(j)) * dur
      IF (1. - b > 0.01) THEN
        axw = (a / (1. - b)) * (1. - bxw)
      ELSE
        axw = 0.
      END IF
      pxw = -axw / bxw
      IF (vo > pxw) THEN
        qday = axw + bxw * vo              !!surface runoff: m^3
        qday = qday / (1000. * hru_km(j))  !!surface runoff: mm
        IF (qday < 0.) qday = 0.
        IF (qday > 0.) THEN
          peakr = (1. / (dur * 3600.)) * (axw - (1. - bxw) * vo)  &
              + bxw * pr1              !!peak rate: m^3/s
          IF (peakr < 0.) peakr = 0.
        END IF
      END IF
    END IF
  END IF
END IF

tloss = qinit - qday
IF (tloss < 0.) THEN
  qday = qinit
  tloss = 0.
END IF

!! add transmission losses to shallow aquifer storage
!! This will be done in gwmod.
!      shallst(j) = shallst(j) + tloss

RETURN
END SUBROUTINE tran
