SUBROUTINE rchuse
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine removes water from reach for consumptive water use

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1       |none          |reach number
!!    i_mo        |none          |month of simulation
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    wurch(:,:)  |10^4 m^3/day  |average daily water removal from the reach
!!                               |for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jrch        |none          |HRU number
!!    wtrin       |m^3 H2O       |water outflow from reach prior to
!!                               |subtracting irrigation diversions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: jrch, ii
REAL :: wtrin

jrch = 0
jrch = inum1

wtrin = 0.
wtrin = rtwtr

rtwtr = rtwtr - wurch(i_mo,jrch) * 10000.
IF (rtwtr < 0.) rtwtr = 0.

IF (ievent > 2) THEN
  DO ii = 1, nstep
    hrtwtr(ii) = hrtwtr(ii) - wurch(i_mo,jrch) * 10000. / REAL(nstep)
    IF (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
  END DO
END IF

IF (wtrin /= rtwtr .AND. wtrin > 0.01) THEN
  sedrch = sedrch * rtwtr / wtrin
  
  rch_san = rch_san * rtwtr / wtrin
  rch_sil = rch_sil * rtwtr / wtrin
  rch_cla = rch_cla * rtwtr / wtrin
  rch_sag = rch_sag * rtwtr / wtrin
  rch_lag = rch_lag * rtwtr / wtrin
  rch_gra = rch_gra * rtwtr / wtrin
  
  IF (sedrch  < 1.e-6) THEN
    sedrch = 0.
    rch_san = 0.
    rch_sil = 0.
    rch_cla = 0.
    rch_sag = 0.
    rch_lag = 0.
    rch_gra = 0.
  END IF
  
  IF (ievent > 2) THEN
    DO ii = 1, nstep
      hsedyld(ii) = hsedyld(ii) * rtwtr / wtrin
      IF (hrtwtr(ii) == 0.) hsedyld(ii) = 0.
    END DO
  END IF
END IF

RETURN
END SUBROUTINE rchuse
