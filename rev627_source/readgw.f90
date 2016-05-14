SUBROUTINE readgw
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the parameters from the HRU/subbasin groundwater
!!    input file (.gw)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bf(:) |1/days        |alpha factor for groundwater recession curve
!!    alpha_bf_d(:) | 1/days     |alpha factor for groudwater recession curve of the deep aquifer
!!    alpha_bfe(:)|none          |Exp(-alpha_bf(:))
!!    alpha_bfe_d (:) |1/days    |Exp(-alpha_bf_d(:)) for deep aquifer
!!    ch_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from bank storage to the root
!!                               |zone as a result of soil moisture depletion
!!    deepst(i)   |mm H2O        |depth of water in deep aquifer
!!    delay(:)    |days          |groundwater delay: time required for water
!!                               |leaving the bottom of the root zone to
!!                               |reach the shallow aquifer
!!    gw_delaye(:)|none          |Exp(-1./(delay(:))
!!    gw_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from the shallow aquifer to
!!                               |the root zone as a result of soil moisture
!!                               |depletion
!!    gw_spyld(:) |m**3/m**3     |specific yield for shallow aquifer
!!    gwht(:)     |m             |groundwater height
!!    gwminp(:)   |mg P/L        |soluble P concentration in groundwater
!!                               |loading to reach
!!    gwno3(:)    |mg N/L        |nitrate-N concentration in groundwater
!!                               |loading to reach
!!    gwqmn(:)    |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required before groundwater flow will occur
!!    rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of
!!                               |root zone percolation that reaches the deep
!!                               |aquifer
!!    revapmn(:)  |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required to allow revap to occur
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    shallst_n(:)|ppm NO3-N     |nitrate concentration in shallow aquifer
!!                               |converted to kg/ha
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line for .gw file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
use rossmod

CHARACTER (LEN=80) :: titldum
INTEGER :: eof
REAL :: hlife_ngw

eof = 0
hlife_ngw = 0.0

DO
  READ (110,5000) titldum
  READ (110,*) shallst(ihru)
  READ (110,*) deepst(ihru)
  READ (110,*) delay(ihru)
  READ (110,*) alpha_bf(ihru)
  READ (110,*) gwqmn(ihru)
  READ (110,*) gw_revap(ihru)
  READ (110,*) revapmn(ihru)
  READ (110,*) rchrg_dp(ihru)
  READ (110,*,IOSTAT=eof) gwht(ihru)
  IF (eof < 0) EXIT
  READ (110,*,IOSTAT=eof) gw_spyld(ihru)
  IF (eof < 0) EXIT
  READ (110,*,IOSTAT=eof) shallst_n(ihru)
  IF (eof < 0) EXIT
  READ (110,*,IOSTAT=eof) gwminp(ihru)
  IF (eof < 0) EXIT
  READ (110,*,IOSTAT=eof) hlife_ngw
  IF (eof < 0) EXIT
!! organic n and p in the lateral flow     - by J.Jeong BREC 2011
  READ (110,*,IOSTAT=eof) lat_orgn(ihru)
  IF (eof < 0) EXIT
  READ (110,*,IOSTAT=eof) lat_orgp(ihru)
  IF (eof < 0) EXIT
  READ (110,*,IOSTAT=eof) alpha_bf_d(ihru)
  EXIT
END DO

!!    set default values for mike van liew
IF (hlife_ngw <= 0.) hlife_ngw = hlife_ngw_bsn
!!    set default values for mike van liew

!!    set default values
IF (deepst(ihru) <= 0.) deepst(ihru) = 1000.
IF (hlife_ngw <= 0.) hlife_ngw = 365.
IF (lat_orgn(ihru) <= 1.e-6) lat_orgn(ihru) = 0.
IF (lat_orgp(ihru) <= 1.e-6) lat_orgp(ihru) = 0.

!!    perform additional calculations
alpha_bfe(ihru) = EXP(-alpha_bf(ihru))
IF(delay(ihru) < .1) delay(ihru) = .1
gw_delaye(ihru) = EXP(-1./(delay(ihru) + 1.e-6))
shallst_n(ihru) = shallst_n(ihru) * shallst(ihru) / 100.
gw_nloss(ihru) = EXP(-.693 / hlife_ngw)

!!    alpha baseflow factor for deep aquifer according to Yi Luo
alpha_bfe_d(ihru) = EXP(-alpha_bf_d(ihru))


!! assign values to channels
ch_revap(i) = gw_revap(ihru)

!! assign values to channels
ch_revap(i) = gw_revap(ihru)

CLOSE (110)

!!-------------------OGXinSWAT Begin------------------------------
!!  store initial groundwater table
!      if (ievent>0) then
!        i=ihru
!        if (gwht(i)<0) then
!          call USTOP('Initial DEPgw is negative')
!        endif
!        SOLCOL(i)%DEPGW0=SOLCOL(i)%DEPGW0+shallst(i)/gw_spyld(i)
!      endif
!!-------------------------End------------------------------------

RETURN
5000 FORMAT (a)
END SUBROUTINE readgw
