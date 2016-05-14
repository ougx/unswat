SUBROUTINE readpest
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads parameters from the toxin/pesticide database
!!    (pest.dat)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mpdb        |none          |maximum number of toxins/pesticides in
!!                               |database
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)    |none          |application efficiency (0-1)
!!    decay_f(:)  |none          |exponential of the rate constant for
!!                               |degradation of the pesticide on foliage
!!    decay_s(:)  |none          |exponential of the rate constant for
!!                               |degradation of the pesticide in soil
!!    hlife_f(:)  |days          |half-life of pesticide on foliage
!!    hlife_s(:)  |days          |half-life of pesticide in soil
!!    pname(:)    |NA            |name of pesticide/toxin
!!    pst_wof(:)  |none          |fraction of pesticide on foliage which
!!                               |is washed-off by a rainfall event
!!    pst_wsol(:) |mg/L (ppm)    |solubility of chemical in water
!!    skoc(:)     |(mg/kg)/(mg/L)|soil adsorption coefficient normalized
!!                               |for soil organic carbon content
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    ip          |none          |counter which represents the array
!!                               |storage number of the pesticide data
!!                               |the array storage number is used by the
!!                               |model to access data for a specific
!!                               |pesticide
!!    ipnum       |none          |number of pesticide/toxin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: ip, ipnum, eof
CHARACTER (LEN=17) :: pstnm
REAL :: skocp, wofp, hlff, hlfs, apefp, pwsol
eof = 0


DO
!!initialize local variables
  apefp = 0.0
  hlff = 0.0
  hlfs = 0.0
  pstnm = ""
  pwsol = 0.0
  skocp = 0.0
  wofp = 0.0
  
  READ (106,5000,IOSTAT=eof) ip, pstnm, skocp, wofp, hlff, hlfs, apefp, pwsol
  IF (eof < 0) EXIT
  
  IF (ip == 0) EXIT
  
  pname(ip) = pstnm
  skoc(ip) = skocp
  pst_wof(ip) = wofp
  hlife_f(ip) = hlff
  hlife_s(ip) = hlfs
  ap_ef(ip) = apefp
  pst_wsol(ip) = pwsol
  
  
!! calculations: the first-order rate law for the decay of pesticides
!! is dP/dt = -kP where P is the amount of pesticide,
!! t is the time and k is the rate constant for degradation. To calculate
!! the amount of pesticide at any time after application the equation
!! P(t) = P_o*Exp(-kt) is used where P_o is the original amount of
!! pesticide. k can be calculate with the equation k = 0.693/hlife.
!! decay_f or decay_s = Exp(-k)
  IF (hlife_f(ip) > 0.) THEN
    decay_f(ip) = EXP(-.693/hlife_f(ip))
  ELSE
    decay_f(ip) = 0.
  END IF
  IF (hlife_s(ip) > 0.) THEN
    decay_s(ip) = EXP(-.693/hlife_s(ip))
  ELSE
    decay_s(ip) = 0.
  END IF
  
!! set values for pesticide routed through main channel network
  IF (ip == irtpest) THEN
    pest_sol = pst_wsol(ip) * 1000.
  END IF
  
END DO

CLOSE (106)

RETURN
5000 FORMAT (i3,a17,f10.1,f5.2,2F8.1,f5.2,f11.3)
END SUBROUTINE readpest
