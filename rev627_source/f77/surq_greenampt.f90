SUBROUTINE surq_greenampt
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Predicts daily runoff given breakpoint precipitation and snow melt
!!    using the Green & Ampt technique

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idt         |minutes       |length of time step used to report
!!                               |precipitation data for sub-daily modeling
!!    ihru        |none          |HRU number
!!    iyr         |year          |year being simulated (eg 1980)
!!    nstep       |none          |max number of time steps per day
!!    newrti(:)   |mm/hr         |infiltration rate for last time step from the
!!                               |previous day
!!    nstep       |none          |number of rainfall time steps for day
!!    precipdt(:) |mm H2O        |precipitation for the time step during day
!!    sol_k(1,:)  |mm/hr         |saturated hydraulic conductivity of 1st soil
!!                               |layer
!!    sol_por(:,:)|none          |total porosity of soil layer expressed as a
!!                               |fraction of the total volume
!!    sol_sumfc(:)|mm H2O        |amount of water held in the soil profile
!!                               |at field capacity
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on
!!                               |any given day
!!    swtrg(:)    |none          |rainfall event flag:
!!                               |  0: no rainfall event over midnight
!!                               |  1: rainfall event over midnight
!!    wfsh(:)     |mm            |average capillary suction at wetting front
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff generated each hour of day
!!                               |in HRU
!!    newrti(:)   |mm/hr         |infiltration rate for last time step from the
!!                               |previous day
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    swtrg(:)    |none          |rainfall event flag:
!!                               |  0: no rainfall event over midnight
!!                               |  1: rainfall event over midnight
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adj_hc      |mm/hr         |adjusted hydraulic conductivity
!!    cuminf(:)   |mm H2O        |cumulative infiltration for day
!!    cumr(:)     |mm H2O        |cumulative rainfall for day
!!    dthet       |mm/mm         |initial moisture deficit
!!    excum(:)    |mm H2O        |cumulative runoff for day
!!    exinc(:)    |mm H2O        |runoff for time step
!!    f1          |mm H2O        |test value for cumulative infiltration
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |hour          |hour of day in which runoff is generated
!!    psidt       |mm            |suction at wetting front*initial moisture
!!                               |deficit
!!    rateinf(:)  |mm/hr         |infiltration rate for time step
!!    rintns(:)   |mm/hr         |rainfall intensity
!!    soilw       |mm H2O        |amount of water in soil profile
!!    tst         |mm H2O        |test value for cumulative infiltration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Exp, Real, Mod

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

INTEGER :: j, k, kk,sb
REAL :: adj_hc, dthet, soilw, psidt, tst, f1
REAL, DIMENSION (nstep+1) :: cumr, cuminf, excum, exinc, rateinf
REAL, DIMENSION (nstep+1) :: rintns
!! array location #1 is for last time step of prev day

j = 0
j = ihru
sb = hru_sub(j)

!! reset values for day
cumr = 0.
cuminf = 0.
excum = 0.
exinc = 0.
rateinf = 0.
rintns = 0.

!! calculate effective hydraulic conductivity
adj_hc = 0.
adj_hc = (56.82 * sol_k(1,j) ** 0.286)  &
    / (1. + 0.051 * EXP(0.062 * cnday(j))) - 2.
IF (adj_hc <= 0.) adj_hc = 0.001

dthet = 0.
IF (swtrg(j) == 1) THEN
  swtrg(j) = 0
  dthet = 0.001 * sol_por(1,j) * 0.95
  rateinf(1) = newrti(j)
  newrti(j) = 0.
ELSE
  soilw = 0.
  IF (sol_sw(j) >= sol_sumfc(j)) THEN
    soilw = 0.999 * sol_sumfc(j)
  ELSE
    soilw = sol_sw(j)
  END IF
  dthet = (1. - soilw / sol_sumfc(j)) * sol_por(1,j) * 0.95
  rateinf(1) = 2000.
END IF

psidt = 0.
psidt = dthet * wfsh(j)

k = 1
rintns(1) = 60. * precipdt(2) / REAL(idt)  !! urban 60./idt  NK Feb 4,08

DO k = 2, nstep+1
!! calculate total amount of rainfall during day for time step
  cumr(k) = cumr(k-1) + precipdt(k)
!! and rainfall intensity for time step
  rintns(k) = 60. * precipdt(k+1) / REAL(idt) !!urban 60./idt NK Feb 4,08
  
!! if rainfall intensity is less than infiltration rate
!! everything will infiltrate
  IF (rateinf(k-1) >= rintns(k-1)) THEN
    cuminf(k) = cuminf(k-1) + rintns(k-1) * REAL(idt) / 60. !!urban 60./idt NK Feb 4,08
    IF (excum(k-1) > 0.) THEN
      excum(k) = excum(k-1)
      exinc(k) = 0.
    ELSE
      excum(k) = 0.
      exinc(k) = 0.
    END IF
  ELSE
!! if rainfall intensity is greater than infiltration rate
!! find cumulative infiltration for time step by successive
!! substitution
    tst = 0.
    tst = adj_hc * REAL(idt) / 60.  !!urban 60./idt NK Feb 4,08
    DO
      f1 = 0.
      f1 = cuminf(k-1) + adj_hc * REAL(idt) / 60. +  &
          psidt * LOG((tst + psidt)/(cuminf(k-1) + psidt))
      IF (ABS(f1 - tst) <= 0.001) THEN
        cuminf(k) = f1
        excum(k) = cumr(k) - cuminf(k)
        exinc(k) = excum(k) - excum(k-1)
        IF (exinc(k) < 0.) exinc(k) = 0.
        hhqday(k-1) = exinc(k)
        EXIT
      ELSE
        tst = 0.
        tst = f1
      END IF
    END DO
  END IF
  
!! Urban Impervious cover
  IF (iurban(j)>0) THEN
!runoff from pervious area
    hhqday(k-1) = hhqday(k-1) * (1.- fcimp(urblu(j)))
    
!runoff from impervious area with initial abstraction
    ubnrunoff(k-1) = (precipdt(k) - abstinit) * fcimp(urblu(j))
    IF ( ubnrunoff(k-1)<0)  ubnrunoff(k-1) = 0.
  END IF
  
!! daily total runoff
  surfq(j) = surfq(j) + hhqday(k-1) + ubnrunoff(k-1)
  
!! calculate new rate of infiltration
  rateinf(k) = adj_hc * (psidt / (cuminf(k) + 1.e-6) + 1.)
  
END DO

IF (sum(precipdt) > 12.) THEN
  swtrg(j) = 1
  newrti(j) = rateinf(nstep)
END IF

RETURN
5000 FORMAT(//,'Excess rainfall calculation for day ',i3,' of year ',  &
    i4,' for sub-basin',i4,'.',/)
5001 FORMAT(t2,'Time',t9,'Incremental',t22,'Cumulative',t35,'Rainfall',  &
    t45,'Infiltration',t59,'Cumulative',t71,'Cumulative',t82,  &
    'Incremental',/,t2,'Step',t10,'Rainfall',t23,'Rainfall',  &
    t35,'Intensity',t49,'Rate',t58,'Infiltration',t73,'Runoff',  &
    t84,'Runoff',/,t12,'(mm)',t25,'(mm)',t36,'(mm/h)',t48,  &
    '(mm/h)',t62,'(mm)',t74,'(mm)',t85,'(mm)',/)
5002 FORMAT(i5,t12,f5.2,t24,f6.2,t36,f6.2,t47,f7.2,t61,f6.2,t73,f6.2,  &
    t84,f6.2)
END SUBROUTINE surq_greenampt
