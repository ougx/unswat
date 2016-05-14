SUBROUTINE wet_pond
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    run wet pond processes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name             |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wtp_evrsv        |none         |detention pond evaporation coefficient
!!    hhvaroute(2,:,:) |m^3 H2O      |water
!!    hhvaroute(3,:,:) |metric tons  |sediment or suspended solid load
!!    i_mo             |none         |current month of simulation
!!    sub_subp_dt(:,:)  |mm H2O      |precipitation for time step in subbasin
!!    wtp_pvol(:       |m^3 H2O      |volume of permanent pool including forebay
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    hhvaroute(2,:,:) |m^3 H2O    |water
!!    hhvaroute(3,:,:) |metric tons|sediment or suspended solid load
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |time step counter
!!    k           |none          |weir stage counter
!!    titldum     |NA            |dummy string
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT:  surf_seep,surf_evap,water_depth
!!    Intrinsic: Log,exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm
IMPLICIT NONE

CHARACTER (LEN=80) :: titldum
INTEGER :: ii, k, sb
REAL :: qin,qout,qpnd,sedin,sedout,sedpnd,spndconc
REAL :: rf,imc,pndwdth,seep,evap,rain,seepa,surfa
REAL :: a1,b1,qdepth,hdep_ext,usettle,tmpw,mu,qhyd,dp
REAL :: alpha,beta,vol,hp,tvol,ht,decayexp,qweir,sedweir

sb = inum1
qout=0.; sedout=0.; qdepth=0.
dp = wtp_dp(sb) * 0.1 !cm

IF (iyr<wtp_iyr(sb).OR.(iyr==wtp_iyr(sb).AND.i_mo<wtp_imo(sb))) THEN
  RETURN
END IF

!!    Estimate Pond Volume using Austin Design Manual if not entered by user
!!    COA Environmental Criteria Manual Chapter 1.6.6 and Table 1-9
IF (wtp_dim(sb)==0) THEN
  imc = subdr_ickm(ihout) / subdr_km(ihout) !fraction impervious cover
  rf = 0.5463 * imc ** 2 + 0.328 * imc + 0.0296
  wtp_pvol(sb) = 0.162 * rf * (subdr_km(ihout) * 247.11) !ac-feet
  wtp_pvol(sb) = wtp_pvol(sb) * 1233.4 !m3
  IF (wtp_pvol(sb)<3000) wtp_pvol(sb) = 3000. !minimum area 21780ft2 * depth 5ft gives ~3000M3
  wtp_pdepth(sb) = 2. !m
  wtp_sdslope(sb) = 4. !h:v
  wtp_lenwdth(sb) = 2. !l:w
  wtp_pdia(sb) = 0.1524 !m (=6inch )
  wtp_plen(sb) = 5. !m
  wtp_pmann(sb) = 0.012 ! concrete surface
  wtp_ploss(sb) = 0.1 !minor loss
  
  CALL ext_dpth(wtp_extdepth(sb))
END IF

alpha = wtp_lenwdth(sb)
beta = wtp_sdslope(sb)
hp = wtp_pdepth(sb)
vol = wtp_pvol(sb)

!pond width at the bottom
a1 = 3. * beta * hp * (alpha + 1.)
b1 = 12. * alpha * (3.* vol / hp - 4.* beta**2 * hp**2)
pndwdth = (- a1 + SQRT(a1**2 + b1)) / (6.* alpha) !m

!surface area of the permanent pool
surfa = wtp_pvol(sb) / wtp_pdepth(sb) !m2,

!pond volume (permanent pool + extended detention
ht = hp + wtp_extdepth(sb)
a1 = 2.* beta * ht
tvol = (alpha * pndwdth **2 + (pndwdth + a1) * (alpha *  &
    pndwdth + a1) + (pndwdth * (alpha * pndwdth + a1) + alpha *  &
    pndwdth * (pndwdth + a1)) / 2.) * ht / 3.

!!    Get initial values from previous day
qpnd = wtp_qi(sb) !m^3
sedpnd = wtp_sedi(sb) * qpnd / 1.e6  !tons

!!    iterate for subdaily flow/sediment routing
DO ii=1,nstep
  
  qout = 0.; spndconc = 0.
  qin = hhvaroute(2,ihout,ii)   !m^3
  qpnd =  qin + qpnd !m^3
  sedin = hhvaroute(3,ihout,ii) !tons
  
!overflow to emergency outlet
  IF (qpnd>tvol) THEN
    qweir = qpnd - tvol
    qpnd = tvol
    sedweir = sedin * qweir / qin
    sedpnd = sedpnd + sedin - sedweir
  ELSE
    qweir = 0.
    sedweir = 0.
    sedpnd = sedin + sedpnd  !tons
  END IF
  IF (qpnd>1E-6) THEN
    spndconc = sedpnd / qpnd * 1.e6 !initial sed conc, mg/l
    IF(spndconc<wtp_sede(sb)) spndconc = wtp_sede(sb)
  ELSE
    cycle
  END IF
  
!!       Estimate water depth, m
  CALL wpnd_depth(qpnd,pndwdth,beta,alpha,qdepth)
  hdep_ext = qdepth - hp
  IF(hdep_ext<0) hdep_ext = 0.
  
!!       Calculate outflow
  IF (hdep_ext>0.1) THEN
!!          Use stage-discharge relationship if available
    IF (wtp_stagdis(sb)==1) THEN
      select case(wtp_sdtype(sb))
      case(1) !! 1 is exponential function
      qout = wtp_sdc1(sb) * EXP(wtp_sdexp(sb) * qdepth) + wtp_sdintc(sb)
      case(2) !! 2 is Linear function
      qout = wtp_sdc1(sb) * qdepth + wtp_sdintc(sb)
      case(3) !! 3 is logarthmic function
      qout = wtp_sdc1(sb) * LOG(qdepth) + wtp_sdintc(sb)
      case(4) !! 4 is power function
      qout = wtp_sdc1(sb) * (qdepth**3) + wtp_sdc2(sb) *  &
          (qdepth**2) + wtp_sdc3(sb) * qdepth + wtp_sdintc(sb)
      case(5)
      qout = wtp_sdc1(sb) * (qdepth**wtp_sdexp(sb)) + wtp_sdintc(sb) !m3/s
    END select
  ELSE
!!                Discharge out of extended detention storage through inverted PVC pipe
    CALL pipe_discharge(wtp_pdia(sb),wtp_plen(sb),  &
        hdep_ext,wtp_pmann(sb),wtp_ploss(sb),qout) !m3/s
    
  END IF
  qout = qout * idt * 60. !m3/s ->m^3
!  no outflow from the permanent pool
  IF (qout>qpnd-wtp_pvol(sb)) qout = qpnd - wtp_pvol(sb)
  IF (qout<0) qout = 0.
  
ELSE
!!          no discharge from the permanent pool
  qout = 0.
  sedout = 0.
END IF

!!       Seepage, evaporation and rainfall
a1 = wtp_lenwdth(sb)
b1 = wtp_sdslope(sb) ** 2 + 1.
seepa = a1 * pndwdth ** 2 + 2. * (a1 + 1) * pndwdth * qdepth *  &
    b1 ** 0.5 + 5.64 * qdepth ** 2 * b1 * wtp_sdslope(sb) !m2
b1 = 2.* beta * qdepth                !jeong 05/22/14
surfa = (a1 * pndwdth + b1) * (pndwdth + b1) !m2

seep = wtp_k(sb) / 1000. / 60. * idt *  seepa !m3
evap = wtp_evrsv(sb) * sub_pet(sb) / 1000. * surfa !m3
rain = sub_subp_dt(sb,ii) / 1000. * surfa !m3

!!       Mass balance for flow
qpnd = qpnd + rain - seep - evap - qout
IF (qpnd<0) qpnd = 0.

!----------------------------------------------------------------------------------
!!       Sediment removal
tmpw = sub_hhwtmp(sb,ii)
! water viscosity (g/cm-s) using 3rd order polynomial interpolation
mu = -3.e-6 * tmpw ** 3 + 0.0006 * tmpw ** 2 - 0.0469 * tmpw + 1.7517
mu = mu * 1.e-2
!settling velocity, cm/s
usettle = 0.7 * 981. / 18. * 1.6 / mu * dp ** 2 !ro,s=1.6g/cm3, g=981cm/s
b1 = 2.* beta * qdepth
surfa = (a1 * pndwdth + b1) * (pndwdth + b1) !m2

IF(qout>0.1) THEN
!WERF equation
  qhyd = qout / idt / 60. / surfa * 100.  ! m3/s / m2 * 100 = cm/s
  spndconc = spndconc * (1. - (1.+ usettle /  &
      (qhyd * wtp_hydeff(sb))) ** (-wtp_hydeff(sb)))  ! mg/l
  sedpnd = spndconc * qpnd * 1E-6 !tons, amount sediment in the pond at the end of the time step
!sediment conc no less than the minimum value
  IF(spndconc<wtp_sede(sb)) spndconc = wtp_sede(sb)
ELSE
!SWAT sediment equation for ponds
  decayexp = EXP(-7.667E-3 * idt / 60. * dp)
  spndconc = (spndconc - wtp_sede(sb)) * decayexp + wtp_sede(sb)
  sedpnd = spndconc * qpnd * 1E-6 !tons
END IF

!Sediment coming out of the pond
sedout = spndconc * qout * 1E-6 !tons


!! Store flow/sediment out of the pond at the subbasin outlet
hhvaroute(2,ihout,ii) = MAX(0.,qout)
hhvaroute(3,ihout,ii) = MAX(0.,sedout)

END DO

! Store end-of-day values for next day
wtp_qi(sb) = qpnd !m^3
IF (qpnd>0.1.AND.sedpnd>0.000001) THEN
  wtp_sedi(sb) = sedpnd / qpnd * 1.e6 !tons
ELSE
  wtp_sedi(sb) = 0
END IF
IF (wtp_sedi(sb)<wtp_sede(sb))  wtp_sedi(sb) = wtp_sede(sb)

END SUBROUTINE
 !- -----------------------------------------------------------------
SUBROUTINE ext_dpth(hmax)
use parm
IMPLICIT NONE

REAL,DIMENSION(40) :: cumrain=(/0.,0.006,0.012,0.019,0.026,0.034,  &
    0.043,0.053,0.064,0.077,0.092,0.11,0.134,0.166,0.212,0.287,0.384,  &
    0.542,0.802,1.262,1.462,1.587,1.688,1.746,1.784,1.811,1.832,  &
    1.849,1.863,1.875,1.885,1.894,1.902,1.91,1.917,1.924,1.93,1.93, 1.93,1.93/)
REAL :: ia, ss, plen,inflow,outflow,pndvol,wdth,pndarea,vtmp,hdep
REAL :: aa,bb,pdia,hvol,alpha
REAL, DIMENSION(40):: fa
REAL, INTENT(out) :: hmax
INTEGER :: ii,sb

sb = inum1
fa=0; inflow=0.

ss = 1000. / sub_cn2(sb) - 10.
plen = wtp_plen(sb) * 3.2808 !ft
pdia = wtp_pdia(sb) + 3.2808 !ft
!volume of permanent pool
pndvol = wtp_pvol(sb) * 3.2808 ** 3 !ft^3
!pond width at the bottom of the pond
wdth = ((140.3**2 - 4*(5828.2-0.072 * pndvol))**0.5 - 140.3) / 2. ! ft
!pond area at the top of permanent pool
pndarea = (wdth + 187.) * (2*wdth + 187.) ! ft^2
alpha = wtp_lenwdth(sb)

hmax=0.
DO ii=1,40
  IF(hvol<0) hvol = 0.
! calculate surface runoff
  ia = 0.2 * ss
  IF (ia>cumrain(ii)) THEN
    ia = cumrain(ii)
    fa(ii) = 0
  ELSE
    fa(ii) = ss * (cumrain(ii) - ia) / (cumrain(ii) - ia + ss)
  END IF
  IF (ii>=2) inflow = fa(ii) - fa(ii-1) !inches
  
! calculate max water depth in extended detention
  inflow = inflow * subdr_km(sb) * 8.97E5 !inch-km2 ->ft^3
  hvol = hvol + inflow  !ft^3
  
  CALL wpnd_depth(hvol,wdth,wtp_sdslope(sb),alpha,hdep)
  IF (hdep>hmax) hmax = hdep
  
!compute discharge from outlet pipe
  CALL pipe_discharge(pdia,plen,hdep,wtp_pmann(sb),wtp_ploss(sb), outflow)
  
  outflow = outflow * 35.31 * 300.  !m3/s ->ft^3 per 5 minutes
  IF (outflow>hvol) outflow = hvol
  hvol = hvol - outflow
  
END DO

hmax = hmax / 3.2808 ! meter

END SUBROUTINE  &
!- -----------------------------------------------------------------

SUBROUTINE wpnd_depth(hvol,width,slp,lenwdth,hdep)
!calculate ponding depth using Newton's method
IMPLICIT NONE
REAL, INTENT(in):: hvol,width,slp,lenwdth
REAL, INTENT(out):: hdep
REAL:: dfn,fn,alp,ll

alp = lenwdth
ll = width
hdep = 0.1; fn = 100
DO WHILE (ABS(fn)>0.1)
  fn = alp * ll ** 2 * hdep + slp * ll * (1.+ alp) * hdep ** 2 +  &
      4. / 3.* slp ** 2 * hdep ** 3 - hvol
  dfn = alp * ll ** 2 + 2. * slp * ll * (1.+ alp) * hdep + 4.*  &
      slp ** 2 * hdep ** 2
  hdep = hdep - fn / dfn
END DO
END SUBROUTINE

!-------------------------------------------------------------------

SUBROUTINE pipe_discharge(pdia,plen,hdep,mann,mloss,outflow)
! calculate discharge from extended detention through pvc pipe,m3/s
IMPLICIT NONE
REAL, INTENT(in):: pdia,plen,hdep,mann,mloss
REAL, INTENT(out):: outflow
REAL:: parea,kf,rh

parea = 3.14159 * (3.2808 * pdia) ** 2 / 4. !ft^2
rh = 3.2808 * pdia / 4. !ft
kf = 29. * plen * mann ** 2 / rh ** 1.33
outflow = parea * ((64.4 * hdep) / (1.+ kf + mloss)) ** 0.5 !cfs
outflow = outflow / 35.31 !m3/s

END SUBROUTINE
