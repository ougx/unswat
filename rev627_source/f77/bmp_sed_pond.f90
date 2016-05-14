SUBROUTINE sed_pond(kk,flw,sed)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through a sedimentation pond in the subbasin

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in which HRU/reach is located
!!    i_mo        |none          |current month of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sb          |none          |subbasin or reach number
!!    kk          |none          |filter id number in the subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
IMPLICIT NONE

INTEGER :: sb, kk, ii
REAL*8 :: tsa,mxvol,pdia,ksat,dp,sub_ha,mxh,hweir,phead,pipeflow
REAL*8 :: qin,qout,qpnd,qpndi,sweir,spndconc,sedpnde,sedpndi,hpnd
REAL*8 :: qweir, qtrns,qpipe,splw,sedconcweir,td,ksed,qevap
REAL, DIMENSION(3,0:nstep), INTENT(inout) :: flw, sed

sb = inum1
sub_ha = da_ha * sub_fr(sb)
qin = 0.; qout = 0.; sedpndi = 0.; sedpnde = 0.
flw(2,:) = 0.; sed(2,:) = 0.

!! Initialize parameters, coefficients, etc
tsa = sp_sa(sb,kk)     !total surface area of pond (m^2)
mxvol = sp_pvol(sb,kk)    !max. capacity of the basin (m^3)
mxh = sp_pvol(sb,kk) / sp_sa(sb,kk) !max. depth of water, m
splw = sp_bpw(sb,kk)       !spillway overflow weir width (m)
pdia = sp_pd(sb,kk)       !outflow orifice pipe diameter (mm)
ksat = sp_k(sb,kk)      !saturated hydraulic conductivity (mm/hr)
dp = sp_dp(sb,kk) * 1000. !median particle size of TSS, micrometer

!! Get initial values from previous day
qpnd = sp_qi(sb,kk) !m^3
spndconc = sp_sedi(sb,kk)
qevap = 0

DO ii=1,nstep
  qweir = 0.; qtrns = 0.; qpipe = 0.
!inflow = runoff + precipitation
  qin = flw(1,ii) * 10. * (sub_ha - tsa / 10000.) +  &
      precipdt(ii) * tsa / 1000.  !m^3
  qpndi = qpnd + qin !m^3
  hpnd = qpndi / tsa !ponding depth on the filter surface, m
  
  IF (qin<0.001.AND.qpndi<0.001)THEN
!No inflow/outflow during this time step
    flw(2,ii) = 0.
  ELSE
!spillway overflow
    IF (hpnd > mxh) THEN
      hweir = MAX(0.,hpnd - mxh)  !water depth over weir crest, m
!weir overflow, m^3
      qweir = 3.33 * splw * hweir ** 1.5 * idt * 60.
      hpnd = MAX(0.,(qpndi - qweir) / tsa) !m
!overflow amount is no larger than surplus water above spillway height
      IF (qweir>qpndi-mxvol) THEN
        qweir = MAX(0.,qpndi - mxvol) !weir outflow
        hpnd = mxh
      END IF
      qpnd = MAX(0.,qpndi - qweir)
    ELSE
      qpnd = qpndi
    END IF
    
!Transmission loss through infiltration
    qtrns = ksat * tsa / 1000./ 60. * idt
    qpnd = qpnd - qtrns
    bmp_recharge(sb) = bmp_recharge(sb) + qtrns / (sub_ha*10000.- tsa) *1000.
    
    IF (qpnd<0) THEN
      qpnd = 0.
      qtrns = 0.
    END IF
    
!Evapotranspiration loss
    qevap = tsa * sub_etday(sb) / 1000. / 1440. * idt !m^3
    IF(qevap<1E-6) qevap = 0.
    qpnd = qpnd - qevap
    IF (qpnd<0) THEN
      qpnd = 0.
      qevap = 0.
    END IF
    
!Outflow through orifice pipe
    hpnd = qpnd / tsa  !m
    phead = MAX(0.,hpnd * 1000. - pdia / 2.)  !mm
!            If (phead>pdia/2.) then
    qpipe = pipeflow(pdia,phead) * idt *60. !m^3
!            else
!               qpipe = qout *  0.9
!            endif
    
!update out flow, m^3
    qout = qpipe
    qpnd = MAX(0.,qpnd - qout)
    
!outflow normalized to subbasin area, mm
    flw(1,ii) = qin / ((sub_ha - tsa / 10000.) *10.)
    flw(2,ii) = qout / ((sub_ha - tsa / 10000.) *10.) !mm
    flw(3,ii) = qweir / ((sub_ha - tsa / 10000.) *10.) !mm
  END IF
  
!---------------------------------------------------------
!! TSS removal
  sweir = 0.
  
!Sediment bypass through spillway overflow, tons
  IF (qweir>0.001) THEN
    IF(qin>0.001) THEN
      sedconcweir = sed(1,ii) / qin !tons/m3
    ELSE
      sedconcweir = spndconc !tons/m3
    END IF
    sweir = sedconcweir * qweir !tons
    sedpndi = sedpnde + sed(1,ii) - sweir !tons
    spndconc = sedpndi / qpndi !tons/m3
  ELSE
    
    IF(qpndi>0.001) THEN
      sedpndi = sedpnde + sed(1,ii)  !tons
      spndconc = sedpndi / qpndi ! tons/m3
    ELSE
      sedpndi = 0
      spndconc = 0
    END IF
    
  END IF
  
! sediment conc at the beginning of the time step
  spndconc = spndconc * 1.e6 !mg/l
  
!Estimate TSS removal due to sedimentation
  IF (spndconc>sp_sede(sb,kk)) THEN
    ksed = MIN(134.8,41.1 * hpnd ** -0.999)  !decay coefficient, Huber et al. 2006
    td = qpnd / qpipe / nstep !detention time, day
    spndconc = (spndconc - sp_sede(sb,kk)) * EXP(-ksed * td) + sp_sede(sb,kk)
  END IF
  
!Sediment coming out of the pond
  sed(2,ii) = spndconc * qpipe * 1.e-6  !tons
  sedpnde = spndconc * qpnd * 1.e-6 !tons
  sed(3,ii) = sweir
  
! total sediment removed from the pond, tons
  sp_sed_cumul(sb,kk) = sp_sed_cumul(sb,kk) + sedpndi - sedpnde
  
!       write(*,'(3i5,20f10.3)') iyr,iida,ii,precipdt(ii),qin,
!     & qpipe,qweir,qtrns,qevap
!       write(*,'(3i5,20f10.3)') iyr,iida,ii,precipdt(ii),qin,
!     & qpipe,qweir,sed(1,ii)*1000,sed(2,ii)*1000,sweir*1000
  
END DO

! Store end-of-day values for next day
sp_qi(sb,kk) = qpnd
sp_sedi(sb,kk) = spndconc


RETURN
END SUBROUTINE
  &
!--- ---------------------------------------------------------------------------
FUNCTION pipeflow(d,h)
!this function calculates orifice pipe flow and returns flow rate (m3/s)
REAL*8, INTENT(in):: d,h
REAL*8:: dia, hdepth, pipeflow, area

dia = d / 1000. !m
hdepth = h / 1000. !m

area = dia **2 / 4 * 3.14159 !m2
pipeflow = 0.6 * area * (19.6 * hdepth) ** 0.5 !m3/s
END FUNCTION
