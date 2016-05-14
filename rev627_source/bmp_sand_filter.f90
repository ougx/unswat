SUBROUTINE sand_filter(kk,flw,sed)

! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through sand filters in the subbasin

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
!!    SWAT: pipeflow coded in bmp_sed_pond.f90

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
IMPLICIT NONE

INTEGER :: sb, ii
INTEGER, INTENT(in) :: kk
REAL*8 :: tsa,ffsa,vfiltr,mxvol,pdia,ksat,por,dp,dc,pden,alp,  &
    wetfsh,whd,sub_ha,dt,qcms,effct,effl,effg,effbr,vpipe,phead,hpnd,  &
    tmpw,qloss,fsat,qpipe,mu,pipeflow,splw,hweir,tst,kb,qintns,qq,  &
    qfiltr,sloss,spndconc,sedpnd,qpndi,qpnde,sedrmeff,sed_removed,  &
    sedconc,qevap,hrd
REAL*8, DIMENSION(:) :: qpnd(0:nstep),qsw(0:nstep),qin(0:nstep),  &
    qout(0:nstep),fc(0:nstep),f(0:nstep)
REAL, DIMENSION(3,0:nstep), INTENT(inout) :: flw, sed

sb = inum1
sub_ha = da_ha * sub_fr(sb)
dt = REAL(idt) / 60. !time interval in hours
qin = 0.; qout = 0.;qevap=0
flw(2,:) = 0.; sed(2,:) = 0.;f=0
qpnd = 0.; qsw = 0.; qpndi = 0.; qpnde = 0.; fc = 0.;qfiltr = 0.
kb = 1.38E-16 !Boltzmann constant, g-cm^2/s^2-K

!! Initialize parameters, coefficients, etc
tsa = ft_sa(sb,kk)     !total surface area of filter (m^2)
IF (tsa>sub_ha*10000.*0.5) tsa = sub_ha*10000.*0.5 !sandfilter should be smaller than 0.5 times the subbasin area
ffsa = ft_fsa(sb,kk)     !fraction of infiltration bed in the filtration basin (m2/m2)
mxvol = ft_h(sb,kk) / 1000. * tsa    !max. capacity of the basin (m^3)
pdia = ft_pd(sb,kk)       !outflow orifice pipe diameter (mm)
splw = ft_bpw(sb,kk)       !spillway overflow weir width (m)
ksat = ft_k(sb,kk)      !saturated hydraulic conductivity (mm/hr)
por = ft_por(sb,kk)     !filter porosity
dp = ft_dp(sb,kk) / 10.      !median diameter of TSS particle (cm)
dc = ft_dc(sb,kk) / 10.      !median diameter of filter media (cm)
pden = tss_den(sb,kk)     !density of tss particle (g/cm3)
alp = ft_alp(sb,kk)     !filter attachment efficiency (0-1)
vfiltr = ft_dep(sb,kk) / 1000. * tsa * ffsa * por   !actual volume of filter column (m^3)

!! wetting front suction head (mm)
wetfsh = 10. * EXP(6.5309 - 7.32561 * por + 3.809479 * por ** 2 -  &
    0.049837 * por * 100. + 0.001608 * por ** 2 * 100. ** 2 -  &
    0.000799 * 100. ** 2 * por)

!! Get initial values from previous day
qpnd(0) = ft_qpnd(sb,kk)
qsw(0) = ft_qsw(sb,kk)
qin(0) = ft_qin(sb,kk)
qout(0) = ft_qout(sb,kk)
sedpnd = ft_sedpnd(sb,kk)
fc(0) = ft_fc(sb,kk)

DO ii=1,nstep
  qloss = 0.

  qin(ii) = flw(1,ii) * 10. * (sub_ha - tsa / 10000.) +  &
      precipdt(ii) * tsa / 1000.  !m^3
  qout(ii) = qout(ii-1)

  IF (qin(ii)<0.001.AND.qpnd(ii-1)<0.001)THEN

    IF (qsw(ii-1)<0.001) THEN
!No flow
      qout(ii) = 0.
      qloss = 0.
    ELSE
      qout(ii) = ksat * dt * qsw(ii-1) / vfiltr / 1000.* tsa * ffsa !m^3

! outflow control
      IF (sf_ptp(sb,kk)==1) THEN
        phead = (qsw(ii-1)/(tsa*ffsa)/por) * 1000.  !mm
!                If (phead>pdia/2.) then
        qpipe = pipeflow(pdia,phead) * dt *3600. !m^3
!                else
!                   qpipe = qout(ii) * 2. !pipe flow does not affect outflow
!                endif

!recalculate water balance if orifice pipe limits outflow
        IF(qout(ii) > qpipe) qout(ii) = qpipe
      END IF

      qsw(ii) = MAX(0.,qsw(ii - 1) - qout(ii)) ! m^3
    END IF

  ELSE
    qpnd(ii) = qpnd(ii-1) + qin(ii) !m^3
    hpnd = qpnd(ii) / tsa * 1000. !ponding depth on the filter surface, mm

!spillway overflow
    IF (hpnd > ft_h(sb,kk)) THEN
      qloss = MAX(0.,qpnd(ii) - mxvol) !weir outflow
      hpnd = ft_h(sb,kk)
      qpnd(ii) = MAX(0.,qpnd(ii) - qloss)
    END IF
    qpndi = qpnd(ii) + qsw(ii-1)

! estimate unsaturated filter flow
    IF(qsw(ii-1)<0.99*vfiltr) THEN

      IF (qpnd(ii)>0) THEN
        whd = (wetfsh + hpnd)
        tst = ksat
        DO  !green and ampt infiltration
          fc(ii) = fc(ii - 1) + ksat * dt + whd * LOG((tst + whd)  &
              / (fc(ii - 1) + whd))
          IF (ABS(fc(ii) - tst) < 0.001) THEN
            EXIT
          ELSE
            tst = fc(ii)
          END IF
        END DO

!infiltration rate
        f(ii) = ksat * (1 + whd / fc(ii)) !mm/hr

!water infiltrated, m^3
        qfiltr = f(ii) * dt / 1000. * tsa * ffsa

!infiltration limited by the total available water
        IF (qfiltr > qpnd(ii)) THEN
          qfiltr = qpnd(ii)
          qpnd(ii) = 0.
        ELSE
          qpnd(ii) = qpnd(ii) - qfiltr
        END IF
        hpnd = qpnd(ii) / tsa * 1000. !mm

!update soil water
        qsw(ii) = qsw(ii-1) + qfiltr

      ELSE
        f(ii) = 0.
        qfiltr = 0.
      END IF

!soil water no more than saturation
      IF (qsw(ii) > vfiltr) THEN
        hrd = qsw(ii) / vfiltr
        qout(ii) = ksat * hrd * dt / 1000. * tsa * ffsa  !m3
        qsw(ii) = qsw(ii) - qout(ii)
        qpnd(ii) = qpndi - qsw(ii) - qout(ii)
      ELSE
        IF (qpnd(ii)>=qpnd(ii-1).AND.qout(ii-1)<0.001) THEN
!rising hydrograph, no outflow
          qout(ii) = 0.
        ELSE
!receding or continuing hydrograph
          qout(ii) = ksat * qsw(ii) / vfiltr * dt / 1000. * tsa * ffsa  !m3
          IF (qout(ii)>qout(ii-1)) qout(ii) = qout(ii-1)
          qsw(ii) = qsw(ii) - qout(ii)
        END IF
      END IF

    ELSE

!darcy flow when saturated
      qfiltr = ksat * (hpnd + ft_dep(sb,kk)) /  &
          ft_dep(sb,kk) * dt / 1000. * tsa * ffsa
      qout(ii) = qfiltr
      qpnd(ii) = qpnd(ii) - qfiltr

      IF(qpnd(ii)<0) THEN
        qpnd(ii) = 0.
        qsw(ii) = qsw(ii-1)+qin(ii)+qpnd(ii-1)-qfiltr
        qfiltr = qin(ii) + qpnd(ii-1)
      ELSE
        qsw(ii) = vfiltr
        qfiltr = qout(ii)
      END IF
    END IF

!Evapotranspiration loss
    qevap = tsa * sub_etday(sb) / 1000. / 1440. * idt !m^3
    IF(qevap<1E-6) qevap = 0.
    qpnd(ii) = qpnd(ii) - qevap
    IF (qpnd(ii)<0) THEN
      qpnd(ii) = 0.
      qevap = 0.
    END IF


!check if orifice pipe limits outflow in case outflow control exists
    IF (sf_ptp(sb,kk)==1) THEN
      phead = (qsw(ii)/(tsa*ffsa)/por + qpnd(ii)/tsa) * 1000. !mm
      qpipe = pipeflow(pdia,phead) * dt *3600. !m^3

!recalculate water balance if orifice pipe limits outflow
      IF(qout(ii) > qpipe) THEN
        qout(ii) = qpipe ! m^3
        IF (qout(ii)<qin(ii)+qpnd(ii-1)+qsw(ii-1)) THEN
          IF (qout(ii)<qin(ii)+qpnd(ii-1)) THEN
            qfiltr = qout(ii)
          ELSE
            qfiltr = qin(ii) + qpnd(ii-1)
          END IF
          qsw(ii) = qsw(ii-1) + qfiltr - qout(ii)
          qpnd(ii) = qin(ii) + qpnd(ii-1) - qfiltr
        ELSE
          qout(ii) = qin(ii) + qpnd(ii-1) + qsw(ii-1)
          qfiltr = qin(ii) + qpnd(ii-1)
          qsw(ii) = 0.
          qpnd(ii) = 0.
        END IF

        qloss = MAX(0.,qpnd(ii) - mxvol)
        qpnd(ii) = MAX(0.,qpnd(ii) - qloss)
      END IF

    END IF
    qpnde = qpnd(ii) + qsw(ii)

  END IF

! no outlet control: all the infiltration water is added to shallow aquifer recharge for next day\
!  IF (sf_ptp(sb,kk)==0) THEN
!    bmp_recharge(sb) = bmp_recharge(sb) + qout(ii) / (sub_ha*10000.0 - tsa) * 1000.0
!    qout(ii) = 0.0         !effluent from the filter unit (through-flow+overflow), normalized TO subbasin area
!  END IF

! store the flow output
  flw(1,ii) = qin(ii) / (sub_ha *10000. - tsa) * 1000.  !mm
  flw(2,ii) = qout(ii) / (sub_ha*10000.- tsa) *1000.  !mm
  flw(3,ii) = qloss / (sub_ha *10000. - tsa) * 1000.  !mm

!         write(*,'(2i3,20f7.3)') iida, ii, qin(ii),qout(ii),qpnd(ii),
!     &      qsw(ii),qloss
!--------------------------------------------------------------------------------------
! TSS removal
  sloss = 0.; sedrmeff = 0.

! sediment bypass in spillway overflow
  IF (qloss>0) THEN
    IF(qin(ii)>0) THEN
      sedconc = sed(1,ii) / qin(ii) !tons/m3
    ELSE
      sedconc = sedpnd / qpnd(ii) !tons/m3
    END IF
    sloss = sedconc * qloss !tons
    sedpnd = sedpnd + sed(1,ii) - sloss !tons
  END IF

  IF (qpndi>0.001) THEN
    spndconc = sedpnd / qpndi ! tons/m^3
  ELSE
    spndconc = 0.
  END IF

  IF (qout(ii)<0.001)THEN
! no outflow through filter media
    IF (qloss>0.001) THEN
      sed(2,ii) = sloss
    ELSE
      sed(2,ii) = 0.
    END IF
  ELSE
! water temperature, C
    tmpw = sub_hhwtmp(sb,ii)
! water viscosity (g/cm-s) using 3rd order polynomial interpolation
    mu = -3.e-6 * tmpw ** 3 + 0.0006 * tmpw ** 2 - 0.0469 * tmpw + 1.7517
    mu = mu * 1.e-2

!filter flow, cm/s
    qcms = qout(ii) / (tsa * ffsa) * 100. / dt / 3600.

    IF (qcms > 0.001) THEN
!sedimentation efficiency
      effg = (pden - 1) * 981 * dp ** 2 / (18 * mu * qcms)
      IF (effg<0.001) effg = 0.001
!brownian motion efficiency
      effbr = 0.905 * (kb * (tmpw+273.) / (mu * dp * dc * qcms)) ** 0.6667
    ELSE
      effg = 0.999
      effbr = 0.999
    END IF

!interception efficiency
    effl = 1.5 * (dp / dc) ** 2

    IF (effl > 0.999) effl = 0.99
    IF (effg > 0.999) effg = 0.99
    IF (effbr > 0.999) effbr = 0.99

!contact efficiency
    effct = effl + effg + effbr
    IF (effct > 0.999) effct = 0.99

! sediment removal efficiency
    sedrmeff = 1. - EXP(-1.5 * (1. - por) * alp * effct *  &
        ft_dep(sb,kk) / ft_dc(sb,kk))
! sediment removed during the time step
    sed_removed =  spndconc * qout(ii) * sedrmeff
! sediment through filter, tons
    sed(2,ii) = spndconc * qout(ii) * (1. - sedrmeff)

    sedpnd = sedpnd - spndconc * qout(ii) !tons
    sed(3,ii) = sloss
    IF (sedpnd<0) sedpnd = 0.

! write cumulative amount of sediment removed
    ft_sed_cumul(sb,kk) = ft_sed_cumul(sb,kk) + sed_removed !tons
  END IF

!       write(*,'(3i6,20f10.3)') iyr,iida,ii,qin(ii),
!     & qout(ii),qsw(ii),qpnd(ii),qloss,qevap
!       write(*,'(3i5,20f10.3)') iyr,iida,ii,precipdt(ii),qin(ii),
!     & qout(ii),qloss,qpndi,qpnde,qpnd(ii),qsw(ii),f(ii),sed(1,ii)*1000,
!     & sed(2,ii)*1000,sloss*1000
END DO

! store end-of-day values for next day
ft_qpnd(sb,kk) = qpnd(nstep)
ft_qsw(sb,kk) = qsw(nstep)
ft_qin(sb,kk) = qin(nstep)
ft_qout(sb,kk) = qout(nstep)
ft_sedpnd(sb,kk) = sedpnd
ft_fc(sb,kk) = fc(nstep)

RETURN
END SUBROUTINE
