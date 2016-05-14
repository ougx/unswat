SUBROUTINE det_pond
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    the purpose of this program is to read in data from the detention pond
!!    input file (.dtp) and perform computations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_weirtype(:)|none          |Type of weir: 1 rectangular and 2 circular
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_wdratio(:,:)|none         |Width depth ratio (rectangular wier) at different stages
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of circular wier at different stages
!!    dtp_retperd(:,:)|years        |Return period at different stages
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    dtp_cdis(:,:)  |none          |coeffieicne of discharge at different stages
!!    hhvaroute(2,:,:) |m^3 H2O     |water
!!    hhvaroute(3,:,:) |metric tons |sediment or suspended solid load
!!    i_mo           |none          |current month of simulation
!!    sub_subp_dt(:,:)  |mm H2O      |precipitation for time step in subbasin
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
REAL :: qin,qout,qpnd,sedin,sedout,sedpnd,spndconc,qdepth,  &
    watdepact,qstage,backup_length,seep_sa,evap_sa,pcp_vol,  &
    evap_vol,seep_vol,warea,pi,qovmax,qaddon,depaddon

pi = 3.14159
sb = inum1
qout = 0.; sedout = 0.; depaddon = 0.

IF (iyr<dtp_iyr(sb) .OR. (iyr==dtp_iyr(sb) .AND. i_mo<dtp_imo(sb))) THEN
  RETURN
END IF

!! Get initial values from previous day
qpnd = dtp_ivol(sb) !m^3
sedpnd = dtp_ised(sb) !tons

!! Storage capacity under addon
qaddon = dtp_addon(sb,1)**3./(3.*dtp_lwratio(sb)*ch_s(2,sb)**2.) !m3 Note: V = d^3 / (3*r*s^2) |modify by j. osorio (3/19/2013)

!! iterate for subdaily flow/sediment routing
DO ii=1,nstep
  
  qout = 0.; qovmax = 0; depaddon = 0.
  qin = hhvaroute(2,ihout,ii) + qpnd !m^3
  sedin = hhvaroute(3,ihout,ii) + sedpnd  !tons
  IF (qin>1E-6) THEN
    spndconc = sedin / qin !initial sed conc, tons/m3
  ELSE
    cycle
  END IF
  
!! Estimate water depth
  qdepth = (3.*qin*dtp_lwratio(sb)*ch_s(2,sb)**2)**0.33333 !! Note: h = (3*V*R*S^2)^3 |modify by j. osorio (3/19/2013)
  
!! skip to next time step if no ponding occurs
  IF (qdepth<=0.0001) cycle
  
  IF (dtp_stagdis(sb)==0) THEN
!! Calculate weir outflow
    DO k = 1, dtp_numstage(sb)
      qstage = 0.
      
!! calculate weir discharge
      
      IF (dtp_weirtype(sb,k)==2) THEN
!! Circular weir
        dtp_depweir(sb,k) = dtp_diaweir(sb,k) + dtp_addon(sb,k)
        
        IF (qdepth>dtp_depweir(sb,k)) THEN
!! Fully submerged
          qdepth = qdepth - dtp_depweir(sb,k)
          watdepact = qdepth + dtp_diaweir(sb,k) / 2
          warea = 3.14159 * dtp_diaweir(sb,k) ** 2 / 4.
          
!! orifice equation
          qstage = dtp_cdis(sb,k) * 0.6 * warea *  &
              SQRT(19.6 * watdepact) !m3/s/unit
          qstage = qstage * dtp_numweir(sb) * 60. * idt !m^3
        ELSE
!! Partially submerged
          watdepact = MAX(qdepth - dtp_addon(sb,k),0.)
          dtp_wrwid(sb,k) = dtp_diaweir(sb,k) * 0.667
          
!! weir/orifice discharge
          qstage = dtp_cdis(sb,k) * 1.84 * dtp_wrwid(sb,k) *  &
              watdepact ** 1.5 !m3/s
          qstage = qstage * dtp_numweir(sb) * 60. * idt !m^3
        END IF
        
      ELSE
!! Rectangular weir
        watdepact = MAX(qdepth - dtp_addon(sb,k),0.)
        
!! Estimate weir/orifice discharge
        qstage = dtp_cdis(sb,k) * 1.84 * dtp_wrwid(sb,k) *  &
            watdepact ** 1.5 !m3/s     The Bureau of Reclamation, in their Water Measurement manual, si units
        qstage = qstage * dtp_numweir(sb) * 60. * idt !m^3
        
      END IF
      
      qout = qout + qstage
    END DO
    
!Limit total outflow amount less than available water above addon
    IF(qout>qin-qaddon) qout = MAX(qin - qaddon,0.)
    
!! Flow over the emergency weir
    watdepact = qdepth - (dtp_depweir(sb,1) + dtp_addon(sb,1))
    IF (dtp_weirtype(sb,k)==1 .AND. watdepact>0.) THEN
      qstage = dtp_cdis(sb,k) * 1.84 *  &
          dtp_totwrwid(sb) * (watdepact ** 1.5) * 60. * idt !m3/s
      qout = qout + qstage
    END IF
    
    
  ELSE
!! Use stage-discharge relationship if available
    IF (dtp_stagdis(sb)==1) THEN
      select case(dtp_reltype(sb))
      case(1) !! 1 is exponential function
      qout = dtp_coef1(sb) * EXP(dtp_expont(sb) * qdepth) + dtp_intcept(sb)
      case(2) !! 2 is Linear function
      qout = dtp_coef1(sb) * qdepth + dtp_intcept(sb)
      case(3) !! 3 is logarthmic function
      qout = dtp_coef1(sb) * LOG(qdepth) + dtp_intcept(sb)
      case(4) !! 4 is power function
      qout = dtp_coef1(sb) * (qdepth**3) + dtp_coef2(sb) *  &
          (qdepth**2) + dtp_coef3(sb) * qdepth + dtp_intcept(sb)
      case(5)
      qout = dtp_coef1(sb)*(qdepth**dtp_expont(sb))+ dtp_intcept(sb)
    END select
    qout = qout * 60. * idt
  END IF  !! end of stage-discharge calculation
END IF

!! Check mass balance for flow
IF (qout>qin) THEN !no detention occurs
  qout = qin
  qpnd = 0.
ELSE !detention occurs
!! Estimating surface area of water
  backup_length = qdepth / ch_s(2,sb)
  seep_sa = backup_length/dtp_lwratio(sb)  &
      + (4. * dtp_lwratio(sb) * qdepth**2) / 3.      !! Note: SSA = w + (4*l*d^2)/(3*w) |modify by j. osorio (3/20/2013)
  evap_sa = 2. * backup_length**2 / 3. * dtp_lwratio(sb) !! Note: ESA = 2 * w * l / 3 |modify by j. osorio (3/20/2013)
  
!! converting surface area to hectares (ha)
  seep_sa = seep_sa / 10000.0
  evap_sa = evap_sa / 10000.0
  
!! Estimate rainfall, evapotranspiration, and seepage
  pcp_vol  = 10.0 * sub_subp_dt(sb,ii) * evap_sa !m^3
  evap_vol = 10.0 * dtp_evrsv(sb) * pet_day * evap_sa !m^3
  seep_vol = 10.0 * ch_k(2,sb) * seep_sa * idt / 60. !m^3
  
!! Check mass balance for water in the pond
  qpnd = qin + pcp_vol - qout - evap_vol - seep_vol
  IF (qpnd<0) qpnd = 0.
END IF

!! Mass balance for sediment
sedout = spndconc * qout !tons
sedpnd = MAX(0.,sedin - sedout) !tons

!! Store flow/sediment out of the pond at the subbasin outlet
hhvaroute(2,ihout,ii) = MAX(0.,qout)
hhvaroute(3,ihout,ii) = MAX(0.,sedout)


END DO  !! Outermost do loop ends here

! Store end-of-day values for next day
dtp_ivol(sb) = qpnd !m^3
dtp_ised(sb) = sedpnd !tons

END SUBROUTINE
