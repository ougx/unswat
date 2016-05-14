SUBROUTINE bmpinit
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine sets default values for urban bmp parameters

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dtp_onoff(:)   |none          |sub-basin detention pond is associated with
!!    dtp_iy(:)      |none          |year of the simulation that the reservoir
!!                                  |becomes operational
!!    dtp_imo(:)     |none          |month the reservoir becomes operational
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_lwratio(:)   |none          |Ratio of length to width of water back up
!!    dtp_totwrwid(:)|m             |Total constructed width of the detention wall across
!!                                  |the creek
!!    dtp_stagdis(:) |none          |0=use weir/orifice discharge equation to calculate
!!                                  |outflow, 1=use stage-dicharge relationship
!!    dtp_reltype(:) |none          |Equations for Stage-Discharge relationship,1=exponential
!!                                  |function, 2=linear, 3=logarithmic, 4=cubic, 5=power
!!    dtp_intcept(:) |none          |Intercept used in regression equations
!!    dtp_expont(:)  |none          |Exponent used in the exponential equation
!!    dtp_coef1(:)   |none          |Coefficient of 3rd degree in the polynomial equation
!!    dtp_coef2(:)   |none          |Coefficient of 2nd degree in the polynomial equation
!!    dtp_coef3(:)   |none          |Coefficient of 1st degree in the polynomial equation
!!    dtp_dummy1(:)   |none         |Dummy variable, backs up space (Rename for use)
!!    dtp_dummy2(:)   |none         |Dummy variable, backs up space (Rename for use)
!!    dtp_dummy3(:)   |none         |Dummy variable, backs up space (Rename for use)
!!    dtp_weirtype(:,:)|none        |Type of weir: 1=rectangular and 2=circular
!!    dtp_weirdim(:,:)|none         |Weir dimensions, 1=read user input, 0=use model calculation
!!    dtp_wdratio(:,:)|none         |Width depth ratio of rectangular weirs
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of orifice hole at different stages
!!    dtp_addon(:,:)  |m            |The distance between spillway levels
!!    dtp_flowrate(:,:)|m3/sec      |Maximum discharge from each stage of the weir/hole
!!    dtp_cdis(:,:)  |none          |Discharge coeffieicne for weir/orifice flow
!!    dtp_retperd(:,:)|years        |Return period at different stages
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    res_out(:,:,:) |m**3/day      |measured average daily outflow from the
!!                                  |reservoir for the month (needed if IRESCO=1)
!!                                  |(read in as m**3/s and converted to m**3/day)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    k           |none          |counter
!!    lnvol       |none          |variable to hold denominator value
!!    mon         |none          |counter
!!    titldum     |NA            |title line in .det file (not used in program)
!!    resmto      |NA            |name of reservoir outflow file
!!                               |(needed if IDETCO = 2)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
IMPLICIT NONE
INTEGER :: j, k, eof,kk
REAL :: hwq,wqv,sub_ha,bmpfr_sf,bmpfr_ri, qstg, hstg

eof = 0; bmpfr_sf=0.; bmpfr_ri=0.; hstg = 0.; qstg=0.
sub_ha = sub_km(i) * 100.

!! Detention pond
!!----------------
IF (dtp_onoff(i)==1) THEN
  IF (dtp_imo(i)<=0)      dtp_imo(i) = 1
  IF (dtp_iyr(i)<=1000)   dtp_iyr(i) = iyr
  IF (dtp_evrsv(i)<=0)    dtp_evrsv(i) = 0.1
  IF (dtp_lwratio(i)>1)   dtp_lwratio(i) = 1
  IF (dtp_numweir(i)<=0)  dtp_numweir(i) = 1
  IF (dtp_numstage(i)<=0) dtp_numstage(i) = 1
  IF (dtp_numstage(i)>1) THEN
    DO k=2,dtp_numstage(i)
      IF (dtp_weirtype(i,k)==1) dtp_addon(i,k) = 0.
    END DO
  END IF
  
!! Estimating design flow rate if not entered by user
  DO k=1,dtp_numstage(i)
    IF (dtp_flowrate(i,k)<=0.0) THEN
      dtp_flowrate(i,k) = 0.5 * 1000.0 * dtp_pcpret(i,k)  &
          * subdr_km(i) / (idt*60.)
    END IF
    
    IF (dtp_flowrate(i,k)<0) THEN
      PRINT *,"Error.. Could not estimate emergency spillway volume"
      PRINT *,"Please enter necessary data in *.pnd input file"
      PRINT *,"for subbasin : ",i
!!            stop
    END IF
  END DO
  
!! Separate cumulative flow information to individual weir
  DO k=2,dtp_numstage(i)
    dtp_flowrate(i,k) = dtp_flowrate(i,k) / dtp_numweir(i)
  END DO
  
!!Estimate weir dimensions based on existing data
  DO k=1,dtp_numstage(i)
    IF (dtp_weirdim(i,k)==0) THEN  !! Estimating weir dimensions
      IF (dtp_weirtype(i,k)==2) THEN !! choosing weir type
        dtp_diaweir(i,k)=(0.479081 * dtp_flowrate(i,k) / dtp_cdis(i,k))**0.4
      ELSE !rectangular weir
        IF (k==1) THEN
          hstg = dtp_depweir(i,k)
          dtp_wrwid(i,k) = dtp_flowrate(i,k) / (1.84*dtp_cdis(i,k)*hstg**1.5)
        ELSE
          qstg = 0.
          DO j=1,k-1
            hstg = sum(dtp_depweir(i,j:k))
            qstg = dtp_cdis(i,k) * 1.84 * dtp_wrwid(i,j) * hstg ** 1.5 !m3/s
            dtp_flowrate(i,k) = MAX(0.,dtp_flowrate(i,k)-qstg)
            
          END DO
          dtp_wrwid(i,k) = dtp_flowrate(i,k)  &
              / (1.84*dtp_cdis(i,k)*dtp_depweir(i,k)**1.5)
        END IF
      END IF
    ELSE  !! read user-entered data
      IF (dtp_weirtype(i,k)==1) THEN
        dtp_wrwid(i,k) = dtp_wdratio(i,k) * dtp_depweir(i,k)
      END IF
    END IF
  END DO
!! divide rectangular weirs into multiple single stage weirs
  DO k = 2, dtp_numstage(i)
    dtp_addon(i,k) = dtp_addon(i,k-1) + dtp_depweir(i,k-1)
  END DO
  
! weir depth from the top to the bottom of each stage
  DO k = dtp_numstage(i), 2, -1
    dtp_depweir(i,k-1) = dtp_depweir(i,k) + dtp_depweir(i,k-1)
  END DO
END IF

!! Wet pond
!!----------
IF (wtp_onoff(i)==1) THEN
  IF (wtp_imo(i)<=0)      wtp_imo(i) = 1
  IF (wtp_iyr(i)<=1000)   wtp_iyr(i) = iyr
  IF (wtp_k(i) <=0)       wtp_k(i) = 0.01
  IF (wtp_evrsv(i)<=0)    wtp_evrsv(i) = 0.01
  IF (wtp_evrsv(i)>1)     wtp_evrsv(i) = 0.99
  IF (wtp_sdslope(i)<3)   wtp_sdslope(i) = 3. !COA Manual 1.6.6.C
  IF (wtp_lenwdth(i)<2)   wtp_lenwdth(i) = 2. !COA Manual 1.6.6.C
  IF (wtp_pdia(i)<=0)     wtp_pdia(i) = 0.1524 !meter(=6inches), COA Manual 1.6.6
  IF (wtp_plen(i)<=2)     wtp_plen(i) = 2 !meter
  IF (wtp_pmann(i)<=0)    wtp_pmann(i) = 0.012 ! concrete surface
  IF (wtp_ploss(i)<=0)    wtp_ploss(i) = 0
END IF

!fraction runoff that directly enters the channel
IF(num_sf(i)>=1) THEN
  DO kk=1,num_sf(i)
    bmpfr_sf = bmpfr_sf + sf_fr(i,kk)
  END DO
END IF
IF(num_ri(i)>=1) THEN
  DO kk=1,num_ri(i)
    bmpfr_ri = bmpfr_ri + ri_fr(i,kk)
  END DO
END IF
IF (bmpfr_sf>1.OR.bmpfr_ri>1) THEN
  WRITE (*,*) " "
  WRITE (*,*) "Urban BMP Warning!!"
  WRITE (*,*) "In subbasin ", i
  WRITE (*,*) "The fraction runoff draining to urban BMPs"
  WRITE (*,*) " are larger than one, so the fraction values"
  WRITE (*,*) " were automatically reassigned"
  WRITE (*,*) " "
  DO kk=1,num_sf(i)
    sf_fr(i,kk) = sf_fr(i,kk) / bmpfr_sf
  END DO
  DO kk=1,num_sf(i)
    ri_fr(i,kk) = ri_fr(i,kk) / bmpfr_ri
  END DO
  bmpfr_sf = 1.; bmpfr_ri=1.
END IF

!!Retention-Irrigation
!!---------------------
DO k=1,num_ri(i)
! skip the pond that has zero inflow
  IF (ri_fr(i,k)==0) cycle
  
! determine water quality volume for defult pond sizes
!City of Austin Design Guideline 1.6.9.2 Table 1-12
!Retention-Irrigation for Barton Springs Zone
  
  hwq = (1.8 * sub_ha_imp(i) / sub_ha_urb(i) + 0.6) !inches
  wqv = hwq / 12. * sub_ha_urb(i) * ri_fr(i,k) * 107639.104167 !ft3
  
  IF (ri_dim(i,k)==0) THEN
!Determine pond size automatically based on City of Austin's Design Guideline 1.6
    ri_vol(i,k) = wqv * 0.028317 !m3
    ri_dep(i,k)=1.5 !assume 1.5m as default retention pond depth
    ri_sa(i,k) = ri_vol(i,k) / ri_dep(i,k)
    ri_dd (i,k)=60.0 !drawdown time, hr
    ri_k(i,k)=2.5
    ri_evrsv(i,k)=0.6
  ELSE
!Evaluate unit variables provided by user
    IF (ri_sa(i,k)<1.OR.ri_vol(i,k)<1) THEN
!City of Austin Design Guideline 1.6.5
      ri_vol(i,k) = wqv * 0.028317 !m3
      ri_sa(i,k) = ri_vol(i,k) / ri_dep(i,k)
    END IF
    
    ri_dep(i,k) = ri_vol(i,k) / ri_sa(i,k)
    IF (ri_dd(i,k)<=0) ri_dd (i,k)=72.0
    IF (ri_k(i,k) <=0 ) ri_k(i,k)=2.5
    IF (ri_evrsv(i,k) <=0 ) ri_evrsv(i,k)=0.1
    IF (ri_dep(i,k)<=1) ri_dep(i,k)=1.
  END IF
  
! draw down time [number of time step]
  ri_ndt(i,k) = (ri_dd(i,k) - 12.) * 60 / idt  !minus the first dry 12 hours no-pumping
  
! pumping rate that empties the basin in 72 hours with initial 12 hour of no operatrion
  ri_pumpv(i,k) = ri_vol(i,k) / ri_ndt(i,k) !m3/dt
  
  IF (ri_im(i,k)<0.OR.ri_im(i,k)>12) ri_im(i,k) = 0
  IF (ri_iy(i,k)<1000.AND.ri_iy(i,k)>0) ri_iy(i,k) = 0
  IF (ri_iy(i,k)==0) ri_iy(i,k) = iyr
  IF (ri_im(i,k)==0) ri_im(i,k) = 1
  
  WRITE(77779,'(a11,i5)') 'Subbasin #:', i   ! bmp_sedfil.out
  WRITE(77779,'(a46)') ''
  WRITE(77779,'(a10,i5)') 'RI #:', k   ! bmp_sedfil.out
  WRITE(77779,'(a17,f10.1,a4)') 'Total volume =', ri_vol(i,k),'m^3'
  WRITE(77779,'(a17,f10.1,a4)') 'Surface area =', ri_sa(i,k),'m^2'
  WRITE(77779,'(a17,f10.1,a3)') 'Drawdown time =', ri_dd (i,k),'hr'
  WRITE(77779,'(a17)') ''
  
END DO

!!Sedimentation-Filtration
!!---------------------

DO k=1,num_sf(i)
  WRITE(77778,'(a11,i5)') 'Subbasin #:', i   ! bmp_sedfil.out
  WRITE(77778,'(a46)') ''
  WRITE(77778,'(a10,i5)') 'SED-FIL #:', k   ! bmp_sedfil.out
!determine water quality volume for defult pond sizes
!City of Austin Design Guideline 1.6.2
  hwq = (0.5 + sub_ha_imp(i) / sub_ha_urb(i) - 0.2) !inches
  wqv = hwq / 12. * sub_ha_urb(i) * sf_fr(i,k) * 107639.104167 !ft3
  
  IF (sf_dim(i,k)==0) THEN
    WRITE(77778,'(a46)') 'This SED-FIL size is automatically'  &
        // ' estimated based on WQV.'
!Determine pond size automatically based on City of Austin's Design Guideline 1.6
    IF (sf_typ(i,k)==1.OR.sf_typ(i,k)==3) THEN
! full scale or sedimentation basin only
      sp_pvol(i,k) = wqv * 0.028317 !m3
      sp_sa(i,k) = sp_pvol(i,k) / 1.5 !assume 1.5m depth for sed pond
      sp_pd(i,k) = SQRT(4. * 2. * sp_sa(i,k) * 1.5**0.5  &
          / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
      ft_sa(i,k) = wqv/(7.+2.33*4.) * 0.093 !m2
      ft_fsa(i,k) = 1.
      
    ELSE
! partial scale
      ft_sa(i,k) = wqv * 0.028317 !m3
!               wqv/(4.+1.33*4.) * 0.093
      
    END IF
    sp_bpw(i,k) = 10. !m overflow weir width
    ft_pd(i,k) = 1524. !mm
    ft_dep(i,k) = 420. !mm
    ft_h(i,k) = 1200. !mm
    
  ELSE
!Evaluate unit variables given by users
    IF (sf_typ(i,k)>3) sf_typ(i,k) = 1
    IF (sp_sa(i,k)<1) THEN
!City of Austin Design Guideline 1.6.5
      sp_pvol(i,k) = wqv * 0.028317 !m3
      sp_sa(i,k) = sp_pvol(i,k) / 1.5 !assume 1.5m depth for sed pond
    END IF
    IF (sp_pd(i,k)<0.1) sp_pd(i,k) = 152.4 !mm diameter, equivalent to 6in
    IF (ft_h(i,k)<5) ft_h(i,k) = 1200. !mm
    IF (ft_sa(i,k)<1) THEN
      IF (sf_typ(i,k)==1) THEN
        ft_sa(i,k) = wqv/(7.+2.33*ft_h(i,k)/304.8) * 0.093 !m2
        ft_fsa(i,k) = 1.
      ELSE
        ft_sa(i,k) = wqv * 0.028317 !m3
        ft_fsa(i,k) = 1. /(4.+1.33*ft_h(i,k)/304.8)
      END IF
    END IF
  END IF
  
!Outflow control
  IF (sp_qfg(i,k)==0) THEN
    sp_pd(i,k) = SQRT(4. * 2. * sp_sa(i,k) * 1.5**0.5  &
        / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
  END IF
  IF (ft_qfg(i,k)==0) THEN
    ft_pd(i,k) = SQRT(4. * 2. * ft_sa(i,k) * 1.5**0.5  &
        / (0.6 * 172800. * 19.6**0.5) / 3.14159) * 1000. !mm
  END IF
  
!Orifice pipe for sand filter should be equal or larger than
!sedimentation pond outlet pipe for full-type SedFils
  IF (ft_pd(i,k)<sp_pd(i,k)) ft_pd(i,k) = sp_pd(i,k)
  
  IF (ft_dep(i,k)<100) ft_dep(i,k) = 100.
  IF (sf_ptp(i,k)>1) sf_ptp(i,k) = 1
!      if (sf_typ(i,k)==1) sf_ptp(i,k) = 0 removed by Jaehak 2014
  IF (sp_pd(i,k)>254) sp_pd(i,k) = 254. ! max 10inches dia
  IF (sp_pd(i,k)<10) sp_pd(i,k) = 10. ! min 10mm dia
  IF (ft_pd(i,k)>254) ft_pd(i,k) = 254. ! max 10inches
  IF (ft_k(i,k)<1) ft_k(i,k) = 1.
  IF (ft_dp(i,k)<0.0001) ft_dp(i,k) = 0.02
  IF (ft_dc(i,k)<0.01) ft_dc(i,k) = 0.762
  IF (ft_por(i,k)<0.1) ft_por(i,k) = 0.45
  IF (tss_den(i,k)<0.5) tss_den(i,k) = 0.5
  IF (ft_alp(i,k)<0.1) ft_alp(i,k) = 0.1
  IF (sf_im(i,k)<0.OR.sf_im(i,k)>12) sf_im(i,k) = 0
  IF (sf_iy(i,k)<1000.AND.sf_iy(i,k)>0) sf_iy(i,k) = 0
  IF (sf_iy(i,k)==0) sf_iy(i,k) = iyr
  IF (sf_im(i,k)==0) sf_im(i,k) = 1
  IF (ft_fsa(i,k)==0) ft_fsa(i,k) = 0.85
  IF (sf_typ(i,k)==1) ft_fsa(i,k) = 1
  
  
  IF (sf_typ(i,k)==1) THEN
    WRITE(77778,'(a37)') 'Full type sed-fil selected'
  ELSE IF (sf_typ(i,k)==2) THEN
    WRITE(77778,'(a40)') 'Partial type sed-fil selected'
  ELSE
    WRITE(77778,'(a43)') 'Sedimentation pond only selected'
  END IF
  IF (sf_typ(i,k)==1.OR.sf_typ(i,k)==3) THEN
    WRITE(77778,'(a18)') 'Sedimentation pond'
    WRITE(77778,'(a17,f10.1,a4)') 'Total volume =', sp_pvol(i,k),'m^3'
    WRITE(77778,'(a17,f10.1,a4)') 'Surface area =', sp_sa(i,k),'m^2'
    WRITE(77778,'(a17,f10.1,a3)') 'Drain Pipe Dia =', sp_pd(i,k),'mm'
    WRITE(77778,'(a17)') ''
  END IF
  IF (sf_typ(i,k)==1.OR.sf_typ(i,k)==2) THEN
    WRITE(77778,'(a11)') 'Sand Filter'
    WRITE(77778,'(a17,f10.1,a4)') 'Surface area =', ft_sa(i,k),'m^2'
    WRITE(77778,'(a17,f10.1,a3)') 'Max ponding =', ft_h(i,k),'mm'
    WRITE(77778,'(a17,f10.1,a3)') 'Filter depth =', ft_dep(i,k),'mm'
    WRITE(77778,'(a17,f10.1,a3)') 'Drain Pipe Dia =', ft_pd(i,k),'mm'
    WRITE(77778,'(a17)') ''
  END IF
END DO


RETURN
END SUBROUTINE bmpinit
