SUBROUTINE readbsn
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the basin input file (.bsn). This file
!!    contains information related to processes modeled or defined at the
!!    watershed level

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adj_pkr     |none          |peak rate adjustment factor in the subbasin.
!!                               |Used in the MUSLE equation to account for
!!                               |impact of peak flow on erosion.
!!    bact_swf    |none          |fraction of manure containing active colony
!!                               |forming units (cfu)
!!    bactkdq     |none          |Bacteria soil partitioning coefficient.
!!                               |Ratio of solution bacteria in surface layer
!!                               |to solution bacteria in runoff
!!                               |soluble and sorbed phase in surface runoff.
!!    bactminlp   |# cfu/m^2     |Threshold detection level for less persistent
!!                               |bacteria
!!                               |when bacteria levels drop to this amount the
!!                               |model considers bacteria in the soil to be
!!                               |insignificant and sets the levels to zero
!!    bactminp    |# cfu/m^2     |Threshold detection level for persistent
!!                               |bacteria
!!                               |when bacteria levels drop to this amount the
!!                               |model considers bacteria in the soil to be
!!                               |insignificant and sets the levels to zero
!!    bactmx      |none          |bacteria percolation coefficient
!!                               |Ratio of solution bacteria in surface layer
!!                               |to solution bacteria in percolate
!!    cdn         |none          |denitrification exponential rate coefficient
!!    cmn         |none          |rate factor for humus mineralization on
!!                               |active organic N
!!    cncoef      |none          |plant ET curve number coefficient
!!    cnfroz      |              |Drainge coefficient (mm day -1)
!!    depimp_bsn  |mm            |depth to impervious layer. Used to model
!!                               |perched water tables in all HRUs in watershed
!!    drain_co_bsn |mm-day-1     |Drainage coeffcient (range 10.0 - 51.0)
!!    ddrain_bsn  |mm            |depth to the sub-surface drain
!!    dorm_hr     |hours         |time threshold used to define dormant
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    esco(:)     |none          |soil evaporation compensation factor (0-1)
!!    evlai       |none          |leaf area index at which no evaporation
!!                               |occurs.  This variable is used in ponded HRUs
!!                               |where evaporation from the water surface is
!!                               |restricted by the plant canopy cover. Evapor-
!!                               |ation from the water surface equals potential
!!                               |ET when LAI = 0 and decreased linearly to O
!!                               |when LAI = EVLAI
!!    evrch       |none          |Reach evaporation adjustment factor.
!!                               |Evaporation from the reach is multiplied by
!!                               |EVRCH. This variable was created to limit the
!!                               |evaporation predicted in arid regions.
!!    ffcb        |none          |initial soil water content expressed as a
!!                               |fraction of field capacity
!!    fixco       |none          |nitrogen fixation coefficient
!!    gdrain      |hours         |drain tile lag time
!!    icfac       |              | icfac = 0 for C-factor calculation using
!!                                  Cmin (as described in manual)
!!                                       = 1 for new C-factor calculation
!!                                  from RUSLE (no minimum needed)
!!    icn         |none          |CN method flag:
!!                               |(for testing alternative method)
!!                               |0 use traditional SWAT method which bases
!!                               |  CN on soil moisture
!!                               |1 use alternative method which bases CN on
!!                               |  plant ET
!!    icrk        |none          |crack flow code
!!                               |1: compute flow in cracks
!!    ideg        |none          |channel degredation code
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!    ipet        |none          |code for potential ET method
!!                               |0 Priestley-Taylor method
!!                               |1 Penman/Monteith method
!!                               |2 Hargreaves method
!!                               |3 read in daily potential ET data
!!    irte        |none          |water routing method:
!!                               |0 variable storage method
!!                               |1 Muskingum method
!!    irtpest     |none          |number of pesticide to be routed through the
!!                               |watershed
!!    ised_det    |none          |max half-hour rainfall fraction calc option:
!!                               |0 generate max half-hour rainfall fraction from
!!                               |  triangular distribution
!!                               |1 use monthly mean max half-hour rainfall
!!                               |  fraction
!!    isubwq      |none          |subbasin water quality code
!!                               |0 do not calculate algae/CBOD
!!                               |1 calculate algae/CBOD
!! drainmod tile equations   01/2006
!!    itdrn       |none          |tile drainage equations flag/code
!!                               |1 simulate tile flow using subroutine drains(wt_shall)
!!                               |0 simulate tile flow using subroutine origtile(wt_shall,d)
!!    iwtdn       |none          |water table depth algorithms flag/code
!!                               |1 simulate wt_shall using subroutine new water table depth routine
!!                               |0 simulate wt_shall using subroutine original water table depth routine
!!    ismax       |none          |maximum depressional storage selection flag/code
!!                               |1 dynamic stmaxd computed as a function of random roughness and rain intensity
!!                               |by depstor.f
!!                               |0 static stmaxd read from .bsn for the global value or .sdr for specific hrus
!! drainmod tile equations   01/2006
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality
!!                               |   (QUAL2E & pesticide transformations)
!!    latksatf_bsn |             |Multiplication factor to determine lateral ksat from SWAT ksat input value for HRU
!!                                  (range 0.01 - 4.0)
!!    msk_co1     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at bankfull depth (phi(10,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_co2     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at 0.1 bankfull depth (phi(13,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_x       |none          |weighting factor controling relative
!!                               |importance of inflow rate and outflow rate
!!                               |in determining storage on reach
!!    nfixmx      |kg/ha         |maximum daily n-fixation
!!    n_updis     |none          |nitrogen uptake distribution parameter
!!                               |This parameter controls the amount of
!!                               |nitrogen removed from the different soil layer
!!                               |layers by the plant. In particular, this
!!                               |parameter allows the amount of nitrogen
!!                               |removed from the surface layer via plant
!!                               |uptake to be controlled. While the relation-
!!                               |ship between UBN and N removed from the
!!                               |surface layer is affected by the depth of the
!!                               |soil profile, in general, as UBN increases
!!                               |the amount of N removed from the surface
!!                               |layer relative to the amount removed from the
!!                               |entire profile increases
!!    nactfr      |none          |nitrogen active pool fraction. The fraction of
!!                               |organic nitrogen in the active pool.
!!    nperco      |none          |nitrate percolation coefficient (0-1)
!!                               |0:concentration of nitrate in surface runoff
!!                               |  is zero
!!                               |1:percolate has same concentration of nitrate
!!                               |  as surface runoff
!!    pc_bsn      |mm h-1        |Pump capacity (def val = 1.042 mm h-1 or 25 mm day-1)
!!    p_updis     |none          |phosphorus uptake distribution parameter
!!                               |This parameter controls the amount of
!!                               |phosphorus removed from the different soil
!!                               |layers by the plant. In particular, this
!!                               |parameter allows the amount of phosphorus
!!                               |removed from the surface layer via plant
!!                               |uptake to be controlled. While the relation-
!!                               |ship between UBP and P uptake from the
!!                               |surface layer is affected by the depth of the
!!                               |soil profile, in general, as UBP increases
!!                               |the amount of P removed from the surface
!!                               |layer relative to the amount removed from the
!!                               |entire profile increases
!!    percop      |none          |pesticide percolation coefficient (0-1)
!!                               |0: concentration of pesticide in surface
!!                               |   runoff is zero
!!                               |1: percolate has same concentration of
!!                               |   pesticide as surface runoff
!!    petfile     |NA            |potential ET file name (.pet)
!!    phoskd      |none          |Phosphorus soil partitioning coefficient
!!                               |Ratio of soluble phosphorus in surface layer
!!                               |to soluble phosphorus in runoff
!!    pperco      |none          |phosphorus percolation coefficient
!!                               |ratio of soluble phosphorus in surface
!!                               |to soluble phosphorus in percolate
!!    prf_bsn     |none          |Basinwide peak rate adjustment factor for sediment
!!                               |routing in the channel. Allows impact of
!!                               |peak flow rate on sediment routing and
!!                               |channel reshaping to be taken into account.
!!    psp         |none          |Phosphorus availibility index. The fraction
!!                               |of fertilizer P remaining in labile pool
!!                               |after initial rapid phase of P sorption.
!!    rcn_sub_bsn |mg/kg         |Concentration of nitrogen in the rainfall
!!    re_bsn      |mm            |Effective radius of drains (range 3.0 - 40.0)
!!    res_stlr_co |none          |reservoir sediment settling coefficient
!!    rsd_covco   |              |residue cover factor for computing frac of cover
!!    rsdco       |none          |residue decomposition coefficient
!!                               |The fraction of residue which will decompose
!!                               |in a day assuming optimal moisture,
!!                               |temperature, C:N ratio, and C:P ratio
!!    sdnco       |none          |denitrification threshold:  fraction of field
!!                               | capacity triggering denitrification
!!    sftmp       |deg C         |Snowfall temperature
!!                               |Mean air temperature at which precipitation
!!                               |is equally likely to be rain as snow/freezing
!!                               |rain.
!!    sdrain_bsn  |mm            |Distance bewtween two drain or tile tubes (range 7600.0 - 30000.0)
!!    sstmaxd(:)  |mm            |static maximum depressional storage; read from .sdr
!----------------------------retention parameter adjustment factor D. Moriasi 4/8/2014
!!    r2adj_bsn   |none          |basinwide retention parameter adjustment factor (greater than 1)! D. Moriasi 4/8/2014
!!    smfmn       |mm/deg C/day  |Minimum melt rate for snow during year (Dec.
!!    smfmn       |mm/deg C/day  |Minimum melt rate for snow during year (Dec.
!!                               |21) where deg C refers to the air temperature.
!!    smfmx       |mm/deg C/day  |Maximum melt rate for snow during year (June
!!                               |21) where deg C refers to the air temperature.
!!                               |SMFMX and SMFMN allow the rate of snow melt
!!                               |to vary through the year. These parameters
!!                               |are accounting for the impact of soil
!!                               |temperature on snow melt.
!!    smtmp       |deg C         |Snow melt base temperature
!!                               |Mean air temperature at which snow melt will
!!                               |occur.
!!    smxco       |              |adjustment factor for max curve number s factor (0-1)
!!    sno50cov    |none          |Fraction of SNOCOVMX that corresponds to 50%
!!                               |snow cover. SWAT assumes a nonlinear relation-
!!                               |ship between snow water and snow cover.
!!    snocov1     |none          |1st shape parameter for snow cover equation
!!                               |This parameter is determined by solving the
!!                               |equation for 50% snow cover
!!    snocov2     |none          |2nd shape parameter for snow cover equation
!!                               |This parameter is determined by solving the
!!                               |equation for 95% snow cover
!!    snocovmx    |mm H2O        |Minimum snow water content that corresponds to
!!                               |100% snow cover. If the snow water content is
!!                               |less than SNOCOVMX, then a certain percentage
!!                               |of the ground will be bare.
!!    spcon       |none          |linear parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    spexp       |none          |exponent parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    surlag      |days          |Surface runoff lag time.
!!                               |This parameter is needed in subbasins where
!!                               |the time of concentration is greater than 1
!!                               |day. SURLAG is used to create a "storage" for
!!                               |surface runoff to allow the runoff to take
!!                               |longer than 1 day to reach the subbasin outlet
!!    tb_adj      |none          |adjustment factor for subdaily unit hydrograph
!!                               |basetime
!!    tdrain_bsn  |hours         |time to drain soil to field capacity
!!    thbact      |none          |temperature adjustment factor for bacteria
!!                               |die-off/growth
!!    timp        |none          |Snow pack temperature lag factor (0-1)
!!                               |1 = no lag (snow pack temp=current day air
!!                               |temp) as the lag factor goes to zero, the snow
!!                               |pack's temperature will be less influenced by
!!                               |the current day's air temperature
!!    trnsrch     |none          |fraction of transmission losses from main
!!                               |channel that enter deep aquifer
!!    ubw         |none          |water uptake distribution parameter
!!                               |This parameter controls the amount of
!!                               |water removed from the different soil layers
!!                               |by the plant. In particular, this parameter
!!                               |allows the amount of water removed from
!!                               |the surface layer via plant uptake to be
!!                               |controlled. While the relationship between
!!                               |UBW and H2O removed from the surface layer is
!!                               |affected by the depth of the soil profile, in
!!                               |general, as UBW increases the amount of water
!!                               |removed from the surface layer relative to the
!!                               |amount removed from the entire profile
!!                               |increases
!!    uobn        |none          |nitrogen uptake normalization parameter
!!                               |This variable normalizes the nitrogen uptake
!!                               |so that the model can easily verify that
!!                               |upake from the different soil layers sums to
!!                               |1.0
!!    uobp        |none          |phosphorus uptake normalization parameter
!!                               |This variable normalizes the phosphorus uptake
!!                               |so that the model can easily verify that
!!                               |uptake from the different soil layers sums to
!!                               |1.0
!!    uobw        |none          |water uptake normalization parameter
!!                               |This variable normalizes the water uptake so
!!                               |that the model can easily verify that uptake
!!                               |from the different soil layers sums to 1.0
!!    wdlpf       |1/day         |Die-off factor for less persistent bacteria on
!!                               |foliage.
!!    wdlpq       |1/day         |Die-off factor for less persistent bacteria in
!!                               |soil solution.
!!    wdlprch     |1/day         |Die-off factor for less persistent bacteria
!!                               |in streams
!!    wdlpres     |1/day         |Die-off factor for less persistent bacteria
!!                               |in reservoirs
!!    wdlps       |1/day         |Die-off factor for less persistent bacteria
!!                               |absorbed to soil particles.
!!    wdpf        |1/day         |Die-off factor for persistent bacteria on
!!                               |foliage.
!!    wdpq        |1/day         |Die-off factor for persistent bacteria in
!!                               |soil solution.
!!    wdprch      |1/day         |Die-off factor for persistent bacteria in
!!                               |streams
!!    wdpres      |1/day         |Die-off factor for persistent bacteria in
!!                               |reservoirs
!!    wdps        |1/day         |Die-off factor for persistent bacteria
!!                               |adsorbed to soil particles.
!!    wglpf       |1/day         |Growth factor for less persistent bacteria on
!!                               |foliage
!!    wglpq       |1/day         |Growth factor for less persistent bacteria in
!!                               |soil solution.
!!    wglps       |1/day         |Growth factor for less persistent bacteria
!!                               |adsorbed to soil particles.
!!    wgpf        |1/day         |Growth factor for persistent bacteria on
!!                               |foliage.
!!    wgpq        |1/day         |Growth factor for persistent bacteria in soil
!!                               |solution.
!!    wgps        |1/day         |Growth factor for persistent bacteria
!!                               |adsorbed to soil particles.
!!    wof_lp      |none          |Wash off fraction for less persistent
!!                               |bacteria on foliage during a rainfall event
!!    wof_p       |none          |Wash off fraction for persistent bacteria on
!!                               |foliage during a rainfall event
!!    wlpq20      |1/day         |Overall rate change for less persistent
!!                               |bacteria in soil solution.
!!    wlps20      |1/day         |Overall rate change for less persistent
!!                               |bacteria adsorbed to soil particles.
!!    wp20lp_plt  |1/day         |Overall rate change for less persistent bacteria
!!                               |on foliage
!!    wp20p_plt   |1/day         |Overall rate change for persistent bacteria on
!!                               |foliage
!!    wpq20       |1/day         |Overall rate change for persistent bacteria in
!!                               |soil solution.
!!    wps20       |1/day         |Overall rate change for persistent bacteria
!!                               |adsorbed to soil particles.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag (=-1 if eof, else =0)
!!    epcobsn     |none          |plant water uptake compensation factor (0-1)
!!    escobsn     |none          |soil evaporation compensation factor (0-1)
!!    r2adjbsn    |none          |retention parameter adjustment factor (=>1) !D.Moriasi 4/8/2014
!!    titldum     |NA            |title line for .bsn file, not used
!!!    wwqfile     |NA            |name of watershed water quality file (.wwq)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: ascrv

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
CHARACTER (LEN=130) :: tlu
CHARACTER (LEN=13) :: wwqfile
INTEGER :: eof, numlu
REAL :: escobsn, epcobsn
REAL :: r2adjbsn  !D. Moriasi 4/8/2014

!!    initialize variables
eof = 0
escobsn = 0.
epcobsn = 0.
r2adjbsn = 0.  !D. Moriasi 4/8/2014
wwqfile = ""
numlu=1

!! read basin parameters
DO
  READ (103,1000) titldum
  READ (103,1000) titldum
  READ (103,1000) titldum
  READ (103,*) sftmp
  READ (103,*) smtmp
  READ (103,*) smfmx
  READ (103,*) smfmn
  READ (103,*) timp
  READ (103,*) snocovmx
  READ (103,*) sno50cov
  READ (103,*) ipet
  READ (103,1000) petfile
  READ (103,*) escobsn
  READ (103,*) epcobsn
  READ (103,*) evlai
  READ (103,*) ffcb
  READ (103,1000) titldum
  READ (103,*) ievent
  READ (103,*) icrk
  READ (103,*) surlag_bsn
  READ (103,*) adj_pkr
  READ (103,*) prf_bsn
  READ (103,*) spcon
  READ (103,*) spexp
  READ (103,1000) titldum
  READ (103,*) rcn_sub_bsn
  READ (103,*) cmn
  READ (103,*) n_updis
  READ (103,*) p_updis
  READ (103,*) nperco
  READ (103,*) pperco
  READ (103,*) phoskd
  READ (103,*) psp
  READ (103,*) rsdco
  READ (103,1000) titldum
  READ (103,*) percop
  READ (103,1000) titldum
  READ (103,*) isubwq
  READ (103,1000) titldum
  READ (103,*) wdpq
  READ (103,*) wgpq
  READ (103,*) wdlpq
  READ (103,*) wglpq
  READ (103,*) wdps
  READ (103,*) wgps
  READ (103,*) wdlps
  READ (103,*) wglps
  READ (103,*) bactkdq
  READ (103,*) thbact
  READ (103,*) wof_p
  READ (103,*) wof_lp
  READ (103,*) wdpf
  READ (103,*) wgpf
  READ (103,*) wdlpf
  READ (103,*) wglpf
  READ (103,1001) ised_det
  READ (103,1000) titldum
  READ (103,*) irte
  READ (103,*) msk_co1
  READ (103,*) msk_co2
  READ (103,*) msk_x
  READ (103,*) ideg
  READ (103,*) iwq
  READ (103,1000) wwqfile
  READ (103,*) trnsrch
  READ (103,*) evrch
  READ (103,*) irtpest
  READ (103,*) icn
  READ (103,*) cncoef
  READ (103,*) cdn
  READ (103,*) sdnco
  READ (103,*) bact_swf
  READ (103,*,IOSTAT=eof) bactmx
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) bactminlp
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) bactminp
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) wdlprch
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) wdprch
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) wdlpres
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) wdpres
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) tb_adj
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) depimp_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ddrain_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) tdrain_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) gdrain_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) cn_froz
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) dorm_hr
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) smxco
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) fixco
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) nfixmx
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) anion_excl_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_onco_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_opco_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) hlife_ngw_bsn
  IF (eof < 0) EXIT
  READ (103,1000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) bc1_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) bc2_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) bc3_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) bc4_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) decr_min
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) icfac
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) rsd_covco
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) vcrit
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) cswat
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) res_stlr_co
  IF (eof < 0) EXIT
!     following reads moved to end of .bsn file
!     read (103,*,iostat=eof) sol_p_model  !! if = 0 use new soil P model
!     if (eof < 0) exit
  READ (103,*,IOSTAT=eof) bf_flg
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) iuh
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) uhalpha
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) titldum
  READ (103,'(a130)') tlu
  DO ii=3,len_trim(tlu)
    IF ((tlu(ii:ii) == ','.AND.tlu(ii-1:ii-1) /= ',').OR.  &
          (tlu(ii:ii) == ' '.AND.tlu(ii-1:ii-1) /= ' ')) THEN
      numlu = numlu + 1
    END IF
  END DO
  IF (len_trim(tlu) <= 3) numlu = 0
  BACKSPACE(103)
  READ (103,*) (lu_nodrain(kk), kk=1,numlu)
  
  
!!   subdaily erosion modeling by Jaehak Jeong
  READ (103,*,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) eros_spl
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) rill_mult
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) eros_expo
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) sed_ch
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) c_factor
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_d50
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) sig_g
  IF (eof < 0) EXIT
!!    Drainmod input variables - 01/2006
  READ (103,*,IOSTAT=eof) re_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) sdrain_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) drain_co_bsn
  IF (eof < 0) EXIT
!!    Drainmod input variables - 01/2006
  READ (103,*,IOSTAT=eof) pc_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) latksatf_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) itdrn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) iwtdn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) sol_p_model  !! if = 0  use new soil P model
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) iabstr
  IF (eof < 0) EXIT
!     iatmodep = 0 - average annual = 1 - monthly
  READ (103,*,IOSTAT=eof) iatmodep
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) r2adj_bsn ! Modified by D. Moriasi 4/8/2014
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) sstmaxd_bsn
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ismax
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) iroutunit
  IF (eof < 0) EXIT
  EXIT
!!    Drainmod input variables - 01/2006
END DO

!!    copy global values to local HRUs
esco = escobsn
epco = epcobsn

!!    set default values for undefined parameters
!!-------------------OGXinSWAT Begin------------------------------
!!  Set the global step parameters
IF (ievent == 1) THEN
  nstep = 24    !number of time steps each day
  idt=60        !minutes in a time step
END IF
!!--------------------End--------------------------------

IF (r2adj_bsn < 1.e-6) r2adj_bsn = 1.
IF (drain_co_bsn < 1.e-6) drain_co_bsn = 10.  &
!!Pa ameter variables added D. Moriasi 4/8/2014
IF (sstmaxd_bsn < 1.e-6) sstmaxd_bsn = 20.  &
!!- --------------------------------------------------------------
IF (res_stlr_co < 1.e-6) res_stlr_co = .184
IF (depimp_bsn < 1.e-6) depimp_bsn = 6000.
IF (bact_swf < 1.e-6) bact_swf = 0.15
IF (adj_pkr <= 0.) adj_pkr = 1.
IF (spcon <= 0.) spcon = .0001
IF (spexp <= 0.) spexp = 1.0
IF (prf_bsn <= 0.) prf_bsn = 1.0
IF (percop <= 0.) percop = .5
IF (n_updis <= 0.) n_updis = 20.
IF (p_updis <= 0.) p_updis = 20.
IF (nperco <= 0.) nperco = .20
IF (pperco <= 0.) pperco = 10.
IF (rsdco <= 0.) rsdco = .05
IF (phoskd <= 0.) phoskd = 175.
IF (psp <= 0.) psp = 0.4
IF (cmn <= 0.) cmn = .0003
IF (smfmx <= 0.) smfmx = 4.5
IF (smfmn <= 0.) smfmn = 4.5
IF (timp <= 0.) timp = 1.0
IF (snocovmx <= 0.) snocovmx = 1.0
IF (sno50cov <= 0.) sno50cov = .5
IF (surlag_bsn <= 0.) surlag_bsn = 4.
IF (evrch <= 0.) evrch = 0.6
IF (bactkdq <= 0.) bactkdq = 75.
IF (thbact <= 0.) thbact = 1.07
IF (msk_x <= 0.) msk_x = 0.2
IF (msk_co1 <= 0. .AND. msk_co2 <= 0.) THEN
  msk_co1 = 0.75
  msk_co2 = 0.25
END IF

IF (evlai <= 0.) evlai = 3.0
IF (cncoef <= 0.) cncoef = 1.0
IF (cdn <= 0.) cdn = 1.4
IF (sdnco <= 0.) sdnco = 1.30
IF (bactmx <= 0.) bactmx = 10.
IF (bactminlp <= 0.) bactminlp = .0
IF (bactminp <= 0.) bactminp = 0.
IF (cn_froz <= 0.) cn_froz = .000862
IF (smxco <= 0.) smxco = 1.0
IF (fixco <= 0.) fixco = 0.5
IF (nfixmx <= 0.) nfixmx = 20.0

!!    mike van liew additions for basins.bsn
IF (anion_excl_bsn <= 1.e-6) anion_excl_bsn = 0.2
IF (ch_onco_bsn <= 1.e-6) ch_onco_bsn = 0.0
IF (ch_opco_bsn <= 1.e-6) ch_opco_bsn = 0.0
IF (hlife_ngw_bsn <= 1.e-6) hlife_ngw_bsn = 5.0
IF (rcn_sub_bsn <= 1.e-6) rcn_sub_bsn = 1.0
IF (bc1_bsn <= 1.e-6) bc1_bsn = 0.1
IF (bc2_bsn <= 1.e-6) bc2_bsn = 0.1
IF (bc3_bsn <= 1.e-6) bc3_bsn = 0.02
IF (bc4_bsn <= 1.e-6) bc4_bsn = 0.35
IF (decr_min <= 1.e-6) decr_min = 0.01
!!    mike van liew additions for basins.bsn

! check parameter values for urban project jaehak 9/15/09
IF(iuh/=1.AND.iuh/=2) THEN
  iuh = 1
END IF
IF(bf_flg>1.OR.bf_flg<0) THEN
  WRITE(*,*) 'The range of BFLO_DIST in bsn file should be 0-1'
!  stop
END IF
IF(sed_ch>2) THEN
  WRITE(*,*) 'Error in choosing channel erosion model:  &
      0-Bagnold, 1-Brownlie, 2-Yang'
  WRITE(*,*) 'Check *.bsn file to correct the error'
!  stop
END IF
IF (icfac <= 0) icfac = 0
IF (rsd_covco <= 1.e-6) rsd_covco = 0.3


CALL caps(petfile)
CALL caps(wwqfile)
OPEN (101,FILE=wwqfile)

!!    calculate normalization parameters for water, nitrogen, and
!!    phosphorus uptake
uobn = 0.0
uobp = 0.0
uobw = 0.0
ubw = 10.0       !! the uptake distribution for water is hardwired
!! users are not allowed to modify the water
!! water uptake distribution
uobw = 1. - EXP(-ubw)
uobn = 1. - EXP(-n_updis)
uobp = 1. - EXP(-p_updis)

!!    determine the shape parameters for the equation which describes area of
!!    snow cover as a function of amount of snow
CALL ascrv(.5,.95,sno50cov,.95,snocov1,snocov2)

!!    calculate additional bacteria parameters
wp20p_plt = wdpf - wgpf
wp20lp_plt = wdlpf - wglpf
wpq20 = wdpq - wgpq
wlpq20 = wdlpq - wglpq
wps20 = wdps - wgps
wlps20 = wdlps - wglps

!!    initialize variables (may make these .bsn inputs for user adjustment
!!    at some future time)
nactfr = 0.02
abstinit = iabstr


CLOSE (103)
!!add by zhang
!!=====================
IF (cswat == 2) THEN
  OPEN (98,FILE="cswat_profile.txt",RECL=356)
  WRITE (98,5102) 'year','day','lay','hru',  &
      'sol_mass','sol_cmass','sol_nmass','sol_LS',  &
      'sol_LM','sol_LSC','sol_LMC','sol_HSC',  &
      'sol_HPC','sol_BMC','sol_LSN','sol_LMN',  &
      'sol_HPN','sol_HSN','sol_BMN','sol_no3',  &
      'sol_fop','sol_orgp','sol_actp','sol_stap', 'sol_solp'
  
  OPEN (100,FILE="cswat_daily.txt",RECL=786)
  WRITE (100,5104) 'year','day','hru','rsdc','sedc',  &
      'percc','latc','emitc','grainc','surfq_c',  &
      'stoverc','NPPC','foc','rspc','tot_mass','tot_cmass','tot_nmass',  &
      'tot_LSC','tot_LMC','tot_HSC','tot_HPC','tot_BMC','Biom_C','rwtf',  &
      'tot_no3_nh3','wdntl',  &
      'ET','Tillfactor','SW1','SW2','SW3','SW4','SW5','SW6','SW7','SW8',  &
      'SW9','SW10','SW11',  &
      'WFSC1','WFSC2','WFSC3','WFSC4','WFSC5','WFSC6','WFSC7','WFSC8',  &
      'WFSC9','WFSC10','WFSC11'
END IF
!!add by zhang
!!=====================

! open (111, file="final_n_balance.txt")
! open (112, file="final_yields.txt")
!! carbon output ends


RETURN
1000 FORMAT (a)
1001 FORMAT (i4)
5102 FORMAT (3A5,30A15)
5104 FORMAT (a4,a4,a8,48A16)
END SUBROUTINE readbsn
