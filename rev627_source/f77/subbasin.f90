SUBROUTINE subbasin
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the
!!    hydrologic cycle

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_wstr(:)   |none          |water stress factor which triggers auto
!!                                  |irrigation
!!    bio_e(:)       |(kg/ha)/      |biomass-energy ratio
!!                   |     (MJ/m**2)|The potential (unstressed) growth rate per
!!                                  |unit of intercepted photosynthetically
!!                                  |active radiation.
!!    canev          |mm H2O        |amount of water evaporated from canopy
!!                                  |storage
!!    ep_day         |mm H2O        |actual amount of transpiration that occurs
!!                                  |on day in HRU
!!    es_day         |mm H2O        |actual amount of evaporation (soil et) that
!!                                  |occurs on day in HRU
!!    gw_q(:)        |mm H2O        |groundwater contribution to streamflow from
!!                                  |HRU on current day
!!    hru_ra(:)      |MJ/m^2        |solar radiation for the day in HRU
!!    iida           |julian date   |day being simulated (current julian date)
!!    idplt(:)       |none          |land cover code from crop.dat
!!    igro(:)        |none          |land cover status code
!!                                  |0 no land cover currently growing
!!                                  |1 land cover growing
!!    inum1          |none          |subbasin number
!!    imp_trig(:)    |none          |release/impound action code:
!!                                  |0 begin impounding water
!!                                  |1 release impounded water
!!    irrsc(:)       |none          |irrigation source code:
!!                                  |1 divert water from reach
!!                                  |2 divert water from reservoir
!!                                  |3 divert water from shallow aquifer
!!                                  |4 divert water from deep aquifer
!!                                  |5 divert water from source outside
!!                                  |  watershed
!!    iurban(:)      |none          |urban simulation code:
!!                                  |0  no urban sections in HRU
!!                                  |1  urban sections in HRU, simulate using
!!                                  |   USGS regression equations
!!                                  |2  urban sections in HRU, simulate using
!!                                  |   build up/wash off algorithm
!!    latq(:)        |mm H2O        |total lateral flow in soil profile for the
!!                                  |day in HRU
!!    nafert(:)      |none          |sequence number of auto-fert application
!!                                  |within the year
!!    nair(:)        |none          |sequence number of auto-irrigation
!!                                  |application within the year
!!    nfert(:)       |none          |sequence number of fertilizer application
!!                                  |within the year
!!    nirr(:)        |none          |sequence number of irrigation application
!!                                  |within the year
!!    nrelease(:)    |none          |sequence number of impound/release
!!                                  |operation within the year
!!    nro(:)         |none          |sequence number of year in rotation
!!    peakr          |m^3/s         |peak runoff rate
!!    pet_day        |mm H2O        |potential evapotranspiration on current
!!                                  |day in HRU
!!    phuacc(:)      |none          |fraction of plant heat units accumulated
!!    phubase(:)     |heat units    |base zero total heat units (used when no
!!                                  |land cover is growing)
!!                                  |pesticide application occurs
!!    pot_fr(:)      |km2/km2       |fraction of HRU area that drains into
!!                                  |pothole
!!    pot_vol(:)     |m**3 H2O      |current volume of water stored in the
!!                                  |depression/impounded area
!!    precipday      |mm H2O        |precipitation for the day in HRU
!!    qday           |mm H2O        |surface runoff loading to main channel from
!!                                  |HRU for day
!!    qtile          |mm H2O        |drainage tile flow in soil layer for the
!!                                  |day
!!    sci(:)         |none          |retention coefficient for CN method based
!!                                  |on plant ET
!!    sedyld(:)      |metric tons   |soil loss for day in HRU
!!    smx(:)         |none          |retention coefficient for CN method based
!!                                  |on soil moisture
!!    surfq(:)       |mm H2O        |surface runoff generated on day in HRU
!!    tmn(:)         |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)       |deg C         |average temperature for the day in HRU
!!    tmx(:)         |deg C         |maximum temperature for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday      |none          |albedo, the fraction of the solar radiation
!!                               |reflected at the soil surface back into
!!                               |space
!!    etday       |mm H2O        |actual evapotranspiration occuring on day
!!                               |in HRU
!!    ihru        |none          |HRU number
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    nafert(:)   |none          |sequence number of auto-fert application
!!                               |within the year
!!    nair(:)     |none          |sequence number of auto-irrigation
!!                               |application within the year
!!    qdfr        |none          |fraction of water yield that is surface
!!                               |runoff
!!    qdr(:)      |mm H2O        |total amount of water entering main channel
!!                               |for day from HRU
!!    sci(:)      |none          |retention coefficient for CN method based
!!                               |on plant ET
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d           |
!!    gma         |kPa/deg C     |psychrometric constant
!!    ho          |              |net radiation
!!    j           |none          |HRU number
!!    pet_alpha   |none          |alpha factor in Priestley-Taylor ET
!!                               |equation
!!    tmpk        |deg K         |average temperature for the day in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: varinit, albedo, solt, surface, percmain, etpot, etact, fert
!!    SWAT: confert, graze, plantmod, nminrl, nitvol, pminrl, gwmod, apply, gwmod_deep
!!    SWAT: washp, decay, pestlch, enrsb, pesty, orgn, psed, nrain, nlch
!!    SWAT: solp, subwq, bacteria, urban, pothole, latsed, surfstor
!!    SWAT: substor, wetland, hrupond, irrsub, autoirr, watuse, watbal
!!    SWAT: sumv, virtual

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
use rossmod

INTEGER :: j,sb,kk
REAL :: tmpk, d, gma, ho, pet_alpha, aphu, phuop

ihru = 0
ihru = hru1(inum1)

CALL sub_subbasin

DO iihru = 1, hrutot(inum1)
  
  j = 0
  j = ihru
  
  
!!by zhang DSSAT tillage
!!======================
!!    deptil(:)   |mm  |depth of mixing caused by tillage operation
!jj is hru number
  IF (cswat == 2) THEN
    IF (tillage_switch(ihru) == 1) THEN
      IF (tillage_days(ihru) >= 30) THEN
        tillage_switch(ihru) = 0
        tillage_days(ihru) = 0
      ELSE
        tillage_days(ihru) = tillage_days(ihru) + 1
      END IF
!tillage_depth(ihru) = dtil
!tillage_switch(ihru) = .TRUE.
    END IF
  END IF
!!by zhang DSSAT tillage
!!======================
  
  
  
  CALL varinit
  IF (icr(j) <= 0) icr(j) = 1
  
  i_wtrhru = 0
  idplrot(icr(j),ihru) = idplt(j)
  IF (idplt(j) /= 0) THEN
    IF (cpnm(idplt(j)) == "WATR") THEN
      i_wtrhru = 1
    END IF
  END IF
  
  IF (i_wtrhru == 1) THEN
    CALL water_hru
  ELSE
    
!! Simulate land covers other than water
    
!! update base zero total heat units
    IF (tmpav(j) > 0. .AND. phutot(hru_sub(j)) > 0.01) THEN
      phubase(j) = phubase(j) + tmpav(j) / phutot(hru_sub(j))
    END IF
    
    CALL schedule_ops
    
!! calculate albedo for day
    CALL albedo
    
!! calculate soil temperature for soil layers
    CALL solt
    
!       if (ipot(j) /= j .and. imp_trig(nro(j),nrelease(j),j)==1)       &  Srini pothole
!
!     &        then
!! calculate surface runoff if HRU is not impounded or an
!! undrained depression--
    CALL surface
    
!! add surface flow that was routed across the landscape on the previous day
!!   qday = qday + surfq_ru(j)
!!   surfq_ru(j) = 0.
    
!!---------------OGXinSWAT----------------------------
!!  skip
    IF (ievent==0) THEN
!! compute effective rainfall (amount that percs into soil)
      inflpcp = MAX(0.,precipday - surfq(j))
    END IF
!!---------------OGXinSWAT----------------------------
!        end if
    
!! perform management operations
    IF (yr_skip(j) == 0) CALL operatn
    
    IF (auto_wstr(j) > 1.e-6 .AND. irrsc(j) > 2) CALL autoirr
    
    
!!-------------------OGXinSWAT Begin------------------------------
!!  skip the soil water pocolation
    IF (ievent>0) THEN
      solcol(j)%irri=solcol(j)%irri+aird(j)/24.
      solcol(j)%runon=solcol(j)%runon+pot_seep(j)/24.   !!OGX: add the auto irrigation AND pothole seepage
      pot_seep(j) = 0.
    ELSE
!! perform soil water routing
      CALL percmain
    END IF
!!--------------------End--------------------------------
    
!! compute evapotranspiration
    CALL etpot
!        if (pot_vol(j) < 1.e-6) call etact
    CALL etact
    
!!-------------------OGXinSWAT Begin------------------------------
!!  Soil evaporation and transpiration
    IF (ievent>0) THEN
      CALL trs(j)
      inflpcp = MAX(0.,precipday - surfq(j))
      
!!irrigation runoff
      surfq(j) = surfq(j) + qird(j)
      qird(j) = 0.
      
      
!! calculate amount of surface runoff reaching main channel during day
!! (qday) and store the remainder
      CALL surfst_h2o
      
!! calculate half-hour rainfall
      IF (precipday > 0.01) CALL alph(0)
      
      IF (qday > 0.0001) THEN
!! compute peak rate - peakr in m3/s
        CALL pkq(0)
      END IF
      
      IF (qday > 0.0001 .AND. peakr > 0.) THEN
!! compute transmission losses for non-HUMUS datasets
        CALL tran
        CALL eiusle
        
!! calculate sediment erosion by rainfall and overland flow
        CALL ovr_sed
      END IF
      
      CALL cfactor
      IF (surfq(j) > 1.e-6 .AND. peakr > 1.e-6) CALL ysed(0)
      
      IF (qday < 0.) qday = 0.
      
    END IF
!!--------------------End--------------------------------
    
!! compute water table depth using climate drivers
    CALL wattable
    
!! new CN method
    IF (icn == 1) THEN
      sci(j) = sci(j) + pet_day*EXP(-cncoef_sub(hru_sub(j))*sci(j)/  &
          smx(j)) - precipday + qday + qtile + latq(j) + sepbtm(j)
    ELSE IF (icn == 2) THEN
      sci(j) = sci(j) + pet_day*EXP(-cncoef_sub(hru_sub(j))*sci(j)/  &
          smx(j)) - precipday + qday + latq(j) + sepbtm(j) + qtile
      sci(j) = AMIN1(sci(j),smxco * smx(j))
    END IF
    
!! apply fertilizer/manure in continuous fert operation
    IF (icfrt(j) == 1) THEN
      ndcfrt(j) = ndcfrt(j) + 1
      CALL confert
    END IF
    
!! apply pesticide in continuous pest operation
    IF (icpst(j) == 1) THEN
      ndcpst(j) = ndcpst(j) + 1
      CALL conapply
    END IF
    
!! remove biomass from grazing and apply manure
    IF (igrz(j) == 1) THEN
      ndeat(j) = ndeat(j) + 1
      CALL graze
    END IF
    
!! compute crop growth
    CALL plantmod
    
!! check for dormancy
    IF (igro(j) == 1) CALL dormant
!! compute actual ET for day in HRU
    etday = ep_day + es_day + canev
    
!! write daily air and soil temperature file
!! can be uncommmented if needed by user and also in readfile.f
    
!      write (120,12112) i,j,tmx(j),tmn(j),(sol_tmp(k,j),k=1,sol_nly(j))
!12112  format (2i4,12f8.2)
    
!! compute nitrogen and phosphorus mineralization
    
    IF (cswat == 0) THEN
      CALL nminrl
    END IF
    IF (cswat == 1) THEN
      CALL carbon
    END IF
    
!! Add by zhang
!!=================
    IF (cswat == 2) THEN
      CALL carbon_zhang2
    END IF
!! Add by zhang
!!=================
    
    CALL nitvol
    IF (sol_p_model == 1) THEN
      CALL pminrl
    ELSE
      CALL pminrl2
    END IF
    
!!    compute biozone processes in septic HRUs
!!    if 1)current is septic hru and 2)  soil temperature is above zero
    IF (isep_opt(j)/=0.AND.iyr>=isep_iyr(j)) THEN
      IF (sol_tmp(i_sep(j),j) > 0.) CALL biozone
    END IF
    
!! compute ground water contribution
    CALL gwmod
    CALL gwmod_deep
    
!! compute pesticide washoff
    IF (precipday >= 2.54) CALL washp
    
!! compute pesticide degradation
    CALL decay
    
!! compute pesticide movement in soil
    CALL pestlch
    
    IF (surfq(j) > 0. .AND. peakr > 1.e-6) THEN
      IF (precipday > 0.) THEN
        CALL enrsb(0)
        IF (sedyld(j) > 0.) CALL pesty(0)
        
        IF (cswat == 0) THEN
          CALL orgn(0)
        END IF
        IF (cswat == 1) THEN
          
          CALL orgncswat(0)
        END IF
        
!! Add by zhang
!! ====================
        IF (cswat == 2) THEN
          CALL orgncswat2(0)
        END IF
!! Add by zhang
!! ====================
        
        CALL psed(0)
      END IF
    END IF
    
!! add nitrate in rainfall to soil profile
    CALL nrain
    
!! compute nitrate movement leaching
    CALL nlch
    
!! compute phosphorus movement
    CALL solp
    
!! compute chl-a, CBOD and dissolved oxygen loadings
    CALL subwq
    
!! compute bacteria transport
    CALL bacteria
    
!! compute loadings from urban areas
    IF (urblu(j) > 0) THEN
      IF(ievent<3) THEN
        CALL urban ! daily simulation
      ELSE
        CALL urbanhr ! subdaily simulation J.Jeong 4/20/2009
      END IF
    END IF
    
!! Srini Pothole
!! compute undrained depression/impounded area (eg rice) processes
!        if (pot_fr(j) > 0.) then
!           if (ievent<3) then
!          call pothole
!           else
!              call potholehr
!           endif
!        endif
    
!! compute sediment loading in lateral flow and add to sedyld
    CALL latsed
    
!! compute nutrient loading in groundwater flow
    CALL gwnutr
    CALL gw_no3
    
!! lag nutrients and sediment in surface runoff
    CALL surfstor
    
!! lag subsurface flow and nitrate in subsurface flow
    
    CALL substor
    
!! add lateral flow that was routed across the landscape on the previous day
!!  latq(j) = latq(j) + latq_ru(j)
!!  latq_ru(j) = 0.
    
!! compute reduction in pollutants due to edge-of-field filter strip
    IF (vfsi(j) >0.)THEN
      CALL filter
      IF (filterw(j) > 0.) CALL buffer
    END IF
    IF (vfsi(j) == 0. .AND. filterw(j) > 0.) THEN
      CALL filtw
      CALL buffer
    END IF
    
!! compute reduction in pollutants due to in field grass waterway
    IF (grwat_i(j) == 1) THEN
      CALL grass_wway
    END IF
    
!! compute reduction in pollutants due to in fixed BMP eff
    IF (bmp_flag(j) == 1) THEN
      CALL bmpfixed
    END IF
    
    
!! compute water yield for HRU
    qdr(j) = qday + latq(j) + gw_q(j) + qtile + gw_qdeep(j)
    IF (qdr(j) < 0.) qdr(j) = 0.
    IF (qdr(j) > 0.) THEN
      qdfr = qday / qdr(j)
    ELSE
      qdfr = 0.
    END IF
    
!! compute wetland processes
    CALL wetlan
    
!! compute pond processes
    IF (ievent<3) THEN
      CALL hrupond
    ELSE
      CALL hrupondhr
    END IF
    
!       Srini pothole
    IF (pot_fr(j) > 0.) CALL pothole
    
    xx = sed_con(j)+soln_con(j)+solp_con(j)+orgn_con(j)+orgp_con(j)
    IF (xx > 1.e-6) THEN
      CALL urb_bmp
    END IF
    
!! consumptive water use (ponds, shallow aquifer, deep aquifer)
    CALL watuse
    
!! perform water balance
    CALL watbal
    
!! qdayout is surface runoff leaving the hru - after wetlands, ponds, and potholes
    qdayout(j) = qday
    
  END IF
  
!! perform output summarization
  CALL sumv
  
!! summarize output for multiple HRUs per subbasin
!! store reach loadings for new fig method
  CALL virtual
  aird(j) = 0.
  
  ihru = ihru + 1
END DO

!! route 2 landscape units
IF (ils2flag(inum1) > 0) THEN
  isub = inum1                        ! save the subbasin number
  
!! calculate outputs from hillslope
  ihout1 = mhyd_bsn + (inum1 - 1) * 4 ! first outflow hyd number
  ihout = ihout1                      ! outflow hyd number
  inum1 = 1                           ! landscape unit number
  inum2 = isub                        ! subbasin number
  CALL routeunit                      ! hillslope unit
  CALL sumhyd
  inum1s(ihout) = inum1
  inum2s(ihout) = inum2
  ihouts(ihout) = ihout
  
!! calculate outputs from valley bottom
  inum1 = 2                           ! landscape unit number
  ihout = ihout + 1                   ! outflow hyd number
  sumdaru = 0.
  DO j = 1, hrutot(isub)
    sumdaru = sumdaru + hru_km(j)
  END DO
  daru_km(inum2,inum1) = sumdaru
  CALL routeunit                      ! valley bottom unit
  CALL sumhyd
  inum1s(ihout) = inum1
  inum2s(ihout) = inum2
  ihouts(ihout) = ihout
  
!! route output from hillslope across valley bottom
  ihout = ihout + 1                   ! outflow hyd number
  inum1 = 2                           ! valley bottom landscape unit
  inum2 = ihout1                      ! inflow hyd=outlfow from hillslope
  inum3 = isub                        ! subbasin number
  rnum1 = 1.                          ! fraction overland flow
  iru_sub = 1                         ! route across landscape unit
!! compute weighted K factor for sediment transport capacity
  sumk = 0.
  ovsl = 0.
  ovs = 0.
  DO j = 1, hrutot(isub)
    sumk = sumk + usle_k(j) * hru_rufr(inum1,j)
    ovsl = ovsl + slsubbsn(j)
    ovs = ovs + hru_slp(j)
  END DO
  ovsl = ovsl / hrutot(isub)
  ovs = ovs / hrutot(isub)
  ru_k(isub,inum1) = sumk
  ru_ovsl(isub,inum1) = ovsl
  ru_ovs(isub,inum1) = ovs
  ru_ktc(isub,inum1) = 50.
  ru_a(isub,inum1) = daru_km(isub,1) / ru_ovsl(isub,inum1)
  CALL routels(iru_sub)               ! route across valley bottom
  CALL sumhyd
  inum1s(ihout) = inum1
  inum2s(ihout) = inum2
  inum3s(ihout) = inum3
  ihouts(ihout) = ihout
  
!! add routed with valley bottom loading
  inum1 = ihout                       ! hyd from routed
  inum2 = ihout - 1                   ! hyd from loading
  ihout = ihout + 1                   ! outflow hyd number
  CALL addh                           ! add hyd's
  CALL sumhyd
  inum1s(ihout) = inum1
  inum2s(ihout) = inum2
  ihouts(ihout) = ihout
  
!! save landscape routed output in place of subbasin output for routing
  varoute(isub,:) = varoute(ihout,:)
END IF

1000 FORMAT(4I10,a10)
RETURN
END SUBROUTINE subbasin
