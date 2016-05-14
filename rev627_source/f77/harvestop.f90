SUBROUTINE harvestop
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest operation (no kill)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    bio_hv(:,:,:)|kg/ha          |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha |annual biomass (dry weight) in the HRU
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    harveff       |none         |harvest efficiency: fraction of harvested
!!                                |yield that is removed from HRU; the
!!                                |remainder becomes residue on the soil
!!                                |surface
!!    hi_ovr      |(kg/ha)/(kg/ha)|harvest index target specified at
!!                                |harvest
!!    hru_dafr(:) |km2/km2        |fraction of watershed area in HRU
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for specific time
!!                                |during growing season
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idc(:)      |none           |crop/landcover category:
!!                                |1 warm season annual legume
!!                                |2 cold season annual legume
!!                                |3 perennial legume
!!                                |4 warm season annual
!!                                |5 cold season annual
!!                                |6 perennial
!!                                |7 trees
!!    idplt(:)    |none           |land cover code from crop.dat
!!    ihru        |none           |HRU number
!!    laiday(:)   |none           |leaf area index
!!    ncut(:)     |none           |sequence number of harvest operation within
!!                                |a year
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    nro(:)      |none           |sequence number of year in rotation
!!    nyskip      |none           |number of years output is not printed/
!!                                |summarized
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha        |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha        |amount of phosphorus in plant biomass
!!    plt_et(:)   |mm H2O         |actual ET simulated during life of plant
!!    plt_pet(:)  |mm H2O         |potential ET simulated during life of plant
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    pltfr_n(:)  |none           |fraction of plant biomass that is nitrogen
!!    pltfr_p(:)  |none           |fraction of plant biomass that is phosphorus
!!    rwt(:)      |none           |fraction of total plant biomass that is
!!                                |in roots
!!    sol_fon(:,:)|kg N/ha        |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha        |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:)|kg/ha          |amount of organic matter in the soil
!!                                |classified as residue
!!    wshd_yldn   |kg N/ha        |amount of nitrogen removed from soil in
!!                                |watershed in the yield
!!    wshd_yldp   |kg P/ha        |amount of phosphorus removed from soil in
!!                                |watershed in the yield
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    yldanu(:)   |metric tons/ha |annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha          |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_hv(:,:,:)|kg/ha          |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha |annual biomass (dry weight) in the HRU
!!    laiday(:)   |none           |leaf area index
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha        |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha        |amount of phosphorus in plant biomass
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    rsr1c(:)    |               |initial root to shoot ratio at beg of growing season
!!    rsr2c(:)    |               |root to shoot ratio at end of growing season
!!    sol_fon(:,:)|kg N/ha        |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha        |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:)|kg/ha          |amount of organic matter in the soil
!!                                |classified as residue
!!    tnyld(:)    |kg N/kg yield  |modifier for autofertilization target
!!                                |nitrogen content for plant
!!    wshd_yldn   |kg N/ha        |amount of nitrogen removed from soil in
!!                                |watershed in the yield
!!    wshd_yldp   |kg P/ha        |amount of phosphorus removed from soil in
!!                                |watershed in the yield
!!    yldanu(:)   |metric tons/ha |annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha          |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    clip        |kg/ha          |yield lost during harvesting
!!    clipn       |kg N/ha        |nitrogen in clippings
!!    clipp       |kg P/ha        |phosphorus in clippings
!!    clippst     |kg pst/ha      |pesticide in clippings
!!    hiad1       |none           |actual harvest index (adj for water/growth)
!!    j           |none           |HRU number
!!    k           |none           |counter
!!    wur         |none           |water deficiency factor
!!    yield       |kg             |yield (dry weight)
!!    yieldn      |kg N/ha        |nitrogen removed in yield
!!    yieldp      |kg P/ha        |phosphorus removed in yield
!!    yldpst      |kg pst/ha      |pesticide removed in yield
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
use parm

INTEGER :: j, k

!!   change per JGA 8/31/2011 gsm PUT YIELD IN modparm.f
!!    real :: hiad1, wur, yield, clip, yieldn, yieldp, clipn, clipp
REAL :: hiad1, wur, clip, clipn, clipp
REAL :: yldpst, clippst, rtresnew
REAL :: clipgrn,cliptbr,clipngrn,clippgrn,yieldngrn
REAL :: yieldntbr,yieldnrsd,yieldpgrn,yieldptbr,yieldprsd
REAL :: clipntbr,clipptbr,rtresp

!!add by zhang
!!===================
REAL :: blg1, blg2, blg3,  clg, sf
REAL :: sol_min_n, resnew, resnew_n, resnew_ne
REAL :: lmf, lsf, lslf, lsnf,lmnf
REAL ::  rln, rlr
orgc_f = 0.
blg1 = 0.
blg2 = 0.
blg3 = 0.
clg = 0.
sf = 0.
sol_min_n = 0.
resnew = 0.
resnew_n = 0.
resnew_ne = 0.
lmf = 0.
lsf = 0.
lslf = 0.
lsnf = 0.
lmnf = 0.

rln = 0.
rlr = 0.
clipgrn = 0.; cliptbr = 0.; clipngrn = 0.; clippgrn = 0.
yieldngrn = 0.; yieldntbr = 0.; yieldnrsd = 0.; yieldpgrn = 0.
yieldptbr = 0.; yieldprsd = 0.; clipntbr = 0.; clipptbr = 0.
rtresnew = 0.; rtresn = 0.; rtresp = 0.

!!add by zhang
!!===================

j = ihru

yieldgrn = 0.
yieldbms = 0.
yieldtbr = 0.
yieldrsd = 0.

ssb = bio_ms(j)                            ! Armen 16 Jan 2009 storing info
ssabg = bio_ms(j) * (1.- rwt(j))     ! Armen 16 Jan 2009 storing info
ssr = ssb * rwt(j)                         ! Armen 16 Jan 2009 storing info
ssn = plantn(j)                            ! Armen 20 May 2006 storing info
ssp = plantp(j)                            ! Armen 20 May 2006 storing info

!! calculate modifier for autofertilization target nitrogen content
tnyld(j) = (1. - rwt(j)) * bio_ms(j) * pltfr_n(j) * auto_eff(j)

!! compute grain yield
hiad1 = 0.
IF (plt_pet(j) < 10.) THEN
  wur = 100.
ELSE
  wur = 100. * plt_et(j) / plt_pet(j)
END IF
hiad1 = (hvstiadj(j) - wsyf(idplt(j))) *  &
    (wur / (wur + EXP(6.13 - .0883 * wur))) + wsyf(idplt(j))
IF (hiad1 > hvsti(idplt(j))) THEN
  hiad1 = hvsti(idplt(j))
END IF


!! check if yield is from above or below ground
IF (hvsti(idplt(j)) > 1.001) THEN
!! compute tuber yields
!!       yieldtbr = bio_ms(j) * (1. - 1. / (1. + hiad1))
!! determine clippings (biomass left behind) and update yield
  yieldtbr = bio_ms(j) * (1. - 1. / (1. + hiad1)) * harveff  !! corrected by cibin nov/2013
  cliptbr = bio_ms(j) * (1. - 1. / (1. + hiad1)) * (1. - harveff) !! corrected by cibin nov/2013
  bio_ms(j) = bio_ms(j) - yieldtbr - cliptbr
!! calculate nutrients removed with yield
  yieldntbr = yieldtbr * cnyld(idplt(j))
  yieldptbr = yieldtbr * cpyld(idplt(j))
  yieldntbr = MIN(yieldntbr, 0.80 * plantn(j))
  yieldptbr = MIN(yieldptbr, 0.80 * plantp(j))
  clipntbr = cliptbr * pltfr_n(j)
  clipptbr = cliptbr * pltfr_p(j)
  clipntbr = MIN(clipntbr, plantn(j) - yieldntbr)
  clipptbr = MIN(clipptbr, plantp(j) - yieldptbr)
  plantn(j) = plantn(j) - yieldntbr - clipntbr
  plantp(j) = plantp(j) - yieldptbr - clipptbr
END IF

IF (hi_bms > 0.) THEN       !! compute biomass yield !! corrected by cibin Nov/2013
  yieldbms = hi_bms * (1.-rwt(j)) * bio_ms(j)*harveff
  clipbms = hi_bms * (1.-rwt(j)) * bio_ms(j) * (1. - harveff)
  bio_ms(j) = bio_ms(j) - yieldbms - clipbms !corrected by Jaehak Jeong sep. 2013
!! calculate nutrients removed with yield
  yieldnbms = yieldbms * cnyld(idplt(j))   !! corrected by cibin Nov/2013
  yieldpbms = yieldbms * cpyld(idplt(j))
  yieldnbms = MIN(yieldnbms, 0.80 * plantn(j))
  yieldpbms = MIN(yieldpbms, 0.80 * plantp(j))
!! calculate nutrients removed with clippings
  clipnbms = clipbms * cnyld(idplt(j))   !! corrected by cibin Nov/2013
  clippbms = clipbms * cpyld(idplt(j))
  clipnbms = MIN(clipnbms, plantn(j) - yieldnbms)
  clippbms = MIN(clippbms, plantp(j) - yieldpbms)
  plantn(j) = plantn(j) - yieldnbms - clipnbms
  plantp(j) = plantp(j) - yieldpbms - clippbms
ELSE
!! compute grain yields
  yieldgrn = (1.-rwt(j)) * bio_ms(j) * hiad1* harveff
!! determine clippings (biomass left behind) and update yield
  
  clipgrn = (1.-rwt(j)) * bio_ms(j) * hiad1 * (1. - harveff)
  bio_ms(j) = bio_ms(j) - yieldgrn - clipgrn
!! calculate nutrients removed with yield
  yieldngrn = yieldgrn * cnyld(idplt(j))
  yieldpgrn = yieldgrn * cpyld(idplt(j))
  yieldngrn = MIN(yieldngrn, 0.80 * plantn(j))
  yieldpgrn = MIN(yieldpgrn, 0.80 * plantp(j))
!! calculate nutrients removed with clippings
  clipngrn = clipgrn * cnyld(idplt(j))
  clippgrn = clipgrn * cpyld(idplt(j))
  clipngrn = MIN(clipngrn, plantn(j) - yieldngrn)
  clippgrn = MIN(clippgrn, plantp(j) - yieldpgrn)
  plantn(j) = plantn(j) - yieldngrn - clipngrn
  plantp(j) = plantp(j) - yieldpgrn - clippgrn
END IF




!! add clippings to residue and organic n and p
sol_rsd(1,j) = sol_rsd(1,j) + clipgrn + clipbms + cliptbr
sol_fon(1,j) = sol_fon(1,j) + clipngrn + clipnbms + cliptbr
sol_fop(1,j) = sol_fop(1,j) + clippgrn + clippbms + cliptbr

!! compute residue yield
IF (hi_rsd > 0.) THEN
  yieldrsd = hi_rsd * sol_rsd(1,j)
  yieldnrsd = hi_rsd * sol_fon(1,j)
  yieldprsd = hi_rsd * sol_fon(1,j)
  sol_rsd(1,j) = sol_rsd(1,j) - yieldrsd
  sol_fon(1,j) = sol_fon(1,j) - yieldnrsd
  sol_fop(1,j) = sol_fop(1,j) - yieldprsd
END IF

yield = yieldgrn + yieldbms + yieldtbr + yieldrsd
yieldn = yieldngrn + yieldnbms + yieldntbr + yieldnrsd
yieldp = yieldpgrn + yieldpbms + yieldptbr + yieldprsd
clip= clipgrn + clipbms + cliptbr   !! cibin nov 2013
clipn = clipngrn + clipnbms + clipntbr !! cibin nov 2013
clipp = clippgrn + clippbms + clipptbr !! cibin nov 2013

!!add by zhang
!!=====================
!!use idplt(:,:,:) to calculate the crop type, then
!! decide which type of crop yield should be used.
IF (cswat == 2) THEN
  grainc_d(j) = grainc_d(j)+ yield * 0.42
  rsdc_d(j) = rsdc_d(j)+(clip+yield) * 0.42
END IF
!!add by zhang
!!=====================


!!insert new biomss by zhang
!!===============================
IF (cswat == 2) THEN
  blg1 = 0.01/0.10 !BLG1/BLG2
  blg2 = 0.99
  blg3 = 0.10 !BLG2
!CALL ASCRV(BLG(1,I),BLG(2,I),.5,1.)
  xx = LOG(0.5/blg1-0.5)
  blg2 = (xx -LOG(1./blg2-1.))/(1.-0.5)
  blg1 = xx + 0.5*blg2
  clg=blg3*phuacc(j)/(phuacc(j)+ EXP(blg1-blg2*phuacc(j)))
  sf = 0.05
!kg/ha
  sol_min_n = 0.
  sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
  resnew = clip
  resnew_n = clipn
  resnew_ne = resnew_n + sf * sol_min_n
!Not sure 1000 should be here or not!
!RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
  rln = (resnew * clg/(resnew_n+1.e-5))
!RLR is the fraction of lignin in the added residue
  rlr = MIN(.8, resnew * clg/1000/(resnew/1000+1.e-5))
!In most cases, lignin content in residue should be less than 30%
!Therefore, RLR is expected to be less than 0.3
!In the future, we may want to add a check make sure LMF is less than 1.0 - RLR.
!this would help to avoid sol_LS becoming less than sol_LSL
  
  lmf = 0.85 - 0.018 * rln
  IF (lmf <0.01) THEN
    lmf = 0.01
  ELSE
    IF (lmf >0.7) THEN
      lmf = 0.7
    END IF
  END IF
  
  lsf =  1 - lmf
  sol_lm(1,j) = sol_lm(1,j) + lmf * resnew
  sol_ls(1,j) = sol_ls(1,j) + lsf * resnew
  
!In Jimmy's code, lignin added to sol_LSL is calculated as RLR*LSF*resnew
!However, I think we should use RLR*resnew; Confirmed with Jimmy
!sol_LSL(1,j) = sol_LSL(1,j) + RLR* LSF * resnew
  sol_lsl(1,j) = sol_lsl(1,j) + rlr*resnew
  sol_lsc(1,j) = sol_lsc(1,j) + 0.42*lsf * resnew
!In allignment with the sol_LSL calculation, sol_LSLC is also changed
!sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*0.42*LSF * resnew
  sol_lslc(1,j) = sol_lslc(1,j) + rlr*0.42+resnew
  sol_lslnc(1,j) = sol_lsc(1,j) - sol_lslc(1,j)
  
!X3 = MIN(X6,0.42*LSF * resnew/150)
  IF (resnew_ne >= (0.42 * lsf * resnew /150)) THEN
    sol_lsn(1,j) = sol_lsn(1,j) + 0.42 * lsf * resnew / 150
    sol_lmn(1,j) = sol_lmn(1,j) + resnew_ne -  &
        (0.42 * lsf * resnew / 150) + 1.e-25
  ELSE
    sol_lsn(1,j) = sol_lsn(1,j) + resnew_ne
    sol_lmn(1,j) = sol_lmn(1,j) + 1.e-25
  END IF
  sol_lmc(1,j) = sol_lmc(1,j) + 0.42 * lmf * resnew
!update no3 and nh3 in soil
  sol_no3(1,j) = sol_no3(1,j) * (1-sf)
  sol_nh3(1,j) = sol_nh3(1,j) * (1-sf)
END IF
!!insert new biomss by zhang
!!=============================

!! Calculation for dead roots allocations, resetting phenology, updating other pools
IF (ssabg > 1.e-6) THEN
  ff3 = (yield + clip) / ssabg ! Armen 20 May 2008 and 16 Jan 2009
ELSE
  ff3 = 1.
END IF
IF (ff3 > 1.0) ff3 = 1.0

! nssr is the new mass of roots
!      nssr = rwt(j) * ssabg * (1. - ff3) / (1. - rwt(j))
!      rtresnew = ssr - nssr
!      if (ssr > 1.e-6) then
!       ff4 = rtresnew / ssr
!      else!
!    ff4 = 0.
!      end if
!      rtresn = ff4 * ssn
!      rtresp = ff4 * ssp


!! reset leaf area index and fraction of growing season
IF (ssb > 0.001) THEN
  laiday(j) = laiday(j) * (1. - ff3)
  IF (laiday(j) < alai_min(idplt(j))) THEN   !Sue
    laiday(j) = alai_min(idplt(j))
  END IF
  phuacc(j) = phuacc(j) * (1. - ff3)
  rwt(j) = .4 - .2 * phuacc(j)
ELSE
  bio_ms(j) = 0.
  laiday(j) = 0.
  phuacc(j) = 0.
END IF


!      !! compute fraction of roots in each layer ! Armen 20 May 2008
!      call rootfr
!
!      !! allocate roots, N, and P to soil pools ! Armen 20 May 2008
!      do l=1, sol_nly(j)
!            sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
!            sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * rtresn
!            sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * rtresp
!      end do
!
!     rtfr = 0.
!! allocate roots, N, and P to soil pools ! Armen 20 May 2008
DO l=1, sol_nly(j)
  sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
  sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * rtresn
  sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * rtresp
  
!!insert new biomss by zhang
!!=============================
  IF (cswat == 2) THEN
    rsdc_d(j) = rsdc_d(j)+rtfr(l) * rtresnew * 0.42
    blg3 = 0.10
    blg1 = 0.01/0.10
    blg2 = 0.99
    
    xx = LOG(0.5/blg1-0.5)
    blg2 = (xx -LOG(1./blg2-1.))/(1.-0.5)
    blg1 = xx + 0.5*blg2
    clg=blg3*phuacc(j)/(phuacc(j)+ EXP(blg1-blg2*phuacc(j)))
!kg/ha
    sol_min_n = 0.
    sol_min_n = (sol_no3(l,j)+sol_nh3(l,j))
    
    resnew = rtfr(l) * rtresnew
!resnew_n = resnew * pltfr_n(j)
!resnew_ne = resnew_n + sf * sol_min_n
    resnew_n = rtfr(l) * rtresn
    resnew_ne = resnew_n + sf * sol_min_n
!Not sure 1000 should be here or not!
!RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
    rln = (resnew * clg/(resnew_n+1.e-5))
    rlr = MIN(.8, resnew * clg/1000/(resnew/1000+1.e-5))
    
    lmf = 0.85 - 0.018 * rln
    IF (lmf <0.01) THEN
      lmf = 0.01
    ELSE
      IF (lmf >0.7) THEN
        lmf = 0.7
      END IF
    END IF
    
    lsf =  1 - lmf
    sol_lm(l,j) = sol_lm(l,j) + lmf * resnew
    sol_ls(l,j) = sol_ls(l,j) + lsf * resnew
    
!here a simplified assumption of 0.5 LSL
    lslf = clg
    sol_lsl(l,j) = sol_lsl(l,j) + rlr* lsf * resnew
    sol_lsc(l,j) = sol_lsc(l,j) + 0.42*lsf * resnew
    sol_lslc(l,j) = sol_lslc(l,j) + rlr*0.42*lsf * resnew
    sol_lslnc(l,j) = sol_lsc(l,j) - sol_lslc(1,j)
    
    IF (resnew_ne >= (0.42 * lsf * resnew /150)) THEN
      sol_lsn(l,j) = sol_lsn(l,j) + 0.42 * lsf * resnew / 150
      sol_lmn(l,j) = sol_lmn(l,j) + resnew_ne -  &
          (0.42 * lsf * resnew / 150) + 1.e-25
    ELSE
      sol_lsn(l,j) = sol_lsn(l,j) + resnew_ne
      sol_lmn(l,j) = sol_lmn(l,j) + 1.e-25
    END IF
    sol_lmc(l,j) = sol_lmc(l,j) + 0.42 * lmf * resnew
!update no3 and nh3 in soil
    sol_no3(1,j) = sol_no3(1,j) * (1-sf)
    sol_nh3(1,j) = sol_nh3(1,j) * (1-sf)
  END IF
!!insert new biomss by zhang
!!=============================
END DO

rtfr = 0.

!! adjust foliar pesticide for plant removal
IF (hrupest(j) == 1) THEN
  DO k = 1, npmx
!! calculate amount of pesticide removed with yield and clippings
    yldpst = 0.
    clippst = 0.
    IF (hvsti(idplt(j)) > 1.001) THEN
      yldpst = plt_pst(k,j)
      plt_pst(k,j) = 0.
    ELSE
      yldpst = hiad1 * plt_pst(k,j)
      plt_pst(k,j) = plt_pst(k,j) - yldpst
      IF (plt_pst(k,j) < 0.) plt_pst(k,j) = 0.
    END IF
    clippst = yldpst * (1. - harveff)
    IF (clippst < 0.) clippst = 0.
!! add pesticide in clippings to soil surface
    sol_pst(k,j,1) = sol_pst(k,j,1) + clippst
  END DO
END IF

!! summary calculations
IF (curyr > nyskip) THEN
  wshd_yldn = wshd_yldn + yieldn * hru_dafr(j)
  wshd_yldp = wshd_yldp + yieldp * hru_dafr(j)
  yldkg(icr(j),j) = yldkg(icr(j),j) + yield
  yldanu(j) = yldanu(j) + yield  / 1000.
  
! select case (idc(idplt(j)))
!   case (3, 6, 7)
!     bio_hv(nro(j),icr(j),j) = (yield + clip) + bio_hv(nro(j),icr(j),j)
!     bio_yrms(j) = bio_yrms(j) + (yield + clip) / 1000.
!   case default
  bio_hv(icr(j),j) = (yield + clip + rtresnew) +  &
      bio_hv(icr(j),j)                       !! Jeff, is this the intention
  bio_yrms(j) = bio_yrms(j) + (yield + clip + rtresnew) / 1000.              !! Jeff, is this the intention
! end select
END IF

ncut(j) = ncut(j) + 1

RETURN
END SUBROUTINE
