SUBROUTINE rtsed_kodatie
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    deposition is based on fall velocity and degradation on stream

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_cov1(:)   |none         |channel bank cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_cov2(:)   |none         |channel bed cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_di(:)    |m             |initial depth of main channel
!!    ch_li(:)    |km            |initial length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_si(:)    |m/m           |initial slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    ch_wdr(:)   |m/m           |channel width to depth ratio
!!    ideg        |none          |channel degredation code
!!                               |0: do not compute channel degradation
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    prf(:)      |none          |Reach peak rate adjustment factor for sediment
!!                               |routing in the channel. Allows impact of
!!                               |peak flow rate on sediment routing and
!!                               |channel reshaping to be taken into account
!!    rchdep      |m             |depth of flow on day
!!    rnum1       |none          |fraction of overland flow
!!    sdti        |m^3/s         |average flow on day in reach
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    spcon       |none          |linear parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    spexp       |none          |exponent parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    varoute(3,:)|metric tons   |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    peakr       |m^3/s         |peak runoff rate in channel
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    sedrch      |metric tons   |sediment transported out of channel
!!                               |during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dat2        |m             |change in channel depth during time step
!!    deg         |metric tons   |sediment reentrained in water by channel
!!                               |degradation
!!    dep         |metric tons   |sediment deposited on river bottom
!!    depdeg      |m             |depth of degradation/deposition from original
!!    depnet      |metric tons   |
!!    dot         |
!!    jrch        |none          |reach number
!!    qdin        |m^3 H2O       |water in reach during time step
!!    vc          |m/s           |flow velocity in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!! Modification to the original SWAT sediment routine
!! By Balaji Narasimhan and Peter Allen
!!    Kodatie (Modified Simons-Li associates) approach combined with Einsteins deposition equation
!!    Plus particle size tracking.

use parm

INTEGER :: jrch, ch_d50type
REAL :: qdin, sedin, vc, cyin, cych, depnet, deg, dep, tbase
REAL :: depdeg, dot, vs, x, sc, tcbnk, tcbed,tbank,tbed,asinea,tou
REAL :: sanin, silin, clain, sagin, lagin, grain, outfract
REAL :: depsan, depsil, depcla, depsag, deplag, depgra
REAL :: degsan, degsil, degcla, deggra, degrte
REAL :: bnksan, bnksil, bnkcla, bnkgra, pdep, pdepbed, bedsize
REAL :: uspower,bnkrte,adddep,fpratio,watdep,bnkrt,bedrt,effbnkbed

jrch = 0
jrch = inum1

IF (rtwtr > 0. .AND. rchdep > 0.) THEN
  
!! initialize water in reach during time step
  qdin = 0.
  qdin = rtwtr + rchstor(jrch)
  
!! initialize sediment in reach during time step
  sedin = 0.
  sanin = 0.
  silin = 0.
  clain = 0.
  sagin = 0.
  lagin = 0.
  sedin = varoute(3, inum2) * (1. - rnum1) + sedst(jrch)
  sanin = varoute(23,inum2) * (1. - rnum1) + sanst(jrch)
  silin = varoute(24,inum2) * (1. - rnum1) + silst(jrch)
  clain = varoute(25,inum2) * (1. - rnum1) + clast(jrch)
  sagin = varoute(26,inum2) * (1. - rnum1) + sagst(jrch)
  lagin = varoute(27,inum2) * (1. - rnum1) + lagst(jrch)
  grain = varoute(28,inum2) * (1. - rnum1) + grast(jrch)
  sedinorg = sedin
  
!! do not perform sediment routing if no water in reach
  IF (qdin > 0.01) THEN
    
!! initialize reach peak runoff rate
    peakr = 1. * sdti  !! prf(jrch) = 1.0
    
!! calculate peak flow velocity
    vc = 0.
    IF (rcharea < .010) THEN
      vc = 0.01
    ELSE
      vc = peakr / rcharea
    END IF
    
    IF (vc > 5.) vc = 5.
    
    tbase = 0.
    tbase = ch_l2(jrch) * 1000. / (3600. * 24. * vc)
    IF (tbase > 1.) tbase = 1.
    
!! JIMMY'S NEW IMPROVED METHOD for sediment transport
    cyin = 0.
    cych = 0.
    depnet = 0.
    deg = 0.
    
    deg1 = 0.
    deg1san = 0.
    deg1sil = 0.
    deg1cla = 0.
    deg1sag = 0.
    deg1lag = 0.
    deg1gra = 0.
    
    degrte = 0.
    degremain = 0.
    deggra = 0.
    degsan = 0.
    degsil = 0.
    degcla = 0.
    bnksan = 0.
    bnksil = 0.
    bnkcla = 0.
    bnkgra = 0.
    bnkrte = 0.
    dep = 0.
    depsan = 0.
    depsil = 0.
    depcla = 0.
    depsag = 0.
    deplag = 0.
    depgra = 0.
    watdep = 0.
    bnkrt = 0.
    bedrt = 0.
    effbnkbed = 0.
    
    c = chside(jrch)
    pbed = phi(6,jrch)
    pbank = 2. * rchdep * SQRT(1. + c * c)
    rh = rcharea / (pbed + pbank)
    
    topw = 0.
    IF (rchdep <= ch_d(jrch)) THEN
      topw = phi(6,jrch) + 2. * rchdep * c
      fpratio = 0.
      watdep = rchdep
    ELSE
      topw = 5 * ch_w(2,jrch) + 2. * (rchdep - ch_d(jrch)) * 4.
      adddep = rchdep - ch_d(jrch)
!! Area Ratio of water in flood plain to total cross sectional area
      fpratio = (rcharea - phi(1,jrch) - ch_w(2,jrch)*adddep)/rcharea
      fpratio = MAX(0.,fpratio)
      watdep = ch_d(jrch)
    END IF
    
!! Applied Bank Shear Stress
!!    Equations from Eaton and Millar (2004)
    sfbank = 10**(-1.4026 * LOG10((pbed/pbank) + 1.5) + 2.247)
    
    tou = 9800. * rchdep * ch_s(2,jrch)
    
    asinea = 1. / SQRT((1.**2) + (c**2))
    
    tbank = tou * (sfbank/100.) * (topw + pbed) * asinea/ (4.*rchdep)
    
    tbed  = tou * (1. - (sfbank/100.)) * (topw/(2.*pbed) + 0.5)
    
!!    Potential Bank Erosion rate in metric tons per day
!!    Assumed on an average Only one bank eroding due to meandering of channel
    bnkrte = ch_bnk_kd(jrch) * (tbank - tc_bnk(jrch)) * 1E-06
    IF (bnkrte < 0.) bnkrte = 0.
    bnkrte = bnkrte * ch_l2(jrch) * 1000.* (watdep * SQRT(1. + c * c))  &
        * ch_bnk_bd(jrch) * 86400.
    
!!    Potential Bed degradation rate in metric tons per day
    degrte = ch_bed_kd(jrch) * (tbed - tc_bed(jrch)) * 1E-06
    IF (degrte < 0.) degrte = 0.
    degrte = degrte * ch_l2(jrch) * 1000.* phi(6,jrch)  &
        * ch_bed_bd(jrch) * 86400.
    
!!    Relative potential for bank/bed erosion
    IF (bnkrte + degrte > 1.e-6) THEN
      bnkrt = bnkrte / (bnkrte + degrte)
    ELSE
      bnkrt = 1.0
    END IF
    bnkrt = MIN(1.0, bnkrt)
!!    Relative potential for bed erosion
    bedrt = 1. - bnkrt
    
!!    silt-bed rivers
    bedsize = ch_bed_d50(jrch)/1000.
    IF (bedsize <= 0.05) THEN
      akod_a = 281.4
      akod_b = 2.622
      akod_c = 0.182
      akod_d = 0
    END IF
    
!!    Very fine to fine-bed rivers
    IF (bedsize > 0.05 .AND. bedsize <= 0.25) THEN
      akod_a = 2829.6
      akod_b = 3.646
      akod_c = 0.406
      akod_d = 0.412
    END IF
    
!!    Medium to very coarse sand-bed rivers
    IF (bedsize > 0.25 .AND. bedsize <= 2.) THEN
      akod_a = 2123.4
      akod_b = 3.300
      akod_c = 0.468
      akod_d = 0.613
    END IF
    
!!    Gravel bed rivers
    IF (bedsize > 2.) THEN
      akod_a = 431884.8
      akod_b = 1.000
      akod_c = 1.000
      akod_d = 2.000
    END IF
    
!!    Incoming sediment concentration
    cyin = sedin/qdin
    
!!    Maximum bedmaterial transport capacity in metric tons/m/day
    qcych =akod_a*(vc**akod_b)*(rchdep**akod_c)*(ch_s(2,jrch)**akod_d)
    
!!    Maximum bedmaterial transport capacity in metric tons/day
    cych = qcych/qdin * (topw + phi(6,jrch))/2.
    
!!    Potential sediment Transport capacity
    depnet = qdin * (cych - cyin)
    
    IF (depnet <= 1.e-6) THEN
      depnet = 0.
      bnkrte = 0.
      degrte = 0.
    ELSE
!! First the deposited material will be degraded before channel bed or bank erosion
      IF (depnet >= depch(jrch)) THEN
!! Effective erosion
        effbnkbed = depnet - depch(jrch)
!! Effective bank erosion
        IF (effbnkbed*bnkrt <= bnkrte) bnkrte = effbnkbed*bnkrt
        bnksan = bnkrte * ch_bnk_san(jrch)
        bnksil = bnkrte * ch_bnk_sil(jrch)
        bnkcla = bnkrte * ch_bnk_cla(jrch)
        bnkgra = bnkrte * ch_bnk_gra(jrch)
        
!! Effective bed erosion
        IF (effbnkbed*bedrt <= degrte) degrte = effbnkbed*bedrt
        degsan = degrte * ch_bed_san(jrch)
        degsil = degrte * ch_bed_sil(jrch)
        degcla = degrte * ch_bed_cla(jrch)
        deggra = degrte * ch_bed_gra(jrch)
        
        deg1 = depch(jrch)
        deg1san = depsanch(jrch)
        deg1sil = depsilch(jrch)
        deg1cla = depclach(jrch)
        deg1sag = depsagch(jrch)
        deg1lag = deplagch(jrch)
        deg1gra = depgrach(jrch)
        
        depch(jrch) = 0.
        depsanch(jrch) = 0.
        depsilch(jrch) = 0.
        depclach(jrch) = 0.
        depsagch(jrch) = 0.
        deplagch(jrch) = 0.
        depgrach(jrch) = 0.
        
      ELSE
        
        bnkrte = 0.
        degrte = 0.
        degsan = 0.
        degsil = 0.
        degcla = 0.
        deggra = 0.
        bnksan = 0.
        bnksil = 0.
        bnkcla = 0.
        bnkgra = 0.
        
        depch(jrch) = depch(jrch) - depnet
        deg1 = depnet
        
        IF (depclach(jrch) >= depnet) THEN
          depclach(jrch) = depclach(jrch) - depnet
          deg1cla = depnet
          degremain = 0.
        ELSE
          degremain = depnet - depclach(jrch)
          deg1cla = depclach(jrch)
          depclach(jrch) = 0.
          IF (depsilch(jrch) >= degremain) THEN
            depsilch(jrch) = depsilch(jrch) - degremain
            deg1sil = degremain
            degremain = 0.
          ELSE
            degremain = degremain - depsilch(jrch)
            deg1sil = depsilch(jrch)
            depsilch(jrch) = 0.
            IF (depsagch(jrch) >= degremain) THEN
              depsagch(jrch) = depsagch(jrch) - degremain
              deg1sag = degremain
              degremain = 0.
            ELSE
              degremain = degremain - depsagch(jrch)
              deg1sag = depsagch(jrch)
              depsagch(jrch) = 0.
              IF (depsanch(jrch) >= degremain) THEN
                depsanch(jrch) = depsanch(jrch) - degremain
                deg1san = degremain
                degremain = 0.
              ELSE
                degremain = degremain - depsanch(jrch)
                deg1san = depsanch(jrch)
                depsanch(jrch) = 0.
                IF (deplagch(jrch) >= degremain) THEN
                  deplagch(jrch) = deplagch(jrch) - degremain
                  deg1lag = degremain
                  degremain = 0.
                ELSE
                  degremain = degremain - deplagch(jrch)
                  deg1lag = deplagch(jrch)
                  deplagch(jrch) = 0.
                  IF (depgrach(jrch) >= degremain) THEN
                    depgrach(jrch) = depgrach(jrch) - degremain
                    deg1gra = degremain
                    degremain = 0.
                  ELSE
                    degremain = degremain - depgrach(jrch)
                    deg1gra = depgrach(jrch)
                    depgrach(jrch) = 0.
                  END IF
                END IF
              END IF
            END IF
          END IF
        END IF
        
      END IF
      
    END IF
    
    IF (depch(jrch) < 1.e-6) THEN
      depch(jrch) = 0.
      depsanch(jrch) = 0.
      depsilch(jrch) = 0.
      depclach(jrch) = 0.
      depsagch(jrch) = 0.
      deplagch(jrch) = 0.
      depgrach(jrch) = 0.
    END IF
    
!! Fall velocity Based on equation 1.36 from SWRRB manual
    vgra = 411.0 * ((2.00)**2.) / (3600.)
    vsan = 411.0 * ((0.20)**2.) / (3600.)
    vsil = 411.0 * ((0.01)**2.) / (3600.)
    vcla = 411.0 * ((0.002)**2.) / (3600.)
    vsag = 411.0 * ((0.03)**2.) / (3600.)
    vlag = 411.0 * ((0.50)**2.) / (3600.)
    
!! Deposition calculated based on Einstein Equation
    x = 0.
    
!! Gravel deposition
    x = 1.055 * 1000. * ch_l2(jrch) * vgra / (vc * rchdep)
    IF (x > 20.) x = 20.
    pdep = MIN((1. - EXP(-x)), 1.)
    depgra = grain * pdep
    
!! sand deposition
    x = 1.055 * 1000. * ch_l2(jrch) * vsan / (vc * rchdep)
    IF (x > 20.) x = 20.
    pdep = MIN((1. - EXP(-x)), 1.)
    depsan = sanin * pdep
    
!! Silt deposition
    x = 1.055 * 1000. * ch_l2(jrch) * vsil / (vc * rchdep)
    IF (x > 20.) x = 20.
    pdep = MIN((1. - EXP(-x)), 1.)
    
    depsil = silin * pdep
    
!! Clay deposition
    x = 1.055 * 1000. * ch_l2(jrch) * vcla / (vc * rchdep)
    IF (x > 20.) x = 20.
    pdep = MIN((1. - EXP(-x)), 1.)
    
    depcla = clain * pdep
    
!! Small aggregates deposition
    x = 1.055 * 1000. * ch_l2(jrch) * vsag / (vc * rchdep)
    IF (x > 20.) x = 20.
    pdep = MIN((1. - EXP(-x)), 1.)
    
    depsag = sagin * pdep
    
!! Large aggregates deposition
    x = 1.055 * 1000. * ch_l2(jrch) * vlag / (vc * rchdep)
    IF (x > 20.) x = 20.
    pdep = MIN((1. - EXP(-x)), 1.)
    
    deplag = lagin * pdep
    
    dep = depsan + depsil + depcla + depsag + deplag + depgra
    
!!    Particles deposited on Floodplain (only silt and clay type particles)
    depfp(jrch)    = depfp(jrch) + (depsil + depcla) * fpratio
    depsilfp(jrch) = depsilfp(jrch) + depsil * fpratio
    depclafp(jrch) = depclafp(jrch) + depcla * fpratio
    
!!    Remaining is deposited in the channel
    depch(jrch)    = depch(jrch)   + dep - (depsil + depcla)*fpratio
    depsilch(jrch) = depsilch(jrch) + depsil * (1. - fpratio)
    depclach(jrch) = depclach(jrch) + depcla * (1. - fpratio)
    depsanch(jrch) = depsanch(jrch) + depsan
    depsagch(jrch) = depsagch(jrch) + depsag
    deplagch(jrch) = deplagch(jrch) + deplag
    depgrach(jrch) = depgrach(jrch) + depgra
    
    sedin  = sedin + degrte + bnkrte + deg1    - dep
    grain  = grain + deggra + bnkgra + deg1gra - depgra
    sanin  = sanin + degsan + bnksan + deg1san - depsan
    silin  = silin + degsil + bnksil + deg1sil - depsil
    clain  = clain + degcla + bnkcla + deg1cla - depcla
    sagin  = sagin + deg1sag - depsag
    lagin  = lagin + deg1lag - deplag
    
    IF (sedin  < 1.e-6) THEN
      sedin = 0.
      sanin = 0.
      silin = 0.
      clain = 0.
      sagin = 0.
      lagin = 0.
      grain = 0.
    END IF
    
    outfract = rtwtr / qdin
    IF (outfract > 1.) outfract = 1.
    
    sedrch =  sedin * outfract
    rch_san = sanin * outfract
    rch_sil = silin * outfract
    rch_cla = clain * outfract
    rch_sag = sagin * outfract
    rch_lag = lagin * outfract
    rch_gra = grain * outfract
    
    IF (sedrch  < 1.e-6) THEN
      sedrch = 0.
      rch_san = 0.
      rch_sil = 0.
      rch_cla = 0.
      rch_sag = 0.
      rch_lag = 0.
      rch_gra = 0.
    END IF
    
    sedst(jrch) = sedin - sedrch
    sanst(jrch) = sanin - rch_san
    silst(jrch) = silin - rch_sil
    clast(jrch) = clain - rch_cla
    sagst(jrch) = sagin - rch_sag
    lagst(jrch) = lagin - rch_lag
    grast(jrch) = grain - rch_gra
    
    IF (sedst(jrch) < 1.e-6) THEN
      sedst(jrch) = 0.
      sanst(jrch) = 0.
      silst(jrch) = 0.
      clast(jrch) = 0.
      sagst(jrch) = 0.
      lagst(jrch) = 0.
      grast(jrch) = 0.
    END IF
    
!!    Mass balance tests
!!      ambalsed = sedinorg + degrte + bnkrte + deg1 - dep - sedrch       &
!!     &            - sedst(jrch))
!!      ambalsed = depch(jrch) - depsanch(jrch)-depsilch(jrch)            &
!!     &-depclach(jrch)-depsagch(jrch)-deplagch(jrch)-depgrach(jrch)
!!      ambalsed = sedst(jrch) - sanst(jrch)-silst(jrch)-clast(jrch)      &
!!     &-sagst(jrch)-lagst(jrch)-grast(jrch)
!!      ambalsed = (sedin-sanin-silin-clain-sagin-lagin-grain)/sedin
!!      ambalsed = sedrch-rch_san-rch_sil-rch_cla-rch_sag-rch_lag-rch_gra
!!      if (abs(ambalsed) .gt. 1e-3) write (*,*) iida,jrch,ambalsed,sedrch
    
!!    Bank erosion
    rchdy(55,jrch) = bnkrte
!!    Channel Degredation
    rchdy(56,jrch) = degrte
!!    Channel Deposition (Only new deposits during the current time step)
    IF (depch(jrch) >= depprch(jrch)) THEN
      rchdy(57,jrch) = depch(jrch) - depprch(jrch)
    ELSE
      rchdy(57,jrch) = 0.
    END IF
!!    Floodplain Deposition (Only new deposits during the current time step)
    IF (depfp(jrch) >= depprfp(jrch)) THEN
      rchdy(58,jrch) = depfp(jrch) - depprfp(jrch)
    ELSE
      rchdy(58,jrch) = 0.
    END IF
!!    Total suspended sediments (only silt and clay)
    rchdy(59,jrch) = (rch_sil + rch_cla)/rtwtr * 1.e6
    
!!    Deposition during the previous time step
    depprch(jrch) = depch(jrch)  !! Channel
    depprfp(jrch) = depfp(jrch)  !! Flood plain
    
!!    Organic nitrogen and Organic Phosphorus contribution from channel erosion
!!    Only bank erosion is assumed to contribute to channel erosion
!!     ch_orgn(jrch) = bnkrte * ch_onco(jrch) * 1000.
!!     ch_orgp(jrch) = bnkrte * ch_opco(jrch) * 1000.
    ch_orgn(jrch) = bnkrte * ch_onco(jrch) / 1000.
    ch_orgp(jrch) = bnkrte * ch_opco(jrch) / 1000.
    
!! compute changes in channel dimensions
    IF (ideg == 1) THEN
      depdeg = 0.
      depdeg = ch_d(jrch) - ch_di(jrch)
      IF (depdeg < ch_si(jrch) * ch_li(jrch) * 1000.) THEN
        IF (qdin > 1400000.) THEN
          dot = 0.
          dot = 358.6 * rchdep * ch_s(2,jrch) * ch_cov1(jrch)
          dat2 = 1.
          dat2 =  dat2 * dot
          ch_d(jrch) = ch_d(jrch) + dat2
          ch_w(2,jrch) = ch_wdr(jrch) * ch_d(jrch)
          ch_s(2,jrch) = ch_s(2,jrch) - dat2 / (ch_l2(jrch) * 1000.)
          ch_s(2,jrch) = MAX(.0001, ch_s(2,jrch))
          CALL ttcoef(jrch)
        END IF
      END IF
    END IF
    
  ELSE
    
    sedst(jrch) = sedin
    sanst(jrch) = sanin
    silst(jrch) = silin
    clast(jrch) = clain
    sagst(jrch) = sagin
    lagst(jrch) = lagin
    grast(jrch) = grain
    
  END IF !! end of qdin > 0.01 loop
  
END IF  !! end of rtwtr and rchdep > 0 loop

RETURN
END SUBROUTINE rtsed_kodatie
