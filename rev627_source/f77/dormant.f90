SUBROUTINE dormant
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai_min(:)    |m**2/m**2     |minimum LAI during winter dormant period
!!    bio_leaf(:)    |none          |fraction of biomass that drops during
!!                                  |dormancy (for trees only)
!!    bio_ms(:)      |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:)    |metric tons/ha|annual biomass (dry weight) in the HRU
!!    dayl(:)        |hours         |day length for current day
!!    daylmn(:)      |hours         |shortest daylength occurring during the
!!                                  |year
!!    dormhr(:)      |hour          |time threshold used to define dormant
!!                                  |period for plant (when daylength is within
!!                                  |the time specified by dormhr from the minimum
!!                                  |daylength for the area, the plant will go
!!                                  |dormant)
!!    icr(:)         |none          |sequence number of crop grown within the
!!                                  |current year
!!    idc(:)         |none          |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
!!    idorm(:)       |none          |dormancy status code:
!!                                  |0 land cover growing
!!                                  |1 land cover dormant
!!    idplt(:)       |none          |land cover code from crop.dat
!!    ihru           |none          |HRU number
!!    nro(:)         |none          |sequence number for year in rotation
!!    phuacc(:)      |none          |fraction of plant heat units accumulated
!!    plantn(:)      |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)      |kg P/ha       |amount of phosphorus in plant biomass
!!    pltfr_n(:)     |none          |fraction of plant biomass that is nitrogen
!!    pltfr_p(:)     |none          |fraction of plant biomass that is phosphorus
!!    sol_fon(:,:)   |kg N/ha       |amount of nitrogen stored in the fresh
!!                                  |organic (residue) pool
!!    sol_fop(:,:)   |kg P/ha       |amount of phosphorus stored in the fresh
!!                                  |organic (residue) pool
!!    sol_rsd(:,:)   |kg/ha         |amount of organic matter in the soil
!!                                  |classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha|annual biomass (dry weight) in the HRU
!!    idorm(:)    |none          |dormancy status code:
!!                               |0 land cover growing
!!                               |1 land cover dormant
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant biomass
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    strsw(:)    |none          |fraction of potential plant growth achieved
!!                               |on the day where the reduction is caused by
!!                               |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

REAL :: resnew
INTEGER :: j

!!by zhang
!!====================

REAL :: blg1, blg2, blg3,  clg, sf
REAL :: sol_min_n,  resnew_n, resnew_ne
REAL :: lmf, lsf, lslf, lsnf,lmnf
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

!!by zhang
!!====================

j = 0
j = ihru


!! check for beginning of dormant season
IF (idc(idplt(j)) == 1 .OR. idc(idplt(j)) == 4) RETURN
IF (idorm(j) == 0 .AND. dayl(j)-dormhr(j) < daylmn(hru_sub(j))) THEN
  
  select case (idc(idplt(j)))
  
!! make sure all operations are scheduled during growing season of warm season annual
  case (1,4)
  dorm_flag = 1
  CALL operatn
  dorm_flag = 0
  
!! beginning of forest dormant period
  case (7)
  idorm(j) = 1
  resnew = 0.
  resnew = bio_ms(j) * bio_leaf(idplt(j))
  
!!add by zhang
!!===================
  IF (cswat == 2) THEN
    rsdc_d(j) = rsdc_d(j) + resnew*0.42
  END IF
!!add by zhang
!!===================
  
!!insert new biomss by zhang
!!=============================
  IF (cswat == 2) THEN
!!all the lignin from STD is assigned to LSL,
!!add STDL calculation
!!
!sol_LSL(k,ihru) = sol_STDL(k,ihru)
!CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
!CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
!SUBROUTINE ASCRV(X1,X2,X3,X4)
!EPIC0810
!THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
!USE PARM
!XX=LOG(X3/X1-X3)
!X2=(XX-LOG(X4/X2-X4))/(X4-X3)
!X1=XX+X3*X2
!RETURN
!END
!HUI(JJK)=HU(JJK)/XPHU
    
    blg1 = 0.01/0.10
    blg2 = 0.99
    blg3 = 0.10
    xx = LOG(0.5/blg1-0.5)
    blg2 = (xx -LOG(1./blg2-1.))/(1.-0.5)
    blg1 = xx + 0.5*blg2
    clg=blg3*phuacc(j)/(phuacc(j)+ EXP(blg1-blg2*phuacc(j)))
    
!if (k == 1) then
    sf = 0.05
!else
!sf = 0.1
!end if
    
!kg/ha
    sol_min_n = 0.
    sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
    
    resnew = bio_ms(j) * bio_leaf(idplt(j))
    resnew_n = resnew * pltfr_n(j)
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
!if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
!    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
!else
!    LMF = 0.
!end if
    
    lsf =  1 - lmf
    
    sol_lm(1,j) = sol_lm(1,j) + lmf * resnew
    sol_ls(1,j) = sol_ls(1,j) + lsf * resnew
    
!here a simplified assumption of 0.5 LSL
    lslf = 0.0
    lslf = clg
    
    sol_lsl(1,j) = sol_lsl(1,j) + rlr* lsf * resnew
    sol_lsc(1,j) = sol_lsc(1,j) + 0.42*lsf * resnew
    
    sol_lslc(1,j) = sol_lslc(1,j) + rlr*0.42*lsf * resnew
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
    
!LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)
    
    sol_lmc(1,j) = sol_lmc(1,j) + 0.42 * lmf * resnew
!LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)
    
!update no3 and nh3 in soil
    sol_no3(1,j) = sol_no3(1,j) * (1-sf)
    sol_nh3(1,j) = sol_nh3(1,j) * (1-sf)
  END IF
!!insert new biomss by zhang
!!===========================
  
  
  sol_rsd(1,j) = sol_rsd(1,j) + resnew
  sol_rsd(1,j) = MAX(sol_rsd(1,j),0.)
  sol_fon(1,j) = resnew * pltfr_n(j) + sol_fon(1,j)
  sol_fop(1,j) = resnew * pltfr_p(j) + sol_fop(1,j)
  bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
  bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.
  bio_ms(j) = bio_ms(j) * (1. - bio_leaf(idplt(j)))
  plantn(j) = plantn(j) - resnew * pltfr_n(j)
  plantp(j) = plantp(j) - resnew * pltfr_p(j)
  strsw(j) = 1.
  laiday(j) = alai_min(idplt(j))
  phuacc(j) = 0.
  laimxfr(j) = 0.        !Sue White - dormancy
  ncrops(icr(j),j) = ncrops(icr(j),j) + 1
  
!! beginning of perennial (pasture/alfalfa) dormant period
  case (3, 6)
  idorm(j) = 1
  resnew = 0.
  resnew = bm_dieoff(idplt(j)) * bio_ms(j)
  
!!add by zhang
!!===================
  IF (cswat == 2) THEN
    rsdc_d(j) = rsdc_d(j) + resnew*0.42
  END IF
!!add by zhang
!!===================
  
!!insert new biomss by zhang
!!=============================
  IF (cswat == 2) THEN
!!all the lignin from STD is assigned to LSL,
!!add STDL calculation
!!
!sol_LSL(k,ihru) = sol_STDL(k,ihru)
!CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
!CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
!SUBROUTINE ASCRV(X1,X2,X3,X4)
!EPIC0810
!THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
!USE PARM
!XX=LOG(X3/X1-X3)
!X2=(XX-LOG(X4/X2-X4))/(X4-X3)
!X1=XX+X3*X2
!RETURN
!END
!HUI(JJK)=HU(JJK)/XPHU
    
    blg1 = 0.01/0.10
    blg2 = 0.99
    blg3 = 0.10
    xx = LOG(0.5/blg1-0.5)
    blg2 = (xx -LOG(1./blg2-1.))/(1.-0.5)
    blg1 = xx + 0.5*blg2
    clg=blg3*phuacc(j)/(phuacc(j)+ EXP(blg1-blg2*phuacc(j)))
    
!if (k == 1) then
    sf = 0.05
!else
!sf = 0.1
!end if
    
!kg/ha
    sol_min_n = 0.
    sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
    
    resnew = bm_dieoff(idplt(j)) * bio_ms(j)
    resnew_n = bm_dieoff(idplt(j)) * plantn(j)
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
!if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
!    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
!else
!    LMF = 0.
!end if
    
    lsf =  1 - lmf
    
    sol_lm(1,j) = sol_lm(1,j) + lmf * resnew
    sol_ls(1,j) = sol_ls(1,j) + lsf * resnew
    
    
    
!here a simplified assumption of 0.5 LSL
!LSLF = 0.0
!LSLF = CLG
    
    sol_lsl(1,j) = sol_lsl(1,j) + rlr*resnew
    sol_lsc(1,j) = sol_lsc(1,j) + 0.42*lsf * resnew
    
    sol_lslc(1,j) = sol_lslc(1,j) + rlr*0.42*resnew
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
    
!LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)
    
    sol_lmc(1,j) = sol_lmc(1,j) + 0.42 * lmf * resnew
!LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)
    
!update no3 and nh3 in soil
    sol_no3(1,j) = sol_no3(1,j) * (1-sf)
    sol_nh3(1,j) = sol_nh3(1,j) * (1-sf)
  END IF
!!insert new biomss by zhang
!!===========================
  
  
  sol_rsd(1,j) = sol_rsd(1,j) + resnew
  sol_rsd(1,j) = MAX(sol_rsd(1,j),0.)
  sol_fon(1,j) = sol_fon(1,j) + bm_dieoff(idplt(j)) * plantn(j)
  sol_fop(1,j) = sol_fop(1,j) + bm_dieoff(idplt(j)) * plantp(j)
  bio_hv(icr(j),j) = bio_ms(j) * bm_dieoff(idplt(j)) +  &
      bio_hv(icr(j),j)
  bio_yrms(j) = bio_yrms(j) + bio_ms(j) * bm_dieoff(idplt(j)) / 1000.
  bio_ms(j) = (1. - bm_dieoff(idplt(j))) * bio_ms(j)
  plantn(j) = (1. - bm_dieoff(idplt(j))) * plantn(j)
  plantp(j) = (1. - bm_dieoff(idplt(j))) * plantp(j)
  strsw(j) = 1.
  laiday(j) = alai_min(idplt(j))
  phuacc(j) = 0.
!            ncrops(icr(j),j) = ncrops(icr(j),j) + 1
  
!! beginning of cool season annual dormant period
  case (2, 5)
  IF (phuacc(j) < 0.75) THEN
    idorm(j) = 1
    strsw(j) = 1.
  END IF
END select
IF (imgt == 1) THEN
  WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida,  &
      cpnm(idplt(j)),"START-DORM", phubase(j), phuacc(j),  &
      sol_sw(j),bio_ms(j), sol_rsd(1,j), sol_sumno3(j), sol_sumsolp(j)
END IF

END IF

!! check if end of dormant period
IF (idorm(j) == 1 .AND. dayl(j)-dormhr(j) >= daylmn(hru_sub(j))) THEN
  
  select case (idc(idplt(j)))
  
!! end of perennial dormant period
  case (3, 6, 7)
  idorm(j) = 0
  
!! end of cool season annual dormant period
  case (2, 5)
  idorm(j) = 0
  phuacc(j) = 0.
  
END select

IF (imgt == 1) THEN
  WRITE (143,1000) subnum(j), hruno(j), iyr, i_mo, iida,  &
      cpnm(idplt(j)), "END-DORM", phubase(j), phuacc(j),  &
      sol_sw(j), bio_ms(j), sol_rsd(1,j), sol_sumno3(j), sol_sumsolp(j)
END IF

END IF

1000  FORMAT (a5,1X,a4,3I6,2A15,7F10.2)
RETURN
END SUBROUTINE dormant
