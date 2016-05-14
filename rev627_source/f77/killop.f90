SUBROUTINE killop
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    curyr        |none          |current year of simulation
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    icr(:)       |none          |sequence number of crop grown within the
!!                                |current year
!!    ihru         |none          |HRU number
!!    ncrops(:,:,:)|
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    nro(:)       |none          |sequence number for year in rotation
!!    nyskip       |none          |number of years to skip output printing/
!!                                |summarization
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    idorm(:)     |none          |dormancy status code:
!!                                |0 land cover growing (not dormant)
!!                                |1 land cover dormant
!!    igro(:)      |none          |land cover status code:
!!                                |0 no land cover currently growing
!!                                |1 land cover growing
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    ncrops(:,:,:)|
!!    phuacc(:)    |none          |fraction of plant heat units accumulated
!!    plantn(:)    |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)    |kg P/ha       |amount of phosphorus in plant biomass
!!    plt_pst(:,:) |kg/ha         |pesticide on plant foliage
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    strsw(:)     |none          |fraction of potential plant growth achieved
!!                                |on the day where the reduction is caused by
!!                                |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, k
REAL :: resnew

!!by zhang
!!====================
REAL :: blg1, blg2, blg3,  clg, sf
REAL :: sol_min_n, resnew_n, resnew_ne
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

!      if (curyr > nyskip) then
!       ncrops(icr(j),j) = ncrops(icr(j),j) + 1
!      endif

!! 22 January 2008
resnew = 0.
rtresnew = 0.
resnew = bio_ms(j) * (1. - rwt(j))
rtresnew = bio_ms(j) * rwt(j)
CALL rootfr

!! update residue, N, P on soil surface
sol_rsd(1,j) = resnew + sol_rsd(1,j)
sol_fon(1,j) = plantn(j) * (1. - rwt(j)) + sol_fon(1,j)
sol_fop(1,j) = plantp(j) * (1. - rwt(j)) + sol_fop(1,j)
sol_rsd(1,j) = MAX(sol_rsd(1,j),0.)
sol_fon(1,j) = MAX(sol_fon(1,j),0.)
sol_fop(1,j) = MAX(sol_fop(1,j),0.)

!!insert new biomss by zhang
!!=================================
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
  clg=blg3*phuacc(j)/(phuacc(j)+EXP(blg1-blg2*phuacc(j)))
  
  
!if (k == 1) then
  sf = 0.05
!else
!sf = 0.1
!end if
  
!kg/ha
  sol_min_n = 0.
  sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
  
  resnew = resnew
  resnew_n = ff1 * (plantn(j) - yieldn)
  resnew_ne = resnew_n + sf * sol_min_n
  
!Not sure 1000 should be here or not!
!RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
  rln = (resnew * clg/(resnew_n+1.e-5))
  rlr = MIN(.8, resnew * clg/(resnew+1.e-5))
  
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
  
  IF (resnew_n >= (0.42 * lsf * resnew /150)) THEN
    sol_lsn(1,j) = sol_lsn(1,j) + 0.42 * lsf * resnew / 150
    sol_lmn(1,j) = sol_lmn(1,j) + resnew_n -  &
        (0.42 * lsf * resnew / 150) + 1.e-25
  ELSE
    sol_lsn(1,j) = sol_lsn(1,j) + resnew_n
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
!!===============================

!! allocate dead roots, N, P to soil layers
DO l=1, sol_nly(j)
  sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
  sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * plantn(j) * rwt(j)
  sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * plantp(j) * rwt(j)
  
!!insert new biomss by zhang
!!==============================
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
    clg=blg3*phuacc(j)/(phuacc(j)+EXP(blg1-blg2*phuacc(j)))
    
    
    IF (l == 1) THEN
      sf = 0.05
    ELSE
      sf = 0.1
    END IF
    
!kg/ha
    sol_min_n = 0.
    sol_min_n = (sol_no3(l,j)+sol_nh3(l,j))
    
    resnew = rtfr(l) * rtresnew
    resnew_n = rtfr(l) *ff2 * (plantn(j) - yieldn)
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
    
    sol_lm(l,j) = sol_lm(l,j) + lmf * resnew
    sol_ls(l,j) = sol_ls(l,j) + lsf * resnew
    
    
    
!here a simplified assumption of 0.5 LSL
!LSLF = 0.0
!LSLF = CLG
    
    sol_lsl(l,j) = sol_lsl(l,j) + rlr*resnew
    sol_lsc(l,j) = sol_lsc(l,j) + 0.42*lsf * resnew
    
    sol_lslc(l,j) = sol_lslc(l,j) + rlr*0.42*resnew
    sol_lslnc(l,j) = sol_lsc(l,j) - sol_lslc(l,j)
    
!X3 = MIN(X6,0.42*LSF * resnew/150)
    
    IF (resnew_ne >= (0.42 * lsf * resnew /150)) THEN
      sol_lsn(l,j) = sol_lsn(l,j) + 0.42 * lsf * resnew / 150
      sol_lmn(l,j) = sol_lmn(l,j) + resnew_ne -  &
          (0.42 * lsf * resnew / 150) + 1.e-25
    ELSE
      sol_lsn(l,j) = sol_lsn(l,j) + resnew_ne
      sol_lmn(l,j) = sol_lmn(l,j) + 1.e-25
    END IF
    
!LSNF = sol_LSN(l,j)/(sol_LS(l,j)+1.E-5)
    
    sol_lmc(l,j) = sol_lmc(l,j) + 0.42 * lmf * resnew
!LMNF = sol_LMN(l,j)/(sol_LM(l,j) + 1.E-5)
    
!update no3 and nh3 in soil
    sol_no3(l,j) = sol_no3(l,j) * (1-sf)
    sol_nh3(l,j) = sol_nh3(l,j) * (1-sf)
  END IF
!!insert new biomss by zhang
!!===============================
  
  
END DO

IF (hrupest(j) == 1) THEN
  DO k = 1, npmx
    sol_pst(k,j,1) = sol_pst(k,j,1) + plt_pst(k,j)
    plt_pst(k,j) = 0.
  END DO
END IF

bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
!bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.

!! reset variables
igro(j) = 0
idorm(j) = 0
bio_ms(j) = 0.
rwt(j) = 0.
plantn(j) = 0.
plantp(j) = 0.
strsw(j) = 1.
laiday(j) = 0.
hvstiadj(j) = 0.
phuacc(j) = 0.
!      phubase(j) = 0.
rtfr = 0. ! Resetting roots fraction per layer array

RETURN
END SUBROUTINE killop
