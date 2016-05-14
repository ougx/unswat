SUBROUTINE sched_mgt
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none          |fertilizer application efficiency calculated
!!                               |as the amount of N applied divided by the
!!                               |amount of N removed at harvest
!!    icpst                      |icpst = 0 do not apply = 1 application period
!!    ipst_freq   |days          |number of days between applications
!!    iday_pest   |day           |current day between applications
!!    ndcpst      |day           |current day within the application period
!!    irramt(:)   |mm H20        |depth of irrigation water applied to HRU
!!    irrsalt(:)  |mg/kg         |concentration of salt in irrigation water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

j = ihru

select case (mgtop(nop(j),j))

case (1)  !! plant operation
igro(j) = 1
lai_init = mgt5op(nop(j),j)
bio_init = mgt6op(nop(j),j)
hi_targ(j) = mgt7op(nop(j),j)
bio_targ(j) = mgt8op(nop(j),j) * 1000.
cnop = mgt9op(nop(j),j)
curyr_mat(j) = mgt3iop(nop(j),j)
IF (curyr_mat(j) == 0) igrotree(j) = 1

idplt(j) = mgt1iop(nop(j),j)

IF (mgt4op(nop(j),j) < 700.) mgt4op(nop(j),j) = 1700.
!            if (mgt4op(nop(j),j) > 5000.) mgt4op(nop(j),j) = 5000.
phu_plt(j) = mgt4op(nop(j),j)

CALL plantop

IF (imgt == 1) THEN
  WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida,  &
      cpnm(idplt(j))," PLANT", phubase(j), phuacc(j), sol_sw(j),  &
      bio_ms(j), sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j)
END IF


case (2)  !! irrigation operation
irr_sc(ihru) = mgt2iop(nop(j),j)     !!NUBZ
irr_no(ihru) = mgt10iop(nop(j),j)
irramt(ihru) = mgt4op(nop(j),j)
irrsalt(ihru) = mgt5op(nop(j),j)
irrefm(ihru) = mgt6op(nop(j),j)
irrsq(ihru) = mgt7op(nop(j),j)
irr_flag(ihru) = 1

IF (irrefm(ihru) < 1.e-6) irrefm(ihru)=1.0
IF (irr_sc(j) <= 0) irr_sc(j) = irrsc(j)
IF (irr_no(j) <= 0) irr_no(j) = irrno(j)
IF (irr_no(j) <= 0) irr_no(j) = hru_sub(j)
IF (irr_sc(ihru) > 2) THEN    !! reach and res flag ??
  CALL irrsub
END IF

IF (imgt ==1) THEN
  WRITE (143, 1002) subnum(j), hruno(j), iyr, i_mo, iida, "        ",  &
      "IRRIGATE", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j),irramt(j), irr_sc(j), irr_no(j)
  1002  FORMAT (a5,1X,a4,3I6,2A15,7F10.2,10X,f10.2,70X,2I7)
  
END IF


case (3)   !! fertilizer operation
ifrttyp = mgt1iop(nop(j),j)
frt_kg = mgt4op(nop(j),j)
frt_surface = mgt5op(nop(j),j)
IF (frt_surface <= 1.e-6) frt_surface = 0.2

CALL fert

IF (imgt ==1) THEN
  WRITE (143, 1004) subnum(j), hruno(j), iyr, i_mo, iida, fertnm(ifrttyp),  &
      "   FERT", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j),frt_kg,  &
      fertno3, fertnh3, fertorgn, fertsolp, fertorgp
  1004  FORMAT (a5,1X,a4,3I6,2A15,7F10.2,20X,f10.2,10X,5F10.2)
END IF


case (4)   !! pesticide operation
hrupest(ihru) = 1
ipest = mgt1iop(nop(j),j)
pst_kg = mgt4op(nop(j),j)
pst_dep = mgt5op(nop(j),j)

CALL apply

IF (imgt ==1) THEN
  WRITE (143, 1004) subnum(j), hruno(j), iyr, i_mo, iida, pname(ipest),  &
      "   PEST", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j),pst_kg
END IF

case (5)   !! harvest and kill operation
cnop = mgt4op(nop(j),j)
hi_ovr = mgt5op(nop(j),j)
frac_harvk = mgt6op(nop(j),j)
biomass = bio_ms(j)

CALL harvkillop

IF (imgt ==1) THEN
  WRITE (143, 1001) subnum(j), hruno(j), iyr, i_mo, iida, cpnm(idplt(j)),  &
      "HARV/KILL", phubase(j), phuacc(j), sol_sw(j),biomass,  &
      sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j),yield,  &
      strsn_sum(j), strsp_sum(j), strstmp_sum(j), strsw_sum(j), strsa_sum(j)
!!1001  format (a5,1x,a4,3i6,2a15,8f10.2,30x,11f10.2)
  1001  FORMAT (a5,1X,a4,3I6,2A15,8F10.2,30X,5F10.2,14X,6F10.2)
END IF

phubase(j) = 0.
phuacc(j) = 0.

case (6)   !! tillage operation
idtill = mgt1iop(nop(j),j)
cnop = mgt4op(nop(j),j)

CALL newtillmix(j,0.)

IF (imgt ==1) THEN
  WRITE (143, 1003) subnum(j), hruno(j),iyr, i_mo, iida, tillnm(idtill),  &
      "TILLAGE", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j), effmix(idtill)
  1003  FORMAT (a5,1X,a4,3I6,2A15,7F10.2,30X,f10.2)
END IF

case (7)  !! harvest only operation
hi_bms = mgt5op(nop(j),j)
hi_rsd = mgt6op(nop(j),j)
harveff = mgt4op(nop(j),j)
IF (harveff <= 0.) harveff = 1.0
CALL harvestop

IF (imgt == 1) THEN
  WRITE (143, 1001) subnum(j), hruno(j), iyr, i_mo, iida, cpnm(idplt(j)),  &
      "HARVEST ONLY", phubase(j), phuacc(j),sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j), sol_sumno3(j), sol_sumsolp(j), yield,  &
      strsn_sum(j), strsp_sum(j), strstmp_sum(j), strsw_sum(j),  &
      strsa_sum(j), yieldgrn, yieldbms, yieldtbr, yieldrsd, yieldn, yieldp
END IF

case (8)   !! kill operation
CALL killop

IF (imgt == 1) THEN
  WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, "         ",  &
      "    KILL", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j)
END IF

phubase(j) = 0.
phuacc(j) = 0.

case (9)    !! grazing operation
manure_id(j) = mgt2iop(nop(j),j)
grz_days(j) = mgt1iop(nop(j),j)
bio_eat(j) = mgt4op(nop(j),j)
bio_trmp(j) = mgt5op(nop(j),j)
manure_kg(j) = mgt6op(nop(j),j)
ndeat(j) = 0
igrz(j) = 1

IF (manure_kg(j) < = 0.) THEN
  manure_kg(j) = 0.95 * mgt4op(nop(j),j)
END IF
CALL graze

IF (imgt == 1) THEN
  WRITE (143, 1005) subnum(j), hruno(j), iyr, i_mo, iida, "         ",  &
      "   GRAZE", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j),manure_kg(j)
  1005  FORMAT (a5,1X,a4,3I6,2A15,7F10.2,20X,f10.2)
END IF

case (10)   !! auto irrigation operation
wstrs_id(j) = mgt1iop(nop(j),j)
auto_wstr(j) = mgt4op(nop(j),j)
irr_eff(j) = mgt5op(nop(j),j)
irr_mx(j) = mgt6op(nop(j),j)
irr_asq(j) = mgt7op(nop(j),j)   !!OGXinSWAT: let it be zero so all water will become precipitation?
irr_sca(j) = mgt2iop(nop(j),j)
irr_noa(j) = mgt10iop(nop(j),j)
IF (irr_noa(j) <= 0) irr_noa(j) = irrno(j)
IF (irr_noa(j) <= 0) irr_noa(j) = hru_sub(j)
IF (wstrs_id(j) <= 0) wstrs_id(j) = 1
IF (irr_eff(j) > 1.) irr_eff(j) = 0.
IF (irr_eff(j) == 0.) irr_eff(j) = 1.
IF (irr_mx(j) < 1.e-6) irr_mx(j) = 25.4
IF (irr_sca(j) <= 0) irr_sca(j) = irrsc(j)
irra_flag(ihru) = 1
IF (imgt ==1) THEN
  WRITE (143, 1010) subnum(j), hruno(j), iyr, i_mo, iida, "        ",  &
      "SCHED AUTORR", phubase(j), phuacc(j), sol_sw(j), bio_ms(j),  &
      sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j)
  1010  FORMAT (a5,1X,a4,3I6,2A15,7F10.2)
END IF


case (11)   !! auto fertilizer operation
iafrttyp(j) = mgt1iop(nop(j),j)
nstress(j) = mgt2iop(nop(j),j)
auto_nstrs(j) = mgt4op(nop(j),j)
auto_napp(j) = mgt5op(nop(j),j)
IF (auto_napp(j) < 1.e-6) auto_napp(j) = 250.
auto_nyr(j) = mgt6op(nop(j),j)
IF (auto_nyr(j) < 1.e-6) auto_nyr(j) = 350.
auto_eff(j) = mgt7op(nop(j),j)
IF (auto_eff(j) <= 0.) auto_eff(j) = 1.3
afrt_surface(j) = mgt8op(nop(j),j)
IF (afrt_surface(j) <= 1.e-6) afrt_surface(j) = .8
!! calculate tnylda for autofertilization
ncrp = idplt(j)
IF (tnylda(j) < 1.e-6)tnylda(j)=150.*cnyld(ncrp)*bio_e(ncrp)
!      if (tnylda(j) < 1.e-6)tnylda(j)=350.*cnyld(ncrp)*bio_e(ncrp)
!         tnylda(j) = 350. * cnyld(ncrp) * bio_e(ncrp)
!        tnylda(j) = 350. * cnyld(ncrp) * bio_e(ncrp)
!       else
!         tnylda(j) = 1000. * cnyld(ncrp) * bio_e(ncrp)
!    endif

case (12)   !! street sweeping (only if iurban=2)

IF (husc > 0.) THEN
  IF (igrow == 1) THEN
    phusw(ihru) = husc
  ELSE
    phusw_nocrop(ihru) = husc
  END IF
END IF
sweepeff = mgt4op(nop(j),j)
fr_curb = mgt5op(nop(j),j)

IF (imgt == 1) THEN
  WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, "         ",  &
      "STREET SWEEP",phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j)
END IF

case (13)    !! release/impound water in rice fields
imp_trig(j) = mgt1iop(nop(j),j)

IF (imgt == 1) THEN
  WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida,  &
      "         ","RELEASE/IMPOUND", phubase(j),phuacc(j),  &
      sol_sw(j),bio_ms(j),sol_rsd(1,j),sol_sumno3(j), sol_sumsolp(j)
END IF

case (14)    !! continuous fertilization operation
fert_days(j) = mgt1iop(nop(j),j)
cfrt_id(j) = mgt2iop(nop(j),j)
ifrt_freq(j) = mgt3iop(nop(j),j)
cfrt_kg(j) = mgt4op(nop(j),j)
icfrt(j) = 1
ndcfrt(j) = 1
iday_fert(j) = ifrt_freq(j)

case (15)    !! continuous pesticide operation
cpst_id(j) = mgt1iop(nop(j),j)
pest_days(j) = mgt2iop(nop(j),j)
ipst_freq(j) = mgt3iop(nop(j),j)
cpst_kg(j) = mgt4op(nop(j),j)
icpst(j) = 1
ndcpst(j) = 0
iday_pest(j) = ipst_freq(j)

case (16)   !! burning
burn_frlb = mgt4op(nop(j),j)
CALL burnop
IF (imgt == 1) THEN
  WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, "         ",  &
      "      BURN", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
      sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j)
END IF

case (17)    !! skip a year
yr_skip(j) = 1

END select

IF (mgtop(nop(j),j) /= 17) THEN
  nop(j) = nop(j) + 1
END IF

IF (nop(j) > nopmx(j)) THEN
  nop(j) = 1
END IF

1000  FORMAT (a5,1X,a4,3I6,2A15,19F10.2)
RETURN

END SUBROUTINE sched_mgt
