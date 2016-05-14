module parm
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01
 
INTEGER :: icalen, prf_bsn

REAL, DIMENSION (:), allocatable :: alph_e
REAL, DIMENSION (:), allocatable :: co_p, surlag

!!   change per JGA 8/31/2011 gsm for output.mgt
REAL :: yield, burn_frlb, pst_kg
REAL :: yieldgrn, yieldbms, yieldtbr, yieldn, yieldp
REAL :: hi_bms, hi_rsd, yieldrsd
!!    arrays for Landscape Transport Capacity 5/28/2009 nadia
REAL, DIMENSION (:), allocatable :: l_k1, l_k2, l_lambda, l_beta
REAL, DIMENSION (:), allocatable :: l_gama, l_harea, l_vleng
REAL, DIMENSION (:), allocatable :: l_vslope, l_ktc

!!    arrays for Biofilm variables
REAL, DIMENSION (:), allocatable :: biofilm_mumax, biofilm_kinv
REAL, DIMENSION (:), allocatable :: biofilm_klw, biofilm_kla
REAL, DIMENSION (:), allocatable :: biofilm_cdet, biofilm_bm


!!    new arrays for routing units
REAL, DIMENSION (:,:), allocatable :: hru_rufr, daru_km, ru_k
REAL, DIMENSION (:,:), allocatable :: ru_c, ru_eiq, ru_ovsl, ru_a
REAL, DIMENSION (:,:), allocatable :: ru_ovs, ru_ktc
REAL, DIMENSION (:), allocatable :: gwq_ru, qdayout
INTEGER, DIMENSION (:), allocatable :: ils2, ils2flag
INTEGER :: iru, mru, irch, isub, idum, mhyd_bsn, ipest, ils_nofig
INTEGER :: mhru1
INTEGER, DIMENSION (:), allocatable :: mhyd1 , irtun

!! septic variables for output.std
REAL :: wshd_sepno3, wshd_sepnh3, wshd_seporgn, wshd_sepfon
REAL :: wshd_seporgp, wshd_sepfop, wshd_sepsolp, wshd_sepbod
REAL :: wshd_sepmm
INTEGER, DIMENSION (:), allocatable :: isep_hru
!! septic variables for output.std
REAL :: fixco, nfixmx, rsd_covco, vcrit, res_stlr_co
REAL :: wshd_sw, wshd_snob, wshd_pndfr, wshd_pndv, wshd_pndsed
REAL :: wshd_wetfr, wshd_resfr, wshd_resha, wshd_pndha, percop
REAL :: wshd_fminp, wshd_ftotn, wshd_fnh3, wshd_fno3, wshd_forgn
REAL :: wshd_forgp, wshd_ftotp, wshd_yldn, wshd_yldp, wshd_fixn
REAL :: wshd_pup, wshd_wstrs, wshd_nstrs, wshd_pstrs, wshd_tstrs
REAL :: wshd_astrs
REAL :: wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_dnit, ffcb
REAL :: wshd_rmp, wshd_voln, wshd_nitn, wshd_pas, wshd_pal, wdpq
REAL :: wshd_plch, wshd_raino3, ressedc, basno3f, basorgnf, wof_p
REAL :: wshd_pinlet, wshd_ptile
REAL :: basminpf, basorgpf, sftmp, smtmp, smfmx, smfmn, wgpq
REAL :: wshd_resv, wshd_ressed, basno3i, basorgni, basminpi, wdlpq
REAL :: basorgpi, peakr, pndsedin, sw_excess, albday, wglpq, wdps
REAL :: wtabelo, timp, tilep, wt_shall
REAL :: sq_rto
REAL :: tloss, inflpcp, snomlt, snofall, fixn, qtile, crk, latlyr
REAL :: pndloss, wetloss,potloss, lpndloss, lwetloss
REAL :: sedrch, fertn, sol_rd, cfertn, cfertp, sepday, bioday
REAL :: sepcrk, sepcrktot, fertno3, fertnh3, fertorgn, fertsolp
REAL :: fertorgp
REAL :: fertp, grazn, grazp, soxy, qdfr, sdti, rtwtr, ressa, wgps
REAL :: rttime, rchdep, rtevp, rttlc, da_km, resflwi, wdlps, wglps
REAL :: resflwo, respcp, resev, ressep,ressedi,ressedo,dtot,wdprch
REAL :: nperco, pperco, rsdco, phoskd, voltot, volcrmin, msk_x
REAL :: uno3d, canev, usle, rcn, surlag_bsn,bactkdq,precipday,wdpf
REAL :: thbact, wpq20, wlpq20, wps20, wlps20, bactrop, bactsedp
REAL :: bactlchp, bactlchlp, enratio, wetpcp, pndpcp, wetsep, wgpf
REAL :: pndsep, wetev, pndev, pndsedo, wetsedo, pndflwi, wetflwi
REAL :: pndflwo, wetflwo, wetsedi, da_ha, twlwet, twlpnd, vpd
REAL :: bactrolp, bactsedlp, evrch, evlai, pet_day, ep_day, wdlpf
REAL :: snoev, sno3up, adj_pkr, n_updis, p_updis, nactfr, reactw
REAL :: sdiegropq, sdiegrolpq, sdiegrops, sdiegrolps, es_day
REAL :: sbactrop, sbactrolp, sbactsedp, sbactsedlp, ep_max, wof_lp
REAL :: sbactlchp, sbactlchlp, psp, rchwtr, resuspst, setlpst
REAL :: bsprev, bssprev, spadyo, spadyev, spadysp, spadyrfv
REAL :: spadyosp
REAL :: qday, usle_ei, al5, pndsedc, no3pcp, rcharea, volatpst
REAL :: wetsedc, uobw, ubw, uobn, uobp, respesti, wglpf
REAL :: snocovmx, snocov1, snocov2, rexp, rcor, lyrtile, lyrtilex
REAL :: ai0, ai1, ai2, ai3, ai4, ai5, ai6, rhoq, tfact, sno50cov
REAL :: mumax, lambda0, lambda1, lambda2, k_l, k_n, k_p, p_n
REAL :: rnum1, autop, auton, etday, hmntl, rwntl, hmptl, rmn2tl
REAL :: rmptl, wdntl, cmn, rmp1tl, roctl, gwseep, revapday, reswtr
REAL :: bury, difus, reactb, solpesto, petmeas, wdlprch, wdpres
REAL :: sorpesto, spcon, spexp, solpesti, sorpesti, wdlpres
REAL :: snoprev, swprev, shallstp, deepstp, msk_co1, msk_co2
REAL :: ressolpo, resorgno, resorgpo, resno3o, reschlao, resno2o
REAL :: resnh3o, qdbank, potpcpmm, potevmm, potsepmm, potflwo
REAL :: potsedo, pest_sol, trnsrch, wp20p_plt, bactminp, bactminlp
REAL :: wp20lp_plt, cncoef, cdn, sdnco, bact_swf, bactmx, bactmin
REAL :: chla_subco, tb_adj, cn_froz, dorm_hr, smxco
REAL :: depimp_bsn, ddrain_bsn, tdrain_bsn, gdrain_bsn
REAL :: rch_san, rch_sil, rch_cla, rch_sag, rch_lag, rch_gra


!!    delcare mike van liew variables
REAL :: hlife_ngw_bsn, ch_opco_bsn, ch_onco_bsn
REAL :: bc1_bsn, bc2_bsn, bc3_bsn, bc4_bsn, rcn_sub_bsn, decr_min
REAL :: anion_excl_bsn
!!    delcare mike van liew variables

!    Drainmod tile equations  01/2006
REAL, DIMENSION (:), allocatable :: wat_tbl,sol_swpwt
REAL, DIMENSION (:,:), allocatable :: vwt
REAL :: re_bsn, sdrain_bsn, sstmaxd_bsn
REAL :: drain_co_bsn, pc_bsn, latksatf_bsn
!    Drainmod tile equations  01/2006
INTEGER :: i_subhw, imgt, idlast, iwtr, ifrttyp, mo_atmo, mo_atmo1
INTEGER :: ifirstatmo, iyr_atmo, iyr_atmo1
INTEGER :: mrg, mch, mcr, mpdb, mcrdb, mfdb, mhru, mhyd, mfcst
INTEGER :: mnr, myr, mcut, mgr, msubo, mrcho, isubwq, ffcst
INTEGER :: nhru, isproj, mo, nbyr, immo, nrch, nres, irte, i_mo
INTEGER :: icode, ihout, inum1, inum2, inum3, inum4, wndsim, ihru
INTEGER :: inum5, inum6, inum7, inum8, icfac
INTEGER :: nrgage, ntgage, nrgfil, ntgfil, nrtot, nttot, mrech
INTEGER :: lao, igropt, npmx, irtpest, curyr, tmpsim, icrk, iihru
!    Drainmod tile equations  01/2006
INTEGER :: ismax, itdrn, iwtdn, iroutunit
!    Drainmod tile equations  01/2006
INTEGER :: mtil, mvaro, mrecd, idist, mudb, mrecm, mrecc, iclb
INTEGER :: mrecy, ipet, nyskip, ideg, ievent, slrsim, iopera
INTEGER :: id1, idaf, idal, leapyr, mo_chk, rhsim, mstdo
INTEGER :: ifirsts, ifirsth, ifirstw, nstot, nhtot, nwtot, icst
INTEGER :: ilog, i, iyr, itotr, iwq, iskip, scenario, ifirstpet
INTEGER :: itotb,itots,iprp,pcpsim,itoth,nd_30,iops,iphr,isto,isol
INTEGER :: iscen, fcstyr, fcstday, fcstcycles, subtot, ogen
INTEGER :: msub, mhruo, mres, mapp, mpst, mlyr, igen, iprint, iida
INTEGER :: fcstcnt, icn, ised_det, mtran, idtill, motot
INTEGER, DIMENSION(100) :: ida_lup, iyr_lup
INTEGER :: no_lup, no_up, nostep
!  routing 5/3/2010 gsm per jga
! date
CHARACTER(LEN=8) :: date
CHARACTER(LEN=10) :: time
CHARACTER(LEN=5) :: zone
CHARACTER(LEN=80) :: prog
CHARACTER(LEN=13) :: slrfile, wndfile, rhfile, petfile, calfile
CHARACTER(LEN=13) :: atmofile, lucfile
CHARACTER(LEN=13) :: septdb
CHARACTER(LEN=13) :: dpd_file, wpd_file, rib_file, sfb_file
INTEGER, DIMENSION (:), allocatable :: ifirstr, idg, ifirsthr
INTEGER, DIMENSION (:), allocatable :: values, ndays
INTEGER, DIMENSION (:), allocatable :: ndays_noleap, ndays_leap
!     apex/command output files
INTEGER :: mapex
REAL, DIMENSION (:), allocatable :: flodaya, seddaya, orgndaya
REAL, DIMENSION (:), allocatable :: orgpdaya, no3daya, minpdaya
REAL, DIMENSION (:), allocatable :: hi_targ, bio_targ, tnyld
INTEGER, DIMENSION (:), allocatable :: idapa, iypa, ifirsta
INTEGER, DIMENSION (:), allocatable :: mo_transb, mo_transe
INTEGER, DIMENSION (:), allocatable :: ih_tran
!     apex/command output files
!  septic inputs
!! septic change added iseptic 1/28/09 gsm
INTEGER :: msdb, iseptic
REAL, DIMENSION (:), allocatable :: sptqs,percp
REAL, DIMENSION (:), allocatable :: sptbodconcs, spttssconcs
REAL, DIMENSION (:), allocatable :: spttnconcs, sptnh4concs
REAL, DIMENSION (:), allocatable :: sptno3concs, sptno2concs
REAL, DIMENSION (:), allocatable :: sptorgnconcs, spttpconcs
REAL, DIMENSION (:), allocatable :: sptminps, sptorgps
REAL, DIMENSION (:), allocatable :: sptfcolis ,failyr,qstemm
!! septic changes added 1/28/09 gsm
REAL, DIMENSION (:), allocatable :: bio_amn, bio_bod, biom,rbiom
REAL, DIMENSION (:), allocatable :: fcoli, bio_ntr, bz_perc
REAL, DIMENSION (:), allocatable :: plqm,sep_cap,bz_area
REAL, DIMENSION (:), allocatable :: bz_z, bz_thk,  bio_bd
!! carbon outputs for .hru file
REAL, DIMENSION (:), allocatable :: cmup_kgh, cmtot_kgh
!! carbon outputs for .hru file
REAL, DIMENSION (:), allocatable :: coeff_bod_dc, coeff_bod_conv
REAL, DIMENSION (:), allocatable :: coeff_fc1, coeff_fc2
REAL, DIMENSION (:), allocatable :: coeff_fecal, coeff_plq
REAL, DIMENSION (:), allocatable :: coeff_mrt, coeff_rsp
REAL, DIMENSION (:), allocatable :: coeff_slg1, coeff_slg2
REAL, DIMENSION (:), allocatable :: coeff_nitr, coeff_denitr
REAL, DIMENSION (:), allocatable :: coeff_pdistrb,coeff_solpslp
REAL, DIMENSION (:), allocatable :: coeff_solpintc,coeff_psorpmax
!! Septic system by Jaehak Jeong
INTEGER, DIMENSION (:), allocatable :: i_sep,isep_typ
INTEGER, DIMENSION (:), allocatable :: isep_opt,sep_tsincefail
INTEGER, DIMENSION (:), allocatable :: isep_tfail,isep_iyr
INTEGER, DIMENSION (:), allocatable :: sep_strm_dist,sep_den

!!   change per JGA 9/8/2011 gsm for output.mgt
REAL, DIMENSION (:), allocatable :: sol_sumno3, sol_sumsolp
REAL, DIMENSION (:), allocatable :: strsw_sum, strstmp_sum
REAL, DIMENSION (:), allocatable :: strsn_sum, strsp_sum
REAL, DIMENSION (:), allocatable :: strsa_sum


!! New pothole variables
REAL, DIMENSION (:), allocatable :: spill_hru,tile_out,hru_in
REAL, DIMENSION (:), allocatable :: spill_precip,pot_seep
REAL, DIMENSION (:), allocatable :: pot_evap,pot_sedin
REAL, DIMENSION (:), allocatable :: pot_solp,pot_solpi
REAL, DIMENSION (:), allocatable :: pot_orgp,pot_orgpi
REAL, DIMENSION (:), allocatable :: pot_orgn,pot_orgni
REAL, DIMENSION (:), allocatable :: pot_mps,pot_mpsi
REAL, DIMENSION (:), allocatable :: pot_mpa,pot_mpai
REAL, DIMENSION (:), allocatable :: pot_no3i,precip_in
REAL, DIMENSION (:), allocatable :: tile_sedo,tile_no3o
REAL, DIMENSION (:), allocatable :: tile_solpo,tile_orgno
REAL, DIMENSION (:), allocatable :: tile_orgpo,tile_minpso
REAL, DIMENSION (:), allocatable :: tile_minpao
! output files
!!  added for binary files 3/25/09 gsm
INTEGER :: ia_b, ihumus, itemp, isnow
INTEGER, DIMENSION (:), allocatable :: icolb,icolr,icolrsv,icols
INTEGER, DIMENSION (:), allocatable :: ipdvar,ipdvab,ipdvas,ipdhru
REAL, DIMENSION (:), allocatable :: wshddayo,wshdmono,wshdyro
REAL, DIMENSION (:), allocatable :: wshdaao,fcstaao
REAL, DIMENSION (:,:), allocatable :: wpstdayo,wpstmono,wpstyro
REAL, DIMENSION (:,:), allocatable :: yldkg, bio_hv
REAL, DIMENSION (:,:), allocatable :: wpstaao,rchmono,rchyro
REAL, DIMENSION (:,:), allocatable :: rchaao,rchdy,hrumono,hruyro
REAL, DIMENSION (:,:), allocatable :: hruaao,submono,subyro,subaao
REAL, DIMENSION (:,:), allocatable :: resoutm,resouty,resouta
REAL, DIMENSION (:,:), allocatable :: wshd_aamon
REAL, DIMENSION (:,:), allocatable :: wtrmon,wtryr,wtraa
REAL, DIMENSION (:,:), allocatable :: sub_smfmx, sub_smfmn
REAL, DIMENSION (:,:,:), allocatable :: hrupstd,hrupsta,hrupstm
REAL, DIMENSION (:,:,:), allocatable :: hrupsty
! mrg = max number of rainfall/temperature gages
INTEGER, DIMENSION (:), allocatable :: ifirstt,ifirstpcp
INTEGER, DIMENSION (:), allocatable :: elevp,elevt
! mfcst = max number of forecast regions
REAL, DIMENSION (:,:), allocatable :: ftmpstdmn,ftmpmn,ftmpmx
REAL, DIMENSION (:,:), allocatable :: ftmpstdmx
REAL, DIMENSION (:,:,:), allocatable :: fpr_w,fpcp_stat
! mch = max number of channels
REAL, DIMENSION (:), allocatable :: flwin,flwout,bankst,ch_wi,ch_d
REAL, DIMENSION (:), allocatable :: ch_onco, ch_opco
REAL, DIMENSION (:), allocatable :: ch_orgn, ch_orgp
REAL, DIMENSION (:), allocatable :: drift,rch_dox,rch_bactp
REAL, DIMENSION (:), allocatable :: alpha_bnk,alpha_bnke
REAL, DIMENSION (:), allocatable :: disolvp,algae,sedst,rchstor
REAL, DIMENSION (:), allocatable :: organicn,organicp,chlora
REAL, DIMENSION (:), allocatable :: nitraten,nitriten,ch_li,ch_si

!      real, dimension (:), allocatable :: ch_cov,ch_di,ch_erod,ch_l2
!      real, dimension (:), allocatable :: ch_san, ch_sil, ch_cla, ch_veg
!      real, dimension (:), allocatable :: ch_rcur, ch_ss, ch_fpr, ch_eqn
!      real, dimension (:), allocatable :: ch_crht

!     Sediment parameters added by Balaji for the new routines

REAL, DIMENSION (:), allocatable :: ch_bnk_san, ch_bnk_sil
REAL, DIMENSION (:), allocatable :: ch_bnk_cla, ch_bnk_gra
REAL, DIMENSION (:), allocatable :: ch_bed_san, ch_bed_sil
REAL, DIMENSION (:), allocatable :: ch_bed_cla, ch_bed_gra
REAL, DIMENSION (:), allocatable :: depfp,depsanfp,depsilfp
REAL, DIMENSION (:), allocatable :: depclafp,depsagfp,deplagfp
REAL, DIMENSION (:), allocatable :: depch,depsanch,depsilch
REAL, DIMENSION (:), allocatable :: depclach,depsagch,deplagch
REAL, DIMENSION (:), allocatable :: depgrach,depgrafp,grast
REAL, DIMENSION (:), allocatable :: depprch,depprfp, prf, r2adj
REAL, DIMENSION (:), allocatable :: sanst,silst,clast,sagst,lagst
REAL, DIMENSION (:), allocatable :: pot_san,pot_sil,pot_cla
REAL, DIMENSION (:), allocatable :: pot_sag,pot_lag
REAL, DIMENSION (:), allocatable :: potsani,potsili,potclai
REAL, DIMENSION (:), allocatable :: potsagi,potlagi
REAL, DIMENSION (:), allocatable :: sanyld,silyld,clayld,sagyld
REAL, DIMENSION (:), allocatable :: lagyld,grayld
REAL, DIMENSION (:), allocatable :: res_san,res_sil,res_cla
REAL, DIMENSION (:), allocatable :: res_sag,res_lag,res_gra
REAL, DIMENSION (:), allocatable :: pnd_san,pnd_sil,pnd_cla
REAL, DIMENSION (:), allocatable :: pnd_sag,pnd_lag
REAL, DIMENSION (:), allocatable :: wet_san,wet_sil,wet_cla
REAL, DIMENSION (:), allocatable :: wet_lag, wet_sag
REAL :: ressano,ressilo,resclao,ressago,reslago, resgrao
REAL :: ressani, ressili, resclai, ressagi, reslagi,resgrai
REAL :: potsano,potsilo,potclao,potsago,potlago
REAL :: pndsanin,pndsilin,pndclain,pndsagin,pndlagin
REAL :: pndsano,pndsilo,pndclao,pndsago,pndlago

REAL, DIMENSION (:), allocatable :: ch_di,ch_erod,ch_l2, ch_cov
REAL, DIMENSION (:), allocatable :: ch_cov1, ch_cov2, ch_bnk_bd
REAL, DIMENSION (:), allocatable :: ch_bed_bd,ch_bnk_kd,ch_bed_kd
REAL, DIMENSION (:), allocatable :: ch_bnk_d50, ch_bed_d50
REAL, DIMENSION (:), allocatable :: tc_bed,tc_bnk
INTEGER, DIMENSION (:), allocatable :: ch_eqn
REAL, DIMENSION (:), allocatable :: chpst_conc,chpst_rea,chpst_vol
REAL, DIMENSION (:), allocatable :: chpst_koc,chpst_stl,chpst_rsp
REAL, DIMENSION (:), allocatable :: chpst_mix,sedpst_conc,ch_wdr
REAL, DIMENSION (:), allocatable :: sedpst_rea,sedpst_bry
REAL, DIMENSION (:), allocatable :: sedpst_act,rch_cbod,rch_bactlp
REAL, DIMENSION (:), allocatable :: chside,rs1,rs2,rs3,rs4,rs5
REAL, DIMENSION (:), allocatable :: rs6,rs7,rk1,rk2,rk3,rk4,rk5
REAL, DIMENSION (:), allocatable :: rk6,bc1,bc2,bc3,bc4,ammonian
REAL, DIMENSION (:), allocatable :: orig_sedpstconc
REAL, DIMENSION (:,:), allocatable :: wurch
INTEGER, DIMENSION (:), allocatable :: icanal
INTEGER, DIMENSION (:), allocatable :: itb
! msub = max number of subbasins
REAL, DIMENSION (:), allocatable :: ch_revap, dep_chan
REAL, DIMENSION (:), allocatable :: harg_petco, subfr_nowtr
REAL, DIMENSION (:), allocatable :: cncoef_sub, dr_sub
REAL, DIMENSION (:), allocatable :: wcklsp,sub_fr,sub_minp,sub_sw
REAL, DIMENSION (:), allocatable :: sub_sumfc,sub_gwno3,sub_gwsolp
REAL, DIMENSION (:), allocatable :: sub_km,sub_tc,wlat,sub_pet,co2
REAL, DIMENSION (:), allocatable :: welev,sub_orgn,sub_orgp,sub_bd
REAL, DIMENSION (:), allocatable :: sub_wtmp,sub_sedpa,sub_sedps
REAL, DIMENSION (:), allocatable :: sub_minpa,sub_minps,daylmn
REAL, DIMENSION (:), allocatable :: latcos,latsin,phutot
REAL, DIMENSION (:), allocatable :: tlaps,plaps,tmp_an,sub_precip
REAL, DIMENSION (:), allocatable :: pcpdays, rcn_sub, rammo_sub
REAL, DIMENSION (:), allocatable :: sub_snom,sub_qd,sub_sedy
REAL, DIMENSION (:), allocatable :: sub_tran,sub_no3,sub_latno3
REAL, DIMENSION (:,:), allocatable :: sub_smtmp,sub_timp,sub_sftmp
REAL, DIMENSION (:), allocatable :: sub_tileno3
REAL, DIMENSION (:), allocatable :: sub_solp,sub_subp,sub_etday
REAL, DIMENSION (:), allocatable :: sub_wyld,sub_surfq,sub_elev
REAL, DIMENSION (:), allocatable :: qird
REAL, DIMENSION (:), allocatable :: sub_gwq,sub_sep,sub_chl
REAL, DIMENSION (:), allocatable :: sub_cbod,sub_dox,sub_solpst
REAL, DIMENSION (:), allocatable :: sub_sorpst,sub_yorgn,sub_yorgp
REAL, DIMENSION (:), allocatable :: sub_bactp,sub_bactlp,sub_lat
REAL, DIMENSION (:), allocatable :: sub_latq, sub_gwq_d,sub_tileq
REAL, DIMENSION (:), allocatable :: sub_dsan, sub_dsil, sub_dcla
REAL, DIMENSION (:), allocatable :: sub_dsag, sub_dlag

!!!!!! drains
REAL, DIMENSION (:), allocatable :: wnan
REAL, DIMENSION (:,:), allocatable :: sol_stpwt
REAL, DIMENSION (:,:), allocatable :: sub_pst,sub_hhqd,sub_hhwtmp
REAL, DIMENSION (:,:), allocatable :: rfinc,tmpinc,radinc,huminc
REAL, DIMENSION (:,:), allocatable :: wndav,ch_k,elevb,elevb_fr
REAL, DIMENSION (:,:), allocatable :: dewpt,ch_w,ch_s,ch_n
REAL, DIMENSION (:,:), allocatable :: amp_r,solarav,tmpstdmx
REAL, DIMENSION (:,:), allocatable :: tmpstdmn,pcf,tmpmn,tmpmx
REAL, DIMENSION (:,:), allocatable :: otmpstdmn,otmpmn,otmpmx
REAL, DIMENSION (:,:), allocatable :: otmpstdmx, ch_erodmo
REAL, DIMENSION (:,:), allocatable :: uh, hqdsave, hsdsave
REAL, DIMENSION (:,:,:), allocatable :: pr_w,pcp_stat
REAL, DIMENSION (:,:,:), allocatable :: opr_w,opcp_stat
INTEGER, DIMENSION (:), allocatable :: hrutot,hru1,ireg
INTEGER, DIMENSION (:), allocatable :: isgage,ihgage,iwgage
INTEGER, DIMENSION (:), allocatable :: irgage,itgage,subgis
INTEGER, DIMENSION (:), allocatable :: fcst_reg, irelh
! mlyr = max number of soil layers
REAL, DIMENSION (:,:), allocatable :: sol_aorgn,sol_tmp,sol_fon
REAL, DIMENSION (:,:), allocatable :: sol_awc,sol_prk,volcr
REAL, DIMENSION (:,:), allocatable :: pperco_sub
REAL, DIMENSION (:,:), allocatable :: sol_actp,sol_stap,conv_wt
REAL, DIMENSION (:,:), allocatable :: sol_solp,sol_ul,sol_fc,crdep
REAL, DIMENSION (:,:), allocatable :: sol_z,sol_up,sol_bd,sol_st
REAL, DIMENSION (:,:), allocatable :: flat,sol_nh3,sol_hk,sol_clay
!  added 1/27/09 when making septic changes
REAL, DIMENSION (:,:), allocatable :: sol_ec
!  added 1/27/09 when making septic changes
REAL, DIMENSION (:,:), allocatable :: sol_orgn,sol_por,sol_wp
REAL, DIMENSION (:,:), allocatable :: sol_orgp,sol_hum,sol_wpmm
REAL, DIMENSION (:,:), allocatable :: sol_k,sol_cbn,sol_no3
REAL, DIMENSION (:,:), allocatable :: sol_rsd,sol_fop
REAL, DIMENSION (:,:), allocatable :: sol_silt, sol_sand, sol_rock
REAL, DIMENSION (:,:), allocatable :: orig_solno3,orig_solorgn
REAL, DIMENSION (:,:), allocatable :: orig_solsolp,orig_solorgp
REAL, DIMENSION (:,:), allocatable :: orig_soltmp,orig_solrsd
REAL, DIMENSION (:,:), allocatable :: orig_solfop,orig_solfon
REAL, DIMENSION (:,:), allocatable :: orig_solaorgn,orig_solst
REAL, DIMENSION (:,:), allocatable :: orig_solactp,orig_solstap
REAL, DIMENSION (:,:), allocatable :: orig_volcr
!    Drainmod tile equations  01/2006
REAL, DIMENSION (:,:), allocatable :: conk
!    Drainmod tile equations  01/2006
REAL, DIMENSION (:,:,:), allocatable :: sol_pst,sol_kp
REAL, DIMENSION (:,:,:), allocatable :: orig_solpst
! mres = max number of reservoirs
REAL, DIMENSION (:), allocatable :: velsetlr, velsetlp
REAL, DIMENSION (:), allocatable :: br1,res_k,lkpst_conc, evrsv
REAL, DIMENSION (:), allocatable :: res_evol,res_pvol,res_vol
REAL, DIMENSION (:), allocatable :: res_psa,lkpst_rea,lkpst_vol
REAL, DIMENSION (:), allocatable :: br2,res_rr,res_sed,lkpst_koc
REAL, DIMENSION (:), allocatable :: lkpst_stl,lkpst_rsp,lkpst_mix
REAL, DIMENSION (:), allocatable :: lkspst_conc,lkspst_rea
REAL, DIMENSION (:), allocatable :: lkspst_bry,lkspst_act,sed_stlr
REAL, DIMENSION (:), allocatable :: wurtnf,res_nsed,resdata,chlar
REAL, DIMENSION (:), allocatable :: res_orgn,res_orgp,res_no3
REAL, DIMENSION (:), allocatable :: res_solp,res_chla,res_seci
REAL, DIMENSION (:), allocatable :: res_esa,seccir,res_no2,res_nh3
REAL, DIMENSION (:), allocatable :: res_bactp, res_bactlp
REAL, DIMENSION (:), allocatable :: oflowmn_fps, starg_fps
REAL, DIMENSION (:), allocatable :: weirc, weirk, weirw
REAL, DIMENSION (:), allocatable :: acoef, bcoef, ccoef
REAL, DIMENSION (:), allocatable :: orig_resvol,orig_ressed
REAL, DIMENSION (:), allocatable :: orig_lkpstconc,orig_lkspstconc
REAL, DIMENSION (:), allocatable :: orig_ressolp,orig_resorgp
REAL, DIMENSION (:), allocatable :: orig_resno3,orig_resno2
REAL, DIMENSION (:), allocatable :: orig_resnh3,orig_resorgn
REAL, DIMENSION (:,:), allocatable :: starg,oflowmx,oflowmn
REAL, DIMENSION (:,:), allocatable :: psetlr,nsetlr,wuresn
REAL, DIMENSION (:,:,:), allocatable :: res_out
INTEGER, DIMENSION (:), allocatable :: ires1,ires2,res_sub
INTEGER, DIMENSION (:), allocatable :: iresco,mores,iyres
INTEGER, DIMENSION (:), allocatable :: iflod1r,iflod2r,ndtargr
! mpdb = max number of pesticides in the database
REAL, DIMENSION (:), allocatable :: skoc,ap_ef,decay_f
REAL, DIMENSION (:), allocatable :: hlife_f,hlife_s,decay_s
REAL, DIMENSION (:), allocatable :: pst_wsol,pst_wof, irramt
REAL, DIMENSION (:), allocatable :: phusw, phusw_nocrop
INTEGER, DIMENSION (:), allocatable :: nope, pstflg, nop
INTEGER, DIMENSION (:), allocatable :: yr_skip, isweep
INTEGER, DIMENSION (:), allocatable :: icrmx, nopmx
! new management scehduling variables
INTEGER, DIMENSION (:,:), allocatable :: mgtop, idop
INTEGER, DIMENSION (:,:), allocatable :: mgt1iop,mgt2iop,mgt3iop
REAL, DIMENSION (:,:), allocatable ::  mgt4op, mgt5op, mgt6op
REAL, DIMENSION (:,:), allocatable :: mgt7op, mgt8op, mgt9op
REAL, DIMENSION (:,:), allocatable :: mgt10iop, phu_op
! mcrdb = maximum number of crops in database
REAL, DIMENSION (:), allocatable :: wac21,wac22,cnyld,rsdco_pl
REAL, DIMENSION (:), allocatable :: wsyf,leaf1,leaf2,alai_min
REAL, DIMENSION (:), allocatable :: t_base,t_opt,hvsti,bio_e
REAL, DIMENSION (:), allocatable :: vpd2,gsi,chtmx,wavp,cvm
REAL, DIMENSION (:), allocatable :: blai,dlai,rdmx,cpyld,bio_leaf
REAL, DIMENSION (:), allocatable :: bio_n1,bio_n2,bio_p1,bio_p2
REAL, DIMENSION (:), allocatable :: bmx_trees,ext_coef,bm_dieoff
REAL, DIMENSION (:), allocatable :: rsr1, rsr2
!     real, dimension (:), allocatable :: air_str
REAL, DIMENSION (:,:), allocatable :: pltnfr,pltpfr
INTEGER, DIMENSION (:), allocatable :: idc, mat_yrs
! mfdb = maximum number of fertilizer in database
REAL, DIMENSION (:), allocatable :: forgn,forgp,fminn,bactpdb
REAL, DIMENSION (:), allocatable :: fminp,fnh3n,bactlpdb,bactkddb
CHARACTER(LEN=8), DIMENSION (200) :: fertnm
! mudb = maximum number of land types in urban database
REAL, DIMENSION (:), allocatable :: fimp,curbden,urbcoef,dirtmx
REAL, DIMENSION (:), allocatable :: thalf,tnconc,tpconc,tno3conc
REAL, DIMENSION (:), allocatable :: fcimp,urbcn2
! mapp = max number of applications
REAL :: sweepeff,frt_kg, pst_dep
!! added pst_dep to statement below 3/31/08 gsm
!!   burn 3/5/09
! mnr = max number years of rotation
!!   burn 3/5/09
! mtil = max number tillages in database  &
!! d ainmod tile equations   06/2006

REAL, DIMENSION (:), allocatable :: ranrns_hru
INTEGER, DIMENSION (:), allocatable :: itill
!! drainmod tile equations   06/2006
REAL, DIMENSION (:), allocatable :: effmix,deptil, ranrns
CHARACTER(LEN=8), DIMENSION (550) :: tillnm
! mhyd = max number of hydrograph nodes
REAL, DIMENSION (:), allocatable :: rnum1s,hyd_dakm
REAL, DIMENSION (:,:), allocatable :: varoute,shyd, vartran
REAL, DIMENSION (:,:,:), allocatable :: hhvaroute
INTEGER, DIMENSION (:), allocatable :: icodes,ihouts,inum1s
INTEGER, DIMENSION (:), allocatable :: inum2s,inum3s,inum4s
INTEGER, DIMENSION (:), allocatable :: inum5s,inum6s,inum7s,inum8s
INTEGER, DIMENSION (:), allocatable :: subed
CHARACTER(LEN=10), DIMENSION (:), allocatable :: recmonps
CHARACTER(LEN=10), DIMENSION (:), allocatable :: reccnstps
CHARACTER(LEN=5), DIMENSION (:), allocatable :: subnum
CHARACTER(LEN=4), DIMENSION (:), allocatable :: hruno

! mhru = maximum number of hydrologic response units
REAL, DIMENSION (:), allocatable :: grwat_n, grwat_i, grwat_l
REAL, DIMENSION (:), allocatable :: grwat_w, grwat_d
REAL, DIMENSION (:), allocatable :: grwat_s, grwat_spcon
REAL, DIMENSION (:), allocatable :: tc_gwat
REAL, DIMENSION (:), allocatable ::pot_volmm,pot_tilemm,pot_volxmm  !!NUBZ
REAL, DIMENSION (:), allocatable :: pot_fr,pot_tile,pot_vol,potsa
REAL, DIMENSION (:), allocatable :: pot_volx,potflwi,potsedi,wfsh
REAL, DIMENSION (:), allocatable :: pot_nsed,pot_no3l,newrti,gwno3
REAL, DIMENSION (:), allocatable :: pot_sed,pot_no3,fsred,tmpavp
REAL, DIMENSION (:), allocatable :: evpot, dis_stream, pot_solpl
REAL, DIMENSION (:), allocatable :: sed_con, orgn_con, orgp_con
REAL, DIMENSION (:), allocatable :: soln_con, solp_con, pot_k
REAL, DIMENSION (:), allocatable :: n_reduc, n_lag, n_ln, n_lnco
INTEGER, DIMENSION (:), allocatable :: ioper
INTEGER, DIMENSION (:), allocatable :: ngrwat
REAL, DIMENSION (:), allocatable :: filterw,sumix,usle_ls,phuacc
REAL, DIMENSION (:), allocatable :: esco,epco,slsubbsn,hru_slp
REAL, DIMENSION (:), allocatable :: erorgn,erorgp,biomix,pnd_seci
REAL, DIMENSION (:), allocatable :: flowmin,divmax,canmx,usle_p
REAL, DIMENSION (:), allocatable :: lat_sed,rch_dakm,pnd_no3s,cn1
REAL, DIMENSION (:), allocatable :: cn2,lat_ttime,flowfr,sol_zmx
REAL, DIMENSION (:), allocatable :: tile_ttime
REAL, DIMENSION (:), allocatable :: slsoil,sed_stl,gwminp,sol_cov
REAL, DIMENSION (:), allocatable :: yldanu,pnd_solp,pnd_no3,ov_n
REAL, DIMENSION (:), allocatable :: driftco,pnd_orgp,pnd_orgn,cn3
REAL, DIMENSION (:), allocatable :: sol_sumul,pnd_chla,hru_fr
REAL, DIMENSION (:), allocatable :: bio_ms,sol_alb,strsw,hru_km
REAL, DIMENSION (:), allocatable :: pnd_fr,pnd_psa,pnd_pvol,pnd_k
REAL, DIMENSION (:), allocatable :: pnd_esa,pnd_evol,pnd_vol,yldaa
REAL, DIMENSION (:), allocatable :: pnd_sed,pnd_nsed,strsa,dep_imp
REAL, DIMENSION (:), allocatable :: evpnd, evwet
REAL, DIMENSION (:), allocatable :: wet_fr,wet_nsa,wet_nvol,wet_k
REAL, DIMENSION (:), allocatable :: wet_mxsa,wet_mxvol,wet_vol
REAL, DIMENSION (:), allocatable :: wet_sed,wet_nsed
REAL, DIMENSION (:), allocatable :: smx,sci,bp1,bp2
REAL, DIMENSION (:), allocatable :: bw1,bw2,bactpq
REAL, DIMENSION (:), allocatable :: bactp_plt,bactlp_plt,cnday
REAL, DIMENSION (:), allocatable :: bactlpq,auto_eff,sol_sw,secciw
REAL, DIMENSION (:), allocatable :: bactps,bactlps,tmpav,chlaw
REAL, DIMENSION (:), allocatable :: subp,sno_hru,hru_ra,wet_orgn
REAL, DIMENSION (:), allocatable :: tmx,tmn,rsdin,tmp_hi,tmp_lo
REAL, DIMENSION (:), allocatable :: rwt,olai,usle_k,tconc,hru_rmx
REAL, DIMENSION (:), allocatable :: usle_cfac,usle_eifac
REAL, DIMENSION (:), allocatable :: anano3,aird,t_ov,sol_sumfc
REAL, DIMENSION (:), allocatable :: sol_avpor,usle_mult,wet_orgp
REAL, DIMENSION (:), allocatable :: aairr,cht,u10,rhd
REAL, DIMENSION (:), allocatable :: shallirr,deepirr,lai_aamx
REAL, DIMENSION (:), allocatable :: canstor,ovrlnd,ch_l1,wet_no3
REAL, DIMENSION (:), allocatable :: irr_mx, auto_wstr
REAL, DIMENSION (:), allocatable :: cfrt_id, cfrt_kg, cpst_id
REAL, DIMENSION (:), allocatable :: cpst_kg
REAL, DIMENSION (:), allocatable :: irr_asq, irr_eff
REAL, DIMENSION (:), allocatable :: irrsq, irrefm, irrsalt
REAL, DIMENSION (:), allocatable :: bio_eat, bio_trmp             !!NUBZ
INTEGER, DIMENSION (:), allocatable :: ifrt_freq,ipst_freq,irr_noa
INTEGER, DIMENSION (:), allocatable :: irr_sc,irr_no
INTEGER, DIMENSION (:), allocatable :: imp_trig, fert_days,irr_sca
INTEGER, DIMENSION (:), allocatable :: pest_days, idplt, wstrs_id
REAL, DIMENSION (:,:), allocatable :: bio_aahv
!    Drainmod tile equations  08/2006
REAL, DIMENSION (:), allocatable :: cumei,cumeira
REAL, DIMENSION (:), allocatable :: cumrt, cumrai
!    Drainmod tile equations  08/2006
REAL, DIMENSION (:), allocatable :: wet_solp,wet_no3s,wet_chla
REAL, DIMENSION (:), allocatable :: wet_seci,pnd_no3g,pstsol
REAL, DIMENSION (:), allocatable :: gwht,delay,gw_q,pnd_solpg
REAL, DIMENSION (:), allocatable :: alpha_bf,alpha_bfe,gw_spyld
REAL, DIMENSION (:), allocatable :: alpha_bf_d,alpha_bfe_d
REAL, DIMENSION (:), allocatable :: gw_qdeep
REAL, DIMENSION (:), allocatable :: gw_delaye,gw_revap,rchrg_dp
REAL, DIMENSION (:), allocatable :: revapmn,anion_excl,rchrg
REAL, DIMENSION (:), allocatable :: ffc,bio_min,surqsolp
REAL, DIMENSION (:), allocatable :: cklsp,deepst,shallst,wet_solpg
REAL, DIMENSION (:), allocatable :: rchrg_src
REAL, DIMENSION (:), allocatable :: wet_no3g,sol_avbd,trapeff
REAL, DIMENSION (:), allocatable :: gwqmn,tdrain,pplnt,snotmp
REAL, DIMENSION (:), allocatable :: ddrain,gdrain,sol_crk,dayl,brt
!    Drainmod tile equations  01/2006
REAL, DIMENSION (:), allocatable ::ddrain_hru,re,sdrain,sstmaxd
REAL, DIMENSION (:), allocatable :: stmaxd,drain_co,pc,latksatf
!    Drainmod tile equations  01/2006
REAL, DIMENSION (:), allocatable :: twash,rnd2,rnd3,sol_cnsw,doxq
REAL, DIMENSION (:), allocatable :: rnd8,rnd9,percn,sol_sumwp
REAL, DIMENSION (:), allocatable :: tauton,tautop,cbodu,chl_a,qdr
REAL, DIMENSION (:), allocatable :: tfertn,tfertp,tgrazn,tgrazp
REAL, DIMENSION (:), allocatable :: latno3,latq,minpgw,no3gw,nplnt
REAL, DIMENSION (:), allocatable :: tileq, tileno3
REAL, DIMENSION (:), allocatable :: sedminpa,sedminps,sedorgn
REAL, DIMENSION (:), allocatable :: sedorgp,sedyld,sepbtm,strsn
REAL, DIMENSION (:), allocatable :: strsp,strstmp,surfq,surqno3
REAL, DIMENSION (:), allocatable :: tcfrtn,tcfrtp,hru_ha,hru_dafr
REAL, DIMENSION (:), allocatable :: drydep_no3, drydep_nh4
REAL, DIMENSION (:), allocatable :: phubase,bio_yrms,hvstiadj
REAL, DIMENSION (:), allocatable :: laimxfr,laiday,chlap,pnd_psed
REAL, DIMENSION (:), allocatable :: wet_psed,seccip,plantn,plt_et
REAL, DIMENSION (:), allocatable :: plt_pet,plantp,bio_aams
REAL, DIMENSION (:), allocatable :: bio_aamx,lai_yrmx,dormhr
REAL, DIMENSION (:), allocatable :: lat_pst
REAL, DIMENSION (:), allocatable :: orig_snohru,orig_potvol,fld_fr
REAL, DIMENSION (:), allocatable :: orig_alai,orig_bioms,pltfr_n
REAL, DIMENSION (:), allocatable :: orig_phuacc,orig_sumix,pltfr_p
REAL, DIMENSION (:), allocatable :: orig_phu, phu_plt
REAL, DIMENSION (:), allocatable :: orig_shallst,orig_deepst
REAL, DIMENSION (:), allocatable :: orig_pndvol,orig_pndsed,rip_fr
REAL, DIMENSION (:), allocatable :: orig_pndno3,orig_pndsolp
REAL, DIMENSION (:), allocatable :: orig_pndorgn,orig_pndorgp
REAL, DIMENSION (:), allocatable :: orig_wetvol,orig_wetsed
REAL, DIMENSION (:), allocatable :: orig_wetno3,orig_wetsolp
REAL, DIMENSION (:), allocatable :: orig_wetorgn,orig_wetorgp
REAL, DIMENSION (:), allocatable :: orig_solcov,orig_solsw
REAL, DIMENSION (:), allocatable :: orig_potno3,orig_potsed
REAL, DIMENSION (:), allocatable :: wtab,wtab_mn,wtab_mx
REAL, DIMENSION (:), allocatable :: shallst_n,gw_nloss,rchrg_n
REAL, DIMENSION (:), allocatable :: det_san, det_sil, det_cla
REAL, DIMENSION (:), allocatable :: det_sag, det_lag
REAL, DIMENSION (:), allocatable :: tnylda, afrt_surface
REAL :: frt_surface
REAL, DIMENSION (:), allocatable :: auto_nyr, auto_napp
REAL, DIMENSION (:), allocatable :: manure_kg, auto_nstrs
REAL, DIMENSION (:,:), allocatable :: rcn_mo, rammo_mo
REAL, DIMENSION (:,:), allocatable :: drydep_no3_mo, drydep_nh4_mo
REAL, DIMENSION (:,:), allocatable :: yldn
REAL, DIMENSION (:,:), allocatable :: gwati, gwatn, gwatl
REAL, DIMENSION (:,:), allocatable :: gwatw, gwatd, gwatveg
REAL, DIMENSION (:,:), allocatable :: gwata, gwats, gwatspcon
REAL, DIMENSION (:,:), allocatable :: rfqeo_30d,eo_30d
REAL, DIMENSION (:,:), allocatable :: wgncur,wgnold,wrt,psetlp
REAL, DIMENSION (:,:), allocatable :: zdb,pst_surq,pst_enr
REAL, DIMENSION (:,:), allocatable :: plt_pst,pst_sed,psetlw
REAL, DIMENSION (:,:), allocatable :: pcpband,wupnd,tavband,phi
REAL, DIMENSION (:,:), allocatable :: wat_phi
REAL, DIMENSION (:,:), allocatable :: wushal,wudeep,tmnband,snoeb
REAL, DIMENSION (:,:), allocatable :: nsetlw,snotmpeb,bss,surf_bs
REAL, DIMENSION (:,:), allocatable :: tmxband,nsetlp
REAL, DIMENSION (:,:), allocatable :: rainsub,frad
REAL, DIMENSION (:),   allocatable ::  rstpbsb
REAL, DIMENSION (:,:), allocatable :: orig_snoeb,orig_pltpst
REAL, DIMENSION (:,:), allocatable :: terr_p, terr_cn, terr_sl
REAL, DIMENSION (:,:), allocatable :: drain_d, drain_t, drain_g
REAL, DIMENSION (:,:), allocatable :: drain_idep
REAL, DIMENSION (:,:), allocatable :: cont_cn, cont_p, filt_w
REAL, DIMENSION (:,:), allocatable :: strip_n, strip_cn, strip_c
REAL, DIMENSION (:,:), allocatable :: strip_p, fire_cn
REAL, DIMENSION (:,:), allocatable :: cropno_upd,hi_upd,laimx_upd
REAL, DIMENSION (:,:,:), allocatable :: pst_lag, phug
!!     integer, dimension (:), allocatable :: ipot,nrelease,swtrg,hrupest
INTEGER, DIMENSION (:), allocatable :: nrelease,swtrg,hrupest
INTEGER, DIMENSION (:), allocatable :: nro,nrot,nfert
INTEGER, DIMENSION (:), allocatable :: igro,nair,ipnd1,ipnd2
INTEGER, DIMENSION (:), allocatable :: nirr,iflod1,iflod2,ndtarg
INTEGER, DIMENSION (:), allocatable :: iafrttyp, nstress
INTEGER, DIMENSION (:), allocatable :: igrotree
!! burn
INTEGER, DIMENSION (:), allocatable :: grz_days
INTEGER, DIMENSION (:), allocatable :: nmgt,icr,ncut,nsweep,nafert
INTEGER, DIMENSION (:), allocatable :: irn,irrno,sol_nly,npcp
INTEGER, DIMENSION (:), allocatable :: igrz,ndeat,ngr,ncf
INTEGER, DIMENSION (:), allocatable :: idorm,urblu,hru_sub,ldrain
INTEGER, DIMENSION (:), allocatable :: hru_seq
INTEGER, DIMENSION (:), allocatable :: iurban,iday_fert,icfrt
INTEGER, DIMENSION (:), allocatable :: ndcfrt,irip,ifld,hrugis
INTEGER, DIMENSION (:), allocatable :: orig_igro,ntil,irrsc
INTEGER, DIMENSION (:), allocatable :: iwatable,curyr_mat
INTEGER, DIMENSION (:), allocatable :: ncpest,icpst,ndcpst
INTEGER, DIMENSION (:), allocatable :: iday_pest, irr_flag
INTEGER, DIMENSION (:), allocatable :: irra_flag
INTEGER, DIMENSION (:,:), allocatable :: rndseed, iterr, iyterr
INTEGER, DIMENSION (:,:), allocatable :: itdrain, iydrain, ncrops
INTEGER, DIMENSION (:), allocatable :: manure_id

!!     gsm added for sdr (drainage) 7/24/08
INTEGER, DIMENSION (:,:), allocatable :: mgt_sdr,idplrot
INTEGER, DIMENSION (:,:), allocatable :: icont, iycont
INTEGER, DIMENSION (:,:), allocatable :: ifilt, iyfilt
INTEGER, DIMENSION (:,:), allocatable :: istrip, iystrip
INTEGER, DIMENSION (:,:), allocatable :: iopday, iopyr, mgt_ops
REAL, DIMENSION (:), allocatable :: wshd_pstap, wshd_pstdg
INTEGER, DIMENSION (:), allocatable :: ndmo,npno,mcrhru
CHARACTER(LEN=13), DIMENSION (18) :: rfile,tfile
!!      character(len=1), dimension (50000) :: hydgrp, kirr  !!for srin's big run

!     character(len=4), dimension (50) :: urbname
CHARACTER(LEN=4), DIMENSION (1000) :: urbname
!!      character(len=16), dimension (50000) :: snam   !! for srin's big runs

CHARACTER(LEN=1), DIMENSION (:), allocatable :: hydgrp, kirr
CHARACTER(LEN=16), DIMENSION (:), allocatable :: snam
CHARACTER(LEN=17), DIMENSION (300) :: pname
!!    adding qtile to output.hru write 3/2/2010 gsm  increased heds(70) to heds(71)
!!    increased hedr(42) to hedr(45) for output.rch gsm 10/17/2011
CHARACTER(LEN=13) :: heds(78),hedb(22),hedr(46),hedrsv(41)
!!      character(len=13) :: heds(73),hedb(21),hedr(42),hedrsv(41)
CHARACTER(LEN=13) :: hedwtr(40)
!     character(len=4) :: title(60), cpnm(250)
CHARACTER(LEN=4) :: title(60), cpnm(5000)
CHARACTER(LEN=17), DIMENSION(50) :: fname
! measured input files
REAL, DIMENSION (:,:,:), allocatable :: flomon,solpstmon,srbpstmon
REAL, DIMENSION (:,:,:), allocatable :: sedmon,orgnmon,orgpmon
REAL, DIMENSION (:,:,:), allocatable :: no3mon,minpmon,nh3mon
REAL, DIMENSION (:,:,:), allocatable :: no2mon,bactpmon,bactlpmon
REAL, DIMENSION (:,:,:), allocatable :: cmtl1mon,cmtl2mon,cmtl3mon
REAL, DIMENSION (:,:,:), allocatable :: chlamon,disoxmon,cbodmon
REAL, DIMENSION (:,:), allocatable :: floyr,sedyr,orgnyr,orgpyr
REAL, DIMENSION (:,:), allocatable :: no3yr,minpyr,nh3yr,no2yr
REAL, DIMENSION (:,:), allocatable :: bactpyr,bactlpyr,cmtl1yr
REAL, DIMENSION (:,:), allocatable :: cmtl2yr,cmtl3yr,chlayr
REAL, DIMENSION (:,:), allocatable :: disoxyr,cbodyr,solpstyr
REAL, DIMENSION (:,:), allocatable :: srbpstyr
REAL, DIMENSION (:,:), allocatable :: sol_mc,sol_mn,sol_mp
REAL, DIMENSION (:), allocatable :: flocnst,sedcnst,orgncnst
REAL, DIMENSION (:), allocatable :: orgpcnst,no3cnst,minpcnst
REAL, DIMENSION (:), allocatable :: nh3cnst,no2cnst,bactpcnst
REAL, DIMENSION (:), allocatable :: cmtl1cnst,cmtl2cnst,bactlpcnst
REAL, DIMENSION (:), allocatable :: cmtl3cnst,chlacnst,disoxcnst
REAL, DIMENSION (:), allocatable :: cbodcnst,solpstcnst,srbpstcnst

! hourly time step (by AVG)
INTEGER :: idt, nstep
REAL, DIMENSION (:), allocatable :: hrtwtr,hhstor,hdepth,hsdti
REAL, DIMENSION (:), allocatable :: hrchwtr,halgae,horgn,hnh4
REAL, DIMENSION (:), allocatable :: hno2,hno3,horgp,hsolp,hbod
REAL, DIMENSION (:), allocatable :: hdisox,hchla,hsedyld,hsedst
REAL, DIMENSION (:), allocatable :: hharea,hsolpst,hsorpst
REAL, DIMENSION (:), allocatable :: hhqday,precipdt
REAL, DIMENSION (:), allocatable :: hhtime,hbactp,hbactlp
! store initial values
INTEGER, DIMENSION (:), allocatable :: ivar_orig
REAL, DIMENSION (:), allocatable :: rvar_orig
! Input Uncertainty, added by Ann van Griensven
INTEGER ::  nauto, nsave, iatmodep
! additional reach variables , added by Ann van Griensven
REAL, DIMENSION (:), allocatable :: wattemp
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
REAL, DIMENSION (:), allocatable :: lkpst_mass, lkspst_mass
REAL, DIMENSION (:), allocatable :: vel_chan
!Additional buffer and filter strip variables Mike White
REAL, DIMENSION (:), allocatable :: vfscon,vfsratio,vfsch,vfsi
REAL, DIMENSION (:,:), allocatable :: filter_i,filter_ratio
REAL, DIMENSION (:,:), allocatable :: filter_con,filter_ch
!! sj, june 07 modifications to carbon balance routines
REAL, DIMENSION (:,:), allocatable :: sol_n
INTEGER :: cswat
!! sj, june 07 end

!! sj, dec 07 dynamic bulk density
REAL, DIMENSION (:,:), allocatable :: sol_bdp
!! sj dec 07 end

!! Armen Jan 08
REAL, DIMENSION (:,:), allocatable :: tillagef
REAL, DIMENSION (:), allocatable :: rtfr
REAL, DIMENSION (:), allocatable :: stsol_rd
!! Armen Jan 08 end
INTEGER:: urban_flag, dorm_flag
REAL :: bf_flg, iabstr
REAL, DIMENSION (:), allocatable :: ubnrunoff,ubntss
REAL, DIMENSION (:,:), allocatable :: sub_ubnrunoff,sub_ubntss, ovrlnd_dt
REAL, DIMENSION (:,:,:), allocatable :: hhsurf_bs

!! subdaily erosion modeling by Jaehak Jeong
INTEGER:: sed_ch,iuh
REAL :: eros_spl, rill_mult, eros_expo, sedprev, c_factor
REAL :: sig_g, ch_d50, uhalpha, abstinit,abstmax
REAL, DIMENSION(:,:), allocatable:: hhsedy, sub_subp_dt
REAL, DIMENSION(:,:), allocatable:: sub_hhsedy,sub_atmp
REAL, DIMENSION(:), allocatable:: rhy,init_abstrc
REAL, DIMENSION(:), allocatable:: dratio, hrtevp, hrttlc
REAL, DIMENSION(:,:,:), allocatable:: rchhr
!! subdaily reservoir modeling by Jaehak Jeong
REAL, DIMENSION(:), allocatable:: hhresflwi, hhresflwo, hhressedi, hhressedo

!! bmp modeling by jaehak jeong
CHARACTER(LEN=4), DIMENSION(:), allocatable:: lu_nodrain
INTEGER, DIMENSION(:), allocatable:: bmpdrain
REAL, DIMENSION(:), allocatable :: sub_cn2, sub_ha_urb, bmp_recharge
!sed-fil
REAL, DIMENSION(:), allocatable:: sub_ha_imp,subdr_km,subdr_ickm
REAL, DIMENSION(:,:), allocatable:: sf_im,sf_iy,sp_sa,  &
    sp_pvol,sp_pd,sp_sedi,sp_sede,ft_sa,ft_fsa,  &
    ft_dep,ft_h,ft_pd,ft_k,ft_dp,ft_dc,ft_por,  &
    tss_den,ft_alp,sf_fr,sp_qi,sp_k,ft_qpnd,sp_dp,  &
    ft_qsw,ft_qin,ft_qout,ft_sedpnd,sp_bpw,ft_bpw, ft_sed_cumul,sp_sed_cumul
INTEGER, DIMENSION(:), allocatable:: num_sf
INTEGER, DIMENSION(:,:), allocatable:: sf_typ,sf_dim,ft_qfg,  &
    sp_qfg,sf_ptp,ft_fc

!detention pond
INTEGER, DIMENSION(:), allocatable :: dtp_subnum,dtp_imo,  &
    dtp_iyr,dtp_numweir,dtp_numstage,dtp_stagdis, dtp_reltype,dtp_onoff
!! sj & armen changes for SWAT-C
REAL, DIMENSION (:), allocatable :: cf, cfh, cfdec
!! sj & armen changes for SWAT-C end
! additional nutrient variables by jeong for montana bitterroot
REAL, DIMENSION(:), allocatable :: lat_orgn, lat_orgp

INTEGER, DIMENSION(:,:), allocatable :: dtp_weirtype,dtp_weirdim

REAL, DIMENSION(:), allocatable ::dtp_evrsv,  &
    dtp_inflvol,dtp_totwrwid,dtp_lwratio,dtp_wdep,dtp_totdep,  &
    dtp_watdepact,dtp_outflow,dtp_totrel,dtp_backoff,dtp_seep_sa,  &
    dtp_evap_sa,dtp_pet_day,dtp_pcpvol,dtp_seepvol,dtp_evapvol,  &
    dtp_flowin,dtp_backup_length,dtp_intcept,dtp_expont,dtp_coef1,  &
    dtp_coef2,dtp_coef3,dtp_dummy1,dtp_dummy2, dtp_dummy3,dtp_ivol,dtp_ised

INTEGER, DIMENSION (:,:),allocatable :: so_res_flag, ro_bmp_flag
REAL, DIMENSION (:,:),allocatable :: sol_watp, sol_solp_pre
REAL, DIMENSION (:,:),allocatable :: psp_store, ssp_store, so_res
REAL, DIMENSION (:,:),allocatable :: sol_cal, sol_ph
INTEGER:: sol_p_model
INTEGER, DIMENSION (:,:),allocatable :: a_days, b_days
REAL, DIMENSION (:), allocatable :: harv_min, fstap, min_res
REAL, DIMENSION (:,:),allocatable :: ro_bmp_sed, ro_bmp_bac
REAL, DIMENSION (:,:),allocatable :: ro_bmp_pp, ro_bmp_sp
REAL, DIMENSION (:,:),allocatable :: ro_bmp_pn, ro_bmp_sn
REAL, DIMENSION (:),allocatable :: bmp_sed, bmp_bac
REAL, DIMENSION (:),allocatable :: bmp_pp, bmp_sp
REAL, DIMENSION (:),allocatable :: bmp_pn, bmp_sn, bmp_flag
REAL, DIMENSION(:,:), allocatable:: dtp_wdratio,dtp_depweir,  &
    dtp_diaweir,dtp_retperd,dtp_pcpret,dtp_cdis,dtp_flowrate, dtp_wrwid,dtp_addon
!!    added for manure Armen Jan 2009
!!     real, dimension (:,:), allocatable :: sol_mc, sol_mn, sol_mp

!retention irrigation
REAL, DIMENSION(:), allocatable:: ri_subkm,ri_totpvol, irmmdt
REAL, DIMENSION(:,:), allocatable:: ri_sed,ri_fr,ri_dim,  &
    ri_im,ri_iy,ri_sa,ri_vol,ri_qi,ri_k,ri_dd,ri_evrsv,  &
    ri_dep,ri_ndt,ri_pmpvol,ri_sed_cumul,hrnopcp,ri_qloss, ri_pumpv,ri_sedi
CHARACTER(LEN=4), DIMENSION(:,:), allocatable:: ri_nirr
INTEGER, DIMENSION(:), allocatable:: num_ri,ri_luflg,num_noirr

!wet pond
INTEGER, DIMENSION(:), allocatable:: wtp_subnum,wtp_onoff,wtp_imo,  &
    wtp_iyr,wtp_dim,wtp_stagdis,wtp_sdtype
REAL, DIMENSION(:), allocatable:: wtp_pvol,wtp_pdepth,wtp_sdslope,  &
    wtp_lenwdth,wtp_extdepth,wtp_hydeff,wtp_evrsv,wtp_sdintc,  &
    wtp_sdexp,wtp_sdc1,wtp_sdc2,wtp_sdc3,wtp_pdia,wtp_plen,  &
    wtp_pmann,wtp_ploss,wtp_k,wtp_dp,wtp_sedi,wtp_sede,wtp_qi

REAL :: bio_init, lai_init, cnop,hi_ovr,harveff,frac_harvk


!! By Zhang for C/N cycling
!!SOM-residue C/N state variables -- currently included
REAL, DIMENSION(:,:), allocatable :: sol_bmc, sol_bmn, sol_hsc,  &
    sol_hsn, sol_hpc, sol_hpn, sol_lm,  &
    sol_lmc, sol_lmn, sol_ls, sol_lsl, sol_lsc, sol_lsn , sol_rnmn,  &
    sol_lslc, sol_lslnc, sol_rspc, sol_woc, sol_won, sol_hp, sol_hs, sol_bm
! HSC mass of C present in slow humus (kg ha-1)
! HSN mass of N present in slow humus (kg ha-1)
! HPC mass of C present in passive humus (kg ha-1)
! HPN mass of N present in passive humus (kg ha-1)
! LM mass of metabolic litter (kg ha-1)
! LMC mass of C in metabolic litter (kg ha-1)
! LMN mass of N in metabolic litter (kg ha-1)
! LS mass of structural litter (kg ha-1)
! LSC mass of C in structural litter (kg ha-1)
! LSL mass of lignin in structural litter (kg ha-1)
! LSN mass of N in structural litter (kg ha-1)

!!SOM-residue C/N state variables -- may need to be included
REAL, DIMENSION(:,:), allocatable :: sol_cac, sol_cec

!!daily updated soil layer associated percolaton and lateral flow Carbon loss
REAL, DIMENSION(:,:), allocatable :: sol_percc
REAL, DIMENSION(:,:), allocatable :: sol_latc

!!Daily carbon change by different means (entire soil profile for each HRU)
REAL, DIMENSION(:), allocatable :: sedc_d, surfqc_d, latc_d,  &
    percc_d, foc_d, nppc_d, rsdc_d, grainc_d, stoverc_d, soc_d, rspc_d, emitc_d
!!emitc_d include biomass_c eaten by grazing, burnt


!!Daily carbon change by different means (entire soil profile for each Subbasin)
!!Only defined the variables, but not used them in the code
REAL, DIMENSION(:), allocatable :: sub_sedc_d, sub_surfqc_d,  &
    sub_latc_d, sub_percc_d, sub_foc_d, sub_nppc_d, sub_rsdc_d,  &
    sub_grainc_d, sub_stoverc_d, sub_emitc_d, sub_soc_d, sub_rspc_d


!!Monthly carbon change by different means (entire soil profile for each HRU)
REAL, DIMENSION(:), allocatable :: sedc_m, surfqc_m, latc_m,  &
    foc_m, nppc_m, rsdc_m, grainc_m, stoverc_m, emitc_m, soc_m, percc_m, rspc_m

!!Yearly carbon change by different means (entire soil profile for each HRU)
REAL, DIMENSION(:), allocatable :: sedc_a, surfqc_a, latc_a,  &
    percc_a, foc_a, nppc_a, rsdc_a, grainc_a, stoverc_a, emitc_a, soc_a, rspc_a


!! The following variables are defined and calculated locally
!! ==================================================================
! HSCTP potential transformation of C in slow humus (kg ha-1 day-1)
! HSNTP potential transformation of N in slow humus (kg ha.1 day-1)
! HPCTP potential transformation of C in passive humus (kg ha-1 day-1)
! HPNTP potential transformation of N in passive humus (kg ha-1 day-1)
! HPR rate of transformation of passive humus under optimal conditions (subsurface
! layers = 0.000012 day-1) (Parton et al.,1993, 1994)
! HSR rate of transformation of slow humus under optimal conditions (all layers
! = 0.0005 day.1) (Parton et al., 1993, 1994; Vitousek et al., 1993)
! KOC liquid C solid partition coefficient for microbial biomass (10^3 m3 Mg-1)
! LMF fraction of the litter that is metabolic
! LMNF fraction of metabolic litter that is N (kg kg-1)
! LMR rate of transformation of metabolic litter under optimal conditions (surface =
!  0.0405 day-1; all other layers = 0.0507 day-1) (Parton et al., 1994)
! LMCTP potential transformation of C in metabolic litter (kg ha-1 day-1)
! LMNTP potential transformation of N in metabolic litter (kg ha-1 day-1)
! LSCTP potential transformation of C in structural litter (kg ha-1 day-1)
! LSF fraction of the litter that is structural
! LSLF fraction of structural litter that is lignin (kg kg-1)
! LSNF fraction of structural litter that is N (kg kg-1)
! LSLCTP potential transformation of C in lignin of structural litter (kg ha-1 day-1)
! LSLNCTP potential transformation of C in nonlignin structural litter (kg ha-1 day-1)
! LSNTP potential transformation of N in structural litter (kg ha-1 day-1)
! LSR rate of potential transformation of structural litter under optimal conditions
! (surface = 0.0107 day.1; all other layers = 0.0132 day.1) (Parton et al., 1994)
! NCBM N/C ratio of biomass
! NCHP N/C ratio passive humus
! NCHS N/C ratio of the slow humus
! OX oxygen control on biological processes with soil depth
! Sf fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter

!!Tillage factor on SOM decomposition
INTEGER, DIMENSION(:), allocatable :: tillage_switch
REAL, DIMENSION(:), allocatable :: tillage_depth
INTEGER, DIMENSION(:), allocatable :: tillage_days
REAL, DIMENSION(:), allocatable :: tillage_factor
! tillage_factor: = 1.6 in 30 days after tillage practices occur; otherwise 1.0;
!! By Zhang for C/N cycling



END module parm
