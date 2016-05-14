SUBROUTINE carbon
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00
 
!! This code simulates organic C, N, and P cycling in the soil
!! The code was developed by Armen R. Kemanian and Stefan Julich
!! It has been adapted from Kemanian and Stockle (2010) (European Journal of Agronomy 32:22-29)
!! and crafted to accomodate to SWAT conventions
!! Plant residues and manure residues are decomposed separately
!! For convenience, the denitrification subroutine is called from here
!! March 2009: testing has been minimal and further adjustments are expected
!! manuscript describing this subroutine to be submitted to Ecological Modelling (September, 2010)
!! use with caution and report anomalous results to akemanian@psu.edu, and jeff.arnold@ars.usda.edu, stefan.julich@tudor.lu


!!!!!!!
!      cmup_kgh       kg/ha    current soil carbon for first soil layer
!      cmtot_kgh      kg/ha    current soil carbon integrated - aggregating
!                                 all soil layers

!! local variables
!! cx = saturated soil carbon concentration (%) (Hassink and Whitmore, 1997)
!! decf = decomposition factor
!! net_N = nitrogen net mineralization
!! net_P = phosphorus net mineralization
!! rnet_N, rnet_P, mnet_N, mnet_P for residue and manure respectively
!! sol_cdec = carbon decomposition (CO2)
!! tilf = tillage factor
!! resc_hum = humified carbon residue
!! manc_hum = humified carbon manure

use parm
 !  private variables
REAL :: cx, decf, rhc, mhc, sol_cdec, tilf
REAL :: resc_hum, manc_hum
REAL :: xx, xx1, xx2, xx3, xx4, csf
REAL :: rdc, mdc, wdn, cdg, sut
REAL :: cnsoil, cpsoil, npsoil
REAL :: cnres, cpres, cnman, cpman, rcnnew, mcnnew
REAL :: sol_thick, sol_mass, sol_cmass, sol_nmass
REAL :: net_n, net_p, rnet_n, rnet_p, mnet_n, mnet_p
REAL :: wc, fc, wf, of, void
 !  mass balance variables
REAL :: sum_c_i, sum_n_i, sum_p_i
REAL :: sum_c_f, sum_n_f, sum_p_f
REAL :: bal_c, bal_n, bal_p
 !  output variables for soil profile
REAL :: cmass_pro, nmass_pro, sol_orgp_pro
REAL :: sol_rsd_pro, sol_fon_pro, sol_fop_pro
REAL :: sol_mc_pro, sol_mn_pro, sol_mp_pro
REAL :: sol_no3_pro, sol_solp_pro, sol_nh3_pro
REAL :: sol_cdec_pro, wdn_pro, net_n_pro, soln_net_min
REAL :: soln_net_min_pro

INTEGER :: j, k, kk
 !  functions
REAL ::fwf, fof, fcdg, ftilf,fcx, fcnnew, fhc, fnetmin


j = 0; wdn = 0
j = ihru
 !  initialize
cmass_pro = 0.
nmass_pro = 0.
sol_orgp_pro = 0.
sol_rsd_pro = 0.
sol_fon_pro = 0.
sol_fop_pro = 0.
sol_mc_pro = 0.
sol_mn_pro = 0.
sol_mp_pro = 0.
sol_no3_pro = 0.
sol_nh3_pro = 0.
sol_solp_pro = 0.
sol_cdec_pro = 0.
wdn_pro = 0.
net_n_pro = 0.
soln_net_min = 0.
soln_net_min_pro=0.
!!    zero new carbon variables for output.hru
cmup_kgh(j) = 0.
cmtot_kgh(j) = 0.
!!    zero new carbon variables for output.hru


IF (sol_cbn(1,j) == 0.) RETURN

DO k = 1, sol_nly(j)
  
  sum_c_i = 0.
  sum_n_i = 0.
  sum_p_i = 0.
  
  rdc = 0.
  mdc = 0.
  rhc  = 0.
  mhc = 0.
  sol_cdec = 0.
  resc_hum = 0.
  manc_hum = 0.
  rnet_n = 0.
  rnet_p = 0.
  mnet_n = 0.
  mnet_p = 0.
  net_n = 0.
  net_p = 0.
  
  rcnnew = 0.
  mcnnew = 0.
  
  wc = 0.
  fc = 0.
  wf = 0.
  of = 0.
  void = 0.
  cdg = 0.
  sut = 0.
  csf = 0.
  
  ffres = 0.
  ffres1 = 0.
  ffres2 = 0.
  
  ffman = 0.
  ffman1 = 0.
  ffman2 = 0.
  
  IF (k == 1) THEN
    sol_thick = sol_z(k,j)
  ELSE
    sol_thick = sol_z(k,j) - sol_z(k-1,j)
  END IF
  
!! soil carbon and nitrogen mass
  sol_mass = (sol_thick / 1000.) * 10000. * sol_bd(k,j)  &
      * 1000. * (1- sol_rock(k,j) / 100.)
  sol_cmass = sol_mass * (sol_cbn(k,j) / 100.)
  sol_nmass = sol_mass * (sol_n(k,j) / 100.)
  
!! sum initial C,N,P for mass balance
  sum_c_i = sol_cmass + 0.43 * sol_rsd (k,j) + sol_mc(k,j)
  sum_n_i = sol_nmass + sol_no3(k,j) + sol_nh3(k,j) + sol_fon(k,j)  &
      + sol_mn(k,j)
  sum_p_i = sol_orgp(k,j) + sol_solp(k,j) + sol_fop(k,j) + sol_mp(k,j)
  
  kk = k
  IF (k == 1) kk = 2
  
  IF (sol_tmp(kk,j) > 0. .AND. sol_st(kk,j) > 0.) THEN
!! microbial processes if temp > 0 C
!! microbial processes if water > pwp
    
!!compute soil water factor - sut
    fc = sol_fc(kk,j) + sol_wpmm(kk,j)  ! units mm
    wc = sol_st(kk,j) + sol_wpmm(kk,j)  ! units mm
    sat = sol_ul(kk,j) + sol_wpmm(kk,j) ! units mm
    void = sol_por(kk,j) * (1. - wc / sat)
    
    wf = fwf(fc,wc,sol_wpmm(kk,j))
    of = fof(void,sol_por(kk,j))
    sut = wf * of
    
!! compute soil temperature factor - cdg
    cdg = fcgd(sol_tmp(kk,j))
    
!! compute combined factor
    xx = 0.
!!  xx = sqrt(cdg * sut)
    xx = (cdg * sut) ** cf(j)
    IF (xx < 0.) xx = 0.
    IF (xx > 1.) xx = 1.
    csf = xx
    
!! call denitrification (to use void and cdg factor)
    wdn = 0.
    IF (cdg > 0. .AND. void <= 0.1) THEN
      CALL ndenit(k,j,cdg,wdn,void)
    END IF
    wshd_dnit = wshd_dnit + wdn * hru_dafr(j)
    wdntl = wdntl + wdn
    
!! calculate soil carbon 'decomposition'
!! tillage factor
    tilf = ftilf(tillagef(k,j), wc, sat)
!! saturated carbon concentration (%)
    cx = fcx(sol_clay(k,j))
!! soil carbon decomposition
    sol_cdec=fsol_cdec(sol_cbn(k,j),cx,cfdec(j), tilf,csf,sol_cmass)
    
!! residue and manure decomposition and N and P mineralization
    cnsoil = sol_cbn(k,j) / sol_n(k,j)
    IF (sol_orgp(k,j) < .01) THEN
      npsoil = 25.
    ELSE
      npsoil = (sol_mass * sol_n(k,j) / 100.)/ sol_orgp(k,j)
    END IF
    sol_orgp(k,j) = AMIN1(sol_orgp(k,j), 25.)
    cpsoil = cnsoil * npsoil
    
    IF (sol_rsd(k,j) > 0.00001) THEN
      
! This IF STATEMENT preserved in case residues with different properties are considered
!if (idplt(nro(j),icr(j),j) > 0) then
!    decr = rsdco_pl(idplt(nro(j),icr(j),j)) * csf
!else
!    decr = 0.05 *  csf
!end if
      
!! residue decomposition
      rdc = 0.05 * csf * sol_rsd(k,j)
      
!! humification factor
      rhc = fhc(sol_clay(k,j),sol_cbn(k,j),cx) * cfh(j)
      
      IF (sol_fon(k,j) < .01) THEN
        cnres = 15.
      ELSE
        cnres = 0.43 * sol_rsd(k,j) / sol_fon(k,j)
      END IF
      cnres = AMIN1(cnres, 15.)
      
      IF (sol_fop(k,j) < .01) THEN
        cpres = 400.
      ELSE
        cpres = 0.43 * sol_rsd(k,j) / sol_fop(k,j)
      END IF
      cpres = AMIN1(cnres, 400.)
      
!! CN of new organic matter (humified residue)
      rcnnew = fcnnew(sol_no3(k,j),sol_mass,cnres, 110.)
      
!! N mineralization / immobilization
      xx1 = sol_rsd(k,j)
      xx2 = sol_no3(k,j) + sol_nh3(k,j)
      rnet_n = fnetmin(rdc,cnres,rcnnew,rhc,ffres1,xx1,xx2,0.43)
      
!! P mineralization / immobilization
      xx2 = sol_solp(k,j)
      rnet_p = fnetmin(rdc,cpres,cpsoil,rhc,ffres2,xx1,xx2,0.43)
      
!! check if P limits N mineralization and re-adjust
      IF (ffres2 < ffres1) THEN
        rnet_n = rdc * 0.43 * (1. / cnres - rhc / rcnnew)
        ffres = ffres2
      ELSE
        ffres = ffres1
      END IF
    END IF
    
! manure decomposition and N and P mineralization
    IF (sol_mc(k,j) > 0.00001) THEN
      
!! residue decomposition
!! decomposition rate about 1/2 of residue
      mdc = 0.025 * csf * sol_mc(k,j)
      
!! humification factor
      mhc = 1.6 * fhc(sol_clay(k,j),sol_cbn(k,j),cx)
      
      cnman = sol_mc(k,j) / sol_mn(k,j)
      cpman = sol_mc(k,j) / sol_mp(k,j)
      
!! CN of new organic matter (humified manure)
      mcnnew = fcnnew(sol_no3(k,j),sol_mass,cnman, 55.)
      
!! N mineralization / immobilization
      xx1 = sol_mc(k,j)
      xx2 = sol_no3(k,j) + sol_nh3(k,j)
      mnet_n = fnetmin(mdc,cnman,mcnnew,mhc,ffman1,xx1,xx2,1.0)
      
!! P mineralization / immobilization
      xx2 = sol_solp(k,j)
      mnet_p = fnetmin(mdc,cpman,cpsoil,mhc,ffman2,xx1,xx2,1.0)
      
!! check if P or N limit mineralization and re-adjust
      IF (ffman2 < ffman1) THEN
        mnet_n = mdc * (1. / cnman - mhc / mcnnew)
        ffman = ffman1
      ELSE
        ffman = ffman2
      END IF
    END IF
    
!! check if sufficient mineral N for both residue and manure decomposition
    IF ((sol_no3(k,j) + sol_nh3(k,j)) > 0.) THEN
      xx = (rnet_n + mnet_n) / (sol_no3(k,j) + sol_nh3(k,j))
      IF (xx < -1.) THEN
        rdc = -rdc / xx
        rnet_n = -rnet_n / xx
        rnet_p = -rnet_p / xx
        ffres = -ffres / xx
        
        mdc = -mdc / xx
        mnet_n = -mnet_n / xx
        mnet_p = -mnet_p / xx
        ffman = -ffman / xx
      END IF
    END IF
    
!! check if sufficient mineral P for both residue and manure decomposition
    IF (sol_solp(k,j) > 0.) THEN
      xx = (rnet_p + mnet_p) / sol_solp(k,j)
      IF (xx < -1.) THEN
        rdc = -rdc / xx
        rnet_n = -rnet_n / xx
        rnet_p = -rnet_p / xx
        ffres = -ffres / xx
        
        mdc = -mdc / xx
        mnet_n = -mnet_n / xx
        mnet_p = -mnet_p / xx
        ffman = -ffman / xx
      END IF
    END IF
    
    resc_hum = rhc * rdc * 0.43
    manc_hum = mhc * mdc
    net_n = rnet_n + mnet_n
    net_p = rnet_p + mnet_p
    IF (resc_hum == 0.) rcnnew = 1000.
    IF (manc_hum == 0.) mcnnew = 1000.
    
!! C N P pools update
    sol_cmass = sol_cmass + resc_hum + manc_hum - sol_cdec
    sol_cbn(k,j) = 100. * sol_cmass / sol_mass
    
    sol_nmass = sol_nmass - sol_cdec / cnsoil  &
        + resc_hum / rcnnew + manc_hum / mcnnew
    
!! april 2010 output net_min
    soln_net_min=-(- sol_cdec / cnsoil  &
        + resc_hum / rcnnew + manc_hum / mcnnew)
    sol_n(k,j) = 100. * sol_nmass / sol_mass
    sol_orgn(k,j) = sol_nmass ! for output
    sol_orgp(k,j) = sol_orgp(k,j) - sol_cdec / cpsoil  &
        + (resc_hum + manc_hum) / cpsoil
    
    IF (ffres > 1.) ffres = 1.
    
    sol_rsd(k,j) = sol_rsd(k,j) * (1. - ffres)
    sol_fon(k,j) = sol_fon(k,j) * (1. - ffres)
    sol_fop(k,j) = sol_fop(k,j) * (1. - ffres)
    
    IF (ffman > 1.) ffman = 1.
    
    sol_mc(k,j) = sol_mc(k,j) * (1. - ffman)
    sol_mn(k,j) = sol_mn(k,j) * (1. - ffman)
    sol_mp(k,j) = sol_mp(k,j) * (1. - ffman)
    
!sol_no3(k,j) = sol_no3(k,j) + net_N +  &
! sol_cdec * (1. / CNsoil)
    
! add positive n-mineralization to ammonia pool in the layer
    IF (rnet_n>0.) sol_nh3(k,j) = sol_nh3(k,j) + rnet_n
    IF (mnet_n>0.) sol_nh3(k,j) = sol_nh3(k,j) + mnet_n
    
    IF (rnet_n<0.) THEN
      IF (ABS(rnet_n) < sol_nh3(k,j)) THEN
        sol_nh3(k,j) = sol_nh3(k,j) + rnet_n
      ELSE
        xx4 = sol_nh3(k,j) + rnet_n
        sol_nh3(k,j) = 0.
        sol_no3(k,j) = sol_no3(k,j) + xx4
      END IF
    END IF
    
    IF (mnet_n<0.) THEN
      IF (ABS(mnet_n) < sol_nh3(k,j)) THEN
        sol_nh3(k,j) = sol_nh3(k,j) + mnet_n
      ELSE
        xx4 = sol_nh3(k,j) + mnet_n
        sol_nh3(k,j) = 0.
        sol_no3(k,j) = sol_no3(k,j) + xx4
      END IF
    END IF
    
    sol_nh3(k,j) = sol_nh3(k,j) + sol_cdec * (1. / cnsoil)
    
    sol_solp(k,j) = sol_solp(k,j) + net_p + sol_cdec * (1. / cpsoil)
    
    wshd_rmn = wshd_rmn + net_n * hru_dafr(j)
    wshd_rmp = wshd_rmp + net_p * hru_dafr(j)
    
    IF (sol_rsd(k,j) < 1E-10) sol_rsd(k,j) = 1E-10
    IF (sol_fon(k,j) < 1E-11) sol_fon(k,j) = 1E-11
    IF (sol_fop(k,j) < 1E-12) sol_fop(k,j) = 1E-12
    IF (sol_mc(k,j) < 1E-10) sol_mc(k,j) = 1E-10
    IF (sol_mn(k,j) < 1E-11) sol_mn(k,j) = 1E-11
    IF (sol_mp(k,j) < 1E-12) sol_mp(k,j) = 1E-12
    IF (sol_no3(k,j) < 1E-12) sol_no3(k,j) = 1E-12
    
  END IF
  
!! balance file cswat_balance
  sum_c_f = sol_cmass + 0.43 * sol_rsd(k,j) + sol_cdec  &
      + (1. - rhc) * rdc * 0.43 + sol_mc(k,j) + (1. - mhc) * mdc
  sum_n_f = sol_nmass + sol_no3(k,j) + sol_nh3(k,j) + sol_fon(k,j)  &
      + sol_mn(k,j) + wdn
  sum_p_f = sol_orgp(k,j) + sol_solp(k,j) + sol_fop(k,j) + sol_mp(k,j)
  
  bal_c = sum_c_i - sum_c_f
  bal_n = sum_n_i - sum_n_f
  bal_p = sum_p_i - sum_p_f
  
!! writing daily output by layer for testing purposes of the routine SJ and AK 2010
!!if (i==365) then
!!    write (98,9000) iyr, i, k, j, sol_cmass, sol_cbn(k,j),
!!     & sol_nmass, sol_n(k,j), sol_orgp(k,j), sol_rsd(k,j),
!!     & sol_fon(k,j), sol_fop(k,j), sol_solp(k,j), sol_mc(k,j),
!!     & sol_mn(k,j), sol_mp(k,j), sol_no3(k,j),
!!     & sol_cmass/sol_nmass, sol_nmass/sol_orgp(k,j), sol_nh3(k,j),
!!     &    tilf, sol_cdec, wdn, net_N
!!        write (99,9001) iyr, i, k, j, bal_c, sum_c_i, sum_c_f,
!!     &     bal_n,sum_n_i, sum_n_f, bal_p, sum_p_i, sum_p_f
!!end if
  
  
!!    carbon outputs for .hru file
  IF (k == 1) cmup_kgh(j) = sol_cmass
  cmtot_kgh(j) = cmtot_kgh(j) + sol_cmass
!!    carbon outputs for .hru file
  
  
  cmass_pro = cmass_pro + sol_cmass
  nmass_pro = nmass_pro + sol_nmass
  sol_rsd_pro = sol_rsd_pro + sol_rsd(k,j)
  sol_cdec_pro = sol_cdec_pro + sol_cdec
  sol_fon_pro = sol_fon_pro + sol_fon(k,j)
  sol_no3_pro = sol_no3_pro + sol_no3(k,j)
  sol_nh3_pro = sol_nh3_pro + sol_nh3(k,j)
  sol_orgp_pro = sol_orgp_pro + sol_orgp(k,j)
  sol_fop_pro = sol_fop_pro + sol_fop(k,j)
  sol_solp_pro = sol_solp_pro + sol_solp(k,j)
  sol_mc_pro = sol_mc_pro + sol_mc(k,j)
  sol_mn_pro = sol_mn_pro + sol_mn(k,j)
  sol_mp_pro = sol_mp_pro + sol_mp(k,j)
  wdn_pro = wdn_pro + wdn
  net_n_pro = net_n_pro + net_n
  soln_net_min_pro = soln_net_min_pro + soln_net_min
  
END DO

!! writing daily profile output
!!if (i==365) then
WRITE (100,9002) iyr, i, j, cmass_pro, sol_rsd_pro, sol_mc_pro


!!end if

!9000  format(i4,';',i3,';',i1,';',i4,20(';',f10.3))
!9001  format(i4,';',i3,';',i1,';',i4,10(';',f10.3))
9002  FORMAT(i4,';',i3,';',i4,';',f11.3,';',f11.3,';',f11.3)

RETURN
END SUBROUTINE


!! LOCAL FUNCTIONS
FUNCTION fwf(fc,wc,pwp)
xx2 = 0.
IF (wc <= pwp) THEN
  xx2 = 0.4 * wc / pwp
ELSE IF (wc <= fc) THEN
  xx2 = 0.4 + 0.6 * (wc - pwp)/(fc - pwp)
ELSE
  xx2 = 1.
END IF

!!fwf = (1. + (1. - xx2) / (1. - 0.75)) * (xx2 / 1.) ** (1./ (1. - 0.75))
fwf = (1. + (1. - xx2) / 0.25) * (xx2) ** 4.
END FUNCTION

FUNCTION fof(void,por)
xx3 = 0.
IF (void >= 0.1) THEN
  xx3 = 0.2 + 0.8 * (void - 0.1) / (por - 0.1)
ELSE
  xx3 = 0.2 * void / 0.1
END IF
fof = 0.5 + 0.5 * xx3 / (xx3 + EXP(-20. * xx3))
END FUNCTION


FUNCTION fcgd(xx)
tn = -5.
top = 35.
tx = 50.
qq = (tn - top)/(top - tx)
fcgd = ((xx-tn)**qq)*(tx-xx)/(((top-tn)**qq)*(tx-top))
IF (fcgd < 0.) fcgd = 0.
END FUNCTION

FUNCTION ftilf(tillage, wc, sat)
!! tillage factor effect on decomposition
!! tillage factor returns to baseline (=1) based on WC
tillage = tillage * (1. - 0.02 * wc/sat)
IF (tillage < 0.) tillage = 0.
ftilf = 1. + tillage
END FUNCTION


FUNCTION fcx(pclay)
!! saturated soil carbon concentration (%) from Hassink and Whitmore 1997
fcx = 2.11 + 0.0375 * pclay
END FUNCTION


FUNCTION fsol_cdec(pcarbon, cx, cfdec, tilf, csf, sol_cmass)
!! decomposition adjustment by current SOC
decf = (pcarbon / cx) ** 0.5
! if (decf > 1.) decf = 1.
!! maximum soil carbon decomposition = 0.045 kg C / kg C per year
fsol_cdec = cfdec / 365. * decf * tilf * csf * sol_cmass
END FUNCTION


FUNCTION fcnnew(yy1,yy2,cnpool,yy5)
!! CN ratio of newly formed organic matter
!! based on CN or decomposing residue and nitrate in soil
!! the same approach used for crop residues and manure
!! CNpool = the CN of decomposing pool
!! yy1 = the layer nitrate mass
!! yy2 = the layer soil mass
!! yy3 = nitrate concentration g/g
!! yy4 = correction factor based on CN of decomposing pool
!! yy5 = input-dependent constant to correct CN ratio

yy3 = yy1 / yy2
yy4 = 5.5 * (1. - 1. / (1. + (cnpool / yy5)**3.))
fcnnew = 8.5 + yy4 * (0.5 + 0.5 / (1. + (yy3 / 8E-6)**3.))
END FUNCTION


FUNCTION fhc(pclay, pcarbon, cx)
!! maximum and actual humification factor
!! hx = maximum humification factor
!! hf = humification adjustment factor
!! pclay = %clay
!! pcarbon = %carbon
!! cx = saturated soil carbon, %

REAL :: hx, hf, pclay, pcarbon

hx = 0.09 + 0.09 * (1. - EXP(-5.5 * pclay / 100.))
!! humification adjustment by current SOC
IF (pcarbon > cx) THEN
  hf = 0.
ELSE
  hf = 1. - (pcarbon / cx) ** 6.
END IF
fhc = hx * hf
END FUNCTION

FUNCTION fnetmin(poold, r1, r2, hc, dummy, poolm, xinorg, cc1)
!! This function computes net mineralization
!! R1 = CN or CP ratio of decomposing pool
!! R2 = CN or CP ratio of receiving pool
!! hc = humification rate
!! dummy = fraction of pool being decomposed
!! poolm = current mass of pool being decomposed
!! poold = mass of pool decomposed
!! xinorg = mass of NO3 or P in solution
!! xx = net mineralization of N or P
!! cc1 = pool's carbon fraction

xx = 0.
xx = poold * cc1 * (1. / r1 - hc / r2)

IF (xx > 0.) THEN
  dummy = poold / poolm
ELSE IF (ABS(xx)< xinorg) THEN
!! net mineralization is positive or
!! immobilization not consuming all mineral N or P
  dummy = poold / poolm
ELSE
!! immobilization, consuming all mineral N or P
  xx = -xinorg
  poold = xx / cc1 * 1. / (1. / r1 - hc / r2)
  dummy = poold / poolm
END IF

fnetmin = xx
END FUNCTION
