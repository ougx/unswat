SUBROUTINE anfert
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine automatically applies Nitrogen and Phosphorus when
!!    Nitrogen stress exceeds a user input threshhold.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fminn(:)    |kg minN/kg frt|fraction of fertilizer which is mineral
!!                               |nitrogen (NO3 + NH3)
!!    fminp(:)    |kg minP/kg frt|fraction of fertilizer which is mineral
!!                               |phosphorus
!!    fnh3n(:)    |kg NH3-N/kg N |fraction of mineral N content of
!!                               |fertilizer which is NH3
!!    forgn(:)    |kg orgN/kg frt|fraction of fertilizer which is organic
!!                               |nitrogen
!!    forgp(:)    |kg orgP/kg frt|fraction of fertilizer which is organic
!!                               |phosphorus
!!    afrt_surface(:) |none          |fraction of fertilizer which is applied
!!                               |to top 10 mm of soil (the remaining
!!                               |fraction is applied to first soil
!!                               |layer)
!!    auto_nyr(:) |kg NO3-N/ha   |maximum NO3-N content allowed to be
!!                               |applied in one year by auto-fertilization
!!    auto_napp(:)|kg NO3-N/ha   |maximum NO3-N content allowed in one
!!                               |fertilizer application
!!    auto_nstrs(:)|none          |nitrogen stress factor which triggers
!!                               |auto fertilization
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert
!!                               |application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert
!!                               |application
!!    bactkddb(:) |none          |fraction of bacteria in solution (the
!!                               |remaining fraction is sorbed to soil
!!                               |particles)
!!    bactlpdb(:) |# bact/kg frt |concentration of less persistent
!!                               |bacteria in fertilizer
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactpdb(:)  |# bact/kg frt |concentration of persistent bacteria in
!!                               |fertilizer
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    curyr       |none          |current year of simulation
!!    hru_dafr(:) |km**2/km**2   |fraction of watershed area in HRU
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    ihru        |none          |HRU number
!!    nro(:)      |none          |sequence number of year in rotation
!!    nyskip      |none          |number of years of output summarization
!!                               |and printing to skip
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    sol_aorgn(:,:)|kg N/ha     |amount of nitrogen stored in the active
!!                               |organic (humic) nitrogen pool in soil layer
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_nh3(:,:)|kg N/ha       |amount of nitrogen stored in the ammonium
!!                               |pool in soil layer
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the
!!                               |nitrate pool in soil layer
!!    sol_orgp(:,:)|kg P/ha      |amount of phosphorus stored in the organic
!!                               |P pool in soil layer
!!    sol_solp(:,:)|kg P/ha      |amount of phosohorus in solution
!!                               |in soil layer
!!    strsn(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |nitrogen stress
!!    strsp(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |phosphorus stress
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_forgn  |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_forgp  |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anano3(:)   |kg N/ha       |total amount of nitrogen applied during the
!!                               |year in auto-fertilization
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert
!!                               |application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert
!!                               |application
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    sol_aorgn(:,:)|kg N/ha     |amount of nitrogen stored in the active
!!                               |organic (humic) nitrogen pool in soil layer
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_nh3(:,:)|kg N/ha       |amount of nitrogen stored in the ammonium
!!                               |pool in soil layer
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the
!!                               |nitrate pool in soil layer
!!    sol_orgp(:,:)|kg P/ha      |amount of phosphorus stored in the organic
!!                               |P pool in soil layer
!!    sol_solp(:,:)|kg P/ha      |amount of phosohorus stored in solution
!!                               |in soil layer
!!    tauton(:)   |kg N/ha       |amount of N applied in autofert operation in
!!                               |year
!!    tautop(:)   |kg P/ha       |amount of P applied in autofert operation in
!!                               |year
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_forgn  |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_forgp  |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dwfert      |kg fert/ha    |amount of fertilizer to be applied to meet
!!                               |nitrogen requirement
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layers)
!!    nstress     |none          |code for approach used to determine amount
!!                               |of nitrogen to HRU
!!                               |0 nitrogen target approach
!!                               |1 annual max approach
!!    rtoaf       |none          |weighting factor used to partition the
!!                               |organic N & P content of the fertilizer
!!                               |between the fresh organic and the active
!!                               |organic pools
!!    targn       |kg N/ha       |target mineral N application
!!    tfp         |kg minP/kg frt|fraction of mineral P to be applied
!!    tpno3       |
!!    tsno3       |
!!    xx          |none          |fraction of total amount of fertilizer to
!!                               |be applied to layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

REAL, PARAMETER :: rtoaf = 0.50
INTEGER :: j, ly, ifrt
REAL :: tsno3, tpno3, dwfert, xx, targn, tfp

j = 0
j = ihru

ifrt = 0
ifrt = iafrttyp(j)

!! determine amount of mineral N to be applied
IF (strsn(j) < auto_nstrs(j)) THEN
  targn = 0.
  IF (nstress(j) == 0) THEN                !! n target approach
    tsno3 = 0.
    tpno3 = 0.
    DO ly = 1, sol_nly(j)
      tsno3 = tsno3 + sol_no3(ly,j) + sol_nh3(ly,j)
    END DO
    tpno3 = plantn(j)
    
    targn = tnylda(j) - tsno3 - tpno3
    IF (targn > auto_napp(j)) targn = auto_napp(j)
    IF (targn < 0.) targn = 0.
    
    anano3(j) = anano3(j) + targn
    IF (anano3(j) >= auto_nyr(j)) THEN
      targn = auto_nyr(j) - (anano3(j) - targn)
      IF (targn < 0.) targn = 0.
      anano3(j) = auto_nyr(j)
    END IF
    
  ELSE                                  !! annual max approach
    targn = auto_napp(j) * (1. - phuacc(j))
    IF (targn > auto_napp(j)) targn = auto_napp(j)
    
    anano3(j) = anano3(j) + targn
    IF (anano3(j) >= auto_nyr(j)) THEN
      targn = auto_nyr(j) - (anano3(j) - targn)
      anano3(j) = auto_nyr(j)
    END IF
  END IF
  IF (targn <= 1.e-6) RETURN
  
  
!! add nutrients to soil based on nitrogen need
  dwfert = 0.
  IF (fminn(ifrt) > 0.0001) THEN
    dwfert = targn / fminn(ifrt)
  ELSE
    dwfert = 0.
  END IF
  
!! add bacteria to surface layer
  bactpq(j) = bactpq(j) + bactkddb(ifrt) * bactpdb(ifrt) * dwfert
  bactlpq(j) = bactlpq(j) + bactkddb(ifrt) * bactlpdb(ifrt) * dwfert
  bactps(j) = bactps(j) + (1. - bactkddb(ifrt)) * bactpdb(ifrt)* dwfert
  bactlps(j) = bactlps(j) + (1. - bactkddb(ifrt)) *bactlpdb(ifrt) * dwfert
  
  DO ly = 1, 2
    xx = 0.
    IF (ly == 1) THEN
      xx = afrt_surface(j)
    ELSE
      xx = 1. - afrt_surface(j)
    END IF
    
    sol_no3(ly,j) = sol_no3(ly,j) + xx * dwfert * fminn(ifrt) *  &
        (1. - fnh3n(ifrt))
    sol_nh3(ly,j) = sol_nh3(ly,j) + xx * dwfert * fminn(ifrt) * fnh3n(ifrt)
    
    IF (cswat == 0) THEN
      sol_fon(ly,j) = sol_fon(ly,j) + rtoaf * xx * dwfert * forgn(ifrt)
      sol_aorgn(ly,j) = sol_aorgn(ly,j) + (1. - rtoaf) * xx  &
          * dwfert * forgn(ifrt)
      sol_fop(ly,j) = sol_fop(ly,j) + rtoaf * xx * dwfert * forgp(ifrt)
      sol_orgp(ly,j) = sol_orgp(ly,j) + (1. - rtoaf) * xx *  &
          dwfert* forgp(ifrt)
    END IF
    IF (cswat == 1) THEN
      sol_mc(ly,j) = sol_mc(ly,j) + xx * dwfert * forgn(ifrt)*10.
      sol_mn(ly,j) = sol_mn(ly,j) + xx * dwfert * forgn(ifrt)
      sol_mp(ly,j) = sol_mp(ly,j) + xx * dwfert * forgp(ifrt)
    END IF
    
!! add by zhang
!!=================
    IF (cswat == 2) THEN
      sol_fop(ly,j) = sol_fop(ly,j) + rtoaf * xx * dwfert * forgp(ifrt)
      sol_orgp(ly,j) = sol_orgp(ly,j) + (1. - rtoaf) * xx *  &
          dwfert* forgp(ifrt)
!!Allocate organic fertilizer to Slow (SWAT_active) N pool;
      sol_hsn(ly,j) = sol_hsn(ly,j) + (1. - rtoaf) * xx * dwfert * forgn(ifrt)
      sol_aorgn(ly,j) = sol_hsn(ly,j)
      
!orgc_f is the fraction of organic carbon in fertilizer
!for most fertilziers this value is set to 0.
      orgc_f = 0.0
      
!X1 is fertlizer applied to layer (kg/ha)
!xx is fraction of fertilizer applied to layer
      x1 = xx * dwfert
      x8 = x1 * orgc_f
      rln = .175 *(orgc_f)/(fminn(ifrt) + forgn(ifrt) + 1.e-5)
      x10 = .85-.018*rln
      IF (x10<0.01) THEN
        x10 = 0.01
      ELSE
        IF (x10 > .7) THEN
          x10 = .7
        END IF
      END IF
      xxx = x8 * x10
      sol_lmc(ly,j) = sol_lmc(ly,j) + xxx
      yy = x1 * x10
      sol_lm(ly,j) = sol_lm(ly,j) + yy
      
      zz = x1 *rtoaf *forgn(ifrt) * x10
      
      sol_lmn(ly,j) = sol_lmn(ly,j) + zz
      sol_lsn(ly,j) = sol_lsn(ly,j) + x1 *forgn(ifrt) -zz
      xz = x1 *orgc_f-xxx
      sol_lsc(ly,j) = sol_lsc(ly,j) + xz
      sol_lslc(ly,j) = sol_lslc(ly,j) + xz * .175
      sol_lslnc(ly,j) = sol_lslnc(ly,j) + xz * (1.-.175)
      yz = x1 - yy
      sol_ls(ly,j) = sol_ls(ly,j) + yz
      sol_lsl(ly,j) = sol_lsl(ly,j) + yz*.175
      
      sol_fon(ly,j) = sol_lmn(ly,j) + sol_lsn(ly,j)
      
    END IF
!! add by zhang
!!=================
    
!! check for P stress
    tfp = 0.
    IF (strsp(j) <= 0.75) THEN
      tfp = fminn(ifrt) / 7.
    ELSE
      tfp = fminp(ifrt)
    END IF
    sol_solp(ly,j) = sol_solp(ly,j) + xx * dwfert * tfp
  END DO
  
  
!! summary calculations
  auton = auton + dwfert * (fminn(ifrt) + forgn(ifrt))
  autop = autop + dwfert * (tfp + forgp(ifrt))
  tauton(j) = tauton(j) + auton
  tautop(j) = tautop(j) + autop
  IF (curyr > nyskip) THEN
    wshd_ftotn = wshd_ftotn + dwfert * (fminn(ifrt) +  &
        forgn(ifrt))* hru_dafr(j)
    wshd_forgn = wshd_forgn + dwfert * forgn(ifrt) * hru_dafr(j)
    wshd_fno3 = wshd_fno3 + dwfert * fminn(ifrt) *  &
        (1. - fnh3n(ifrt)) * hru_dafr(j)
    wshd_fnh3 = wshd_fnh3 + dwfert * fminn(ifrt) * fnh3n(ifrt) * hru_dafr(j)
    wshd_fminp = wshd_fminp + dwfert * tfp * hru_dafr(j)
    wshd_forgp = wshd_forgp + dwfert * forgp(ifrt) * hru_dafr(j)
  END IF
  
  IF (imgt == 1) THEN
    WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, "         ",  &
        "AUTOFERT", phubase(j), phuacc(j), sol_sw(j),bio_ms(j),  &
        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j), dwfert,  &
        fertno3, fertnh3, fertorgn, fertsolp, fertorgp
  END IF
  
END IF


1000  FORMAT (a5,1X,a4,3I6,2A15,7F10.2,20X,f10.2,10X,5F10.2)

RETURN
END SUBROUTINE
