SUBROUTINE readpnd
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the HRU/subbasin pond input file (.pnd).
!!    This file contains data related to ponds and wetlands in the
!!    HRUs/subbasins.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrutot(:)   |none          |number of HRUs in subbasin
!!    i           |none          |subbasin number
!!    ihru        |none          |HRU number
!!    nhru        |none          |number of last HRU in previous subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlap(:)    |none          |chlorophyll-a production coefficient for pond
!!    chlaw(:)    |none          |chlorophyll-a production coefficient for
!!                               |wetland
!!    iflod1(:)   |none          |beginning month of non-flood season
!!    iflod2(:)   |none          |ending month of non-flood season
!!    ipnd1(:)    |none          |beginning month of nutrient settling season
!!    ipnd2(:)    |none          |ending month of nutrient settling season
!!    ndtarg(:)   |none          |number of days required to reach target
!!                               |storage from current pond storage
!!    nsetlp(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlp(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    nsetlw(1,:) |m/day         |nitrogen settling rate for 1st season
!!    nsetlw(2,:) |m/day         |nitrogen settling rate for 2nd season
!!    pnd_esa(:)  |ha            |surface area of ponds when filled to
!!                               |emergency spillway
!!    pnd_evol(:) |10**4 m**3 H2O|runoff volume from catchment area needed
!!                               |to fill the ponds to the emergency spillway
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into ponds
!!    pnd_k(:)    |mm/hr         |hydraulic conductivity through bottom of
!!                               |ponds
!!    pnd_no3(:)  |kg N          |amount of nitrate in pond
!!    pnd_nsed(:) |mg/L          |normal sediment concentration in pond water
!!    pnd_orgn(:) |kg N          |amount of organic N in pond
!!    pnd_orgp(:) |kg P          |amount of organic P in pond
!!    pnd_psa(:)  |ha            |surface area of ponds when filled to
!!                               |principal spillway
!!    pnd_pvol(:) |10**4 m**3 H2O|runoff volume from catchment area needed to
!!                               |fill the ponds to the principal spillway
!!    pnd_sed(:)  |mg/L          |sediment concentration in pond water
!!    pnd_solp(:) |kg P          |amount of soluble P in pond
!!    pnd_vol(:)  |10**4 m**3 H2O|volume of water in ponds
!!    psetlp(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlp(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    psetlw(1,:) |m/day         |phosphorus settling rate for 1st season
!!    psetlw(2,:) |m/day         |phosphorus settling rate for 2nd season
!!    seccip(:)   |none          |water clarity coefficient for pond
!!    secciw(:)   |none          |water clarity coefficient for wetland
!!    wet_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into wetlands
!!    wet_k(:)    |mm/hr         |hydraulic conductivity of bottom of wetlands
!!    wet_mxsa(:) |ha            |surface area of wetlands at maximum water
!!                               |level
!!    wet_mxvol(:)|10**4 m**3 H2O|runoff volume from catchment area needed to
!!                               |fill wetlands to maximum water level
!!    wet_no3(:)  |kg N          |amount of nitrate in wetland
!!    wet_nsa(:)  |ha            |surface area of wetlands in subbasin at
!!                               |normal water level
!!    wet_nsed(:) |mg/L          |normal sediment concentration in wetland
!!                               |water
!!    wet_nvol(:) |10**4 m**3 H2O|runoff volume from catchment area needed to
!!                               |fill wetlands to normal water level
!!    wet_orgn(:) |kg N          |amount of organic N in wetland
!!    wet_orgp(:) |kg P          |amount of organic P in wetland
!!    wet_sed(:)  |mg/L          |sediment concentration in wetland water
!!    wet_solp(:) |kg P          |amount of soluble P in wetland
!!    wet_vol(:)  |10**4 m**3 H2O|volume of water in wetlands
!!    dtp_onoff(:)   |none          |sub-basin detention pond is associated with
!!    dtp_iy(:)      |none          |year of the simulation that the reservoir
!!                                  |becomes operational
!!    dtp_imo(:)     |none          |month the reservoir becomes operational
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_lwratio(:)   |none          |Ratio of length to width of water back up
!!    dtp_totwrwid(:)|m             |Total constructed width of the detention wall across
!!                                  |the creek
!!    dtp_stagdis(:) |none          |0=use weir/orifice discharge equation to calculate
!!                                  |outflow, 1=use stage-dicharge relationship
!!    dtp_reltype(:) |none          |Equations for Stage-Discharge relationship,1=exponential
!!                                  |function, 2=linear, 3=logarithmic, 4=cubic, 5=power
!!    dtp_intcept(:) |none          |Intercept used in regression equations
!!    dtp_expont(:)  |none          |Exponent used in the exponential equation
!!    dtp_coef1(:)   |none          |Coefficient of 3rd degree in the polynomial equation
!!    dtp_coef2(:)   |none          |Coefficient of 2nd degree in the polynomial equation
!!    dtp_coef3(:)   |none          |Coefficient of 1st degree in the polynomial equation
!!    dtp_dummy1(:)   |none         |Dummy variable, backs up space
!!    dtp_dummy2(:)   |none         |Dummy variable, backs up space
!!    dtp_dummy3(:)   |none         |Dummy variable, backs up space
!!    dtp_weirtype(:,:)|none        |Type of weir: 1=rectangular and 2=circular
!!    dtp_weirdim(:,:)|none         |Weir dimensions, 1=read user input, 0=use model calculation
!!    dtp_wdratio(:,:)|none         |Width depth ratio of rectangular weirs
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of orifice hole at different stages
!!    dtp_addon(:,:)  |m            |The distance between spillway levels
!!    dtp_flowrate(:,:)|m3/sec      |Maximum discharge from each stage of the weir/hole
!!    dtp_cdis(:,:)  |none          |Discharge coeffieicne for weir/orifice flow
!!    dtp_retperd(:,:)|years        |Return period at different stages
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    evpnd       |none          |pond evaporation coefficient
!!    j           |none          |counter
!!    schla       |none          |value for CHLA used in subbasin
!!    schlaw      |none          |value for CHLAW used in subbasin
!!    sifld1      |none          |value for IFLOD1 used in subbasin
!!    sifld2      |none          |value for IFLOD2 used in subbasin
!!    sn1         |m/year        |value for NSETL1 used in subbasin
!!    sn2         |m/year        |value for NSETL2 used in subbasin
!!    sndt        |none          |value for NDTARG used in subbasin
!!    snw1        |m/year        |value for NSETLW1 used in subbasin
!!    snw2        |m/year        |value for NSETLW2 used in subbasin
!!    sp1         |m/year        |value for PSETL1 used in subbasin
!!    sp2         |m/year        |value for PSETL2 used in subbasin
!!    spnd1       |none          |value for IPND1 used in subbasin
!!    spnd2       |none          |value for IPND2 used in subbasin
!!    spndesa     |ha            |value for PND_ESA used in subbasin
!!    spndev      |10^4 m^3 H2O  |value for PND_EVOL used in subbasin
!!    spndfr      |none          |value for PND_FR used in subbasin
!!    spndk       |mm/hr         |value for PND_K used in subbasin
!!    spndns      |mg/L          |value for PND_NSED used in subbasin
!!    spndpsa     |ha            |value for PND_PSA used in subbasin
!!    spndpv      |10^4 m^3 H2O  |value for PND_PVOL used in subbasin
!!    spnds       |mg/L          |value for PND_SED used in subbasin
!!    spndv       |10^4 m^3 H2O  |value for PND_VOL used in subbasin
!!    spno3       |mg N/L        |concentration of NO3 in pond
!!    sporgn      |mg N/L        |concentration of organic N in pond
!!    sporgp      |mg P/L        |concentration of organic P in pond
!!    spsolp      |mg P/L        |concentration of soluble P in pond
!!    sseci       |none          |value for SECCI used in subbasin
!!    sseciw      |none          |value for SECCIW used in subbasin
!!    sw1         |m/year        |value for PSETLW1 used in subbasin
!!    sw2         |m/year        |value for PSETLW2 used in subbasin
!!    swetfr      |none          |value for WET_FR used in subbasin
!!    swetk       |mm/hr         |value for WET_K used in subbasin
!!    swetmsa     |ha            |value for WET_MSA used in subbasin
!!    swetmv      |10^4 m^3 H2O  |value for WET_MVOL used in subbasin
!!    swetns      |mg/L          |value for WET_NSED used in subbasin
!!    swetnsa     |ha            |value for WET_NSA used in subbasin
!!    swetnv      |10^4 m^3 H2O  |value for WET_NVOL used in subbasin
!!    swets       |mg/L          |value fro WET_SED used in subbasin
!!    swetv       |10^4 m^3 H2O  |value for WET_VOL used in subbasin
!!    swno3       |mg N/L        |concentration of NO3 in water
!!    sworgn      |mg N/L        |concentration of organic N in water
!!    sworgp      |mg P/L        |concentration of organic P in water
!!    swsolp      |mg P/L        |concentration of soluble P in water
!!    titldum     |NA            |title line of .pnd file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
CHARACTER (LEN=200) :: lus
INTEGER :: eof, j, sifld1, sifld2, sndt, spnd1, spnd2
REAL :: spndfr, spndpsa, spndpv, spndesa, spndev, spndv, spnds
REAL :: spndns, spndk, swetfr, swetnsa, swetnv, swetmsa, sp1
REAL :: swetmv, swetv, swets, swetns, swetk, sp2, sw1, sw2
REAL :: sn1, sn2, snw1, snw2, schla, schlaw, sseci, sseciw
REAL :: spno3, spsolp, sporgn, sporgp, swno3, swsolp, sworgn
REAL :: sworgp, sub_ha, velsetlpnd

eof = 0
spndfr = 0.
spndpsa = 0.
spndpv = 0.
spndesa = 0.
spndev = 0.
spndv = 0.
spnds = 0.
spndns = 0.
spndk = 0.
sifld1 = 0
sifld2 = 0
sndt = 0
sp1 = 0.
sp2 = 0.
sn1 = 0.
sn2 = 0.
schla = 0.
sseci = 0.
spno3 = 0.
spsolp = 0.
sporgn = 0.
sporgp = 0.
spnd1 = 0
spnd2 = 0
swetfr = 0.
swetnsa = 0.
swetnv = 0.
swetmsa = 0.
swetmv = 0.
swetv = 0.
swets = 0.
swetns = 0.
swetk = 0.
sw1 = 0.
sw2 = 0.
snw1 = 0.
snw2 = 0.
schlaw = 0.
sseciw = 0.
swno3 = 0.
swsolp = 0.
sworgn = 0.
sworgp = 0.
pndevcoeff = 0.
wetevcoeff = 0.
sub_ha = sub_km(i) * 100.
velsetlpnd = 0.

DO
  READ (104,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndfr
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndpsa
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndpv
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndesa
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndev
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndv
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spnds
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndns
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spndk
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sifld1
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sifld2
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sndt
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sp1
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sp2
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sn1
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sn2
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) schla
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sseci
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spno3
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spsolp
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sporgn
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sporgp
  IF (eof < 0) EXIT
  READ (104,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  IF (titldum == '             '.OR.titldum == 'Inputs used in')THEN
    vselsetlpnd = 10.0
  ELSE
    BACKSPACE 104
    READ (104,*,IOSTAT=eof) pnd_d50
    pnd_d50mm = pnd_d50 / 1000.        !! micrometers to millimeters
    velsetlpnd = 24. * 411. * pnd_d50mm ** 2.
  END IF
  READ (104,*,IOSTAT=eof) spnd1
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) spnd2
  IF (eof < 0) EXIT
  READ (104,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetfr
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetnsa
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetnv
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetmsa
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetmv
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetv
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swets
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetns
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swetk
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sw1
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sw2
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) snw1
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) snw2
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) schlaw
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sseciw
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swno3
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) swsolp
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sworgn
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sworgp
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) pndevcoeff
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) wetevcoeff
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,5101,IOSTAT=eof) dpd_file
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,5101,IOSTAT=eof) wpd_file
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,5101,IOSTAT=eof) rib_file
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,5101,IOSTAT=eof) sfb_file
  CLOSE (104)
  
!! Detention pond  -- read from a separate file (.dpd)
  IF (dpd_file /= '             ' .AND. ievent > 2) THEN
    OPEN (104,FILE=dpd_file)
    READ (104,5100,IOSTAT=eof) titldum
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_onoff(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_imo(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_iyr(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_evrsv(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_numweir(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_numstage(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_lwratio(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_totwrwid(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_stagdis(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_reltype(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_intcept(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_expont(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_coef1(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_coef2(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_coef3(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_dummy1(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_dummy2(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) dtp_dummy3(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_weirtype(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_weirdim(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_wdratio(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_depweir(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_diaweir(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_addon(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_flowrate(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_cdis(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_retperd(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (dtp_pcpret(i,k),k=1,dtp_numstage(i))
    IF (eof < 0) EXIT
    CLOSE (104)
  ELSE
  END IF
  
!!  END DETENTION POND FILE
  
!! Wet pond (.wpd file)
  IF (wpd_file /= '             ' .AND. ievent > 2) THEN
    OPEN (104,FILE=wpd_file)
    READ (104,5100,IOSTAT=eof) titldum
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_onoff(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_imo(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_iyr(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_k(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_evrsv(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_hydeff(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_dp(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_qi(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sedi(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sede(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_dim(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_pvol(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_pdepth(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sdslope(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_lenwdth(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_stagdis(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sdtype(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sdintc(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sdexp(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sdc1(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sdc2(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_sdc3(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_extdepth(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_pdia(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_plen(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_pmann(i)
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) wtp_ploss(i)
    IF (eof < 0) EXIT
    CLOSE (104)
  ELSE
  END IF
  
  
!! end wet pond (.wpd file)
  
!! Retention-Irrigation
  IF (rib_file /= '             '.AND. ievent > 2) THEN
    OPEN (104,FILE=rib_file)
    READ (104,5100,IOSTAT=eof) titldum
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) num_ri(i)
    IF (eof < 0) EXIT
    READ (104,'(a200)',IOSTAT=eof) lus
    IF (eof < 0) EXIT
    
    DO ii=2,len_trim(lus)
      num_noirr(i) = 1
      IF (lus(ii:ii) == ',' .OR. lus(ii:ii) == ' ') THEN
        IF (lus(ii-1:ii-1) /= ' '.AND.lus(ii-1:ii-1) /= ',') THEN
          num_noirr(i) = num_noirr(i) + 1
        END IF
      END IF
    END DO
    IF (num_noirr(i)>0) THEN
      BACKSPACE(104)
      READ (104,*) (ri_nirr(i,k), k=1,num_noirr(i))
      IF (eof < 0) EXIT
    END IF
    
    READ (104,5100,IOSTAT=eof) titldum
    READ (104,*,IOSTAT=eof) (ri_fr(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_dim(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_im(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_iy(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_sa(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_vol(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_qi(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_k(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_dd(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) (ri_evrsv(i,k),k=1,num_ri(i))
    IF (eof < 0) EXIT
    CLOSE (104)
  ELSE
  END IF
!! end .rib file
  
!! Sedimentaton-Filtration (.sfb file)
  IF (sfb_file /= '             '.AND. ievent > 2) THEN
    OPEN (104,FILE=sfb_file)
    READ (104,'(a20)',IOSTAT=eof) titldum
    IF (eof < 0) EXIT
    READ (104,*,IOSTAT=eof) num_sf(i)
    IF (eof < 0) EXIT
    READ (104,5100,IOSTAT=eof) titldum
    
    READ (104,*,IOSTAT=eof) (sf_fr(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sf_typ(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sf_dim(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sf_ptp(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sf_im(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sf_iy(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_sa(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_pvol(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_qfg(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_pd(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_qi(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_bpw(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_k(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_dp(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_sedi(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (sp_sede(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_sa(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_fsa(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_qfg(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_pd(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_dep(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_bpw(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_k(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_dp(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_dc(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_h(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_por(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (tss_den(i,k),k=1,num_sf(i))
    READ (104,*,IOSTAT=eof) (ft_alp(i,k),k=1,num_sf(i))
    CLOSE (104)
  ELSE
  END IF
  
  
!! end .sfb file
  EXIT
END DO

IF (isproj == 2) swetfr = 0.0

!     set default values
IF (sndt <= 0) sndt = 15
IF (spndv > spndpv) spndv = spndpv
IF (swetv > swetnv) swetv = .95 * swetnv
IF (schla <= 0.) schla = 1.
IF (schlaw <= 0.) schlaw = 1.
IF (sseci <= 0.) sseci = 1.
IF (sseciw <= 0.) sseciw = 1.
IF (pndevcoeff <= 0.) pndevcoeff = 0.6
IF (wetevcoeff <= 0.) wetevcoeff = 0.6

!!    perform conversions
sp1 = sp1 / 365.                     !! m/yr to m/day
sp2 = sp2 / 365.
sw1 = sw1 / 365.
sw2 = sw2 / 365.
sn1 = sn1 / 365.
sn2 = sn2 / 365.
snw1 = snw1 / 365.
snw2 = snw2 / 365.
spno3 = spno3 * spndv * 10.          !! mg/L to kg
spsolp = spsolp * spndv * 10.
sporgn = sporgn * spndv * 10.
sporgp = sporgp * spndv * 10.
swno3 = swno3 * spndv * 10.
swsolp = swsolp * spndv * 10.
sworgn = sworgn * spndv * 10.
sworgp = sworgp * spndv * 10.

!! assign values to HRUs
DO j = 1, hrutot(i)
  ihru = 0
  ihru = nhru + j
  pnd_fr(ihru) = spndfr
  pnd_psa(ihru) = spndpsa
  pnd_pvol(ihru) = spndpv
  pnd_esa(ihru) = spndesa
  pnd_evol(ihru) = spndev
  pnd_vol(ihru) = spndv
  pnd_sed(ihru) = spnds
  pnd_nsed(ihru) = spndns
  pnd_k(ihru) = spndk
  iflod1(ihru) = sifld1
  iflod2(ihru) = sifld2
  ndtarg(ihru) = sndt
  psetlp(1,ihru) = sp1
  psetlp(2,ihru) = sp2
  nsetlp(1,ihru) = sn1
  nsetlp(2,ihru) = sn2
  chlap(ihru) = schla
  seccip(ihru) = sseci
  pnd_no3(ihru) = spno3
  pnd_solp(ihru) = spsolp
  pnd_orgn(ihru) = sporgn
  pnd_orgp(ihru) = sporgp
  ipnd1(ihru) = spnd1
  ipnd2(ihru) = spnd2
  velsetlp(ihru) = velsetlpnd
  wet_fr(ihru) = swetfr
  wet_nsa(ihru) = swetnsa
  wet_nvol(ihru) = swetnv
  wet_mxsa(ihru) = swetmsa
  wet_mxvol(ihru) = swetmv
  wet_vol(ihru) = swetv
  wet_sed(ihru) = swets
  wet_nsed(ihru) = swetns
  wet_k(ihru) = swetk
  psetlw(1,ihru) = sw1
  psetlw(2,ihru) = sw2
  nsetlw(1,ihru) = snw1
  nsetlw(2,ihru) = snw2
  chlaw(ihru) = schlaw
  secciw(ihru) = sseciw
  wet_no3(ihru) = swno3
  wet_solp(ihru) = swsolp
  wet_orgn(ihru) = sworgn
  wet_orgp(ihru) = sworgp
  evpnd(ihru) = pndevcoeff
  evwet(ihru) = wetevcoeff
END DO

!!    close (104)

!! Set default values for urban BMP parameters
IF (ievent == 3) CALL bmpinit

RETURN
5100 FORMAT (a)
5101 FORMAT(a13)
END SUBROUTINE readpnd
