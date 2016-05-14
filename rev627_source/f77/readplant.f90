SUBROUTINE readplant
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads input parameters from the landuse/landcover
!!    database (plant.dat)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name      |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mcrdb     |none             |maximum number of crops/landcover in
!!                                |database file (crop.dat)
!!    rsdco     |none             |residue decomposition coefficient
!!                                |The fraction of residue which will decompose
!!                                |in a day assuming optimal moisture,
!!                                |temperature, C:N ratio, and C:P ratio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai_min(:)|m**2/m**2        |minimum LAI during winter dormant period
!!    bio_e(:)   |(kg/ha)/(MJ/m**2)|biomass-energy ratio
!!                                 |The potential (unstressed) growth rate per
!!                                 |unit of intercepted photosynthetically
!!                                 |active radiation.
!!    bio_leaf(:)|none             |fraction of leaf/needle biomass that drops
!!                                 |during dormancy (for trees only)
!!    bio_n1(:)  |none             |1st shape parameter for plant N uptake
!!                                 |equation
!!    bio_n2(:)  |none             |2nd shape parameter for plant N uptake
!!                                 |equation
!!    bio_p1(:)  |none             |1st shape parameter for plant P uptake
!!                                 |equation
!!    bio_p2(:)  |none             |2st shape parameter for plant P uptake
!!                                 |equation
!!    blai(:)    |none             |maximum (potential) leaf area index
!!    bm_dieoff(:) fraction        |fraction above ground biomass that dies
!!                                 |   off at dormancy
!!    chtmx(:)   |m                |maximum canopy height
!!    cnyld(:)   |kg N/kg yield    |fraction of nitrogen in yield
!!    cpnm(:)    |NA               |four character code to represent crop name
!!    cpyld(:)   |kg P/kg yield    |fraction of phosphorus in yield
!!    cvm(:)     |none             |natural log of USLE_C
!!    dlai(:)    |none             |fraction of growing season when leaf
!!                                 |area declines
!!    gsi(:)     |m/s              |maximum stomatal conductance
!!    hvsti(:)   |(kg/ha)/(kg/ha)  |harvest index: crop yield/aboveground
!!                                 |biomass
!!    idc(:)     |none             |crop/landcover category:
!!               |                 |1 warm season annual legume
!!               |                 |2 cold season annual legume
!!               |                 |3 perennial legume
!!               |                 |4 warm season annual
!!               |                 |5 cold season annual
!!               |                 |6 perennial
!!               |                 |7 trees
!!    leaf1(:)   |none             |1st shape parameter for leaf area
!!                                 |development equation.
!!    leaf2(:)   |none             |2nd shape parameter for leaf area
!!                                 |development equation.
!!    pltnfr(1,:)|kg N/kg biomass  |nitrogen uptake parameter #1: normal
!!                                 |fraction of N in crop biomass at emergence
!!    pltnfr(2,:)|kg N/kg biomass  |nitrogen uptake parameter #2: normal
!!                                 |fraction of N in crop biomass at 0.5
!!                                 |maturity
!!    pltnfr(3,:)|kg N/kg biomass  |nitrogen uptake parameter #3: normal
!!                                 |fraction of N in crop biomass at maturity
!!    pltpfr(1,:)|kg P/kg biomass  |phosphorus uptake parameter #1: normal
!!                                 |fraction of P in crop biomass at emergence
!!    pltpfr(2,:)|kg P/kg biomass  |phosphorus uptake parameter #2: normal
!!                                 |fraction of P in crop biomass at 0.5
!!                                 |maturity
!!    pltpfr(3,:)|kg P/kg biomass  |phosphorus uptake parameter #3: normal
!!                                 |fraction of P in crop biomass at maturity
!!    rdmx(:)    |m                |maximum root depth
!!    rsdco_pl(:)|none             |plant residue decomposition coefficient. The
!!                                 |fraction of residue which will decompose in
!!                                 |a day assuming optimal moisture,
!!                                 |temperature, C:N ratio, and C:P ratio
!!    rsr1c      |                 |initial root to shoot ratio at the beg of growing season
!!    rsr2c      |                 |root to shoot ratio at the end of the growing season
!!    t_base(:)  |deg C            |minimum temperature for plant growth
!!    t_opt(:)   |deg C            |optimal temperature for plant growth
!!    vpd2(:)    |(m/s)*(1/kPa)    |rate of decline in stomatal conductance per
!!                                 |unit increase in vapor pressure deficit
!!    wac21(:)   |none             |1st shape parameter for radiation use
!!                                 |efficiency equation.
!!    wac22(:)   |none             |2nd shape parameter for radiation use
!!                                 |efficiency equation.
!!    wavp(:)    |none             |Rate of decline in radiation use efficiency
!!                                 |as a function of vapor pressure deficit
!!    wsyf(:)    |(kg/ha)/(kg/ha)  |Value of harvest index between 0 and HVSTI
!!                                 |which represents the lowest value expected
!!                                 |due to water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name      |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alaimin   |m**2/m**2        |minimum leaf area index for winter dormant
!!                                |period
!!    b1        |none             |variable to hold calculation results
!!    b2        |none             |variable to hold calculation results
!!    b3        |none             |variable to hold calculation results
!!    bioehi    |(kg/ha)/(MJ/m**2)|biomass-energy ratio when plant is in
!!                                |an environment with CO2 level equal to
!!                                |the value of CO2HI. This biomass-energy
!!                                |ratio is used to set the 2nd point on the
!!                                |radiation use efficiency curve
!!    bioleaf   |none             |fraction of biomass accumulated each year
!!                                |that is leaf/needle
!!    c1        |none             |variable to hold calculation results
!!    co2hi     |uL CO2/L air     |CO2 concetration higher than the ambient
!!                                |corresponding to the 2nd point on radiation
!!                                |use efficiency curve
!!    frgmax    |none             |fraction of maximum stomatal conductance
!!                                |that is achieved at the vapor pressure
!!                                |deficit defined by VPDFR
!!    frgrw1    |none             |fraction of the growing season corresponding
!!                                |to the 1st point on optimal leaf area
!!                                |development curve
!!    frgrw2    |none             |fraction of the growing season corresponding
!!                                |to the 2nd point on optimal leaf area
!!                                |development curve
!!    eof       |none             |end of file flag (=-1 of eof, else =0)
!!    ic        |none             |landuse/landcover array storage number
!!                                |when a land cover is assigned in the
!!                                |.mgt file, the variables for the land
!!                                |cover are accessed by the array number.
!!                                |Landuse/landcover numbers (ICNUM) in
!!                                |crop.dat need to be assigned consecutively
!!                                |to ensure that the crop number used by the
!!                                |user is the same as the array storage number
!!    icnum     |none             |crop/landcover number. Reference number only.
!!    laimx1    |none             |fraction of maximum leaf area index
!!                                |corresponding to the 1st point on optimal
!!                                |leaf area development curve
!!    laimx2    |none             |fraction of maximum leaf area index
!!                                |corresponding to the 2nd point on optimal
!!                                |leaf area development curve
!!    usle_c    |none             |minimum value of the USLE C factor for water
!!                                |erosion
!!    vpdfr     |kPa              |vapor pressure deficit at which FRGMAX is
!!                                |valid
!!    xx        |none             |dummy variable to hold IDC expressed as a
!!                                |real number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    NInt, Int, Log, ascrv

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: ic, eof, icnum, yrsmat
REAL :: xx, usle_c, frgrw2, laimx2, co2hi, bioehi, vpdfr, blaic
REAL :: b1, b2, b3, c1, frgrw1, laimx1, frgmax, bioe, hvstc, dlaic
REAL :: chtmxc, rdmxc, topt, tbase, cnyldc, cpyldc, bn1, bn2, bn3
REAL :: bp1c, bp2c, bp3c, wsyfc, gsic, wavpc, rsdcopl, alaimin
REAL :: bioleaf
CHARACTER (LEN=4) :: cname

eof = 0


DO
  
!!      initialize locals in loop
  alaimin = 0.0
  bioe = 0.0
  bioehi = 0.0
  bioleaf = 0.0
  blaic = 0.0
  bn1 = 0.0
  bn2 = 0.0
  bn3 = 0.0
  bp1c = 0.0
  bp2c = 0.0
  bp3c = 0.0
  chtmxc = 0.0
  cname = ""
  cnyldc = 0.0
  co2hi = 0.0
  cpyldc = 0.0
  dlaic = 0.0
  frgmax = 0.0
  frgrw1 = 0.0
  frgrw2 = 0.0
  gsic = 0.0
  hvstc = 0.0
  ic = 0
  laimx1 = 0.0
  laimx2 = 0.0
  rdmxc = 0.0
  rsdcopl = 0.0
  tbase = 0.0
  topt = 0.0
  usle_c = 0.0
  vpdfr = 0.0
  wavpc = 0.0
  wsyfc = 0.0
  xx = 0.0
  
  READ (104,*,IOSTAT=eof) ic, cname, idtype
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) bioe, hvstc, blaic, frgrw1, laimx1,  &
      frgrw2, laimx2, dlaic, chtmxc, rdmxc
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) topt, tbase, cnyldc, cpyldc, bn1, bn2,  &
      bn3, bp1c, bp2c, bp3c
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) wsyfc, usle_c, gsic, vpdfr, frgmax,  &
      wavpc, co2hi, bioehi, rsdcopl, alaimin
  IF (eof < 0) EXIT
  READ (104,777,IOSTAT=eof) bioleaf, yrsmat, biomxtrees, extcoef,  &
      bmdieoff, rsr1c, rsr2c
!! 777    format (7f8.3)
  777    FORMAT (f8.3,i5,5F8.3)
  
  IF (eof < 0) EXIT
  
  IF (ic <= 0) EXIT
  
  IF (bmdieoff <= 1.e-6) bmdieoff = 1.00
  
  cpnm(ic) = cname
  idc(ic) = idtype
  bio_e(ic) = bioe
  hvsti(ic) = hvstc
  blai(ic) = blaic
  dlai(ic) = dlaic
  chtmx(ic) = chtmxc
  rdmx(ic) = rdmxc
  t_opt(ic) = topt
  t_base(ic) = tbase
  cnyld(ic) = cnyldc
  cpyld(ic) = cpyldc
  pltnfr(1,ic) = bn1
  pltnfr(2,ic) = bn2
  pltnfr(3,ic) = bn3
  pltpfr(1,ic) = bp1c
  pltpfr(2,ic) = bp2c
  pltpfr(3,ic) = bp3c
  wsyf(ic) = wsyfc
  gsi(ic) = gsic
  wavp(ic) = wavpc
  rsdco_pl(ic) = rsdcopl
  alai_min(ic) = alaimin
  bio_leaf(ic) = bioleaf
  mat_yrs(ic) = yrsmat
  bmx_trees(ic) = 1000. * biomxtrees
  ext_coef(ic) = extcoef
  bm_dieoff(ic) = bmdieoff
  rsr1(ic) = rsr1c
  rsr2(ic) = rsr2c
  
!! set default value
  IF (ext_coef(ic) < 1.e-6) ext_coef(ic) = 0.65
  IF (rsdco_pl(ic) < 1.e-6) rsdco_pl(ic) = rsdco
  IF (usle_c <= 0.0) usle_c = 0.0
  IF (usle_c >= 1.0) usle_c = 1.0
  IF (blai(ic) <= 0.0) blai(ic) = 0.0
  IF (blai(ic) >= 13.0) blai(ic) = 13.0 !! modified by Cibin from 10 to 13
  IF (rsr1(ic) <= 0.0) rsr1(ic) = 0.4
  IF (rsr2(ic) <= 0.0) rsr2(ic) = 0.2
  
  
  IF (bio_e(ic) > 0. .AND. cpnm(ic) /= 'WATR') THEN
    
!!        determine shape parameters for the leaf area development equation
    CALL ascrv(laimx1,laimx2,frgrw1,frgrw2,leaf1(ic),leaf2(ic))
    
!!        The other point used to determine shape parameters for radiation
!!        use efficiency is the ambient CO2 level (330 ul/l) and the
!!        biomass-energy ratio (bio_e) given for the crop/land cover.
    b1 = 0.0
    b2 = 0.0
    c1 = 330.                        !! ambient CO2
    IF (co2hi == 330.) co2hi = 660.
    b1 = bio_e(ic) * .01             !! "ambient" bio-e ratio/100
    b2 = bioehi * .01                !! "elevated" bio-e ratio/100
    
    
!!        determine shape parameters for the radiation use efficiency equation
    CALL ascrv(b1, b2, c1, co2hi, wac21(ic), wac22(ic))
    
    IF (usle_c < 1.e-4) usle_c = 0.001
    cvm(ic) = LOG(usle_c)
    
    
!!        nitrogen uptake parameters
!!        fix bad input for pltnfr(3,ic)
    IF (pltnfr(1,ic) - pltnfr(2,ic) < .0001)  &
        pltnfr(2,ic) = pltnfr(1,ic) - .0001
    IF (pltnfr(2,ic) - pltnfr(3,ic) < .0001)  &
        pltnfr(3,ic) = .75 * pltnfr(3,ic)
    b1 = 0.0
    b2 = 0.0
    b3 = 0.0
    b1 = pltnfr(1,ic) - pltnfr(3,ic)           !!normalize N fractions
    b2 = 1. - (pltnfr(2,ic) - pltnfr(3,ic)) / b1
    b3 = 1. - .00001 / b1
!!        determine shape parameters for plant nitrogen uptake equation
    CALL ascrv(b2, b3, 0.5, 1.0, bio_n1(ic), bio_n2(ic))
    
    
!!        phosphorus uptake parameters
!!        fix bad input for pltpfr(3,ic)
    IF (pltpfr(1,ic) - pltpfr(2,ic) < .0001)  &
        pltpfr(2,ic) = pltpfr(1,ic) - .0001
    IF (pltpfr(2,ic) - pltpfr(3,ic) < .0001)  &
        pltpfr(3,ic) = .75 * pltpfr(3,ic)
    b1 = 0.0
    b2 = 0.0
    b3 = 0.0
    b1 = pltpfr(1,ic) - pltpfr(3,ic)        !!normalize P fractions
    b2 = 1. - (pltpfr(2,ic) - pltpfr(3,ic)) / b1
    b3 = 1. - .00001 / b1
!!        determine shape parameters for plant phosphorus uptake equation
    CALL ascrv(b2, b3, .5, 1., bio_p1(ic), bio_p2(ic))
    
    
!!        calculate slope in stomatal conductance equation
    vpd2(ic) = (1. - frgmax) / (vpdfr - 1.)
    
  END IF
  
END DO

CLOSE (104)
RETURN
END SUBROUTINE readplant
