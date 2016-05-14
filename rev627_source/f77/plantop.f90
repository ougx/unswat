SUBROUTINE plantop
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the plant operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lai_init       |none          |initial leaf area index of transplants
!!    bio_init       |kg/ha         |initial biomass of transplants
!!    cnop           |none          |SCS runoff curve number for moisture
!!                                  |condition II
!!    icr(:)         |none          |sequence number of crop grown within the
!!                                  |current year
!!    ihru           |none          |HRU number
!!    nro(:)         |none          |sequence number of year in rotation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for current time during
!!                               |growing season
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    idorm(:)    |none          |dormancy status code:
!!                               |0 land cover growing (not dormant)
!!                               |1 land cover dormant
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    laimxfr(:)  |
!!    olai(:)     |
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant biomass
!!    plt_et(:)   |mm H2O        |actual ET simulated during life of plant
!!    plt_pet(:)  |mm H2O        |potential ET simulated during life of plant
!!    rwt(:)      |none          |fraction of total plant biomass that is
!!                               |in roots
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j

j = 0
j = ihru

igro(j) = 1
idorm(j) = 0
phuacc(j) = 0.
plantn(j) = 0.
plantp(j) = 0.
plt_et(j) = 0.
plt_pet(j) = 0.
laimxfr(j) = 0.
hvstiadj(j) = 0.
olai(j) = 0.
rwt(j) = 0.
icr(j) = icr(j) + 1
!!   added for Srini in output.mgt per JGA by gsm 9/8/2011
strsw_sum = 0.
strstmp_sum = 0.
strsn_sum = 0.
strsp_sum = 0.
strsa_sum = 0.

IF (icr(j) > icrmx(j)) THEN
  icr(j) = 1
END IF

!! initialize transplant variables
IF (lai_init > 0.) THEN
  laiday(j) = lai_init
  bio_ms(j) = bio_init
END IF

!! compare maximum rooting depth in soil to maximum rooting depth of plant
nly = sol_nly(j)
sol_zmx(ihru) = sol_z(nly,j)
plt_zmx = 1000. * rdmx(idplt(j))
sol_zmx(ihru) = MIN(sol_zmx(ihru),plt_zmx)

!! reset curve number if given in .mgt file
IF (cnop > 0.) CALL curno(cnop,j)

RETURN
END SUBROUTINE plantop
