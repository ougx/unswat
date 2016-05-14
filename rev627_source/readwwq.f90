SUBROUTINE readwwq
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the watershed stream water quality input
!!    data (.wwq file) and initializes the QUAL2E variables which apply to
!!    the entire watershed

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0         |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1         |mg N/mg alg   |fraction of algal biomass that is nitrogen
!!    ai2         |mg P/mg alg   |fraction of algal biomass that is phosphorus
!!    ai3         |mg O2/mg alg  |the rate of oxygen production per unit of
!!                               |algal photosynthesis
!!    ai4         |mg O2/mg alg  |the rate of oxygen uptake per unit of algae
!!                               |respiration
!!    ai5         |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
!!                               |nitrogen oxidation
!!    ai6         |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
!!                               |nitrogen oxidation
!!    chla_subco  |fraction      |regional adjustment on sub chla_a loading
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    igropt      |none          |Qual2E option for calculating the local
!!                               |specific growth rate of algae
!!                               |1: multiplicative:
!!                                   u = mumax * fll * fnn * fpp
!!                               |2: limiting nutrient
!!                               |   u = mumax * fll * Min(fnn, fpp)
!!                               |3: harmonic mean
!!                               |   u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
!!    k_l         |MJ/(m2*hr)    |half-saturation coefficient for light
!!    k_n         |mg N/L        |michaelis-menton half-saturation constant
!!                               |for nitrogen
!!    k_p         |mg P/L        |michaelis-menton half saturation constant
!!                               |for phosphorus
!!    lambda0     |1/m           |non-algal portion of the light extinction
!!                               |coefficient
!!    lambda1     |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2     |(1/m)(ug chla/L)**(-2/3)
!!                               |nonlinear algal self-shading coefficient
!!    lao         |NA            |Qual2E light averaging option. Qual2E defines
!!                               |four light averaging options. The only option
!!                               |currently available in SWAT is #2.
!!    mumax       |1/day or 1/hr |maximum specific algal growth rate
!!    p_n         |none          |algal preference factor for ammonia
!!    rhoq        |1/day or 1/hr |algal respiration rate
!!    tfact       |none          |fraction of solar radiation computed in the
!!                               |temperature heat balance that is
!!                               |photosynthetically active
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    titldum     |NA            |title line for .wwq file, not used
!!    eof         |none          |end of file flag
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof

!!    initialize variables
eof = 0

DO
  READ (101,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) lao
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) igropt
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) ai0
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) ai1
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) ai2
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) ai3
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) ai4
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) ai5
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) ai6
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) mumax
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) rhoq
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) tfact
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) k_l
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) k_n
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) k_p
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) lambda0
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) lambda1
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) lambda2
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) p_n
  IF (eof < 0) EXIT
  READ (101,*,IOSTAT=eof) chla_subco
  IF (eof < 0) EXIT
  EXIT
END DO


!!    set default values for undefined parameters

IF (lao <= 0) lao = 2
IF (igropt <= 0) igropt = 2
IF (ai0 <= 0.) ai0 = 50.
IF (ai1 <= 0.) ai1 = 0.08
IF (ai2 <= 0.) ai2 = 0.015
IF (ai3 <= 0.) ai3 = 1.60
IF (ai4 <= 0.) ai4 = 2.0
IF (ai5 <= 0.) ai5 = 3.5
IF (ai6 <= 0.) ai6 = 1.07
IF (mumax <= 0.) mumax = 2.0
IF (rhoq <= 0.) rhoq = 2.5      !! previous 0.3
IF (tfact <= 0.) tfact = 0.3
IF (k_l <= 0.) k_l = 0.75
IF (k_n <= 0.) k_n = 0.02
IF (k_p <= 0.) k_p = 0.025
IF (lambda0 <= 0.) lambda0 = 1.0
IF (lambda1 <= 0.) lambda1 = 0.03
IF (lambda2 <= 0.) lambda2 = 0.054
IF (p_n <= 0.) p_n = 0.5
IF (chla_subco <= 0.) chla_subco = 40.0


!!    currently, only one of the four light averaging options is coded
!!    lao must be set to this value
lao = 2

!! convert units on k_l:read in as kJ/(m2*min), use as MJ/(m2*hr)
k_l = k_l * 1.e-3 * 60.

!! change units from day to hour if hourly (subdaily) routing is performed
IF (ievent == 3) THEN
  mumax = mumax / 24.
  rhoq = rhoq / 24.
END IF

CLOSE (101)
RETURN
5100 FORMAT (a)
END SUBROUTINE readwwq
