SUBROUTINE readlwq
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the lake water quality input file (.lwq).
!!    This file contains data related to initial pesticide and nutrient levels
!!    in the lake/reservoir and transformation processes occuring within the
!!    lake/reservoir. Data in the lake water quality input file is assumed to
!!    apply to all reservoirs in the watershed.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i            |none          |reservoir number
!!    res_vol(:)   |m**3          |reservoir volume
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlar(:)      |none          |chlorophyll-a production coefficient for
!!                                 |reservoir
!!    ires1(:)      |none          |beginning of mid-year nutrient settling
!!                                 |"season"
!!    ires2(:)      |none          |end of mid-year nutrient settling "season"
!!    lkpst_conc(:) |mg/m**3       |pesticide concentration in lake water
!!    lkpst_koc(:)  |m**3/g        |pesticide partition coefficient between
!!                                 |water and sediment in lake water
!!    lkpst_mix(:)  |m/day         |mixing velocity (diffusion/dispersion) in
!!                                 |lake water for pesticide
!!    lkpst_rea(:)  |1/day         |pesticide reaction coefficient in lake water
!!    lkpst_rsp(:)  |m/day         |resuspension velocity in lake water for
!!                                 |pesticide sorbed to sediment
!!    lkpst_stl(:)  |m/day         |settling velocity in lake water for
!!                                 |pesticide sorbed to sediment
!!    lkpst_vol(:)  |m/day         |pesticide volatilization coefficient in lake
!!                                 |water
!!    lkspst_act(:) |m             |depth of active sediment layer in lake for
!!                                 |for pesticide
!!    lkspst_bry(:) |m/day         |pesticide burial velocity in lake bed
!!                                 |sediment
!!    lkspst_conc(:)|mg/m**3       |pesticide concentration in lake bed sediment
!!    lkspst_rea(:) |1/day         |pesticide reaction coefficient in lake bed
!!                                 |sediment
!!    nsetlr(1,:)   |m/day         |nitrogen settling rate for mid-year
!!                                 |period (read in as m/year and converted to
!!                                 |m/day)
!!    nsetlr(2,:)   |m/day         |nitrogen settling rate for remainder of
!!                                 |year (read in as m/year and converted to
!!                                 |m/day)
!!    psetlr(1,:)   |m/day         |phosphorus settling rate for mid-year
!!                                 |period (read in as m/year and converted to
!!                                 |m/day)
!!    psetlr(2,:)   |m/day         |phosphorus settling rate for remainder of
!!                                 |year (read in as m/year and converted to
!!                                 |m/day)
!!    res_nh3(:)    |kg N          |amount of ammonia in reservoir
!!    res_no2(:)    |kg N          |amount of nitrite in reservoir
!!    res_no3(:)    |kg N          |amount of nitrate in reservoir
!!    res_orgn(:)   |kg N          |amount of organic N in reservoir
!!    res_orgp(:)   |kg P          |amount of organic P in reservoir
!!    res_solp(:)   |kg P          |amount of soluble P in reservoir
!!    seccir(:)     |none          |water clarity coefficient for reservoir
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    nh3i        |mg N/L        |initial concentration of ammonia in reservoir
!!    no2i        |mg N/L        |initial concentration of nitrite in reservoir
!!    no3i        |mg N/L        |initial concentration of nitrate in reservoir
!!    orgni       |mg N/L        |initial concentration of organic N in
!!                               |reservoir
!!    orgpi       |mg P/L        |initial concentration of organic P in
!!                               |reservoir
!!    solpi       |mg P/L        |initial concentration of soluble P in
!!                               |reservoir
!!    titldum     |NA            |title line of .lwq file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: eof
CHARACTER (LEN=80) :: titldum
REAL :: orgpi, solpi, orgni, no3i, nh3i, no2i
REAL :: lkarea

eof = 0
titldum = ""
orgpi = 0.
solpi = 0.
orgni = 0.
no3i = 0.
nh3i = 0.
no2i = 0.

!!    read lake water quality data
DO
  READ (106,1000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,1000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) ires1(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) ires2(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) psetlr(1,i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) psetlr(2,i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) nsetlr(1,i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) nsetlr(2,i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) chlar(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) seccir(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) orgpi
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) solpi
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) orgni
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) no3i
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) nh3i
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) no2i
  IF (eof < 0) EXIT
  READ (106,1000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkpst_conc(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkpst_rea(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkpst_vol(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkpst_koc(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkpst_stl(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkpst_rsp(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkpst_mix(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkspst_conc(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkspst_rea(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkspst_bry(i)
  IF (eof < 0) EXIT
  READ (106,*,IOSTAT=eof) lkspst_act(i)
  IF (eof < 0) EXIT
  EXIT
END DO

!!    convert units
psetlr(1,i) = psetlr(1,i) / 365.         !m/yr -> m/day
psetlr(2,i) = psetlr(2,i) / 365.
nsetlr(1,i) = nsetlr(1,i) / 365.
nsetlr(2,i) = nsetlr(2,i) / 365.
!     set initial n and p concentrations --> (ppm) * (m^3) / 1000 = kg
!                                            ppm = t/m^3 * 10^6
res_solp(i) = solpi * res_vol(i) / 1000.
res_orgp(i) = orgpi * res_vol(i) / 1000.
res_no3(i) = no3i  * res_vol(i) / 1000.
res_no2(i) = no2i  * res_vol(i) / 1000.
res_nh3(i) = nh3i  * res_vol(i) / 1000.
res_orgn(i) = orgni * res_vol(i) / 1000.

!!    lake pesticide mass
lkpst_mass(i) = lkpst_conc(i) * res_vol(i)
lkarea = br1(i) * res_vol(i) ** br2(i)
lkspst_mass(i) = lkspst_conc(i) * lkspst_act(i) * lkarea * 10000.

CLOSE (106)

RETURN
1000 FORMAT (a80)
END SUBROUTINE readlwq
