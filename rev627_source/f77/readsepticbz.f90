SUBROUTINE readsepticbz
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the septic input file (.sep).  This file
!!    contains information related to septic tanks modeled or defined at the
!!    watershed level

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru             |none          |HRU number

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bz_z(:)          |mm            |Depth of biozone layer
!!    bz_thk(:)        |mm            |thickness of biozone
!!    bio_bd(:)        |kg/m^3        |density of biomass
!!    coeff_bod_dc(:)  |m^3/day       |BOD decay rate coefficient
!!    coeff_bod_conv(:)|none          |BOD to live bacteria biomass conversion factor
!!    coeff_denitr(:)  |none          |Denitrification rate coefficient
!!    coeff_fc1(:)     |none          |field capacity calibration parameter 1
!!    coeff_fc2(:)     |none          |field capacity calibration parameter 2
!!    coeff_fecal(:)   |m^3/day       |Fecal coliform bacteria decay rate coefficient
!!    coeff_mrt(:)     |none          |mortality rate coefficient
!!    coeff_nitr(:)    |none          |Nitrification rate coefficient
!!    coeff_plq(:)     |none          |Conversion factor for plaque from TDS
!!    coeff_rsp(:)     |none          |respiration rate coefficient
!!    coeff_slg1(:)    |none          |slough-off calibration parameter
!!    coeff_slg2(:)    |none          |slough-off calibration parameter
!!    sep_cap(:)      |none          |Number of permanent residents in the hourse
!!    isep_typ(:)      |none          |Septic system type
!!    isep_opt(:)      |none          |Septic system operation flag (1=active,2=failing,3=not operated)
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof              |none          |end of file flag (=-1 if eof, else =0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof

!!    initialize variables
eof = 0


!! read septic parameters
DO
  READ (172,1000) titldum
  READ (172,*,IOSTAT=eof) isep_typ(ihru)
  IF (eof < 0) EXIT
  IF (isep_typ(ihru) <= 0) RETURN
  READ (172,*,IOSTAT=eof) isep_iyr(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) isep_opt(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) sep_cap(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) bz_area(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) isep_tfail(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) bz_z(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) bz_thk(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) sep_strm_dist(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) sep_den(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) bio_bd(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_bod_dc(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_bod_conv(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_fc1(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_fc2(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_fecal(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_plq(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_mrt(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_rsp(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_slg1(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_slg2(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_nitr(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_denitr(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_pdistrb(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_psorpmax(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_solpslp(ihru)
  IF (eof < 0) EXIT
  READ (172,*,IOSTAT=eof) coeff_solpintc(ihru)
  EXIT
END DO

coeff_mrt(ihru) = 0.01 * coeff_mrt(ihru)
coeff_rsp(ihru) = 0.01 * coeff_rsp(ihru)
coeff_slg1(ihru) = 0.001 * coeff_slg1(ihru)
coeff_nitr(ihru) = 0.01 * coeff_nitr(ihru)
coeff_denitr(ihru) = 0.01 * coeff_denitr(ihru)

!!Convert QSTE from volume to depth unit, mm
qstemm(ihru) = sptqs(isep_typ(ihru)) * sep_cap(ihru) / bz_area(ihru) * 1000.

!!    set default values for undefined parameters
IF (isep_iyr(ihru)==0) isep_iyr(ihru) = iyr
IF (bz_z(ihru) <= 1.e-6) bz_z(ihru) = 500.
IF (bz_thk(ihru) <= 1.e-6) bz_thk(ihru) = 20.
IF (bio_bd(ihru) <= 1.e-6) bio_bd(ihru) = 1000.
IF (coeff_bod_dc(ihru) <= 1.e-6) coeff_bod_dc(ihru) = 9.33
IF (coeff_bod_conv(ihru) <= 1.e-6) coeff_bod_conv(ihru) = 0.42
IF (coeff_fc1(ihru) <= 1.e-6) coeff_fc1(ihru) = 30.0
IF (coeff_fc2(ihru) <= 1.e-6) coeff_fc2(ihru) = 0.7
IF (coeff_fecal(ihru) <= 1.e-6) coeff_fecal(ihru) = 0.11
IF (coeff_plq(ihru) <= 1.e-6) coeff_plq(ihru) = 0.10
IF (coeff_mrt(ihru) <= 1.e-6) coeff_mrt(ihru) = 0.025
IF (coeff_rsp(ihru) <= 1.e-6) coeff_rsp(ihru) = 0.0156
IF (coeff_slg1(ihru) <= 1.e-6) coeff_slg1(ihru) = 4. e-8
IF (coeff_slg2(ihru) <= 1.e-6) coeff_slg2(ihru) = 1.5
IF (coeff_nitr(ihru) <= 1.e-6) coeff_nitr(ihru) = 0.086
IF (coeff_denitr(ihru) <= 1.e-6) coeff_denitr(ihru) = 0.00432


CLOSE (172)
1000  FORMAT (a)
RETURN
END SUBROUTINE readsepticbz
