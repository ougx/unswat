SUBROUTINE lwqdef
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine assigns default values for the lake water quality
!!    (.lwq) when the lake water quality file does not exists

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i            |none          |reservoir number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chlar(:)      |none          |chlorophyll-a production coefficient for
!!                                 |reservoir
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
!!    seccir(:)     |none          |water clarity coefficient for reservoir
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

!!    set default values for parameters
IF (chlar(i) <= 1.e-6) chlar(i) = 1.
IF (seccir(i) <= 1.e-6) seccir(i) = 1.
IF (lkpst_conc(i) <= 1.e-6) lkpst_conc(i) = 0.
IF (lkpst_rea(i) <= 1.e-6) lkpst_rea(i) = 0.007
IF (lkpst_vol(i) <= 1.e-6) lkpst_vol(i) = 0.01
IF (lkpst_koc(i) <= 1.e-6) lkpst_koc(i) = 0.
IF (lkpst_stl(i) <= 1.e-6) lkpst_stl(i) = 1.
IF (lkpst_rsp(i) <= 1.e-6) lkpst_rsp(i) = 0.002
IF (lkpst_mix(i) <= 1.e-6) lkpst_mix(i) = 0.001
IF (lkspst_conc(i) <= 1.e-6) lkspst_conc(i) = 0.
IF (lkspst_rea(i) <= 1.e-6) lkspst_rea(i) = 0.05
IF (lkspst_bry(i) <= 1.e-6) lkspst_bry(i) = 0.002
IF (lkspst_act(i) <= 1.e-6) lkspst_act(i) = 0.030

RETURN
END SUBROUTINE lwqdef
