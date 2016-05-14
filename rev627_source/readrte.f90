SUBROUTINE readrte
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the reach (main channel) input file
!!    (.rte). This file contains data related to channel attributes. Only
!!    one reach file should be made for each subbasin. If multiple HRUs are
!!    modeled within a subbasin, the same .rte file should be listed for all
!!    HRUs in file.cio

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bnk(:)  |days        |alpha factor for bank storage recession curve
!!    alpha_bnke(:) |none        |Exp(-alpha_bnk(:))
!!    chside(:)     |            |change in horizontal distance per unit
!!                               |  vertical distance (0.0 - 5)
!!                               |0 = for vertical channel bank
!!                               |5 = for channel bank with gentl side slope
!!    ch_bnk_bd(:)  |(g/cc)      |bulk density of channel bank sediment (1.1-1.9)
!!    ch_bed_bd(:)  |(g/cc)      |bulk density of channel bed sediment (1.1-1.9)
!!    ch_bnk_kd(:)  |            |erodibility of channel bank sediment by jet test
!!                               | (Peter Allen needs to give more info on this)
!!    ch_bed_kd(:)  |            |erodibility of channel bed sediment by jet test
!!                               | (Peter Allen needs to give more info on this)
!!    ch_bnk_d50(:) |            |D50(median) particle size diameter of channel
!!                               |  bank sediment (0.001 - 20)
!!    ch_bed_d50(:) |            |D50(median) particle size diameter of channel
!!                               |  bed sediment (micrometers) (0.001 - 20)
!!    ch_cov1(:)    |none        |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_cov2(:)    |none        |channel cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_d(:)       |m           |average depth of main channel
!!    ch_di(:)      |m           |initial depth of main channel
!!    ch_eqn        |            |sediment routine methods:
!!                               | 0 = original SWAT method
!!                               | 1 = Bagnold's
!!                               | 2 = Kodatie
!!                               | 3 = Molinas WU
!!                               | 4 = Yang
!!    ch_erod(:)    |none        |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_k(2,:)     |mm/hr       |effective hydraulic conductivity of
!!                               |main channel alluvium
!!    ch_l2(:)      |km          |length of main channel
!!    ch_li(:)      |km          |initial length of main channel
!!    ch_n(2,:)     |none        |Manning's "n" value for the main channel
!!    ch_onco(:)    |ppm         |channel organic n concentration
!!    ch_opco(:)    |ppm         |channel organic p concentration
!!    ch_s(2,:)     |m/m         |average slope of main channel
!!    ch_si(:)      |m/m         |initial slope of main channel
!!    ch_w(2,:)     |m           |average width of main channel
!!    ch_wdr(:)     |m/m         |channel width to depth ratio
!!    prf(:)      |none          |Reach peak rate adjustment factor for sediment
!!                               |routing in the channel. Allows impact of
!!                               |peak flow rate on sediment routing and
!!                               |channel reshaping to be taken into account.
!!    tc_bed        |N/m2        |critical shear stress of channel bed
!!    tc_bnk        |N/m2        |critical shear stress of channel bank
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line of .rte file (not used elsewhere)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ SUBROUTINES/FUNCTIONS ~ ~ ~ ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof
REAL :: bnksize, bedsize

eof = 0
DO
  READ (103,5000) titldum
  READ (103,*) ch_w(2,irch)
  READ (103,*) ch_d(irch)
  READ (103,*) ch_s(2,irch)
  READ (103,*) ch_l2(irch)
  READ (103,*) ch_n(2,irch)
  READ (103,*) ch_k(2,irch)
  READ (103,*) ch_cov1(irch)
  READ (103,*,IOSTAT=eof) ch_cov2(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_wdr(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) alpha_bnk(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) icanal(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_onco(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_opco(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) chside(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_bnk_bd(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_bed_bd(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_bnk_kd(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_bed_kd(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_bnk_d50(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_bed_d50(irch)
  IF (eof < 0) EXIT
  READ (103,5000,IOSTAT=eof) tc_bnk(irch)
  IF (eof < 0) EXIT
  READ (103,5000,IOSTAT=eof) tc_bed(irch)
  IF (eof < 0) EXIT
  READ (103,5100,IOSTAT=eof) (ch_erodmo(irch,mo), mo = 1,12)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) ch_eqn(irch)
  IF (eof < 0) EXIT
  READ (103,*,IOSTAT=eof) prf(irch)
  EXIT
END DO

!!    set default values for parameters
!!     if (tc_bnk(irch) <= 1.e-6) tc_bnk(irch) = 0.001
!!     if (tc_bed(irch) <= 1.e-6) tc_bed(irch) = 0.001
IF (ch_s(2,irch) <= 0.) ch_s(2,irch) = .0001
IF (ch_n(2,irch) <= 0.01) ch_n(2,irch) = .01
IF (ch_n(2,irch) >= 0.70) ch_n(2,irch) = 0.70
IF (ch_l2(irch) <= 0.) ch_l2(irch) = .0010
IF (ch_wdr(irch) <= 0.) ch_wdr(irch) = 3.5
IF (chside(irch) <= 1.e-6) chside(irch) = 2.0
IF (tc_bnk(irch) <= 0.) tc_bnk(irch)=0. !! Critical shear stress (N.m^2)
IF (tc_bed(irch) <= 0.) tc_bed(irch)=0. !! Critical shear stress (N.m^2)
IF (prf(irch) <= 0.) prf(irch) = prf_bsn

IF (ch_eqn(irch) <= 0) THEN
  ch_eqn(irch)=0 !! SWAT Default sediment routing routine
  IF (ch_cov1(irch) <= 0.0) ch_cov1(irch) = 0.0
  IF (ch_cov2(irch) <= 0.0) ch_cov2(irch) = 0.0
  IF (ch_cov1(irch) >= 1.0) ch_cov1(irch) = 1.0
  IF (ch_cov2(irch) >= 1.0) ch_cov2(irch) = 1.0
ELSE
  IF (ch_cov1(irch) <= 0.0) ch_cov1(irch) = 1.0
  IF (ch_cov2(irch) <= 0.0) ch_cov2(irch) = 1.0
  IF (ch_cov1(irch) >= 25.) ch_cov1(irch) = 25.
  IF (ch_cov2(irch) >= 25.) ch_cov2(irch) = 25.
END IF


!!    Bank material is assumed to be silt type partcile if not given.
IF (ch_bnk_d50(irch) <= 1.e-6) ch_bnk_d50(irch) = 50. !! Units are in Micrometer
IF (ch_bnk_d50(irch) > 10000) ch_bnk_d50(irch) = 10000.


bnksize = ch_bnk_d50(irch)/1000.  !! Units conversion Micrometer to Millimeters
!!    Channel sediment particle size distribution
!!    Clayey bank
IF (bnksize <= 0.005) THEN
  ch_bnk_cla(irch) = 0.65
  ch_bnk_sil(irch) = 0.15
  ch_bnk_san(irch) = 0.15
  ch_bnk_gra(irch) = 0.05
END IF

!!    Silty bank
IF (bnksize > 0.005 .AND. bnksize <= 0.05) THEN
  ch_bnk_sil(irch) = 0.65
  ch_bnk_cla(irch) = 0.15
  ch_bnk_san(irch) = 0.15
  ch_bnk_gra(irch) = 0.05
END IF

!!    Sandy bank
IF (bnksize > 0.05 .AND. bnksize <= 2.) THEN
  ch_bnk_san(irch) = 0.65
  ch_bnk_sil(irch) = 0.15
  ch_bnk_cla(irch) = 0.15
  ch_bnk_gra(irch) = 0.05
END IF

!!    Gravel bank
IF (bnksize > 2.) THEN
  ch_bnk_gra(irch) = 0.65
  ch_bnk_san(irch) = 0.15
  ch_bnk_sil(irch) = 0.15
  ch_bnk_cla(irch) = 0.05
END IF

!!    Bed material is assumed to be sand type partcile if not given.
IF (ch_bed_d50(irch) <= 1.e-6) ch_bed_d50(irch) = 500 !! Units are in Micrometer
IF (ch_bed_d50(irch) > 10000) ch_bed_d50(irch) = 10000.

!!    Channel sediment particle size distribution
!!    Clayey bed
bedsize = ch_bed_d50(irch)/1000.  !! Units conversion Micrometer to Millimeters
IF (bedsize <= 0.005) THEN
  ch_bed_cla(irch) = 0.65
  ch_bed_sil(irch) = 0.15
  ch_bed_san(irch) = 0.15
  ch_bed_gra(irch) = 0.05
END IF

!!    Silty bed
IF (bedsize > 0.005 .AND. bedsize <= 0.05) THEN
  ch_bed_sil(irch) = 0.65
  ch_bed_cla(irch) = 0.15
  ch_bed_san(irch) = 0.15
  ch_bed_gra(irch) = 0.05
END IF

!!    Sandy bed
IF (bedsize > 0.05 .AND. bedsize <= 2.) THEN
  ch_bed_san(irch) = 0.65
  ch_bed_sil(irch) = 0.15
  ch_bed_cla(irch) = 0.15
  ch_bed_gra(irch) = 0.05
END IF

!!    Gravel bed
IF (bedsize > 2.) THEN
  ch_bed_gra(irch) = 0.65
  ch_bed_san(irch) = 0.15
  ch_bed_sil(irch) = 0.15
  ch_bed_cla(irch) = 0.05
END IF

!!    Bulk density of channel bank sediment
IF (ch_bnk_bd(irch) <= 1.e-6) ch_bnk_bd(irch) = 1.40 !! Silty loam bank

!!    Bulk density of channel bed sediment
IF (ch_bed_bd(irch) <= 1.e-6) ch_bed_bd(irch) = 1.50  !! Sandy loam bed


!!    An estimate of Critical shear stress if it is not given (N/m^2)
!! Critical shear stress based on silt and clay %
!! Critical Shear Stress based on Julian and Torres (2005)
!!    Units of critical shear stress (N/m^2)
sc = 0.
IF  (tc_bnk(irch) <= 1.e-6) THEN
  sc = (ch_bnk_sil(irch) + ch_bnk_cla(irch)) * 100.
  tc_bnk(irch) = (0.1 + (0.1779*sc) + (0.0028*(sc)**2)  &
      - ((2.34E-05)*(sc)**3)) * ch_cov1(irch)
END IF

IF  (tc_bed(irch) <= 1.e-6) THEN
  sc = (ch_bed_sil(irch) + ch_bed_cla(irch)) * 100.
  tc_bed(irch) = (0.1 + (0.1779*sc) + (0.0028*(sc)**2)  &
      - ((2.34E-05)*(sc)**3)) * ch_cov2(irch)
END IF

!!  An estimate of channel bank erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
IF (ch_bnk_kd(irch) <= 1.e-6) THEN
  IF (tc_bnk(irch) <= 1.e-6) THEN
    ch_bnk_kd(irch) = 0.2
  ELSE
    ch_bnk_kd(irch) = 0.2 / SQRT(tc_bnk(irch))
  END IF
END IF

!!  An estimate of channel bed erodibility coefficient from jet test if it is not available
!!  Units of kd is (cm^3/N/s)
!!  Base on Hanson and Simon, 2001
IF (ch_bed_kd(irch) <= 1.e-6) THEN
  IF (tc_bed(irch) <= 1.e-6) THEN
    ch_bed_kd(irch) = 0.2
  ELSE
    ch_bed_kd(irch) = 0.2 / SQRT(tc_bed(irch))
  END IF
END IF

sumerod = 0.
DO mo = 1, 12
  sumerod = sumerod + ch_erodmo(irch,mo)
END DO

IF (sumerod < 1.e-6) THEN
  DO mo = 1, 12
    ch_erodmo(irch,mo) = ch_cov1(irch)
  END DO
END IF

!!    set default values for mike van liew
IF (ch_onco(irch) <= 0.) ch_onco(irch) = ch_onco_bsn
IF (ch_opco(irch) <= 0.) ch_opco(irch) = ch_opco_bsn
!!    set default values for mike van liew


!!    initialize variables for channel degradation
ch_di(irch) = ch_d(irch)
ch_li(irch) = ch_l2(irch)
ch_si(irch) = ch_s(2,irch)
ch_wi(irch) = ch_w(2,irch)

CLOSE (103)
RETURN
5000  FORMAT (a)
5100  FORMAT (12F6.2)
END SUBROUTINE readrte
