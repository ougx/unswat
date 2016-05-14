SUBROUTINE schedule_ops
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the
!!    hydrologic cycle

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida           |julian date   |day being simulated (current julian date)
!!    inum1          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, plant_no , zz
REAL:: b
j = 0
j = ihru


iops = ioper(ihru)

DO WHILE (iida == iopday(iops,ihru).AND.iyr == iopyr(iops,ihru))
  
  select case(mgt_ops(iops,ihru))
  
  case (0)
  
  case (1)
  xm = 0.6 * (1. - EXP(-35.835 * hru_slp(ihru)))
  sin_sl = SIN(ATAN(hru_slp(ihru)))
  usle_ls(ihru) = (terr_sl(iops,ihru)/22.128) ** xm * (65.41 *  &
      sin_sl * sin_sl + 4.56 * sin_sl + .065)
  usle_mult(ihru) = sol_rock(1,ihru) * usle_k(ihru) *  &
      terr_p(iops,ihru) * usle_ls(ihru) * 11.8
  IF (terr_cn(iops,ihru) > 1.e-6) THEN
    CALL curno(terr_cn(iops,ihru),ihru)
  END IF
  
  case (2)
  ddrain(ihru) = drain_d(iops,ihru)
  gdrain(ihru) = drain_g(iops,ihru)
  tdrain(ihru) = drain_t(iops,ihru)
  dep_imp(ihru) = drain_idep(iops,ihru)
  
!!    define soil layer that the drainage tile is in
  IF (ddrain(ihru) > 0) THEN
    DO jj = 1, sol_nly(ihru)
      IF (ddrain(ihru) < sol_z(jj,ihru)) ldrain(ihru) = jj
      IF (ddrain(ihru) < sol_z(jj,ihru)) EXIT
    END DO
  ELSE
    ldrain(ihru) = 0
  END IF
!!    setting tile lage time
  IF (ldrain(j) > 0 .AND. gdrain(j) > 0.01) THEN
    tile_ttime(j) = 1. - EXP(-24. / gdrain(j))
  ELSE
    tile_ttime(j) = 0.
  END IF
  
  case (3)
  usle_mult(ihru) = usle_mult(ihru) * cont_p(iops,ihru) / usle_p(ihru)
  CALL curno(cont_cn(iops,ihru),ihru)
  
  case (4) !! filter strip
  vfsi(ihru) = filter_i(iops,ihru) !! on off flag
  vfsratio(ihru) = filter_ratio(iops,ihru)
  vfscon(ihru) = filter_con(iops,ihru)
  vfsch(ihru) = filter_ch(iops,ihru)
  
!! Set some defaults if needed
  IF (vfsratio(ihru) <= 0.) vfsratio(ihru) = 0.
!! minimum value for vfsratio is 0 max is 300
  IF (vfsratio(ihru) <= 0.) vfsratio(ihru) = 0.
  IF (vfsratio(ihru) > 300) vfsratio(ihru) = 300
!! minimum value for vfscon is 0.1 default is 0.5 max is 0.95
  IF (vfscon(ihru) <= 0) vfscon(ihru) = 0.5
  IF (vfscon(ihru) <= 0.1) vfscon(ihru) = 0.1
  IF (vfscon(ihru) > 0.95) vfscon(ihru) = 0.95
!! minimum value for vfsch is 0 max is .95
  IF (vfsch(ihru) <= 0.) vfsch(ihru) = 0.
  IF (vfsch(ihru) > .95) vfsch(ihru) = .95
  
  
  case (5)
  CALL curno(strip_cn(iops,ihru),ihru)
  usle_mult(ihru) = usle_mult(ihru) * strip_p(iops,ihru) / usle_p(ihru)
  tover = .0556 * (slsubbsn(j) * strip_n(iops,ihru)) ** .6 / hru_slp(j) ** .3
  
  tconc(ihru) = tconc(ihru) + tover - t_ov(ihru)
  
!       ioper(ihru) = ioper(ihru) + 1
!       iops = ioper(ihru)
  
  case (6)
  CALL curno (fire_cn(iops,ihru),ihru)
  
  case (7)
  IF (ngrwat(ihru) < 0)  ngrwat(ihru) = 0
  ngrwat(ihru) = ngrwat(ihru) + 1
  grwat_n(ihru) = gwatn(iops,ihru)
  grwat_i(ihru) = gwati(iops,ihru)
  grwat_l(ihru) = gwatl(iops,ihru)
  grwat_w(ihru) = gwatw(iops,ihru)
  grwat_d(ihru) = gwatd(iops,ihru)
  grwat_s(ihru) = gwats(iops,ihru)
  grwat_spcon(ihru) = gwatspcon(iops,ihru)
!! Calculate time of concentration for waterway similar to hydroinit.f
  tch = .62 * grwat_l(ihru) * grwat_n(ihru) ** .6 /  &
      (hru_km(ihru) ** .125 * grwat_s(ihru) ** .375)
  tc_gwat(ihru) = tch + t_ov(ihru)
!! Set counter
  k = mhru + ngrwat(ihru)
!!Check the channel to make sure the enter width and depth will work with 8:1 trap channel, assume width is correct
  b = grwat_w(ihru) - 2. * grwat_d(ihru) * 8
!! Depth and Width not possible with 8:1 sideslope and trapazoidal channel assume b =.25*width
  IF (b <= 0.) grwat_d(ihru) = 3. / 64. * grwat_w(ihru)
  
  CALL ttcoef_wway
  
  case (8)
  plant_no = cropno_upd(iops,ihru)
  blai(plant_no) = laimx_upd(iops,ihru)
  hvsti(plant_no) = hi_upd(iops,ihru)
  
  case (9)
!! Implement Residue Management MJW
  IF (so_res_flag(iops,ihru) == 1)  THEN
    min_res(ihru) = so_res(iops,ihru)
  ELSE
    min_res(ihru) = 0.
  END IF
  
  case (10) !! User defined Upland CP removal MJW
  IF (ro_bmp_flag (iops,ihru) == 1) THEN
    bmp_flag(ihru) = 1
    bmp_sed(ihru) = ro_bmp_sed(iops,ihru)  !! Sediment
    bmp_pp(ihru) = ro_bmp_pp(iops,ihru) !! Particulate P
    bmp_sp(ihru) = ro_bmp_sp(iops,ihru)  !! Soluble P
    bmp_pn(ihru) =  ro_bmp_pn(iops,ihru)  !! Particulate N
    bmp_sn(ihru) = ro_bmp_sn(iops,ihru)  !! Soluble N
    bmp_bac(ihru) = ro_bmp_bac(iops,ihru)  !! Bacteria
  ELSE
    bmp_flag(ihru) = 0
    bmp_sed(ihru) = 0.  !! Sediment
    bmp_pp(ihru) = 0. !! Particulate P
    bmp_sp(ihru) = 0.  !! Soluble P
    bmp_pn(ihru) =  0.  !! Particulate N
    bmp_sn(ihru) = 0.  !! Soluble N
    bmp_bac(ihru) = 0.  !! Bacteria
  END IF
  
END select

ioper(ihru) = ioper(ihru) + 1
iops = ioper(ihru)

END DO

RETURN
END SUBROUTINE schedule_ops
