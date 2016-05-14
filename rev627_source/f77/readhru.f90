SUBROUTINE readhru
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU general input file (.hru).
!!    This file contains data related to general processes modeled
!!    at the HRU level.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ifld(:)     |none          |number of HRU (in subbasin) that is a
!!                               |floodplain
!!    ihru        |none          |HRU number
!!    ipot(:)     |none          |number of HRU (in subbasin) that is ponding
!!                               |water--the HRU that the surface runoff from
!!                               |current HRU drains into. This variable is
!!                               |used only for rice paddys or closed
!!                               |depressional areas
!!    irip(:)     |none          |number of HRU (in subbasin) that is a
!!                               |riparian zone
!!    da_km       |km2           |area of the watershed in square kilometers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    canmx(:)    |mm H2O        |maximum canopy storage
!!    cf          |              |this parameter controls the response of
!!                               |decomposition to the combined effect of soil
!!                               |temperature and moisture.
!!    cfh         |              |Maximum humification rate``
!!    cfdec       |              |the undisturbed soil turnover rate under
!!                               |optimum soil water and temperature. Increasing
!!                               |it will increase carbon and organic N decomp.
!!    dis_stream(:) | m          |average distance to stream
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    erorgn(:)   |none          |organic N enrichment ratio, if left blank
!!                               |the model will calculate for every event
!!    erorgp(:)   |none          |organic P enrichment ratio, if left blank
!!                               |the model will calculate for every event
!!    esco(:)     |none          |soil evaporation compensation factor (0-1)
!!    evpot(:)    |none          |pothole evaporation coefficient
!!    fld_fr(:)   |km2/km2       |fraction of HRU area that drains into floodplain
!!    hru_fr(:)   |km2/km2       |fraction of subbasin area contained in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    hru_slp(:)  |m/m           |average slope steepness
!!    lat_sed(:)  |g/L           |sediment concentration in lateral flow
!!    lat_ttime(:)|days          |lateral flow travel time
!!    ov_n(:)     |none          |Manning's "n" value for overland flow
!!    pot_fr(:)   |km2/km2       |fraction of HRU area that drains into pothole
!!    pot_k       |              |conductivity of soil surface layer for pothole infiltration
!!    pot_no3l(:) |1/day         |nitrate decay rate in impounded area
!!    pot_nsed(:) |mg/L          |normal sediment concentration in impounded
!!                               |water (needed only if current HRU is IPOT)
!!    pot_tile(:) |m3/s          |average daily outflow to main channel from
!!                               |tile flow if drainage tiles are installed in
!!                               |pothole (needed only if current HRU is IPOT)
!!    pot_vol(:)  |mm            |initial volume of water stored in the
!!                               |depression/impounded area (read in as mm
!!                               |and converted to m^3) (needed only if current
!!                               |HRU is IPOT)
!!    pot_volx(:) |mm            |maximum volume of water stored in the
!!                               |depression/impounded area (read in as mm
!!                               |and converted to m^3) (needed only if current
!!                               |HRU is IPOT)
!!    rip_fr(:)   |km2/km2       |fraction of HRU area that drains into riparian
!!                               |zone
!!    rsdin(:)    |kg/ha         |initial residue cover
!!    slsoil(:)   |m             |slope length for lateral subsurface flow
!!    slsubbsn(:) |m             |average slope length for subbasin
!!    usle_ls(:)  |none          |USLE equation length slope (LS) factor
!!Modified parameter variable! D. Moriasi 4/8/2014
!!    r2adj       |none          |retention parameter adjustment factor (greater than 1)

!----------------------------------------------------------------------------------------------
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag (=-1 if eof, else =0)
!!    epcohru     |none          |plant water uptake compensation factor (0-1)
!!    escohru     |none          |soil evaporation compensation factor (0-1)
!!    r2adjhru    |none          |retention parameter adjustment factor (=>1) !D.Moriasi 4/8/2014
!!    sin_sl      |none          |Sin(slope angle)
!!    titldum     |NA            |title line of .sub file (not used)
!!    xm          |none          |exponential in equation to calculate
!!                               |USLE LS
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Sin, Atan

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof
REAL :: xm, sin_sl, epcohru, escohru
REAL :: r2adjhru !D. Moriasi 4/4/2014

eof = 0
escohru = 0.
epcohru = 0.
r2adjhru = 0.    !D. Moriasi 4/4/2014

DO
  READ (108,5100) titldum
  READ (108,*) hru_fr(ihru)
  READ (108,*) slsubbsn(ihru)
  READ (108,*) hru_slp(ihru)
  READ (108,*) ov_n(ihru)
  READ (108,*) lat_ttime(ihru)
  READ (108,*) lat_sed(ihru)   !read in in mg/L
  READ (108,*) slsoil(ihru)
  READ (108,*,IOSTAT=eof) canmx(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) escohru
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) epcohru
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) rsdin(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) erorgn(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) erorgp(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) pot_fr(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) fld_fr(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) rip_fr(ihru)
  IF (eof < 0) EXIT
  READ (108,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
!      if (ipot(ihru) == ihru) then   Srini pothole
  READ (108,*,IOSTAT=eof) pot_tilemm(ihru)    !!NUBZ
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) pot_volxmm(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) pot_volmm(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) pot_nsed(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) pot_no3l(ihru)
  IF (eof < 0) EXIT
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!        read (108,5100,iostat=eof) titldum
!        if (eof < 0) exit
!      end if
  READ (108,*,IOSTAT=eof) dep_imp(ihru)
  IF (eof < 0) EXIT
  READ (108,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (108,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (108,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) evpot(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) dis_stream(ihru)
  IF (eof < 0) EXIT
!! armen & stefan changes for SWAT-C
  READ (108,*,IOSTAT=eof) cf(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) cfh(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) cfdec(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) sed_con(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) orgn_con(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) orgp_con(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) soln_con(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) solp_con(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) pot_solpl(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) pot_k(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) n_reduc(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) n_lag(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) n_ln(ihru)
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) n_lnco(ihru)
!-------------------------------------------------------Moriasi 4/8/2014
  IF (eof < 0) EXIT
  READ (108,*,IOSTAT=eof) surlag(ihru)
!-------------------------------------------------------Moriasi 4/8/2014
  READ (108,*,IOSTAT=eof) r2adj(ihru) !Soil retention parameter D. Moriasi 4/8/2014
  EXIT
END DO

IF (n_reduc(ihru) <= 0.) n_reduc(ihru) = 300.
IF (n_lag(ihru) <= 0.) n_lag(ihru) = 0.25
IF (n_ln(ihru) <= 0.) n_ln(ihru) = 2.0
IF (n_lnco(ihru) <= 0.) n_lnco(ihru) = 2.0

!!    compare .hru input values to .bsn input values
IF (escohru > 1.e-4) esco(ihru) = escohru
IF (epcohru > 1.e-4) epco(ihru) = epcohru

!!    set default values
IF (dep_imp(ihru) <=0.) dep_imp(ihru) = depimp_bsn
IF (surlag(ihru) <=0.) surlag(ihru) = surlag_bsn
!     if (ddrain(ihru) <= 0.) ddrain(ihru) = 1000.
!     if (tdrain(ihru) <= 0.) tdrain(ihru) = 24.
!     if (gdrain(ihru) <= 0.) gdrain(ihru) = 96.
!New and modified parameters D. Moriasi 4/8/2014
IF (r2adj(ihru) <= 0.) r2adj(ihru) = r2adj_bsn
!! comment the following line for the hru_fraction data !!
IF (hru_fr(ihru) <= 0.) hru_fr(ihru) = .0000001
IF (slsubbsn(ihru) <= 0.) slsubbsn(ihru) = 50.0
IF (hru_slp(ihru) <= 0.0001) hru_slp(ihru) = .0001
IF (hru_slp(ihru) >= 1.0) hru_slp(ihru) = 1.0
IF (slsoil(ihru) <= 0.)  slsoil(ihru) = slsubbsn(ihru)
IF (esco(ihru) <= 0.) esco(ihru) = .95
!     if (dep_imp(ihru) <= 0.) dep_imp(ihru) = 6000.
!     esco(ihru) = 1. - esco(ihru)
IF (epco(ihru) <= 0. .OR. epco(ihru) > 1.) epco(ihru) = 1.0
IF (evpot(ihru) <= 0.) evpot(ihru) = 0.5
IF (dis_stream(ihru) <= 0.) dis_stream(ihru) = 35.0

!! armen & stefan changes for SWAT-C  &
IF (cf(ihru) <= 0.) cf(ihru)= 1.0
IF (cfh(ihru) <= 0.) cfh(ihru)= 1.0
IF (cfdec(ihru) <= 0.) cfdec(ihru)= 0.055
!! armen & stefan end


!!    calculate USLE slope length factor
xm = 0.
sin_sl = 0.
xm = .6 * (1. - EXP(-35.835 * hru_slp(ihru)))
sin_sl = SIN(ATAN(hru_slp(ihru)))
usle_ls(ihru) = (slsubbsn(ihru)/22.128)**xm * (65.41 * sin_sl *  &
    sin_sl + 4.56 * sin_sl + .065)

!!    other calculations
hru_km(ihru) = sub_km(i) * hru_fr(ihru)
hru_ha(ihru) = hru_km(ihru) * 100.
lat_sed(ihru) = lat_sed(ihru) * 1.e-3     !!mg/L => g/L
pot_vol(ihru) = pot_volmm(ihru)
pot_volx(ihru) = pot_volxmm(ihru)
pot_tile(ihru) = pot_tilemm(ihru)

xx = 10. * pot_volmm(ihru) * hru_ha(ihru) / 1000000.  !! mg/L * m3 * 1000L/m3 * t/1,000,000,000   srini pothole
pot_sed(ihru) = pot_nsed(ihru) * xx
pot_san(ihru) = 0.
pot_sil(ihru) = 0.
pot_cla(ihru) = pot_nsed(ihru) * xx
pot_sag(ihru) = 0.
pot_lag(ihru) = 0.

CLOSE (108)
RETURN
5100 FORMAT (a)
END SUBROUTINE readhru
