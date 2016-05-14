SUBROUTINE readswq
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads parameters from the subbasin instream water
!!    quality file (.swq) and initializes the QUAL2E variables which apply to
!!    the individual subbasins

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bc1(:)      |1/day or 1/hr |rate constant for biological oxidation of NH3
!!                               |to NO2 in reach at 20 deg C
!!    bc2(:)      |1/day or 1/hr |rate constant for biological oxidation of NO2
!!                               |to NO3 in reach at 20 deg C
!!    bc3(:)      |1/day or 1/hr |rate constant for hydrolysis of organic N to
!!                               |ammonia in reach at 20 deg C
!!    bc4(:)      |1/day or 1/hr |rate constant for the decay of organic P to
!!                               |dissolved P in reach at 20 deg C
!!    chpst_koc(:)  |m**3/g      |pesticide partition coefficient between
!!                               |water and sediment in reach
!!    chpst_mix(:)  |m/day       |mixing velocity (diffusion/dispersion) for
!!                               |pesticide in reach
!!    chpst_rea(:)  |1/day       |pesticide reaction coefficient in reach
!!    chpst_rsp(:)  |m/day       |resuspension velocity in reach for pesticide
!!                               |sorbed to sediment
!!    chpst_stl(:)  |m/day       |settling velocity in reach for pesticide
!!                               |sorbed to sediment
!!    chpst_vol(:)  |m/day       |pesticide volatilization coefficient in reach
!!    rk1(:)      |1/day or 1/hr |CBOD deoxygenation rate coefficient in reach
!!                               |at 20 deg C
!!    rk2(:)      |1/day or 1/hr |reaeration rate in accordance with Fickian
!!                               |diffusion in reach at 20 deg C
!!    rk3(:)      |1/day or 1/hr |rate of loss of CBOD due to settling in reach
!!                               |at 20 deg C
!!    rk4(:)      |mg O2/        |sediment oxygen demand rate in reach
!!                |  ((m**2)*day)|at 20 deg C
!!                |or mg O2/((m**2)*hr)
!!    rk5(:)      |1/day         |coliform die-off rate in reach
!!    rk6(:)      |1/day         |decay rate for arbitrary non-conservative
!!                               |constituent in reach
!!    rs1(:)      |m/day or m/hr |local algal settling rate in reach at 20 deg C
!!    rs2(:)      |(mg disP-P)/  |benthos source rate for dissolved phosphorus
!!                |  ((m**2)*day)|in reach at 20 deg C
!!                |or (mg disP-P)/((m**2)*hr)|
!!    rs3(:)      |(mg NH4-N)/   |benthos source rate for ammonia nitrogen in
!!                |  ((m**2)*day)|reach at 20 deg C
!!                |or (mg NH4-N)/((m**2)*hr)|
!!    rs4(:)      |1/day or 1/hr |rate coefficient for organic nitrogen
!!                               |settling in reach at 20 deg C
!!    rs5(:)      |1/day or 1/hr |organic phosphorus settling rate in reach at
!!                               |20 deg C
!!    rs6(:)      |1/day         |rate coefficient for settling of arbitrary
!!                               |non-conservative constituent in reach
!!    rs7(:)      |(mg ANC)/     |benthal source rate for arbitrary
!!                   ((m**2)*day)|non-conservative constituent in reach
!!    sedpst_act(:) |m           |depth of active sediment layer in reach for
!!                               |pesticide
!!    sedpst_bry(:) |m/day       |pesticide burial velocity in river bed
!!                               |sediment
!!    sedpst_conc(:)|mg/(m**3)   |inital pesticide concentration in river bed
!!                               |sediment
!!    sedpst_rea(:) |1/day       |pesticide reaction coefficient in river bed
!!                               |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line in .wq file (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof

eof = 0

DO
  READ (104,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rs1(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rs2(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rs3(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rs4(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rs5(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rs6(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rs7(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rk1(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rk2(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rk3(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rk4(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rk5(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) rk6(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) bc1(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) bc2(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) bc3(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) bc4(irch)
  IF (eof < 0) EXIT
  READ (104,5100,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) chpst_rea(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) chpst_vol(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) chpst_koc(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) chpst_stl(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) chpst_rsp(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) chpst_mix(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sedpst_conc(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sedpst_rea(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sedpst_bry(irch)
  IF (eof < 0) EXIT
  READ (104,*,IOSTAT=eof) sedpst_act(irch)
  IF (eof < 0) EXIT
!      read (104,*,iostat=eof) biofilm_mumax(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_kinv(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_klw(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_kla(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_cdet(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_bm(irch)
  EXIT
END DO

!!    set default values for undefined parameters
IF (rs1(irch) <= 0.) rs1(irch) = 1.0
IF (rs2(irch) <= 0.) rs2(irch) = 0.05
IF (rs3(irch) <= 0.) rs3(irch) = 0.5
IF (rs4(irch) <= 0.) rs4(irch) = 0.05
IF (rs5(irch) <= 0.) rs5(irch) = 0.05
IF (rs6(irch) <= 0.) rs6(irch) = 2.5
IF (rs7(irch) <= 0.) rs7(irch) = 2.5
IF (rk1(irch) <= 0.) rk1(irch) = 1.71
IF (rk2(irch) <= 0.) rk2(irch) = 1.0    ! previous 50.0
IF (rk4(irch) <= 0.) rk4(irch) = 2.0
IF (rk5(irch) <= 0.) rk5(irch) = 2.0
IF (rk6(irch) <= 0.) rk6(irch) = 1.71
IF (bc1(irch) <= 0.) bc1(irch) = 0.55
IF (bc2(irch) <= 0.) bc2(irch) = 1.1
IF (bc3(irch) <= 0.) bc3(irch) = 0.21
IF (bc4(irch) <= 0.) bc4(irch) = 0.35
IF (chpst_rea(irch) <= 1.e-6) chpst_rea(irch) = 0.007
IF (chpst_vol(irch) <= 1.e-6) chpst_vol(irch) = 0.01
IF (chpst_koc(irch) <= 1.e-6) chpst_koc(irch) = 0.
IF (chpst_stl(irch) <= 1.e-6) chpst_stl(irch) = 1.
IF (chpst_rsp(irch) <= 1.e-6) chpst_rsp(irch) = 0.002
IF (chpst_mix(irch) <= 1.e-6) chpst_mix(irch) = 0.001
IF (sedpst_conc(irch) <= 1.e-6) sedpst_conc(irch) = 0.
IF (sedpst_rea(irch) <= 1.e-6) sedpst_rea(irch) = 0.05
IF (sedpst_bry(irch) <= 1.e-6) sedpst_bry(irch) = 0.002
IF (sedpst_act(irch) <= 1.e-6) sedpst_act(irch) = 0.030

!!  set default values for mike van liew
IF (bc1(irch) <= 0.) bc1(irch) = bc1_bsn
IF (bc2(irch) <= 0.) bc2(irch) = bc2_bsn
IF (bc3(irch) <= 0.) bc3(irch) = bc3_bsn
IF (bc4(irch) <= 0.) bc4(irch) = bc4_bsn
!!  set default values for mike van liew

!! change units from day to hour if hourly routing is performed
IF (ievent > 2) THEN
  rs1(irch) = rs1(irch) / 24.
  rs2(irch) = rs2(irch) / 24.
  rs3(irch) = rs3(irch) / 24.
  rs4(irch) = rs4(irch) / 24.
  rs5(irch) = rs5(irch) / 24.
  rk1(irch) = rk1(irch) / 24.
  rk2(irch) = rk2(irch) / 24.
  rk3(irch) = rk3(irch) / 24.
  rk4(irch) = rk4(irch) / 24.
  bc1(irch) = bc1(irch) / 24.
  bc2(irch) = bc2(irch) / 24.
  bc3(irch) = bc3(irch) / 24.
  bc4(irch) = bc4(irch) / 24.
END IF

CLOSE (104)
RETURN
5100 FORMAT (a)
END SUBROUTINE readswq
