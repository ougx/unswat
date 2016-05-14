SUBROUTINE readsol
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin soil properties file
!!    (.sol). This file contains data related to soil physical properties and
!!    general chemical properties.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    mlyr          |none          |maximum number of soil layers
!!    idplt(:)      |none          |land cover/crop identification code for
!!                                 |first crop grown in HRU (the only crop if
!!                                 |there is no rotation)
!!    rdmx(:)       |m             |maximum root depth of plant
!!    rsdin(:)      |kg/ha         |initial residue cover
!!    sol_no3(:,:)  |mg N/kg       |concentration of nitrate in soil layer
!!    sol_orgn(1,:) |mg N/kg soil  |organic N concentration in top soil layer
!!    sol_orgp(1,:) |mg P/kg soil  |organic P concentration in top soil layer
!!    sol_solp(1,:) |mg P/kg soil  |soluble P concentration in top soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anion_excl(:) |none          |fraction of porosity from which anions
!!                                 |are excluded
!!    sol_clay(:,:) |%             |percent clay content in soil material
!!    sol_rock(:,:) |%            |percent of rock fragments in soil layer
!!    sol_silt(:,:) |%             |percent silt content in soil material
!!    snam(:)       |NA            |soil series name
!!    sol_alb(:)    |none          |albedo when soil is moist
!!    sol_awc(:,:)  |mm H20/mm soil|available water capacity of soil layer
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_crk(:)    |none          |crack volume potential of soil
!!    sol_k(:,:)    |mm/hr         |saturated hydraulic conductivity of soil
!!                                 |layer
!!    sol_nly(:)    |none          |number of soil layers
!!    sol_no3(:,:)  |mg N/kg       |concentration of nitrate in soil layer
!!    sol_orgn(1,:) |mg N/kg soil  |organic N concentration in top soil layer
!!    sol_orgp(1,:) |mg P/kg soil  |organic P concentration in top soil layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil layer
!!                                 |classified as residue
!!    sol_solp(1,:) |mg P/kg soil  |soluble P concentration in top soil layer
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus
!!                                 |pool
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    sol_zmx(:)    |mm            |maximum rooting depth
!!    usle_k(:)     |none          |USLE equation soil erodibility (K) factor
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flag        |none          |flag to exit do loop
!!    j           |none          |counter
!!    jj          |none          |dummy variable to hold value
!!    n           |none          |counter
!!    nly         |none          |number of soil layers
!!    plt_zmx     |mm            |rooting depth of plant
!!    sol_sand(:,:) |%             |percent sand content of soil material
!!    sol_ec(:)   |dS/m          |electrical conductivity of soil layer
!!    titldum     |NA            |title line/skipped line in .sol file
!!    xx          |none          |variable to hold value
!!    yy          |none          |variable to hold value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Exp, Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
use rossmod

CHARACTER (LEN=80) :: titldum
!      integer :: j, nly, n, jj, flag, eof
INTEGER :: j, nly, eof            !claire: jj, n, and flag are not used 12/02/09
!      real :: xx, plt_zmx, yy
REAL :: plt_zmx                   !Claire, xx and yy are not used 12/2/09

!!    initialize local variables
nly = 0
plt_zmx = 0.

READ (107,5500) titldum
READ (107,5100) snam(ihru)
READ (107,5200) hydgrp(ihru)
READ (107,5300) sol_zmx(ihru)
READ (107,5400) anion_excl(ihru)
READ (107,5600) sol_crk(ihru)
READ (107,5500) titldum
READ (107,5000) (sol_z(j,ihru), j = 1, mlyr)


!! calculate number of soil layers in HRU soil series
DO j = 1, mlyr
!!    khan soils
!      sol_z(j,ihru) = sol_z(j,ihru) / 5.0
  IF (sol_z(j,ihru) <= 0.001) sol_nly(ihru) = j - 1
  IF (sol_z(j,ihru) <= 0.001) EXIT
END DO
IF (sol_nly(ihru) == 0) sol_nly(ihru) = 10
nly = sol_nly(ihru)

eof = 0
DO
  READ (107,5000) (sol_bd(j,ihru), j = 1, nly)
  READ (107,5000) (sol_awc(j,ihru), j = 1, nly)
  READ (107,5000) (sol_k(j,ihru), j = 1, nly)
  READ (107,5000) (sol_cbn(j,ihru), j = 1, nly)
  READ (107,5000) (sol_clay(j,ihru), j = 1, nly)
  READ (107,5000) (sol_silt(j,ihru), j = 1, nly)
  READ (107,5000) (sol_sand(j,ihru), j = 1, nly)
  
  READ (107,5000) (sol_rock(j,ihru), j = 1, nly)
  READ (107,5000) sol_alb(ihru)
  READ (107,5000) usle_k(ihru)
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes
  READ (107,5000,IOSTAT=eof) (sol_ec(j,ihru), j = 1, nly)
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes
  
!! MJW added rev 490
!!CaCo3 content (%)
  IF (eof < 0) EXIT
  READ (107,5000,IOSTAT=eof) (sol_cal(j,ihru), j = 1, nly)
!! PH-H20
  IF (eof < 0) EXIT
  READ (107,5000,IOSTAT=eof) (sol_ph(j,ihru), j = 1, nly)
  
  IF (eof < 0) EXIT
  EXIT
END DO

!!Armen January 2009
DO j=1, nly
  IF (sol_rock(j,ihru) > 98.0) sol_rock(j,ihru) = 98.0
  IF (sol_awc(j,ihru) <= .01) sol_awc(j,ihru) = .01
  IF (sol_awc(j,ihru) >= .80) sol_awc(j,ihru) = .80
  sol_n(j,ihru) = sol_cbn(j,ihru) / 11.0
END DO
!!Armen January 2009 end

!!OGXinSWAT: label if the first layer is split
IF (ievent >0) solcol(ihru)%lay1_split=0

!!    add 10mm layer at surface of soil
IF (sol_z(1,ihru) > 10.1) THEN
  sol_nly(ihru) = sol_nly(ihru) + 1
  nly = nly + 1
  
!!OGXinSWAT
  IF (ievent >0) solcol(ihru)%lay1_split=1
  
  DO j = nly, 2, -1
    sol_z(j,ihru) = sol_z(j-1,ihru)
    sol_bd(j,ihru) = sol_bd(j-1,ihru)
    sol_awc(j,ihru) = sol_awc(j-1,ihru)
    sol_k(j,ihru) = sol_k(j-1,ihru)
    sol_cbn(j,ihru) = sol_cbn(j-1,ihru)
!!Armen January 2009
    sol_n(j,ihru) = sol_n(j-1,ihru)
!                 sol_mc(j,ihru) = sol_mc(j-1,ihru)
!                 sol_mn(j,ihru) = sol_mn(j-1,ihru)
!                 sol_mp(j,ihru) = sol_mp(j-1,ihru)
    sol_rock(j,ihru) = sol_rock(j-1,ihru) !!! Armen 13 Jan 2008
    sol_clay(j,ihru) = sol_clay(j-1,ihru)
    sol_sand(j,ihru) = sol_sand(j-1,ihru) !!! Claire 2 Dec 2009
    sol_silt(j,ihru) = sol_silt(j-1,ihru) !!! Claire 2 Dec 2009
    sol_ph(j,ihru) = sol_ph(j-1,ihru) !! mjw rev 490
    sol_cal(j,ihru) = sol_cal(j-1,ihru) !! mjw rev 490
!!Armen January 2009 end
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes
    sol_ec(j,ihru) = sol_ec(j-1,ihru)
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes
    sol_no3(j,ihru) = sol_no3(j-1,ihru)
    sol_orgn(j,ihru) = sol_orgn(j-1,ihru)
    sol_orgp(j,ihru) = sol_orgp(j-1,ihru)
    sol_solp(j,ihru) = sol_solp(j-1,ihru)
  END DO
  sol_z(1,ihru) = 10.
END IF

IF (isproj == 2) THEN
  CALL estimate_ksat(sol_clay(j,ihru),sol_k(j,ihru))  !!  NK June 28, 2006
END IF


!!    compare maximum rooting depth in soil to maximum rooting depth of
!!    plant
IF (sol_zmx(ihru) <= 0.001) sol_zmx(ihru) = sol_z(nly,ihru)
plt_zmx = 0.
IF (idplt(ihru) > 0) THEN
  IF (idc(idplt(ihru)) > 0) THEN
    plt_zmx = 1000. * rdmx(idplt(ihru))
  END IF
END IF
IF (sol_zmx(ihru) > 1. .AND. plt_zmx > 1.) THEN
  sol_zmx(ihru) = MIN(sol_zmx(ihru),plt_zmx)
ELSE
!! if one value is missing it will set to the one available
  sol_zmx(ihru) = MAX(sol_zmx(ihru),plt_zmx)
END IF

!! create a layer boundary at maximum rooting depth (sol_zmx)
!if (sol_zmx(i) > 0.001.and.sol_zmx(ihru)/=sol_z(nly,ihru)) then
!   call layersplit (sol_zmx(ihru))
!end if

!! create a bizone layer in septic HRUs
IF (isep_opt(ihru) /= 0) THEN
  IF (bz_z(ihru)+bz_thk(ihru) > sol_z(nly,ihru)) THEN
    IF (sol_z(nly,ihru)>bz_thk(ihru)+10.) THEN !min. soil thickness for biozone layer (10MM top+biozone layer thickness)
      bz_z(ihru) = sol_z(nly,ihru) - bz_thk(ihru)
    ELSE
      bz_z(ihru) = sol_z(nly,ihru)
      sol_z(nly,ihru) = sol_z(nly,ihru) + bz_thk(ihru)
    END IF
  END IF
  IF (bz_z(ihru) > 0.) THEN
    CALL layersplit (bz_z(ihru))
    dep_new = bz_z(ihru) + bz_thk(ihru)
    CALL layersplit (dep_new)
    i_sep(ihru) = iseptic
  END IF
END IF

nly = sol_nly(ihru)

!!    set default values/initialize variables
IF (sol_alb(ihru) < 0.1) sol_alb(ihru) = 0.1
IF (anion_excl(ihru) <= 1.e-6) anion_excl(ihru) = anion_excl_bsn
IF (anion_excl(ihru) >= 1.) anion_excl(ihru) = 0.99
IF (rsdin(ihru) > 0.) sol_rsd(1,ihru) = rsdin(ihru)
DO j = 1, nly
  a = 50.0
  b = 20.0
  c = 5.0
  d = 2.0
  nota = 10
  IF (sol_k(j,ihru) <= 0.0) THEN
    IF (hydgrp(ihru) == "A") THEN
      sol_k(j,ihru) = a
    ELSE
      IF (hydgrp(ihru) == "B") THEN
        sol_k(j,ihru) = b
      ELSE
        IF (hydgrp(ihru) == "C") THEN
          sol_k(j,ihru) = c
        ELSE
          IF (hydgrp(ihru) == "D") THEN
!            sol_k(j,ihru) = c
            sol_k(j,ihru) = d          !Claire 12/2/09
          ELSE
            sol_k(j,ihru) = nota
          END IF
        END IF
      END IF
    END IF
  END IF
  IF (sol_bd(j,ihru) <= 1.e-6) sol_bd(j,ihru) = 1.3
  IF (sol_bd(j,ihru) > 2.) sol_bd(j,ihru) = 2.0
  IF (sol_awc(j,ihru) <= 0.) sol_awc(j,ihru) = .005
!! Defaults for ph and calcium mjw average of 20,000 SSURGO soils mjw rev 490
  IF (sol_cal(j,ihru)<= 1.e-6) sol_cal(j,ihru) = 2.8
  IF (sol_ph(j,ihru)<= 1.e-6) sol_ph(j,ihru) = 6.5
END DO


CLOSE (107)
RETURN
5000 FORMAT (27X,15F12.2)
5100 FORMAT (12X,a16)
5200 FORMAT (24X,a1)
5300 FORMAT (28X,f12.2)
5400 FORMAT (51X,f5.3)
5500 FORMAT (a80)
5600 FORMAT (33X,f5.3)
END SUBROUTINE readsol
