SUBROUTINE urb_bmp
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:05
 
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name       |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

j = 0
j = ihru

!! convert to ppm -> (kg/ha)*100./mm = ppm
IF (qdr(j) > 0.1) THEN
  xx = 100. / qdr(j)
  sedppm = 1000. * xx * sedyld(j) / hru_ha(j)
  solnppm = xx * (surqno3(j) + latno3(j) + no3gw(j))
  solpppm = xx * (surqsolp(j) + minpgw(j))
  sednppm = xx * sedorgn(j)
  sedpppm = xx * (sedorgp(j) + sedminpa(j) + sedminps(j))
  
  IF (sedppm > sed_con (j)) THEN
    sedyld(j) = sed_con(j) * hru_ha(j) / xx / 1000.
  END IF
  
  IF (solnppm > soln_con(j)) THEN
    surqno3(j) = soln_con(j) / xx
    latno3(j) = soln_con(j) / xx
    no3gw(j) = soln_con(j) / xx
  END IF
  
  IF (solpppm > solp_con(j)) THEN
    surqsolp(j) = solp_con(j) / xx
    minpgw(j) = solp_con(j) / xx
  END IF
  
  IF (sednppm > orgn_con(j)) THEN
    sedorgn(j) = orgn_con(j) / xx
  END IF
  
  IF (sedpppm > orgp_con(j)) THEN
    sedorgn(j)= orgp_con(j) / xx
    sedminpa(j)= orgp_con(j) / xx
    sedminps(j)= orgp_con(j) / xx
  END IF
  
END IF

RETURN
END SUBROUTINE urb_bmp
