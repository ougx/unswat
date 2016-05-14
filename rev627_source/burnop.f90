SUBROUTINE burnop
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ibrn        |none          |counter in readmgt
!!    iburn(:     |julian date   |date of burning
!!    burn_frlb   |none          |fraction of biomass and residue that burn(input in
!!                               |management file) range (0 - 1.0)
!!    nro         |none          |sequence number of year in rotation
!!    phub        |              |heat units to schedule burning
!!    pburn       |              |amount of phosphorus that burns - removed from plant
!!                               |phosphorus and added to soil organic phosphorus

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j
REAL :: aphu

j = 0
j = ihru

xx = burn_frlb
bio_ms(j) = bio_ms(j) * xx
plantn(j) = plantn(j) * xx
pburn = plantp(j) * xx
sol_orgp(1,j) = sol_orgp(1,j) + pburn
plantp(j) = plantp(j) - pburn
sol_rsd(1,j) = sol_rsd(1,j) * xx
sol_fon(1,j) = sol_fon(1,j) * xx
sol_aorgn(1,j) = sol_aorgn(1,j) * xx
sol_orgn(1,j) = sol_orgn(1,j) * xx

!!insert new biomss by zhang
!!=================================
IF (cswat == 2) THEN
  sol_lm(1,j) = sol_lm(1,j) * xx
  sol_ls(1,j) = sol_ls(1,j) * xx
  sol_lsc(1,j) = sol_lsc(1,j) * xx
  sol_lsn(1,j) =sol_lsn(1,j) * xx
  sol_lmc(1,j) = sol_lmc(1,j) * xx
  sol_lmn(1,j) = sol_lmn(1,j) * xx
  sol_lsl(1,j) = sol_lsl(1,j) * xx
  
  emitc_d(j) = emitc_d(j) + bio_ms(j) * (1.-xx)
  emitc_d(j) = emitc_d(j) + sol_rsd(1,j) * (1.-xx)
END IF
!!insert new biomss by zhang
!!=================================

RETURN
END SUBROUTINE burnop
