SUBROUTINE soil_write
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes output to the output.sol file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, l
REAL :: solp_t, solno3_t, solorgn_t, solorgp_t

DO j = 1,nhru
  solp_t = 0.
  solno3_t = 0.
  solorgn_t = 0.
  solorgp_t = 0.
  DO l = 1,sol_nly(j)
    solp_t = solp_t + sol_solp(l,j)
    solno3_t = solno3_t + sol_no3(l,j)
!if (cswat == 0) then
! solorgn_t = solorgn_t + sol_orgn(l,j)
!else
! solorgn_t = solorgn_t + sol_n(l,j)
!end if
    
!!By Zhang
!!============
    IF (cswat == 0) THEN
      solorgn_t = solorgn_t + sol_orgn(l,j)
    END IF
    IF (cswat == 1) THEN
      solorgn_t = solorgn_t + sol_n(l,j)
    END IF
    IF (cswat ==2) THEN
      solorgn_t = solorgn_t + sol_hsn(l,j) + sol_hpn(l,j)
    END IF
!!By Zhang
!!============
    
    solorgp_t = solorgp_t + sol_orgp(l,j)
  END DO
  WRITE (121,1000) i, subnum(j), hruno(j), sol_rsd(1,j), solp_t,  &
      solno3_t, solorgn_t, solorgp_t, cnday(j)
END DO

RETURN
1000 FORMAT ('SNU   ',i4,1X,a5,a4,1X,6F10.2)
END SUBROUTINE soil_write
