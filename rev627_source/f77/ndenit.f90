SUBROUTINE ndenit(k,j,cdg,wdn,void)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01
 
!!    this subroutine computes denitrification

use parm

INTEGER, INTENT(INOUT)                   :: k
INTEGER, INTENT(INOUT)                   :: j
REAL, INTENT(IN)                         :: cdg
REAL, INTENT(OUT)                        :: wdn
REAL, INTENT(INOUT)                      :: void



wdn = 0.
vof = 1. / (1. + (void/0.04)**5)
wdn = sol_no3(k,j) * (1. - EXP(-cdn * cdg * vof * sol_cbn(k,j)))
sol_no3(k,j) = sol_no3(k,j) - wdn

RETURN
END SUBROUTINE ndenit
