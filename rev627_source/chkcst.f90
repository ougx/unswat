SUBROUTINE chkcst(nopt,xi,bl,bu,ibound)

! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!     This subroutine check if the trial point satisfies all
!     constraints.

!     ibound - violation indicator
!            = -1 initial value
!            = 0  no violation
!            = 1  violation
!     nopt = number of optimizing variables
!     ii = the ii'th variable of the arrays x, bl, and bu


IMPLICIT REAL*8 (a-h,o-z)
INTEGER, INTENT(IN)                      :: nopt
REAL, INTENT(IN)                         :: xi(nopt)
REAL, INTENT(IN)                         :: bl(nopt)
REAL, INTENT(IN)                         :: bu(nopt)
INTEGER, INTENT(OUT)                     :: ibound


ibound = -1

!     Check if explicit constraints are violated

DO ii=1, nopt
  IF (xi(ii) < bl(ii) .OR. xi(ii) > bu(ii)) GO TO 10
END DO
IF (nopt == 1) GO TO 9

!     Check if implicit constraints are violated
!     (no implicit constraints for this function)

!     No constraints are violated

9 ibound = 0
RETURN

!     At least one of the constraints are violated

10 ibound = 1
RETURN
END SUBROUTINE chkcst
