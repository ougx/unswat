SUBROUTINE aveval(itel,ave)

! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

IMPLICIT REAL*8 (a-h,o-z)
INTEGER, INTENT(IN)                      :: itel
REAL*8, INTENT(OUT)                      :: ave


REAL*8 sumdat, sumout, cal1, cal2

sumdat=0.
sumout=0.
REWIND(7778)
DO ii=1,itel
  READ(7778, 78) cal1,cal2
  78    FORMAT(2E12.5)
  sumdat=sumdat+cal1
  sumout=sumout+cal2
END DO
ave=(sumout-sumdat)*100./sumdat
RETURN
END SUBROUTINE aveval
