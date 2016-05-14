SUBROUTINE rootfr
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03
 
!! This subroutine distributes dead root mass through the soil profile
!! code developed by Armen R. Kemanian in 2008
!! March, 2009 further adjustments expected

use parm

REAL :: sol_thick(sol_nly(ihru))
REAL :: cum_rd, cum_d, cum_rf, x1, x2
INTEGER :: k, l, jj

jj = ihru

IF (stsol_rd(jj) < 1.e-6) THEN
  rtfr(1) = 1
  RETURN
END IF

! Normalized Root Density = 1.15*exp[-11.7*NRD] + 0.022, where NRD = normalized rooting depth
! Parameters of Normalized Root Density Function from Dwyer et al 19xx
a = 1.15
b = 11.7
c = 0.022
d = 0.12029 ! Integral of Normalized Root Distribution Function
! from 0 to 1 (normalized depth) = 0.12029

l = 0
k = 0
cum_d = 0.
cum_rf = 0.
sol_thick(:) = 0.
rtfr = 0.

DO l=1, sol_nly(jj)
  IF (l == 1) THEN
    sol_thick(l) = sol_z(l,jj)
  ELSE
    sol_thick(l) = sol_z(l,jj) - sol_z(l-1,jj)
  END IF
  
  cum_d = cum_d + sol_thick(l)
  IF (cum_d >= stsol_rd(jj)) cum_rd = stsol_rd(jj)
  IF (cum_d < stsol_rd(jj)) cum_rd = cum_d
  x1 = (cum_rd - sol_thick(l)) / stsol_rd(jj)
  x2 = cum_rd / stsol_rd(jj)
  xx1 = -b * x1
  IF (xx1 > 20.) xx1 = 20.
  xx2 = -b * x2
  IF (xx2 > 20.) xx2 = 20.
  rtfr(l)=(a/b*(EXP(xx1) - EXP(xx2)) + c *(x2 - x1)) / d
  xx = cum_rf
  cum_rf = cum_rf + rtfr(l)
  IF (cum_rf > 1.) THEN
    rtfr(l) = 1. - xx
    cum_rf = 1.0
  END IF
  k = l
  IF (cum_rd >= stsol_rd(jj)) EXIT
  
END DO

!!  ensures that cumulative fractional root distribution = 1
DO l=1, sol_nly(jj)
  rtfr(l) = rtfr(l) / cum_rf
  IF (l == k) EXIT ! exits loop on the same layer as the previous loop
END DO

END SUBROUTINE
