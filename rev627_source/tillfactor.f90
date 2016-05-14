SUBROUTINE tillfactor(jj,bmix,emix,dtil,sol_thick)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04
 
!!!!!!!!!!!!!!!!!!!!!!!
! Armen 16 January 2008
! This procedure increases tillage factor (tillagef(l,jj) per layer for each operation
! The tillage factor settling will depend of soil moisture (tentatively) and must be called every day
! For simplicity the settling is calculated now at the soil carbon sub because soil water content is available

! The tillage factor depends on the cumulative soil disturbance rating = csdr
! For simplicity, csdr is a function of emix
! First step is to calculate "current" csdr by inverting tillage factor function
! The effect of texture on tillage factor (ZZ) is removed first (and recovered at the end of the procedure)
! YY = tillagef(l,jj) / ZZ
! Since the tillage factor function is non linear, iterations are needed
! XX = 0.5 is the initial value that works OK for the range of values observed
! If a layer is only partially tilled then emix is corrected accordingly
use parm

INTEGER, INTENT (in) :: jj
REAL, INTENT (in) :: bmix
INTEGER :: l, m1, m2
REAL :: emix, dtil
REAL :: sol_thick(sol_nly(jj))

emix = emix - bmix ! this is to avoid affecting tillage factor with biological mixing

IF (emix > 0.) THEN
  
  DO l=1, sol_nly(jj)
    
    IF (sol_z(l,jj) <= dtil) THEN
      emix = emix
    ELSE IF (sol_z(l,jj) > dtil .AND. sol_z(l-1,jj) < dtil) THEN
      emix = emix * (dtil - sol_z(l-1,jj)) / sol_thick(l)
    ELSE
      emix = 0.
    END IF
    
! to save computation time if emix = 0 here then the other layers can be avoided
! tillage always proceeds from top to bottom
    IF (emix == 0.) EXIT
    
    xx = 0.
    zz = 3. + (8. - 3.) * EXP(-5.5 * sol_clay(l,jj)/100.)
    yy = tillagef(l,jj) / zz
    m1 = 1
    m2 = 2
    
! empirical solution for x when y is known and y=x/(x+exp(m1-m2*x))
    IF (yy > 0.01) THEN
      xx1 = yy ** EXP(-0.13 + 1.06 * yy)
      xx2 = EXP(0.64 + 0.64 * yy ** 100.)
      xx = xx1 * xx2
    END IF
    
    csdr = xx + emix
    tillagef(l,jj) = zz * (csdr / (csdr + EXP(m1 - m2 * csdr)))
    
  END DO
  
END IF

RETURN
END SUBROUTINE
