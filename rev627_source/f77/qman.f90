FUNCTION qman(x1,x2,x3,x4)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates flow rate or flow velocity using Manning's
!!    equation. If x1 is set to 1, the velocity is calculated. If x1 is set to
!!    cross-sectional area of flow, the flow rate is calculated.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    x1          |m^2 or none   |cross-sectional flow area or 1.
!!    x2          |m             |hydraulic radius
!!    x3          |none          |Manning's "n" value for channel
!!    x4          |m/m           |average slope of channel
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    qman        |m^3/s or m/s  |flow rate or flow velocity
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


REAL, INTENT(IN)                         :: x1
REAL, INTENT(IN)                         :: x2
REAL, INTENT(IN)                         :: x3
REAL, INTENT(INOUT)                      :: x4

REAL :: qman

qman = 0.
qman = x1 * x2 ** .6666 * SQRT(x4) / x3

RETURN
END FUNCTION qman
