SUBROUTINE gcycl
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes the random number seeds. If the user
!!    desires a different set of random numbers for each simulation run,
!!    the random number generator is used to reset the values of the
!!    seeds.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    igen        |none          |random number generator code:
!!                               | 0: use default numbers
!!                               | 1: generate new numbers in every simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    rndseed(:,:)|none          |random number seeds. The seeds in the array
!!                               |are used to generate random numbers for the
!!                               |following purposes
!!                               |(1) wet/dry day probability
!!                               |(2) solar radiation
!!                               |(3) precipitation
!!                               |(4) USLE rainfall erosion index
!!                               |(5) wind speed
!!                               |(6) 0.5 hr rainfall fraction
!!                               |(7) relative humidity
!!                               |(8) maximum temperature
!!                               |(9) minimum temperature
!!                               |(10) generate new random numbers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |variable to hold calculated value
!!    j           |none          |counter
!!    k           |none          |counter, and variable
!!    rn          |none          |random number between 0.0 and 1.0
!!    rndseed10   |none          |seed for random number generator that is
!!                               |used to reset other random number seeds
!!    xx          |none          |dummy variable to accept function value
!!                               |which is then discarded
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Aunif

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

REAL :: xx, rn
INTEGER :: ii, j, k, rndseed10

!!    initialize random number array locator
idg = (/1,2,3,4,5,6,7,8,9/)

!!    initialize random number seeds


DO j = 1, mhru
  rndseed(1,j) = 748932582
  rndseed(2,j) = 1985072130
  rndseed(3,j) = 1631331038
  rndseed(4,j) = 67377721
  rndseed(5,j) = 366304404
  rndseed(6,j) = 1094585182
  rndseed(7,j) = 1767585417
  rndseed(8,j) = 1980520317
  rndseed(9,j) = 392682216
END DO
rndseed10 = 64298628

IF (igen /= 0) THEN
!! assign new random number seeds
  DO j = 1, 9
    rn = 0.
    ii = 0
    rn = aunif(rndseed10)
    ii = 100 * igen * rn
    DO k = 1, ii
      xx = aunif(rndseed10)
    END DO
    rndseed(j,1) = rndseed10
  END DO
  
!! shuffle seeds randomly (Bratley, Fox, Schrage, p34)
  DO j = 9, 1, -1
    ii = 0
    rn = 0.
    ii = idg(j)
    rn = aunif(rndseed10)
    k = j * rn + 1
    idg(j) = idg(k)
    idg(k) = ii
  END DO
END IF

!! assign half-hour maximum rainfall seed to second array location for use
!! in sub-daily pcp generator
DO j = 1, mhru
  rndseed(10,j) = rndseed(idg(6),j)
END DO

DO j = 1, mhru
  rnd2(j) = aunif(rndseed(idg(2),j))
  rnd3(j) = aunif(rndseed(idg(3),j))
  rnd8(j) = aunif(rndseed(idg(8),j))
  rnd9(j) = aunif(rndseed(idg(9),j))
END DO

RETURN
END SUBROUTINE gcycl
