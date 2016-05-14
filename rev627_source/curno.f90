SUBROUTINE curno(cnn,h)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the curve numbers for moisture conditions
!!    I and III and calculates coefficents and shape parameters for the
!!    water retention curve
!!    the coefficents and shape parameters are calculated by one of two methods:
!!    the default method is to make them a function of soil water, the
!!    alternative method (labeled new) is to make them a function of
!!    accumulated PET, precipitation and surface runoff.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnn         |none          |SCS runoff curve number for moisture
!!                               |condition II
!!    h           |none          |HRU number
!!    sol_sumfc(:)|mm H2O        |amount of water held in soil profile at
!!                               |field capacity
!!    sol_sumul(:)|mm H2O        |amount of water held in soil profile at
!!                               |saturation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cn1(:)      |none          |SCS runoff curve number for moisture
!!                               |condition I
!!    cn3(:)      |none          |SCS runoff curve number for moisture
!!                               |condition III
!!    sci(:)      |none          |retention coefficient for cn method based on
!!                               |plant ET
!!    smx(:)      |none          |retention coefficient for cn method based on
!!                               |soil moisture
!!    wrt(1,:)    |none          |1st shape parameter for calculation of
!!                               |water retention
!!    wrt(2,:)    |none          |2nd shape parameter for calculation of
!!                               |water retention
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c2          |none          |variable used to hold calculated value
!!    rto3        |none          |fraction difference between CN3 and CN1
!!                               |retention parameters
!!    rtos        |none          |fraction difference between CN=99 and CN1
!!                               |retention parameters
!!    s3          |none          |retention parameter for CN3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: ascrv

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


REAL, INTENT(IN)                         :: cnn
INTEGER, INTENT(INOUT)                   :: h


REAL :: c2, s3, rto3, rtos

cn2(h) = cnn
IF (cn1(h) > 1.e-6) smxold = 254.* (100. / cn1(h) - 1.)
c2 = 0.
cn3(h) = 0.
s3 = 0.
cn1(h) = 0.

!! calculate moisture condition I and III curve numbers
c2 = 100. - cnn
cn1(h) = cnn - 20. * c2 / (c2 + EXP(2.533 - 0.0636 * c2))
cn1(h) = MAX(cn1(h), .4 * cnn)
cn3(h) = cnn * EXP(.006729 * c2)

!! calculate maximum retention parameter value
smx(h) = 254. * (100. / cn1(h) - 1.)

!! calculate retention parameter value for CN3
s3 = 254. * (100. / cn3(h) - 1.)

!! calculate fraction difference in retention parameters
rto3 = 0.
rtos = 0.
rto3 = 1. - s3 / smx(h)
rtos = 1. - 2.54 / smx(h)
!! calculate shape parameters
CALL ascrv(rto3,rtos,sol_sumfc(h),sol_sumul(h),wrt(1,h),wrt(2,h))

IF (curyr == 0) THEN
  sci(h) = 0.9 * smx(h)
ELSE
  sci(h) = (1. - ((smxold - sci(h)) / smxold)) * smx(h)      !! plant ET
END IF

RETURN
END SUBROUTINE curno
