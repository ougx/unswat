SUBROUTINE addh
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59
 
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine adds loadings from two sources for routing

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhvaroute(:,:,:)|varies    |routing storage array for hourly time step
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihout       |none          |outflow hydrograph storage location number
!!    inum1       |none          |hydrograph storage location number of
!!                               |first dataset to be added
!!    inum2       |none          |hydrograph storage location number of
!!                               |second dataset to be added
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    varoute(:,:)|varies        |routing storage array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhvaroute(:,:,:)|varies        |routing storage array for hourly time step
!!    varoute(:,:)    |varies        |routing storage array for daily time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name       |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii         |none          |counter
!!    kk         |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: ii, kk

!! zero flow out variables
DO ii = 1, mvaro
  varoute(ii,ihout) = 0.
END DO

!! add loadings and store in new hydrograph location
IF (varoute(2,inum1) + varoute(2,inum2) > 0.1) THEN
  varoute(1,ihout) = (varoute(1,inum1) * varoute(2,inum1) +  &
      varoute(1,inum2) * varoute(2,inum2)) / (varoute(2,inum1) + varoute(2,inum2))
  varoute(18,ihout) = (varoute(18,inum1) * varoute(2,inum1) +  &
      varoute(18,inum2) * varoute(2,inum2)) /  &
      (varoute(2,inum1) + varoute(2,inum2))
  varoute(19,ihout) = (varoute(19,inum1) * varoute(2,inum1) +  &
      varoute(19,inum2) * varoute(2,inum2)) /  &
      (varoute(2,inum1) + varoute(2,inum2))
END IF
DO ii = 2, 17
  varoute(ii,ihout) = varoute(ii,inum1) + varoute(ii,inum2)
END DO
DO ii = 20, mvaro
  varoute(ii,ihout) = varoute(ii,inum1) + varoute(ii,inum2)
END DO
IF (varoute(2,ihout) < 0.) varoute(2,ihout) = 0.


!! add hydrograph points (hourly time step)
IF (ievent > 2) THEN
!     do kk = 1, 24
  DO kk = 1, nstep  ! modified for urban modeling by J.Jeong 4/15/2008
    IF (hhvaroute(2,inum1,kk) + hhvaroute(2,inum2,kk) > 0.1) THEN
      hhvaroute(1,ihout,kk) = (hhvaroute(1,inum1,kk) *  &
          hhvaroute(2,inum1,kk) + hhvaroute(1,inum2,kk) *  &
          hhvaroute(2,inum2,kk)) / (hhvaroute(2,inum1,kk) + hhvaroute(2,inum2,kk))
    END IF
  END DO
  DO ii = 2, mvaro
!       do kk = 1, 24
    DO kk = 1, nstep  ! modified for urban modeling by J.Jeong 4/15/2008
      hhvaroute(ii,ihout,kk) = hhvaroute(ii,inum1,kk) + hhvaroute(ii,inum2,kk)
    END DO
  END DO
END IF

DO ii = 29, mvaro
  varoute(ii,inum1) = 0.
  varoute(ii,inum2) = 0.
END DO

RETURN
END SUBROUTINE addh
