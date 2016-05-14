SUBROUTINE readcnst
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the loading information for the reccnst command

!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     i            |none          |file number associated with reccnst
!!                                 |command
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     bactlpcnst(:)|# bact/day    |average daily less persistent bacteria
!!                                 |loading to reach
!!     bactpcnst(:) |# bact/day    |average daily persistent bacteria loading
!!                                 |to reach
!!     cbodcnst(:)  |kg/day        |average daily loading of CBOD to reach
!!     chlacnst(:)  |kg/day        |average daily loading of chlorophyll-a
!!     cmtl1cnst(:) |kg/day        |average daily conservative metal #1 loading
!!     cmtl2cnst(:) |kg/day        |average daily conservative metal #2 loading
!!     cmtl3cnst(:) |kg/day        |average daily conservative metal #3 loading
!!     disoxcnst(:) |kg/day        |average daily loading of dissolved O2
!!     flocnst(:)   |m^3 H2O/day   |average daily water loading to reach
!!     minpcnst(:)  |kg P/day      |average daily soluble P loading to reach
!!     nh3cnst(:)   |kg N/day      |average daily ammonia loading to reach
!!     no2cnst(:)   |kg N/day      |average daily nitrite loading to reach
!!     no3cnst(:)   |kg N/day      |average daily nitrate loading to reach
!!     orgncnst(:)  |kg N/day      |average daily organic N loading to reach
!!     orgpcnst(:)  |kg P/day      |average daily organic P loading to reach
!!     sedcnst(:)   |metric tons/d |average daily sediment loading for reach
!!     solpstcnst(:)|mg/day        |average daily soluble pesticide loading
!!     srbpstcnst(:)|mg/day        |average daily sorbed pesticide loading
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     ii           |none          |counter
!!     titldum      |NA            |description line
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: ii


DO ii = 1, 6
  READ (109,5000) titldum
END DO
READ (109,*) flocnst(i), sedcnst(i), orgncnst(i), orgpcnst(i),  &
    no3cnst(i), nh3cnst(i), no2cnst(i), minpcnst(i),  &
    cbodcnst(i), disoxcnst(i), chlacnst(i),  &
    solpstcnst(i), srbpstcnst(i), bactpcnst(i),  &
    bactlpcnst(i), cmtl1cnst(i), cmtl2cnst(i), cmtl3cnst(i)

CLOSE (109)

RETURN
5000 FORMAT (a80)
END SUBROUTINE readcnst
