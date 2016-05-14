SUBROUTINE readwus
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the HRU/subbasin water use input file
!!    (.wus). The water use file extracts water from the subbasin and it is
!!    considered to be lost from the watershed. These variables should be used
!!    to remove water transported outside the watershed.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wudeep(:,:) |10^4 m^3/day  |average daily water removal from the deep
!!                               |aquifer for the month
!!    wupnd(:,:)  |10^4 m^3/day  |average daily water removal from the pond
!!                               |for the month
!!    wurch(:,:)  |10^4 m^3/day  |average daily water removal from the reach
!!                               |for the month
!!    wushal(:,:) |10^4 m^3/day  |average daily water removal from the shallow
!!                               |aquifer for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    mon         |none          |counter
!!    titldum     |NA            |title line of .wus file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof, mon, j
REAL :: swupnd(12), swush(12), swudp(12)

eof = 0
swupnd = 0.
swush = 0.
swudp = 0.

DO
  READ (105,5300,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (105,5300,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (105,5300,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (swupnd(mon),mon = 1,6)
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (swupnd(mon),mon = 7,12)
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (wurch(mon,i),mon = 1,6)
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (wurch(mon,i),mon = 7,12)
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (swush(mon),mon = 1,6)
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (swush(mon),mon = 7,12)
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (swudp(mon),mon = 1,6)
  IF (eof < 0) EXIT
  READ (105,5100,IOSTAT=eof) (swudp(mon),mon = 7,12)
  EXIT
END DO

DO j = 1, hrutot(i)
  ihru = 0
  ihru = nhru + j
  DO mon = 1, 12
    wupnd(mon,ihru) = swupnd(mon)
    wushal(mon,ihru) = swush(mon)
    wudeep(mon,ihru) = swudp(mon)
  END DO
END DO

CLOSE (105)

RETURN
5100 FORMAT (6F10.1)
5300 FORMAT (a80)
END SUBROUTINE readwus
