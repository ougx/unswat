SUBROUTINE readru
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the sub input file (.sub).
!!    This file contains data related to routing .

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definitionov
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    da_ru       |ha            |area of routing unit
!!    ovsl        |(m)           |average slope length
!!    ovs_ru      |(m)           |average slope steepness
!!    ovn_ru      |              |Manning's N value overland flow
!!    chl_ru      |(km)          |channel length
!!    chs_ru      |(m/m)         |ave slope
!!    chw_ru      |(mm/km)       |ave width
!!    chk_ru      |(mm/hr)       |eff hydr cond
!!    chn_ru      |              |Manning's N tributary channels
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof

eof = 0
DO
  READ (113,5000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
!       read (113,*,iostat=eof) tck
!       if (eof < 0) exit
  READ (113,*,IOSTAT=eof) da_ru
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) ovsl
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) ovs
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) ovn_ru
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) chl_ru
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) chs_ru
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) chw_ru
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) chk_ru
  IF (eof < 0) EXIT
  READ (113,*,IOSTAT=eof) chn_ru
  IF (eof < 0) EXIT
  EXIT
END DO

IF (ovsl < 1.e-6) ovsl = 50.

DO j = 1, hrutot(i)
  READ (113,*) ix, hru_rufr(iru,j)
END DO

!! compute weighted K factor for sediment transport capacity
sumk = 0.
DO j = 1, hrutot(i)
  sumk = sumk + usle_k(j) * hru_rufr(iru,j)
END DO
ru_k(isub,iru) = sumk
ru_ovsl(isub,iru) = ovsl
ru_ovs(isub,iru) = ovs
ru_ktc(isub,iru) = tck
!daru_km(isub,iru) = da_ru

5000  FORMAT (a)
RETURN
END SUBROUTINE readru
