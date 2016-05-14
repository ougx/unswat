SUBROUTINE readsno
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads snow data from the HRU/subbasin soil chemical input

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!                               |watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    sub_sftmp    |deg C         |Snowfall temperature
!!                                |Mean air temperature at which precipitation
!!                                |is equally likely to be rain as snow/freezing
!!                                |rain.
!!    sub_smfmn    |mm/deg C/day  |Minimum melt rate for snow during year (Dec.
!!                                |21) where deg C refers to the air temperature
!!    sub_smfmx    |mm/deg C/day  |Maximum melt rate for snow during year (June
!!                                |21) where deg C refers to the air temperature
!!                                |SMFMX and SMFMN allow the rate of snow melt
!!                                |to vary through the year. These parameters
!!                                |are accounting for the impact of soil
!!                                |temperature on snow melt.
!!    sub_smtmp    |deg C         |Snow melt base temperature
!!                                |Mean air temperature at which snow melt will
!!                                |occur.
!!    sub_timp     |none          |snow pack temperature lag factor (0-1)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: eof
eof = 0

DO
  READ (113,1001) titldum
  IF (eof < 0) EXIT
  READ (113,1000,IOSTAT=eof) (sub_sftmp(ib,i), ib = 1, 10)
  IF (eof < 0) EXIT
  READ (113,1000,IOSTAT=eof) (sub_smtmp(ib,i), ib = 1, 10)
  IF (eof < 0) EXIT
  READ (113,1000,IOSTAT=eof) (sub_smfmx(ib,i), ib = 1, 10)
  IF (eof < 0) EXIT
  READ (113,1000,IOSTAT=eof) (sub_smfmn(ib,i), ib = 1, 10)
  IF (eof < 0) EXIT
  READ (113,1000,IOSTAT=eof) (sub_timp(ib,i), ib = 1, 10)
  IF (eof < 0) EXIT
  EXIT
END DO

DO ib = 1, 10
  IF (sub_sftmp(ib,i) <= 0.) sub_sftmp(ib,i) = sftmp
  IF (sub_smtmp(ib,i) <= 0.) sub_smtmp(ib,i) = smtmp
  IF (sub_smfmx(ib,i) <= 0.) sub_smfmx(ib,i) = smfmx
  IF (sub_smfmn(ib,i) <= 0.) sub_smfmn(ib,i) = smfmn
  IF (sub_timp(ib,i) <= 0.) sub_timp(ib,i) = timp
END DO

CLOSE (113)

RETURN
1000 FORMAT (10F8.3)
1001 FORMAT (a)
END SUBROUTINE readsno
