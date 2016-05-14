SUBROUTINE readatmodep
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the atmospheric deposition values
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drydep_no3  |kg/ha/yr      |atmospheric dry deposition of nitrates
!!    drydep_nh4  |kg/ha/yr      |atmospheric dry deposition of ammonia
!!    rammo_sub   |mg/l          |atmospheric deposition of ammonium values for
!!                                 entire watershed
!!    rcn_sub     |mg/l          |atmospheric deposition of nitrate for
!!                                 entire watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
CHARACTER (LEN=80) :: titldum
INTEGER :: eof

eof = 0

rcn_sub = rcn_sub_bsn

!!    Atmosperic deposition filename present in file.cio
IF (atmofile /= '             ') THEN
  OPEN (127,FILE=atmofile)
  DO iii = 1, 5
    READ (127,5101) titldum
  END DO
  IF (iatmodep == 0) THEN
    DO isub = 1, subtot
      READ (127,1000,IOSTAT=eof) rammo_sub(isub), rcn_sub(isub),  &
          drydep_nh4(isub), drydep_no3(isub)
      IF (eof < 0) EXIT
    END DO
  ELSE
    READ (127,1001,IOSTAT=eof) mo_atmo1, iyr_atmo1
    1001        FORMAT (2I6)
    iii = 0
    momax = 12 * nbyr
    DO iii = 1, msub
      READ (127,1002) (rammo_mo(imo,iii),imo = 1,momax)
      READ (127,1002) (rcn_mo(imo,iii), imo = 1,momax)
      READ (127,1002) (drydep_nh4_mo(imo,iii), imo = 1, momax)
      READ (127,1002) (drydep_no3_mo(imo,iii), imo = 1,momax)
      1002          FORMAT (600F10.3)
    END DO
  END IF
ELSE
!!    no filename present in file.cio - set defaults
  rammo_sub = 0.
  rcn_sub = rcn_sub_bsn
END IF


1000  FORMAT (8X,4F10.3)
5101  FORMAT (a80)
RETURN
END SUBROUTINE readatmodep
