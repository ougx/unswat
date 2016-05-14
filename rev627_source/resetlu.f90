SUBROUTINE resetlu
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin management input file
!!    (.mgt). This file contains data related to management practices used in
!!    the HRU/subbasin.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                 |urban.dat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                    |daily
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Jdt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
CHARACTER(LEN=80) :: titldum

OPEN (9123,FILE=fname(no_lup))
READ (9123, 5101) titldum
DO j = 1, mhru
  READ (9123,*,END=99) hru, hru_fr(j)
END DO

!!    reset all hru_fr variables
DO j = 1, mhru
  IF (hru_fr(j) <= 0.) hru_fr(j) = .0000001
  hru_km(j) = sub_km(hru_sub(j)) * hru_fr(j)
  hru_ha(j) = hru_km(j) * 100.  !MJW
  hru_dafr(j) = hru_km(j) / da_km  !MJW
  DO mon = 1, 12
    wupnd(mon,j) = wupnd(mon,j) * hru_fr(j)
    wushal(mon,j) = wushal(mon,j) * hru_fr(j)
    wudeep(mon,j) = wudeep(mon,j) * hru_fr(j)
  END DO
  pnd_psa(j) = pnd_psa(j) * hru_fr(j)
  pnd_esa(j) = pnd_esa(j) * hru_fr(j)
  pnd_pvol(j) = pnd_pvol(j) * hru_fr(j)
  pnd_evol(j) = pnd_evol(j) * hru_fr(j)
  pnd_vol(j) = pnd_vol(j) * hru_fr(j)
  wet_nsa(j) = wet_nsa(j) * hru_fr(j)
  wet_mxsa(j) = wet_mxsa(j) * hru_fr(j)
  wet_nvol(j) = wet_nvol(j) * hru_fr(j)
  wet_mxvol(j) = wet_mxvol(j) * hru_fr(j)
  wet_vol(j) = wet_vol(j) * hru_fr(j)
  hru_ha(j) = hru_km(j) * 100.
!   pot_vol(j) = 10. * pot_volmm(j) * hru_ha(j)   !! mm => m^3     NUBZ
  pot_volx(j) = pot_volxmm(j)
  pot_tile(j) = pot_tilemm(j)
END DO

5101  FORMAT (a80)
99    RETURN
END SUBROUTINE resetlu
