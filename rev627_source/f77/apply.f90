SUBROUTINE apply
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies pesticide

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_ef(:)     |none             |application efficiency (0-1)
!!    curyr        |none             |current year of simulation
!!    drift(:)     |kg               |amount of pesticide drifting onto main
!!                                   |channel in subbasin
!!    driftco(:)   |none             |coefficient for pesticide drift directly
!!                                   |onto stream
!!    hru_dafr(:)  |km**2/km**2      |fraction of watershed area in HRU
!!    hru_km(:)    |km**2            |area of HRU in square kilometers
!!    ihru         |none             |HRU number
!!    ipest        |none             |pesticide identification number from
!!                                   |pest.dat
!!    irtpest      |none             |the sequence number of the pesticide type
!!                                   |in NPNO(:) which is to be routed through
!!                                   |the watershed
!!    laiday(:)    |none             |leaf area index
!!    nope(:)      |none             |sequence number of pesticide in NPNO(:)
!!    nro(:)       |none             |sequence number of year in rotation
!!    nyskip       |none             |number of years to skip output
!!                                   |summarization/printing
!!    plt_pst(:,:) |kg/ha            |pesticide on plant foliage
!!    pst_dep      |kg/ha          |depth of pesticide in soil
!!    pst_kg       |kg/ha            |amount of pesticide applied to HRU
!!    sol_pst(:,:,1)|kg/ha           |pesticide in first layer of soil
!!    wshd_pstap(:)|kg/ha            |total amount of pesticide type applied in
!!                                   |watershed during simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drift(:)    |kg            |amount of pesticide drifting onto main
!!                               |channel in subbasin
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    wshd_pstap(:)|kg/ha         |total amount of pesticide type applied in
!!                               |watershed during simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gc          |none          |fraction of ground covered by plant foliage
!!    j           |none          |HRU number
!!    jj          |none          |subbasin number
!!    k           |none          |sequence number of pesticide in NPNO(:)
!!    kk          |none          |pesticide identification number from
!!                               |pest.dat
!!    xx          |kg/ha         |amount of pesticide applied to HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, kk, k, jj
REAL :: xx, gc

j = 0
j = ihru

!! initialize local variables
kk = 0
k = 0
jj = 0
xx = 0.

kk = ipest

k = nope(kk)

xx = pst_kg

jj = inum1

!! calculate amount of pesticide drifting onto main channel in subbasin
!      if (k == irtpest) then
!        drift(jj) = drift(jj) + xx * hru_km(j) * 100. * driftco(j) *    &
!     *                                                              1.e6
!      end if
!      xx = xx * ap_ef(kk) * (1. - driftco(j))
xx = xx * ap_ef(kk)

!   added for pesticide incorporation 3/31/08 gsm
IF (pst_dep > 1.e-6) THEN
  DO nly = 1,sol_nly(j)
    IF (nly == 1) THEN
      IF (pst_dep < sol_z(nly,j)) THEN
        sol_pst(k,j,1) = sol_pst(k,j,1) + xx
        EXIT
      END IF
    ELSE
      IF (pst_dep > sol_z((nly-1),j) .AND. pst_dep < sol_z(nly,j)) THEN
        sol_pst(k,j,nly) = sol_pst(k,j,nly) + xx
        EXIT
      END IF
    END IF
  END DO
ELSE
!   added above for pesticide incorporation 3/31/08 gsm
  
!! calculate ground cover
  gc = 0.
  gc = (1.99532 - erfc(1.333 * laiday(j) - 2.)) / 2.1
  IF (gc < 0.) gc = 0.
  
!! update pesticide levels on ground and foliage
  plt_pst(k,j) = plt_pst(k,j) + gc * xx
  sol_pst(k,j,1) = sol_pst(k,j,1) + (1. - gc) * xx
!! added endif for pesticide incorporation 3/31/08 gsm
END IF

!! summary calculations
IF (curyr > nyskip) THEN
  wshd_pstap(k) = wshd_pstap(k) + pst_kg                    *  &
      ap_ef(kk) * hru_dafr(j)
END IF

RETURN
END SUBROUTINE apply
