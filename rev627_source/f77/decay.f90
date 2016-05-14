SUBROUTINE decay
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates degradation of pesticide in the soil and on
!!    the plants

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    decay_f(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide on foliage
!!    decay_s(:)    |none          |exponential of the rate constant for
!!                                 |degradation of the pesticide in soil
!!    hru_dafr(:)   |none          |fraction of watershed area in HRU
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    ihru          |none          |HRU number
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_nly(:)    |none          |number of layers in soil profile
!!    sol_pst(:,:,:)|kg/ha         |pesticide in soil layer
!!    wshd_pstdg(:) |kg pst/ha     |amount of pesticide lost through degradation
!!                                 |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,:)|kg/ha         |pesticide in soil layer
!!    wshd_pstdg(:) |kg pst/ha     |amount of pesticide lost through degradation
!!                                 |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from pest.dat
!!    l           |none          |counter (soil layers)
!!    x1          |kg/ha         |amount of pesticide present at beginning of
!!                               |day
!!    xx          |kg/ha         |amount of pesticide present at end of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, k, kk, l
REAL :: x1, xx

j = 0
j = ihru

IF (hrupest(j) == 0) RETURN

DO k = 1, npmx
  kk = 0
  kk = npno(k)
  IF (kk > 0) THEN
    
!! calculate degradation in soil
    DO l = 1, sol_nly(j)
      x1 = 0.
      x1 = sol_pst(k,j,l)
      IF (x1 >= 0.0001) THEN
        xx = 0.
        xx = x1 * decay_s(kk)
        wshd_pstdg(k) = wshd_pstdg(k) + (x1 - xx) * hru_dafr(j)
        sol_pst(k,j,l) = xx
      END IF
    END DO
    
!! calculate degradation off plant foliage
    x1 = 0.
    x1 = plt_pst(k,j)
    IF (x1 >= 0.0001) THEN
      xx = 0.
      xx = x1 * decay_f(kk)
      wshd_pstdg(k) = wshd_pstdg(k) + (x1 - xx) * hru_dafr(j)
      plt_pst(k,j) = xx
    END IF
  END IF
END DO

RETURN
END SUBROUTINE decay
