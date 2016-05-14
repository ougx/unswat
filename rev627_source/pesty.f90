SUBROUTINE pesty(iwave)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates pesticide transported with suspended sediment

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    hrupest(:)    |none         |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru          |none         |HRU number
!!    iwave         |none         |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    npmx          |none         |number of different pesticides used in
!!                                |the simulation
!!    npno(:)       |none         |array of unique pesticides used in watershed
!!    pst_enr(:,:)  |none         |pesticide enrichment ratio
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion in
!!                                |HRU
!!    sol_kp(:,:,:)|(mg/kg)/(mg/L)|pesticide sorption coefficient, Kp; the
!!                                |ratio of the concentration in the solid
!!                                |phase to the concentration in solution
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer in HRU
!!    sub_pst(:,:)  |kg/ha        |amount of pesticide in layer in subbasin
!!    zdb(:,:)      |mm           |division term from net pesticide equation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pst_sed(:,:)  |kg/ha        |pesticide loading from HRU sorbed onto
!!                                |sediment
!!    sol_pst(:,:,:)|kg/ha        |amount of pesticide in layer in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conc        |              |concentration of pesticide in soil
!!    er          |none          |enrichment ratio for pesticides
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |none          |pesticide number from database
!!    xx          |kg/ha         |amount of pesticide in soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm


INTEGER, INTENT(INOUT)                   :: iwave

INTEGER :: j, k, kk
REAL :: xx, conc, er

j = 0
j = ihru

IF (hrupest(j) == 0) RETURN
DO k = 1, npmx
  kk = 0
  kk = npno(k)
  IF (kk > 0) THEN
    xx = 0.
    IF (iwave <= 0) THEN
      xx = sol_pst(k,j,1)
    ELSE
      xx = sub_pst(kk,iwave)
    END IF
    
    IF (xx >= .0001) THEN
      conc = 0.
      er = 0.
      conc = 100. * sol_kp(k,j,1) * xx / (zdb(k,j)+1.e-10)
      IF (pst_enr(k,j) > 0.) THEN
        er = pst_enr(k,j)
      ELSE
        er = enratio
      END IF
      
      pst_sed(k,j) = .001 * sedyld(j) * conc * er / hru_ha(j)
      IF (pst_sed(k,j) < 0.) pst_sed(k,j) = 0.
      IF (pst_sed(k,j) > xx) pst_sed(k,j) = xx
      sol_pst(k,j,1) = xx - pst_sed(k,j)
    END IF
  END IF
END DO

RETURN
END SUBROUTINE pesty
