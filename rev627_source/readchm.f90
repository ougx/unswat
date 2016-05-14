SUBROUTINE readchm
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads data from the HRU/subbasin soil chemical input
!!    file (.chm). This file contains initial amounts of pesticides/nutrients
!!    in the first soil layer. (Specifics about the first soil layer are given
!!    in the .sol file.) All data in the .chm file is optional input.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    mlyr        |none          |maximum number of soil layers
!!    mpst        |none          |maximum number of pesticides used in
!!                               |watershed
!!    nope(:)     |none          |sequence number of pesticide in NPNO(:)
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    npno(:)     |none          |array of unique pesticides used in
!!                               |watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    nope(:)       |none          |sequence number of pesticide in NPNO(:)
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in
!!                                 |watershed
!!    plt_pst(:,:)  |kg/ha         |pesticide on plant foliage
!!    sol_pst(:,:,1)|mg/kg         |pesticide concentration in soil
!!    pst_enr(:,:)  |none          |pesticide enrichment ratio
!!    sol_no3(:,:)  |mg N/kg       |concentration of nitrate in soil layer
!!    sol_orgn(1,:) |mg N/kg soil  |organic N concentration in top soil layer
!!    sol_orgp(1,:) |mg P/kg soil  |organic P concentration in top soil layer
!!    sol_solp(1,:) |mg P/kg soil  |soluble P concentration in top soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    j           |none          |counter
!!    k           |none          |counter
!!    newpest     |none          |new pesticide flag
!!    pltpst      |kg/ha         |pesticide on plant foliage
!!    pstenr      |none          |pesticide enrichment ratio
!!    pstnum      |none          |pesticide number
!!    solpst      |mg/kg         |pesticide concentration in soil
!!    titldum     |NA            |title line for .chm file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
INTEGER :: j, eof, k, newpest, pstnum
REAL :: pltpst, solpst, pstenr

eof = 0


DO
  READ (106,5000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,5000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,5000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,5100,IOSTAT=eof) (sol_no3(j,ihru), j = 1, mlyr)
  IF (eof < 0) EXIT
  READ (106,5100,IOSTAT=eof) (sol_orgn(j,ihru), j = 1, mlyr)
  IF (eof < 0) EXIT
  READ (106,5100,IOSTAT=eof) (sol_solp(j,ihru), j = 1, mlyr)
  IF (eof < 0) EXIT
  READ (106,5100,IOSTAT=eof) (sol_orgp(j,ihru), j = 1, mlyr)
  IF (eof < 0) EXIT
  READ (106,5100,IOSTAT=eof) (pperco_sub(j,ihru), j = 1, mlyr)
  IF (eof < 0) EXIT
  READ (106,5000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,5000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (106,5000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
!! end do
  
  DO j = 1, mpst
    pstnum = 0
    pltpst = 0.
    solpst = 0.
    pstenr = 0.
    READ (106,*,IOSTAT=eof) pstnum, pltpst, solpst, pstenr
    IF (pstnum > 0) THEN
      hrupest(ihru) = 1
      newpest = 0
      DO k = 1, npmx
        IF (pstnum == npno(k)) THEN
          newpest = 1
          EXIT
        END IF
      END DO
      IF (newpest == 0) THEN
        npno(npmx) = pstnum
        nope(pstnum) = npmx
        npmx = npmx + 1
      END IF
      
      k = 0
      k = nope(pstnum)
      plt_pst(k,ihru) = pltpst
      sol_pst(k,ihru,1) = solpst
      pst_enr(k,ihru) = pstenr
    END IF
    
    IF (eof < 0) EXIT
  END DO
  EXIT
  
  
END DO

CLOSE (106)

DO j = 1, mlyr
  IF (pperco_sub(j,ihru) <= 1.e-6) pperco_sub(j,ihru) = pperco
END DO

RETURN
5000 FORMAT (a)
5100 FORMAT (27X,10F12.2)
END SUBROUTINE readchm
