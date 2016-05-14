SUBROUTINE autoirr
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:55:59

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the auto-irrigation operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)        |mm H2O        |amount of water applied to HRU on current
!!                                  |day
!!    auto_wstr(:)   |none or mm    |water stress threshold that triggers irrigation
!!    deepst(:)      |mm H2O        |depth of water in deep aquifer
!!    hru_sub(:)     |none          |subbasin in which HRU is located
!!    wstrs_id(:)    |none          |water stress identifier:
!!                                  |1 plant water demand
!!                                  |2 soil water deficit
!!    ihru           |none          |HRU number
!!    irrno(:)       |none          |irrigation source location
!!                                  |if IRR=1, IRRNO is the number of the
!!                                  |          reach
!!                                  |if IRR=2, IRRNO is the number of the
!!                                  |          reservoir
!!                                  |if IRR=3, IRRNO is the number of the
!!                                  |          subbasin
!!                                  |if IRR=4, IRRNO is the number of the
!!                                  |          subbasin
!!                                  |if IRR=5, not used
!!    irrsc(:)       |none          |irrigation source code:
!!                                  |1 divert water from reach
!!                                  |2 divert water from reservoir
!!                                  |3 divert water from shallow aquifer
!!                                  |4 divert water from deep aquifer
!!                                  |5 divert water from source outside
!!                                  |  watershed
!!    nhru           |none          |number of HRUs in watershed
!!    shallst(:)     |mm H2O        |depth of water in shallow aquifer
!!    sol_sumfc(:)   |mm H2O        |amount of water held in the soil profile
!!                                  |at field capacity
!!    sol_sw(:)      |mm H2O        |amount of water stored in soil profile on any
!!                                  |given day
!!    strsw(:)       |none          |fraction of potential plant growth achieved
!!                                  |on the day where the reduction is caused by
!!                                  |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepirr(:)  |mm H2O        |amount of water removed from deep aquifer
!!                               |for irrigation
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    shallirr(:) |mm H2O        |amount of water removed from shallow aquifer
!!                               |for irrigation
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    vmm         |mm H2O        |maximum amount of water to be applied
!!    vmma        |mm H2O        |amount of water in source
!!    vmmd        |m^3 H2O       |total amount of water in subbasin deep
!!                               |aquifer
!!    vmms        |m^3 H2O       |total amount of water in subbasin shallow
!!                               |aquifer
!!    vol         |mm H2O        |volume of water applied to HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, k
REAL :: vmma, vmm, cnv, vol, vmms, vmmd

j = 0
j = ihru

!!!! Srin's irrigation source by each application changes
irrsc(j) = irr_sca(j)
irrno(j) = irr_noa(j)
!!!! Srin's irrigation source by each application changes

IF ((wstrs_id(j) == 1 .AND. strsw(j) < auto_wstr(j) .OR.  &
      (wstrs_id(j)==2.AND.sol_sumfc(j)-sol_sw(j)>auto_wstr(j)))) THEN
!! determine available amount of water in source
!! ie upper limit on water removal on day
  vmma = 0.
  vmms = 0.
  vmmd = 0.
  vmm = 0.
  select case (irrsc(j))
  case (3)   !! shallow aquifer source
  DO k = 1, nhru
    IF (hru_sub(k) == irrno(j)) THEN
      cnv = 0.
      cnv = hru_ha(k) * 10.
      vmma = vmma + shallst(k) * cnv
    END IF
  END DO
  vmms = vmma
  cnv = 0.
  cnv = hru_ha(j) * 10.
  vmma = vmma / cnv
  vmm = MIN(sol_sumfc(j), vmma)
  
  case (4)   !! deep aquifer source
  DO k = 1, nhru
    IF (hru_sub(k) == irrno(j)) THEN
      cnv = 0.
      cnv = hru_ha(k) * 10.
      vmma = vmma + deepst(k) * cnv
    END IF
  END DO
  vmmd = vmma
  cnv = 0.
  cnv = hru_ha(j) * 10.
  vmma = vmma / cnv
  vmm = MIN(sol_sumfc(j), vmma)
  
  case (5)   !! unlimited source
  vmm = sol_sumfc(j)
END select


!! if water available from source, proceed with irrigation
IF (vmm > 0.) THEN
  vmm = MIN(vmm,irr_mx(j))
  sq_rto = irr_asq(j)
  CALL irrigate(j,vmm)
  
!! subtract irrigation from shallow or deep aquifer
  vol = 0.
  cnv = 0.
  cnv = hru_ha(j) * 10.
  vol = aird(j) * cnv
  select case (irrsc(j))
  case (3)   !! shallow aquifer source
  DO k = 1, nhru
    IF (hru_sub(k) == irrno(j)) THEN
      cnv = 0.
      vmma = 0.
      cnv = hru_ha(k) * 10.
      IF (vmms > 1.e-4) THEN
        vmma = vol * (shallst(k) * cnv / vmms)
      END IF
      vmma = vmma / cnv
      vmma = vmma / irr_eff(k)
      shallst(k) = shallst(k) - vmma
      IF (shallst(k) < 0.) THEN
        vmma = vmma + shallst(k)
        shallst(k) = 0.
      END IF
      shallirr(k) = shallirr(k) + vmma
    END IF
  END DO
  
  case (4)   !! deep aquifer source
  DO k = 1, nhru
    IF (hru_sub(k) == irrno(j)) THEN
      cnv = 0.
      vmma = 0.
      cnv = hru_ha(k) * 10.
      IF (vmmd>1.e-4) vmma = vol * (deepst(k) * cnv / vmmd)
      vmma = vmma / cnv
      vmma = vmma / irr_eff(k)
      deepst(k) = deepst(k) - vmma
      IF (deepst(k) < 0.) THEN
        vmma = vmma + deepst(k)
        deepst(k) = 0.
      END IF
      deepirr(k) = deepirr(k) + vmma
    END IF
  END DO
END select

IF (imgt == 1) THEN
  WRITE (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida,  &
      "         ",  " AUTOIRR", phubase(j), phuacc(j)  &
      , sol_sw(j),bio_ms(j), sol_rsd(1,j),sol_sumno3(j)  &
      ,sol_sumsolp(j), aird(j) ,irrsc(j), irrno(j)
END IF

END IF
END IF

1000  FORMAT (a5,1X,a4,3I6,2A15,7F10.2,10X,f10.2,70X,i10,10X,i10)

RETURN
END SUBROUTINE autoirr
