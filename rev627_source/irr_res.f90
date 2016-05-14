SUBROUTINE irr_res
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the irrigation operation when the water
!!    source is a reservoir

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)         |mm H2O        |amount of water applied to HRU on current
!!                                   |day
!!    auto_wstr(:)    |none or mm    |water stress factor which triggers auto
!!                                   |irrigation
!!    iida            |julian date   |day being simulated (current julian date)
!!    wstrs_id(:)     |none          |water stress identifier:
!!                                   |1 plant water demand
!!                                   |2 soil water deficit
!!    flag                           |1 = manual 2 = auto
!!    inum1           |none          |reservoir number
!!    ipot(:)         |none          |number of HRU (in subbasin) that is ponding
!!                                   |water--the HRU that the surface runoff from
!!                                   |current HRU drains into. This variable is
!!                                   |used only for rice paddys or closed
!!                                   |depressional areas
!!    irramt(:)       |mm H2O        |depth of irrigation water applied to
!!                                   |HRU
!!    irrno(:)        |none          |irrigation source location
!!                                   |if IRR=1, IRRNO is the number of the
!!                                   |          reach
!!                                   |if IRR=2, IRRNO is the number of the
!!                                   |          reservoir
!!                                   |if IRR=3, IRRNO is the number of the
!!                                   |          subbasin
!!                                   |if IRR=4, IRRNO is the number of the
!!                                   |          subbasin
!!                                   |if IRR=5, not used
!!    irrsc(:)        |none          |irrigation source code:
!!                                   |1 divert water from reach
!!                                   |2 divert water from reservoir
!!                                   |3 divert water from shallow aquifer
!!                                   |4 divert water from deep aquifer
!!                                   |5 divert water from source outside
!!                                   |  watershed
!!    nair(:)         |none          |sequence number of auto-irrigation
!!                                   |application within the year
!!    nhru            |none          |number of HRUs in watershed
!!    nirr(:)         |none          |sequence number of irrigation application
!!                                   |within the year
!!    nro(:)          |none          |sequence number of year in rotation
!!    phuacc(:)       |none          |fraction of plant heat units accumulated
!!    pot_vol(:)      |m**3 H2O      |current volume of water stored in the
!!                                   |depression/impounded area
!!    res_vol(:)      |m**3          |reservoir volume
!!    sol_sumfc(:)    |mm H2O        |amount of water held in the soil profile
!!                                   |at field capacity
!!    sol_sw(:)       |mm H2O        |amount of water stored in soil profile on any
!!                                   |given day
!!    strsw(:)        |none          |fraction of potential plant growth achieved
!!                                   |on the day where the reduction is caused by
!!                                   |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nirr(:)     |none          |sequence number of irrigation application
!!                               |within the year
!!    pot_vol(:)  |m**3 H2O      |current volume of water stored in the
!!                               |depression/impounded area
!!    res_vol(:)  |m**3          |reservoir volume
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm => m^3)
!!    flag        |none          |irrigation flag:
!!                               |0 no irrigation operation on current day
!!                               |1 scheduled irrigation
!!                               |2 auto irrigation
!!    jres        |none          |reservoir number
!!    k           |none          |HRU number
!!    vmm         |mm H2O        |depth of irrigation water over HRU
!!    vmxi        |mm H2O        |amount of water specified in irrigation
!!                               |operation
!!    vol         |m^3 H2O       |volume of water applied in irrigation
!!                               |operation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs, Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: jres, k, flag
REAL :: cnv, vmm, vol, vmxi

jres = 0
jres = inum1

DO k = 1, nhru
  IF (irrsc(k) == 2 .AND. irrno(k) == jres) THEN
    
!! check for timing of irrigation operation
    flag = irr_flag(k)
    IF (auto_wstr(k) > 0.) THEN
      IF (wstrs_id(k) == 1 .AND. strsw(k) < auto_wstr(k)) flag = 2
      IF (wstrs_id(k) == 2 .AND. sol_sumfc(k) - sol_sw(k) >  &
          auto_wstr(k)) flag = 2
    END IF
    
    IF (flag == 1) THEN
      sq_rto = irrsq(k)
      irrsc(k) = irr_sc(k)
      irrno(k) = irr_no(k)
    ELSE
      sq_rto = irr_asq(k)
      irrsc(k) = irr_sca(k)
      irrno(k) = irr_noa(k)
    END IF
    
    IF (flag > 0) THEN
      cnv = 0.
      cnv = hru_ha(k) * 10.
      
!! compute maximum amount of water available for irrigation
!! from reach
      vmm = 0.
      vmm = res_vol(jres) / cnv
      
!! check available against set amount in scheduled operation
      IF (flag == 1) THEN
        vmxi = 0.
        vmxi = irramt(k)
        IF (vmxi < 1.e-6) vmxi = sol_sumfc(k)
        IF (vmm > vmxi) vmm = vmxi
      END IF
      IF (flag == 2) THEN
        vmxi = 0.
        vmxi = irr_mx(k)
        IF (vmm > vmxi) vmm = vmxi
      END IF
      
      IF (vmm > 0.) THEN
        vol = 0.
        vol = vmm * cnv
        
!!   if (ipot(k) == k) then
        IF (pot_fr(k) > 1.e-6) THEN
          pot_vol(k) = pot_vol(k) + vol
        ELSE
          CALL irrigate(k,vmm)
        END IF
        
        irramt(k) = vmm
        IF (imgt == 1) THEN
          WRITE (143, 1000) subnum(k), hruno(k), iyr, i_mo, iida,  &
              "         ",  " AUTOIRR", phubase(k), phuacc(k),  &
              sol_sw(k), bio_ms(k), sol_rsd(1,k),sol_sumno3(k),  &
              sol_sumsolp(k), aird(k), irrsc(k), irrno(k)
          1000  FORMAT (a5,1X,a4,3I6,2A15,7F10.2,10X,f10.2,70X,i10,10X,i10)
        END IF
!! subtract irrigation from reservoir volume
!!     if (ipot(k) /= k) then
        IF (pot_fr(k) > 1.e-6) THEN
          vol = 0.
          vol = aird(k) * cnv
        END IF
        vol = vol / irr_eff(k)       !! BN inserted to account for irr. efficiency
        res_vol(jres) = res_vol(jres) - vol
        IF (res_vol(jres) < 0.) res_vol(jres) = 0.
        
!! advance irrigation operation number
        IF (flag == 1) THEN
          nirr(k) = nirr(k) + 1
        END IF
        
      END IF
    END IF
  END IF
END DO

RETURN
END SUBROUTINE irr_res
