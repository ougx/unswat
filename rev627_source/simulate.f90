SUBROUTINE simulate
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine contains the loops governing the modeling of processes
!!    in the watershed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    biomix(:)   |none          |biological mixing efficiency.
!!                               |Mixing of soil due to activity of earthworms
!!                               |and other soil biota. Mixing is performed at
!!                               |the end of every calendar year.
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    idaf        |julian date   |beginning day of simulation
!!    idal        |julian date   |ending day of simulation
!!    idplt(:)    |none          |land cover code from crop.dat
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    iyr         |none          |beginning year of simulation
!!    mcr         |none          |max number of crops grown per year
!!    nbyr        |none          |number of years in simulation
!!    ncrops(:,:,:)|
!!    nhru        |none          |number of HRUs in watershed
!!    nro(:)      |none          |sequence number of year in rotation
!!    nrot(:)     |none          |number of years of rotation
!!    nyskip      |none          |number of years to not print output
!!    phu_plt(:)  |heat units    |total number of heat units to bring plant
!!                               |to maturity
!!    sub_lat(:)  |degrees       |latitude of HRU/subbasin
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year in simulation (sequence)
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for current time during
!!                               |growing season
!!    i           |julian date   |current day in simulation--loop counter
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    id1         |julian date   |first day of simulation in current year
!!    iida        |julian date   |day being simulated (current julian day)
!!    idplt(:)    |none          |land cover code from crop.dat
!!    iyr         |year          |current year of simulation (eg 1980)
!!    laimxfr(:)  |
!!    leapyr      |none          |leap year flag:
!!                               |0  leap year
!!                               |1  regular year
!!    ncrops(:,:,:)|
!!    ncut(:)     |none          |sequence number of harvest operation within
!!                               |a year
!!    ndmo(:)     |days          |cumulative number of days accrued in the
!!                               |month since the simulation began where the
!!                               |array location number is the number of the
!!                               |month
!!    nro(:)      |none          |sequence number of year in rotation
!!    ntil(:)     |none          |sequence number of tillage operation within
!!                               |current year
!!    phu_plt(:)  |heat units    |total number of heat units to bring plant
!!                               |to maturity
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ic          |none          |counter
!!    idlst       |julian date   |last day of simulation in current year
!!    iix         |none          |sequence number of current year in rotation
!!    iiz         |none          |sequence number of current crop grown
!!                               |within the current year
!!    j           |none          |counter
!!    xx          |none          |current year in simulation sequence
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Mod, Real
!!    SWAT: sim_inityr, std3, xmon, sim_initday, clicon, command
!!    SWAT: writed, writem, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: idlst, j, iix, iiz, ic, mon, ii
REAL :: xx


DO curyr = 1, nbyr
  WRITE (*,1234) curyr
  
!! initialize annual variables
  CALL sim_inityr
  
!! write header for watershed annual table in .std file
  CALL std3
  
  
!!determine beginning and ending dates of simulation in current year
  IF (MOD(iyr,4) == 0) THEN
    leapyr = 0   !!leap year
    ndays = ndays_leap
  ELSE
    leapyr = 1   !!regular year
    ndays = ndays_noleap
  END IF
  
!! set beginning day of simulation for year
  id1 = 0
  IF (curyr == 1 .AND. idaf > 0) THEN
    id1 = idaf
  ELSE
    id1 = 1
  END IF
  
!! set ending day of simulation for year
  idlst = 0
  IF (curyr == nbyr .AND. idal > 0) THEN
    idlst = idal
  ELSE
    idlst = 366 - leapyr
  END IF
  
!! set current julian date to begin annual simulation
  iida = 0
  iida = id1
  
  CALL xmon
  IF (ifirstatmo == 1) THEN
    ifirstatmo = 0
    IF (iatmodep == 1) THEN
      iyr_at = iyr_atmo1
      mo_at = mo_atmo1
      DO
        mo_atmo = mo_atmo + 1
        IF (iyr_at == iyr .AND. mo_at == i_mo) EXIT
        mo_at = mo_at + 1
        IF (mo_at > 12) THEN
          mo_at = 1
          iyr_at = iyr_at + 1
        END IF
        IF (mo_atmo > 1000) EXIT
      END DO
    END IF
  END IF
  
  
  DO i = id1, idlst                            !! begin daily loop
    
!screen print days of the year for subdaily runs (dt<60min)
    IF (ievent>1.AND.idt<60) THEN
      WRITE(*,'(3x,I5,a6,i4)') iyr,'  day:', iida
    END IF
    
!!if last day of month
    IF (i_mo /= mo_chk) THEN
      immo = immo + 1
    END IF
    
!! initialize variables at beginning of day
    CALL sim_initday
!! added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
    
    sol_sumno3 = 0.
    sol_sumsolp = 0.
    DO j = 1, mhru
      DO ly = 1, sol_nly(j)
        sol_sumno3(j) = sol_sumno3(j) + sol_no3(ly,j) + sol_nh3(ly,j)
        sol_sumsolp(j) = sol_sumsolp(j) + sol_solp(ly,j)
      END DO
    END DO
    
    IF ( fcstyr == iyr .AND. fcstday == i) THEN
      ffcst = 1
      pcpsim = 2
      tmpsim = 2
      rhsim = 2
      slrsim = 2
      wndsim = 2
      igen = igen + iscen
      CALL gcycl
      DO j = 1, subtot
        ii = 0
        ii = fcst_reg(j)
        IF (ii <= 0) ii = 1
        DO mon = 1, 12
          tmpmx(mon,j) = 0.
          tmpmn(mon,j) = 0.
          tmpstdmx(mon,j) = 0.
          tmpstdmn(mon,j) = 0.
          pcp_stat(mon,1,j) = 0.
          pcp_stat(mon,2,j) = 0.
          pcp_stat(mon,3,j) = 0.
          pr_w(1,mon,j) = 0.
          pr_w(2,mon,j) = 0.
          tmpmx(mon,j) = ftmpmx(mon,ii)
          tmpmn(mon,j) = ftmpmn(mon,ii)
          tmpstdmx(mon,j) = ftmpstdmx(mon,ii)
          tmpstdmn(mon,j) = ftmpstdmn(mon,ii)
          pcp_stat(mon,1,j) = fpcp_stat(mon,1,ii)
          pcp_stat(mon,2,j) = fpcp_stat(mon,2,ii)
          pcp_stat(mon,3,j) = fpcp_stat(mon,3,ii)
          pr_w(1,mon,j) = fpr_w(1,mon,ii)
          pr_w(2,mon,j) = fpr_w(2,mon,ii)
        END DO
      END DO
    END IF
    
    dtot = dtot + 1.
    nd_30 = nd_30 + 1
    IF (nd_30 > 30) nd_30 = 1
    
    IF (curyr > nyskip) ndmo(i_mo) = ndmo(i_mo) + 1
    
    IF (pcpsim < 3) CALL clicon      !! read in/generate weather
    
!! call resetlu
    IF (ida_lup(no_lup) == i .AND. iyr_lup(no_lup) == iyr) THEN
      CALL resetlu
      no_lup = no_lup + 1
    END IF
    
    CALL command              !! command loop
    
    
    DO ihru = 1, nhru
      IF (idaf > 180 .AND. sub_lat(hru_sub(ihru)) < 0) THEN
        IF (i == 180) THEN
          IF (mgtop(nop(ihru),ihru) /=17) THEN
            dorm_flag = 1
            CALL operatn
            dorm_flag = 0
          END IF
          nop(ihru) = nop(ihru) + 1
          
          IF (nop(ihru) > nopmx(ihru)) THEN
            nop(ihru) = 1
          END IF
          
          phubase(ihru) = 0.
          yr_skip(ihru) = 0
        END IF
        
      END IF
    END DO
    
!! write daily and/or monthly output
    IF (curyr > nyskip) THEN
      CALL writed
      
!! output.sol file
      IF (isol == 1) CALL soil_write
      
      iida = i + 1
      CALL xmon
      CALL writem
    ELSE
      iida = i + 1
      CALL xmon
    END IF
    
  END DO                                        !! end daily loop
  
!! perform end-of-year processes
  DO j = 1, nhru
    
!! compute biological mixing at the end of every year
    
!          if (biomix(j) > .001) call tillmix (j,biomix(j))
    IF (biomix(j) > .001) CALL newtillmix (j,biomix(j))
    
!! update sequence number for year in rotation to that of
!! the next year and reset sequence numbers for operations
    IF (idplt(j) > 0) THEN
      IF (idc(idplt(j)) == 7) THEN
        curyr_mat(j) = curyr_mat(j) + 1
        curyr_mat(j) = MIN(curyr_mat(j), mat_yrs(idplt(j)))
      END IF
    END IF
    
!! update target nitrogen content of yield with data from
!! year just simulated
    DO ic = 1, mcr
      xx = 0.
      xx = REAL(curyr)
      tnylda(j) = (tnylda(j) * xx + tnyld(j)) / (xx + 1.)
    END DO
    
    IF (idaf < 181) THEN
      IF (mgtop(nop(j),j) /= 17) THEN
        dorm_flag = 1
        ihru = j
        CALL operatn
        dorm_flag = 0
      END IF
      nop(j) = nop(j) + 1
      
      IF (nop(j) > nopmx(j)) THEN
        nop(j) = 1
      END IF
      
      phubase(j) = 0.
      yr_skip(j) = 0
    END IF
    IF (mgtop(nop(j),j) == 17) THEN
      nop(j) = nop(j) + 1
      IF (mgtop(nop(j),j) == 17) THEN
        yr_skip(j) = 1
      END IF
    END IF
    
  END DO
  
!! update simulation year
  iyr = iyr + 1
END DO            !!     end annual loop

RETURN
1234 FORMAT (1X,' Executing year ', i4)
END SUBROUTINE simulate
