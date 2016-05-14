SUBROUTINE routels(iru_sub)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icodes(:)   |none          |routing command code:
!!                               |0 = finish       9 = save
!!                               |1 = subbasin    10 = recday
!!                               |2 = route       11 = reccnst
!!                               |3 = routres     12 = structure
!!                               |4 = transfer    13 =
!!                               |5 = add         14 = saveconc
!!                               |6 = rechour     15 =
!!                               |7 = recmon      16 = autocal
!!                               |8 = recyear     17 = routing unit
!!    ihouts(:)   |none          |outflow hydrograph storage location
!!    inum1s(:)   |none          |routing unit number - runon
!!    inum2s(:)   |none          |inflow hydrograph number number
!!    inum3s(:)   |none          |subbasin number
!!    inum4s(:)   |none          |not used in this command
!!    rnum1s(:)   |none          |drainage area of routing unit in km2
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
use parm
use rossmod


INTEGER, INTENT(INOUT)                   :: iru_sub
REAL :: latqout, gwqout, latqrunon, surfqrunon, latqlyr

!!    compute infiltration from surface runon to next landscape unit
REAL :: ls_overq, ls_latq, ls_tileq, ls_gwq

!!    water
!      if (rnum1 > 1.e-4) return
!      xx = varoute(29,inum2) + varoute(30,inum2) + varoute(31,inum2) +
!     &        varoute(32,inum2)
!      if (xx < 1.e-6) then
!        ls_overq = varoute(1,inum2) * rnum1
!        ls_latq = 0.
!        ls_tileq = 0.
!        ls_gwq = 0.
!      else
!        ls_overq = varoute(29,inum2) * rnum1
!        ls_latq = varoute(30,inum2) * rnum1
!        ls_tileq = varoute(31,inum2) * rnum1
!        ls_gwq = varoute(32,inum2) * rnum1
!      end if

IF (inum5 == 0 .OR. inum8 == 0) THEN
  ls_overq = 0.
  surfqrunon = 0.
  surfqout = 0.
  IF (inum5 == 0) THEN
    ls_overq = varoute(29,inum2) * rnum1
  END IF
  IF (inum8 == 0) THEN
    ls_overq = ls_overq + varoute(31,inum2) * rnum1
  END IF
!!    sediment
  sed = varoute(3,inum2) * rnum1
!! use surface runoff (mm) for eiq - m3/(10 * 100*km2) = mm
  ru_eiq(inum3,inum1) = ls_overq / (1000. * daru_km(inum3,inum1))
  trancap = ru_ktc(inum3,inum1) * ru_c(inum3,inum1) *  &
      ru_eiq(inum3,inum1) * ru_k(inum3,inum1) *  &
      ru_a(inum3,inum1)**1.4 * ru_ovs(inum3,inum1)**1.4
  trancap = trancap * daru_km(inum3,inum1) * 100.   !! t/ha -> t
  IF (sed > trancap) THEN
    varoute(3,ihout) = trancap
    dr = varoute(3,ihout) / sed
  ELSE
    varoute(3,ihout) = sed
    dr = 1.
  END IF
  
!!    organic nitrogen
  orgn = varoute(4,inum2) * rnum1
  cy = varoute(3,inum2) / (varoute(2,inum2) + 1.e-6)
  IF (cy > .01) THEN
    enratio = .78 * cy ** (-.2468)
  ELSE
    enratio = 3.
  END IF
  enratio = MIN(enratio,3.)
  dr_er = dr * enratio
  dr_er = MIN(dr_er,1.)
  varoute(4,ihout) = orgn * dr_er
  
!!    organic phosphorus
  orgp = varoute(5,inum2) * rnum1
  varoute(5,ihout) = orgp * dr_er
  
!!    nitrate (& nitrite)
  no3 = (varoute(6,inum2) + varoute(15,inum2)) * rnum1
!!    soluble phosphorus
  slbp = varoute(7,inum2) * rnum1
!!    soluble pesticide not routed
!!    sorbed pesticide not routed
!!    chlorophyll-a not routed
!!    ammonium
  nh3 = varoute(14,inum2) * rnum1
!!    CBOD not routed
!!    dissolved oxygen not routed
!!    persistent bacteria not routed
!!    less persistent bacteria not routed
  
!!    dstor = rnum1
  
!!    compute infiltration from surface runon to next landscape unit
  IF (ls_overq > 1.e-6) THEN
    DO kk = 1, hrutot(inum3)
      jj= hru1(inum3) + kk - 1
      IF (iru_sub == 0) THEN
        frac = hru_fr(jj)
        dakm = sub_km(inum3)
      ELSE
        frac = hru_rufr(inum1,kk)
        dakm = daru_km(inum3,inum1)
      END IF
      IF (frac > 1.e-9) THEN
        xx = frac * dakm * 100.     !!km2*100 = ha
        surfqrunon = ls_overq / (10. * xx)
        
        IF (pot_volxmm(jj) > 1.e-6) THEN
          pot_vol(jj) = pot_vol(jj) + surfqrunon
        ELSE
          
!!        add surface runon to soil layers - use percmain like rainfall infiltration
          qs = surfqrunon / 24.
          
          
          vs = (qs ** .4) * (hru_slp(jj) ** .3) / (ov_n(jj) ** .6)
          trt = slsubbsn(jj) / (3600. * vs)
          inflpcp = sol_k(1,jj) * trt + dstor
          inflpcp = MIN (inflpcp, surfqrunon)
          surfqout = surfqout + (surfqrunon - inflpcp) * 10. * xx
          ihru = jj
          latq(jj) = 0.
          sepbtm(jj) = 0.
          qtile = 0.
          
!!-------------------OGXinSWAT Begin------------------------------
!!  add surface runon
          IF (ievent>0) THEN
            solcol(jj)%runon=solcol(jj)%runon+inflpcp/24.
          ELSE
            CALL percmain
          END IF
!!--------------------End--------------------------------
          
          latqout = latqout + latq(jj) * 10. * xx
          gwqout = gwqout + sepbtm(jj) * 10. * xx
        END IF
      END IF
    END DO
    varoute(29,ihout) = varoute(29,ihout) + surfqout
    varoute(30,ihout) = varoute(30,ihout) + latqout
    varoute(32,ihout) = varoute(32,ihout) + gwqout
!    surfq_ru(jj) = surfqout / (10. * xx)
!    latq_ru(jj) = latqout / (10. * xx)
!    infl_ru(jj) = inflpcp
    varoute(2,ihout) = varoute(2,ihout) + surfqout + latqout + gwqout
  END IF
END IF

!!    compute lateral flow to next landscape unit
IF (inum6 == 0) THEN
  ls_latq = varoute(30,inum2) * rnum1
  latqout = 0.
  latqrunon = 0.
  IF (ls_latq > 1.e-9) THEN
    DO kk = 1, hrutot(inum3)
      jj= hru1(inum3) + kk - 1
      IF (iru_sub == 0) THEN
        frac = hru_fr(jj)
        dakm = sub_km(inum3)
      ELSE
        frac = hru_rufr(inum1,kk)
        dakm = daru_km(inum3,inum1)
      END IF
      IF (frac > 1.e-9) THEN
        xx = frac * dakm * 100.     !!km2*100 = ha
        latqrunon = ls_latq / (10. * xx)
        jj= hru1(inum3) + kk - 1
        
!!-------------------OGXinSWAT Begin------------------------------
!!  lateral in flow
        IF (ievent>0) THEN
          solcol(jj)%qlatin=solcol(jj)%qlatin+latqrunon/24.
        ELSE
          
!!          put in soil layers - weighted by depth of soil layer
          dep = 0.
          xslat = 0.
          DO lyr = 1, sol_nly(jj)
            latqlyr = ((sol_z(lyr,jj)-dep) / sol_z(sol_nly(jj),jj))  &
                * latqrunon
            dep = sol_z(lyr,jj)
            sol_st(lyr,jj) = sol_st(lyr,jj) + latqlyr
            IF (sol_st(lyr,jj) > sol_ul(lyr,jj)) THEN
              xslat = xslat + (sol_st(lyr,jj) - sol_ul(lyr,jj))
              sol_st(lyr,jj) = sol_ul(lyr,jj)
            END IF
          END DO
!!          add excess water to next landscape unit
          latqout = latqout + xslat * 10. * xx
        END IF
!!--------------------End--------------------------------
      END IF
    END DO
    varoute(30,ihout) = varoute(30,ihout) + latqout
    varoute(2,ihout) = varoute(2,ihout) + latqout
  END IF
END IF

!!    compute groundwater flow to next landscape unit -
!!    used the next day in gwmod - routed with recharge
IF (inum7 == 0) THEN
  ls_gwq = varoute(32,inum2) * rnum1
  IF (ls_gwq > 1.e-6) THEN
    gwqout = 0.
    gwqrunon = 0.
    DO kk = 1, hrutot(inum3)
      jj= hru1(inum3) + kk - 1
      IF (iru_sub == 0) THEN
        frac = hru_fr(jj)
        dakm = sub_km(inum3)
      ELSE
        frac = hru_rufr(inum1,kk)
        dakm = daru_km(inum3,inum1)
      END IF
      IF (frac > 1.e-9) THEN
        xx = frac * dakm * 100.     !!km2*100 = ha
        gwqrunon = ls_gwq / (10. * xx)
        jj= hru1(inum3) + kk - 1
        gwq_ru(jj) = gwqrunon
      END IF
    END DO
  END IF
END IF

DO ii = 29, mvaro
  varoute(ii,inum2) = 0.
END DO

RETURN
END SUBROUTINE routels
