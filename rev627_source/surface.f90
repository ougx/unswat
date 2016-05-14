SUBROUTINE surface
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine models surface hydrology at any desired time step

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ovrlnd(:)   |mm H2O        |overland flow onto HRU from upstream
!!                               |routing unit
!!    peakr       |mm/hr         |peak runoff rate
!!    precipday   |mm H2O        |effective precipitation for the day in HRU
!!    qday        |mm H2O        |surface runoff loading to main channel
!!                               |for day
!!    surfq(:)    |mm H2O        |surface runoff generated in HRU during
!!                               |the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    precipday   |mm H2O        |effective precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: canopyint, snom, crackvol, dailycn, volq, crackflow, surfst_h2o,
!!    SWAT: alph, pkq, tran, eiusle, ysed

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm
use rossmod

INTEGER :: j,sb,kk
REAL :: precip_fr,dt
REAL :: irfr,hruvirr

j = 0
j = ihru
sb = hru_sub(j)
hruirrday = 0.
irmmdt = 0.

!! compute canopy interception
IF (idplt(j) > 0) THEN
  CALL canopyint
END IF

!! compute snow melt
CALL snom

!! output by elevation band to output.snw
IF (isnow == 1) THEN
  WRITE(115,1010) i, iyr, subnum(j), hruno(j), (snoeb(ib,j), ib = 1,10)
END IF

!!-------------------OGXinSWAT Begin------------------------------
!!  skip crackvol
!! compute crack volume
IF (ievent==0) THEN
  IF (icrk == 1) CALL crackvol
END IF
!!----------------------End--------------------------------------

!! add overland flow from upstream routing unit
precipday = precipday + ovrlnd(j)
IF (nstep > 0) THEN
  dt=24./nstep
  DO ii = 1, nstep
    
!!-------------------OGXinSWAT Begin------------------------------
!!  define precip and overland flow
    IF (ievent>0) THEN
      solcol(j)%rain(ii)=precipdt(ii+1)/dt
      solcol(j)%runon(ii)=solcol(j)%runon(ii)+ovrlnd_dt(j,ii)/dt
    END IF
!!-------------------------End------------------------------------
    precipdt(ii+1) = precipdt(ii+1) + ovrlnd_dt(j,ii)
  END DO
END IF

!! add irrigation from retention-irrigation ponds to soil water
IF (ri_luflg(j)==1) THEN
  irfr = hru_km(j)* (1.-fimp(urblu(j))) / ri_subkm(sb)
  DO ii=1,nstep
!amount irrigated in hru
    hruvirr = ri_totpvol(ii) * irfr !m3
    irmmdt(ii) = hruvirr / (hru_km(j) * (1.- fimp(urblu(j))) * 1000.) !mm/dt
    
!!---------------OGXinSWAT Begin----------------------------
!!  add irrigation to precipitation
    IF (ievent>0) THEN
      solcol(j)%irri(ii)=solcol(j)%irri(ii) + irmmdt(ii)/dt  !!OGX: mm/hr
    ELSE
!add irrigated water to soil water content
      
      DO kk=1,sol_nly(j)
        IF(irmmdt(ii)<sol_ul(kk,j)-sol_st(kk,j)) THEN
          sol_st(kk,j) = sol_st(kk,j) + irmmdt(ii)
          EXIT
        ELSE
          sol_st(kk,j) = sol_ul(kk,j)
          irmmdt(ii) = irmmdt(ii) - (sol_ul(kk,j)-sol_st(kk,j))
        END IF
      END DO
    END IF
!!---------------End----------------------------
    
  END DO
END IF

!!calculate subdaily curve number value
CALL dailycn

!! compute runoff - surfq in mm H2O

!!---------------OGXinSWAT Begin----------------------------
!!skip

IF (ievent>0) THEN
  RETURN
END IF
!!--------------------------End-----------------------------


IF (precipday > 0.1) THEN
  CALL volq
  
!! adjust runoff for loss into crack volume
  IF (surfq(j) > 0. .AND. icrk == 1) CALL crackflow
END IF

surfq(j) = surfq(j) + qird(j)
qird(j) = 0.


!! calculate amount of surface runoff reaching main channel during day
!! (qday) and store the remainder
CALL surfst_h2o

!! calculate half-hour rainfall
IF (precipday > 0.01) CALL alph(0)

IF (qday > 0.0001) THEN
!! compute peak rate - peakr in m3/s
  CALL pkq(0)
END IF

IF (qday > 0.0001 .AND. peakr > 0.) THEN
!! compute transmission losses for non-HUMUS datasets
  CALL tran
  CALL eiusle
  
!! calculate sediment erosion by rainfall and overland flow
  CALL ovr_sed
END IF

CALL cfactor
IF (surfq(j) > 1.e-6 .AND. peakr > 1.e-6) CALL ysed(0)

IF (qday < 0.) qday = 0.

1010  FORMAT (2(i4,1X),a5,a4,1X,10F8.3)
RETURN
END SUBROUTINE surface
