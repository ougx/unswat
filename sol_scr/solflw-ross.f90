!Gengxin Michael Ou
!2014-6-1
!This module is modified based on the code developed by Ross (2003)

subroutine TRS(k)
  !PCP -- irrigation plus preciptation
  !ETS -- potential soil evaporation, (mm/hr)
  !ETP -- potential transpitation, (mm/hr)
  !ZGW -- groundwater depth, (mm)
  !ZROOT- root depth, (mm)

  use parm
  use ROSSMOD
  implicit none
  !real, intent(in) :: DTTOTEnd  !PCP,ETS,ETP,ZGW,ZROOT,
  integer, intent(in) :: k
  !real, intent(in) :: DTTOTEnd
  integer :: nNOD, iTER, iTERTotal, iLAY, iStep, nNODm1
  integer :: iTMP, iTMP0, iTMP1, iTMP2
  integer :: j,N,nun,ntot, iis, iie
  integer :: ITOPTYPE, NLAY, iDelta_t

  real						:: WCS								!water depth of the saturated portion within the cell intersected by groundwater table
  real 						:: RTMP0

  real :: s0
  real :: s1
  real :: rain

  real :: WCSTOR0, WCSTOR1               !water storage, before and after a time step
  real :: WCSTOR0TOT, WCSTOR1TOT         !water storage, before and after a day
  real :: DTTOL         !

  real :: HMIN                           !lowest pressure head at top
  real :: HBUB                           !entry pressure head of the bottom layer


  real :: POV                            !parameter, used in subsurface lateral flow and overland flow calculation

  real :: QOV0                           !overland flow at the beginning of a iteration
  real :: QOV1                           !overland flow at the end of a iteration
  real :: QSURF                          !surface flow,   mm/hr
  real :: qNET                           !prescribed potential net flux on the top,   mm/hr
  real :: RMASS                          !residual of mass balance,  mm

  !real :: qNETt(mstep)                   !prescribed potential net flux on the top,   mm/hr
  !real :: EPMAX(mstep)                   !irrigation plus rainfall (mm/hr)

  real :: rchr  !recharge
  real :: qprec, qevap, QUB(mstep),sic,soc,src
  real     ::  dzgw, Frac_Impervious, hbot
  real, dimension(MAXNODE) :: DZ,FZN,qsum,sicum,socum,srcum,hh
  integer, dimension(MAXNODE) :: jt
!%%%%%%%%%%%%%%%%%%%%%%% outgoings
  !real :: RUNOFF(mstep)      !runoff of Impervious cover, mm
  !real ::       !runoff of Impervious cover, mm
  !real ::       !runoff of Impervious cover, mm
  !real :: FLOWTI     				!cumulative infiltration at the top(mm)
  !real :: FLOWTO     				!cumulative exfiltration at the top (mm)
  !real :: FLOWBI,FLOWBO			!total flow at the bottom for each step (mm) bottom in ~~~ GW ET; bottom out ~~~ GW recharge
  !real :: FLOWLI,FLOWLO      !total horizontal flow (in and out) within the unsaturated portion
  !real :: WATSTOR(0:mstep)   !water storage of the soil


  !real, dimension(mstep) :: evap,infil,drn,runoff
  real  :: evap,infil,drn,runoff(mstep),irritot
  !type(SOILMAT), pointer :: mat
  type(SOILCOLUMN), pointer	::	scol
  logical :: lowgw    !if the groundwater level is lower than the bottom of the soil profile
!%%%%%%%%%%%%%%%%%%%%%%% outgoings

  !if (debuGGing) write(IFPROFILE,*) 'idst: ', DELT,'         Elapse: ', Telapse

!~~~~~~~~~~~~~~~start daily variables (not change in the sobroutine)~~~~~~~~~~~~~~~~~~~


  !
  if (k /= 10) return
  rchrg(k) = ZERO

  scol=>SOLCOL(k)
  ntot=scol%NNOD
  NLAY=scol%NLAY
  nun=ntot

  !QURBAN=zero
  !ESACT=zero
  !EPACT=zero
  !qLAT=scol%QLATOUT
  !``````````````````````````````
  !subroutine SOLCOL_Initialize_WatFlow(kHRU,N,DZ,DZ2,H,HD,WC,K,CAP,FZN,H1m,HB,qUB,SRT,SL,qNET)
  !call SOLCOL_Initialize_WatFlow(kHRU,nNOD,WCSTOR0,DZ,DZ2,HOLD,HDERI0,WCOLD,K1,CAP,FROZEN,HATM,HBUB,QURBAN,SROOT0,SLAT0,qNETt)

  if (scol%IHATM==0) then !soil evaporation will be limited by atmosphere condition

    !HMIN should be selected such so that the effective water content is at least higher than 0.05.
    !HMIN (S=0.05) is stored as scol%HCRIT when scol%IHATM==0
    !It should also be lower (when negative) than P3 when the root water uptake is considered.
    !When both limits for root water uptake (P3) and evaporation (HMIN) are reached,
    !HMIN>P3 leads to inflow since it controls the flux across the boundary.
    !first calculate form the
    !rhd(:)      |none              |relative humidity for the day in HRU
    !tmpav(:)    |deg C             |average air temperature on current day in HRU
    HMIN=1000*8.314*(tmpav(k)+273.15)*log(rhd(k))/0.018015/GRAVITY    !mm

  !!check the top soil residual
    if (HMIN<scol%HCRIT) HMIN=scol%HCRIT
  else
    HMIN=scol%HCRIT
  endif


  !check if the soil layer is frozen
  FZN=1.
  do iLAY=1, sol_nly(k)
    if (sol_tmp(iLAY,k) <= 0.) then
    iis=scol%ILBNOD(iLAY-1)+1
    iie=scol%ILBNOD(iLAY)
    FZN(iis: iie)=scol%FACFROZN
    endif
  enddo


  !! Urban Impervious cover
  if (iurban(k)>0) then
    Frac_Impervious=fcimp(urblu(k))
  else
    Frac_Impervious=ZERO
  endif

  !search the lowest unsaturated node
  DZ(1:ntot)=scol%DZ(1:ntot)


  !call searchgw(k,ntot,nun,DZ,dzgw,hh,scol%DEPGW)


  call bottom_pressure(k,ntot,nun,DZ,dzgw,scol%DEPGW)


  write (IFDEBUG,*) ""
  write (IFDEBUG,*) " RUNNING ROSS FOR HRU ",k, " YEAR ", curyr, " DAY ", iida

#ifdef debugMODE
  !call print_var(IFPROFILE,nun,SOLCOL(k)%var,iyr+iida/1000.)
  !stop
#endif



  !if (dzgw < 0.) then
  !  lowgw = .true.
  !  nun=ntot
  !else
  !  lowgw = .false.
  !endif


#ifdef debugMODE
  !call print_var(IFPROFILE,nun,SOLCOL(k)%var,iyr+iida/1000.)
  !stop
#endif
  !update storage before soil water model

  !call SOLCOL_Update_Storage(k)
  s0=sum(scol%WC(1:nun)*scol%DZ(1:nun))

  !~~~~~~~~~~~~~~~end daily variables (not change in the sobroutine)~~~~~~~~~~~~~~~~~~~

  qNET=ZERO
  qsum=ZERO
  sicum=ZERO
  socum=ZERO
  srcum=ZERO
  runoff=ZERO
  evap=ZERO
  infil=ZERO
  drn=ZERO
  irritot=ZERO
  rain=scol%HPOND

  if (scol%nwel > 0) scol%wel%qtot = 0.

  !DZ(nun)=DZ(nun)-dzgw

  !solve(k,mstep,ts,qprec,qevap,n,dx,jt,hmin,h0,var,fzn,snl,kappa,evap,runoff,infil,drn, &
  !							qsum,sicum,socum,srcum,dtmin,dtmax,dSmax,dSmaxr,dSfac,dpmaxr)

  jt(1:ntot)=scol%jt(1:ntot)

#ifdef debugMODE
    !call print_var(IFPROFILE,nun,scol%var,0.0)
  !stop
#endif
  qevap=scol%ESMAX            			!potential soil evaporation ,  mm/hr
  !print *, precipday,sum(precipdt)

  do iStep=1, mstep
    qprec=scol%RAIN(iSTEP)*(ONE-Frac_Impervious)+scol%IRRI(iSTEP)+scol%RUNON(iStep)    !mm/hr

    !update groundwater level every hour
    hbot = dzgw + drn/gw_spyld(k)

    if (iurban(k)>0) then
      !runoff from impervious area with initial abstraction
      RTMP0 = (scol%RAIN(iSTEP)*(tstep(iSTEP)-tstep(iSTEP-1)) - abstinit) * Frac_Impervious
      if (RTMP0<ZERO)  RTMP0 = ZERO
      QUB(iSTEP)=RTMP0
    else
      QUB(iSTEP)=ZERO
    endif
    qNET=qNET+(qprec-qevap)*(tstep(iSTEP)-tstep(iSTEP-1))
    rain=rain+qprec*(tstep(iSTEP)-tstep(iSTEP-1))
    call solve(k,tstep(iSTEP-1),tstep(iSTEP),qprec,qevap,nun,DZ,jt,scol%hqmin,HMIN,hbot,scol%HPOND,scol%Kdn,scol%Kup,scol%var, &
                FZN,scol%POV,FiveThd,evap,runoff(iStep),infil,drn,irritot,qsum,sicum,socum,srcum, &
                scol%dtmin,scol%dtmax,scol%dSmax,scol%dSmaxr,scol%dSfac,scol%dpmaxr,lowgw)
  enddo

  !clear input
  scol%RAIN=ZERO
  scol%RUNON=ZERO
  scol%EPMAX=ZERO
  scol%ESMAX=ZERO
  scol%IRRI=ZERO
  scol%QLATIN=ZERO

!        str1=sum((SOLMAT(jt)%WCS-SOLMAT(jt)%WCSR*(1.0-S))*dx)
!        rlat=sum(socum-sicum+srcum)
!        roff=sum(runoff)
!        write (IFBAL,'(//10A15,/12(1PE15.6))') 't','h0','h1','prec','et','runoff','infil','drn','qlat','Str', &
!          t,h0,var(1)%h,prectot,evap,roff,infil,drn,rlat, &
!          str1,str0+infil-drn-str1-rlat, &
!          !win-(wp-wpi+h0+evap+drn+runoff)
!        prectot-(str1-str0+h0+evap+drn+roff+rlat)



  !recalculate the water content for the cell intersected by groundwater table
  !unsaturated water content + saturated water content
  !DZ(nun)=DZ(nun)+dzgw
  !call setsaturation(DZ(nun),dzgw,scol%var(nun),jt(nun),scol%WC(nun))

  call FinalizeDayUN(k,nun,ntot,runoff,QUB,qsum,evap,sicum,socum,srcum,irritot)

  scol%QSUM=qsum(1:ntot)

#ifdef debugMODE
    !write (IFDEBUG,*) "drn"
    !write (IFDEBUG,*) drn
    !write (IFDEBUG,*) ""
    !call print_var(IFPROFILE,nun,scol%var,iyr*1000.+iida,scol%WC)
  !stop
#endif
  rain = rain + irritot
  if (scol%IPRINT <0 .and. scol%IPRINT >= -2) then
    s1=sum(scol%WC(1:nun)*scol%DZ(1:nun))
    sic=sum(sicum)
    soc=sum(socum)
    src=sum(srcum)
    write (IFBAL,'(I5,18((F12.4)))') k,tstep(mstep)/24.,scol%DEPGW,dzgw,qNET,scol%EPMAX*24.,s0,s1, &
                              sum(runoff),scol%HPOND,infil,evap,drn,irritot,sic,soc,src, &
                              s0+infil-drn+sic-soc-src-s1, &
                              rain-(s1-s0+scol%HPOND+drn+evap+soc+src+sum(runoff)-sic)
  endif
  if (scol%IPRINT == -1) then
    call print_var(IFPROFILE,k,nun,scol%var,tstep(mstep)/24.,scol%DEPGW,scol%WC,DZ,qsum,socum,srcum)
  elseif (scol%IPRINT > 0) then
    do j=1, scol%IPRINT
      iLAY = scol%OBLay(j)
      write(IFLAY,"(2I10,4F12.3)") k,iLAY,scol%OBDep(j),tstep(mstep)/24.,scol%WC(iLAY),scol%var(iLAY)%h
    enddo
  endif
  !write (*,*) 'excuting at year:', iyr, '   day:', iida


    do j = 1, scol%nwel
      s0 = hru_km(k)*scol%wel(j)%qtot*1.e3    !m3
      iStep = int(dtot)

      write(IFWEL,"(2I5,I10,1PE15.8)") k,j,iStep,s0
    enddo

end subroutine




