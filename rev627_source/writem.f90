SUBROUTINE writem
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:05

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly output

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year of simulation
!!    hrumono(:,:)|varies        |HRU monthly output array
!!    hrupest(:)  |none          |pesticide use flag:
!!                               | 0: no pesticides used in HRU
!!                               | 1: pesticides used in HRU
!!    hrupstm(:,1,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU during month
!!                               |(in solution)
!!    hrupstm(:,2,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU during month
!!                               |(sorbed to sediment)
!!    i           |julian date   |current day of simulation
!!    idaf        |julian date   |beginning day of simulation
!!    idal        |julian date   |ending day of simulation
!!    iprint      |none          |print code:
!!                               |0 monthly
!!                               |1 daily
!!                               |2 annually
!!    iprp        |none          |print code for output.pst file
!!                               |0 do not print pesticide output
!!                               |1 print pesticide output
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    leapyr      |none          |leap year flag
!!                               |0  leap year
!!                               |1  regular year
!!    i_mo        |none          |month for next day of simulation
!!    mo_chk      |none          |month for current day of simulation
!!    nbyr        |none          |number of years in simulation
!!    ndays(:)    |julian date   |julian date for last day of preceding
!!                               |month (where the array location is the
!!                               |number of the month). The dates are for
!!                               |leap years
!!    nhru        |none          |number of HRUs in watershed
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    nres        |none          |total number of reservoirs in the
!!                               |watershed
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    res_seci(:) |m             |secchi-disk depth
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    resoutm(1,:)|m^3/s         |flow into reservoir during month
!!    resoutm(2,:)|m^3/s         |flow out of reservoir during month
!!    resoutm(3,:)|metric tons   |sediment entering reservoir during month
!!    resoutm(4,:)|metric tons   |sediment leaving reservoir during month
!!    resoutm(5,:)|mg/L          |sediment concentration in reservoir during
!!                               |month
!!    resoutm(6,:)|mg pst        |pesticide entering reservoir during month
!!    resoutm(7,:)|mg pst        |pesticide lost from reservoir through
!!                               |reactions during month
!!    resoutm(8,:)|mg pst        |pesticide lost from reservoir through
!!                               |volatilization during month
!!    resoutm(9,:)|mg pst        |pesticide moving from water to sediment
!!                               |through settling during month
!!    resoutm(10,:)|mg pst        |pesticide moving from sediment to water
!!                               |through resuspension during month
!!    resoutm(11,:)|mg pst        |pesticide moving from water to sediment
!!                               |through diffusion during month
!!    resoutm(12,:)|mg pst        |pesticide lost from reservoir sediment layer
!!                               |through reactions during month
!!    resoutm(13,:)|mg pst        |pesticide lost from reservoir sediment layer
!!                               |through burial during month
!!    resoutm(14,:)|mg pst        |pesticide transported out of reservoir during
!!                               |month
!!    resoutm(15,:)|mg pst/m^3    |pesticide concentration in reservoir water
!!                               |during month
!!    resoutm(16,:)|mg pst/m^3    |pesticide concentration in reservoir sediment
!!                               |layer during month
!!    resoutm(17,:)|m^3 H2O       |evaporation from reservoir during month
!!    resoutm(18,:)|m^3 H2O       |seepage from reservoir during month
!!    resoutm(19,:)|m^3 H2O       |precipitation on reservoir during month
!!    resoutm(22,:)|kg N          |organic N entering reservoir during month
!!    resoutm(23,:)|kg N          |organic N leaving reservoir during month
!!    resoutm(24,:)|kg P          |organic P entering reservoir during month
!!    resoutm(25,:)|kg P          |organic P leaving reservoir during month
!!    resoutm(26,:)|kg N          |nitrate entering reservoir during month
!!    resoutm(27,:)|kg N          |nitrate leaving reservoir during month
!!    resoutm(28,:)|kg N          |nitrite entering reservoir during month
!!    resoutm(29,:)|kg N          |nitrite leaving reservoir during month
!!    resoutm(30,:)|kg N          |ammonia entering reservoir during month
!!    resoutm(31,:)|kg N          |ammonia leaving reservoir during month
!!    resoutm(32,:)|kg P          |mineral P entering reservoir during month
!!    resoutm(33,:)|kg P          |mineral P leaving reservoir during month
!!    resoutm(34,:)|kg chla       |chlorophyll-a entering reservoir during month
!!    resoutm(35,:)|kg chla       |chlorophyll-a leaving reservoir during month
!!    resoutm(36,:)|mg P/L        |organic P concentration in reservoir water
!!                               |during month
!!    resoutm(37,:)|mg P/L        |mineral P concentration in reservoir water
!!                               |during month
!!    resoutm(38,:)|mg N/L        |organic N concentration in reservoir water
!!                               |during month
!!    resoutm(39,:)|mg N/L        |nitrate concentration in reservoir water during
!!                               |month
!!    resoutm(40,:)|mg N/L        |nitrite concentration in reservoir water during
!!                               |month
!!    resoutm(41,:)|mg N/L        |ammonia concentration in reservoir water during
!!                               |month
!!    wshddayo(35)|mm H2O        |amount of water stored in soil profile in
!!                               |watershed at end of day
!!    wshdmono(1) |mm H2O        |average amount of precipitation in watershed
!!                               |for the month
!!    wshdmono(3) |mm H2O        |surface runoff in watershed for month
!!    wshdmono(4) |mm H2O        |lateral flow contribution to streamflow in
!!                               |watershed for month
!!    wshdmono(5) |mm H2O        |water percolation past bottom of soil profile
!!                               |in watershed for month
!!    wshdmono(6) |mm H2O        |water yield to streamflow from HRUs in
!!                               |watershed for month
!!    wshdmono(7) |mm H2O        |actual evapotranspiration in watershed
!!                               |for month
!!    wshdmono(8) |deg C         |average maximum temperature in watershed for
!!                               |the month
!!    wshdmono(9) |deg C         |average minimum temperature in watershed for
!!                               |the month
!!    wshdmono(12)|metric tons   |sediment yield from HRUs in watershed for
!!                               |the month
!!    wshdmono(39)|mm H2O        |freezing rain/snow fall in watershed for
!!                               |the month
!!    wshdmono(40)|kg N/ha       |organic N loading to stream in watershed for
!!                               |the month
!!    wshdmono(41)|kg P/ha       |organic P loading to stream in watershed for
!!                               |the month
!!    wshdmono(42)|kg N/ha       |nitrate loading to stream in surface runoff
!!                               |in watershed for the month
!!    wshdmono(43)|kg P/ha       |soluble P loading to stream in watershed for
!!                               |the month
!!    wshdmono(44)|kg N/ha       |plant uptake of N in watershed for the month
!!    wshdmono(45)|kg N/ha       |nitrate loading to stream in lateral flow
!!                               |in watershed for the month
!!    wshdmono(46)|kg N/ha       |nitrate percolation past bottom of soil
!!                               |profile in watershed for the month
!!    wshdmono(104)|mm H2O        |groundwater contribution to stream in
!!                               |watershed for the month
!!    wshdmono(108)|mm H2O        |potential evapotranspiration in watershed
!!                               |for the month
!!    wshdmono(109)|mm H2O        |drainage tile flow contribution to stream
!!                               |in watershed for the month
!!    wtrmon(:,:) |varies        |HRU impoundment monthly output array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrumono(:,:)|varies        |HRU monthly output array
!!    hrupstm(:,:,:)|varies      |HRU monthly pesticide output array
!!    hrupsty(:,:,:)|varies      |HRU annual pesticide output array
!!    hruyro(:,:) |varies        |HRU annual output array
!!    immo        |none          |current cumulative month of simulation
!!    rchmono(:,:)|varies        |reach monthly output array
!!    rchyro(:,:) |varies        |reach annual output array
!!    resoutm(:,:)|varies        |reservoir monthly output array
!!    resouty(:,:) |varies        |reservoir annual output array
!!    submono(:,:)|varies        |subbasin monthly output array
!!    subyro(:,:) |varies        |subbasin annual output array
!!    wshd_aamon(:,1)|mm H2O        |average annual precipitation in watershed
!!                               |falling during month
!!    wshd_aamon(:,2)|mm H2O        |average annual freezing rain in watershed
!!                               |falling during month
!!    wshd_aamon(:,3)|mm H2O        |average annual surface runoff in watershed
!!                               |during month
!!    wshd_aamon(:,4)|mm H2O        |average annual lateral flow in watershed
!!                               |during month
!!    wshd_aamon(:,5)|mm H2O        |average annual water yield in watershed
!!                               |during month
!!    wshd_aamon(:,6)|mm H2O        |average annual actual evapotranspiration
!!                               |in watershed during month
!!    wshd_aamon(:,7)|metric tons   |average annual sediment yield in watershed
!!                               |during month
!!    wshd_aamon(:,8)|mm H2O        |average annual potential evapotranspiration
!!                               |in watershed during month
!!    wshdmono(:) |varies        |watershed monthly output array
!!    wshdyro(:)  |varies        |watershed annual output array
!!    wtrmon(:,:) |varies        |HRU impoundment monthly output array
!!    wtryr(:,:)  |varies        |HRU impoundment annual output array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idlast      |none          |number of days simulated in month
!!    j           |none          |counter
!!    k           |none          |counter
!!    sum         |mg            |total pesticide loading
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real
!!    SWAT: hrumon, impndmon, submon, rchmon, writea

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER ::  j, k
REAL :: sum

!! if last day of month or last day in last year

IF (i_mo /= mo_chk .OR. (curyr == nbyr .AND. i == idal)) THEN
  mo_atmo = mo_atmo + 1
  
!! calculate current month (cumulative) of simulation
!       immo = immo + 1
  
!! calculate number of days in month
  idlast = 0
  IF (immo == 1 .AND. idaf > 0) THEN
    idlast = ndays(mo_chk+1) - (idaf - 1)
    IF (leapyr == 1 .AND. mo_chk == 2) idlast = idlast - 1
  ELSE IF (curyr == nbyr .AND. i == idal) THEN
    idlast = i - ndays(mo_chk)
  ELSE
    idlast = ndays(mo_chk+1) - ndays(mo_chk)
    IF (leapyr == 1 .AND. mo_chk == 2) idlast = idlast - 1
  END IF
  
!! calculate average temperature for month in watershed
  IF (idlast > 0.) THEN
    wshdmono(8) = wshdmono(8) / REAL(idlast)
    wshdmono(9) = wshdmono(9) / REAL(idlast)
  ELSE
    wshdmono(8) = 0.
    wshdmono(9) = 0.
  END IF
  
  IF (iprint /= 2 .AND. curyr > nyskip) THEN
    
!! monthly write--output.std
    IF (iscen == 1) THEN
      WRITE (26,6200) mo_chk, wshdmono(1), wshdmono(3), wshdmono(4),  &
          wshdmono(104), wshdmono(5), wshdmono(109),  &
          wshddayo(35), wshdmono(7), wshdmono(108), wshdmono(6),  &
          wshdmono(12), wshdmono(42), wshdmono(45),  &
          wshdmono(46), wshdmono(44), wshdmono(40),  &
          wshdmono(43), wshdmono(41), wshdmono(111)
    ELSE IF (isproj == 1) THEN
      WRITE (19,6200) mo_chk, wshdmono(1), wshdmono(3), wshdmono(4),  &
          wshdmono(104), wshdmono(5), wshdmono(109),  &
          wshddayo(35), wshdmono(7), wshdmono(108), wshdmono(6),  &
          wshdmono(12), wshdmono(42), wshdmono(45),  &
          wshdmono(46), wshdmono(44), wshdmono(40),  &
          wshdmono(43), wshdmono(41), wshdmono(111)
    END IF
    
    IF (iprint == 0) THEN
!! monthly write--pesticide output (output.pst) for HRUs
      DO j = 1, nhru
        IF (hrupest(j) == 1) THEN
          sum = 0.
          DO k = 1, npmx
            sum = sum + hrupstm(k,1,j) + hrupstm(k,2,j)
          END DO
          IF (sum > 0. .AND. iprp == 1) THEN
            WRITE (30,5100) subnum(j), hruno(j), iyr, mo_chk,  &
                (hrupstm(k,1,j), hrupstm(k,2,j), k = 1, npmx)
          END IF
        END IF
      END DO
      
!! monthly write--reservoir output (.rsv)
      DO j = 1, nres
!! monthly write-reservoir file
        IF (iyr > iyres(j) .OR.  &
              (mo_chk >= mores(j) .AND. iyr == iyres(j))) THEN
          IF (iscen == 1 .AND. isproj == 0) THEN
            WRITE (8,5800) j, mo_chk, res_vol(j),  &
                resoutm(1,j) / REAL(idlast), resoutm(2,j) / REAL(idlast),  &
                resoutm(19,j), resoutm(17,j), resoutm(18,j),  &
                resoutm(3,j), resoutm(4,j), resoutm(5,j) / REAL(idlast),  &
                (resoutm(k,j), k = 22, 23), resoutm(38,j) / REAL(idlast),  &
                (resoutm(k,j), k = 24, 25), resoutm(36,j) / REAL(idlast),  &
                (resoutm(k,j), k = 26, 27), resoutm(39,j) / REAL(idlast),  &
                (resoutm(k,j), k = 28, 29), resoutm(40,j) / REAL(idlast),  &
                (resoutm(k,j), k = 30, 31), resoutm(41,j) / REAL(idlast),  &
                (resoutm(k,j), k = 32, 33), resoutm(37,j) / REAL(idlast),  &
                (resoutm(k,j), k = 34, 35), res_seci(j),  &
                (resoutm(k,j), k = 6, 14), resoutm(15,j) / REAL(idlast),  &
                resoutm(16,j) / REAL(idlast)
          ELSE IF (isproj == 1) THEN
            WRITE (22,5800) j, mo_chk, res_vol(j),  &
                resoutm(1,j) / REAL(idlast), resoutm(2,j) / REAL(idlast),  &
                resoutm(19,j), resoutm(17,j), resoutm(18,j),  &
                resoutm(3,j), resoutm(4,j), resoutm(5,j) / REAL(idlast),  &
                (resoutm(k,j), k = 22, 23), resoutm(38,j) / REAL(idlast),  &
                (resoutm(k,j), k = 24, 25), resoutm(36,j) / REAL(idlast),  &
                (resoutm(k,j), k = 26, 27), resoutm(39,j) / REAL(idlast),  &
                (resoutm(k,j), k = 28, 29), resoutm(40,j) / REAL(idlast),  &
                (resoutm(k,j), k = 30, 31), resoutm(41,j) / REAL(idlast),  &
                (resoutm(k,j), k = 32, 33), resoutm(37,j) / REAL(idlast),  &
                (resoutm(k,j), k = 34, 35), res_seci(j),  &
                (resoutm(k,j), k = 6, 14), resoutm(15,j) / REAL(idlast),  &
                resoutm(16,j) / REAL(idlast)
          ELSE IF (iscen == 1 .AND. isproj == 2) THEN
            WRITE (8,6800) j, mo_chk, res_vol(j),  &
                resoutm(1,j) / REAL(idlast), resoutm(2,j) / REAL(idlast),  &
                resoutm(19,j), resoutm(17,j), resoutm(18,j),  &
                resoutm(3,j), resoutm(4,j), resoutm(5,j) / REAL(idlast),  &
                (resoutm(k,j), k = 22, 23), resoutm(38,j) / REAL(idlast),  &
                (resoutm(k,j), k = 24, 25), resoutm(36,j) / REAL(idlast),  &
                (resoutm(k,j), k = 26, 27), resoutm(39,j) / REAL(idlast),  &
                (resoutm(k,j), k = 28, 29), resoutm(40,j) / REAL(idlast),  &
                (resoutm(k,j), k = 30, 31), resoutm(41,j) / REAL(idlast),  &
                (resoutm(k,j), k = 32, 33), resoutm(37,j) / REAL(idlast),  &
                (resoutm(k,j), k = 34, 35), res_seci(j),  &
                (resoutm(k,j), k = 6, 14), resoutm(15,j) / REAL(idlast),  &
                resoutm(16,j) / REAL(idlast), iyr
          END IF
        END IF
        
      END DO
      
!! monthly write--HRU output (output.hru)
      CALL hrumon
      
      CALL impndmon
      
!! monthly write--subbasin output (output.sub)
      CALL submon
      
!! monthly write--reach output (.rch)
      IF (idlast > 0) CALL rchmon(idlast)
      
!! monthly write--sediment routing output (.sed)
      IF (idlast > 0) CALL rsedmon(idlast)
      
    END IF
    
  END IF
  
  IF (curyr > nyskip) THEN
!! calculating monthly averages
    wshd_aamon(mo_chk,1) = wshd_aamon(mo_chk,1) + wshdmono(1)
    wshd_aamon(mo_chk,2) = wshd_aamon(mo_chk,2) + wshdmono(39)
    wshd_aamon(mo_chk,3) = wshd_aamon(mo_chk,3) + wshdmono(3)
    wshd_aamon(mo_chk,4) = wshd_aamon(mo_chk,4) + wshdmono(4)
    wshd_aamon(mo_chk,5) = wshd_aamon(mo_chk,5) + wshdmono(6)
    wshd_aamon(mo_chk,6) = wshd_aamon(mo_chk,6) + wshdmono(7)
    wshd_aamon(mo_chk,7) = wshd_aamon(mo_chk,7) + wshdmono(12)
    wshd_aamon(mo_chk,8) = wshd_aamon(mo_chk,8) + wshdmono(108)
    
!! sum annual values
    wshdyro = wshdyro + wshdmono
    wpstyro = wpstyro + wpstmono
    hruyro = hruyro + hrumono
    wtryr = wtryr + wtrmon
    subyro = subyro + submono
    rchyro = rchyro + rchmono
    resouty = resouty + resoutm
    hrupsty = hrupsty + hrupstm
  END IF
  
  wshdmono = 0.
  wpstmono = 0.
  hrumono = 0.
  wtrmon = 0.
  submono = 0.
  rchmono = 0.
  resoutm = 0.
  hrupstm = 0.
  
  CALL writea
END IF

RETURN
5100 FORMAT (1X,a5,a4,1X,i4,1X,i3,1X,250(e16.4,1X))
5200 FORMAT (/,1X,i4,a4,1X,10F12.2)
5300 FORMAT (1X,i4,a4,1X,10F12.2,/)
5800 FORMAT ('RES   ',i8,1X,i4,41E12.4)
6800 FORMAT ('RES   ',i8,1X,i4,41E12.4,1X,i4)
6200 FORMAT (i5,15F8.2,1X,4F8.2)
END SUBROUTINE writem
