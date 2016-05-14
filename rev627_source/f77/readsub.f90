SUBROUTINE readsub
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:03

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin general input file
!!    (.sub). This file contains data related to general processes modeled
!!    at the HRU/subbasin level.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |subbasin number
!!    da_km       |km2           |area of the watershed in square kilometers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_k(1,:)   |mm/hr         |effective hydraulic conductivity of tributary
!!                               |channel alluvium
!!    ch_l(1,:)   |km            |longest tributary channel length in subbasin
!!    ch_n(1,:)   |none          |Manning's "n" value for the tributary channels
!!    ch_s(1,:)   |m/m           |average slope of tributary channels
!!    ch_w(1,:)   |m             |average width of tributary channels
!!    cncoef_sub  |              |soil water depletion coefficient used
!!                               | in the new (modified curve number method)
!!                               | same as soil index coeff used in APEX
!!                               | range: 0.5 - 2.0
!!    co2(:)      |ppmv          |CO2 concentration
!!    driftco(:)  |none          |coefficient for pesticide drift directly
!!                               |onto stream
!!    elevb(:,:)  |m             |elevation at the center of the band
!!    elevb_fr(:,:)|none         |fraction of subbasin area within elevation
!!                               |band (the same fractions should be listed for
!!                               |all HRUs within the subbasin)
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    esco(:)     |none          |soil evaporation compensation factor (0-1)
!!    harg_petco(:)              |coefficient related to radiation used
!!                               | in hargreaves eq (range: 0.0019 - 0.0032)
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    huminc(:,:) |none          |monthly humidity adjustment. Daily values
!!                               |for relative humidity within the month are
!!                               |rasied or lowered by the specified amount.
!!                               |(used in climate change studies)
!!    ifld(:)     |none          |number of HRU (in subbasin) that is a
!!                               |floodplain
!!    ihgage(:)    |none         |subbasin relative humidity data code
!!    ipot(:)     |none          |number of HRU (in subbasin) that is ponding
!!                               |water--the HRU that the surface runoff from
!!                               |current HRU drains into. This variable is
!!                               |used only for rice paddys or closed
!!                               |depressional areas
!!    irgage(:)    |none         |subbasin rain gage data code
!!    irip(:)     |none          |number of HRU (in subbasin) that is a
!!                               |riparian zone
!!    isgage(:)    |none         |subbasin radiation gage data code
!!    itgage(:)    |none         |subbasin temp gage data code
!!    iwgage(:)    |none         |subbasin wind speed gage data code
!!    plaps(:)     |mm H2O/km    |precipitation lapse rate: precipitation
!!                               |change due to change in elevation
!!    radinc(:,:) |MJ/m^2        |monthly solar radiation adjustment. Daily
!!                               |radiation within the month is raised or
!!                               |lowered by the specified amount. (used in
!!                               |climate change studies)
!!    rfinc(:,:)  |%             |monthly rainfall adjustment. Daily rainfall
!!                               |within the month is adjusted to the specified
!!                               |percentage of the original value (used in
!!                               |climate change studies)
!!    sub_smfmx(:)|mm/deg C/day  |max melt rate for snow during year (June 21)
!!                               | for subbasin(:)
!!                               | where deg C refers to the air temperature
!!                               | SUB_SMFMX and SMFMN allow the rate of snow
!!                               | melt to vary through the year.  These
!!                               | parameters are accounting for the impact
!!                               | of soil temperature on snow melt.
!!                               | (range: -5.0/5.0)
!!    sub_smfmn(:)|mm/deg C/day  |min melt rate for snow during year (Dec 21)
!!                                 for subbasin(:)
!!                               | (range: -5.0/5.0)
!!                               | where deg C refers to the air temperature
!!    sub_sftmp(:)|deg C         |Snowfall temperature for subbasin(:)
!!                               | Mean air temperature at which precip is
!!                               | equally likely to be rain as snow/freezing
!!                               | rain (range: -5.0/5.0)
!!    sub_smtmp(:)|deg C         |Snow melt base temperature for subbasin(:)
!!                               | mean air temperature at which snow melt will
!!                               | occur (range: -5.0/5.0)
!!    sno_hru(:)  |mm H2O        |amount of water stored as snow
!!    snoeb(:,:)  |mm H2O        |initial snow water content in elevation band
!!    sub_elev(:) |m             |average elevation of subbasin
!!    sub_km(:)   |km**2         |area of subbasin in square kilometers
!!    sub_lat(:)  |degrees       |latitude of subbasin
!!    tlaps(:)    |deg C/km      |temperature lapse rate: temperature change
!!                               |due to change in elevation
!!    tmpinc(:,:) |deg C         |monthly temperature adjustment. Daily maximum
!!                               |and minimum temperatures within the month are
!!                               |raised or lowered by the specified amount
!!                               |(used in climate change studies)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    chmfile     |NA            |HRU soil chemical data
!!    eof         |none          |end of file flag (=-1 if eof, else =0)
!!    gwfile      |NA            |HRU groundwater data
!!    hrufile     |NA            |name of HRU general data file
!!    ltcfile     |NA            |name of land transport capacity input file
!!    opsfile     |NA            |name of operation schedule file for Phil G.
!!    gsm 7/24/08 for tile drainage
!!    sdrfile     |NA            |name of subbasin drainage file
!!    if          |none          |number of HRU in subbasin that is floodplain
!!    ip          |none          |number of HRU in subbasin that is pothole
!!    ir          |none          |number of HRU in subbasin that is riparian zone
!!    j           |none          |counter
!!    jj          |none          |variable to take special HRUs into account
!!    mgtfile     |NA            |HRU management data
!!    mon         |none          |monthly counter
!!    pndfile     |NA            |subbasin impoundment file (.pnd)
!!    septfile    |NA            |septic input data file (.sep)
!!    solfile     |NA            |HRU soil data
!!    sumebfr     |none          |total area of subbasin modeled in
!!                               |elevation bands
!!    titldum     |NA            |title line of .sub file (not used)
!!    wgnfile     |NA            |name of weather generator data file
!!    wusfile     |NA            |subbasin water use file (.wus)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: caps, readhru, readmgt, readchm, readsol, readgw

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum, snofile
CHARACTER (LEN=13) :: hrufile, chmfile, mgtfile, solfile, gwfile
CHARACTER (LEN=13) :: opsfile, wgnfile, pndfile, wusfile, septfile
CHARACTER (LEN=13) :: sdrfile, ltcfile
INTEGER :: eof, mon, j, jj, ip, IF, ir
REAL :: ssnoeb(10), sno_sub, ch_ls, sumebfr

wgnfile = ""
pndfile = ""
wusfile = ""
eof = 0
ssnoeb = 0.
sno_sub = 0.
ch_ls = 0.
jj = 1
ip = 0
IF = 0
ir = 0

READ (101,5100) titldum
READ (101,*) sub_km(i)
IF (isproj == 2) THEN
  READ (101,5101) harg_petco(i), cncoef_sub(i), sub_smfmx(1,i),  &
      sub_smfmn(1,i), sub_sftmp(1,i), sub_smtmp(1,i), sub_timp(1,i)
  DO ib = 2, 10
    sub_smfmx(ib,i) = sub_smfmx(1,i)
    sub_smfmn(ib,i) = sub_smfmn(1,i)
    sub_sftmp(ib,i) = sub_sftmp(1,i)
    sub_smtmp(ib,i) = sub_smtmp(1,i)
    sub_timp(ib,i) = sub_timp(1,i)
  END DO
ELSE
  READ (101,5100) titldum
END IF
READ (101,5100) titldum
READ (101,*) sub_lat(i)
READ (101,*) sub_elev(i)
READ (101,*) irgage(i)
READ (101,*) itgage(i)
READ (101,*) isgage(i)
READ (101,*) ihgage(i)
READ (101,*) iwgage(i)
READ (101,5300) wgnfile
CALL caps(wgnfile)
OPEN (114,FILE=wgnfile)
READ (101,*) fcst_reg(i)
READ (101,5100) titldum
READ (101,5100) titldum
READ (101,5200) (elevb(j,i), j = 1, 10)
READ (101,5100) titldum
READ (101,5200) (elevb_fr(j,i), j = 1, 10)
READ (101,5100) titldum
READ (101,5200) (ssnoeb(j), j = 1, 10)
READ (101,*) plaps(i)
READ (101,*) tlaps(i)
READ (101,*) sno_sub
READ (101,5100) titldum
READ (101,*) ch_ls
READ (101,*) ch_s(1,i)
READ (101,*) ch_w(1,i)
READ (101,*) ch_k(1,i)
READ (101,*) ch_n(1,i)
READ (101,5100) titldum
READ (101,5300) pndfile
CALL caps(pndfile)
OPEN (104,FILE=pndfile)
READ (101,5100) titldum
READ (101,5300) wusfile
CALL caps(wusfile)
OPEN (105,FILE=wusfile)
READ (101,5100) snofile
IF(snofile /='             ' .OR. snofile /= 'Climate Change')THEN
  IF (snofile /='             ') THEN
    IF (snofile /= 'Climate Change') THEN
      OPEN (113,FILE=snofile)
      CALL caps (snofile)
      CALL readsno
    END IF
  END IF
END IF
READ (101,*) co2(i)
READ (101,5100) titldum
READ (101,5200) (rfinc(i,mon),mon = 1,6)
READ (101,5100) titldum
READ (101,5200) (rfinc(i,mon),mon = 7,12)
READ (101,5100) titldum
READ (101,5200) (tmpinc(i,mon),mon = 1,6)
READ (101,5100) titldum
READ (101,5200) (tmpinc(i,mon),mon = 7,12)
READ (101,5100) titldum
READ (101,5200) (radinc(i,mon),mon = 1,6)
READ (101,5100) titldum
READ (101,5200) (radinc(i,mon),mon = 7,12)
READ (101,5100) titldum
READ (101,5200) (huminc(i,mon),mon = 1,6)
READ (101,5100) titldum
READ (101,5200) (huminc(i,mon),mon = 7,12)
READ (101,5100) titldum
!! read HRU input data
READ (101,*) hrutot(i)
READ (101,5100) titldum
READ (101,5100) titldum
READ (101,5100) titldum
READ (101,5100) titldum
READ (101,5100) titldum
READ (101,5100) titldum
READ (101,5100) titldum
!!General HRUs
READ (101,5100) titldum
DO j = jj, hrutot(i)
  ihru = 0
  ihru = nhru + j
  IF (j == 1) hru1(i) = ihru
!         ipot(ihru) = ip
!         ifld(ihru) = if
!         irip(ihru) = ir
  chmfile = ""
  hrufile = ""
  mgtfile = ""
  solfile = ""
  gwfile = ""
  opsfile = ""
  septfile = ""
  sdrfile = ""
  READ (101,5300) hrufile, mgtfile, solfile, chmfile, gwfile,  &
      opsfile, septfile, sdrfile, ils2(ihru)
  CALL caps(hrufile)
  CALL caps(mgtfile)
  CALL caps(solfile)
  CALL caps(chmfile)
  CALL caps(gwfile)
  IF (septfile /='             ') THEN
    CALL caps (septfile)
    OPEN (172,FILE=septfile, STATUS='old')
    isep_hru(ihru) = 1
    CALL readsepticbz
  END IF
  IF (sdrfile /= '             ') THEN
    CALL caps(sdrfile)
    OPEN (112,FILE=sdrfile)
    CALL readsdr
  END IF
  
  OPEN (106,FILE=chmfile)
  OPEN (107,FILE=solfile)
  OPEN (108,FILE=hrufile)
  OPEN (109,FILE=mgtfile)
  OPEN (110,FILE=gwfile)
  CALL readhru
  CALL readchm
  CALL readmgt
  CALL readsol
  CALL readgw
  
!!-------------------OGXinSWAT Begin------------------------------
!!  open and read the extra soil file of each HRU
  IF (ievent>0) THEN
    WRITE (solfile,'(A9,A4)') solfile(1:9), ".usf"
    OPEN(newunit=k,FILE=solfile)
    CALL readsoilcol(k ,ihru)
    CLOSE (k)
  END IF
!!-------------------------End------------------------------------
  
  
  IF (opsfile /= '             ') THEN
    CALL caps(opsfile)
    OPEN (111,FILE=opsfile)
    CALL readops
  END IF
  
! set up variables for landscape routing
!          if (ils_nofig == 1) then
  IF (ils2(ihru) == 0) THEN
    ils = 1
  ELSE
    ils = 2
    ils2flag(i) = 1
  END IF
  daru_km(i,ils) = daru_km(i,ils) + hru_fr(j) * sub_km(i)
!         end if
  
! estimate drainage area for urban distributed bmps in hectares - jaehak
  IF (urblu(ihru)>0) THEN
    kk=1
    bmpdrain(ihru) = 1
    DO WHILE(lu_nodrain(kk) /= "    ")
      IF (urbname(urblu(ihru)) == lu_nodrain(kk)) THEN
        bmpdrain(ihru) = 0
        EXIT
      END IF
      kk = kk + 1
      IF (kk>30) EXIT
    END DO
    IF(bmpdrain(ihru)==1) THEN
      sub_ha_imp(i) = sub_ha_imp(i) + hru_ha(ihru) * fimp(urblu(ihru))
      sub_ha_urb(i) = sub_ha_urb(i) + hru_ha(ihru)
    END IF
  END IF
  
! HRU selection criteria for Irrigation by retention-irrigation basins
  IF (num_ri(i)>0) THEN
    IF(sol_z(sol_nly(ihru),ihru)>300 !!    - soil thickness > 12 inches  &
          .AND.sol_k(1,ihru)>0.76       !    - permeability > 0.03 inches/hr (=0.76mm/hr)  &
          .AND.hru_slp(ihru)<0.1        !!    - hru slope < 10%  &
          .AND.urblu(ihru)>0) THEN !urban LU
      
      ri_luflg(ihru) = 1 !irrigate HRU
    END IF
    
    DO kk=1,num_noirr(i)
      IF (urbname(urblu(ihru)) == ri_nirr(i,kk)) THEN
        ri_luflg(ihru) = 0 !exclude these land uses from irrigation
      END IF
    END DO
    
    IF (ri_luflg(ihru) == 1) THEN
      ri_subkm(i) = ri_subkm(i) + hru_km(ihru)* (1.-fimp(urblu(ihru))) !km2
    END IF
  END IF
  
! estimate impervious cover in the upstream drainage area for on-line bmps
  IF (iurban(ihru) > 0) THEN
    subdr_ickm(i) = subdr_ickm(i) + hru_km(ihru) *  fimp(urblu(ihru))
  END IF
! estimate average Curve Number for the subbasin
  sub_cn2(i) = sub_cn2(i) + cn2(ihru) * hru_fr(ihru)
END DO      ! hru loop

!! set up routing unit fractions for landscape routing
DO j = jj, hrutot(i)
  ihru = nhru + j
  IF (ils2(ihru) == 0) THEN
    ils = 1
  ELSE
    ils = 2
  END IF
  
END DO
IF (ils == 2) THEN
  DO j = jj, hrutot(i)
    hru_rufr(ils,ihru) = hru_fr(ihru) * sub_km(i) / daru_km(i,ils)
  END DO
END IF

!!  routing changes gsm per jga 5/3/2010
!!      irunits = 0
!!      read (101,*,iostat=eof) titldum
!!      read (101,*,iostat=eof) irunits
!!     if (irunits = = 1) then
!!        call readfig_sub
!!      endif

!!    set default values
DO ihru = jj, hrutot(i)
  IF (re(ihru) <= 0.) re(ihru) = re_bsn
  IF (sdrain(ihru) <= 0.) sdrain(ihru) = sdrain_bsn
  IF (drain_co(ihru) <= 0.) drain_co(ihru) = drain_co_bsn
  IF (pc(ihru) <= 0.) pc(ihru) = pc_bsn
  IF (latksatf(ihru) <= 0.) latksatf(ihru) = latksatf_bsn
  IF (sstmaxd(ihru) <= 0.) sstmaxd(ihru) = sstmaxd_bsn
END DO
!     estimate drainage area for urban on-line bmps in square km
!subdr_km(i) = subdr_km(i) + sub_km(i)


!!    set default values
IF (sub_km(i) <= 0.) sub_km(i) = 1.
IF (harg_petco(i) < 1.e-6) harg_petco(i) = .0023
IF (cncoef_sub(i) <= 1.e-6) THEN
  IF (cncoef > 1.e-6) THEN
    cncoef_sub(i) = cncoef
  ELSE
    cncoef_sub(i) = 1.
  END IF
END IF

IF (fcst_reg(i) <= 0.) fcst_reg(i) = 1
IF (co2(i) <= 0.) co2(i) = 330.
IF (ch_s(1,i) <= 0.) ch_s(1,i) = .0001
IF (ch_n(1,i) <= 0.005) ch_n(1,i) = 0.005
IF (ch_n(1,i) >= 0.70) ch_n(1,i) = 0.70
DO ib = 1, 10
  IF (sub_smtmp(ib,i) < 1.e-6) sub_smtmp(ib,i) = smtmp
  IF (sub_sftmp(ib,i) < 1.e-6) sub_sftmp(ib,i) = sftmp
  IF (sub_smfmx(ib,i) < 1.e-6) sub_smfmx(ib,i) = smfmx
  IF (sub_smfmn(ib,i) < 1.e-6) sub_smfmn(ib,i) = smfmn
  IF (sub_timp(ib,i) < 1.e-6) sub_timp(ib,i) = timp
END DO

!!    check elevation band fractions
sumebfr = 0.
DO j = 1, 10
  sumebfr = sumebfr + elevb_fr(j,i)
END DO
IF (sumebfr > 1.e-5) THEN
  IF (sumebfr < .99) WRITE (24,1000) i
END IF

!!    This equation given to us by EPA, in the process of getting reference
sdrift = 0.
sdrift = .01 * (10.**(-.00738 * (7.62 * ch_w(1,i)) - 2.5889) + .2267) / 2.

!! assign subbasin values to HRUs where needed
DO j = 1, hrutot(i)
  ihru = 0
  ihru = nhru + j
  hru_sub(ihru) = i
!!   hru_seq = sequential hru number within the subbasin
  hru_seq(ihru) = j
  hrugis(ihru) = subgis(i)
  DO k = 1, 10
    snoeb(k,ihru) = ssnoeb(k)
  END DO
  sno_hru(ihru) = sno_sub
  ch_l1(ihru) = ch_ls
  driftco(ihru) = sdrift
END DO

!! calculate watershed land area
da_km = da_km + sub_km(i)

!!read in weather generator parameter values
CALL readwgn
plaps(i) = plaps(i) / pcpdays(i)
!!read in subbasin impoundment parameter values
CALL readpnd
!!read in subbasin water use parameter values
CALL readwus

!! sediment delivery ration for the subbasin..... urban modeling by J.Jeong
dratio(i) = 0.42 * sub_km(i) ** -0.125
IF(dratio(i)>0.9) dratio(i) = 0.9

CLOSE (101)
RETURN
1000 FORMAT ('ERROR: Elevation Band Fractions in Subbasin ',i4,  &
    ' do not add up to 100% of subbasin area!')
5100 FORMAT (a)
5101 FORMAT (f8.4,f4.2,5F8.3)
5200 FORMAT (10F8.1)
5300 FORMAT (8A13,i6)
5400 FORMAT (i4,6F8.3)
5500 FORMAT (2I4)
END SUBROUTINE readsub
