SUBROUTINE readfile
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   this subroutine opens the main input and output files and reads watershed
!!   information from the file.cio

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mhruo       |none          |maximum number of variables in output.hru file
!!    mrcho       |none          |maximum number of variables in output.rch file
!!    msubo       |none          |maximum number of variables in output.sub file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    calfile     |NA          |name of file containing calibration parameters
!!    fcstcycles  |none        |number of times forecast period is simulated
!!                             |(using different weather generator seeds each
!!                             |time)
!!    fcstday     |julian date |beginning date of forecast period
!!    fcstyr      |year        |beginning year of forecast period
!!    iclb        |none        |auto-calibration flag
!!    idaf        |julian date |beginning day of simulation
!!    idal        |julian date |ending day of simulation
!!    idg(:)      |none        |array location of random number seed
!!                             |used for a given process
!!    idist       |none        |rainfall distribution code
!!                             |  0 for skewed normal dist
!!                             |  1 for mixed exponential distribution
!!    idt         |minutes     |length of time step used to report
!!                             |precipitation data for sub-daily modeling
!!    igen        |none        |random number generator seed code
!!    ilog        |none        |streamflow print code
!!    iprint      |none        |print code:0=monthly,1=daily,2=annual
!!    ipdhru(:)   |none        |HRUs whose output information will be
!!                             |printed to the output.hru and output.wtr
!!                             |files
!!    ipdvab(:)   |none        |output variable codes for output.sub file
!!    ipdvar(:)   |none        |output variable codes for output.rch file
!!    ipdvas(:)   |none        |output varaible codes for output.hru file
!!    iprp        |none        |print code for output.pst file
!!                             |0 do not print pesticide output
!!                             |1 print pesticide output
!!    isproj      |none        |special project code:
!!                             |1 test rewind (run simulation twice)
!!    itotb       |none        |number of output variables printed
!!                             |(output.sub)
!!    itoth       |none        |number of HRUs printed (output.hru/output.wtr)
!!    itotr       |none        |number of output variables printed (output.rch)
!!    itots       |none        |number of output variables printed (output.hru)
!!    iyr         |year        |beginning year of simulation
!!    nbyr        |none        |number of calendar years simulated
!!    nhtot       |none        |number of relative humidity records in file
!!    nrgage      |none        |number of raingage files
!!    nrgfil      |none        |number of rain gages per file
!!    nrtot       |none        |total number of rain gages
!!    nstep       |none        |number of lines of rainfall data for each
!!                             |day
!!    nstot       |none        |number of solar radiation records in file
!!    ntgage      |none        |number of temperature gage files
!!    ntgfil      |none        |number of temperature gages per file
!!    nttot       |none        |total number of temperature gages
!!    nwtot       |none        |number of wind speed records in file
!!    nyskip      |none        |number of years to not print output
!!    pcpsim      |none        |rainfall input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    rcor        |none        |correction coefficient for generated rainfall
!!                             |to ensure that the annual means for generated
!!                             |and observed values are comparable. (needed
!!                             |only if IDIST=1)
!!    rexp        |none        |value of exponent for mixed exponential
!!                             |rainfall distribution (needed only if
!!                             |IDIST=1)
!!    rfile(:)    |NA          |rainfall file names (.pcp)
!!    rhfile      |NA          |relative humidity file name (.hmd)
!!    rhsim       |none        |relative humidity input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    rndseed(:,:)|none        |random number generator seed
!!    slrfile     |NA          |solar radiation file name (.slr)
!!    slrsim      |none        |solar radiation input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    tfile(:)    |NA          |temperature file names (.tmp)
!!    title       |NA          |description lines in file.cio(1st 3 lines)
!!    tmpsim      |none        |temperature input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    wndfile     |NA          |wind speed file name (.wnd)
!!    wndsim      |none        |wind speed input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bsnfile     |NA          |name of basin input file (.bsn)
!!    fcstfile    |NA          |name of weather forecast data file (.cst
!!    fertdb      |NA          |name of fertilizer database file (fert.dat)
!!    figfile     |NA          |name of watershed configuration file (.fig)
!!    ii          |none        |counter
!!    j           |none        |counter
!!    pestdb      |NA          |name of pesticide database input file(pest.dat)
!!    plantdb     |NA          |name of LU/LC database input file (crop.dat)
!!    rn          |none        |random number generator seed
!!    sumv        |none        |variable to hold intermediate calculation
!!    tilldb      |NA          |name of tillage database input file(till.dat)
!!    urbandb     |NA          |name of urban database file (urban.dat)
!!    xx          |none        |random number between 0.0 and 1.0
!!    septdb      |none        |name of pesticide database input file(septwq.dat) !! CS
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: gcycl, caps, Aunif
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

CHARACTER (LEN=13) :: figfile, bsnfile, plantdb, tilldb, urbandb,  &
    pestdb, fertdb, fcstfile

CHARACTER (LEN=80) :: titldum
REAL :: sumv, xx
INTEGER :: rn, j, ii, eof
eof = 0

bsnfile = ""
fcstfile = ""
plantdb = ""
fertdb = ""
pestdb = ""
figfile = ""
tilldb = ""
urbandb = ""
septdb = ""   !!SEPTIC CHANGES GSM 1/30/09

OPEN (101,FILE="file.cio")

!! Read project description
READ (101,5101) titldum
READ (101,5101) titldum
READ (101,5100) title

!! Read general information/watershed configuration
READ (101,5101) titldum
READ (101,5000) figfile
READ (101,*) nbyr
READ (101,*) iyr
READ (101,*) idaf
READ (101,*) idal

CALL caps(figfile)
OPEN (102,FILE=figfile)

!! Read climate information
READ (101,5101) titldum
READ (101,*) igen
READ (101,*) pcpsim
READ (101,*) idt
READ (101,*) idist
READ (101,*) rexp
READ (101,*) nrgage
READ (101,*) nrtot
READ (101,*) nrgfil
READ (101,*) tmpsim
READ (101,*) ntgage
READ (101,*) nttot
READ (101,*) ntgfil
READ (101,*) slrsim
READ (101,*) nstot
READ (101,*) rhsim
READ (101,*) nhtot
READ (101,*) wndsim
READ (101,*) nwtot
READ (101,*) fcstyr
READ (101,*) fcstday
READ (101,*) fcstcycles
READ (101,5101) titldum
READ (101,5000) (rfile(j),j = 1,18)
READ (101,5101) titldum
READ (101,5000) (tfile(j),j = 1,18)
READ (101,5000) slrfile
READ (101,5000) rhfile
READ (101,5000) wndfile
READ (101,5000) fcstfile

!! calculate precipitation data lines per day
IF (idt > 0) nstep = 1440 / idt
!!    added air and soil temperature file for carbon
!!    can be commented if needed by user
!     open (120, file='air_soil.out')
!     write (120,12111)
!12111  format ('  Day','  Hru','  Tmax','   Tmin','   Soil Temp for
!     & Soil Layers')

CALL gcycl

!! calculate values related to exponential rainfall distribution
IF (idist == 1) THEN
  IF (rexp <= 0.) rexp = 1.3
  sumv = 0.
  rn = 0
  rn = rndseed(idg(3),1)
  DO j = 1, 10000
    xx = 0.
    xx = aunif(rn)
    sumv = sumv + (-LOG(xx))**rexp
  END DO
  IF (sumv > 0.) THEN
    rcor = 10100. / sumv
  ELSE
    rcor = 1.
  END IF
END IF

IF (nrgfil <= 0) nrgfil = 10
IF (nrtot <= 0) nrtot = nrgage * nrgfil
IF (ntgfil <= 0) ntgfil = 10
IF (nttot <= 0) nttot = nrgage * ntgfil

!! check for values on forecast variables
IF (fcstyr > 0 .AND. fcstday > 0) THEN
  IF (fcstcycles <= 0) fcstcycles = 1
ELSE
  fcstcycles = 1
END IF

DO j = 1, nrgage
  CALL caps(rfile(j))
END DO
DO j = 1, ntgage
  CALL caps(tfile(j))
END DO
CALL caps(slrfile)
CALL caps(rhfile)
CALL caps(wndfile)

CALL caps(fcstfile)
IF (fcstfile /= '             ') THEN
  OPEN (109,FILE=fcstfile)
ELSE
  fcstyr = 0
  fcstday = 0
END IF

!!Open watershed modeling option file
READ (101,5101) titldum
READ (101,5000) bsnfile

CALL caps(bsnfile)
OPEN (103,FILE=bsnfile)

!!Open database files
READ (101,5101) titldum
READ (101,5000) plantdb
READ (101,5000) tilldb
READ (101,5000) pestdb
READ (101,5000) fertdb
READ (101,5000) urbandb

CALL caps(plantdb)
CALL caps(tilldb)
CALL caps(pestdb)
CALL caps(fertdb)
CALL caps(urbandb)
OPEN (104,FILE=plantdb)
OPEN (105,FILE=tilldb)
OPEN (106,FILE=pestdb)
OPEN (107,FILE=fertdb)
OPEN (108,FILE=urbandb)


!!Special Projects input
READ (101,5101) titldum
READ (101,*) isproj
READ (101,*) iclb
READ (101,5000) calfile

!!Output Information input
READ (101,5101) titldum
READ (101,*) iprint
READ (101,*) nyskip
!!! check that nyskip input is not greater or equal to nbyr input
IF (nyskip >= nbyr) nyskip = nbyr - 1

READ (101,*) ilog
READ (101,*) iprp
READ (101,5101) titldum

!!The user has the option of limiting the number of output
!!variables printed to the output.rch, output.sub and
!!output.hru files. Lines 60-67 of file.cio are used to
!!identify the variables the user wants to print


!!Output variables printed in REACH (output.rch) file

READ (101,5101) titldum
READ (101,*) (ipdvar(ii),ii=1,20)

!!IPDVAR  - Output variables to output.rch file
!![   1] Streamflow into reach (cms)
!![   2] Streamflow out of reach (cms)
!![   3] Loss of water from reach via evaporation (cms)
!![   4] Loss of water from reach via transmission (cms)
!![   5] Sediment entering reach (tons)
!![   6] Sediment transported out of reach (tons)
!![   7] Sediment concentration in reach (mg/kg)
!![   8] Organic N entering reach (kg N)
!![   9] Organic N transported out of reach (kg N)
!![  10] Organic P entering reach (kg P)
!![  11] Organic P transported out of reach (kg P)
!![  12] NO3 entering reach (kg N)
!![  13] NO3 transported out of reach (kg N)
!![  14] NH4 entering reach (kg N)
!![  15] NH4 transported out of reach (kg N)
!![  16] NO2 entering reach (kg N)
!![  17] NO2 transported out of reach (kg N)
!![  18] Soluble P entering reach (kg P)
!![  19] Soluble P transported out of reach (kg P)
!![  20] Chlorophyll-a  entering reach (kg)
!![  21] Chlorophyll-a transported out of reach (kg)
!![  22] CBOD entering reach (kg O2)
!![  23] CBOD transported out of reach (kg O2)
!![  24] Dissolved O2 entering reach (kg O2)
!![  25] Dissolved O2 transported out of reach (kg O2)
!![  26] Soluble pesticide entering reach (mg ai)
!![  27] Soluble pesticide transported out of reach (mg ai)
!![  28] Sorbed pesticide entering reach (mg ai)
!![  29] Sorbed pesticide transported out of reach (mg ai)
!![  30] Loss of pesticide from water by reaction (mg ai)
!![  31] Loss of pesticide from water by volatilization (mg ai)
!![  32] Transfer of pesticide from water to bed sediment by
!!       settling (mg ai)
!![  33] Transfer of pesticide from bed sediment to water by
!!       resuspension (mg ai)
!![  34] Transfer of pesticide between water and bed sediment by
!!       diffusion (mg ai)
!![  35] Loss of pesticide from bed sediment by reaction (mg ai)
!![  36] Loss of pesticide from bed sediment by burial (mg ai)
!![  37] Amount of pesticide in bed sediment (mg ai)
!![  38] Persistent bacteria transported out of reach (#)
!![  39] Less persistent bacteria transported out of reach (#)
!![  40] Conservative metal #1 transported out of reach (kg)
!![  41] Conservative metal #2 transported out of reach (kg)
!![  42] Conservative metal #3 transported out of reach (kg)
!![  43] Total N (org N + no3 + no2 + nh4 outs) to output.rch gsm 10/17/2011
!![  44] Total P (org P + sol p outs)to output.rch gsm 10/17/2011
!![  45] NO3 concentration output.rch (daily only) gsm 10/30/2011


!!Output variables printed in SUBASIN (output.sub) file

READ (101,5101) titldum
READ (101,*) (ipdvab(ii),ii=1,15)

!!IPDVAB  - Output variables to output.sub file
!![   1] Total precipitation falling on subbasin (mm)
!![   2] Snow melt in subbasin (mm)
!![   3] Potential evapotranspiration (mm)
!![   4] Evapotranspiration (mm)
!![   5] Soil water content (mm)
!![   6] Water percolating out of soil profile (mm)
!![   7] Amount of water entering reach from surface runoff (mm)
!![   8] Groundwater discharge into reach from subbasin (mm)
!![   9] Net water contribution to reach from subbasin (mm)
!![  10] Amount of sediment entering reach from subbasin (t/ha)
!![  11] Organic N released to reach from subbasin (kg N/ha)
!![  12] Organic P released to reach from subbasin (kg P/ha)
!![  13] NO3 released to reach from subbasin (kg N/ha)
!![  14] Soluble P released to reach from subbasin (kg P/ha)


!!Output variables printed in HRU (output.hru) file

READ (101,5101) titldum
READ (101,*) (ipdvas(ii),ii=1,20)

!!IPDVAS  - Output variables to output.hru file
!![   1] Total precipitation falling on HRU (mm)
!![   2] Precipitation falling as snow, ice, or freezing rain(mm)
!![   3] Amount of snow or ice melt(mm)
!![   4] Amt of irrigation water applied to HRU (mm)
!![   5] Potential evapotranspiration (mm)
!![   6] Loss of water by evapotranspiration (mm)
!![   7] Soil water content at beginning of day/ave soil water (mm)
!![   8] Soil water content at end of time step(mm)
!![   9] Amt of water percolating past soil zone (mm)
!![  10] Amt of water entering aquifers by percolation (mm)
!![  11] Amt of water entering deep aquifer (mm)
!![  12] Amt of water moving from shallow aquifer to soil zone(mm)
!![  13] Amt of water removed from shallow aquifer to irrigate(mm)
!![  14] Amt of water removed from deep aquifer to irrigate (mm)
!![  15] Amt of water in shallow groundwater storage (mm)
!![  16] Amt of water in deep groundwater storage (mm)
!![  17] Surface runoff generated in time step (mm)
!![  18] Surface runoff contribution to reach (mm)
!![  19] Loss of water by transmission from stream channels
!!       within HRU-enters shallow aquifer (mm)
!![  20] Lateral flow contribution to reach (mm)
!![  21] Groundwater contribution to reach (mm)
!![  22] Net amt of water contributed by HRU to reach (mm)
!![  23] Curve number
!![  24] Average air temperature (deg C)
!![  25] Average of daily max air temperatures (deg C)
!![  26] Average of daily min air temperatures (deg C)
!![  27] Average soil temperature for time period (deg C)
!![  28] Average daily solar radiation (MJ/m^2)
!![  29] Amount of sediment entering reach from HRU (t/ha)
!![  30] Sediment yield calculated with USLE (t/ha)
!![  31] Amt of N fertilizer applied (kg N/ha)
!![  32] Amt of P fertilizer applied (kg P/ha)
!![  33] Amt of N fertilizer auto-applied (kg N/ha)
!![  34] Amt of P fertilizer auto-applied (kg P/ha)
!![  35] Amt of N applied in grazing operation (kg N/ha)
!![  36] Amt of P applied in grazing operation (kg N/ha)
!![  37] Amt of N applied in continuous fert operation (kg N/ha)
!![  38] Amt of P applied in continuous fert operation (kg P/ha)
!![  39] Amt of N added to soil in rainwater (kg N/ha)
!![  40] Amt of N added to soil via fixation by legumes (kg N/ha)
!![  41] Transformation of N from fresh organic to mineral pool
!!       (kg N/ha)
!![  42] Transformation of N from active organic to mineral pool
!!       (kg N/ha)
!![  43] Transformation of N from active organic to stable organic
!!       pool (kg N/ha)
!![  44] Transformation of P from fresh organic to mineral pool
!!       (kg P/ha)
!![  45] Transformation of P from organic to labile pool (kg P/ha)
!![  46] Transformation of P from labile to active mineral pool
!!       (kg P/ha)
!![  47] Transformation of P from active mineral to stable mineral
!!       pool (kg P/ha)
!![  48] Amt of N removed from soil via denitrification (kg N/ha)
!![  49] Nitrogen uptake by plants (kg N/ha)
!![  50] Phosphorus uptake by plants (kg P/ha)
!![  51] Organic N contributed by HRU to reach (kg N/ha)
!![  52] Organic P contributed by HRU to reach (kg P/ha)
!![  53] Mineral P attached to sediment in surface runoff to
!!       reach (kg P/ha)
!![  54] NO3 contributed by HRU in surface runoff to reach(kgN/ha)
!![  55] NO3 contributed by HRU in lateral flow to reach (kgN/ha)
!![  56] NO3 leached below soil profile (kg N/ha)
!![  57] NO3 contributed by HRU in baseflow to reach(kgN/ha)
!![  58] Soluble P contributed by HRU in surface runoff to
!!       reach (kg P/ha)
!![  59] Soluble P contributed by HRU in baseflow to reach(kgP/ha)
!![  60] Number of water stress days
!![  61] Number of temperature stress days
!![  62] Number of nitrogen stress days
!![  63] Number of phosphorus stress days
!![  64] Total plant biomass (t/ha)
!![  65] Leaf area index
!![  66] Harvested yield (t/ha)
!![  67] Persistent bacteria in surface runoff (count)
!![  68] Less persistent bacteria in surface runoff (count)

!!HRUs printed in HRU (output.hru,output.wtr) files

READ (101,5101) titldum
READ (101,*) (ipdhru(ii),ii=1,20)

!! Atmospheric deposition file (Kannan/Santhi input file)
DO
  READ (101,5101,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (101,5000,IOSTAT=eof) atmofile
  IF (eof < 0) EXIT
  EXIT
END DO

!!   mauro code for printing hourly output file hard wired (hourq.out)
!!   IPHR = 0 no print
!!   IPHR = 1 print file
iphr = 0
READ (101,*,IOSTAT=eof) iphr
!!   code for printing soil storage values by soil layer (output.swr)
!!   ISTO = 0 no print
!!   ISTO = 1 print file
isto = 0
READ (101,*,IOSTAT=eof) isto

!!   code for printing output.sol file (formerly 'output.sol' - now output.snu)
!!   isol = 0 no print
!!   isol = 1 print file
isol = 0
READ (101,*,IOSTAT=eof) isol
IF (isol == 1) THEN
  OPEN (121,FILE='output.snu')
  WRITE (121,12222)
  12222   FORMAT (t25,'SURFACE',t39,'-------  SOIL PROFILE  -------',/,  &
      t8,'DAY',t15,'GISnum',t25,'SOL_RSD',t37,'SOL_P',t48,  &
      'NO3',t57,'ORG_N',t67,'ORG_P',t80,'CN'/,t26, '(t/ha)',t35,'(kg/ha)',t45,  &
      '(kg/ha)',t55,'(kg/ha)',t66,'(kg/ha)')
END IF
!! headwater code (0=do not route; 1=route)
i_subhw = 0
READ (101,*,IOSTAT=eof) i_subhw

!! SEPTIC CHANGES GSM 01/29/09
!!   gsm had to take do off when added ia_b ??? 3/25/09 for binary files
!!      do
READ (101,5000,IOSTAT=eof) septdb
!!      if (eof < 0) exit
CALL caps(septdb)
!! end do

!!    read from readlup (landuse update file)
OPEN (122,FILE='lup.dat')

!!    added for binary files 3/25/09 gsm
!!    ia_b  print ascii or binary files
!!       0 for ascii file
!!       1 for binary file
ia_b = 0
READ (101, *, IOSTAT=eof) ia_b

!!    read code to turn on output.wqr output file
!!      ihumus = 0 (do not print file)
!!      ihumus = 1 (print formerly watqual.out - now output.wql)
READ(101,*,IOSTAT=eof) ihumus


!!   flag for output files named tempvel.out and tempdep.out
!!   this flag will print both files
!!   default is = 0; no print
READ (101,*,IOSTAT=eof) itemp


!!    output by elevation band to (formerly 'snowband.out')
READ (101,*,IOSTAT=eof) isnow
IF (isnow == 1) THEN
  OPEN (115,FILE='output.snw')
  WRITE (115,1010)
END IF


!!   read landuse change file
!     read (101,5000,iostat=eof)  lucfile
!     call caps (lucfile)


!!Set default output variables for REACH, SUBBASIN and HRU files if none
!!were specified

DO ii = 1, 20
  IF (ipdvar(ii) > 0) itotr = itotr + 1
END DO

IF (ipdvar(1) <= 0) THEN  &
!! c ange 42 to 45 for output.rch file gsm 10/30/2011
  DO ii = 1, 46
    ipdvar(ii) = ii
  END DO
  itotr = 46
END IF


DO ii = 1, 15
  IF (ipdvab(ii) > 0) itotb = itotb + 1
END DO

IF (ipdvab(1) <= 0) THEN
  DO ii = 1, msubo
    ipdvab(ii) = ii
  END DO
  itotb = msubo
END IF


DO ii = 1, 20
  IF (ipdvas(ii) > 0) itots = itots + 1
END DO

IF (ipdvas(1) <= 0) THEN
  DO ii = 1, mhruo
    ipdvas(ii) = ii
  END DO
  itots = mhruo
END IF


DO ii = 1, 20
  IF (ipdhru(ii) > 0) itoth = itoth + 1
END DO

IF (ipdhru(1) <= 0) THEN
  DO ii = 1, mhru
!       do ii = 1, mhruo
    ipdhru(ii) = ii
  END DO
  itoth = mhru
!       itoth = mhruo
END IF

!!Open output files
OPEN (24,FILE="input.std")
OPEN (26,FILE="output.std")

OPEN (28,FILE="output.hru",RECL=1500)
IF (ia_b == 1) THEN
  OPEN (33333,FILE="outputb.hru",FORM='unformatted')
END IF
OPEN (30,FILE="output.pst",RECL=600)
OPEN (31,FILE="output.sub",RECL=600)
IF (ia_b == 1) THEN
  OPEN (66666,FILE = "outputb.sub", FORM = 'unformatted')
END IF
OPEN (7,FILE="output.rch",RECL=800)
OPEN (8,FILE="output.rsv",RECL=800)
IF (ia_b == 1) THEN
  OPEN (77777,FILE = "outputb.rch", FORM = 'unformatted')
END IF

!!    sediment routing output file
OPEN (84,FILE="output.sed",RECL=800)
!! write headings to sediment outputfile (output.sed)
WRITE (84,1080)
1080  FORMAT (t8,'RCH',t17,'GIS',t23,'MON',t31,'AREAkm2',  &
    t40,'SED_INtons',t51,'SED_OUTtons',t63,'SAND_INtons',t74,  &
    'SAND_OUTtons',t87,'SILT_INtons',t98,'SILT_OUTtons',t111,  &
    'CLAY_INtons',t122,'CLAY_OUTtons',t135,'SMAG_INtons',t146,  &
    'SMAG_OUTtons',t160,'LAG_INtons',t171,'LAG_OUTtons',t184,  &
    'GRA_INtons',t195,'GRA_OUTtons',t208,'CH_BNKtons',t220,  &
    'CH_BEDtons',t232,'CH_DEPtons',t244,'FP_DEPtons',t259,'TSSmg/L')

! Jaehak, sedimentation-filtration output
OPEN (77778,FILE = "bmp-sedfil.out") !jaehak temp urban print out
WRITE(77778,'(a46)') 'Sed-Fil Basins Configuration'
WRITE(77778,'(a46)') ''   !
!retention-irrigation output
OPEN (77779,FILE = "bmp-ri.out") !jaehak temp urban print out
WRITE(77779,'(a46)') 'Retention-Irrigation Basins Configuration'
WRITE(77779,'(a46)') ''   !


!! srin output file from watqual.f
IF (ihumus ==1) THEN
  OPEN (82,FILE='output.wql')
  WRITE (82,6000)
  6000   FORMAT (18X,'WTEMP(C)',' ALGAE_INppm','  ALGAE_Oppm',  &
      '  ORGN_INppm',' ORGN_OUTppm','   NH4_INppm','  NH4_OUTppm',  &
      '   NO2_INppm','  NO2_OUTppm','   NO3_INppm','  NO3_OUTppm',  &
      '  ORGP_INppm',' ORGP_OUTppm','  SOLP_INppm',' SOLP_OUTppm',  &
      '  CBOD_INppm',' CBOD_OUTppm','   SAT_OXppm',' DISOX_INppm',  &
      '  DISOX_Oppm',' H20VOLUMEm3',' TRVL_TIMEhr')
END IF

!! mauro/jerry whittaker hourly output file
IF (iphr > 0) THEN
  OPEN (83,FILE='hourq.out')
  WRITE (83,6001)
  6001    FORMAT (t29,'TOTAL',/,t27,'WATER YLD',/,  &
      t3,'YEAR',t10,'DAY',t15,'HOUR',t22,'HYD',t29,'(m**3)')
END IF
!! end hourly codes

!!darrell output files added for interface plotting
OPEN (11,FILE='rch.dat')
OPEN (12,FILE='hru.dat')
OPEN (13,FILE='sub.dat')
OPEN (14,FILE='rsv.dat')
!!darrell output files added for interface plotting
OPEN (11123,FILE='hyd.out')
OPEN (16,FILE='chan.deg')
!!    open (17,file='wbl.out')
OPEN (18,FILE='swat.qst')
!! output amount of water stored in the soil layer (formerly 'soilst.out')
IF (isto > 0) THEN
  OPEN (129,FILE='output.swr')
  WRITE (129,5001)
  5001    FORMAT (t20,'Soil Storage (mm)',/,t15,'Layer #',/,t3,'Day',t13,  &
      'HRU',t28,'1',t40,'2',t52,'3',t64,'4',t76,'5',t87,'6',t100,  &
      '7',t112,'8',t124,'9',t135,'10')
END IF


!! Output daily streamflow velocity for each channel (subbasin)
IF (itemp == 1) THEN
  OPEN (141,FILE='output.vel')
  WRITE (141,4999)
  4999     FORMAT(t17,'CH_VEL',/,t3,'Day',t7,'Year',t18,'(m/s)')
  OPEN (142,FILE='output.dep')
  WRITE (142,4998)
  4998    FORMAT(t17,'AVE WATER',/,t3,'Day',t7,'Year',t18,'DEPTH(m)')
END IF

!! Code for output.mgt file
!  0=no print 1=print
READ (101, *,IOSTAT=eof) imgt
IF (imgt==1) THEN
  OPEN (143, FILE="output.mgt", RECL=600)
  WRITE (143,999)
  999      FORMAT(2X,'Sub',2X,'Hru',2X,'Year',3X,'Mon',3X,'Day',3X,  &
      'crop/fert/pest', 4X,  &
      'Operation',4X,'phubase',3X,'phuacc',4X,'sol_sw',4X,'bio_ms',3X,  &
      'sol_rsd',7X,'sol',7X,'sol',5X,'yield',3X,'irr amt',  &
      5X,'amt',5X,'mix eff', 5X,'strsn',  &
      5X,'strsp',3X,'strstmp',5X,'strsw',5X,'strsa',2X,'irrsc',  &
      2X,'irrno',5X,'grain',3X,'biomass',5X,'tuber',3X,'residue',7X,  &
      'nit',6X,'phos',/,111X,  &
      ' sumno3',2X,' sumsolp',23X,'frt-kg',17X,' sum',6X,' sum',6X,  &
      ' sum',6X,' sum',6X,' sum',20X,'yld',6X,'yld',8X,'yld',6X,'yld',  &
      9X,'yld',6X,'yld'/,85X,'mm', 6X,'kg/ha',5X,'kg/ha',5X,  &
      'kg/ha', 5X,'kg/ha',5X, 'kg/ha',5X, 'mm',4X,'or dwfert',3X,  &
      'frac',6X,'fertno3',7X,'nh3',6X,'orgn',6X,'solp',6X,'orgp',19X,  &
      'kg/ha',4X,'kg/ha',6X,'kg/ha',4X,'kg/ha',7X,'kg/ha',5X,'kg/ha',  &
      /,'_______________________________________________________________  &
      __________________________________________________________________  &
      __________________________________________________________________  &
      __________________________________________________________________  &
      ________________________________',/)
END IF

!! Code for output.wtr and output.pot files
! 0 =no print  1 =print
READ (101,*,IOSTAT=eof) iwtr
IF (iwtr == 1) THEN
  OPEN (29,FILE="output.wtr",RECL=800)
! write statement added for Aziz (06/25/09)
  OPEN (125,FILE='output.pot')
  WRITE (125, 1000)
END IF

1000  FORMAT (1X,'SUB',t6,'HRU',t12,'DAY',t17,'YEAR',t26,'VOL-I',t37,  &
    'SA-I',t46,'SPILLO',  &
    t56,'POTSEP',t66,'POTEV',t75,'SOL_SW',t85,'TILE-O',t96,'VOL-F',  &
    t106,'SA-F',/,t27,'(mm)',t37,'(ha)',t47,'(mm)',t57,'(mm)',t67,  &
    '(mm)',t77,'(mm)',t87,'(mm)',t97,'(mm)',t107,'(ha)')

!     code for writing out calendar day or julian day to output.rch, .sub, .hru files
!     icalen = 0 (print julian day) 1 (print month/day/year)
READ (101,*, IOSTAT=eof) icalen
!!!!! if icalen == 1 (print month/day/year) - force iprint to be daily  <--nubz asked srin 06/11/2012
IF (icalen == 1) iprint = 1

IF (isproj == 1) THEN
  OPEN (19,FILE="output2.std")
  OPEN (20,FILE="output2.rch",RECL=600)
  OPEN (21,FILE="output2.hru",RECL=800)
  OPEN (22,FILE="output2.rsv",RECL=800)
END IF

IF (cswat == 1) THEN
  OPEN (100,FILE="cswat_profile.txt",RECL=280)
  WRITE (100,*) 'year',';','day',';','hru',';','cmass',';','sol_rsd',  &
      ';','mancmass'
END IF

!! septic result  J.Jeong Feb2009
OPEN (173,FILE='septic.out')
WRITE(173,5102) 'HRU','YEAR','DAY','Precip', 'PERC',  &
    'sol_ul','sol_st','sol_fc','nh3init','nh3bgn','nh3end',  &
    'no3init','no3bgn','no3end', 'nitrN','denitrN','solpinit',  &
    'solpbgn','solpend','solpconc'
WRITE(173,5102) '#','','','(mm)','(m3)','(mm)',  &
    '(mm)','(mm)','(kg/ha)','(kg/ha)','(kg/ha)','(kg/ha)',  &
    '(kg/ha)','(kg/ha)','(kg/ha)','(kg/ha)','(kg/ha)',  &
    '(kg/ha)','(kg/ha)','(mg/l)'

CLOSE (101)
RETURN

1010 FORMAT (32X,'SNOW(mm) at ELEVATION BAND (1-10)',/,  &
    1X,'DAY','   YR',t14,'GISnum',t28,'1',t36,'2',t44,'3',t52,'4',  &
    t60,'5',t68,'6',t76,'7',t84,'8',t92,'9',t99,'10')
5000 FORMAT (6A)
5100 FORMAT (20A4)
5101 FORMAT (a80)
5102 FORMAT (3A5,30A15)
END SUBROUTINE readfile
