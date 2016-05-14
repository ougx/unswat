SUBROUTINE readfig
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the routing information from the watershed configuration
!!     input file (.fig) and calculates the number of subbasins, reaches,
!!     and reservoirs

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     hru_sub(:)   |none          |subbasin in which HRU is located
!!     icodes(:)    |none          |routing command code:
!!                                 |0 = finish       9 = save
!!                                 |1 = subbasin    10 = recday
!!                                 |2 = route       11 = reccnst
!!                                 |3 = routres     12 = structure
!!                                 |4 = transfer    13 = apex
!!                                 |5 = add         14 = saveconc
!!                                 |6 = rechour     15 =
!!                                 |7 = recmon
!!                                 |8 = recyear
!!     ihouts(:)    |none          |For ICODES equal to
!!                                 |0: not used
!!                                 |1,2,3,5,7,8,10,11: hydrograph storage
!!                                 |                     location number
!!                                 |4: departure type
!!                                 |   (1=reach)
!!                                 |   (2=reservoir)
!!                                 |9: hydrograph storage location of data to
!!                                 |   be printed to event file
!!                                 |14:hydrograph storage location of data to
!!                                 |   be printed to saveconc file
!!     inum1s(:)    |none          |For ICODES equal to
!!                                 |0: not used
!!                                 |1: subbasin number
!!                                 |2: reach number
!!                                 |3: reservoir number
!!                                 |4: reach or res # flow is diverted from
!!                                 |5: hydrograph storage location of 1st
!!                                 |   dataset to be added
!!                                 |7,8,9,10,11,14: file number
!!     inum2s(:)    |none          |For ICODES equal to
!!                                 |0,1,7,8,10,11: not used
!!                                 |2,3: inflow hydrograph storage location
!!                                 |4: destination type
!!                                 |   (1=reach)
!!                                 |   (2=reservoir)
!!                                 |5: hydrograph storage location of 2nd
!!                                 |   dataset to be added
!!                                 |9,14:print frequency
!!                                 |   (0=daily)
!!                                 |   (1=hourly)
!!     inum3s(:)    |none          |For ICODES equal to
!!                                 |0,1,2,3,5,7,8,10,11: not used
!!                                 |4: destination number. Reach or
!!                                 |   reservoir receiving water
!!                                 |9: print format
!!                                 |   (0=normal, fixed format)
!!                                 |   (1=txt format for AV interface,recday)
!!     inum4s(:)    |none          |For ICODES equal to
!!                                 |0,2,3,5,7,8,9,10,11: not used
!!                                 |1: GIS code printed to output file
!!                                 |   (optional)
!!                                 |4: rule code governing transfer of water
!!                                 |   (1=fraction transferred out)
!!                                 |   (2=min volume or flow left)
!!                                 |   (3=exact amount transferred)
!!     nrch         |none          |number of reaches in watershed
!!     nres         |none          |number of reservoirs in watershed
!!     rnum1s(:)    |none          |For ICODES equal to
!!                                 |0,1,3,5,9: not used
!!                                 |2: Fraction of flow in channel
!!                                 |4: amount of water transferred (as
!!                                 |   defined by INUM4S)
!!                                 |7,8,10,11: drainage area in square kilometers
!!                                 |   associated with the record file
!!     subgis(:)    |none          |GIS code printed to output files(output.sub)
!!     subtot       |none          |number of subbasins in watershed
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     a            |NA            |comment flag in .fig file(*=comment)
!!     annual_in    |NA            |name of file containing average annual
!!                                 |loadings to reach (fig command 11)
!!     day_in       |NA            |name of file containing daily loadings
!!                                 |to reach (fig command 10)
!!     eof          |none          |end of file flag (=-1 at end of file)
!!     hour_in      |NA            |name of file containing hourly loadings
!!                                 |to reach (fig command 6)
!!     idum         |none          |counter
!!     ii           |none          |counter
!!     lwqfile      |NA            |reservoir water quality file names (.lwq)
!!     month_in     |NA            |name of file containing monthly loadings
!!                                 |to reach (fig command 7)
!!     resfile      |NA            |reservoir file names (.res)
!!     rtefile      |NA            |reach input file (.rte)
!!     swqfile      |NA            |stream water quality file (.swq)
!!     titldum      |NA            |description line
!!     year_in      |NA            |name of file containing annual loadings
!!                                 |to reach (fig command 8)
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!     Intrinsic: Max
!!     SWAT: caps

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=80) :: titldum
CHARACTER (LEN=1) ::  a
CHARACTER (LEN=13) :: month_in, day_in, annual_in, year_in
CHARACTER (LEN=13) :: apex_in
CHARACTER (LEN=13) :: hour_in, resfile, lwqfile, rtefile, swqfile
CHARACTER (LEN=13) :: subfile, auto_in , rufile
INTEGER :: ii, eof

CHARACTER (LEN=3), DIMENSION (mhyd) :: char6, char7, char8
char6 = "   "
char7 = "   "
char8 = "   "

!!    initialize variables
mhyd_bsn = 0
idum = 0
eof = 0

DO
  a = ""
  READ (102,5002,IOSTAT=eof) a
  IF (eof < 0) EXIT
  IF (a /= "*") THEN
    BACKSPACE 102
    idum = idum + 1
    
!!    CEAP project
    IF (isproj == 2) THEN
      READ (102,5003) a, icodes(idum), ihouts(idum), inum1s(idum),  &
          inum2s(idum), inum3s(idum), rnum1s(idum), inum4s(idum)
    ELSE
      READ (102,5000) a, icodes(idum), ihouts(idum), inum1s(idum),  &
          inum2s(idum), inum3s(idum), rnum1s(idum), inum4s(idum),
!!    &    inum5s(idum), inum6s(idum), inum7s(idum), inum8s(idum)  &
      inum5s(idum), char6(idum), char7(idum), char8(idum)
    END IF
    mhyd_bsn = mhyd_bsn + 1
    
!!!!!! inum6s, inum7s and inum8s (integer) read in as char6, char7, char8 (character) and
!!!!!! converted back to integer due to "Subbasin:" included in the .fig file by ArcSWAT
    jjii = 1     !! inum6s/inum7s
    iijj = 0     !! inum8s
    
!     if (char6(idum) == "Sub") inum6s = 0
!     if (char7(idum) == "bas") inum7s = 0
!     if (char8(idum) == "in:") inum8s = 0
    
    IF (char6(idum) == "  1")  THEN
      xyz = 0.
      WRITE (char6(idum), FMT=' (i3)') jjii
      inum6s(idum) = jjii
    ELSE
      inum6s(idum) = 0
    END IF
    
    IF (char7(idum) == "  1")  THEN
      xyz = 0.
      WRITE (char7(idum), FMT=' (i3)') jjii
      inum7s(idum) = jjii
    ELSE
      inum7s(idum) = 0
    END IF
    
    IF (char8(idum) == "  0")  THEN
      xyz = 0.
      WRITE (char8(idum), FMT=' (i3)') iijj
      inum8s(idum) = iijj
    ELSE
      inum8s(idum) = 1
    END IF
!!!!!! end convert code
    
    select case(icodes(idum))
    
    case (0)  !! icode = 0  FINISH command
    EXIT
    
    case (1)  !! icode = 1  SUBBASIN command
    subtot = subtot + 1
    subgis(inum1s(idum)) = inum4s(idum)
    subfile = ""
    READ (102,5100) subfile
    CALL caps(subfile)
    i = 0
    i = inum1s(idum)
    subed(ihouts(idum)) = inum4s(idum)
    OPEN (101,FILE=subfile)
    CALL readsub
    nhru = nhru + hrutot(i)
    
    case (2)  !! icode = 2  ROUTE CHANNEL command
    nrch = nrch + 1
!!            assume subbasin is the same number as the reach (if zero)
    IF (inum3s(idum) == 0) THEN
      inum3s(idum) = inum1s(idum)
    END IF
    rtefile = ""
    swqfile = ""
    READ (102,5100) rtefile, swqfile
    CALL caps(rtefile)
    CALL caps(swqfile)
    irch = 0
    irch = inum1s(idum)
    OPEN (103,FILE=rtefile)
    OPEN (104,FILE=swqfile)
    CALL readrte
    CALL readswq
    
    case (3)  !! icode = 3  ROUTE RESERVOIR command
    nres = nres + 1
    resfile = ""
    lwqfile = ""
    READ (102,5100) resfile, lwqfile
    CALL caps(resfile)
    CALL caps(lwqfile)
    i = 0
    i = inum1s(idum)
    OPEN (105,FILE=resfile)
    CALL readres
    IF (lwqfile /= '             ') THEN
      OPEN (106,FILE=lwqfile)
      CALL readlwq
    END IF
!! lake water quality default values
    CALL lwqdef
    
    case (4)  !! icode = 4  TRANSFER command: read in beg/end month
    READ (102,5004) mo_transb(inum5s(idum)),  &
        mo_transe(inum5s(idum)), ih_tran(inum5s(idum))
    
    
    case (6)  !! icode = 6  RECHOUR command: read in hourly values
!! with water in m^3 and rest in tons/kgs
    hour_in = ""
    READ (102,5100) hour_in
    CALL caps(hour_in)
    OPEN (200+inum1s(idum),FILE=hour_in,RECL=350)
    DO ii = 1, 6
      READ (200+inum1s(idum),5200) titldum
    END DO
    
    case (7)  !! icode = 7  RECMON command:
!!  read in monthly values
    month_in = ""
    READ (102,5100) month_in
    recmonps(ihouts(idum)) = month_in(1:INDEX(month_in,'.')-1)
    CALL caps(month_in)
    i = 0
    i = inum1s(idum)
    OPEN (107,FILE=month_in,RECL=350)
    CALL readmon
    
    case (8)  !! icode = 8  RECYEAR command:
!! read in average daily loadings for each year
    year_in = ""
    READ (102,5100) year_in
    
    CALL caps(year_in)
    i = 0
    i = inum1s(idum)
    OPEN (108,FILE=year_in,RECL=350)
    CALL readyr
    
    CLOSE (108)
    
    case (9)  !! icode = 9  SAVE command: saves daily reach
!! constituent masses from a hydrograph node on
!! the channel network to an output file
    day_in = ""
    READ (102,5100) day_in
    CALL caps(day_in)
    IF (inum1s(idum) <= 10 .AND. inum1s(idum) > 0) THEN
      OPEN (40+inum1s(idum),FILE=day_in,RECL=350)
      IF (inum3s(idum) == 0) THEN
        WRITE (40+inum1s(idum),5400) title
        WRITE (40+inum1s(idum),5500)
      ELSE
        iida = 0
        iida = idaf
        CALL xmon
        WRITE (40+inum1s(idum),5501) iyr, i_mo, (iida - ndays(i_mo))
      END IF
    END IF
    
    case (10) !! icode = 10  RECDAY command: read in daily values
!! with water in cms and rest in tons
    day_in = ""
    READ (102,5100) day_in
    CALL caps(day_in)
    OPEN (555+inum1s(idum),FILE=day_in,RECL=350)
    DO ii = 1, 6
      READ (555+inum1s(idum),5200) titldum
    END DO
    
    case (11) !! icode = 11  RECCNST command: read in average
!! annual values with water in m^3, sed in t, and
!! the nutrients in kg
    annual_in = ""
    READ (102,5100) annual_in
    reccnstps(ihouts(idum))=annual_in(1:INDEX(annual_in,'.')-1)
    CALL caps(annual_in)
    i = 0
    i = inum1s(idum)
    OPEN (109,FILE=annual_in,RECL=350)
    CALL readcnst
    
!! code to read from apex output file
    case (13)
    apex_in = ""
    READ (102,5100) apex_in
    CALL caps(apex_in)
!      i = 0
!      i = inum1s(idum)
    OPEN (112+inum1s(idum),FILE=apex_in,RECL=350)
    DO ii = 1, 9
      READ (112+inum1s(idum),5200) titldum
    END DO
!! code to read from apex output file
    
    
    case (14) !! icode = 14 SAVECONC command: saves hourly or
!! daily reach constituent concentrations to an
!! output file from a hydrograph node on the
!! channel network
    day_in = ""
    READ (102,5100) day_in
    CALL caps(day_in)
    IF (inum1s(idum) <= 50 .AND. inum1s(idum) > 0) THEN
      OPEN (50+inum1s(idum),FILE=day_in,RECL=350)
      WRITE (50+inum1s(idum),5400) title
      WRITE (50+inum1s(idum),5600)
    END IF
    
    case (17)  !! icode = 17  ROUTING UNIT command
    rufile = ""
    READ (102,5100) rufile
    CALL caps(rufile)
    iru = inum1s(idum)
    isub = inum2s(idum)
    daru_km(isub,iru) = rnum1s(idum)
    OPEN (113,FILE=rufile)
    CALL readru
    CLOSE(113)
    
    case (18)  !! icode = 18  LANDSCAPE ROUTING command
!!if (rnum1s(idum) < 1.e-9) rnum1s(idum) = 1.
    
  END select
  
!! calculate upstream drainage area (km2) and impervious cover (km2)
!! in the drainage arae at each subbasin outlet
  IF (icodes(idum)==1) THEN      !subbasin
    subdr_km(ihouts(idum)) = sub_km(inum1s(idum))
  ELSE IF (icodes(idum)==17) THEN !routing unit
    subdr_km(ihouts(idum)) = daru_km(inum2s(idum),inum1s(idum))
  ELSE IF (icodes(idum)==5) THEN  !add
    subdr_km(ihouts(idum)) = subdr_km(inum1s(idum)) + subdr_km(inum2s(idum))
    subdr_ickm(ihouts(idum)) = subdr_ickm(inum1s(idum))  &
        + subdr_ickm(inum2s(idum))
  ELSE IF (icodes(idum)==2) THEN !route
    IF(inum1s(idum)==inum2s(idum)) THEN
      subdr_km(ihouts(idum)) = subdr_km(inum1s(idum))
      subdr_ickm(ihouts(idum)) = subdr_ickm(inum1s(idum))
    ELSE
      subdr_km(ihouts(idum)) = subdr_km(inum1s(idum)) + subdr_km(inum2s(idum))
      subdr_ickm(ihouts(idum)) = subdr_ickm(inum1s(idum))  &
          + subdr_ickm(inum2s(idum))
    END IF
  ELSE IF (icodes(idum)==18) THEN  !routels
    subdr_km(ihouts(idum)) = subdr_km(inum2s(idum))
    ru_a(inum3s(idum),inum1s(idum)) = subdr_km(ihouts(idum)) *
!     &                        daru_km(inum3s(idum),inum1s(idum))) /  &
    100. / ru_ovsl(inum3s(idum),inum1s(idum))
  END IF
  
END IF
END DO

!! close .fig file
CLOSE (102)


RETURN
!! isproj = 0
!! 5000 format (a1,9x,5i6,f6.3,i9,4i3)
5000 FORMAT (a1,9X,5I6,f6.3,i9,i3,3A3)
5001 FORMAT (7X,i3,4X,6F12.3)
5002 FORMAT(a)
5004 FORMAT (10X,3I4)
!! isproj = 2 (CEAP)
5003 FORMAT (a1,9X,4I6,i5,f8.0,i8)
5100 FORMAT (10X,2A13)
5200 FORMAT (a80)
5300 FORMAT (2I6)
5400 FORMAT (20A4)
5500 FORMAT (//," DAY YEAR HR ","  WATER m^3 ","   SED tons ",  &
    "    ORGN kg ","    ORGP kg ","   NO3-N kg ",  &
    "   NH3-N kg ","   NO2-N kg ","    MINP kg ",  &
    "    CBOD kg ","   DISOX kg ","    CHLA kg ",  &
    "  SOLPST mg ","  SORPST mg ","    BACTP # ",  &
    "   BACTLP # ","CMETAL#1 kg ","CMETAL#2 kg ", "CMETAL#3 kg ","  TEMP degC ")
5501 FORMAT (i4,2I2,", Point Discharge File created with Save Comand")
5600 FORMAT (//," Year  Day Step  FLOWm^3/s    SEDmg/L   ORGNmg/L",  &
    "   ORGPmg/L    NO3mg/L    NH3mg/L    NO2mg/L",  &
    "   MINPmg/L   CBODmg/L  DISOXmg/L   CHLAug/L",  &
    " SOLPSTmg/L SORPSTmg/L  BACTPct/L BACTLPct/L",  &
    "CMETAL1mg/LCMETAL2mg/LCMETAL3mg/L   TEMPdegC")
END SUBROUTINE readfig
