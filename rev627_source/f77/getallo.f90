SUBROUTINE getallo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of HRUs, subbasins, etc. in the
!!   simulation. These values are used to allocate array sizes.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none        |maximum number of applications
!!    mch         |none        |maximum number of channels
!!    mcr         |none        |maximum number of crops grown per year
!!    mcrdb       |none        |max number of lu/lc defined in crop.dat
!!    mcut        |none        |maximum number of cuttings per year
!!    mfcst       |none        |maximum number of forecast stations
!!    mfdb        |none        |max number of fertilizers in fert.dat
!!    mgr         |none        |maximum number of grazings per year
!!    mhru        |none        |maximum number of HRUs in watershed
!!    mhyd        |none        |maximum number of hydrograph nodes
!!    mlyr        |none        |maximum number of soil layers
!!    mnr         |none        |max number of years of rotation
!!    mpst        |none        |max number of pesticides used in wshed
!!    mpdb        |none        |max number of pesticides in pest.dat
!!    mrecc       |none        |maximum number of reccnst files
!!    mrecd       |none        |maximum number of recday files
!!    mrech       |none        |maximum number of rechour files
!!    mrecm       |none        |maximum number of recmon files
!!    mrecy       |none        |maximum number of recyear files
!!    mres        |none        |maximum number of reservoirs
!!    mrg         |none        |max number of rainfall/temp gages
!!    nstep       |none        |max number of time steps per day
!!    msub        |none        |maximum number of subbasins
!!    mtil        |none        |max number of tillage types in till.dat
!!    mudb        |none        |maximum number of urban land types in urban.dat
!!    myr         |none        |max number of years of simulation
!!    pstflg(:)   |none        |flag for types of pesticide used in watershed
!!                             |array location is pesticide ID number
!!                             |0: pesticide not used
!!                             |1: pesticide used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a           |NA          |comment flag
!!    plantdb     |NA          |name of LU/LC database input file (crop.dat)
!!    eof         |none        |end of file flag
!!    fcstfile    |NA          |name of weather forecast input file (.cst)
!!    fcsttot     |none        |total number of forecast regions in database
!!    fertdb      |NA          |name of fertilizer database file (fert.dat)
!!    figfile     |NA          |name of watershed configuration file (.fig)
!!    i           |none        |counter
!!    ic          |none        |number of land cover in crop database
!!    icd         |none        |routing command code (.fig)
!!    ifcst       |none        |number of forecast region in database file
!!    ifnum       |none        |number of fertilizer type in database file
!!    iht         |none        |hydrograph storage location number (.fig)
!!    inm1        |none        |1st routing command variable (.fig)
!!    inm2        |none        |2nd routing command variable (.fig)
!!    inm3        |none        |3rd routing command variable (.fig)
!!                             |if icd=1, inm3=subbasin #
!!    ipnum       |none        |number of pesticide type in database file
!!    itnum       |none        |number of tillage implement in database file
!!    iunum       |none        |number of urban land type in database file
!!    j           |none        |counter
!!    nhtot       |none        |number of relative humidity records in file
!!    nrgage      |none        |number of raingage files
!!    nrgfil      |none        |number of rain gages per file
!!    nrtot       |none        |total number of rain gages
!!    nsave       |none        |number of save commands in .fig file
!!    nstot       |none        |number of solar radiation records in file
!!    ntgage      |none        |number of temperature gage files
!!    ntgfil      |none        |number of temperature gages per file
!!    nttot       |none        |total number of temperature gages
!!    numhru      |none        |number of HRUs listed in subbasin file
!!    nwtot       |none        |number of wind speed records in file
!!    pestdb      |NA          |name of pesticide database input file(pest.dat)
!!    subfile     |NA          |name of subbasin input file (.sub)
!!    tilldb      |NA          |name of tillage database input file(till.dat)
!!    title       |NA          |description lines in file.cio(1st 3 lines)
!!    titldum     |NA          |variable to read in data line
!!    urbandb     |NA          |name of urban land type database file
!!                             |(urban.dat)
!!    septdb      !  NA        ! name of septic tank database file
!!                             |(septwq1.dat)  !!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=13) :: urbandb, plantdb, tilldb, pestdb, figfile,  &
    fertdb, subfile, fcstfile, bsnfile
CHARACTER (LEN=1) ::  a
CHARACTER (LEN=80) ::  titldum
INTEGER :: icd, inm1, inm2, inm3, iht, eof, numhru, ic
INTEGER :: ipnum, ifnum, iunum, itnum, j, ifcst, fcsttot, k
!     septic database
INTEGER  :: isnum   !! CS

!!    initialize variables
title = ""
plantdb = ""
tilldb = ""
pestdb = ""
fertdb = ""
urbandb = ""
figfile = ""
bsnfile = ""
!     septic database file
septdb = ""
nrgage = 0
ntgage = 0
nrtot = 0
nttot = 0
nrgfil = 0
ntgfil = 0
nstot = 0
nhtot = 0
nwtot = 0
nstep = 0
myr = 0

OPEN (23,FILE="file.cio")
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,5100) title
READ (23,6000) titldum
READ (23,5000) figfile
READ (23,*) myr
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,*) nstep
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,*) nrgage
READ (23,*) nrtot
READ (23,*) nrgfil
READ (23,6000) titldum
READ (23,*) ntgage
READ (23,*) nttot
READ (23,*) ntgfil
READ (23,6000) titldum
READ (23,*) nstot
READ (23,6000) titldum
READ (23,*) nhtot
READ (23,6000) titldum
READ (23,*) nwtot
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,6000) titldum
READ (23,5000) fcstfile
READ (23,6000) titldum
READ (23,5000) bsnfile
READ (23,6000) titldum
READ (23,5000) plantdb
READ (23,5000) tilldb
READ (23,5000) pestdb
READ (23,5000) fertdb
READ (23,5000) urbandb


!  septic database file
DO nlines = 1, 24
  READ (23,6000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
END DO

!      do
READ (23,5000,IOSTAT=eof) septdb   !! CS

!!    added for binary files 3/25/09 gsm
!!    ia_b  print ascii or binary files
!!       0 for ascii file
!!       1 for binary file!!    added for
READ (23, *, IOSTAT=eof) ia_b

CLOSE (23)
!! calculate max number of years simulated, daily time increment
myr = myr + 2
IF (nstep <= 0) THEN
  nstep = 1
ELSE
  nstep = 1440 / nstep
END IF
nstep = nstep + 1

CALL caps(plantdb)
CALL caps(fertdb)
CALL caps(pestdb)
CALL caps(figfile)
CALL caps(tilldb)
CALL caps(urbandb)
!     septic database
CALL caps(septdb)  !! CS

!! open .bsn file to get ievent input
OPEN (103,FILE=bsnfile)
CALL caps (bsnfile)
DO nlines = 1, 17
  READ (103,6000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
END DO
READ (103,*,IOSTAT=eof) ievent
IF (ievent == 1) nstep = 24
CLOSE (103)


!!    open routing file



!!    initialize variables
a = ""
icd = 1
iht = 0
inm1 = 0
inm2 = 0
inm3 = 0
mhru = 0
mch = 1
mru = 1
msub = 0
mhyd = 1
mres = 0
mlyr = 0
mpst = 0
mcr = 0
mapp = 0
mgr = 0
mcut = 0
mnr = 0
mapex = 0
mrecc = 0
mrecd = 0
mrech = 0
mrecm = 0
mrecy = 0
mtran = 0
nsave = 0
nlsu = 0

!! calculate number of records in plant growth database
eof = 0
mcrdb = 0
OPEN (29,FILE=plantdb)
DO
  ic = 0
  READ (29,*,IOSTAT=eof) ic
  IF (eof < 0) EXIT
  READ (29,6000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (29,6000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (29,6000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  READ (29,6000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  mcrdb = MAX(mcrdb,ic)
END DO
CLOSE (29)
IF (mcrdb <= 0) mcrdb = 1

!! calculate number of records in urban database
eof = 0
mudb = 0
OPEN (8,FILE=urbandb)
DO
  iunum = 0
  READ (8,6200,IOSTAT=eof) iunum
  IF (eof < 0) EXIT
  READ (8,6000,IOSTAT=eof) titldum
  IF (eof < 0) EXIT
  mudb = MAX(mudb,iunum)
END DO
CLOSE (8)
IF (mudb <= 0) mudb = 1

!!     calculate number of records in septic tank database !! CS

eof = 0
msdb = 0
!!    read title lines from septic database file
!     septic database
IF (septdb /= '             ') THEN
  OPEN (171,FILE=septdb) !! CS
  DO jj = 1,4
    READ (171,6000) titldum
  END DO
  DO
    isnum = 0
    READ (171,6200,IOSTAT=eof) isnum  !!
    IF (eof < 0) EXIT
    READ (171,6000,IOSTAT=eof) titldum  !!
    READ (171,6000,IOSTAT=eof) titldum  !!
    IF (eof < 0) EXIT
    msdb = MAX(msdb,isnum)
  END DO
  IF (msdb <= 0) msdb = 1
  CLOSE (171)
END IF

!! calculate number of records in fertilizer database
eof = 0
mfdb = 0
OPEN (7,FILE=fertdb)
DO
  ifnum = 0
  READ (7,6300,IOSTAT=eof) ifnum
  IF (eof < 0) EXIT
  mfdb = MAX(mfdb,ifnum)
END DO
IF (mfdb <= 0) mfdb = 1
CLOSE (7)

!! calculate number of records in pesticide database
eof = 0
mpdb = 0
OPEN (31,FILE=pestdb)
DO
  ipnum = 0
  READ (31,6200,IOSTAT=eof) ipnum
  IF (eof < 0) EXIT
  mpdb = MAX(mpdb,ipnum)
END DO
CLOSE (31)
IF (mpdb <= 0) mpdb = 1

!! calculate number of records in tillage database
eof = 0
mtil = 0
OPEN (30,FILE=tilldb)
DO
  itnum = 0
  READ (30,6300,IOSTAT=eof) itnum
  IF (eof < 0) EXIT
  mtil = MAX(mtil,itnum)
END DO
IF (mtil <= 0) mtil = 1
CLOSE (30)


!! process .fig file
allocate (pstflg(mpdb))
pstflg = 0
mhru1 = 1
OPEN (27,FILE=figfile)
DO WHILE (icd > 0)
  READ (27,5002) a
  IF (a /= "*") THEN
    BACKSPACE 27
    
    READ (27,5001) a, icd, iht, inm1, inm2, inm3
    
    select case (icd)
    case (1)                      !! icd = 1  SUBBASIN command
    msub = msub + 1             !! # subbasins
!! calculate total number of HRUs in watershed
    subfile = ""
    numhru = 0
    READ (27,6100) subfile
    CALL caps(subfile)
    OPEN (25,FILE=subfile)
    DO j = 1,52
      READ (25,6000) titldum
    END DO
    READ (25,*) numhru
    mhru = mhru + numhru
    DO j = 1, 8
      READ (25,6000) titldum
    END DO
    CALL hruallo
    mhru1 = mhru + 1
    CLOSE (25)
    case (2)                      !! icd = 2  ROUTE command
    mch = mch + 1               !! # channels
    READ (27,5002) a
    case (3)                      !! icd = 3  ROUTE RESERVOIR command
    mres = mres + 1
    READ (27,5002) a
    case (4)
    READ (27,5002) a             !! icd = 4  TRANSFER command
    mtran = mtran + 1
    case (6)                      !! icd = 6  RECALL HOUR command
    READ (27,5002) a
    mrech = mrech + 1
    mrech = MAX(mrech,inm1)
    case (7)                      !! icd = 7  RECALL MONTH command
    READ (27,5002) a
    mrecm = mrecm + 1
    mrecm = MAX(mrecm,inm1)
    case (8)                      !! icd = 8  RECALL YEAR command
    READ (27,5002) a
    mrecy = mrecy + 1
    mrecy = MAX(mrecy,inm1)
    case (9)                      !! icd = 9  SAVE command
    READ (27,5002) a
    nsave = nsave + 1
    case (10)                     !! icd = 10 RECALL DAY command
    READ (27,5002) a
    mrecd = mrecd + 1
    mrecd = MAX(mrecd,inm1)
    case (11)                     !! icd = 11 RECALL CONSTANT command
    READ (27,5002) a
    mrecc = mrecc + 1
    mrecc = MAX(mrecc,inm1)
    case (13)                     !! icd = 13 APEX command
    READ (27,5002) a
    mapex = mapex + 1
    mapex = MAX(mapex,inm1)
    case (14)                     !! icd = 14 SAVECONC command
    READ (27,5002) a
    nsave = nsave + 1
    case (17)                     !! icd = 17 ROUTING UNIT command
    READ (27,5002) a
    mru = mru + 1
  END select
  
  mhyd = MAX(mhyd,iht)
  
END IF
END DO
CLOSE (27)

IF (ils_nofig == 1) THEN
  mru = MAX(mru,2*msub)
END IF

!      mhyd = mhyd + mrecc + mrecd + mrech + mrecm + mrecy + nsave
!     &                                                       + mtran + 1
IF (mhru <= 0) mhru = 1
IF (msub <= 0) msub = 1
IF (mch <= 0) mch = 1
IF (mrecc <= 0) mrecc = 1
IF (mrecd <= 0) mrecd = 1
IF (mrech <= 0) mrech = 1
IF (mrecm <= 0) mrecm = 1
IF (mrecy <= 0) mrecy = 1
IF (mres <= 0) mres = 1

mhyd = mhyd + nsave + mtran + 1

IF (ils_nofig == 1) THEN
  mhyd = mhyd + 6 * msub
END IF

!!    septic change 1-28-09 gsm
mlyr = mlyr + 4
!!    septic change 1-28-09 gsm

mcr = mcr + 1
mcr = MAX(2,mcr)
mapp = mapp + 1
mgr = mgr + 1
mcut = mcut + 1
mnr = mnr + 1
mpst = sum(pstflg) + 1

!! calculate max number of climate gages
mrg = 0
mrg = MAX(nrtot,nttot,nstot,nhtot,nwtot)
IF (mrg <= 0) mrg = 1

!! calculate max number of forecast stations
mfcst = 0
CALL caps(fcstfile)
IF (fcstfile /= '             ') THEN
  fcsttot = 0
  OPEN (12,FILE=fcstfile)
  READ (12,5002,END=99) titldum
  
  READ (12,6400) fcsttot
  DO j = 1, fcsttot
    READ (12,5002) titldum
    READ (12,6400) ifcst
    DO k = 1, 10
      READ (12,5002) titldum
    END DO
    mfcst = MAX(mfcst, ifcst)
  END DO
  mfcst = mfcst + 1
  99       CLOSE (12)
ELSE
  mfcst = 1
END IF

RETURN
5000 FORMAT (6A)
5001 FORMAT (a1,9X,5I6)
5002 FORMAT(a)
5100 FORMAT (20A4)
!$$$$$$  5200 format (10i4)
6000 FORMAT (a80)
6100 FORMAT (10X,a13)
6200 FORMAT (i3)
6300 FORMAT (i4)
6400 FORMAT (i6)
END SUBROUTINE getallo
