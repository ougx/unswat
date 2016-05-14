SUBROUTINE std1
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes general information to the standard output file
!!    and header lines to miscellaneous output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_km       |km**2         |area of the watershed in square kilometers
!!    icrk        |none          |crack flow code
!!                               |1: compute flow in cracks
!!    ideg        |none          |channel degredation code
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    idg(:)      |none          |array location of random generator seed
!!    idt         |minutes       |length of time step used to report
!!                               |precipitation data for sub-daily modeling
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    igen        |none          |random number generator seed code
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality (QUAL2E)
!!    nbyr        |none          |number of calendar years simulated
!!    pcpsim      |none          |rainfall input code
!!    prog        |NA            |program name and version
!!    rndseed(:,:)|none          |random number seeds
!!    tmpsim      |none          |temperature input code
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

!!    input summary file
WRITE (24,1000) prog,values(2),values(3),values(1),values(5),  &
    values(6),values(7)
WRITE (24,1010) title
WRITE (24,1020) nbyr, da_km
IF (igen == 0) THEN
  WRITE (24,1030)
ELSE
  WRITE (24,1040) igen
END IF
WRITE (24,1050) rndseed(idg(1),1)
WRITE (24,1051) rndseed(idg(2),1)
WRITE (24,1052) rndseed(idg(3),1)
WRITE (24,1053) rndseed(idg(4),1)
WRITE (24,1054) rndseed(idg(5),1)
WRITE (24,1055) rndseed(idg(6),1)
WRITE (24,1056) rndseed(idg(7),1)
WRITE (24,1057) rndseed(idg(8),1)
WRITE (24,1058) rndseed(idg(9),1)

WRITE (24,1060)
select case (pcpsim)
case (1)
WRITE (24,1061)
IF (ievent > 1) THEN
  WRITE (24,1062) idt
ELSE
  WRITE (24,1063)
END IF
case (2)
WRITE (24,1064)
END select
WRITE (24,1070)
select case (tmpsim)
case (1)
WRITE (24,1071)
case (2)
WRITE (24,1072)
END select

select case (ipet)
case (0)
WRITE (24,1080)
case (1)
WRITE (24,1081)
case (2)
WRITE (24,1082)
case (3)
WRITE (24,1083)
END select

WRITE (24,1090)
select case (ievent)
case (0)
WRITE (24,1091)
case (1)
WRITE (24,1092)
case (2)
WRITE (24,1093)
case (3)
WRITE (24,1094)
END select
select case (irte)
case (0)
WRITE (24,1095)
case (1)
WRITE (24,1096)
END select
select case (ideg)
case (0)
WRITE (24,1097)
case (1)
WRITE (24,1098)
END select
select case (isubwq)
case (0)
WRITE (24,1101)
case (1)
WRITE (24,1102)
END select
select case (iwq)
case (0)
WRITE (24,1099)
case (1)
WRITE (24,1100)
END select

IF (icrk == 1) WRITE (24,1110)

!!    standard output file
WRITE (26,1000) prog,values(2),values(3),values(1),values(5),  &
    values(6),values(7)
WRITE (26,1010) title
WRITE (26,1020) nbyr, da_km
IF (isproj == 1) THEN
  WRITE (19,1000) prog,values(2),values(3),values(1),values(5),  &
      values(6),values(7)
  WRITE (19,1010) title
  WRITE (19,1020) nbyr, da_km
END IF

!!    hyd.out file
WRITE (11123,5000)

!!    chan.deg file
WRITE (16,7000)

RETURN
1000 FORMAT ('1',/t5,a80,t105,2(i2,'/'),i4,5X,2(i2,':'),i2)
1010 FORMAT (/(t5,20A4))
1020 FORMAT (t10,'Number of years in run: ',i4/t10,  &
    'Area of watershed: ',f12.3,' km2')
1030 FORMAT (t10,'Random number generator cycles: 0, use default number s')
1040 FORMAT (t10,'Random number generator cycles: ',i4)
1050 FORMAT (/t10,'Initial random number seed: wet/dry day prob  ',1X, i14)
1051 FORMAT (t10,'Initial random number seed: radiation         ',1X, i14)
1052 FORMAT (t10,'Initial random number seed: precipitation     ',1X, i14)
1053 FORMAT (t10,'Initial random number seed: 0.5 hr rainfall   ',1X, i14)
1054 FORMAT (t10,'Initial random number seed: wind speed        ',1X, i14)
1055 FORMAT (t10,'Initial random number seed: irrigation        ',1X, i14)
1056 FORMAT (t10,'Initial random number seed: relative humidity ',1X, i14)
1057 FORMAT (t10,'Initial random number seed: max temperature   ',1X, i14)
1058 FORMAT (t10,'Initial random number seed: min temperature   ',1X, i14)
1060 FORMAT (/t10,'Precipitation data used in run:')
1061 FORMAT (t11,'Multiple gages read for watershed')
1062 FORMAT (t14,'Subdaily rainfall data used, summarized every ',i2, 'min')
1063 FORMAT (t14,'Daily rainfall data used')
1064 FORMAT (t11,'Multiple gages simulated for watershed')
1070 FORMAT (/t10,'Temperature data used in run:')
1071 FORMAT (t11,'Multiple gages read for watershed')
1072 FORMAT (t11,'Multiple gages simulated for watershed')
1080 FORMAT (/t10,'PET method used: Priestley-Taylor')
1081 FORMAT (/t10,'PET method used: Penman-Monteith')
1082 FORMAT (/t10,'PET method used: Hargreaves')
1083 FORMAT (/t10,'PET method used: read in values')
1090 FORMAT (/t10,'Rainfall/Runoff/Routing Option:')
1091 FORMAT (t11,'Daily rainfall data',/t11,'Runoff estimated with ',  &
    'curve number method',/t11,'Daily stream routing')
1092 FORMAT (t11,'Daily rainfall data',/t11,'Runoff estimated with ',  &
    'Green & Ampt method',/t11,'Daily stream routing')
1093 FORMAT (t11,'Subdaily rainfall data',/t11,'Runoff estimated with',  &
    ' Green & Ampt method',/t11,'Daily stream routing')
1094 FORMAT (t11,'Subdaily rainfall data',/t11,'Runoff estimated with',  &
    ' Green & Ampt method',/t11,'Hourly stream routing')
1095 FORMAT (t12,'Variable Storage routing method')
1096 FORMAT (t12,'Muskingum routing method')
1097 FORMAT (t12,'Channel dimensions remain constant')
1098 FORMAT (t12,'Channel dimensions change due to deposition/degrad',  &
    'ation')
1099 FORMAT (t12,'In-stream nutrient transformations not modeled')
1100 FORMAT (t12,'In-stream nutrient transformations modeled using',  &
    ' QUAL2E equations')
1101 FORMAT (t12,'Subbasin algae/CBOD loadings not modeled')
1102 FORMAT (t12,'Subbasin algae/CBOD loadings modeled')
1110 FORMAT (/t10,'Crack flow modeled')
5000 FORMAT ('  icode',t11,'ic',t14,'inum1',t20,'inum2',t26,'inum3',  &
    t34,'subed',t41,'recmonps',t50,'reccnstps',t61,'flow(m^3)',  &
    t73,'sed(t)',t85,'orgn(kg)',t97,'orgp(kg)',t109,'nitrate(kg)',  &
    t121,'sol.p(kg)',t133,'sol.pst(mg)',t145,'sor.pst(mg)')
7000 FORMAT (/,' Initial Dimen',' Channel Dimensions ',/,' Reach',  &
    '    Depth (m)','  Width (m)','  Slope (m/m)')
END SUBROUTINE std1
