!include 'modparm.f'
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01
 
PROGRAM main
!!    this is the main program that reads input, calls the main simulation
!!    model, and writes output.
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!         ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the
!!                               |month and dd is the day
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    values(1)   |year          |year simulation is performed
!!    values(2)   |month         |month simulation is performed
!!    values(3)   |day           |day in month simulation is performed
!!    values(4)   |minutes       |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    values(5)   |hour          |hour simulation is performed
!!    values(6)   |minutes       |minute simulation is performed
!!    values(7)   |seconds       |second simulation is performed
!!    values(8)   |milliseconds  |millisecond simulation is performed
!!    zone        |NA            |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: date_and_time
!!    SWAT: getallo, allocate_parms, readfile, readfig
!!    SWAT: readbsn, std1, readwwq, readinpt, std2, storeinitial
!!    SWAT: openwth, headout, simulate, finalbal, writeaa, pestw


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm
IMPLICIT NONE
prog = "SWAT Jun 11 2014    VER 2012/Rev 627"
WRITE (*,1000)
1000 FORMAT(1X,"               SWAT2012               ",/,  &
    "               Rev. 627              ",/,  &
    "      Soil & Water Assessment Tool    ",/,  &
    "               PC Version             ",/,  &
    " Program reading from file.cio . . . executing",/)

!! process input

CALL getallo
!print *, "Finish getallo"
CALL allocate_parms
!print *, "Finish allocate_parms"
CALL readfile
!print *, "Finish readfile"
CALL readbsn
!print *, "Finish readbsn"
CALL readwwq
!print *, "Finish readwwq"
IF (fcstyr > 0 .AND. fcstday > 0) CALL readfcst
CALL readplant             !! read in the landuse/landcover database
!print *, "Finish readplant"
CALL readtill              !! read in the tillage database
!print *, "Finish readtill"
CALL readpest              !! read in the pesticide database
!print *, "Finish readpest"
CALL readfert              !! read in the fertilizer/nutrient database
!print *, "Finish readfert"
CALL readurban             !! read in the urban land types database
!print *, "Finish readurban"
CALL readseptwq            !! read in the septic types database
!print *, "Finish readseptwq"
CALL readlup
!print *, "Finish readlup"

!!-------------------OGXinSWAT Begin------------------------------
!!  open and read the soil material file
IF (ievent>0) THEN
  CALL readsoilmat           !! OGX: read soil materials
!print *, "Start reading soil materials"
END IF
!!--------------------End--------------------------------

CALL readfig
CALL readatmodep
CALL readinpt
CALL std1
CALL std2
CALL openwth
CALL headout

!! convert integer to string for output.mgt file
subnum = ""
hruno = ""
DO i = 1, mhru
  WRITE (subnum(i),FMT=' (i5.5)') hru_sub(i)
  WRITE (hruno(i),FMT=' (i4.4)') hru_seq(i)
END DO

IF (isproj == 2) THEN
  hi_targ = 0.0
END IF

!! save initial values
IF (isproj == 1) THEN
  scenario = 2
  CALL storeinitial
ELSE IF (fcstcycles > 1) THEN
  scenario =  fcstcycles
  CALL storeinitial
ELSE
  scenario = 1
END IF
IF (iclb /= 4) THEN
  DO iscen = 1, scenario
    
    
!! simulate watershed processes
    CALL simulate
    
!! perform summary calculations
    CALL finalbal
    CALL writeaa
    CALL pestw
    
!!reinitialize for new scenario
    IF (scenario > iscen) CALL rewind_init
  END DO
END IF
DO i = 101, 109       !Claire 12/2/09: change 1, 9  to 101, 109.
  CLOSE (i)
END DO
CLOSE(124)
WRITE (*,1001)
1001 FORMAT (/," Execution successfully completed ")

iscen=1
!! file for Mike White to review to ensure simulation executed normally
OPEN (9999,FILE='fin.fin')
WRITE (9999,*) 'Execution successful'
CLOSE (9999)

STOP
END PROGRAM main
