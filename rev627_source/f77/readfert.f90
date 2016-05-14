SUBROUTINE readfert
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:02

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine reads input parameters from the fertilizer/manure
!!     (i.e. nutrient) database (fert.dat)

!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     mfdb        |none            |maximum number of fertilizers in
!!                                  |database
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     bactkddb(:) |none            |bacteria partition coefficient:
!!                                  |1: all bacteria in solution
!!                                  |0: all bacteria sorbed to soil particles
!!     bactlpdb(:) |# cfu/g manure  |concentration of less persistent
!!                                  |bacteria in manure(fertilizer)
!!     bactpdb(:)  |# cfu/g manure  |concentration of persistent bacteria
!!                                  |in manure(fertilizer)
!!     fertnm(:)   |NA              |name of fertilizer
!!     fminn(:)    |kg minN/kg fert |fraction of mineral N (NO3 + NH3)
!!     fminp(:)    |kg minP/kg fert |fraction of mineral P
!!     fnh3n(:)    |kg NH3-N/kg minN|fraction of NH3-N in mineral N
!!     forgn(:)    |kg orgN/kg fert |fraction of organic N
!!     forgp(:)    |kg orgP/kg fert |fraction of organic P
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof         |none            |end of file flag
!!     it          |none            |counter which represents the array
!!                                  |storage number of the pesticide data
!!                                  |the array storage number is used by the
!!                                  |model to access data for a specific
!!                                  |fertilizer
!!     ifnum       |none            |number of fertilizer/manure. reference
!!                                  |only
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: it, ifnum, eof
REAL :: ffminn, ffminp, fforgn, fforgp, ffnh3n, bctpdb, bctlpdb
REAL :: bctkddb
CHARACTER (LEN=8) :: fnm

ifnum = 0
eof = 0

DO
!! initialize local variables
  bctkddb = 0.
  bctlpdb = 0.
  bctpdb = 0.
  ffminn = 0.
  ffminp = 0.
  ffnh3n = 0.
  fforgn = 0.
  fforgp = 0.
  fnm = ""
  
  READ (107,5000,IOSTAT=eof) it, fnm, ffminn, ffminp, fforgn,  &
      fforgp, ffnh3n, bctpdb, bctlpdb, bctkddb
  
  IF (eof < 0) EXIT
  
  IF (it == 0) EXIT
  
  fertnm(it) = fnm
  fminn(it) = ffminn
  fminp(it) = ffminp
  forgn(it) = fforgn
  forgp(it) = fforgp
  fnh3n(it) = ffnh3n
  bactpdb(it) = bctpdb
  bactlpdb(it) = bctlpdb
  bactkddb(it) = bctkddb
END DO

CLOSE (107)
RETURN
!! format changed for prem - last two bacteria inputs ok for all
5000 FORMAT (i4,1X,a8,6F8.3,2F11.0)
END SUBROUTINE readfert
