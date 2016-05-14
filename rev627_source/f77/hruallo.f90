SUBROUTINE hruallo
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of management operation types, etc.
!!   used in the simulation. These values are used to allocate array sizes for
!!   processes occurring in the HRU.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none        |max number of applications
!!    mcr         |none        |max number of crops grown per year
!!    mcut        |none        |max number of cuttings per year
!!    mgr         |none        |max number of grazings per year
!!    mlyr        |none        |max number of soil layers
!!    mnr         |none        |max number of years of rotation
!!    pstflg(:)   |none        |flag for types of pesticide used in watershed
!!                             |array location is pesticide ID number
!!                             |0: pesticide not used
!!                             |1: pesticide used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_af       |none        |number of autofertilizer operations in mgt file
!!    ap_ai       |none        |number of autoirrigation operations in mgt file
!!    ap_cc       |none        |number of continuous cuuting operations in mgt
!!    ap_cf       |none        |number of continuous fertilization operations in mgt
!!    ap_ci       |none        |number of continuous irrigation operations in mgt
!!    ap_f        |none        |number of fertilizer operations in mgt file
!!    ap_i        |none        |number of irrigation operations in mgt file
!!    ap_p        |none        |number of pesticide operations in mgt file
!!    ap_r        |none        |number of release/impound operations in mgt file
!!    ap_s        |none        |number of sweep operations in mgt file
!!    ap_t        |none        |number of tillage operations in mgt file
!!    chmfile     |NA          |HRU soil chemical data file name (.chm)
!!    cut         |none        |number of harvest only operations in mgt file
!!    depth(:)    |mm          |depth to bottom of soil layer
!!    eof         |none        |end of file flag (=-1 if eof, else =0)
!!    grz         |none        |number of grazing operations in mgt file
!!    hkll        |none        |number of harvest/kill operations in mgt file
!!    hru         |none        |number of HRUs in subbasin
!!    hrufile     |NA          |name of HRU general data file name (.hru)
!!    ii          |none        |counter
!!    j           |none        |counter
!!    k           |none        |counter
!!    kll         |none        |number of kill operations in mgt file
!!    lyrtot      |none        |total number of layers in profile
!!    mgt_op      |none        |manangement operation code
!!    mgt1i       |none        |sixth parameter in mgt file operation line
!!    mgtfile     |NA          |HRU management data file name (.mgt)
!!    plt         |none        |number of plant operations in mgt file
!!    pstnum      |none        |pesticide ID number from database file
!!    rot         |none        |number of years in rotation used in HRU
!!    solfile     |NA          |HRU soil data file name (.sol)
!!    titldum     |NA          |input lines in .sub that are not processed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


use parm

CHARACTER (LEN=13) :: hrufile, mgtfile, solfile, chmfile
CHARACTER (LEN=80) ::  titldum
INTEGER :: eof, j, k, lyrtot, rot, plt, ap_f, ap_p, ap_t, ap_i
INTEGER :: grz, cut, mgt1i, pstnum, ii, ap_r, ap_s, kll, hkll
INTEGER :: ap_ai, ap_af, mgt_op, ap_cf, ap_cc, ap_ci, jj
INTEGER :: iopera_sub
REAL :: depth(25)

DO j= mhru1, mhru
  mgtfile = ""
  solfile = ""
  chmfile = ""
  READ (25,5300,IOSTAT=eof)hrufile, mgtfile, solfile, chmfile, ilnds
  IF (eof < 0) RETURN
  IF (ilnds > 0) THEN
    ils_nofig = 1
  END IF
  CALL caps(mgtfile)
  CALL caps(solfile)
  CALL caps(chmfile)
  OPEN (9,FILE=solfile,RECL=350)
!! calculate # of soil layers in profile
  depth = 0.
  lyrtot = 0
  READ (9,6000) titldum
  READ (9,6000) titldum
  READ (9,6000) titldum
  READ (9,6000) titldum
  READ (9,6000) titldum
  READ (9,6000) titldum
  READ (9,6000) titldum
  READ (9,6100) (depth(k), k = 1, 25)
  DO k = 1, 25
    IF (depth(k) <= 0.001) lyrtot = k - 1
    IF (depth(k) <= 0.001) EXIT
  END DO
  mlyr = MAX(mlyr,lyrtot)
  CLOSE (9)
  OPEN (10,FILE=mgtfile)
  
!!  calculate max number of operations per hru
  iopera_sub = 1
  mcri = 0
  DO kk = 1, 30
    READ (10,6000) titldum
  END DO
  
  DO kk = 1, 1000
    READ (10,6300,IOSTAT=eof) mgt_op, mgt1i
    IF (eof < 0) EXIT
    IF (mgt_op == 1) THEN
      mcri = mcri + 1
    END IF
    IF (mgt_op == 4 .AND. mgt1i > 0) pstflg(mgt1i) = 1
    iopera_sub = iopera_sub + 1
  END DO
  iopera = MAX(iopera,iopera_sub)
  mcr = MAX(mcr,mcri)
  
  CLOSE (10)            !!   nubz test
  
  OPEN (11,FILE=chmfile)
  eof = 0
  DO
    DO k = 1, 11
      READ (11,6000,IOSTAT=eof) titldum
      IF (eof < 0) EXIT
    END DO
    IF (eof < 0) EXIT
    DO
      pstnum = 0
      READ (11,*,IOSTAT=eof) pstnum
      IF (eof < 0) EXIT
      IF (pstnum > 0) pstflg(pstnum) = 1
    END DO
    IF (eof < 0) EXIT
  END DO
  CLOSE (11)
END DO    ! hru loop

RETURN
5000 FORMAT (6A)
5001 FORMAT (a1,9X,5I6)
5002 FORMAT(a)
5100 FORMAT (20A4)
5200 FORMAT (10I4)
5300 FORMAT (4A13,52X,i6)
6000 FORMAT (a80)
6100 FORMAT (27X,25F12.2)
6200 FORMAT (1X,i3)
6300 FORMAT (16X,i2,1X,i4)
END SUBROUTINE hruallo
