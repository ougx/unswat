SUBROUTINE headout
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes the headings to the major output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hedb(:)     |NA            |column titles in subbasin output files
!!    hedr(:)     |NA            |column titles in reach output files
!!    hedrsv(:)   |NA            |column titles in reservoir output files
!!    heds(:)     |NA            |column titles in HRU output files
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ipdvab(:)   |none          |output variable codes for output.sub file
!!    ipdvar(:)   |none          |output variable codes for .rch file
!!    ipdvas(:)   |none          |output variable codes for output.hru file
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotb       |none          |number of output variables printed (output.sub)
!!    itotr       |none          |number of output variables printed (.rch)
!!    itots       |none          |number of output variables printed (output.hru)
!!    msubo       |none          |maximum number of variables written to
!!                               |subbasin output file (output.sub)
!!    mhruo       |none          |maximum number of variables written to
!!                               |HRU output file (output.hru)
!!    mrcho       |none          |maximum number of variables written to
!!                               |reach output file (.rch)
!!    prog        |NA            |program name and version
!!    title       |NA            |title lines from file.cio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ilen        |none          |width of data columns in output file
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    header

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, ilen

CALL header

!! write headings to HRU output file (output.hru)
WRITE (28,1000) prog, values(2), values(3), values(1), values(5),  &
    values(6), values(7)
WRITE (28,1010) title


IF (ipdvas(1) > 0) THEN
  IF (icalen == 0) WRITE (28,1020) (heds(ipdvas(j)), j = 1, itots) !!custom printout
  IF (icalen == 1) WRITE (28,1021) (heds(ipdvas(j)), j = 1, itots) !!custom printout
ELSE
  IF (icalen == 0) WRITE (28,1020) (heds(j), j = 1, mhruo)         !!default printout
  IF (icalen == 1) WRITE (28,1021) (heds(j), j = 1, mhruo)         !!default printout
END IF

!! write headings to HRU output file (output2.hru)
IF (isproj == 1) THEN
  WRITE (21,1000)prog, values(2), values(3), values(1), values(5),  &
      values(6), values(7)
  WRITE (21,1010) title
  IF (ipdvas(1) > 0) THEN
    WRITE (21,1020) (heds(ipdvas(j)), j = 1, itots) !!custom printout
  ELSE
    WRITE (21,1020) (heds(j), j = 1, mhruo)         !!default printout
  END IF
END IF

!! write headings to subbasin output file (output.sub)
WRITE (31,1000) prog, values(2), values(3), values(1), values(5),  &
    values(6), values(7)
WRITE (31,1010) title


IF (ipdvab(1) > 0) THEN
  IF (icalen == 0) WRITE (31,1030) (hedb(ipdvab(j)), j = 1, itotb) !!custom printout
  IF (icalen == 1) WRITE (31,1031) (hedb(ipdvab(j)), j = 1, itotb) !! month/day/yr PRINT
ELSE
  IF (icalen == 0) WRITE (31,1030) (hedb(j), j = 1, msubo)         !!default printout
  IF (icalen == 1) WRITE (31,1031) (hedb(j), j = 1, msubo)         !!month/day/yr PRINT
  1031  FORMAT (//6X,' SUB      GIS  MO DA  YR   AREAkm2',22(a10))
END IF

!! write headings to reach output file (output.rch)
WRITE (7,1000) prog, values(2), values(3), values(1), values(5),  &
    values(6), values(7)
WRITE (7,1010) title


IF (ipdvar(1) > 0) THEN
  IF (iprint /= 3) THEN
    IF (icalen == 0) WRITE (7,1040) (hedr(ipdvar(j)), j = 1, itotr)  !! daily/monthly output - julian day
    IF (icalen == 1) WRITE (7,1042) (hedr(ipdvar(j)), j = 1, itotr)  !! daily output - calendar day
    1042 FORMAT (//7X,'RCH      GIS  MO DA   YR     AREAkm2',56A12)
    
  ELSE
    WRITE (7,1041) (hedr(ipdvar(j)), j = 1, itotr)  !! subdaily output
  END IF
ELSE     !! default printout
  IF (iprint /= 3) THEN
    IF (icalen == 0) WRITE (7,1040) (hedr(j), j = 1, mrcho)       !! daily/monthly output - julian day
    IF (icalen == 1) WRITE (7,1042) (hedr(j), j = 1, mrcho)       !! daily output - calendar day
  ELSE
    WRITE (7,1041) (hedr(j), j = 1, mrcho)          !! subdaily output
  END IF
END IF

!! write headings to reach output file (output2.rch)
IF (isproj == 1) THEN
  WRITE (20,1000)prog, values(2), values(3), values(1), values(5),  &
      values(6), values(7)
  WRITE (20,1010) title
  IF (ipdvar(1) > 0) THEN
    WRITE (20,1040) (hedr(ipdvar(j)), j = 1, itotr)  !! custom printout
  ELSE
    WRITE (20,1040) (hedr(j), j = 1, mrcho)          !! default printout
  END IF
END IF

!! write headings to reservoir output file (output.rsv)
WRITE (8,1000) prog, values(2), values(3), values(1), values(5),  &
    values(6), values(7)
WRITE (8,1010) title
WRITE (8,1050) (hedrsv(j), j = 1, 41)
!! write headings to reservoir output file (output2.rsv)
IF (isproj == 1) THEN
  WRITE (22,1000) prog, values(2), values(3), values(1), values(5),  &
      values(6), values(7)
  WRITE (22,1010) title
  WRITE (22,1050) (hedrsv(j), j = 1, 41)
END IF

!! write headings to HRU impoundment output file (output.wtr)
IF (iwtr == 1) THEN
  WRITE (29,1000)prog, values(2), values(3), values(1), values(5),  &
      values(6), values(7)
  WRITE (29,1010) title
  WRITE (29,1020) (hedwtr(j), j = 1, 40)
END IF

!! write headings to pesticide output file (output.pst)
IF (iprp /= 0) THEN
  WRITE (30,1000)prog, values(2), values(3), values(1), values(5),  &
      values(6), values(7)
  WRITE (30,1010) title
  WRITE (30,3000)
  WRITE (30,3001) (npno(j),npno(j), j = 1, npmx)
  WRITE (30,3002) (pname(npno(j)),pname(npno(j)), j = 1, npmx)
  WRITE (30,3003) (("SOLUBLE mg       SORBED mg"), j = 1, npmx)
END IF
!! Jaehak subdaily bmp output header
!bmp-sedfil.out
WRITE(77778,'(a21)') 'SED-FIL Basins output'
WRITE(77778,'(a170)') '------------------------------   ----------  &
    ------------ Sedimentation Pond --------------------------   -----  &
    ----------------------- Sand Filter ------------------------------ '
WRITE(77778,'(5a6,30a12)') 'year', 'day','time','sub','SFnum',  &
    'inflw(m3)','outflw(m3)','bypass(m3)','sedin(kg)','sedout(kg)',  &
    'sbypass(kg)','inflw(m3)','outflw(m3)','bypass(m3)','sedin(kg)',  &
    'sedout(kg)','sbypass(kg)'

!bmp-ri.out
WRITE(77779,'(a21)') 'Retention-Irrigation output'
WRITE(77779,'(5a6,30a12)') 'year', 'day','time','sub','RInum',  &
    'inflw(m3)','qbypass(m3)','pmpflw(m3)','sedin(kg)','sbypass(kg)', 'pmpsed(kg)'

RETURN
1000 FORMAT ('1',/t5,a80,t105,2(i2,'/'),i4,5X,2(i2,':'),i2)
1010 FORMAT (/(t5,20A4))
1020 FORMAT (//'LULC  HRU       GIS  SUB  MGT  MON','   AREAkm2', 78(a10))
!    * 76(a10),"    GISnum")
1021  FORMAT (//'LULC  HRU       GIS  SUB  MGT MO DA   YR',  &
    '   AREAkm2', 78(a10))
1030 FORMAT (//6X,' SUB      GIS  MON   AREAkm2',22(a10))
1040 FORMAT (//7X,'RCH      GIS   MON     AREAkm2',56A12)
1041 FORMAT (//7X,'RCH      GIS   DAY   DET     AREAkm2',45A12)
1050 FORMAT (//6X,'     RES  MON',41A12)
1060 FORMAT (//6X,'RCH GIS  MON',26A12)
2000 FORMAT (a12,12X,i4,4X,i4)
3000 FORMAT ("Pesticide loadings to main channel by HRU",/)
3001 FORMAT ("Pesticide #",250(18X,i3,1X))
3002 FORMAT ("Pesticide name:      ",250(a16,1X))
3003 FORMAT (4X,'GISnum YEAR MON',7X,125(a26,8X))
END SUBROUTINE headout
