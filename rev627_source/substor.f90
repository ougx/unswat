SUBROUTINE substor
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine stores and lags lateral soil flow and nitrate

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bss(1,:)      |mm H2O       |amount of lateral flow lagged
!!    bss(2,:)      |kg N/ha      |amount of nitrate in lateral flow lagged

!!    bss(3,:)      |mm           |amount of tile flow lagged
!!    bss(4,:)      |kg N/ha      |amount of nitrate in tile flow lagged
!!    hrupest(:)    |none         |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru          |none         |HRU number
!!    lat_pst(:)    |kg pst/ha    |amount of pesticide in lateral flow in HRU
!!                                |for the day
!!    lat_ttime(:)  |none         |Exponential of the lateral flow travel time
!!    latno3(:)     |kg N/ha      |amount of NO3-N in lateral flow in HRU for
!!                                |the day
!!    latq(:)       |mm H2O       |amount of water in lateral flow in HRU for
!!                                |the day
!!    qtile(:)      |mm H2O       |amount of water in tile flow in HRU for the day
!!    tile_ttime(:) |none         |Exponential of tile flow travel time
!!    pst_lag(:,3,:)|kg pst/ha    |amount of pesticide lagged
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bss(1,:)      |mm H2O        |amount of lateral flow lagged
!!    bss(2,:)      |kg N/ha       |amount of nitrate in lateral flow lagged
!!    bss(3,:)      |mm            |amount of tile flow lagged
!!    bss(4,:)      |kg N/ha       |amount of nitrate in tile flow lagged
!!    bssprev       |mm H2O        |lateral flow lagged from prior day of
!!                                 |simulation
!!    lat_pst(:)    |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                                 |for the day
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!                                 |the day
!!    latq(:)       |mm H2O        |amount of water in lateral flow in HRU for
!!                                 |the day
!!    pst_lag(:,3,:)|kg pst/ha     |amount of pesticide lagged
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j, k

j = 0
j = ihru

bssprev = bss(1,j)
bss(1,j) = bss(1,j) + latq(j)
bss(2,j) = bss(2,j) + latno3(j)
bss(3,j) = bss(3,j) + qtile
bss(4,j) = bss(4,j) + tileno3(j)
IF (bss(1,j) < 1.e-6) bss(1,j) = 0.0
IF (bss(2,j) < 1.e-6) bss(2,j) = 0.0
IF (bss(3,j) < 1.e-6) bss(3,j) = 0.0
IF (bss(4,j) < 1.e-6) bss(4,j) = 0.0

IF (hrupest(j) == 1) THEN
  DO k = 1, npmx
    IF (pst_lag(k,3,j) < 1.e-6) pst_lag(k,3,j) = 0.0
!MFW, 3/3/12: Modified lagged pesticide to include decay in lag
    pst_lag(k,3,j) = (pst_lag(k,3,j) * decay_s(npno(k))) + lat_pst(k)
! pst_lag(k,3,j) = pst_lag(k,3,j) + lat_pst(k)
  END DO
END IF

latq(j) = bss(1,j) * lat_ttime(j)
latno3(j) = bss(2,j) * lat_ttime(j)
qtile = bss(3,j) * tile_ttime(j)
tileno3(j) = bss(4,j) * tile_ttime(j)
IF (latq(j) < 1.e-6) latq(j) = 0.
IF (latno3(j) < 1.e-6) latno3(j) = 0.
IF (qtile < 1.e-6) qtile = 0.
IF (tileno3(j) < 1.e-6) tileno3(j) = 0.
IF (hrupest(j) == 1) THEN
  DO k = 1, npmx
    lat_pst(k) = pst_lag(k,3,j) * lat_ttime(j)
  END DO
END IF

bss(1,j) = bss(1,j) - latq(j)
bss(2,j) = bss(2,j) - latno3(j)
bss(3,j) = bss(3,j) - qtile
bss(4,j) = bss(4,j) - tileno3(j)
IF (hrupest(j) == 1) THEN
  DO k = 1, npmx
    pst_lag(k,3,j) = pst_lag(k,3,j) - lat_pst(k)
  END DO
END IF

RETURN
END SUBROUTINE substor
