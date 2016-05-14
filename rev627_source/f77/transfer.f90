SUBROUTINE transfer
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine transfers water

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icodes(:)   |none          |routing command code:
!!                               |0 = finish       9 = save
!!                               |1 = subbasin    10 = recday
!!                               |2 = route       11 = reccnst
!!                               |3 = routres     12 = structure
!!                               |4 = transfer    13 =
!!                               |5 = add         14 = saveconc
!!                               |6 = rechour     15 =
!!                               |7 = recmon      16 = autocal
!!                               |8 = recyear
!!    ihout       |none          |water source type:
!!                               |1 reach
!!                               |2 reservoir
!!    ihouts(:)   |none          |For ICODES equal to
!!                               |0: not used
!!                               |1,2,3,5,6,7,8,10,11: hydrograph storage
!!                               |                     location number
!!                               |4: water source type
!!                               |   (1=reach)
!!                               |   (2=reservoir)
!!                               |9: hydrograph storage location of data to
!!                               |   be printed to event file
!!                               |14:hydrograph storage location of data to
!!                               |   be printed to saveconc file
!!    inum1       |none          |reach or reservoir # from which water is
!!                               |removed
!!    inum1s(:)   |none          |For ICODES equal to
!!                               |0: not used
!!                               |1: subbasin number
!!                               |2: reach number
!!                               |3: reservoir number
!!                               |4: reach or res # flow is diverted from
!!                               |5: hydrograph storage location of 1st
!!                               |   dataset to be added
!!                               |6,7,8,9,10,11,14: file number
!!    inum2       |none          |water destination type:
!!                               |1 reach
!!                               |2 reservoir
!!    inum3       |none          |reach or reservoir # to which water is
!!                               |added
!!    inum4       |none          |rule governing transfer of water
!!                               |1 fraction of water in source transferred
!!                               |2 minimum volume (res) or flow (rch) left
!!                               |3 exact amount transferred
!!    mhyd        |none          |maximum number of hydrographs
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    rchdy(2,:)  |m^3/s         |flow out of reach on day
!!    rchdy(6,:)  |metric tons   |sediment transported out of reach on day
!!    rchdy(9,:)  |kg N          |organic N transported out of reach on day
!!    rchdy(11,:) |kg P          |organic P transported out of reach on day
!!    rchdy(13,:) |kg N          |nitrate transported out of reach on day
!!    rchdy(15,:) |kg N          |ammonia transported out of reach on day
!!    rchdy(17,:) |kg N          |nitrite transported out of reach on day
!!    rchdy(19,:) |kg P          |soluble P transported out of reach on day
!!    rchdy(21,:) |kg chla       |chlorophyll-a transported out of reach on day
!!    rchdy(23,:) |kg O2         |CBOD transported out of reach on day
!!    rchdy(25,:) |kg O2         |dissolved oxygen transported out of reach on
!!                               |day
!!    rchdy(27,:) |mg pst        |soluble pesticide transported out of reach on
!!                               |day
!!    rchdy(29,:) |mg pst        |sorbed pesticide transported out of reach on
!!                               |day
!!    rchdy(38,:) |kg bact       |persistent bacteria transported out of reach
!!                               |on day
!!    rchdy(39,:) |kg bact       |less persistent bacteria transported out of
!!                               |reach on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    rnum1       |m^3 H2O       |amount of water transferred
!!    varoute(:,:)|varies        |routing storage array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rchdy(2,:)  |m^3/s         |flow out of reach on day
!!    rchdy(6,:)  |metric tons   |sediment transported out of reach on day
!!    rchdy(9,:)  |kg N          |organic N transported out of reach on day
!!    rchdy(11,:) |kg P          |organic P transported out of reach on day
!!    rchdy(13,:) |kg N          |nitrate transported out of reach on day
!!    rchdy(15,:) |kg N          |ammonia transported out of reach on day
!!    rchdy(17,:) |kg N          |nitrite transported out of reach on day
!!    rchdy(19,:) |kg P          |soluble P transported out of reach on day
!!    rchdy(21,:) |kg chla       |chlorophyll-a transported out of reach on day
!!    rchdy(23,:) |kg O2         |CBOD transported out of reach on day
!!    rchdy(25,:) |kg O2         |dissolved oxygen transported out of reach on
!!                               |day
!!    rchdy(27,:) |mg pst        |soluble pesticide transported out of reach on
!!                               |day
!!    rchdy(29,:) |mg pst        |sorbed pesticide transported out of reach on
!!                               |day
!!    rchdy(38,:) |kg bact       |persistent bacteria transported out of reach
!!                               |on day
!!    rchdy(39,:) |kg bact       |less persistent bacteria transported out of
!!                               |reach on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    varoute(:,:)|varies        |routing storage array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    k           |none          |counter
!!    ratio       |none          |fraction of reach outflow diverted
!!    tranmx      |m^3 H2O       |maximum amount of water to be transferred
!!    volum       |m^3 H2O       |volume of water in source
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
use parm

INTEGER :: k, ii
REAL :: volum, tranmx, ratio

!! check beg/end months summer or winter
IF (mo_transb(inum5) < mo_transe(inum5)) THEN
  IF (i_mo < mo_transb(inum5) .OR. i_mo > mo_transe(inum5)) RETURN
ELSE
  IF (i_mo > mo_transe(inum5) .AND. i_mo < mo_transb(inum5))RETURN
END IF
!! compute volume of water in source
volum = 0.
IF (ihout == 2) THEN
  volum = res_vol(inum1)
ELSE
  volum = rchdy(2,inum1) * 86400.
END IF
IF (volum <= 0.) RETURN

!! compute maximum amount of water allowed to be transferred
tranmx = 0.
select case (inum4)
case (1)     !! transfer fraction of water in source
tranmx = volum * rnum1
case (2)     !! leave minimum volume or flow
tranmx = volum - rnum1
IF (tranmx < 0.) tranmx = 0.
case (3)     !! transfer volume specified
tranmx = rnum1
IF (tranmx > volum) tranmx = volum
END select

IF (tranmx > 0.) THEN
  
!! TRANSFER WATER TO DESTINATION
!        select case (inum2)
!          case (1)          !! TRANSFER WATER TO A CHANNEL
!            rchstor(inum3) = rchstor(inum3) + tranmx
!
!          case (2)          !! TRANSFER WATER TO A RESERVOIR
!            res_vol(inum3) = res_vol(inum3) + tranmx
!        end select
  
!! SUBTRACT AMOUNT TRANSFERED FROM SOURCE
  IF (ihout == 2) THEN
    res_vol(inum1) = res_vol(inum1) - tranmx
  ELSE
    xx = tranmx
!          if (xx > rchstor(inum1)) then
!            xx = tranmx - rchstor(inum1)
!            rchstor(inum1) = 0.
!          else
!            rchstor(inum1) = rchstor(inum1) - xx
!            xx = 0.
!          end if
    
    nhyd_tr = ih_tran(inum5)
    
    IF (xx > varoute(2,nhyd_tr)) THEN
      xx = tranmx - varoute(2,nhyd_tr)
      varoute(2,nhyd_tr) = 0.
    ELSE
      varoute(2,nhyd_tr) = varoute(2,nhyd_tr) - xx
      xx = 0.
    END IF
    
    ratio = 0.
    IF (rchdy(2,inum1) > 1.e-6) THEN
      xx = tranmx - xx
      ratio = 1. - xx / (rchdy(2,inum1) * 86400.)
    END IF
    
    ratio1 = 1. - ratio
    rchmono(2,inum1) = rchmono(2,inum1) - rchdy(2,inum1) * ratio1
    rchmono(6,inum1) = rchmono(6,inum1) - rchdy(6,inum1) * ratio1
    rchmono(9,inum1) = rchmono(9,inum1) - rchdy(9,inum1) * ratio1
    rchmono(11,inum1)=rchmono(11,inum1) - rchdy(11,inum1) * ratio1
    rchmono(13,inum1)=rchmono(13,inum1) - rchdy(13,inum1) * ratio1
    rchmono(15,inum1)=rchmono(15,inum1) - rchdy(15,inum1) * ratio1
    rchmono(17,inum1)=rchmono(17,inum1) - rchdy(17,inum1) * ratio1
    rchmono(19,inum1)=rchmono(19,inum1) - rchdy(19,inum1) * ratio1
    rchmono(21,inum1)=rchmono(21,inum1) - rchdy(21,inum1) * ratio1
    rchmono(23,inum1)=rchmono(23,inum1) - rchdy(23,inum1) * ratio1
    rchmono(25,inum1)=rchmono(25,inum1) - rchdy(25,inum1) * ratio1
    rchmono(27,inum1)=rchmono(27,inum1) - rchdy(27,inum1) * ratio1
    rchmono(29,inum1)=rchmono(29,inum1) - rchdy(29,inum1) * ratio1
    rchmono(38,inum1)=rchmono(38,inum1) - rchdy(38,inum1) * ratio1
    rchmono(39,inum1)=rchmono(39,inum1) - rchdy(39,inum1) * ratio1
    rchmono(40,inum1)=rchmono(40,inum1) - rchdy(40,inum1) * ratio1
    rchmono(41,inum1)=rchmono(41,inum1) - rchdy(41,inum1) * ratio1
    
    rchdy(2,inum1) = rchdy(2,inum1) * ratio
    rchdy(6,inum1) = rchdy(6,inum1) * ratio
    rchdy(9,inum1) = rchdy(9,inum1) * ratio
    rchdy(11,inum1) = rchdy(11,inum1) * ratio
    rchdy(13,inum1) = rchdy(13,inum1) * ratio
    rchdy(15,inum1) = rchdy(15,inum1) * ratio
    rchdy(17,inum1) = rchdy(17,inum1) * ratio
    rchdy(19,inum1) = rchdy(19,inum1) * ratio
    rchdy(21,inum1) = rchdy(21,inum1) * ratio
    rchdy(23,inum1) = rchdy(23,inum1) * ratio
    rchdy(25,inum1) = rchdy(25,inum1) * ratio
    rchdy(27,inum1) = rchdy(27,inum1) * ratio
    rchdy(29,inum1) = rchdy(29,inum1) * ratio
    rchdy(38,inum1) = rchdy(38,inum1) * ratio
    rchdy(39,inum1) = rchdy(39,inum1) * ratio
    rchdy(40,inum1) = rchdy(40,inum1) * ratio
    rchdy(41,inum1) = rchdy(41,inum1) * ratio
    rchdy(42,inum1) = rchdy(42,inum1) * ratio
  END IF
  
!!subratct from source
  DO ii = 3, mvaro
    varoute(ii,nhyd_tr) = varoute(ii,nhyd_tr) * ratio
  END DO
!!save vartran to add in rchinit and resinit
  vartran(2,inum3) = varoute(2,nhyd_tr) / ratio * ratio1
  DO ii = 3, mvaro
    vartran(ii,inum3) = varoute(ii,nhyd_tr) * ratio1
  END DO
  
END IF

RETURN
END SUBROUTINE transfer
