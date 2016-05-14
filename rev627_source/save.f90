SUBROUTINE SAVE
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:04

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily records of loadings from a particular
!!    hydrograph storage location in the event output file. The save command
!!    is used when a watershed is broken into several individual runs and
!!    outflow from an upstream watershed needs to be stored for reading into
!!    a simulation of the downstream portion of the watershed. The recday
!!    command is used to read in the data.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida          |julian date  |current day of simulation
!!    ihout         |none         |hydrograph storage location number for data
!!                                |to be printed to event file
!!    iyr           |year         |current year of simulation (actual year)
!!    varoute(1,:)  |deg C        |temperature
!!    varoute(2,:)  |m^3          |volume of water
!!    varoute(3,:)  |metric tons  |sediment
!!    varoute(4,:)  |kg N         |organic N
!!    varoute(5,:)  |kg P         |organic P
!!    varoute(6,:)  |kg N         |NO3-N
!!    varoute(7,:)  |kg P         |mineral (soluble) P
!!    varoute(11,:) |mg pst       |pesticide in solution
!!    varoute(12,:) |mg pst       |pesticide sorbed to sediment
!!    varoute(13,:) |kg           |chlorophyll-a
!!    varoute(14,:) |kg N         |NH3
!!    varoute(15,:) |kg N         |NO2
!!    varoute(16,:) |kg           |carbonaceous biological oxygen demand
!!    varoute(17,:) |kg           |dissolved oxygen
!!    varoute(18,:) |# cfu/100ml  |persistent bacteria
!!    varoute(19,:) |# cfu/100ml  |less persistent bacteria
!!    varoute(20,:) |kg           |conservative metal #1
!!    varoute(21,:) |kg           |conservative metal #2
!!    varoute(22,:) |kg           |conservative metal #3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: ii

IF (inum1 <= 10 .AND. inum1 > 0) THEN
  IF (ievent == 3 .AND. inum2 == 1) THEN
!! Write subdaily values
    DO ii = 1, nstep
      IF (inum3 == 0) THEN
        WRITE (40+inum1,5000) iida, iyr, ii-1, hhvaroute(2,ihout,ii),  &
            hhvaroute(3,ihout,ii), hhvaroute(4,ihout,ii),  &
            hhvaroute(5,ihout,ii), hhvaroute(6,ihout,ii),  &
            hhvaroute(14,ihout,ii), hhvaroute(15,ihout,ii),  &
            hhvaroute(7,ihout,ii), hhvaroute(16,ihout,ii),  &
            hhvaroute(17,ihout,ii), hhvaroute(13,ihout,ii),  &
            hhvaroute(11,ihout,ii), hhvaroute(12,ihout,ii),  &
            hhvaroute(18,ihout,ii), hhvaroute(19,ihout,ii),  &
            hhvaroute(20,ihout,ii), hhvaroute(21,ihout,ii),  &
            hhvaroute(22,ihout,ii), hhvaroute(1,ihout,ii)
      ELSE
        WRITE (40+inum1,5000) hhvaroute(2,ihout,ii),  &
            hhvaroute(3,ihout,ii), hhvaroute(4,ihout,ii),  &
            hhvaroute(5,ihout,ii), hhvaroute(6,ihout,ii),  &
            hhvaroute(14,ihout,ii), hhvaroute(15,ihout,ii),  &
            hhvaroute(7,ihout,ii), hhvaroute(16,ihout,ii),  &
            hhvaroute(17,ihout,ii), hhvaroute(13,ihout,ii),  &
            hhvaroute(11,ihout,ii), hhvaroute(12,ihout,ii),  &
            hhvaroute(18,ihout,ii), hhvaroute(19,ihout,ii),  &
            hhvaroute(20,ihout,ii), hhvaroute(21,ihout,ii),  &
            hhvaroute(22,ihout,ii), hhvaroute(1,ihout,ii)
      END IF
    END DO
  ELSE
    IF (inum3 == 0) THEN
      WRITE (40+inum1,5002) iida, iyr, varoute(2,ihout),  &
          varoute(3,ihout), varoute(4,ihout),  &
          varoute(5,ihout), varoute(6,ihout),  &
          varoute(14,ihout), varoute(15,ihout),  &
          varoute(7,ihout), varoute(16,ihout),  &
          varoute(17,ihout), varoute(13,ihout),  &
          varoute(11,ihout), varoute(12,ihout),  &
          varoute(18,ihout), varoute(19,ihout),  &
          varoute(20,ihout), varoute(21,ihout),  &
          varoute(22,ihout), varoute(1,ihout)
    ELSE
      WRITE (40+inum1,5003) varoute(2,ihout),  &
          varoute(3,ihout), varoute(4,ihout),  &
          varoute(5,ihout), varoute(6,ihout),  &
          varoute(14,ihout), varoute(15,ihout),  &
          varoute(7,ihout), varoute(16,ihout),  &
          varoute(17,ihout), varoute(13,ihout),  &
          varoute(11,ihout), varoute(12,ihout),  &
          varoute(18,ihout), varoute(19,ihout),  &
          varoute(20,ihout), varoute(21,ihout),  &
          varoute(22,ihout), varoute(1,ihout)
    END IF
  END IF
END IF

RETURN
5000 FORMAT (1X,i3,1X,i4,1X,i2,1X,18(e11.5,1X),f11.1)
5001 FORMAT (18(e11.5,","),f11.1)
5002 FORMAT (1X,i3,1X,i4,4X,18(e11.5,1X),f11.1)
5003 FORMAT (18(e11.5,","),f11.1)
END SUBROUTINE SAVE
