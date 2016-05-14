SUBROUTINE impnd_init
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:01


!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables related to impoundments (ponds,
!!    wetlands, reservoirs and potholes)


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha       |ha            |drainage area of watershed in hectares
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    hru_fr(:)   |none          |fraction of subbasin area in HRU
!!    nhru        |none          |number of HRUs in watershed
!!    pnd_esa(:)  |ha            |surface area of ponds when filled to the
!!                               |emergency spillway
!!    pnd_evol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to
!!                               |fill the ponds to the emergency spillway
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin that drains into
!!                               |pond
!!    pnd_nsed(:) |mg/kg         |normal sediment concentration in pond water
!!    pnd_psa(:)  |ha            |surface area of ponds when filled to
!!                               |principal spillway
!!    pnd_pvol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to
!!                               |fill the ponds to the principal spillway
!!    pnd_sed(:)  |mg/kg         |sediment concentration in pond water
!!    pnd_vol(:)  |10^4 m^3 H2O  |volume of water in pond at any given time
!!    sol_silt(:,:)|%             |percent silt content in soil material
!!    sol_clay(:,:)|%             |percent clay content in soil material
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    wet_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into wetlands
!!    wet_mxsa(:) |ha            |surface area of wetlands at maximum water
!!                               |level
!!    wet_mxvol(:)|10^4 m^3 H2O  |runoff volume from catchment area needed to
!!                               |fill wetlands to maximum water level
!!    wet_nsa(:)  |ha            |surface area of wetlands at normal water
!!                               |level
!!    wet_nsed(:) |mg/kg         |normal sediment concentration in wetland water
!!    wet_nvol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to
!!                               |fill wetlands to normal water level
!!    wet_sed(:)  |mg/L          |sediment concentration in wetland water
!!    wet_vol(:)  |10^4 m^3 H2O  |volume of water in wetlands
!!    wshd_resfr  |none          |fraction of watershed area that drains into
!!                               |reservoirs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bp1(:)      |none          |1st shape parameter for pond surface area
!!                               |equation
!!    bp2(:)      |none          |2nd shape parameter for the pond surface area
!!                               |equation
!!    bw1(:)      |none          |1st shape parameter for wetland surface area
!!                               |equation
!!    bw2(:)      |none          |2nd shape parameter for the wetland surface
!!                               |area equation
!!    pnd_evol(:) |m^3 H2O       |runoff volume from catchment area needed to
!!                               |fill the ponds to the emergency spillway
!!                               |(UNIT CHANGE!)
!!    pnd_nsed(:) |kg/kg         |normal ratio of sediment to water in pond
!!                               |(UNIT CHANGE!)
!!    pnd_pvol(:) |m^3 H2O       |runoff volume from catchment area needed to
!!                               |fill the ponds to the principal spillway
!!                               |(UNIT CHANGE!)
!!    pnd_sed(:)  |kg/kg         |ratio of sediment to water in pond
!!                               |(UNIT CHANGE!)
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond at any given time
!!                               |(UNIT CHANGE!)
!!    sed_stl(:)  |kg/kg         |fraction of sediment remaining suspended in
!!                               |impoundment after settling for one day
!!    sol_silt(:) |none          |fraction silt content in soil material
!!                               |(UNIT CHANGE!)
!!    sol_clay(:,:)|none          |fraction clay content in soil material
!!                               |(UNIT CHANGE!)
!!    wet_mxvol(:)|m^3 H2O       |runoff volume from catchment area needed to
!!                               |fill wetlands to maximum water level
!!                               |(UNIT CHANGE!)
!!    wet_nsed(:) |kg/kg         |ratio of sediment to water in wetland
!!                               |(UNIT CHANGE!)
!!    wet_nvol(:) |m^3 H2O       |runoff volume from catchment area needed to
!!                               |fill wetlands to normal water level
!!                               |(UNIT CHANGE!)
!!    wet_sed(:)  |kg/L          |ratio of sediment to water in wetland
!!                               |(UNIT CHANGE!)
!!    wet_vol(:)  |m^3 H2O       |volume of water in wetlands (UNIT CHANGE!)
!!    wshd_pndfr  |none          |fraction of watershed area which drains into
!!                               |ponds
!!    wshd_pndha  |ha            |watershed area in hectares which drains into
!!                               |ponds
!!    wshd_pndsed |metric tons   |total amount of suspended sediment in ponds
!!                               |in the watershed
!!    wshd_pndv   |m^3           |total volume of water in ponds in the
!!                               |watershed
!!    wshd_resha  |ha            |watershed area in hectares which drains into
!!                               |reservoirs
!!    wshd_wetfr  |none          |fraction of watershed area which drains into
!!                               |wetlands
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cl          |none          |variable to hold calculation result
!!    j           |none          |counter
!!    mnpsz       |none          |mean particle size
!!    lnvol       |none          |variable to hold denominator value
!!    pe_sa       |ha            |local variable to hold value for pnd_esa(:)
!!    pe_vo       |m^3           |local variable to hold value for pnd_evol(:)
!!    pp_sa       |ha            |local variable to hold value for pnd_psa(:)
!!    pp_vo       |m^3           |local variable to hold value for pnd_pvol(:)
!!    sol_sand(:,:) |none          |fraction of sand in soil material
!!    si          |none          |variable to hold calculation result
!!    targ        |10^4 m^3 H2O  |target pond volume
!!    wetdif      |m^3           |difference between maximum and normal amounts
!!                               |of water held in wetlands
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Log10
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

use parm

INTEGER :: j
REAL :: cl, si, mnpsz, targ, lnvol
REAL :: pe_sa, pp_sa, pe_vo, pp_vo, wetdif


DO j = 1, nhru
  
!! calculate the sediment settling rate
  cl = 0.
  si = 0.
  sa = 0.
  mnpsz = 0.
  cl = 0.4100 * sol_clay(1,j) / 100.
  si = 2.7100 * sol_silt(1,j) / 100.
  sa = 5.7000 * sol_sand(1,j) / 100.
  mnpsz = EXP(cl + si + sa)
  sed_stl(j) = EXP(-res_stlr_co * mnpsz)
  
  
!!      set initial pond/wetland parameters
  IF (pnd_fr(j) /= 0.) THEN
    
!! fill in missing parameters
    
    IF (pnd_evol(j) + pnd_pvol(j) > 0.) THEN
      IF (pnd_evol(j) <= 0.) pnd_evol(j) = 1.11 * pnd_pvol(j)
      IF (pnd_pvol(j) <= 0.) pnd_pvol(j) = .9 * pnd_evol(j)
      IF (pnd_psa(j) <= 0.) pnd_psa(j) = 0.08 * pnd_pvol(j)
      IF (pnd_esa(j) <= 0.) pnd_esa(j) = 1.5 * pnd_psa(j)
      targ = 0
      targ = pnd_pvol(j) + .1 * (pnd_evol(j) - pnd_pvol(j))
      IF (pnd_vol(j) > targ) pnd_vol(j) = targ
      
!! convert to new units
      pnd_sed(j) = pnd_sed(j) * 1.e-6     !! mg/L => tons/m^3
      
      pnd_san(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3
      pnd_sil(j) = pnd_sed(j) * 1.e-6 * 1.      !! mg/L => tons/m^3
      pnd_cla(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3
      pnd_sag(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3
      pnd_lag(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3
      
      pnd_nsed(j) = pnd_nsed(j) * 1.e-6   !! mg/L => tons/m^3
      pnd_pvol(j) = 10000. * pnd_pvol(j)  !! 10^4 m^3 => m^3
      pnd_evol(j) = 10000. * pnd_evol(j)  !! 10^4 m^3 => m^3
      pnd_vol(j) = 10000. * pnd_vol(j)    !! 10^4 m^3 => m^3
      
!! calculate watershed pond values
      wshd_pndfr = wshd_pndfr + (hru_dafr(j) * pnd_fr(j))
      wshd_pndv = wshd_pndv + pnd_vol(j)
      wshd_pndsed = wshd_pndsed + pnd_vol(j) * pnd_sed(j)
      
!! calculate shape parameters for surface area equation
      pe_sa = 0.
      pp_sa = 0.
      pp_vo = 0.
      pe_vo = 0.
      pp_vo = pnd_pvol(j)
      pe_vo = pnd_evol(j)
      pe_sa = pnd_esa(j)
      pp_sa = pnd_psa(j)
      IF ((pe_sa - pp_sa) > 0. .AND. (pe_vo - pp_vo) > 0.) THEN
        lnvol = 0.
        lnvol = LOG10(pe_vo) - LOG10(pp_vo)
        IF (lnvol > 1.e-4) THEN
          bp2(j) = (LOG10(pe_sa) - LOG10(pp_sa)) / lnvol
        ELSE
          bp2(j) = (LOG10(pe_sa) - LOG10(pp_sa)) / 0.001
        END IF
        IF (bp2(j) > .9) THEN
          bp2(j) = .9
          bp1(j) = (pnd_psa(j) / pnd_pvol(j)) ** .9
        ELSE
          bp1(j) = (pnd_esa(j) / pnd_evol(j)) ** bp2(j)
        END IF
      ELSE
        bp2(j) = .9
        bp1(j) = (pnd_psa(j) / pnd_pvol(j)) ** .9
      END IF
      
    ELSE
      pnd_fr(j) = 0.
    END IF
    
!! partition pond volume/surface areas between HRUS in subbasin
    pnd_psa(j) = pnd_psa(j) * hru_fr(j)
    pnd_esa(j) = pnd_esa(j) * hru_fr(j)
    pnd_pvol(j) = pnd_pvol(j) * hru_fr(j)
    pnd_evol(j) = pnd_evol(j) * hru_fr(j)
    pnd_vol(j) = pnd_vol(j) * hru_fr(j)
    
  ELSE
    pnd_vol(j) = 0.
  END IF
  
  
  
!!      set initial wetland parameters
  IF (wet_fr(j) /= 0.) THEN
    
!! fill in missing parameters
    IF (wet_mxvol(j) + wet_nvol(j) > 0.) THEN
      IF (wet_mxvol(j) <= 0.) wet_mxvol(j) = 1.11 * wet_nvol(j)
      IF (wet_nvol(j) <= 0.) wet_nvol(j) = .9 * wet_mxvol(j)
      IF (wet_nsa(j) <= 0.) wet_nsa(j) = .08 * wet_nvol(j)
      IF (wet_mxsa(j) <= 0.) wet_mxsa(j) = 1.5 * wet_nsa(j)
      IF (wet_vol(j) <= 0.) wet_vol(j) = wet_nvol(j)
      IF (wet_vol(j) > wet_mxvol(j)) wet_vol(j) = wet_mxvol(j)
      
!! unit conversions
      wet_sed(j) = wet_sed(j) * 1.e-6        !! mg/L => kg/L
      
      wet_san(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L
      wet_sil(j) = wet_sed(j) * 1.e-6 * 1.         !! mg/L => kg/L
      wet_cla(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L
      wet_sag(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L
      wet_lag(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L
      
      wet_nsed(j) = wet_nsed(j) * 1.e-6      !! mg/L => kg/L
      wet_nvol(j) = 10000. * wet_nvol(j)     !! 10^4 m^3 => m^3
      wet_mxvol(j) = 10000. * wet_mxvol(j)   !! 10^4 m^3 => m^3
      wet_vol(j) = 10000. * wet_vol(j)       !! 10^4 m^3 => m^3
      
!! calculate watershed variables
      wshd_wetfr = wshd_wetfr + (hru_dafr(j) * wet_fr(j))
      
!! calculate shape parameters for surface area equation
      wetdif = 0.
      wetdif = wet_mxvol(j) - wet_nvol(j)
      IF ((wet_mxsa(j) - wet_nsa(j)) > 0. .AND. wetdif > 0.) THEN
        lnvol = 0.
        lnvol = LOG10(wet_mxvol(j)) - LOG10(wet_nvol(j))
        IF (lnvol > 1.e-4) THEN
          bw2(j) = (LOG10(wet_mxsa(j)) - LOG10(wet_nsa(j))) / lnvol
        ELSE
          bw2(j) = (LOG10(wet_mxsa(j)) - LOG10(wet_nsa(j))) / 0.001
        END IF
        IF (bw2(j) > 0.9) bw2(j) = .9
        bw1(j) = (wet_mxsa(j) / wet_mxvol(j)) ** bw2(j)
      ELSE
        bw2(j) = .9
        bw1(j) = (wet_nsa(j) / wet_nvol(j)) ** .9
      END IF
    ELSE
      wet_fr(j) = 0.
    END IF
    
    
!! partition wetland volume/surface areas between HRUS in subbasin
    wet_nsa(j) = wet_nsa(j) * hru_fr(j)
    wet_mxsa(j) = wet_mxsa(j) * hru_fr(j)
    wet_nvol(j) = wet_nvol(j) * hru_fr(j)
    wet_mxvol(j) = wet_mxvol(j) * hru_fr(j)
    wet_vol(j) = wet_vol(j) * hru_fr(j)
    
  ELSE
    wet_vol(j) = 0.
  END IF
  
END DO

!!    initialize watershed variables
wshd_resha = wshd_resfr * da_ha
wshd_pndha = wshd_pndfr * da_ha

RETURN
END SUBROUTINE impnd_init
