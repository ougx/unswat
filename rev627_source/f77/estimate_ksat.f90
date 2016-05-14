SUBROUTINE estimate_ksat(perc_clay,esti_ksat)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2015-03-30  Time: 03:56:00

!! This subroutine calculates ksat value for a soil layer
!! given the % of clay in the soil layer

!! Background: Published work of Walter Rawls- calculated
!! ksat values  based on soil texture (sand,silt and clay)
!! idea: there exists a relationship between % clay and Ksat
!! Equations used in this subroutine are based on the above
!! idea (Jimmy willimas).

!! NK June 28,2006


NO TYPE, INTENT(IN)                      :: perc_clay
NO TYPE, INTENT(OUT)                     :: esti_ksat
IMPLICIT NONE

INTEGER::i,eof
REAL::esti_ksat,perc_clay,xc,exksat

exksat=5.0

! print *,"Enter the % clay in the soil layer"
! read *,perc_clay

xc=100.0-perc_clay
esti_ksat=12.7*xc/(xc+EXP(11.45-0.097*xc))+1.0

! print *,"The estimated ksat value is ",min(esti_ksat,exksat)

RETURN

END SUBROUTINE estimate_ksat
