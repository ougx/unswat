!2014-11-26:  Modification was made to account for saturation excessive runoff
!             when the soil column becomes fully saturated, the ROSS model stops, the recharge rate keeps constant;
!             all rainfall, if larger than recharge it's  stacks on the surface

#define debugMODE
module ROSSMOD
  ![Length Unit] = mm
  ![Time Unit] = hour
  implicit none
  logical,  parameter   ::  debuGGing   =.TRUE.
  !integer,  parameter   ::  IFIN        =20000
  integer,  parameter   ::  MAXNODE     =5000
  integer,  parameter   ::  MAXLAY      =100
  integer,  parameter:: dp=kind(0.d0)                   ! double precision, usage: real(dp) :: a, b, c; 1.0_dp, 3.5_dp, 1.34e8_dp
  !REAL, PARAMETER            ::   RZERO  = 0.0
  !REAL, PARAMETER            ::   RLONE  = 1.0
!  doubleprecision,parameter  ::   DZERO  =0D0
!  doubleprecision,parameter  ::   DHALF  =0.5D0
!  doubleprecision,parameter  ::   DBONE  =1.0D0
!  doubleprecision,parameter  ::   DBTWO  =2.0D0
!  doubleprecision,parameter  ::   DFITH  =5.0D0/3.0D0    !five third, used in overland calculation
!  doubleprecision,PARAMETER  ::   DEPSILON    = EPSILON(1.0D-00)
!  doubleprecision,PARAMETER  ::   GRAVITY      = 9.80665D0
!  doubleprecision,PARAMETER  ::   DBPI      = 3.14159265358979323846264338327950288419716939937510582097494459231D0
  REAL,PARAMETER						 ::		ZERO=0.0e0,HALF=0.5e0,ONE=1.0e0,TWO=2.0e0
  REAL,PARAMETER						 ::		UnitU2S=36.0e0 !m/s to mm/hr, used for overland flow calculation
  REAL,PARAMETER						 ::		GRAVITY      = 9.80665
  !real,     save        ::  TEMPFAC   !temperature factor on K, K'=K*TEMPFAC, TEMPFAC=0.001, if temperature of soil layer < 0 C, otherwise TEMPFAC = 1.0

  real,parameter  ::   H10kPa    =-1000    !mm, 10 kPa field capacity, 0.1 bar
  real,parameter  ::   H33kPa    =-3399.054    !mm, 33 kPa field capacity, 1/3 bar
  real,parameter  ::   H1500kPa    =-152957.43  !mm, -1,500 kPa Permanent Wilting Point, 15 bar
  !real, parameter :: FifBar= -152957.43, ThirdBar = -339.9054				 !used to calculate the field capacity and wilting point
  real,parameter	::	 FiveThd			= 5.e0/3.e0		!five third, used in overland calculation
  real,parameter  ::   CLOSEZERO   =1.0e-5
  real,parameter  ::   gf=1.0
  real,parameter  ::   h0min=-0.02				!min (negative) value for surface pond when it empties.
  real,parameter  ::   Smax=1.001					!max value for layer saturation to allow some overshoot.






  character(len=400)    ::  STRFMT    !format string
  character(len=400)    ::  STRFMT0   !format string
  character(len=400)    ::  STRFMT1   !format string
  character(len=400)    ::  STRFMT2   !format string

  TYPE vars
    INTEGER::isat
    REAL ::h,phi,phiS,K,KS
  END TYPE vars

  !  type SOILCOMP
  !    !integer             ::  IHRU     !index of hru
  !    !integer             ::  IMAT      !soil horizon number
  !    !integer             ::  ILAY      !layer of SWAT soil or aqufier
  !    !real     						::  H       !water pressure after a computation iteration, (L)
  !    !real								::	MFP				!matric flux potential by the Kirchhoff transform, (L2/T)
  !    !real                ::  ZUP, ZDN  !elevations of upper and lower boundaries
  !    real     						::  DZ      	!thickness (L)
  !    real     						::  H      		!hydraulic conductivity
  !    !real     						::  S      		!hydraulic conductivity
  !    !real     ::  CAP       !soil water capacity
  !    !real     ::  WC        !volumetric water content after a computation iteration
  !    real     						::  QW        !inter-nodal water flow (L/T)
  !    real     						::  SROOT     !root uptake
  !    real     						::  SLAT_O    !lateral outflow (to downslope)
  !    real     						::  SLAT_I    !lateral inflow (from inflow)
  !    !type(vars), allocatable :: SOLCOL(:)
  !  end type SOILCOMP

  type SOILMAT
    !parameters
    real                ::  KSAT      	!saturated hydraulic conductivity, (L/T)
    real                ::  HBUB      	!bublle pressure head, (L)
    real                ::  WCS      	!water content at bublle pressure head
    real                ::  WCSR      !saturation - residual
    real                ::  PHIE     	!matric flux potential at bublle pressure head, (L)
    real                ::  KSE      	!detitative of K over S at bublle pressure head, (L/T)
    real                ::  PHISE     !detitative of MP over S at bublle pressure head, (L/T)
    real                ::  WCR     !residual water content
    real                ::  WCFC      !field capacity water content
    real                ::  WCAWC     !available water content
    real                ::  WCWP      !water content at permanent point
    real                ::  LAM       !Brook-Corey parameters define the shapes of the water retention and conductivity curves
    real                ::  ETA      	!Brook-Corey parameters define the shapes of the water retention and conductivity curves, usually ETA = 2 / LAM + 3
    !real                ::  ALPHA     !inverse of the air-entry value (or bubbling pressure), (1/L)
    !real                ::  PSDI      !Pore size distribution index, PSDI > 1
    !real                ::  LPOR      !Pore-connectivity parameter
    !integer(1)          ::  IMODEL    !soil hydraulic properties model:
    !                                                 = 0; Van Genuchten's [1980] model with six parameters.
    !                                                 = 1; Van Genuchten's [1980] model with air-entry value of -2 cm and with six parameters.
    !                                                 = 2; Ippisch [2006]
    !                                                 = 3; Brooks and Corey's [1964] model with six parameters.

    !   ----------------START OF TABULATION DATA----------------   !
    !		integer   ::  TAB_N                             !USED TO DIMENSION THE TABULATION
    !		double precision                ::  TAB_DH      !Step Lenth of TABULATION
    !		double precision,   allocatable ::  TAB_H(:)    !CROSS-SECTIONAL AREA OF WATER
    !		double precision,   allocatable ::  TAB_WC(:)    !STREAMFLOW THROUGH THE CROSS-SECTION
    !		double precision,   allocatable ::  TAB_CAP(:)    !VELOCITY THROUGH THE CROSS-SECTION
    !		double precision,   allocatable ::  TAB_K(:)    !STREAM STAGE AT THE CROSS-SECTION
    !		double precision,   allocatable ::  TAB_SE(:)    !TOP WIDTH AT THE CROSS-SECTION
    !		double precision,   allocatable ::  TAB_dCAP(:)    !VELOCITY THROUGH THE CROSS-SECTION
    !		double precision,   allocatable ::  TAB_dK(:)    !STREAM STAGE AT THE CROSS-SECTION
    !		double precision ::  hTabMin=-1d-2      !value of the upper limit [L] of the pressure head interval below which a table of hydraulic properties will be generated internally for each material
    !		double precision ::  hTabMax=-1d6       !value of the lower limit
    !   ----------------END OF TABULATION DATA----------------   !
  end type SOILMAT

  type SOILCOLUMN
    integer(1)          ::  Lay1_split 		!whether the first layer is splite because 10 mm limit: 1: yse, 0: no split
    integer             ::  ISUB		  		!index of subbasin
    integer             ::  IHRU      		!index of hru within subbasin
    integer             ::  LAYAQ    			!number of aquifer layers
    integer             ::  NLAY    		!number of soil layer and aquifer layers (physical layer)
    integer             ::  NNOD    			!number of total layers of the column (calculation layer)
    integer             ::  NUNS   				!number of unsaturated soil layers

    !integer(1)          ::  ITOPTYPE  		!method used to define top boundary 1 Neuman's method 2 Fisrt Order Exchange Coefficient method
    real     						::  FACFROZN			!factor used to multiply hydraulic conductivity when the soil is frozen
    !real     						::  	DT        !time-step size (hr)
    !real     						::  FACTIM	  		!used to assign the water content during the time step. WC=(1-FACTIM)*WCOLD + FACTIM*WCNEW, 0.5-1.0

    !real     						::  R_MASS	  		!mass balance residual
    !real     						::  DSMAX	  			!maximum change in effective saturation
    !real     						::  DHMAX	  			!maximum change in pressure head

    real     						::  HPOND	  			!depth of ponding water on surface
    !real     						::  DTMIN     		!minimum time step length (hr)
    !real     						::  DTMAX     		!maximum time step length (hr)
    !integer             ::  ITERMAX   		!maximum iteration
    real     						::	hqmin		      !mm, minimum ponding depth to generate surface runoff



    !		------------------------------ ross method ------------------------------
    real     ::  dSmax     	!max change in S (the "effective saturation") of any unsaturated
    !            layer to aim for each time step; controls time step size.	----> 0.05
    real     ::  dSmaxr     !maximum negative relative change in S each time step. This
    !            parameter helps avoid very small or negative S.	----> 0.5
    real     ::  dtmax  		!max time step allowed            ----> 1 hour
    real     ::  dtmin  		!minimum time step allowed            ----> 1 hour
    real     ::  dSfac     	!a change in S of up to dSfac*dSmax is accepted.            ----> 1.25
    real     ::  dpmaxr     !relative change in matric flux potential (MFP) phi that is
    !            accepted for convergence when finding head h at soil interfaces.		----> 0.5


    !------------------------------ non-iterative method ------------------------------
    !		real     ::  err_a     !absolute truncation tolerence    ----> 0.1~0.0001
    !		real     ::  err_r     !relative truncation tolerence    ----> 0
    !		real     ::  safe_fac  !safe factor                      ----> 0.8~0.9
    !		real     ::  r_max     !multiplier constraint            ----> 4.0
    !		real     ::  r_min     !multiplier constraint            ----> 0.1


    !------------------------------ regular iterative method -------------------------
    !		real     ::  DTINI	    !initial time step length (hr)
    !		real     ::  DTMIN     !minimum time step length (hr)
    !		real     ::  DTMAX     !maximum time step length (hr)
    !		real     ::  DTUP      !increase factor (> 1.0, usually 1.5) for the next time step is iteration number is less than 5
    !		real     ::  DTDN      !reduce factor (< 1.0, usually 0.5) for the next time step is iteration number is greater than 10
    !If, during a particular time step, the number of iterations at any time level becomes greater than a
    !prescribed maximum (usually between 10 and [50]), the iterative process for that time level is
    !terminated. The time step is subsequently reset to ��t/4, and the iterative process restarted.
    !real     						::  E1      			!allowable excess

    !integer(1)          ::  ISOLVER    		!Solver of the equations:
    !                                                 = 0; Picard, like HYDRUS
    !                                                 = 1; Picard with time stepping, Kavetski (2001)
    !                                                 = 2; Non-terative, Kavetski (2002)

    !integer             ::  NCOM      		!total number of cells in a column
    !integer             ::  ILCNOD(:) !index of the node representating the layer (center of the layer)
    integer,allocatable ::  ILBNOD(:) 		!index of the ending node representating the layer (bottom of the layer)
    integer,allocatable	::	jt(:)					!index of soil type
    !integer(1)          ::  we
    integer(1)          ::  IHATM 	  		!how to define soil evaporation limit
    !

    real                ::  ZTOP      		!land surface elevation of the HRU(mm)
    real                ::  DEPGW0    		!refernence point of groundwater table (mm)

    real                ::  Kdn    		!hydraulic conductivity of the top skin layer for infiltration (downward) (mm/hour)
    real                ::  Kup    		!hydraulic conductivity of the top skin layer for evaporation (upward) (mm/hour)

    !		real                ::  H1        		!the pressure head below which the roots start to extract water from the soil
    !		real                ::  H2        		!the pressure head below which the roots start to extract water from the soil
    !		real                ::  H3H       		!the limiting pressure head at which the roots cannot extract water at the maximum rate, assuming a potential transpiration rate of r2H
    !		real                ::  H3L       		!the limiting pressure head at which the roots cannot extract water at the maximum rate, assuming a potential transpiration rate of r2L
    !		real                ::  H4        		!the pressure head below which the roots start to extract water from the soil
    !		real                ::  R2H       		!saturated water content
    !		real                ::  R2L       		!saturated water content



    real, allocatable   ::  DZ(:)   		!grid size
    !-------------------- input  -----------------------------
    real                ::  ESMAX     		!daily potential soil evaporation, mm/hr
    real                ::  EPMAX     		!daily potential transpiration, mm/hr
    !real                ::  SNWMELT   		!daily snow melt, mm/hr
    real                ::  QLATIN    		!lateral flow from upstream subbasin, mm/hr
    real, allocatable   ::  RUNON(:)   		!run-on from upstream, mm/hr
    real, allocatable   ::  IRRI(:)   		!irrigation on ground, mm/hr
    real, allocatable   ::  RAIN(:)   		!rainfall on ground, mm/hr

    real                ::  DEPGW     		!current groundwater table depth
    real                ::  DEPROT    		!root depth
    !real                ::  HGW       		 !saturated water content
    !real                ::  WCCRIT    		!water content that evaporation starts to be limited by soil water availibility
    real                ::  HCRIT     		!pressure head that evaporation starts to be limited by soil water availibility

    !-------------------- output -----------------------------
    real                ::  RUNOFF    		!surface runoff, mm
    real                ::  INFIL     		!infiltration, mm
    real                ::  RECHARGE  		!groundwater recharge, mm
    real                ::  ESACT     		!actural soil evaporation, mm
    real                ::  EPACT     		!actural transpiration, mm


    !real                ::  QOV0     !initial overland flow, mm
    real     						::  QOV       		!overland flow, mm/hr
    real     						::  POV       		!overland flow parameter = sqrt(slope)/n/L*36


    !real     ::  QLATOUT(:)  !lateral outflow of each layer, mm/hr
    !real     						::  QINOUT(20)  !used for mass balance computation, mm/hr
    real, allocatable   ::  WC(:)   		!water content
    real, allocatable   ::  QSUM(:)   		!

    type(vars),   allocatable :: var(:)
  end type SOILCOLUMN

  save
  integer	::	nmat	!# of soil materials
  integer	::	nless
  integer	::	slogfile	!file number for message logs
  real, allocatable :: tstep(:)          !time step point of SWAT, in hr: e.g. 0,1,2,3,4....24
  integer ::  mstep           !
  integer ::  IFPROFILE       !file number for soil colume output
  integer ::  IFMAT           !file number for soil horizon output
  integer ::  IFBAL           !file number for water balance
  integer ::  IFDEBUG					!file number for debug info
  type(SOILCOLUMN), target, allocatable :: SOLCOL(:)
  type(SOILMAT),    target, allocatable :: SOLMAT(:)

contains

  pure real function MAT_S2WC(IMAT,S)
    real, intent(in) 								:: 		S
    integer, intent(in) 						::		IMAT
    !real, intent(out) 							::		MAT_S2WC
    if (S>=1.0) then
      MAT_S2WC=SOLMAT(IMAT)%WCS
    else
      MAT_S2WC=SOLMAT(IMAT)%WCR+S*SOLMAT(IMAT)%WCSR
    endif
  end function

  pure real function MAT_WC2S(IMAT,WC)
    real, intent(in) 								:: 		 WC
    integer, intent(in) 						::		IMAT

    if (WC>=SOLMAT(IMAT)%WCS) then
      MAT_WC2S=1.0
    elseif (WC<=SOLMAT(IMAT)%WCR) then
      MAT_WC2S=0.0
    else
      MAT_WC2S=(WC-SOLMAT(IMAT)%WCR)/SOLMAT(IMAT)%WCSR
    endif
  end function

  pure real function MAT_S2H(IMAT,S)
    real, intent(in) 								:: 		S
    integer, intent(in) 						::		IMAT
    if (S>=1.0) then
      MAT_S2H=SOLMAT(IMAT)%HBUB
    else
      MAT_S2H=S**(-1/SOLMAT(IMAT)%LAM)*SOLMAT(IMAT)%HBUB
    endif
  end function

  pure real function MAT_S2MP(IMAT,S)
    real, intent(in) 								:: 		S
    integer, intent(in) 						::		IMAT
    if (S>=1.0) then
      MAT_S2MP=SOLMAT(IMAT)%PHIE
    else
      MAT_S2MP=SOLMAT(IMAT)%PHIE*S**(SOLMAT(IMAT)%ETA-1/SOLMAT(IMAT)%LAM)
    endif
  end function

  pure real function MAT_S2K(IMAT,S)
    real, intent(in) 								:: 		S
    integer, intent(in) 						::		IMAT
    if (S>=1.0) then
      MAT_S2K=SOLMAT(IMAT)%KSAT
    else
      MAT_S2K=SOLMAT(IMAT)%KSAT*S**SOLMAT(IMAT)%ETA
    endif
  end function


  pure real function MAT_WC2H(IMAT,WC)
    real, intent(in) 								:: 		WC
    integer, intent(in) 						::		IMAT

    if (WC>=SOLMAT(IMAT)%WCS) then
      MAT_WC2H=SOLMAT(IMAT)%HBUB
    else
      MAT_WC2H=MAT_S2H(IMAT,MAT_WC2S(IMAT,WC))
    endif
  end function


  pure real function MAT_H2MP(IMAT,H)
    real, intent(in) 								:: 		H
    integer, intent(in) 						::		IMAT
    !real	:: S
    if (H>=SOLMAT(IMAT)%HBUB) then
      MAT_H2MP=SOLMAT(IMAT)%PHIE+SOLMAT(IMAT)%KSAT*(H-SOLMAT(IMAT)%HBUB)
    else
      !S=(H/SOLMAT(IMAT)%HBUB)**(-SOLMAT(IMAT)%LAM)
      !MAT_H2MP=SOLMAT(IMAT)%HBUB*S**(SOLMAT(IMAT)%ETA-1.0/SOLMAT(IMAT)%LAM)
      MAT_H2MP=SOLMAT(IMAT)%PHIE*(H/SOLMAT(IMAT)%HBUB)**(ONE-SOLMAT(IMAT)%ETA*SOLMAT(IMAT)%LAM)
    endif
  end function

  pure real function MAT_MP2H(IMAT,MP)
    real, intent(in) 								:: 		MP
    integer, intent(in) 						::		IMAT
    !real	::	S
    if (MP>=SOLMAT(IMAT)%PHIE) then
      MAT_MP2H=SOLMAT(IMAT)%HBUB+(MP-SOLMAT(IMAT)%PHIE)/SOLMAT(IMAT)%KSAT
    else
      !S=(MP/SOLMAT(IMAT)%PHIE)**(1/(SOLMAT(IMAT)%ETA-1/SOLMAT(IMAT)%LAM))
      !MAT_MP2H=S**(-1/SOLMAT(IMAT)%LAM)*SOLMAT(IMAT)%HBUB
      MAT_MP2H=SOLMAT(IMAT)%HBUB*(MP/SOLMAT(IMAT)%PHIE)**(ONE/(ONE-SOLMAT(IMAT)%ETA*SOLMAT(IMAT)%LAM))
    endif
  end function

  pure real function MAT_H2S(IMAT,H)
    real, intent(in) 								:: 		H
    integer, intent(in) 						::		IMAT
    if (H>=SOLMAT(IMAT)%HBUB) then
      MAT_H2S=1.0
    else
      MAT_H2S=(H/SOLMAT(IMAT)%HBUB)**(-SOLMAT(IMAT)%LAM)
    endif
  end function


  pure real function MAT_MP2S(IMAT,MP)
    real, intent(in) 								:: 		MP
    integer, intent(in) 						::		IMAT
    if (MP>=SOLMAT(IMAT)%PHIE) then
      MAT_MP2S=1.0
    else
      MAT_MP2S=(MP/SOLMAT(IMAT)%PHIE)**(1/(SOLMAT(IMAT)%ETA-1/SOLMAT(IMAT)%LAM))
    endif
  end function

  pure real function MAT_MP2WC(IMAT,MP)
    real, intent(in) 								:: 		MP
    integer, intent(in) 						::		IMAT
    real	::	S
    if (MP>=SOLMAT(IMAT)%PHIE) then
      MAT_MP2WC=SOLMAT(IMAT)%WCS
    else
      MAT_MP2WC=MAT_S2WC(IMAT,MAT_MP2S(IMAT,MP))
    endif
  end function

  pure real function MAT_H2K(IMAT,H)
    real, intent(in) 								:: 		H
    integer, intent(in) 						::		IMAT
    if (H>SOLMAT(IMAT)%HBUB) then
      MAT_H2K=SOLMAT(IMAT)%KSAT
    else
      MAT_H2K=SOLMAT(IMAT)%KSAT*MAT_H2S(IMAT,H)**SOLMAT(IMAT)%ETA
    endif
  end function

  pure real function MAT_MP2K(IMAT,MP)
    real, intent(in) 								:: 		MP
    integer, intent(in) 						::		IMAT
    if (MP>=SOLMAT(IMAT)%PHIE) then
      MAT_MP2K=SOLMAT(IMAT)%KSAT
    else
      MAT_MP2K=SOLMAT(IMAT)%KSAT*MAT_MP2S(IMAT,MP)**SOLMAT(IMAT)%ETA
    endif
  end function

  pure real function MAT_H2WC(IMAT,H)
    real, intent(in) 								:: 		H
    integer, intent(in) 						::		IMAT
    if (H>=SOLMAT(IMAT)%HBUB) then
      MAT_H2WC=SOLMAT(IMAT)%WCS
    else
      MAT_H2WC=MAT_S2WC(IMAT,MAT_H2S(IMAT,H))
    endif
  end function

  FUNCTION weight(j,h,K,phi,dz)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::j
    REAL,INTENT(IN)::h,K,phi,dz
    REAL::weight
    ! Get conductivity weighting for gravity flux calculations.
    ! Definitions of arguments:
    ! j   - soil type no.
    ! h   - matric head.
    ! K   - conductivity.
    ! phi - MFP.
    ! dz  - flow path length.
    LOGICAL::done
    REAL::a,hz,Khz,Kz,phiz,w,x
    TYPE(SOILMAT),POINTER::p
    p=>SOLMAT(j); done=.false.
    hz=h-gf*dz ! gf is gravity fac in direction of dz
    if (h<p%HBUB) then
      a=p%lam*p%eta; x=-gf*dz/h
      if (a<=3.0.or.x*(a-3.0)<=4.0) then ! use predetermined approx.
        w=(60.0+x*(70.0+10.0*a+x*(16.0+a*(5.0+a))))/ &
          (120.0+x*(120.0+x*(22.0+2.0*a**2)))
        done=.true.
      end if
    end if
    if (.not.done) then
      call hyofh(hz,j,Kz,Khz,phiz) ! accurate but slower
      if (abs(Kz-K)<CLOSEZERO) then		!OGXinROSS
        w=0.5
      else
        w=-((phiz-phi)/(gf*dz)+K)/(Kz-K)
      endif
    end if
    weight=min(max(w,zero),one)
  END FUNCTION weight

  SUBROUTINE hyofS(S,nl,jt,var)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::nl,jt(:)
    REAL,INTENT(IN)::S(:)
    TYPE(vars),DIMENSION(:),TARGET,INTENT(OUT)::var
    ! Get soil water variables from S.
    ! Definitions of arguments:
    ! S(1:nl)   - degree of saturation ("effective satn") of layers.
    ! nl        - no. of soil layers.
    ! jt(1:nl)  - layer soil type nos.
    ! var(1:nl) - other water vars of layers.
    INTEGER::i,j
    REAL::lnS,v1,v2
    TYPE(SOILMAT),POINTER::p
    TYPE(vars),POINTER::v
    do i=1,nl
      if (S(i)<one) then
        j=jt(i); v=>var(i)
        p=>SOLMAT(j)
        lnS=log(S(i))
        v1=exp(-lnS/p%LAM); v2=exp(p%ETA*lnS)
        v%h=p%HBUB*v1; v%K=p%KSAT*v2; v%phi=p%phie*v1*v2
        v%KS=p%eta*v%K/S(i); v%phiS=(p%eta-one/p%lam)*v%phi/S(i)
        !v%wc=S(i)*p%WCS
      end if
    end do
  END SUBROUTINE hyofS

  SUBROUTINE hyofh(h,j,K,Kh,phi)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::j
    REAL,INTENT(IN)::h
    REAL,INTENT(OUT)::K,Kh,phi
    ! Get soil water variables from h.
    ! Definitions of arguments:
    ! h   - matric head.
    ! j   - soil type no.
    ! K   - hydraulic conductivity.
    ! Kh  - derivative dK/dh.
    ! phi - matric flux potential (MFP).
    REAL::a
    TYPE(SOILMAT),POINTER::p
    p=>SOLMAT(j); a=-p%lam*p%eta
    K=p%KSAT*exp(a*log(h/p%HBUB)); Kh=a*K/h ! exp and log may be faster than **
    phi=K*h/(one+a)
  END SUBROUTINE hyofh


  function ResidWatCon(Clay, Sand, Porosity)
    !Clay %; Sand %; Porosity %.
    !Calculate Brooks-Corey residual water content (Handbook of Hydrology, p. 5.15)
    ! Note that this value is much less than lower limit (wilting point)
    real :: Clay, Sand, Porosity, ResidWatCon
    ResidWatCon = -0.0182482 + (0.00087269 * Sand) + (0.00513488 * Clay) &
      + (0.02939286 * Porosity) - (0.00015395 * Clay ** 2) &
      - (0.0010827 * Sand * Porosity) - (0.00018233 * Clay ** 2 * Porosity ** 2) &
      + (0.00030703 * Clay ** 2 * Porosity) - (0.0023584 * Porosity ** 2 * Clay)
  end function

  function PSDI(Clay, Sand, Porosity)
    !Calculate Brooks-Corey pore size distribution index, LAM (Handbook of Hydrology, p. 5.15)
    !  Note that LAM = 1/b, where b is Campbell's PSDI
    real :: Clay, Sand, Porosity, PSDI
    PSDI = Exp(-0.7842831 + (0.0177544 * Sand) - (1.062498 * Porosity) &
      - (0.00005304 * Sand ** 2) - (0.00273493 * Clay ** 2) + (1.11134946 * Porosity ** 2) &
      - (0.03088295 * Sand * Porosity) + (0.00026587 * Sand ** 2 * Porosity ** 2) &
      - (0.00610522 * Clay ** 2 * Porosity ** 2) - (0.00000235 * Sand ** 2 * Clay) &
      + (0.00798746 * Clay ** 2 * Porosity) - (0.00674491 * Porosity ** 2 * Clay))
  end function

  function BubPres(Clay, Sand, Porosity)
    !Calculate Brooks-Corey bubbling pressure [cm] (Handbook of Hydrology, p. 5.15)
    real :: Clay, Sand, Porosity, BubPres
    BubPres = Exp(5.3396738 + (0.1845038 * Clay) - (2.48394546 * Porosity) &
      - (0.00213853 * Clay ** 2) - (0.04356349 * Sand * Porosity) - (0.61745089 * Clay * Porosity) &
      + (0.00143598 * Sand ** 2 * Porosity ** 2) - (0.00855375 * Clay ** 2 * Porosity ** 2) &
      - (0.00001282 * Sand ** 2 * Clay) + (0.00895359 * Clay ** 2 * Porosity) &
      - (0.00072472 * Sand ** 2 * Porosity) + (0.0000054 * Clay ** 2 * Sand) &
      + (0.5002806 * Porosity ** 2 * Clay))
  end function

  subroutine SOLCOL_Update_Storage(k)
    !this subroutine sums up the soil water storage for SWAT layer and the entire hru
    !input: m, n, jt, var
    !output: wc, totwc
    use parm, only: sol_sw,sol_st,sol_nly
    integer       ::  k           !ID of column
    !    integer       ::  m           !# of unsaturated
    !    integer       ::  n           !total # of cells
    !    integer       ::  jt(n)       !soil type
    !    type(vars)    ::  var(n)      !soil condition
    !    !real          ::  wc(n)       !(mm/mm) soil moisture,
    !    real          ::  totwc       !(mm) total water mass of the column, including soil water content and groundwater storage
    !---------------------------------------------------------------------------------
    integer       ::  l,iis,iie
    type(SOILCOLUMN), pointer :: scol
    scol=>SOLCOL(k)

    do l=1, sol_nly(k)
      iis=scol%ILBNOD(l-1)+1
      iie=scol%ILBNOD(l)
      sol_st(l,k)=sum(scol%WC(iis:iie)*scol%DZ(iis:iie))
    enddo
    sol_sw(k)=sum(sol_st(:,k))

  end subroutine



  !	function SOLCOL_WATER_MASS(kHRU, nCOM1, nCOM2)
  !		!report mass of the soil column (L)
  !		!storage change + top flow + bottom flow
  !		!water ponding on the top is not included
  !		implicit none
  !		integer :: kHRU, nCOM1, nCOM2
  !		real :: SOLCOL_WATER_MASS
  !		real :: WCSAT1,WCSAT2, WCLAY, DZ, WCCOL, HE1,HE2
  !		integer :: iCOM,iMAT1,iMAT2
  !		type(SOILMAT), pointer :: mat1,mat2
  !
  !		WCCOL=0.0
  !		do iCOM=nCOM1, nCOM2
  !			mat1=>SOLMAT(SOLCOL(kHRU)%jt(iCOM))
  !			HE1=mat1%HBUB
  !			!HE2=SOLMAT(iMAT2)%HBUB
  !			DZ=SOLCOL(kHRU)%DZ(iCOM)
  !
  !			if (SOLCOL(kHRU)%SOLCMP(iCOM)%H   >HE1) then
  !				!saturated node, separate saturated and unsaturated components
  !				WCSAT2=SOLMAT(iMAT2)%WCS
  !				if (SOLCOL(kHRU)%SOLCMP(iCOM)%H   >HE1) then
  !					!fully saturated
  !					WCSAT1=mat1%WCS
  !					WCLAY=DZ*(WCSAT2+WCSAT1)*0.5
  !				else
  !					!partially saturated
  !					WCLAY=(SOLCOL(kHRU)%SOLCMP(iCOM)%WC+WCSAT2)*0.5
  !					WCLAY=WCLAY*(DZ-SOLCOL(kHRU)%SOLCMP(iCOM+1)%H+HE1)
  !					WCLAY=WCLAY+(SOLCOL(kHRU)%SOLCMP(iCOM)%H-HE1)*WCSAT2
  !				endif
  !			else
  !				!fully unsaturated nodes
  !				WCLAY=(SOLCOL(kHRU)%SOLCMP(iCOM)%WC+SOLCOL(kHRU)%SOLCMP(iCOM+1)%WC)*0.5
  !				WCLAY=WCLAY*DZ
  !			endif
  !
  !			WCCOL=WCCOL+WCLAY
  !		enddo
  !		SOLCOL_WATER_MASS=WCCOL
  !		return
  !	end function
  SUBROUTINE solve_steady(nn,dx,jt,htop,hbot,var,qflux,wc)

    IMPLICIT NONE
    !integer, parameter :: nn=10
    INTEGER,INTENT(IN)::nn
    INTEGER,INTENT(IN)::jt(nn)
    REAL,INTENT(IN):: dx(nn),htop,hbot
    TYPE(vars),INTENT(INOUT)::var(nn)
    REAL,INTENT(INOUT)::qflux,wc(nn)
    !REAL(RK),INTENT(OUT):: S(nn)

    LOGICAL :: convergence
    INTEGER::i, iTER
    REAL,DIMENSION(nn+1)::K,H,Htmp,dz
    REAL,DIMENSION(nn+1)::aa,bb,cc,dd,ee

    REAL  :: khalf1,khalf2,TolH
    REAL  :: r1,r2
    integer :: n


    n=nn+1
    TolH=0.001
    dz=ZERO
    dz(1:nn-1)=HALF*(dx(1:nn-1)+dx(2:nn))
    dz(nn)=HALF*dx(nn)

    aa=zero
    bb=zero
    cc=zero
    dd=zero
    !initial guess: linear distribution
    !parameters
    r1=(htop-hbot)/(sum(dz)-HALF*dz(1))
    H(1)=htop
    H(n)=hbot
    do i=nn,2,-1
      H(i)=H(i+1)+dz(i)*r1
    enddo

    !top boundary
    aa(1)=zero
    bb(1)=one
    cc(1)=zero
    dd(1)=htop
    K(1)=MAT_H2K(jt(1),htop)

    !bottom boundary
    !hbot=min(SOLMAT(jt(nn))%HBUB,hbot)
    cc(n)=zero
    bb(n)=one
    aa(n)=zero
    dd(n)=hbot    !top of the saturated zone (top of capilary zone)
    K(n)=MAT_H2K(jt(nn),hbot)

#ifdef debugMODE
    !write (IFDEBUG,"(6A20)") 'i','Conductivity','H(i)','HTMP(i)','H(i)-HTMP(i)','TolH'
#endif

    do iTER=1,3000
      !store initial values
      Htmp=H

      !calculate parameters
      do i=2, nn
        K(i)=MAT_H2K(jt(i),H(i))
      enddo

      !if (debuGGing) write(*,*) K
      !GENERATE TERMS OF MATRIX EQUATION
      i=1

      khalf2=half*(k(i+1)+k(i))
      r2=khalf2/dz(i)


      do i=2, nn
        r1=r2
        khalf1=khalf2
        khalf2=half*(k(i+1)+k(i))
        r2=khalf2/dz(i)

        aa(i)=-r1
        bb(i)=r1+r2
        cc(i)=-r2
        dd(i)=khalf1-khalf2

      enddo


      !if (debuGGing) write(*,*) aa

      !solve the matrix
      call tri(0,nn,aa,bb,cc,dd,ee,H)
      !call solve_tridiag(1,nn,aa,bb,cc,dd,H)

      !check convergence
      convergence=.true.
      do i = 1, nn
        !call SOLNOD_H2ALL(kHRU,iidx(1:nn-1),HNEW(1:nn-1), WCNEW(1:nn-1), K1(1:nn-1), CAP(1:nn-1))
        !convergence=all((HNEW  - Htmp )>TolH )
        !convergence=all((WCNEW - WCTMP)>TolWC)
        if (abs(H(i)-Htmp(i))>TolH)  then
          convergence=.false.
#ifdef debugMODE
          !write (IFDEBUG,"(I20,6G20.11)") i,K(i),H(i),Htmp(i),abs(H(i)-Htmp(i)),TolH,dz(i)
#endif
          !if (debuGGing)
        endif

      enddo

      if (convergence) exit
    enddo   !iteration loop: iTER

    if (.not. convergence) then
      write (*,*) 'solve_steady: Reach the maximum iteration number, steady state solution not archived'
      write (*,*) 'solve_steady: The initial value will be set as the average of the last two iteration.'
      write (IFDEBUG,*) 'solve_steady: Reach the maximum iteration number, steady state solution not archived'
      write (IFDEBUG,*) 'solve_steady: The initial value will be set as the average of the last two iteration.'
      write (IFDEBUG,"(6A20)") 'i','DX(i)','K(i)','H(i)','TEMP_H(i)',"Diff"
      do i=1,nn
        write (IFDEBUG,"(I20,5G20.11)") i,dx(i),K(i),H(i),Htmp(i),abs(Htmp(i)-H(i))
      enddo
      H=0.5*(H+Htmp)
    endif
    !output
    qflux=khalf1*((H(nn-1)-H(nn))/dz(nn-1)+1)

#ifdef debugMODE
    write (IFDEBUG,"(3a5,6A15)") 'i',"isat",'mat','DX(i)','K(i)','H(i)','phi(i)',"WC","S"
#endif
    do i=1,nn
      !!update status
      if (H(i)>SOLMAT(jt(i))%HBUB) then
        var(i)%isat=1
        var(i)%K=SOLMAT(jt(i))%KSAT
        var(i)%KS=ZERO
        var(i)%phiS=ZERO
      else
        var(i)%isat=0
      endif
      var(i)%h=H(i)
      var(i)%phi=MAT_H2MP(jt(i),H(i))
      wc(i)=MAT_H2WC(jt(i),H(i))

#ifdef debugMODE
      write (IFDEBUG,"(3I5,7E15.7)") i,var(i)%isat,jt(i),dx(i),K(i),H(i),var(i)%phi,wc(i), &
        MAT_H2S(jt(i),H(i)),MAT_MP2H(jt(i),var(i)%phi)
#endif

    enddo

#ifdef debugMODE
    write (IFDEBUG,*) "qflux is ", qflux
    !write (IFDEBUG,*) "khalf2, H(nn-1), H(nn), dz(nn-1),hbot", khalf1, H(nn-1), H(nn), dz(nn-1),hbot
    !write (IFDEBUG,*) "K*(Hn-Hbot)*2.0/dz(i)-K ", khalf2*((H(nn)-hbot)/dz(nn)+1)
#endif


  end subroutine solve_steady


  SUBROUTINE solve(k,ts,tfin,qprec,qevap,n,dx,jt,hqmin,hETmin,hbot,h0,Kd,Ku,var,fzn,snl,kappa,evap,runoff,infil,drn, &
      qsum,sicum,socum,srcum,dtmin,dtmax,dSmax,dSmaxr,dSfac,dpmaxr)
    IMPLICIT NONE
    INTEGER,INTENT(IN)::k,n,jt(n)
    REAL,INTENT(IN):: ts,tfin,qprec,qevap,hbot
    REAL,INTENT(IN)::hqmin,hETmin,dtmin,dtmax,dSmax,dSmaxr,dSfac,dpmaxr,Kd,Ku
    REAL,INTENT(IN)::dx(n),fzn(n)
    TYPE(vars),DIMENSION(n),TARGET,INTENT(INOUT)::var
    !INTEGER,INTENT(INOUT)::msteps
    REAL, INTENT(INOUT)::h0,evap,runoff,infil,drn
    REAL,DIMENSION(0:n), INTENT(INOUT)::qsum
    REAL,DIMENSION(n), INTENT(INOUT)::sicum,socum,srcum
    REAL, intent(in)::snl   !OGX: slope/ n/ L_s; Manning's equation to calculate the overland flow
    REAL, intent(in)::kappa   !OGX: used with Manning's equation to calculate the overland flow

    ! Solves the RE
    ! Definitions of arguments:
    ! Required args:
    ! k 			- index number of soil column (HRU)
    ! qprec   - precipitation (or water input) rate (fluxes are in mm/h).
    ! qevap   - potl evaporation rate from soil surface.
    ! n       - no. of soil layers.
    ! nsol    - no. of solutes.
    ! dx(1:n) - layer thicknesses.
    ! jt(1:n) - layer soil type nos.
    ! h0      - surface head, equal to depth of surface pond.
    ! S(1:n)  - degree of saturation ("effective satn") of layers.
    ! evap    - cumulative evaporation from soil surface (mm, not initialised).
    ! runoff  - cumulative runoff.
    ! infil   - cumulative net infiltration (time integral of flux across surface).
    ! drn     - cumulative net drainage (time integral of flux across bottom).
    ! msteps  - cumulative no. of time steps for RE soln.
    ! Optional args:
    ! heads(1:n)      - matric heads h of layers at finish.
    ! qexsub          - subroutine to get layer water extraction rates (cm/h) by
    !                   plants. Note that there is no solute extraction and osmotic
    !                   effects due to solute are ignored. Arguments:
    !                   jt(1:n) - layer soil type nos; h(1:n) - layer matric heads;
    !                   qex(1:n) - layer extraction rates; qexh(1:n) - partial
    !                   derivs of qex wrt h.
    ! wex(1:n)        - cumulative water extraction from layers.
    ! cin(1:nsol)     - solute concns in water input (user's units/cc).
    ! c0(1:nsol)      - solute concns in surface pond.
    ! sm(1:n,1:nsol)  - solute (mass) concns in layers.
    ! soff(1:nsol)    - cumulative solute runoff (user's units).
    ! sinfil(1:nsol)  - cumulative solute infiltration.
    ! sdrn(1:nsol)    - cumulative solute drainage.
    ! nssteps(1:nsol) - cumulative no. of time steps for ADE soln.
    ! isosub          - subroutine to get adsorbed solute (units/g soil) from concn
    !                   in soil water according to chosen isotherm code.
    !                   Arguments: iso - 2 character code; c - concn in soil water;
    !                   p(:) - isotherm parameters; f - adsorbed mass/g soil;
    !                   fc - deriv of f wrt c (slope of isotherm curve). Note that
    !                   linear adsorption does not require a sub, and other types
    !                   are available in sub isosub.
    LOGICAL again,getq0,getqn,init,initpond,nexthour,pondmove,pond
    INTEGER::i,iflux,ih0,iok,itmp,j,ns,nsat,nsatlast,msteps0,it,msteps
    REAL::accel,dmax,dt,dwinfil,dwoff,fac,infili,KhETmin1,Kmin1,phimin1,phip, &
      qpme,qprec1,rsig,rsigdt,sig,t,ti,win
    real:: snlmm
    real:: prectot,str0,str1,roff,rlat
    REAL,DIMENSION(1)::Sbot
    REAL,DIMENSION(n-1)::dz
    REAL,DIMENSION(n)::hint,phimin
    REAL,DIMENSION(n)::qt,qli,qlo
    REAL,DIMENSION(n)::thi,thf
    REAL,DIMENSION(n)::S
    REAL,DIMENSION(0:n)::aa,bb,cc,dd,dy,ee,q,qya,qyb
    !REAL,DIMENSION(nsol)::cav,sinfili
    !REAL,DIMENSION(n,nsol)::c
    TYPE(vars)::vtop,vbot
    TYPE(vars)::vcall(1)
    TYPE(vars),POINTER::v
    TYPE(SOILMAT),POINTER::p
    ! The derived types params and vars hold soil water parameters and variables.
    ! Parameter names often end in e, which loosely denotes "air entry", i.e.,
    ! values at h=HBUB. While values of water content th and hydraulic conductivity K
    ! at h=HBUB are equal to those at saturation, the derivs wrt S are nonzero. The
    ! MFP phi for h>HBUB is given by phi=phie+KSAT*(h-HBUB). The saturation status of a
    ! layer is stored as 0 or 1 in isat since S may be >1 (because of previous
    ! overshoot) when a layer desaturates. Fluxes at the beginning of a time step
    ! and their partial derivs wrt S or phi of upper and lower layers or boundaries
    ! are stored in q, qya and qyb.


    !REAL::h0et,S1et,K0et,Kh0et,phi0et !OGX:
    REAL::qhov  !OGX: derivative of q_ov WRT h_0
    REAL::qov   !OGX: overland flow/L_s, q_surf in the paper
    !TYPE(vars)::v0et  !OGX
    logical :: limitET, SatEx
    REAL,DIMENSION(n)::sli,slo,srt,qex

    snlmm=snl
    j=jt(1); p=>SOLMAT(j)
    phip=max(p%phie-p%HBUB*p%KSAT,1.00001*p%phie) ! phi at h=0
    ! get K, Kh and phi at hETmin (hETmin is smallest h, stored in hyprops)
    !OGX: hETmin is the lowest value control ET
    call hyofh(hETmin,j,Kmin1,KhETmin1,phimin1)
    dz=half*(dx(1:n-1)+dx(2:n)) ! flow paths

    !----- set up for boundary conditions
    getq0=.true.
    getqn=.true.
    j=jt(n); p=>SOLMAT(j)
    !hbot=ZERO
    vbot=vars(1,hbot,(hbot-p%HBUB)*p%KSAT+p%phie,zero,p%KSAT,zero)
    !----- end set up for boundary conditions
    !----- initialise

#ifdef debugMODE
    !write(IFDEBUG,*)
    !write(IFDEBUG,*)  "Phi_n-1, H_n-1", var(n-1)%phi,MAT_MP2H(jt(n-1),var(n-1)%phi),var(n-1)%isat
    !write(IFDEBUG,*)  "Phi_n, H_n", var(n)%phi,MAT_MP2H(jt(n),var(n)%phi),var(n)%isat
#endif
    t=ts
    msteps=0
    msteps0=msteps
    ! initialise saturated regions
    !			var%isat=0
    !			where (S>=one)
    !				var%phi=SOLMAT(jt)%phie; var%K=SOLMAT(jt)%KSAT
    !				var%isat=1
    !			end where
    !			if (nsol>0) then
    !				! set solute info
    !				thi=SOLMAT(jt)%the-SOLMAT(jt)%WCSR*(one-S) ! initial th (note: thre=the-thr)
    !				ti=t; infili=infil; sinfili=sinfil
    !				if (h0>zero.and.count(c0/=cin)>0) then
    !					initpond=.true. ! initial pond with different solute concn
    !				else
    !					initpond=.false.
    !				end if
    !				c=zero ! temp storage for soln concns
    !			end if
    !S=ZEROqya
    do i=1,n
      S(i)=MAT_MP2S(jt(i),var(i)%phi) !calculate S of each node
    enddo
#ifdef debugMODE
    !write (IFDEBUG,*) 'S is:   ', S(1:n)
    !write (IFDEBUG,*) 'isat is:   ', var(1:n)%isat
    !prectot=zero
#endif

    where (S>=one) var%K=SOLMAT(jt)%KSAT

    !----- end initialise
    !----- solve until tfin

    str0=sum((SOLMAT(jt)%WCS-SOLMAT(jt)%WCSR*(1.0-S))*dx)
    qpme=qprec-qevap ! input rate

    do while (t<tfin)

      if (all(var%isat==1)) then
        !call USTOP("Fully Saturated, ISAT = 1")

        !saturation excessive runflow
        SatEx=.true.
        !time left
        dt=tfin-t
        !examine if flowin > flowout, assuming the bottom is free drainage, unit hydraulic gradient.
        if (qpme>SOLMAT(jt(n))%KSAT+SOLCOL(k)%EPMAX) then

          !runoff = runoff +
        endif

      endif

      !----- take next time step
      do iflux=1,2 ! sometimes need twice to adjust phi at satn

        pondmove=.false.
        pond=.false.
        limitET=.false.
        initpond=.false.
        SatEx=.false.

        if (msteps==msteps0.and.iflux==1) then
          init=.true. ! flag to initialise h at soil interfaces
        else
          init=.false.
        end if
        nsatlast=nsat ! for detecting onset of profile saturation
        nsat=sum(var%isat) ! no. of sat layers
        sig=half; IF (nsat/=0) sig=one ! time weighting sigma
        rsig=one/sig
        ! update variables
        if (iflux==1) call hyofS(S,n,jt,var) ! for layers where S<1
        ! phi is solution var at satn, so h calc from phi where S>=1
        where (S>=one)
          var%h=SOLMAT(jt)%HBUB+(var%phi-SOLMAT(jt)%phie)/SOLMAT(jt)%KSAT
          !var%K=SOLMAT(jt)%KSAT
        end where

        var%K=var%K*fzn
        !----qya- get fluxes and derivs
        ! get surface condition
        p=>SOLMAT(jt(1))
        !          if (S(1)<S1et.and.qevap>qprec) then !OGX: restrited ET
        !            ns=1
        !            vtop=v0et
        !          else

        if (h0>zero) then
          ! ponding
          ns=0
          if (Kd>ZERO) then
              !            h, phi,                 phiS,K,   KS
            vtop=vars(1,h0,(h0-p%HBUB)*p%KSAT+p%phie,zero,Kd,ZERO)
          else
            !            h, phi,                 phiS,K,   KS
            vtop=vars(1,h0,(h0-p%HBUB)*p%KSAT+p%phie,zero,p%KSAT,ZERO)
          endif

        elseif (qpme<ZERO) then
          !et flux boundary
          ns=1 ! start index for eqns
          if (Ku>ZERO) then     !OGX: hETmin will be threthold for restrited ET
            !            h,  phi,    phiS,K,    KS
            vtop=vars(0,hETmin,phimin1,zero,Ku,ZERO) ! vars at soil surface
          else
            !            h,  phi,    phiS,K,    KS
            vtop=vars(0,hETmin,phimin1,zero,Kmin1,ZERO) ! vars at soil surface
          endif
        else
          ns=1
          if (Kd>ZERO) then
              !            h, phi,                 phiS,K,   KS
            vtop=vars(1,hqmin,(hqmin-p%HBUB)*p%KSAT+p%phie,zero,Kd,ZERO)
          else
            !           h,    phi,                      phiS,K,   KS
            vtop=vars(1,hqmin,(hqmin-p%HBUB)*p%KSAT+p%phie, zero,p%KSAT,ZERO)
          endif
        endif


        !          if (var(1)%phi<=phip.and.h0<=zero.and.nsat<n) then ! no ponding
        !            ns=1 ! start index for eqns
        !            !            h,  phi,    phiS,K,    KS
        !            vtop=vars(0,hETmin,phimin1,zero,Kmin1,ZERO) ! vars at soil surface  !OGX: hETmin will be threthold for restrited ET
        !            qov=zero
        !          else
        !            ns=0
        !            !            h, phi,                 phiS,K,   KS
        !            vtop=vars(1,h0,(h0-p%HBUB)*p%KSAT+p%phie,zero,p%KSAT,ZERO)
        !          end if
        ! get bottom bdry condn
        !          if (botbc=="seepage") then
        !            if (var(n)%h>-half*gf*dx(n)) then
        !              getqn=.true.
        !              p=>SOLMAT(jt(n))
        !              vbot=vars(1,zero,(zero-p%HBUB)*p%KSAT+p%phie,zero,p%KSAT,zero)
        !            else
        !              getqn=.false.
        !            end if
        !          end if
        ! get fluxes
        !if (debuGGing) write(IFDEBUG,*) "S,K:", S
        !if (debuGGing) write(IFDEBUG,*) "dz ", dz
        !if (debuGGing) write(IFDEBUG,*) "getq0,getqn: ", getq0,getqn
        !if (debuGGing) write(IFDEBUG,*) "vtop: ", vtop
        !if (debuGGing) write(IFDEBUG,*) "vbot: ", vbot
        !if (debuGGing) call print_var(IFDEBUG, n, var, t)
        call getfluxes(n,jt,dx,dz,vtop,vbot,var,hint,phimin,q,qya,qyb, &
          iflux,init,getq0,getqn,dpmaxr)
        ! adjust for top and bottom bdry condns
        !qprec1=qprec(it) ! may change qprec1 to maintain pond if required
        qov=zero
        qhov=zero
        if (ns==1) then   !OGX: starting index is 1
          if (qpme<zero) then
            if (q(0)<qpme) then
              q(0)=qpme; qyb(0)=zero    !(partial q0)/ (partial S1) = 0
            else
              limitET=.true.    !OGX: the q is "negatively" larger than the possible ET rate
            end if
          else
            if (q(0)>qpme) then
              q(0)=qpme; qyb(0)=zero
            else
              !OGX: initial ponding occurs
              !OGX: the top boundary is set as hqmin
              !OGX: the rest of infiltration will become runoff
              initpond= .true.
            end if
          endif


          ! correction 10/2/2010
          !maxpond=.false.
        else  !OGX: starting index is 0, ponding

          !OGX: comment the following lines for runoff generation
          !            if (h0>=h0max.and.qpme>q(0)) then
          !              maxpond=.true.
          !              ns=1
          !            else
          !              maxpond=.false.
          !            end if
          !maxpond=.true.
          ! change qya(0) from dq/dphi (returned by getfluxes) to dq/dh
          qya(0)=SOLMAT(jt(1))%KSAT*qya(0)
          pond= .true.

          !OGX: include runoff
          if (h0>=hqmin) then
            qov=snlmm*h0**kappa
            qhov=kappa*snlmm*h0**(kappa-1.0)
          endif

        end if
        !          if (botbc/="constant head") then
        !            select case (botbc)
        !              case ("zero flux")
        !                q(n)=zero
        !                qya(n)=zero
        !              case ("free drainage")
        !                v=>var(n)
        !                q(n)=gf*v%K
        !                if (v%isat==0) then
        !                  qya(n)=gf*v%KS
        !                else
        !                  qya(n)=zero
        !                end if
        !              case ("seepage")
        !                if (var(n)%h<=-half*gf*dx(n)) then
        !                  q(n)=zero
        !                  qya(n)=zero
        !                end if
        !              case default
        !                write (*,*) "solve: illegal bottom boundary condn"
        !                stop
        !            end select
        !          end if

        !===== OGX: lateral flows start
        !							if (extraction) then ! get rate of extraction - ignore solute
        !								call qexsub(var%h,jt,qex,qexd)
        !								qexd=qexd*var%phiS/var%K ! to get deriv qexS
        !							end if
        !===== OGX: lateral flow end
        again=.false. ! flag for recalcn of fluxes
        !----- end get fluxes and derivs
        !----- estimate time step dt
        !dmax=zero
        thf=zero ! use thf as temp storage
        !							if (extraction) then
        !							 ! correction 4/4/07
        !							 !where (var%isat==0) thf=abs(q(1:n)-q(0:n-1)-qex)/(SOLMAT(jt)%WCSR*dx)
        !							else
        call SetSS(k,n,var,dx,slo,sli,srt)
        qex=slo-sli+srt   !!OGX: mm/hr

#ifdef debugMODE
        !qex=ZERO
#endif


        where (var%isat==0) thf=abs(q(1:n)-q(0:n-1)+qex)/(SOLMAT(jt)%WCSR*dx)
          !where (var%isat==0) thf=abs(q(1:n)-q(0:n-1))/(SOLMAT(jt)%WCSR*dx)
          !							end if
          dmax=maxval(thf) ! max derivative |dS/dt|

#ifdef debugMODE
          !write (*,*) "t, dt,dmax,qpme,h0", t, dt,dmax,qpme,h0
!          write (IFDEBUG,"(//6A15,/,6G15.7)") "t", "dt","dmax","qpme","h0","qov", t, dt,dmax,qpme,h0,qov
!          if (dmax==ZERO .or. dmax>300) then
!            write (IFDEBUG,"(//A5, 5A15)") "isat","Q", "Qlat","q1-q0+qex","thf","demoInEq9"
!            write (IFDEBUG,"(A5,E15.7)")  "Q0",q(0)
!            write (IFDEBUG,"(I5,5E15.7)") &
!              (var(i)%isat,q(i),qex(i),q(i)-q(i-1)+qex(i),thf(i),abs(q(i)-q(i-1)+qex(i))/(SOLMAT(jt(i))%WCSR*dx(i)), i=1, n)
!            !write(*,*) "dmax == 0"
!          endif
#endif
          if (dmax>zero) then
            dt=dSmax/dmax
#ifdef debugMODE
!            write (IFDEBUG,*) "dt=dSmax/dmax",dSmax,dmax,dt,t
#endif
            ! if pond going adjust dt
            if (h0>zero.and.(qov+q(0)-qpme)>h0/dt) then
              dt=(h0-half*h0min)/(qov+q(0)-qpme) !OGX: include overland flow qov
              pondmove=.true.
            endif
          else ! steady state flow  !OGX:|dS/dt| == zero / Or the column is fully saturated
            if (qpme>=q(n)+qov) then   ! pond is accumulated
              ! step to finish
              dt=tfin-t
            else
              dt=-(h0-half*h0min)/(qpme-q(n)-qov) ! pond going so adjust dt
              pondmove=.true.
            end if
          end if

          if (dt>dtmax) dt=dtmax ! user's limit
          !if (dt<dtmin) dt=dtmin ! user's limit
          ! if initial step, improve phi where S>=1
          if (msteps==msteps0.and.nsat>0.and.iflux==1) then
            again=.true.
            dt=1.0e-20*(tfin-ts)
          end if
          if (nsat==n.and.nsatlast<n.and.iflux==1) then
            ! profile has just become saturated so adjust phi values
            again=.true.
            dt=1.0e-20*(tfin-ts)
          end if


          !if (dt<=dtmin) then dt=dtmin
          if (t+1.1*dt+dtmin>=tfin) then ! step to finish     !OGX: check if need to move next step
            dt=tfin-t
            t=tfin
          else
            t=t+dt ! tentative update
          end if
          !----- end estimate time step dt

          !----- get and solve eqns

          rsigdt=one/(sig*dt)
          ! aa, bb, cc and dd hold coeffs and rhs of tridiag eqn set
          aa(ns+1:n)=qya(ns:n-1); cc(ns:n-1)=-qyb(ns:n-1)
          !if (extraction) then
          dd(1:n)=-(q(0:n-1)-q(1:n)-qex)*rsig
          !else
          !dd(1:n)=-(q(0:n-1)-q(1:n))*rsig
          !end if
          iok=0 ! flag for time step test
          itmp=0 ! counter to abort if not getting solution
          do while (iok==0) ! keep reducing time step until all ok
            itmp=itmp+1
            accel=one-0.05*min(10,max(0,itmp-4)) ! acceleration
            if (itmp>40) then
              write (*,*) "solve: too many iterations of equation solution"
              write (*,*) "accel",accel
              write (*,*) "itmp",itmp
              stop
            end if
            if (ns<1) then
              bb(0)=-(qya(0)+rsigdt+qhov)     !OGX: include overland flow qhov: derivative of q_ov WRT h_0
              dd(0)=-(qpme-q(0)-qov)*rsig   !OGX: include overland flow qov
            end if
            !if (extraction) thenstr0
            !where (var%isat==0) bb(1:n)=qyb(0:n-1)-qya(1:n)-qexd- &
              !SOLMAT(jt)%WCSR*dx*rsigdt
            !else
            !where (var%isat==0) bb(1:n)=qyb(0:n-1)-qya(1:n)- &
              !SOLMAT(jt)%WCSR*dx*rsigdt
            !end if
            where (var%isat==0)
              bb(1:n)=-(SOLMAT(jt(1:n))%WCSR*dx(1:n)*rsigdt-qyb(0:n-1)+qya(1:n))
            elsewhere
              bb(1:n)=qyb(0:n-1)-qya(1:n)
            endwhere
            call tri(ns,n,aa,bb,cc,dd,ee,dy)
            !            if (ns<1 .and. qov+sig*(qhov*dy(0))<0.0) then  !OGX runoff = 0 or negative
            !							qhov=0.0; qov=0.0
            !              bb(0)=-qya(0)-rsigdt     !OGX: exclude overland flow qhov: derivative of q_ov WRT h_0
            !              dd(0)=-(qpme-q(0))*rsig   !OGX: exclude overland flow qov
            !              call tri(ns,n,aa,bb,cc,dd,ee,dy)
            !            end if
            ! dy contains dS or, for sat layers, dphi values
            iok=1
            if (.not.again) then
              ! check if time step ok, if not then set fac to make it less
              iok=1
              do i=1,n
                if (var(i)%isat==0) then ! check change in S
                  if (abs(dy(i))>dSfac*dSmax) then
                    fac=max(half,accel*abs(dSmax/dy(i))); iok=0; exit
                  end if
                  if (-dy(i)>dSmaxr*S(i)) then
                    fac=max(half,accel*dSmaxr*S(i)/(-dSfac*dy(i))); iok=0; exit
                  end if
                  if (S(i)<one.and.S(i)+dy(i)>Smax) then
                    fac=accel*(half*(one+Smax)-S(i))/dy(i); iok=0; exit
                  end if
                  if (S(i)>=one.and.dy(i)>half*(Smax-one)) then
                    fac=0.25*(Smax-one)/dy(i); iok=0; exit
                  end if
                end if
              end do
              !              if (iok==1.and.ns<1.and.h0<h0max.and.h0+dy(0)>h0max+dh0max) then
              !                ! start of runoff
              !                fac=(h0max+half*dh0max-h0)/dy(0); iok=0
              !              end if
              if (iok==1.and.ns<1) then
                if (h0>hqmin .and. h0+dy(0)<(hqmin+h0min)) then    !h0min is overshooting
                  ! runoff going
                  fac=-(h0-(hqmin+half*h0min))/dy(0); iok=0
                elseif (h0>zero .and. h0+dy(0)<h0min) then
                  ! pond going
                  fac=-(h0-half*h0min)/dy(0); iok=0
                end if
              end if
              if (iok==0) then ! reduce time step
                t=t-dt; dt=fac*dt; t=t+dt; rsigdt=1./(sig*dt)
                !nless=nless+1 ! count step size reductions
              end if
              v=>var(1)
              if (v%isat/=0.and.iflux==1 .and. v%phi<phip.and. &
                  v%phi+dy(1)>phip) then
                ! incipient ponding - adjust state of saturated regions
                t=t-dt; dt=1.0e-20*(tfin-ts); rsigdt=1./(sig*dt)
                again=.true.; iok=0
              end if
            end if
          end do
          !----- end get and solve eqns
          !----- update unknowns
          ih0=0

          if (.not.again) then
            dwoff=zero

            if (ns<1) then
              ! note that fluxes required are q at sigma of time step
              dwinfil=(q(0)+sig*(qya(0)*dy(0)+qyb(0)*dy(1)))*dt

              runoff = runoff + qpme*dt - dwinfil - max(dy(0),-h0)

              h0=h0+dy(0)
              if (h0<zero.and.dy(0)<zero) ih0=1 ! pond gone0

            else
              dwinfil=(q(0)+sig*qyb(0)*dy(1))*dt
#ifdef debugMODE
            write(IFDEBUG,*)  "q(n)dt",q(n)*dt,drn,MAT_MP2H(jt(n),var(n)%phi),var(n)%isat
#endif
            end if

            if (limitET) then
              evap=evap+qprec*dt-dwinfil
            else
              evap=evap+qevap*dt
            endif

            !for initial ponding, runoff is the residual
            if (initpond) then
              h0=min(hqmin, qpme*dt - dwinfil)
#ifdef debugMODE
              write(*,*) "After inital pond, h0 = " , h0
#endif

              if (h0<0.) then
                call USTOP("negative ho after initial pond")
              endif
              runoff = runoff + qpme*dt - dwinfil - h0
            endif



            !write (*,*) limitET,qevap,(qprec1*dt-dwinfil)/dt
            infil=infil+dwinfil

            drn=drn+(q(n)+sig*qya(n)*dy(n))*dt
#ifdef debugMODE
            !write(IFDEBUG,*)  "q(n)dt",q(n)*dt,drn,MAT_MP2H(jt(n),var(n)%phi),var(n)%isat
#endif
            !OGX: cumulative internodeal flow
            qsum(1:n-1)=qsum(1:n-1)+(q(1:n-1)+sig*(qya(1:n-1)*dy(1:n-1)+qyb(1:n-1)*dy(2:n)))*dt

            !OGX: source and sink term
            srcum=srcum+srt*dt
            sicum=sicum+sli*dt
            socum=socum+slo*dt

          end if

          do i=1,n
            j=jt(i); p=>SOLMAT(j); v=>var(i)
            if (v%isat==0) then
              if (.not.again) then
                S(i)=S(i)+dy(i)
                if (S(i)>one.and.dy(i)>zero) then ! saturation of layer
                  v%isat=1; v%K=p%KSAT; v%phi=p%phie
                end if
              end if
            else
              v%phi=v%phi+dy(i)
              if (i==1.and.ih0/=0.and.v%phi>=p%phie) v%phi=0. ! pond gone
              if (v%phi<p%phie) then ! desaturation of layer
                v%isat=0; v%K=p%KSAT; v%phi=p%phie
                v%KS=p%KSe; v%phiS=p%phiSe
              end if
            end if
            !OGX: frozen soil
            v%K=v%K*fzn(i)
          end do
          !----- end update unknowns
          if (.not.again) exit
        end do
        if (dt<dtmin .and. .not. pondmove .and. itmp>10) then
           write (*,*) "solve: time step = ",dt," is smaller than user defined"
          write (*,*) "t, dt, ts(it)  ", t, dt
          write (*,*) "fac=  ",fac
          write (IFBAL,*) "solve: time step = ",dt," is smaller than user defined"
          write (IFBAL, *) "The 24h precipitation rate is"
          write (IFBAL, *) qprec
          stop
        end if
        !----- end take ne`````````````````````````````````````````````````````````````````````````````````````xt time step
        ! remove negative h0 (optional)
        if (h0<zero.and.var(1)%isat==0) then
          infil=infil+h0
          S(1)=S(1)+h0/(SOLMAT(jt(1))%WCSR*dx(1)); h0=zero
        end if
        msteps=msteps+1

#ifdef debugMODE
        !!mass balance check
!        str1=sum((SOLMAT(jt)%WCS-SOLMAT(jt)%WCSR*(1.0-S))*dx)
!        rlat=sum(socum-sicum+srcum)
!        roff=sum(runoff)
!        write (IFBAL,'(//10A15,/12(1PE15.6))') 't','h0','h1','prec','et','runoff','infil','drn','qlat','Str', &
!          t,h0,var(1)%h,prectot,evap,roff,infil,drn,rlat, &
!          str1,str0+infil-drn-str1-rlat, &
!          !win-(wp-wpi+h0+evap+drn+runoff)
!        prectot-(str1-str0+h0+evap+drn+roff+rlat)
!        write (IFBAL,*) initpond

#endif
      end do

      !update Phi
      do i=1,n
          if (var(i)%isat==0) var(i)%phi=MAT_S2MP(jt(i),S(i))
      enddo

      qsum(0)=infil
      qsum(n)=drn

      !----- end solve until tfin



    END SUBROUTINE solve


    SUBROUTINE getfluxes(n,jt,dx,dz,vtop,vbot,var,hint,phimin,q,qya,qyb, &
        iflux,init,getq0,getqn,dpmaxr)
      IMPLICIT NONE
      LOGICAL,INTENT(IN)::init,getq0,getqn
      INTEGER,INTENT(IN)::n,jt(n),iflux
      REAL,INTENT(IN)::dx(n),dz(n-1),dpmaxr
      TYPE(vars),INTENT(IN)::vtop,vbot
      TYPE(vars),TARGET,INTENT(IN)::var(n)
      REAL,INTENT(INOUT)::hint(n),phimin(n)
      REAL,INTENT(OUT)::q(0:n),qya(0:n),qyb(0:n)
      ! Gets fluxes q and partial derivs qya, qyb wrt S (if unsat) or phi (if sat).
      ! Fluxes at top and bottom of profile, and fluxes due to plant extraction of
      ! water are included.
      ! Definitions of arguments:
      ! n           - no. of soil layers.
      ! jt(1:n)     - layer soil type nos.
      ! dx(1:n)     - layer thicknesses.
      ! dz(1:n-1)   - distances between layer centres.
      ! vtop        - water vars at soil surface.
      ! vbot        - water vars at bottom of profile.
      ! var(1:n)    - water vars at layer centres.
      ! hint(1:n)   - values of h at interfaces are stored sequentially in hint.
      ! phimin(1:n) - similarly for phi at hmin in layers above interfaces.
      ! q(0:n)      - fluxes; q(i), i=1,...,n-1 is flux from layer i to layer i+1.
      !               q(0) is surface flux and q(n) is flux at bottom of profile.
      ! qya(0:n)    - partial deriv of q(i), i=0,...,n, wrt the variable to be solved
      !               for (S, phi or h) at upper end of flow path.
      ! qyb(0:n)    - ditto for var at lower end.
      ! iflux       - if iflux/=1, get only fluxes involving sat layers.
      ! init        - true if hint and phimin to be initialised.
      ! getq0       - true if q(0) required.
      ! getqn       - true if q(n) required.
      LOGICAL flag,limit
      INTEGER::i,itmp,j,l,m
      REAL::dphii1,dhi,h1,h2,hi,Khi1,Khi2,phii1,q2,qya2,qyb2,y,y1,y2
      TYPE(SOILMAT),POINTER::p,pm
      TYPE(vars)::vi1,vi2
      TYPE(vars),POINTER::v,vp
      real, parameter	::	hmin=-1.e7
      integer	::	nitsi
      nitsi = 1
      v=>var(1)
      if (iflux==1.or.v%isat/=0) then ! get top flux if required
        if (getq0) then
          call flux(jt(1),vtop,v,half*dx(1),q(0),qya(0),qyb(0))
        end if
      end if
      ! get other fluxes
      l=0

      do i=1,n-1
        j=jt(i); p=>SOLMAT(j)
        v=>var(i); vp=>var(i+1)
        if (iflux==1.or.v%isat/=0.or.vp%isat/=0) then ! get flux
          if (j==jt(i+1)) then ! same soil type, no interface
            call flux(j,v,vp,dz(i),q(i),qya(i),qyb(i))
          else ! interface
            l=l+1; m=jt(i+1); pm=>SOLMAT(m)
            if (init) then ! initialise
              call hyofh(hmin,j,vi1%K,Khi1,phimin(l)) ! get phi at hmin
              h1=v%h; h2=vp%h
              y1=v%K*dx(i+1); y2=vp%K*dx(i)
              ! equate fluxes (K constant) to get initial estimate of h at interface
              hint(l)=(y1*h1+y2*h2+half*gf*(v%K-vp%K)*dx(i)*dx(i+1))/(y1+y2)
            end if
            hi=hint(l)
            flag=.true.; itmp=0
            ! iterate to get hi at interface for equal fluxes using Newton's method
            ! get dphii1 at interface in upper layer, because of better linearity,
            ! then convert to dhi
            do while (flag)
              itmp=itmp+1
              if (itmp>1000) then
                write (*,*) "getfluxes: too many iterations finding interface h"
                stop
              end if
              if (hi<p%HBUB) then
                vi1%isat=0
                call hyofh(hi,j,vi1%K,Khi1,phii1)
                vi1%KS=Khi1/vi1%K ! use dK/dphi, not dK/dS
              else
                vi1%isat=1
                vi1%K=p%KSAT; phii1=p%phie+(hi-p%HBUB)*p%KSAT; vi1%KS=zero
              end if
              vi1%h=hi; vi1%phi=phii1; vi1%phiS=one ! use dphi/dphi not dphi/dS
              call flux(j,v,vi1,half*dx(i),q(i),qya(i),qyb(i))
              if (hi<pm%HBUB) then
                vi2%isat=0
                call hyofh(hi,m,vi2%K,Khi2,vi2%phi)
                vi2%KS=Khi2/vi2%K ! dK/dphi
              else
                vi2%isat=1; vi2%K=pm%KSAT; vi2%phi=pm%phie+(hi-pm%HBUB)*pm%KSAT
              end if
              vi2%h=hi; vi2%phiS=one ! dphi/dphi
              call flux(m,vi2,vp,half*dx(i+1),q2,qya2,qyb2)
              qya2=qya2*vi2%K/vi1%K ! partial deriv wrt phii1
              ! adjust for equal fluxes
              dphii1=-(q(i)-q2)/(qyb(i)-qya2)
              limit=.false.
              if (phii1+dphii1<=phimin(l)) then ! out of range
                limit=.true.; dphii1=-half*(phii1-phimin(l))
              end if
              phii1=phii1+dphii1
              dhi=dphii1/(vi1%K+half*vi1%KS*dphii1) ! 2nd order Pade approx
              if (-vi1%KS*dphii1>1.5*vi1%K) then ! use 1st order approx for dhi
                dhi=dphii1/vi1%K
              end if
              hi=hi+dhi
              ! check for convergence - dphi/(mean phi)<=dpmaxr
              if (limit.or.abs(dphii1/(phii1-half*dphii1))>dpmaxr) then
                nitsi=nitsi+1 ! accumulate no. of interface its
              else
                flag=.false.
              end if
            end do
            q(i)=q(i)+qyb(i)*dphii1
            hint(l)=hi
            ! adjust derivs
            y=1./(qya2-qyb(i))
            qya(i)=qya(i)*qya2*y; qyb(i)=-qyb2*qyb(i)*y
          end if
        end if
      end do
      v=>var(n)
      if (iflux==1.or.v%isat/=0) then ! get bottom flux if required
        if (getqn) then
          call flux(jt(n),v,vbot,half*dx(n),q(n),qya(n),qyb(n))
        end if
      end if
    END SUBROUTINE getfluxes

    SUBROUTINE flux(j,v1,v2,dz,q,qya,qyb)
      IMPLICIT NONE
      INTEGER,INTENT(IN)::j
      REAL,INTENT(IN)::dz
      REAL,INTENT(OUT)::q,qya,qyb
      TYPE(vars),INTENT(IN)::v1,v2
      ! Last modified 26th October 2009 to use subroutine fluxsuf.
      ! Gets flux and partial derivs for specified flow path.
      ! Definitions of arguments:
      ! j   - soil type no.
      ! v1  - water vars at upper end of path.
      ! v2  - ditto at lower end.
      ! dz  - length of path.
      ! q   - flux.
      ! qya - partial deriv of flux wrt S (if unsat) or phi (if sat) at upper end.
      ! qyb - ditto at lower end.
      REAL::w,rdz,temp
      TYPE(SOILMAT),POINTER::p
      ! gf is gravity factor (-1 to 1) assumed available in module
      p=>SOLMAT(j)
      if (v1%phi<p%phie.and.v2%phi>p%phie) then
        call fluxsuf(v1,v2,gf)
      elseif (v1%phi>p%phie.and.v2%phi<p%phie) then
        call fluxsuf(v2,v1,-gf) ! reverse direction to use same soln
        q=-q
        temp=qya; qya=-qyb; qyb=-temp
      else
        if (gf<zero) then
          if ((v1%isat/=0.and.v2%isat/=0).or.v1%h-gf*(-dz)>=p%HBUB) then
            ! correction 21/5/07
            !w=zero
            w=one
          else
            w=weight(j,v1%h,v1%K,v1%phi,-dz)
            w=one-w
          end if
        else
          if ((v1%isat/=0.and.v2%isat/=0).or.v2%h-gf*dz>=p%HBUB) then
            w=zero
          else
            w=weight(j,v2%h,v2%K,v2%phi,dz)
          end if
        end if
        rdz=one/dz
        q=(v1%phi-v2%phi)*rdz+gf*(w*v1%K+(one-w)*v2%K)
        if (v1%isat==0) then
          qya=v1%phiS*rdz+gf*w*v1%KS
        else
          qya=rdz
        end if
        if (v2%isat==0) then
          qyb=-v2%phiS*rdz+gf*(1.-w)*v2%KS
        else
          qyb=-rdz
        end if
      end if
    CONTAINS
      SUBROUTINE fluxsuf(v1,v2,g)
        IMPLICIT NONE
        REAL,INTENT(IN)::g
        TYPE(vars),INTENT(IN)::v1,v2
        ! Gets flux and partial derivs for sat/unsat flow.
        ! Uses variables in routine flux.
        ! Definitions of arguments:
        ! v1  - water vars at upper end of path.
        ! v2  - ditto at lower end.
        ! g  - gravity factor for solution.
        INTEGER,PARAMETER::nit=5 ! max its - alter as required
        INTEGER::it
        REAL,PARAMETER::ddzsmaxr=0.01 ! relative accuracy - alter as required
        REAL::a,b,c,dphis,dzs,dzsp,dzsphi,dzsS,rdz,w,y,y1
        dphis=p%phie-v2%phi; c=dphis*dz; dzs=half*dz
        do it=1,nit
          ! the correct call to weight depends on soln g and global gf
          if (g>=0) then
            w=weight(j,p%HBUB,p%KSAT,p%phie,sign(dz-dzs,gf))
          else
            w=weight(j,v1%h,v1%K,v1%phi,sign(dz-dzs,gf))
          end if
          ! solve quadratic for saturated length dzs
          a=g*w*(v1%K-p%KSAT)
          b=-(v1%phi-v2%phi+a*dz); dzsp=dzs
          !if (debuGGing) then
          !write(IFDEBUG,*) "a,b,dzsp",a,b,dzsp

          !write(IFDEBUG,"(I15,5E15.6)") v1
          !write(IFDEBUG,"(I15,5E15.6)") v2
          !endif
          y=-half*(b+sign(sqrt(b**2-4.0*a*c),b))
          dzs=c/y
          if (dzs<zero.or.dzs>dz) then
            dzs=y/a
          end if
          if (abs(dzs-dzsp)<ddzsmaxr*dz) exit
        end do
        rdz=one/dzs; y=one/(two*a*dzs+b); y1=g*w*v1%KS
        dzsS=-(y1*dzs-v1%phiS-y1*dz)*dzs*y; dzsphi=(dz-dzs)*y
        q=dphis*rdz+g*p%KSAT
        qya=-dphis*rdz**2*dzsS
        qyb=-rdz-dphis*rdz**2*dzsphi
      END SUBROUTINE fluxsuf
    END SUBROUTINE flux

    subroutine SetSS(k,n,var,DZ,SLout,SLin,SRT)
      !set up the sorce and sink
      !N    ~ node number of unsaturated zone and the virtual node
      !H    ~ pressure head of each node
      !WC0  ~ initial water content of each node
      !WC0  ~ updated water content of each node
      !SLout~ lateral outflow, mm/hr
      !SLin ~ lateral inflow, mm/hr
      !SRT  ~ root uptake, mm/hr

      use parm, only: ubw, hru_slp, slsoil
      implicit none
      integer, intent(in) :: k, n
      type(vars), intent(in) :: var(n)
      real, dimension(n), intent(in)	:: DZ
      real, dimension(n), intent(out)	:: SLout,SLin,SRT

      !!local variables
      integer :: iLAY, i, iSTEP, j, iTMP0, iTMP1, nNOD, nNODm1
      real ::  r0,r1,dep
      real ::  WC, AWC, WCFC, WCSAT, SSURF, LSOIL, EPMAX, DEPRT, WUP, WDN, CON
      real ::  W(n)
      type(SOILMAT), pointer :: mat


      LSOIL=slsoil(k)        !soil slope length, used in subsurface lateral flow calculation, m
      SSURF=hru_slp(k)       !hru surface slope
      EPMAX=SOLCOL(k)%EPMAX  !maximum transpiration rate, mm/hr
      DEPRT=SOLCOL(k)%DEPROT	!depth of root

      !compute source and sink (the S term)
      SLin=ZERO
      SLout=ZERO
      SRT=ZERO
      if (SOLCOL(k)%QLATIN>0.0) then
        r0=SOLCOL(k)%QLATIN/sum(DZ)  !lateral inflow
        SLin=DZ*r0
      endif

      WDN=ZERO
      dep=ZERO
      do i=1, n !lateral outflow and root uptake
        j=SOLCOL(k)%jt(i)
        mat=>SOLMAT(j)
        !if (iLAY>sol_nly(k)) exit !not calculate deep root?
        AWC=  mat%WCAWC
        WCFC= mat%WCFC
        WCSAT=mat%WCS

        WC=MAT_H2WC(j,var(i)%h)
        !DTMP=WCNEW(i)

        !!  unsaturated lateral flow, perched water table
        !!  COMPUTE LATERAL FLOW USING HILLSLOPE STORAGE METHOD
        if (WC>WCFC) then   !not including saturated zone
          CON=MAT_H2K(j,var(i)%h)
          SLout(i)=0.001*2.0*DZ(i)*CON*(WC-WCFC)*SSURF/((WCSAT-WCFC)*LSOIL)  !unit is mm/hr
        endif

        if (SLout(i)<CLOSEZERO) SLout(i)=ZERO

        !root water uptake.
        WUP=WDN
        dep=dep+DZ(i)
        if (dep<DEPRT) then
          AWC=0.25*AWC
          WDN=EPMAX/(1-exp(-ubw))*(1-exp(-ubw*dep/DEPRT))  !ubw: water uptake distribution parameter
          if (WC<AWC) then
            SRT(i)=(WDN-WUP)*exp(5.0*(WC/AWC-ONE))
          else
            SRT(i)=(WDN-WUP)
          endif
        else
          SRT(i)=ZERO
        endif
        if (SRT(i)<CLOSEZERO) SRT(i)=ZERO
      enddo

      if (DEPRT>dep) then !root possibly extends to saturated zone
        WDN=EPMAX
        SRT(n)=WDN-WUP
      endif

    end subroutine SetSS


    subroutine setsaturation(dz,dzgw,var,j,wc)
      !recalculate the water content for the cell intersected by groundwater table
      !unsaturated water content + saturated water content
      !dz			---	length of the entire cell
      !dzgw		---	length of saturated portion
      !var		---	status of the intersected cell
      !j			---	soil mat number of the intersected cell


      implicit none
      integer, intent(in)	::	j
      real, intent(in)	::	dz, dzgw
      real, intent(out)	::	wc
      type(vars), intent(inout)	::	var

      real :: h0,h1,wc0,wc1
      type(SOILMAT), pointer	::	mat

      mat=>SOLMAT(j)
      h0=var%h
      wc0=(dz-dzgw)*MAT_H2WC(j,h0)+dzgw*mat%WCS
      wc1=wc0/dz
      h1=MAT_WC2H(j,wc1)
      var%h=h1
      var%phi=(h1-mat%HBUB)*mat%KSAT+mat%phie
      wc=wc1


    end subroutine setsaturation


    subroutine searchgw(k,n,nu,dz,dzgw,var,depgw,wc)
      !search the lowest unsaturated node
      !set the saturated water content below groundwater table
      use parm, only: shallst, gw_spyld
      implicit none
      integer, intent(in)	::	k,n
      real, intent(in)	::	dz(n)

      real, intent(out)	::	dzgw,wc(n)
      real, intent(inout)	::	depgw
      integer, intent(inout)	::	nu
      type(vars), intent(inout)	::	var(n)


      real		::	dep,h
      integer	::	i
      type(SOILMAT), pointer	:: mat

      !var%isat=0
      depgw=SOLCOL(k)%DEPGW0-shallst(k)/gw_spyld(k)

      nu=n+1;dep=0.0
      do i=1, n
        dep=dep+dz(i)
        !var(i)%isat=0
        if (dep>=depgw) then
          nu=i
          dzgw=dep-depgw
          exit
        endif

      enddo



      !


      !dep=dep-dz(nu)
      do i=nu+1,n
        !fully saturated zone
        dep=dep+dz(i)
        mat=>SOLMAT(SOLCOL(k)%jt(i))
        h=dep-depgw-0.5*dz(i)	!hydraulic head at the center of a cell
        var(i)%isat=1
        var(i)%h=h
        var(i)%phi=(h-mat%HBUB)*mat%KSAT+mat%phie
        wc(i)=mat%WCS
      enddo

      if (nu>n) then
        !groundwater table is below the bottom of the soil column, exit the program
        write(IFPROFILE,*) 'Groundwater table is below the bottom of Soil Column or Fully Saturated Soil', k
        !write(*,*) 'Groundwater table is below the bottom of Soil Column or Fully Saturated Soil', k
        call USTOP('Groundwater table is below the bottom of the soil column or Fully Saturated Soil, exit the program')
        !nu=n
      endif
    end subroutine searchgw


    subroutine discretize(IDIV,THICK1,THICK2,DifRto,NL,ZBOT,JTL,DZ,NZ,ILBNOD,jt,LAY1)
      implicit none
      integer, intent(in)	::	IDIV, NL,JTL(MAXLAY),LAY1
      real, intent(in)		::	THICK1,THICK2, DifRto
      real, intent(inout)	::	ZBOT(0:NL),DZ(MAXNODE)
      integer, intent(out)::	NZ,ILBNOD(0:MAXLAY),jt(MAXNODE)


      integer	:: l
      real :: dep0,dep1,thick,middle,r0,dzf

      ILBNOD(0)=0
      if (IDIV==1) then
        do l=1, NL
          DZ(l)=ZBOT(l)-ZBOT(l-1)
          ILBNOD(l)=l
          jt(l)=JTL(l)
        enddo
        NZ=NL
        return
      endif


      DZ=0.
      dzf=THICK1




      do l=1, NL

        middle=0.5*(ZBOT(l-1)+ZBOT(l))

        if (l<=1+LAY1) then
          if (THICK1>ZBOT(1)) then
            call USTOP("The first layer starting thickness is larger than the layer thickness")
          endif
          NZ=0
          dep0=0.0
          thick=THICK1
        else
          thick=THICK2
        endif


        if (IDIV==5 .and. thick<=1.0) then  !avoid first term is less than 1,0, resulting in a convergent sequence
          r0=1.2/thick
        else
          r0=1.0
        endif

        do
          NZ=NZ+1
          jt(NZ)=JTL(l)
          dep1=dep0+thick
          !if (ZNOD(NNOD)>=ZBOT(l-1)+0.5*(ZBOT(l)-ZBOT(l-1)))
          if (dep1>=ZBOT(l)) then
            !ZNOD(NNOD)=ZBOT(l)
            DZ(NZ)=ZBOT(l)-dep0
            if (DZ(NZ)*5<DZ(NZ-1)) then  !to avoid too small trancated lower end
              DZ(NZ)=0.5*(DZ(NZ-1)+DZ(NZ))
              DZ(NZ-1)=DZ(NZ)
            endif
            dep0=ZBOT(l)
            dzf = DZ(NZ)
            exit
          else
            if (thick>dzf*5.0) then !to avoid the small trancated lower end connecting to a large upper end of the next cell
              DZ(NZ)=dzf*3.0
              dep1=dep1-thick+DZ(NZ)
            else
              DZ(NZ)=thick
            endif
            dep0=dep1
            dzf = DZ(NZ)
          endif

          !increase the intervel until the middle of a physical layer then decrease to the bottom of the layer

          if ( dep0 < middle) then
            if (IDIV==3) thick=thick+ DifRto
            if (IDIV==4) thick=thick* DifRto
            if (IDIV==5) thick=(thick*r0)**DifRto/r0
          else
            if (IDIV==3) thick=thick- DifRto
            if (IDIV==4) thick=thick/ DifRto
            if (IDIV==5) thick=(thick*r0)**(1./DifRto)/r0
          endif
        enddo !node within a soil or aquifer layer

        ILBNOD(l)=NZ
      enddo !layer: l
    end subroutine



    SUBROUTINE tri(ns,n,aa,bb,cc,dd,ee,dy)
      IMPLICIT NONE
      INTEGER,INTENT(IN)::ns,n
      REAL,DIMENSION(0:n),INTENT(IN)::aa,cc,dd
      REAL,DIMENSION(0:n),INTENT(INOUT)::bb,ee,dy
      ! Solves tridiag set of linear eqns. Coeff arrays aa and cc left intact.
      ! Definitions of arguments:
      ! ns      - start index for eqns.
      ! n       - end index.
      ! aa(0:n) - coeffs below diagonal; ns+1:n used.
      ! bb(0:n) - coeffs on diagonal; ns:n used.
      ! cc(0:n) - coeffs above diagonal; ns:n-1 used.
      ! dd(0:n) - rhs coeffs; ns:n used.
      ! ee(0:n) - work space.
      ! dy(0:n) - solution in ns:n.
      INTEGER::i
      dy(ns)=dd(ns) ! decomposition and forward substitution
      do i=ns,n-1
        ee(i)=cc(i)/bb(i)
        dy(i)=dy(i)/bb(i)
        bb(i+1)=bb(i+1)-aa(i+1)*ee(i)
        dy(i+1)=dd(i+1)-aa(i+1)*dy(i)
      end do
      dy(n)=dy(n)/bb(n) ! back substitution
      do i=n-1,ns,-1
        dy(i)=dy(i)-ee(i)*dy(i+1)
      end do
    END SUBROUTINE tri

    subroutine FinalizeDayUN(k,nun,NNOD,runoff,qub,qsum,evap,sicum,socum,srcum)
      use parm
      !, only: es_day,snoev,sol_sw,sol_st,flat,rchrg,revapday,ep_day,ep_max, &
        !								sno3up,sol_sumfc,sol_sumul,strsa,latq,mstep, strsw, &
        !								isep_opt,bz_perc,sol_prk,i_sep,sol_nly,sol_no3,hru_dafr,hhqday
      implicit none
      integer, intent(in)		::	k,nun,NNOD
      real, intent(in)			::	evap
      real, intent(in)			::	runoff(mstep),qub(mstep)
      real, intent(in)			::	qsum(0:NNOD),sicum(NNOD),socum(NNOD),srcum(NNOD)

      !local variables
      type(SOILCOLUMN), pointer ::	scol
      integer	::	l,j,ii,iis,iie
      real		::	r0,r1,r2,r3
      real,dimension(NNOD) ::	wc,DZ
      !-------------------------------------------
      scol=>SOLCOL(k)
      DZ=scol%DZ


      !soil water storage
      do l=1, nun
        scol%WC(l) = MAT_MP2WC(scol%jt(l),scol%var(l)%phi)
      enddo
      call SOLCOL_Update_Storage(k)


      !!compute no3 flux from layer 2 to 1 by soil evaporation
      r0=qsum(scol%ILBNOD(1))
      if (r0<0.) then		!uptake
        r0=-r0
        r1 = 0.1 * r0 * sol_no3(2,k) / (sol_st(2,k) + 1.e-6) !OGX: EQ 3:1.7.1, effnup=0.1
        r2 = sol_no3(2,k)
        r3 = Min(r1, r2)
        sno3up = sno3up + r3 * hru_dafr(k)
        sol_no3(2,k) = sol_no3(2,k) - r3
        sol_no3(1,k) = sol_no3(1,k) + r3
      endif

      !!bottom seepage from soil profile
      !			r0=qsum(scol%ILBNOD(sol_nly(k)))
      !			if (ro>0)
      !			sepbtm(k) = sepbtm(k)+ qsum(scol%ILBNOD(sol_nly(k)))

      !2.Soil ET includes snow sublimation
      es_day=evap+snoev
      if (es_day < 0.) es_day = 0.

      !3.groundwater
      if (qsum(nun)>0.) then
        rchrg(k)=qsum(nun)
        revapday=0.
      else
        rchrg(k)=0.
        revapday=-qsum(nun)
      endif

      !4.lateral in and out flow, & percolation array
      do l=1, sol_nly(k)
        if (l==1) then
          iis=scol%ILBNOD(l-1)+1
        else
          iis=scol%ILBNOD(l-1)
        endif
        iie=scol%ILBNOD(l)

        flat(l,k)=sum(socum(iis:iie))

        if (qsum(iie)>0) then
          sol_prk(l,k) = sol_prk(l,k) + qsum(iie)
        endif

      enddo

      !5.plant root uptake
      ep_day=sum(srcum)
      ep_day=min(ep_max, ep_day)


      !6.compute aeration stress
      if (ep_max <= 0.01) then
        strsw(k) = 1.
      else
        if (sol_sw(k) > sol_sumfc(k)) then
          r0 = (sol_sw(k) - sol_sumfc(k)) / (sol_sumul(k) -sol_sumfc(k))
          r1 = .85
          r2 = 100. * (r0 - r1) / (1.0001 - r1)
          if (r2 > 0.) then
            strsa(k) = 1. - (r2 / (r2 + Exp(2.9014 - .03867 * r2)))
          else
            strsa(k) = 1.
          end if
        end if
        strsw(k) = ep_day / ep_max
      endif

      !7.daily total runoff
      latq(k)=latq(k)+sum(socum)

      if (nstep>0) then
        do l=1, nstep
          hhqday(l)=runoff(l)
          ubnrunoff(l)=qub(l)
          surfq(k) = surfq(k) + hhqday(l)+ ubnrunoff(l)
        enddo
      else
        surfq(k) = surfq(k) + sum(runoff)+sum(qub)
      endif


      !8.biozone layer perc amount
      if(isep_opt(k)==1) then
        bz_perc(k) = sol_prk(i_sep(k),k)
      end if

      !
      !call tileflow

    end subroutine finalizedayun

    subroutine tileflow
      !! compute shallow water table depth and tile flow

      use parm

      integer :: j, j1, nn, k
      real d,por_air,sumqtile,sw_del,wt_del,wat,xx,yy
      j = 0
      j = ihru
      qtile = 0.
      wt_shall = 0.    !CB 8/24/09
      wt_shall = dep_imp(j)
      !! drainmod tile equations   08/11/2006
      if (sol_tmp(2,j) > 0.) then   !Daniel 1/29/09
        por_air = 0.5
        d = dep_imp(j) - ddrain(j)
        !! drainmod wt_shall equations   10/23/2006
        if (iwtdn == 0) then !compute wt_shall using original eq-Daniel 10/23/06
          if (sol_sw(j) > sol_sumfc(j)) then
            yy = sol_sumul(j) * por_air
            if (yy < 1.1 * sol_sumfc(j)) then
              yy = 1.1 * sol_sumfc(j)
            end if
            xx = (sol_sw(j) - sol_sumfc(j)) / (yy - sol_sumfc(j))
            if (xx > 1.) xx = 1.
            wt_shall = xx * dep_imp(j)
            wat = dep_imp(j) - wt_shall
            if(wat > dep_imp(j)) wat = dep_imp(j)
          end if
        else
          !compute water table depth using Daniel's modifications
          do j1 = 1, sol_nly(j)
            if (wat_tbl(j) < sol_z(j1,j)) then
              sw_del = sol_swpwt(j) - sol_sw(j)
              wt_del = sw_del * vwt(j1,j)
              wat_tbl(j) = wat_tbl(j) + wt_del
              if(wat_tbl(j) > dep_imp(j)) wat_tbl(j) = dep_imp(j)
              wt_shall = dep_imp(j) - wat_tbl(j)
              sol_swpwt(j) = sol_sw(j)
              exit
            end if
          end do
        end if
        !! drainmod wt_shall equations   10/23/2006

        if (ddrain(j) > 0.) then
          if (wt_shall <= d) then
            qtile = 0.
          else
            !! Start Daniel's tile equations modifications  01/2006
            if (itdrn == 1) then
              call drains     ! compute tile flow using drainmod tile equations
              !! drainmod tile equations   01/2006
            else !! compute tile flow using existing tile equations
              call origtile(d)! existing tile equations
              if(qtile < 0.) qtile=0.
            end if
          end if
        end if
      end if
      !! End Daniel's tile equations modifications  01/2006

      if (qtile > 0.) then
        !! update soil profile water after tile drainage
        sumqtile = qtile
        do j1 = 1, sol_nly(j)
          xx = sol_st(j1,j) - sol_fc(j1,j)
          if (xx > 0.) then
            if (xx > sumqtile) then
              sol_st(j1,j) = sol_st(j1,j) - sumqtile
              sumqtile = 0.
            else
              sumqtile = sumqtile - xx
              sol_st(j1,j) = sol_fc(j1,j)
            end if
          end if
        end do
        if (sumqtile > 0.) then
          qtile = qtile - sumqtile
          qtile = amax1(0., qtile)
        end if
      end if
    end subroutine

    subroutine print_var(ifout, n, var, t, wc)
      implicit none
      integer, intent(in)		::	ifout, n
      real, intent(in)		::	t
      type(vars), intent(inout)	::	var(n)
      real, optional, intent(in)		::	wc(n)

      integer			::	i

      if (present(wc)) then

        !print header
        write (ifout,"(7A15,A20,G20.13)")  "isat","h","phi","phiS","K","KS","WC","At time: ",t
        do i=1, n
          write (ifout,"(I15,6E15.7)")  var(i)%isat,var(i)%h,var(i)%phi,var(i)%phiS,var(i)%K,var(i)%KS,wc(i)
        enddo

      else
        !print header
        write (ifout,"(6A15,A20,G20.13)")  "isat","h","phi","phiS","K","KS","At time: ",t
        do i=1, n
          write (ifout,"(I15,5E15.6)")  var(i)%isat,var(i)%h,var(i)%phi,var(i)%phiS,var(i)%K,var(i)%KS
        enddo
      endif

    end subroutine print_var

  end module


  subroutine readsoilcol(IFIN, k)
    !read and prepare data
    !k is index of HRU; j is index of soil layer
    use ROSSMOD
    use parm
    implicit none
    integer, intent(in) :: IFIN, k

    !local variables
    character(len=400) :: sLine
    integer :: j, l,NNOD, nSolLay, LAYAQ, NLAY, NUNS, iie,iis
    real :: depth, pdepth, thick
    real :: ThickMin1, ThickMin2, DifRto, Half1
    real :: sumpor,r0,r1,r2,r3
    real :: q1,q2,sr,sl,d1,d2,k1,k2,k3,fc
    integer :: iTmp,iStart,iStop,iLoc
    real	::	HCRIT,HINI
    real :: WCWP1, WCSAT1, ALFA1, WCRES1, PSDI1, LPOR1, KE1,WCAWC1,qsteady
    integer(1), dimension(MAXLAY) :: IMOD1
    integer, dimension(MAXLAY) :: IIDX




    real	::	 dzgw
    integer	::	iLay1, ii
    integer		          ::  IHCRIT
    integer		          ::  IHINI
    integer		          ::  IDIV		  !Method of node discretization,  1 coinside with soil layer
    !                                                                !2 uniformly divided into minimum thickness
    !                                                                !3 variably divide by artimetic progression
    !                                                                !4 variably divide by geometric progression
    !                                                                !5 variably divide by exponential progression
    real, dimension(0:MAXLAY) ::  ZBOT
    integer, dimension(0:MAXLAY) ::  ILBNOD
    integer, dimension(MAXLAY)		::  jt_lay
    real, dimension(0:MAXNODE) :: ZNOD
    real, dimension(MAXNODE) 	:: DZ,WC
    integer, dimension(MAXNODE) 	:: jt
    type(SOILCOLUMN), pointer	::	scol
    type(SOILMAT), pointer	::	smat

    !IALFA		  !alpha (reciprocal of entry pressure) 0 input by user 1 calculate based on clay, sand and porosity
    !IWCRES		!Residual WC, 0 input by user 1 calculate based on clay, sand and porosity
    !IPSDI		  !Pore size distribution index, 0 input by user 1 calculate based on clay, sand and porosity
    !ILPOR
    !IWCWP		  !WC at wilting point, 0 input by user, 1 calculate based on clay and bulk density
    !IWCSAT		!Saturated WC, 0 input by user, 1 calculate based on bulk density and particle density
    !IKMEAN		!internodal K method, 1. Arithmetic mean



    !open(IFIN, file="solflw.001")
    scol=>SOLCOL(k)
    allocate(scol%IRRI(mstep))
    allocate(scol%RUNON(mstep))
    allocate(scol%RAIN(mstep))



    scol%QLATIN=0.0
    scol%RUNON=0.0
    scol%IRRI=0.0
    scol%RAIN=0.0
    scol%EPMAX=0.0
    scol%ESMAX=0.0

    scol%dtmin=1.e-10

    STRFMT="(30('-'), A, I4, 30('-'))"
    write (slogfile, STRFMT) 'STARTING READING SOIL FLOW DATA FOR SOL COLUMN ', k

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,2,scol%NLAY,r0,slogfile,IFIN) !# of physical layers
    NLAY=scol%NLAY



    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%FACFROZN,slogfile,IFIN) !frozen factor


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,2,IHCRIT,r0,slogfile,IFIN)   !IHCRIT


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,HCRIT,slogfile,IFIN)   !HCRIT


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%hqmin,slogfile,IFIN)   !hqmin



    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%dSmax,slogfile,IFIN)   !dSmax


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%dSmaxr,slogfile,IFIN)   !dSmaxr


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%dtmin,slogfile,IFIN)   !dtmin


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%dtmax,slogfile,IFIN)   !dtmax


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%dSfac,slogfile,IFIN)   !dSfac


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%dpmaxr,slogfile,IFIN)   !dpmaxr


    !ZTOP	WCINI1	IWCINI	IDIV	[THICKMIN1]	[THICKMIN2]	[DIFRTO]
    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%ZTOP,slogfile,IFIN)  !ZTOP
    STRFMT="('THE TOP ELEVATON OF HRU IS ', 1PG15.7)"
    write (slogfile, STRFMT) scol%ZTOP


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,2,IHINI,r0,slogfile,IFIN)   !


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,HINI,slogfile,IFIN)  !initial head


    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,2,IDIV,r0,slogfile,IFIN)    !
    !IDIV=iTmp
    if (IDIV==1) then
      write (slogfile, "(A)") "DISCRETIZATION IS COINSIDE WITH SOIL AND AQUIFER LAYER."
    elseif (IDIV==2) then
      write (slogfile, "(A)") "NODES ARE UNIFORMLY DIVIDED WITHIN EACH SOIL AND AQUIFER LAYER."
    elseif (IDIV==3) then
      write (slogfile, "(A)") "NODES ARE VARIABLY DIVIDED BY ARTIMETIC PROGRESSION WITHIN EACH SOIL AND AQUIFER LAYER."
    elseif (IDIV==4) then
      write (slogfile, "(A)") "NODES ARE VARIABLY DIVIDED BY GEOMETRIC PROGRESSION WITHIN EACH SOIL AND AQUIFER LAYER."
    elseif (IDIV==5) then
      write (slogfile, "(A)") "NODES ARE VARIABLY DIVIDED BY EXPONENTIAL PROGRESSION WITHIN EACH SOIL AND AQUIFER LAYER."
    else
      write (slogfile, "(A)") "UNKONWN SOIL DISCRETIZATION"
      call USTOP('UNKONWN SOIL DISCRETIZATION')
    endif

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,ThickMin1,slogfile,IFIN)

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,ThickMin2,slogfile,IFIN)   !

    if (IDIV>=2) then
      !uniformly distributed!
      write (slogfile, "(A, 1PG15.7)") "MINIMUM INTERVAL BETWEEN NODES IN THE TOP SOIL LAYER IS ", ThickMin1
      write (slogfile, "(A, 1PG15.7)") "MINIMUM INTERVAL BETWEEN NODES IN THE LOWER SOIL AND AQUIFER LAYER IS ", ThickMin2
      DifRto=1.0
    endif

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,DifRto,slogfile,IFIN)    !

    if (IDIV>=3) then
      !variablyly distributed
      if (IDIV==3 .and. DifRto<0.0) then
        write (slogfile, "(A)") "COMMON DIFFERENCE FOR SOIL DISCRETIZATION IS LESS THAN 0.0"
        call USTOP('COMMON DIFFERENCE FOR SOIL DISCRETIZATION IS LESS THAN 0.0')
      endif
      if (IDIV>=4 .and. DifRto<1.0) then
        write (slogfile, "(A)") "COMMON RATIO FOR SOIL DISCRETIZATION IS LESS THAN 1.0"
        call USTOP('COMMON RATIO FOR SOIL DISCRETIZATION IS LESS THAN 1.0')
      endif
    endif

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%Kdn,slogfile,IFIN)    !

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,scol%Kup,slogfile,IFIN)    !

    !reading layer infomation

    iLay1=scol%Lay1_split
    NLAY=NLAY+iLay1

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    do  l = 1+iLay1, NLAY
      call URWORD(sLine,iLoc,iStart,iStop,2,jt_lay(l),r0,slogfile,IFIN)    !
    enddo

    call URDCOM(IFIN,slogfile,sLine)
    iLoc=1
    do  l = 1+iLay1, NLAY
      call URWORD(sLine,iLoc,iStart,iStop,3,iTmp,ZBOT(l),slogfile,IFIN)    !
    enddo


    if (iLay1>0) then
      !top thin layer division by SWAT
      ZBOT(1) = 10
      jt_lay(1) = jt_lay(2)
    endif


    !calculate the first layer Hmin of control ET
    j=jt_lay(1)
    if (IHCRIT==0) then
      scol%HCRIT=HCRIT
    elseif (IHCRIT==1) then
      scol%HCRIT=MAT_WC2H(j,HCRIT)
    else
      scol%HCRIT=MAT_S2H(j,0.05)
    endif
    scol%IHATM=IHCRIT

    if (IHINI==1) then
      HINI=MAT_WC2H(j,HINI)
    elseif (IHINI==2) then
      HINI=MAT_S2H(j,HINI)
    endif

    NNOD=0      !number of nodes
    ZBOT(0)=0.0

    call discretize(IDIV,ThickMin1,ThickMin2,DifRto,NLAY,ZBOT,jt_lay,DZ,NNOD,ILBNOD,jt,iLay1)

    allocate(scol%ILBNOD(0:NLAY))
    allocate(scol%var(NNOD))
    allocate(scol%jt(NNOD))
    allocate(scol%DZ(NNOD))
    allocate(scol%WC(NNOD))
    allocate(scol%QSUM(0:NNOD))

    scol%NNOD=NNOD
    scol%NLAY = NLAY
    scol%ILBNOD(0:NLAY)=ILBNOD(0:NLAY)
    scol%jt(1:NNOD)=jt(1:NNOD)
    scol%DZ(1:NNOD)=DZ(1:NNOD)
    scol%QSUM=0.


    !search the lowest unsaturated node
    call searchgw(k,NNOD,NUNS,DZ(1:NNOD),dzgw,scol%var,scol%DEPGW,scol%WC)
    !steady-state simulation
    !DZ(NUNS)=DZ(NUNS)-dzgw

    call solve_steady(NUNS,DZ,jt,HINI,dzgw,scol%var,qsteady,scol%WC)
    !DZ(NUNS)=DZ(NUNS)+dzgw

    !update water content
    !call setsaturation(DZ(NUNS),dzgw,scol%var(NUNS),jt(NUNS),scol%WC(NUNS))

    write (IFDEBUG,*) "phi_n after strady", scol%var(NUNS)%phi

    ! initialize watershed water parameters
    ! ToCheck: It needs to be calculated after initial steady-run

    scol%HPOND=0.0

    !compute and transfer needed parameter values for SWAT
    wshd_sw = 0.0
    depth=0.0
    sumpor = 0.0
    sol_sumfc(k) = 0.0
    sol_sumul(k) = 0.0
    sol_sw(k) = 0.0
    sol_sumwp(k) = 0.0
    sol_avbd(k) = 0.0

    nSolLay=sol_nly(k)

    call SOLCOL_Update_Storage(k)

    do l=1, nSolLay


      j=jt_lay(l)
      smat=>SOLMAT(j)
      thick = sol_z(l,k)-depth

      !WP and AWC are calculated based on head
      WCWP1=smat%WCWP
      WCAWC1=smat%WCAWC
      WCSAT1=smat%WCS



      sol_wp(l,k) = WCWP1
      sol_up(l,k) = (WCWP1 + WCAWC1)
      sol_por(l,k) = WCSAT1


      !! compute drainable porosity and variable water table factor - Daniel
      r0 = sol_por(l,k) - sol_up(l,k)
      vwt(l,k) = (437.13 * r0**2) - (95.08 * r0) + 8.257

      !! synchonize parameters between SWAT and un-swat (un-swat => SWAT)
      sol_avbd(k) = sol_avbd(k) + sol_bd(l,k) * (1 - sol_por(l,k)) * thick
      sol_ul(l,k) = (sol_por(l,k) - sol_wp(l,k)) * thick
      sol_sumul(k) = sol_sumul(k) + sol_ul(l,k)
      sol_fc(l,k) = (sol_up(l,k) - sol_wp(l,k)) * thick
      sol_sumfc(k) = sol_sumfc(k) + sol_fc(l,k)
      sol_hk(l,k) = (sol_ul(l,k) - sol_fc(l,k)) / sol_k(l,k)
      if (sol_hk(l,k) < 1.) sol_hk(l,k) = 1.
      sol_wpmm(l,k) = sol_wp(l,k) * thick
      sol_sumwp(k) = sol_sumwp(k) + sol_wpmm(l,k)
      crdep(l,k) = sol_crk(k) * 0.916 * Exp(-.0012 * sol_z(l,k)) * thick
      volcr(l,k) = crdep(l,k) * (sol_fc(l,k) - sol_st(l,k)) / (sol_fc(l,k))


      depth=sol_z(l,k)
    enddo !layer: l

    sol_swpwt(k) = sol_sw(k)
    sol_avbd(k) = sol_avbd(k)/sol_z(nSolLay,k)
    wshd_sw = wshd_sw + sol_sw(k) * hru_dafr(k)
    wshd_snob = wshd_snob + sno_hru(k) * hru_dafr(k)  !how much snow stored in the watershed

    !initial overland flow

    scol%POV=sqrt(hru_slp(k))/ov_n(k)/slsubbsn(k)*3.6e1

    scol%QOV=0.

    !scol%iter_tot=0



  end subroutine

  subroutine readsoilmat
    use ROSSMOD

    implicit none
    integer			::	sfile
    integer			::	i, iis,iie,icol
    integer			::	k
    real				::	r
    integer   	::	iwp				!wilting point, 0 input by user, 1 calculate at -15 bar
    integer   	::	iawc			!available water content, 0 input by user, 1 calculate at -15 bar
    character(len=400) :: sline
    type(SOILMAT), pointer	::	p

    open(newunit=sfile,file='soil.mat')
    open(newunit=slogfile,file='soil.log')

    read(sfile,'(A)')	sline
    icol=1
    call URWORD(sline,icol,iis,iie,2,nmat,r,slogfile,sfile)
    call URWORD(sline,icol,iis,iie,2,iwp,r,slogfile,sfile)
    call URWORD(sline,icol,iis,iie,2,iawc,r,slogfile,sfile)

    allocate (SOLMAT(nmat))

    do i=1, nmat
      p=>SOLMAT(i)
      read(sfile,'(A)')	sline
      icol=1
      call URWORD(sline,icol,iis,iie,3,k,p%KSAT,slogfile,sfile)
      call URWORD(sline,icol,iis,iie,3,k,p%WCR,slogfile,sfile)
      call URWORD(sline,icol,iis,iie,3,k,p%WCS,slogfile,sfile)
      call URWORD(sline,icol,iis,iie,3,k,p%LAM,slogfile,sfile)
      call URWORD(sline,icol,iis,iie,3,k,p%ETA,slogfile,sfile)
      call URWORD(sline,icol,iis,iie,3,k,p%HBUB,slogfile,sfile)

      p%WCSR=p%WCS-p%WCR
      p%PHIE=p%KSAT*p%HBUB/(one-p%lam*p%eta) ! MFP at HBUB
      p%KSE=p%ETA*p%KSAT ! dK/dS at HBUB
      p%PHISE=(p%ETA-ONE/p%LAM)*p%PHIE ! dphi/dS at HBUB

      if (iwp==1) then
        call URWORD(sline,icol,iis,iie,2,k,p%WCWP,slogfile,sfile)
      else
        p%WCWP=MAT_H2WC(i,H1500kPa)
      endif
      if (iawc==1) then
        call URWORD(sline,icol,iis,iie,2,k,p%WCAWC,slogfile,sfile)
        p%WCFC=p%WCAWC+p%WCWP
        if (p%WCFC<0.0) then
          call USTOP('Wrong AWC in input file of soil material')
        endif
      else
        p%WCFC=MAT_H2WC(i,H33kPa)
        p%WCAWC=p%WCFC-p%WCWP
      endif

    enddo
    close(sfile)
  end subroutine readsoilmat

  subroutine SOIL_ALL

    !!OGXinSWAT
    use ROSSMOD
    use parm, only: mhru,nstep,ievent
    real :: dt
#ifdef debugMODE
    print *, 'nstep=',nstep
#endif

    allocate(SOLCOL(mhru))
    !allocate(SOLMAT(NMAT))
    open(newunit=IFPROFILE,file="output.sol.col")
    open(newunit=IFBAL,    file="output.sol.bal")
    open(newunit=IFDEBUG,  file="solflw.info")

    write(IFBAL,"(A5,15A10)") "ihru","Time","GWLevel","Qnet","EpMax","Stor0","Stor1","Runoff", &
                              "Hpond","SeepTop","Esoil","SeepBot","LatIn","LatOut","Root","BalErr"
    !open(newunit=IFMAT,file="output.sol.mat")
    iMAT=0
    !	WRITE(IFMAT,111)
    !111   FORMAT(/7X,'TABLE OF HYDRAULIC PROPERTIES WHICH ARE INTERPOLATED',
    !     &	 ' IN SIMULATION'/7X,65('=')/)
    !
    !	  write(IFMAT,"(A7,3(7x,A8),3x,A12,2x,A13,2(5x,A10))")
    !     &     'MAT. NO','PRESSURE','MOISTURE','CAPACITY','CONDUCTIVITY',
    !     &     'RELATIVE SAT.','DERI. CON.','DERI. CAP.'
    !	  !'YearDay','dSTORmm','QTOPmm','QBOTmm','ROOTmm','QLATmm','ESACTmm','QSURFmm','dSTOR/STOR0 %'
    !	  write(IFBAL,"(A4,2A8, 10A15)")'Year','Day','HRU','dSTORmm',
    !     &  'QTOPmm','QBOTmm','QLATmm','ROOTmm','QLATOUTmm','ESACTmm',
    !     &  'QSURFmm','IRR_RAINmm','DS/S0%'


    dt=24./nstep
    !nstep=240
    allocate(tstep(0:nstep))
    tstep(0)=ZERO
    do ii=1, nstep
      tstep(ii)=tstep(ii-1)+dt
    enddo
    tstep=tstep-24.

    mstep=nstep
  end subroutine SOIL_ALL
