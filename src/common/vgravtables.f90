! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute

! This file is part of SNAP. SNAP is free software: you can
! redistribute it and/or modify it under the terms of the
! GNU General Public License as published by the
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.

! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.

! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.

subroutine vgravtables
!==========================================

!   program for calculating the gravitational settling velocities
!   for small and large particles (outside the Stokes low)
!---------------------------------------
  USE snapparML

#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
  real :: t        ! absolute temperature (K)
  external vgrav
  real :: vgrav    ! external function for calculating initial vg
  real :: dp        ! particle size um
  real :: rp        ! particle density in g/m3
  real :: vg        ! gravitational setling in cm/s (Stokes low)
  real :: vgmod    ! gravitational setling in cm/s after iteration
  real :: p        ! atmospheric pressure

  integer :: n,m,ip,it
!---------------------------------------


#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('VGRAVTABLES',0,ZHOOK_HANDLE)
#endif

  tincrvg= 200./float(numtempvg-1)
  tbasevg= 273. - 120. - tincrvg

  pincrvg= 1200./float(numpresvg-1)
  pbasevg= 0. - pincrvg

  do n=1,ncomp
  
    m= idefcomp(n)
  ! radius to diameter
    dp= 2. * radiusmym(m)
    rp= densitygcm3(m)
  
    do ip=1,numpresvg
    
      p= pbasevg + ip*pincrvg
      if(p < 1.) p=1.
    
      do it=1,numtempvg
      
        t= tbasevg + it*tincrvg
      
        vg=vgrav(dp,rp,p,t)
        call iter(vgmod,vg,dp,rp,p,t)
      
      !..table in unit m/s (cm/s computed)
        vgtable(it,ip,m)= vgmod*0.01
      
      end do
    
    end do
  
  end do

#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('VGRAVTABLES',1,ZHOOK_HANDLE)
#endif
  return
end subroutine vgravtables

  real function visc(t)
!==========================================
!   program for calculating viscosity of the air depending on
!   temperature. According to RAFF (1999), Kyle (1991).
!   etha(T) = 1.72e-2*(393/(T+120))*(T/273)**1.5
!   etha         - viscosity of the air (g cm-1 s-1)
!   T             - absolute temperature (K)
!---------------------------------------
  implicit none
! c    real etha    ! viscosity of the air (g cm-1 s-1)
  real :: t        ! absolute temperature (K)
!---------------------------------------
  visc=1.72e-4*(393.0/(t+120.0))*(t/273.0)**1.5

  return
  end function visc

  real function cun(dp)
!==========================================
!   program for calculating Cunningham factor for the small particles
!   according to Zannetti (1990).
!   c = 1 + (2*l/dp)*(a1+a2*exp(-a3*dp/l))
!   c             - Cunningham factor
!   l=0.0653 um     - free path of air molecules
!   dp            - particle size (*10e-4 for dp in micro meters)
!   a1=1.257        - constant
!   a2=0.40        - constant
!   a3=0.55        - constant
!---------------------------------------
  implicit none
! c    real c        ! Cunningham factor
  real :: a1,a2,a3    ! constants
  real :: dp        ! particle size in micro meters
  real :: dp_fac    ! conversion factor micro meters -> cm
  real :: l        ! free path of air molecules
!---------------------------------------
  l=0.0653
  a1=1.257
  a2=0.40
  a3=0.55

  cun=1.0+(2.0*l/dp)*(a1+a2*exp(-a3*dp/l))

  return
  end function cun

  real function fit(u,dp,rp,p,t)
!==========================================
!   program for calculating function for iteration procedure
!   u            - vg during the iteration
!   dp             - particle diameter um
!   rp             - particle density g/cm3
!   etha         - viscosity of the air (g cm-1 s-1)
!   p            - atmospheric presure (hPa)
!   t            - air absolute temperature (K)
!   a1=0.15        - constant
!   a2=0.687        - constant
!---------------------------------------
  implicit none
  real :: u        ! vg during the iteration
  real :: u0        ! vg according to Stokes low
  real :: a1,a2    ! constants
  real :: dp        ! particle size in micro meters
  real :: rp            ! particle density in g/cm3
  real :: etha    ! viscosity of the air
  real :: re        ! Reynolds number
! c    real fit    ! value of the function
  real :: p        ! atmospheric pressure
  real :: t        ! temperature of the air
  external vgrav
  real :: vgrav    ! external function for calculating initial vg
  external visc
  real :: visc    ! external function for calculating viscosity
  external roa
  real :: roa    ! external function for calculating viscosity
!---------------------------------------
  a1=0.15
  a2=0.687

!    write(*,*)
!    write(*,*) 'Function fit'
!    write(*,*) 'rp=',rp
!    write(*,*) 'dp=',dp
  u0=vgrav(dp,rp,p,t)
!    write(*,*) 'u0=',u0
!    write(*,*) 'u=',u
  etha=visc(t)
  re=u*dp*1.0e-4*roa(p,t)/etha
!    write(*,*) 'etha=',etha
!    write(*,*) 're=',re
!    write(*,*) 'u*a1*re**a2=',u*a1*re**a2
  fit=u*(1.0+a1*re**a2)-u0
  if(u == 0.0) fit=-u0
!    write(*,*) 'fit=',fit
!    write(*,*)

  return
  end function fit

  real function vgrav(dp,rp,p,t)
!==========================================
!   program for calculating gravitational setling velocity for particles
!   according to Zannetti (1990).
!   vg = g*(rp-ra(p,t))*c(dp)*dp**2/(18*etha(t))
!   vg=vg(dp,rp,p,t)     - gravitational setling (cm/s)
!   g=981        - acceleration of gravity (cm/s2)
!   rp            - density of particle (g/cm3)
!   ra=ra(p,t)        - density of the air (g/cm3)
!   dp            - particle size (micro meters)
!   etha=etha(t)    - viscosity of the air (g cm-1 s-1)
!   c(dp)        - Cunningham factor for the small particles
!   p            - atmospheric presure (hPa)
!   t            - air absolute temperature (K)
!---------------------------------------
  implicit none
! c    real vgrav    ! gravitational setling
  real :: g        ! acceleration of gravity
  real :: rp        ! density of particle
  real :: ra        ! density of the air
  real :: dp        ! particle size in micro meters
  real :: p        ! atmospheric presure (hPa)
  real :: t        ! air absolute temperature (K)
  real :: dp_fac    ! conversion factor micro meters -> cm
  real :: etha    ! viscosity of the air
  real :: cun    ! external function for calculating C
  external cun
  real :: roa    ! external function for calculating density of the air
  external roa
  real :: visc    ! external function for calculating viscosity of the air
  external visc
!---------------------------------------
  dp_fac=1.0e-4
  ra=roa(p,t)
  g=981.0
  etha=visc(t)
!    write(*,*) 'dp=',dp
!    write(*,*) 'rp=',rp
!    write(*,*) 'p=',p
!    write(*,*) 't=',t
!    write(*,*) 'ra=',ra
!    write(*,*) 'etha=',etha
!    write(*,*) 'c=',cun(dp)
!    vgrav=0.01*(dp*0.0001)**2*g*(rp-ra)*cun(dp)/(18.0*etha)
  vgrav=(dp*0.0001)**2*g*(rp-ra)*cun(dp)/(18.0*etha)
!    vgrav=(dp)**2*g*(rp-ra)*cun(dp)/(18.0*etha)
!    write(*,*) 'vg=',vgrav

  return
  end function vgrav

subroutine iter(vg,u0,dp,rp,p,t)
!=======================================
!   iteration procedure for calculating vg
!   vg            - gravitational settling velocity
!   u0            - vg according to Stokes low
!   y            - fit value during the iteration
!   dp             - particle diameter um
!   rp             - particle density g/cm3
!   etha         - viscosity of the air (g cm-1 s-1)
!   x1,x2        - boundary of the domain for fit
!   x            - value from the fit domain
!   eps            - accuracy of computed vg (0.1%)
!---------------------------------------
  implicit none
  real :: vg        ! computed gravitational settling velocity
  real :: u0        ! vg according to Stokes low
  real :: a1,a2    ! constants
  real :: dp        ! particle size in micro meters
  real :: rp            ! particle density in g/cm3
  real :: etha    ! viscosity of the air
  real :: re        ! Reynolds number
  real :: fit    ! value of the function
! c    real fit    ! external function for calculating iterative vg
! c    real dpdata(4)    ! particle sizes um
  real :: eps    ! accuracy of computed vg (0.1%)
  real :: x1,x2    ! boundary of the domain for fit
  real :: x        ! value from the fit domain
  real :: y,y1,y2    ! fit values during the iteration
  integer :: it    ! itereation number
  real :: p        ! atmospheric pressure
  real :: t        ! temperature of the air
!---------------------------------------
  eps=0.001*u0
  x1=0.0
  x2=u0
  x=x2
  y1=fit(x1,dp,rp,p,t)
  y2=fit(x2,dp,rp,p,t)
  y=fit(x,dp,rp,p,t)
!    write(*,*)
!    write(*,*) 'ITERATION'
!    write(*,*)
!    write(*,*) 'x1=',x1
!    write(*,*) 'x2=',x2
!    write(*,*) 'x=',x
!    write(*,*) 'eps=',eps
!    write(*,*) 'y(0)=',y1
!    write(*,*) 'y(vg)=',y2
!    write(*,*) 'y(vg)=',y
! c    it=0
!    write(*,*)
!    write(*,'(1x,i4,3f10.2,2x,3f10.2,3x,f10.2)')
!     .    it,x1,x2,x,y1,y2,y,x
! c    do it=0,100
! c       x=x1+real(it)*0.01*(x2-x1)
! c       y=fit(x,dp,rp,p,t)
!       write(*,*) it,x,y
! c    enddo
  it=0

!... beginning of iteration

  100 continue
  it=it+1
  x=0.5*(x2+x1)
  y=fit(x,dp,rp,p,t)
!    write(*,*) 'x=',x
!    write(*,*) 'y=',y
!    write(*,'(1x,i4,3f10.2,2x,3f10.2,3x,f10.2)')
!     .    it,x1,x2,x,y1,y2,y,x
  if(it == 100) then
    write(*,*) 'Program terminated'
    stop
  endif
  if(abs(y) < eps) then
    vg=x
    return
  endif
  if(y1*y < 0.0) then
    x2=x
    y2=fit(x2,dp,rp,p,t)
  !       write(*,*) 'y1*y2=',y1*y2
  !       write(*,*) 'x2=',x2
  !       write(*,*) 'y2=',y2
  else
    x1=x
    y1=fit(x1,dp,rp,p,t)
  !       write(*,*) 'y1*y2=',y1*y2
  !       write(*,*) 'x1=',x1
  !       write(*,*) 'y1=',y1
  endif
!    write(*,'(1x,i4,3f10.2,2x,3f10.2,3x,f10.3)')
!     .    it,x1,x2,x,y1,y2,y,x
  go to 100

  return
end subroutine iter

  real function roa(p,t)
!==========================================
!   program for calculating density of the air depending on
!   temperature and pressure. According to RAFF (1999).
!   ro(P,T) = P/(R*T)
!   ro         - density of the dry air (g/cm3)
!   T         - absolute temperature (K)
!   P        - presure (hPa)
!   R=287.04     - universal gas constant J/kg/K
!---------------------------------------
  implicit none
! c    real ro        ! density of the dry air (g/cm3)
  real :: t        ! absolute temperature (K)
  real :: p        ! presure (hPa)
  real :: r        ! gas constant
!---------------------------------------
  r=287.04
  roa=0.001*p*100.0/(r*t)

  return
  end function roa
