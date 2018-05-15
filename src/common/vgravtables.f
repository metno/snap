! SNAP: Servere Nuclear Accident Programme
! Copyright (C) 1992-2017   Norwegian Meteorological Institute
! 
! This file is part of SNAP. SNAP is free software: you can 
! redistribute it and/or modify it under the terms of the 
! GNU General Public License as published by the 
! Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
!
      subroutine vgravtables
c==========================================
c
c   program for calculating the gravitational settling velocities
c   for small and large particles (outside the Stokes low)
c---------------------------------------
      USE snapparML

#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
       real t        ! absolute temperature (K)
       external vgrav
       real vgrav    ! external function for calculating initial vg
       real dp        ! particle size um
       real rp        ! particle density in g/m3
       real vg        ! gravitational setling in cm/s (Stokes low)
       real vgmod    ! gravitational setling in cm/s after iteration
       real p        ! atmospheric pressure
c
       integer n,m,ip,it
c---------------------------------------
c
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('VGRAVTABLES',0,ZHOOK_HANDLE)
#endif
c
       tincrvg= 200./float(numtempvg-1)
       tbasevg= 273. - 120. - tincrvg
c
       pincrvg= 1200./float(numpresvg-1)
       pbasevg= 0. - pincrvg
c
       do n=1,ncomp
c
         m= idefcomp(n)
c radius to diameter
         dp= 2. * radiusmym(m)
         rp= densitygcm3(m)
c
         do ip=1,numpresvg
c
           p= pbasevg + ip*pincrvg
           if(p.lt.1.) p=1.
c
           do it=1,numtempvg
c
             t= tbasevg + it*tincrvg
c
             vg=vgrav(dp,rp,p,t)
             call iter(vgmod,vg,dp,rp,p,t)
c
c..table in unit m/s (cm/s computed)
             vgtable(it,ip,m)= vgmod*0.01
c
           end do
c
         end do
c
       end do
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('VGRAVTABLES',1,ZHOOK_HANDLE)
#endif
       return
      end
c
      real function visc(t)
c==========================================
c   program for calculating viscosity of the air depending on
c   temperature. According to RAFF (1999), Kyle (1991).
c   etha(T) = 1.72e-2*(393/(T+120))*(T/273)**1.5
c   etha         - viscosity of the air (g cm-1 s-1)
c   T             - absolute temperature (K)
c---------------------------------------
       implicit none
ccc    real etha    ! viscosity of the air (g cm-1 s-1)
       real t        ! absolute temperature (K)
c---------------------------------------
       visc=1.72e-4*(393.0/(t+120.0))*(t/273.0)**1.5
c
       return
      end
c
      real function cun(dp)
c==========================================
c   program for calculating Cunningham factor for the small particles
c   according to Zannetti (1990).
c   c = 1 + (2*l/dp)*(a1+a2*exp(-a3*dp/l))
c   c             - Cunningham factor
c   l=0.0653 um     - free path of air molecules
c   dp            - particle size (*10e-4 for dp in micro meters)
c   a1=1.257        - constant
c   a2=0.40        - constant
c   a3=0.55        - constant
c---------------------------------------
       implicit none
ccc    real c        ! Cunningham factor
       real a1,a2,a3    ! constants
       real dp        ! particle size in micro meters
       real dp_fac    ! conversion factor micro meters -> cm
       real l        ! free path of air molecules
c---------------------------------------
       l=0.0653
       a1=1.257
       a2=0.40
       a3=0.55
c
       cun=1.0+(2.0*l/dp)*(a1+a2*exp(-a3*dp/l))
c
       return
      end
c
      real function fit(u,dp,rp,p,t)
c==========================================
c   program for calculating function for iteration procedure
c   u            - vg during the iteration
c   dp             - particle diameter um
c   rp             - particle density g/cm3
c   etha         - viscosity of the air (g cm-1 s-1)
c   p            - atmospheric presure (hPa)
c   t            - air absolute temperature (K)
c   a1=0.15        - constant
c   a2=0.687        - constant
c---------------------------------------
       implicit none
       real u        ! vg during the iteration
       real u0        ! vg according to Stokes low
       real a1,a2    ! constants
       real dp        ! particle size in micro meters
       real rp            ! particle density in g/cm3
       real etha    ! viscosity of the air
       real re        ! Reynolds number
ccc    real fit    ! value of the function
       real p        ! atmospheric pressure
       real t        ! temperature of the air
       external vgrav
       real vgrav    ! external function for calculating initial vg
       external visc
       real visc    ! external function for calculating viscosity
       external roa
       real roa    ! external function for calculating viscosity
c---------------------------------------
       a1=0.15
       a2=0.687
c
c    write(*,*)
c    write(*,*) 'Function fit'
c    write(*,*) 'rp=',rp
c    write(*,*) 'dp=',dp
       u0=vgrav(dp,rp,p,t)
c    write(*,*) 'u0=',u0
c    write(*,*) 'u=',u
       etha=visc(t)
       re=u*dp*1.0e-4*roa(p,t)/etha
c    write(*,*) 'etha=',etha
c    write(*,*) 're=',re
c    write(*,*) 'u*a1*re**a2=',u*a1*re**a2
       fit=u*(1.0+a1*re**a2)-u0
       if(u .eq. 0.0) fit=-u0
c    write(*,*) 'fit=',fit
c    write(*,*)
c
       return
       end
c
      real function vgrav(dp,rp,p,t)
c==========================================
c   program for calculating gravitational setling velocity for particles
c   according to Zannetti (1990).
c   vg = g*(rp-ra(p,t))*c(dp)*dp**2/(18*etha(t))
c   vg=vg(dp,rp,p,t)     - gravitational setling (cm/s)
c   g=981        - acceleration of gravity (cm/s2)
c   rp            - density of particle (g/cm3)
c   ra=ra(p,t)        - density of the air (g/cm3)
c   dp            - particle size (micro meters)
c   etha=etha(t)    - viscosity of the air (g cm-1 s-1)
c   c(dp)        - Cunningham factor for the small particles
c   p            - atmospheric presure (hPa)
c   t            - air absolute temperature (K)
c---------------------------------------
       implicit none
ccc    real vgrav    ! gravitational setling
       real g        ! acceleration of gravity
       real rp        ! density of particle
       real ra        ! density of the air
       real dp        ! particle size in micro meters
       real p        ! atmospheric presure (hPa)
       real t        ! air absolute temperature (K)
       real dp_fac    ! conversion factor micro meters -> cm
       real etha    ! viscosity of the air
       real cun    ! external function for calculating C
       external cun
       real roa    ! external function for calculating density of the air
       external roa
       real visc    ! external function for calculating viscosity of the air
       external visc
c---------------------------------------
       dp_fac=1.0e-4
       ra=roa(p,t)
       g=981.0
       etha=visc(t)
c    write(*,*) 'dp=',dp
c    write(*,*) 'rp=',rp
c    write(*,*) 'p=',p
c    write(*,*) 't=',t
c    write(*,*) 'ra=',ra
c    write(*,*) 'etha=',etha
c    write(*,*) 'c=',cun(dp)
c    vgrav=0.01*(dp*0.0001)**2*g*(rp-ra)*cun(dp)/(18.0*etha)
       vgrav=(dp*0.0001)**2*g*(rp-ra)*cun(dp)/(18.0*etha)
c    vgrav=(dp)**2*g*(rp-ra)*cun(dp)/(18.0*etha)
c    write(*,*) 'vg=',vgrav
c
       return
      end
c
      subroutine iter(vg,u0,dp,rp,p,t)
c=======================================
c   iteration procedure for calculating vg
c   vg            - gravitational settling velocity
c   u0            - vg according to Stokes low
c   y            - fit value during the iteration
c   dp             - particle diameter um
c   rp             - particle density g/cm3
c   etha         - viscosity of the air (g cm-1 s-1)
c   x1,x2        - boundary of the domain for fit
c   x            - value from the fit domain
c   eps            - accuracy of computed vg (0.1%)
c---------------------------------------
       implicit none
       real vg        ! computed gravitational settling velocity
       real u0        ! vg according to Stokes low
       real a1,a2    ! constants
       real dp        ! particle size in micro meters
       real rp            ! particle density in g/cm3
       real etha    ! viscosity of the air
       real re        ! Reynolds number
       real fit    ! value of the function
ccc    real fit    ! external function for calculating iterative vg
ccc    real dpdata(4)    ! particle sizes um
       real eps    ! accuracy of computed vg (0.1%)
       real x1,x2    ! boundary of the domain for fit
       real x        ! value from the fit domain
       real y,y1,y2    ! fit values during the iteration
       integer it    ! itereation number
       real p        ! atmospheric pressure
       real t        ! temperature of the air
c---------------------------------------
       eps=0.001*u0
       x1=0.0
       x2=u0
       x=x2
       y1=fit(x1,dp,rp,p,t)
       y2=fit(x2,dp,rp,p,t)
       y=fit(x,dp,rp,p,t)
c    write(*,*)
c    write(*,*) 'ITERATION'
c    write(*,*)
c    write(*,*) 'x1=',x1
c    write(*,*) 'x2=',x2
c    write(*,*) 'x=',x
c    write(*,*) 'eps=',eps
c    write(*,*) 'y(0)=',y1
c    write(*,*) 'y(vg)=',y2
c    write(*,*) 'y(vg)=',y
ccc    it=0
c    write(*,*)
c    write(*,'(1x,i4,3f10.2,2x,3f10.2,3x,f10.2)')
c     .    it,x1,x2,x,y1,y2,y,x
ccc    do it=0,100
ccc       x=x1+real(it)*0.01*(x2-x1)
ccc       y=fit(x,dp,rp,p,t)
c       write(*,*) it,x,y
ccc    enddo
       it=0
c
c... beginning of iteration
c
100    continue
       it=it+1
       x=0.5*(x2+x1)
       y=fit(x,dp,rp,p,t)
c    write(*,*) 'x=',x
c    write(*,*) 'y=',y
c    write(*,'(1x,i4,3f10.2,2x,3f10.2,3x,f10.2)')
c     .    it,x1,x2,x,y1,y2,y,x
       if(it .eq. 100) then
          write(*,*) 'Program terminated'
          stop
       endif
       if(abs(y) .lt. eps) then
          vg=x
          return
       endif
       if(y1*y .lt. 0.0) then
          x2=x
          y2=fit(x2,dp,rp,p,t)
c       write(*,*) 'y1*y2=',y1*y2
c       write(*,*) 'x2=',x2
c       write(*,*) 'y2=',y2
       else
          x1=x
          y1=fit(x1,dp,rp,p,t)
c       write(*,*) 'y1*y2=',y1*y2
c       write(*,*) 'x1=',x1
c       write(*,*) 'y1=',y1
       endif
c    write(*,'(1x,i4,3f10.2,2x,3f10.2,3x,f10.3)')
c     .    it,x1,x2,x,y1,y2,y,x
       go to 100
c
       return
      end
c
      real function roa(p,t)
c==========================================
c   program for calculating density of the air depending on
c   temperature and pressure. According to RAFF (1999).
c   ro(P,T) = P/(R*T)
c   ro         - density of the dry air (g/cm3)
c   T         - absolute temperature (K)
c   P        - presure (hPa)
c   R=287.04     - universal gas constant J/kg/K
c---------------------------------------
       implicit none
ccc    real ro        ! density of the dry air (g/cm3)
       real t        ! absolute temperature (K)
       real p        ! presure (hPa)
       real r        ! gas constant
c---------------------------------------
       r=287.04
       roa=0.001*p*100.0/(r*t)
c
       return
      end
