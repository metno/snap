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
      subroutine rwalk(tstep,blfullmix,np,pextra)
      use particleML
c
c  Purpose:  Diffusion, in and above boudary layer.
c
c  Method:   Random walk.
c
c  Input:
c     tstep     - time step in seconds (trajectory calculations)
c     blfullmix - full mixing in boundarylayer (true=old,false=new)
c
c---------------------------------------------------------------------
c   24.04.2009 Jerzy Bartnicki: Model particle which goes below the
c   ground or above the top boundary in the random walk is reflected
c   26.03.2011 Jerzy Bartnicki: New parameterization of vertical diffusion in the
c   mixing layer. l-eta proportional to mixing height and the time step.
c   For mixing height = 2500 m and time step = 15 min:
c   In ABL: l-eta=0.28
c   Above ABL: l-eta=0.003
c   For 200< mixing height<2500 and arbitrary time step:
c   In ABL: l-eta=0.28*(mh/2500m)*(tstep/tstep-mix)
c   Above ABL: l-eta=0.003*(tstep/tstep-mix)
c   Entrainment zone = 10%*h
c
#if defined(DRHOOK)
      USE PARKIND1  ,ONLY : JPIM     ,JPRB
      USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
      implicit none
#if defined(DRHOOK)
      REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snappar.inc'
c
      REAL, INTENT(IN) ::    tstep
      LOGICAL, INTENT(IN) :: blfullmix
c particle-loop index, np = 0 means init
      INTEGER, INTENT(IN)  :: np
      TYPE(extraParticle), INTENT(IN) :: pextra
c

      integer i
      real*8, save ::    a,cona,conb,vrange,vrdbla,vrdblb,vrqrt
      real*8, save :: hmax ! maximum mixing height = 2500m
      real*8, save :: tmix ! Characteristic mixing time = 15 min
      real*8, save :: lmax ! Maximum l-eta in the mixing layer = 0.28
      real*8, save :: labove ! Standard l-eta above the mixing layer
      real*8, save :: tfactor ! tfactor=tstep/tmix
c
      real*8 rnd(3), xrand, yrand, zrand, u, v, rl, vabs
      real*8 hfactor, rv, rvmax
c

#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('RWALK',0,ZHOOK_HANDLE)
#endif
c initialization
      if (np .eq. 0) then
        hmax = 2500.0
        tmix = 15.0*60.0
        lmax = 0.28
        labove=0.03
        tfactor=tstep/tmix
c	write(*,*) 'hmax, tmix,lmax, labove, tfactor',
c     &hmax, tmix,lmax, labove, tfactor
c
        a=0.5
        conb=2.*tstep*0.5*(tstep**0.75)*(a**2)
        a=0.25
        cona=2.*tstep*0.5*(tstep**0.75)*(a**2)
c
        vrange=0.02
c       vrdbla=vrange*0.5
c l-eta above mixing height
        vrdbla=labove*tfactor
c
c l-eta below mixing height
        vrdblb=lmax*tfactor
        vrqrt=vrange*0.25
#if defined(DRHOOK)
c     before the return statement
        IF (LHOOK) CALL DR_HOOK('RWALK',1,ZHOOK_HANDLE)
#endif
        return
      end if
c
c--------------------------------------
      if (blfullmix) then
c--------------------------------------
c
c      do np=1,npart // moved particle loop out
c
c..the rand function returns random real numbers between 0.0 and 1.0
c
        call random_number(rnd)
        xrand = rnd(1) - 0.5
        yrand = rnd(2) - 0.5
        zrand = rnd(3) - 0.5

c
        if(pdata(np)%z.gt.pdata(np)%tbl) then
c
c..particle in boundary layer.....................................
c
c..horizontal diffusion
          u=pextra%u
          v=pextra%v
          vabs=sqrt(u*u+v*v)
          rl=sqrt(conb*vabs**1.75)*2.
          pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
          pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
c
c..vertical diffusion
c
          pdata(np)%z=1.-(1.-pdata(np)%tbl)*1.1*(zrand+0.5)
c
        else
c
c..particle above boundary layer..................................
c
c..horizontal diffusion
          u=pextra%u
          v=pextra%v
          vabs=sqrt(u*u+v*v)
          rl=sqrt(cona*vabs**1.75)*2.
          pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
          pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
c
c..vertical diffusion
          pdata(np)%z=pdata(np)%z+vrdbla*zrand
c
        end if

c      end do
c
c--------------------------------------
      else
c--------------------------------------
c
c... no full mixing
c
c      do np=1,npart  // moved particle loop out
c	write(*,*) np,(pdata(i,np),i=1,5)
c
c..the rand function returns random real numbers between 0.0 and 1.0
c
        call random_number(rnd)
        xrand = rnd(1) - 0.5
        yrand = rnd(2) - 0.5
        zrand = rnd(3) - 0.5
c
        if(pdata(np)%z.gt.pdata(np)%tbl) then
c
c..particle in boundary layer.....................................
c
c..horizontal diffusion
          u=pextra%u
          v=pextra%v
          vabs=sqrt(u*u+v*v)
          rl=sqrt(conb*vabs**1.75)*2.
          pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
          pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
c
c..vertical diffusion
         hfactor=pdata(np)%hbl/hmax
         rv=lmax*hfactor*tfactor
         rvmax=1.0-pdata(np)%tbl
c	write(*,*) 'hfactor,rv,rvmax,pdata(np)%tbl,pdata(np)%hbl',
c     &hfactor,rv,rvmax,pdata(np)%tbl,pdata(np)%hbl
c	if(rv .gt. rvmax) then
c	write(*,*) 'rv,rvmax,pdata(np)%z,pdata(np)%tbl',
c     &  rv,rvmax,pdata(np)%z,pdata(np)%tbl
c	stop
c	endif
c
       if(rv .gt. rvmax) rv=rvmax
        pdata(np)%z=pdata(np)%z+rv*zrand
c
c... reflection from the ABL top
c
       if(pdata(np)%z .lt. pdata(np)%tbl) then
c	  write(*,*) 'gora-przed, p4=',pdata(np)%tbl
c	  write(*,*) 'gora-przed, p3=',pdata(np)%z
         pdata(np)%z= 2.0*pdata(np)%tbl-pdata(np)%z
c	  write(*,*) 'gora-po   , p3=',pdata(np)%z
       endif
c
c... reflection from the bottom
c
       if(pdata(np)%z .gt. 1.0) then
c	  write(*,*) 'dol-przed, p=',pdata(np)%z
         pdata(np)%z= 2.0-pdata(np)%z
c	  write(*,*) 'dol-po   , p=',pdata(np)%z
       endif
c
c..vertical limits
         if(pdata(np)%z .gt. 1.0) pdata(np)%z=1.0
         if(pdata(np)%z .lt. pdata(np)%tbl)
     &    pdata(np)%z=pdata(np)%tbl
c
        else
c
c..particle above boundary layer..................................
c
c..horizontal diffusion
          u=pextra%u
          v=pextra%v
          vabs=sqrt(u*u+v*v)
          rl=sqrt(cona*vabs**1.75)*2.

          pdata(np)%x=pdata(np)%x+rl*xrand*pextra%rmx
          pdata(np)%y=pdata(np)%y+rl*yrand*pextra%rmy
c
c..vertical diffusion
          pdata(np)%z=pdata(np)%z+vrdbla*zrand
c
        end if

c      end do
c
c--------------------------------------
      end if
c--------------------------------------
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('RWALK',1,ZHOOK_HANDLE)
#endif
      return
      end
