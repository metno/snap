      subroutine rwalk(tstep,blfullmix)
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
      real    tstep
      logical blfullmix
c
      integer np,i
      real    a,cona,conb,vrange,vrdbla,vrdblb,vrqrt
      real    xrand,yrand,zrand,u,v,vabs,rl,rv,rv2,vpos,rvmax
       real hmax	! maximum mixing height = 2500m
       real tmix	! Characteristic mixing time = 15 min
       real lmax	! Maximum l-eta in the mixing layer = 0.28
       real labove	! Standard l-eta above the mixing layer
       real tfactor	! tfactor=tstep/tmix
       real hfactor    ! hfactor=h/hmax		!
c
c..for random number functions
      integer irand,iseed
c
#if defined(linux)
      real rand
#else
      double precision rand
#endif
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('RWALK',0,ZHOOK_HANDLE)
#endif
c
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
c      vrdbla=vrange*0.5
c l-eta above mixing height
      vrdbla=labove*tfactor
c
c l-eta below mixing height
      vrdblb=lmax*tfactor
      vrqrt=vrange*0.25	
c
c..for random number generation
c..(better if moved into particle loop ?)
      iseed=irand()
      call srand(iseed)
c
c--------------------------------------
      if (blfullmix) then
c--------------------------------------
c
      do np=1,npart
c
c..the rand function returns random real numbers between 0.0 and 1.0
c
        xrand=rand()
        yrand=rand()
        zrand=rand()
c
        xrand=(xrand-0.5)
        yrand=(yrand-0.5)
        zrand=(zrand-0.5)
c
        if(pdata(3,np).gt.pdata(4,np)) then
c
c..particle in boundary layer.....................................
c
c..horizontal diffusion
         u=pwork(np,1)
         v=pwork(np,2)
          vabs=sqrt(u*u+v*v)
          rl=sqrt(conb*vabs**1.75)*2.
          pdata(1,np)=pdata(1,np)+rl*xrand*pdata(6,np)
          pdata(2,np)=pdata(2,np)+rl*yrand*pdata(7,np)
c
c..vertical diffusion
c
          pdata(3,np)=1.-(1.-pdata(4,np))*1.1*(zrand+0.5)
c
        else
c
c..particle above boundary layer..................................
c
c..horizontal diffusion
         u=pwork(np,1)
         v=pwork(np,2)
          vabs=sqrt(u*u+v*v)
          rl=sqrt(cona*vabs**1.75)*2.	   
          pdata(1,np)=pdata(1,np)+rl*xrand*pdata(6,np)
          pdata(2,np)=pdata(2,np)+rl*yrand*pdata(7,np)
c
c..vertical diffusion
          pdata(3,np)=pdata(3,np)+vrdbla*zrand
c
        end if

      end do
c
c--------------------------------------
      else
c--------------------------------------
c
c... no full mixing
c
      do np=1,npart
c	write(*,*) np,(pdata(i,np),i=1,5)
c
c..the rand function returns random real numbers between 0.0 and 1.0
c
        xrand=rand()
        yrand=rand()
        zrand=rand()
c
        xrand=(xrand-0.5)
        yrand=(yrand-0.5)
        zrand=(zrand-0.5)
c
        if(pdata(3,np).gt.pdata(4,np)) then
c
c..particle in boundary layer.....................................
c
c..horizontal diffusion
         u=pwork(np,1)
         v=pwork(np,2)
          vabs=sqrt(u*u+v*v)
          rl=sqrt(conb*vabs**1.75)*2.
          pdata(1,np)=pdata(1,np)+rl*xrand*pdata(6,np)
          pdata(2,np)=pdata(2,np)+rl*yrand*pdata(7,np)
c
c..vertical diffusion
         hfactor=pdata(5,np)/hmax
         rv=lmax*hfactor*tfactor
         rvmax=1.0-pdata(4,np)
c	write(*,*) 'hfactor,rv,rvmax,pdata(4,np),pdata(5,np)',
c     &hfactor,rv,rvmax,pdata(4,np),pdata(5,np)
c	if(rv .gt. rvmax) then
c	write(*,*) 'rv,rvmax,pdata(3,np),pdata(4,np)',
c     &  rv,rvmax,pdata(3,np),pdata(4,np)
c	stop
c	endif
c
       if(rv .gt. rvmax) rv=rvmax
        pdata(3,np)=pdata(3,np)+rv*zrand
c
c... reflection from the ABL top
c
       if(pdata(3,np) .lt. pdata(4,np)) then
c	  write(*,*) 'gora-przed, p4=',pdata(4,np)
c	  write(*,*) 'gora-przed, p3=',pdata(3,np)
         pdata(3,np)= 2.0*pdata(4,np)-pdata(3,np)
c	  write(*,*) 'gora-po   , p3=',pdata(3,np)
       endif
c
c... reflection from the bottom
c
       if(pdata(3,np) .gt. 1.0) then
c	  write(*,*) 'dol-przed, p=',pdata(3,np)
         pdata(3,np)= 2.0-pdata(3,np)
c	  write(*,*) 'dol-po   , p=',pdata(3,np)
       endif
c
c..vertical limits
         if(pdata(3,np) .gt. 1.0) pdata(3,np)=1.0
         if(pdata(3,np) .lt. pdata(4,np)) 
     &    pdata(3,np)=pdata(4,np)
c
        else
c
c..particle above boundary layer..................................
c
c..horizontal diffusion
         u=pwork(np,1)
         v=pwork(np,2)
          vabs=sqrt(u*u+v*v)
          rl=sqrt(cona*vabs**1.75)*2.
          pdata(1,np)=pdata(1,np)+rl*xrand*pdata(6,np)
          pdata(2,np)=pdata(2,np)+rl*yrand*pdata(7,np)
c
c..vertical diffusion
          pdata(3,np)=pdata(3,np)+vrdbla*zrand
c
        end if

      end do
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
