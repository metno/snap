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
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snappar.inc'
c
      real    tstep
      logical blfullmix
c
      integer np
      real    a,cona,conb,vrange,vrdbla,vrdblb,vrqrt
      real    xrand,yrand,zrand,u,v,vabs,rl
c
c..for random number functions
      integer irand,iseed
c
#if defined(linux)
      real rand
#else
      double precision rand
#endif
c
c
      a=0.5
      conb=2.*tstep*0.5*(tstep**0.75)*(a**2)
      a=0.25
      cona=2.*tstep*0.5*(tstep**0.75)*(a**2)
c
      vrange=0.02
      vrdbla=vrange*0.5
      vrdblb=vrange*4.
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
cjb
cjb... limits for the mixing height
cjb
	if(pdata(4,np) .gt. 0.97) pdata(4,np)=0.97
	if(pdata(4,np) .lt. 0.72) pdata(4,np)=0.72
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
cjb...13.11 No entrainment
c          pdata(3,np)=1.-(1.-pdata(4,np)+vrqrt)*(zrand+0.5)
          pdata(3,np)=1.-(1.-pdata(4,np))*(zrand+0.5)
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
cjb
cjb... limits for the mixing height
cjb
	if(pdata(4,np) .gt. 0.97) pdata(4,np)=0.97
	if(pdata(4,np) .lt. 0.72) pdata(4,np)=0.72
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
c          pdata(3,np)=pdata(3,np)+vrdblb*zrand
cjb...16.11
          pdata(3,np)=pdata(3,np)+
     &    zrand*0.2*vabs*tstep*(pdata(4,np)-0.72)/0.28
c..vertical limits
	  if(pdata(3,np) .gt. 1.0) pdata(3,np)=1.0
	  if(pdata(3,np) .lt. pdata(4,np)) 
     &    pdata(3,np)=2.0*pdata(4,np)-pdata(3,np)
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
      return
      end
