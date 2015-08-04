      subroutine posint(mode,tf1,tf2,tnow,pextra)
c
c  Purpose:  Interpolation of boundary layer top and height
c            and precipitation intensity to particle positions
c
c  Input:
c	mode=-1: interpolation for the last released plume
c mode>0: interpolate only one particle, np = mode
c		(with all particles at the same horizontal position)
c       tf1:   time in seconds for field set 1 (e.g. 0.)
c       tf2:   time in seconds for field set 2 (e.g. 21600, if 6 hours)
c       tnow:  time in seconds for current paricle positions
c
c
      USE particleML
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
c
      INTEGER, INTENT(IN) :: mode
      REAL, INTENT(IN) ::    tf1,tf2,tnow
      TYPE(extraParticle), INTENT(OUT) :: pextra
c
      integer np,npi, np1,np2,i,j,i1,i2,k
      real    rt1,rt2,dxgrid,dygrid,dx,dy,c1,c2,c3,c4,bl,hbl,rmx,rmy
      real    pr,precmin,p1,p2,plim
c
      REAL, SAVE ::    vminprec = -1.
c
c
c
      precmin=0.01
c initialization, only run once
      if(vminprec.lt.0.) then
       plim=550.
       p2=1000.
       k=1
       do while (p2.gt.plim .and. k.lt.nk)
         k=k+1
         p1=p2
         p2=alevel(k)+blevel(k)*1000.
       end do
       if(k.gt.1) then
         vminprec= vlevel(k-1)
     +       +(vlevel(k)-vlevel(k-1))*(p1-plim)/(p1-p2)
       else
         vminprec=vlevel(nk)
       end if
       write(9,*) 'POSINT. precmin,vminprec: ',precmin,vminprec
      end if
c

c..for linear interpolation in time
      rt1=(tf2-tnow)/(tf2-tf1)
      rt2=(tnow-tf1)/(tf2-tf1)
c
      dxgrid=gparam(7)
      dygrid=gparam(8)
c
      if(mode.eq.-1) then
        np1=npart
        np2=npart
        np=npart
      elseif (mode.gt.0) then
        np = mode
      else
        write(*,*) 'wrong mode to posint:', mode
        call exit(1)
      end if
c
c
      if (pdata(np)%active) then
c
c..for horizotal interpolations
        i=pdata(np)%x
        j=pdata(np)%y
        dx=pdata(np)%x-i
        dy=pdata(np)%y-j
        c1=(1.-dy)*(1.-dx)
        c2=(1.-dy)*dx
        c3=dy*(1.-dx)
        c4=dy*dx
c
c..interpolation
c
c..top of boundary layer
        bl= rt1*(c1*bl1(i,j)  +c2*bl1(i+1,j)
     -          +c3*bl1(i,j+1)+c4*bl1(i+1,j+1))
     -     +rt2*(c1*bl2(i,j)  +c2*bl2(i+1,j)
     -          +c3*bl2(i,j+1)+c4*bl2(i+1,j+1))
c..height of boundary layer
        hbl= rt1*(c1*hbl1(i,j)  +c2*hbl1(i+1,j)
     -           +c3*hbl1(i,j+1)+c4*hbl1(i+1,j+1))
     -      +rt2*(c1*hbl2(i,j)  +c2*hbl2(i+1,j)
     -           +c3*hbl2(i,j+1)+c4*hbl2(i+1,j+1))
c
c..map ratio
        rmx= c1*xm(i,j)  +c2*xm(i+1,j)
     -      +c3*xm(i,j+1)+c4*xm(i+1,j+1)
        rmy= c1*ym(i,j)  +c2*ym(i+1,j)
     -      +c3*ym(i,j+1)+c4*ym(i+1,j+1)
c
c..precipitation intensity (mm/hour)
        pr= c1*precip(i,j,  iprecip)+c2*precip(i+1,j,  iprecip)
     -     +c3*precip(i,j+1,iprecip)+c4*precip(i+1,j+1,iprecip)
c
c..update boundary layer top and height, map ratio and precipitation
c
        pdata(np)%tbl=bl
        pdata(np)%hbl=hbl
        pextra%rmx=rmx/dxgrid
        pextra%rmy=rmy/dygrid
        pextra%prc=pr

c..reset precipitation to zero if pressure less than approx. 550 hPa.
c..and if less than a minimum precipitation intensity (mm/hour)
        if(pdata(np)%z.lt.vminprec .or.
     +     pextra%prc.lt.precmin) pextra%prc=0.
c
c end loop ove active particles
      endif
c
      if(mode.eq.-1) then
c..copy interpolated data to the other particles in the plume
       np1=iplume(1,nplume)
       do npi=np1,np2-1
         pdata(npi)%tbl = pdata(np2)%tbl
         pdata(npi)%hbl = pdata(np2)%hbl
       end do
      end if
c
      return
      end
