
      subroutine forwrd_dx(tf1,tf2,tnow,tstep,np,nparticle,
     +                     delx, dely, delz, u, v)
      use particleML
c
c  Purpose:  calculate dx,dy,dz forward movement of particle at pos np
c
c  Notes:
c    - sigma levels (norlam) or eta levels (hirlam,...)
c      defined by alevel and blevel
c    - horizontal wind components in unit m/s
c    - vertical   wind component  in unit vlevel/second
c      (vlevel = sigma or eta(alevel,blevel))
c    - all wind components in non-staggered horizontal grid
c      and in the same levels
c    - lower model level is level 2
c
c  Input:
c       tf1:   time in seconds for field set 1 (e.g. 0.)
c       tf2:   time in seconds for field set 2 (e.g. 21600, if 6 hours)
c       tnow:  time in seconds for current paricle positions
c       tstep: timestep in seconds
c  Output:
c       delx, dely, delz: offset in x,y,z; x/y without mapfactor
c       u, v: wind-speed in x/y
      implicit none
      real, INTENT(IN) :: tf1,tf2,tnow,tstep
      INTEGER, INTENT(IN) :: np
      TYPE(particle), INTENT(IN) :: nparticle
      REAL, INTENT(OUT) :: delx, dely, delz, u, v

      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
      include 'snaptab.inc'

      integer i,j,m,ilvl,k1,k2,itab,kt1,kt2,ip,it
      real    dt,rt1,rt2,dx,dy,c1,c2,c3,c4,vlvl
      real    dz1,dz2,uk1,uk2,vk1,vk2,wk1,wk2,w
      real    th,tk1,tk2,ps,p,pi,t,gravity,grav1,grav2,pvg,tvg
      real    pi1,pi2,dz,deta,wg,rtab
ccc
c
      real, save :: cpinv, rcpinv, ginv, vmax, dxgrid,dygrid
c
c

c initialization
      if (np .eq. 0) then
        j=0
        do i=1,ncomp
          m= idefcomp(i)
          if(kgravity(m).gt.1) j=j+1
        end do
        if (j.gt.0) then
         write(9,*) 'Computing gravity tables...'
         call vgravtables
         write(9,*) 'Surface gravity (1000hPa, 300K):'
         it = (300-tbasevg)/tincrvg
         ip = (1000-pbasevg)/pincrvg
         do i=1,ncomp
           m= idefcomp(i)
           write(9,*) ' particle ', compname(m), ": ", vgtable(it,ip,m)
         end do
        else
          write(9,*) 'Computation of gravity tables not needed'
        end if
        ginv= 1./g
        cpinv= 1./cp
        rcpinv=cp/r
        dxgrid=gparam(7)
        dygrid=gparam(8)
        vmax=vlevel( 1)
      return

      end if
c
      dt=tstep
c
c
c..for linear interpolation in time
      rt1=(tf2-tnow)/(tf2-tf1)
      rt2=(tnow-tf1)/(tf2-tf1)
c
c
c      do np=1,npart // particle loop moved out (snap.F)
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
c..for vertical interpolation (sigma/eta levels)
        vlvl=pdata(np)%z
        ilvl=vlvl*10000.
        k1=ivlevel(ilvl)
        k2=k1+1
        dz1=(vlvl-vlevel(k2))/(vlevel(k1)-vlevel(k2))
        dz2=1.-dz1
c
c..interpolation
c
c..u
        uk1= rt1*(c1*u1(i,j,k1)  +c2*u1(i+1,j,k1)
     -           +c3*u1(i,j+1,k1)+c4*u1(i+1,j+1,k1))
     -      +rt2*(c1*u2(i,j,k1)  +c2*u2(i+1,j,k1)
     -           +c3*u2(i,j+1,k1)+c4*u2(i+1,j+1,k1))
        uk2= rt1*(c1*u1(i,j,k2)  +c2*u1(i+1,j,k2)
     -           +c3*u1(i,j+1,k2)+c4*u1(i+1,j+1,k2))
     -      +rt2*(c1*u2(i,j,k2)  +c2*u2(i+1,j,k2)
     -           +c3*u2(i,j+1,k2)+c4*u2(i+1,j+1,k2))
        u=uk1*dz1+uk2*dz2
c..v
        vk1= rt1*(c1*v1(i,j,k1)  +c2*v1(i+1,j,k1)
     -           +c3*v1(i,j+1,k1)+c4*v1(i+1,j+1,k1))
     -      +rt2*(c1*v2(i,j,k1)  +c2*v2(i+1,j,k1)
     -           +c3*v2(i,j+1,k1)+c4*v2(i+1,j+1,k1))
        vk2= rt1*(c1*v1(i,j,k2)  +c2*v1(i+1,j,k2)
     -           +c3*v1(i,j+1,k2)+c4*v1(i+1,j+1,k2))
     -      +rt2*(c1*v2(i,j,k2)  +c2*v2(i+1,j,k2)
     -           +c3*v2(i,j+1,k2)+c4*v2(i+1,j+1,k2))
        v=vk1*dz1+vk2*dz2
c..w
        wk1= rt1*(c1*w1(i,j,k1)  +c2*w1(i+1,j,k1)
     -           +c3*w1(i,j+1,k1)+c4*w1(i+1,j+1,k1))
     -      +rt2*(c1*w2(i,j,k1)  +c2*w2(i+1,j,k1)
     -           +c3*w2(i,j+1,k1)+c4*w2(i+1,j+1,k1))
        wk2= rt1*(c1*w1(i,j,k2)  +c2*w1(i+1,j,k2)
     -           +c3*w1(i,j+1,k2)+c4*w1(i+1,j+1,k2))
     -      +rt2*(c1*w2(i,j,k2)  +c2*w2(i+1,j,k2)
     -           +c3*w2(i,j+1,k2)+c4*w2(i+1,j+1,k2))
        w=wk1*dz1+wk2*dz2
c
        m=icomp(np)
c
       if(kgravity(m).gt.0) then
c
c..potential temperature (no pot.temp. at surface...)
         kt1= max(k1,2)
         kt2= kt1+1
          tk1= rt1*(c1*t1(i,j,  kt1)+c2*t1(i+1,j,  kt1)
     -             +c3*t1(i,j+1,kt1)+c4*t1(i+1,j+1,kt1))
     -        +rt2*(c1*t2(i,j,  kt1)+c2*t2(i+1,j,  kt1)
     -             +c3*t2(i,j+1,kt1)+c4*t2(i+1,j+1,kt1))
          tk2= rt1*(c1*t1(i,j,  kt2)+c2*t1(i+1,j,  kt2)
     -             +c3*t1(i,j+1,kt2)+c4*t1(i+1,j+1,kt2))
     -        +rt2*(c1*t2(i,j,  kt2)+c2*t2(i+1,j,  kt2)
     -             +c3*t2(i,j+1,kt2)+c4*t2(i+1,j+1,kt2))
          th=tk1*dz1+tk2*dz2
c
c..pressure
          ps= rt1*(c1*ps1(i,j)  +c2*ps1(i+1,j)
     +            +c3*ps1(i,j+1)+c4*ps1(i+1,j+1))
     +       +rt2*(c1*ps2(i,j)  +c2*ps2(i+1,j)
     +            +c3*ps2(i,j+1)+c4*ps2(i+1,j+1))
c
          if(kgravity(m).eq.2) then
            p= alevel(k1) + blevel(k1)*ps
            rtab=p*pmult
            itab=int(rtab)
            pi1= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
            p= alevel(k2) + blevel(k2)*ps
            rtab=p*pmult
            itab=rtab
            pi2= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
           pi= pi1*dz1+pi2*dz2
            t=th*pi*cpinv
           p= 1000.*(pi*cpinv)**rcpinv
c.old       gravity= vgrav(radiusmym(m),densitygcm3(m),p,t)
           ip= (p-pbasevg)/pincrvg
           if(ip.lt.1) ip=1
           if(ip.ge.numpresvg) ip=numpresvg-1
           pvg= pbasevg + ip*pincrvg
           it= (t-tbasevg)/tincrvg
           if(it.lt.1) it=1
           if(it.ge.numtempvg) it=numtempvg-1
           tvg= tbasevg + it*tincrvg
           grav1= vgtable(it,ip,m)
     +        + (vgtable(it+1,ip,m)-vgtable(it,ip,m))
     +        *(t-tvg)/tincrvg
           ip= ip+1
           grav2= vgtable(it,ip,m)
     +        + (vgtable(it+1,ip,m)-vgtable(it,ip,m))
     +        *(t-tvg)/tincrvg
           gravity= grav1 + (grav2-grav1) * (p-pvg)/pincrvg
c######################################################################
c     if(np.lt.21) write(6,*) '  p,t,gravity: ',p,t,gravity
c######################################################################
         else
           gravity= gravityms(m)
         end if
c
         pdata(np)%grv= gravity

c
c..gravity ... a very simple, probably too simple (!!!) conversion
c............. from m/s to model etadot/sigmadot "vertical velocity"
          k1=ivlayer(ilvl)
c.......................................???????????????????????????????
         if (k1.eq.nk) k1=k1-1
c.......................................???????????????????????????????
          k2=k1+1
          p=ahalf(k1)+bhalf(k1)*ps
          rtab=p*pmult
          itab=rtab
          pi1= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
          p=ahalf(k2)+bhalf(k2)*ps
          rtab=p*pmult
          itab=rtab
          pi2= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
         dz= th*(pi1-pi2)*ginv
         deta= vhalf(k1)-vhalf(k2)
         wg= gravity*deta/dz
c######################################################################
c   if(np.lt.21) write(6,*) 'np,k2,w,wg: ',np,k2,w,wg
c######################################################################
#if !defined(TRAJ)
         w= w + wg
#endif
       end if

c
c..update position
c
        delx=u*dt
        dely=v*dt
        delz=w*dt
c
c      end do
c
      return
      end
