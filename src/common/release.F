      subroutine release(istep,nsteph,tf1,tf2,tnow,ierror)
c
c  Purpose:  Release one plume of particles
c            The particles are spread in a cylinder volume if radius>0,
c	     otherwise in a column
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
      include 'snappos.inc'
      include 'snapfld.inc'
      include 'snaptab.inc'
c
c..input/output
      integer istep,nsteph
      real    tf1,tf2,tnow
      integer ierror
c
c..local
      integer i,j,n,k,m,itab,nprel,nt,npar1,numradius,nrad
      integer nrel(mcomp),nrel2(mcomp,2)
      real    x,y,dxgrid,dygrid,dx,dy,xrand,yrand,zrand,twodx,twody
      real    rt1,rt2,dxx,dyy,c1,c2,c3,c4,tstep
      real    ginv,ps,rtab,th,p,pihu,pih,pif,hhu,h1,h2,h,vlev
      real    e,a,b,c,ecos,esin,s,gcos,gsin,rcos,smax,b2,en,en2
      real    rbqmax,rbq,pscale,radius,hlower,hupper,stemradius
      real    pi,volume1,volume2
      real    relbq(mcomp),pbq(mcomp)
      real    hlevel(nk)
      real    hlower2(2),hupper2(2),radius2(2)

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
      IF (LHOOK) CALL DR_HOOK('RELEASE',0,ZHOOK_HANDLE)
#endif
c
      if(itprof.eq.2) then
c..single bomb release
        tstep=1.
      else
        tstep=3600./float(nsteph)
      end if
c
c..particle number scaled according to max Bq released
      rbqmax=0.
      do n=1,ntprof
       rbq=0.
       do m=1,ncomp
         rbq=rbq+relbqsec(n,m)
       end do
       if(rbqmax.lt.rbq) rbqmax=rbq
      end do
      pscale= float(mprel)/(rbqmax*tstep)
c
      nt=1
      do n=2,ntprof
       if(irelhour(n)*nsteph.le.istep) nt=n
      end do
c
      if(itprof.ne.4 .and. nt.lt.ntprof) then
       c1=irelhour(nt)  *nsteph
       c2=irelhour(nt+1)*nsteph
       c3=istep
       rt1=(c2-c3)/(c2-c1)
       rt2=(c3-c2)/(c2-c1)
       do n=1,ncomp
         relbq(n)= relbqsec(nt,n)*rt1 + relbqsec(nt+1,n)*rt2
       end do
       radius=         relradius(nt)*rt1 +     relradius(nt+1)*rt2
       hupper=          relupper(nt)*rt1 +      relupper(nt+1)*rt2
       hlower=          rellower(nt)*rt1 +      rellower(nt+1)*rt2
       stemradius= relstemradius(nt)*rt1 + relstemradius(nt+1)*rt2
      else
       do n=1,ncomp
         relbq(n)= relbqsec(nt,n)
       end do
       radius=         relradius(nt)
       hupper=          relupper(nt)
       hlower=          rellower(nt)
       stemradius= relstemradius(nt)
      end if
c
      nprel=0
      do m=1,ncomp
#if defined(BOMB)
c Number of particles equal for each component, but not Bq
c shouldn't this be everywhere ?????
       nrel(m)=nint(float(mprel)/float(ncomp))
#else
       nrel(m)= nint(relbq(m)*tstep*pscale)
#endif
       if(nrel(m).gt.0) then
         pbq(m)= relbq(m)*tstep/float(nrel(m))
         nprel= nprel + nrel(m)
       else
         nrel(m)= 0
         pbq(m)=0.
       end if
      end do
c
c################################################################
c      if(mod(istep,nsteph).eq.0) then
        do m=1,ncomp
ccc	  write(6,*) 'release comp,num,bq:',m,nrel(m),pbq(m)
         write(9,*) 'release comp,num,bq:',m,nrel(m),pbq(m)
        end do
ccc	write(6,*) 'nprel: ',nprel
       write(9,*) 'nprel: ',nprel
c      end if
c################################################################
c
      if(nplume+1.gt.mplume .or. npart+nprel.gt.mpart) then
       ierror=1
       return
      end if
c
      npar1= npart+1
c
c---------------------------------------------------
c
      x= relpos(3,irelpos)
      y= relpos(4,irelpos)
      i= nint(x)
      j= nint(y)
c
c..compute height in model (eta/sigma) levels
c
      rt1=(tf2-tnow)/(tf2-tf1)
      rt2=(tnow-tf1)/(tf2-tf1)
      i= int(x)
      j= int(y)
      dxx=x-i
      dyy=y-j
      c1=(1.-dyy)*(1.-dxx)
      c2=(1.-dyy)*dxx
      c3=dyy*(1.-dxx)
      c4=dyy*dxx

      ps= rt1*(c1*ps1(i,j)  +c2*ps1(i+1,j)
     +        +c3*ps1(i,j+1)+c4*ps1(i+1,j+1))
     +   +rt2*(c1*ps2(i,j)  +c2*ps2(i+1,j)
     +        +c3*ps2(i,j+1)+c4*ps2(i+1,j+1))

      rtab=ps*pmult
      itab=rtab
      pihu=  pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)

      hlevel(1)= 0.
      hhu= 0.
c
      ginv=1./g
c
      do k=2,nk
c
        th= rt1*(c1*t1(i,j,  k)+c2*t1(i+1,j,  k)
     +          +c3*t1(i,j+1,k)+c4*t1(i+1,j+1,k))
     +     +rt2*(c1*t2(i,j,  k)+c2*t2(i+1,j,  k)
     +          +c3*t2(i,j+1,k)+c4*t2(i+1,j+1,k))
c
        p=ahalf(k)+bhalf(k)*ps
        rtab=p*pmult
        itab=rtab
        pih= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
c
        p=alevel(k)+blevel(k)*ps
        rtab=p*pmult
        itab=rtab
        pif= pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
c
       h1=hhu
        h2=h1 + th*(pihu-pih)*ginv
        hlevel(k)= h1 + (h2-h1)*(pihu-pif)/(pihu-pih)
c
       hhu= h2
       pihu=pih
c
      end do
c
      if (hupper.gt.hlevel(nk)) hupper= hlevel(nk)
c
c---------------------------------------------------
c
c..release types:
c..1) one coloumn
c..2) one cylinder
c..3) two cylinders, mushroom (upside/down mushroom is possible!)
c
      if(stemradius.le.0. .or. hlower.lt.1. .or. radius.le.0.) then
c
        numradius=1
        hlower2(1)=hlower
        hupper2(1)=hupper
        radius2(1)=radius
c
        do m=1,ncomp
          nrel2(m,1)=nrel(m)
        end do
c
      else
c
        numradius=2
        hlower2(1)=0.
        hupper2(1)=hlower
        radius2(1)=stemradius
        hlower2(2)=hlower
        hupper2(2)=hupper
        radius2(2)=radius
c
        pi=3.141592654
        volume1=pi * stemradius**2 * hlower
        volume2=pi * radius**2 * (hupper - hlower)
c
        do m=1,ncomp
          nrel2(m,1)= nint(nrel(m)*volume1/(volume1+volume2))
          nrel2(m,2)= nrel(m)-nrel2(m,1)
        end do
c
      end  if
c
c---------------------------------------------------
c
c++++++++++++++++++++++++++++++++++++++++++
      do nrad=1,numradius
c++++++++++++++++++++++++++++++++++++++++++
c
      radius=radius2(nrad)
      hlower=hlower2(nrad)
      hupper=hupper2(nrad)
c
      dxgrid= gparam(7)
      dygrid= gparam(8)
      dx= radius/(dxgrid/xm(i,j))
      dy= radius/(dygrid/ym(i,j))
c
      if (x-dx.lt.1.01 .or. x+dx.gt.nx-0.01 .or.
     +    y-dy.lt.1.01 .or. y+dy.gt.ny-0.01) then
       write(6,*) 'RELEASE ERROR: Bad position'
       ierror=1
       return
      end if
c
      twodx= 2.*dx
      twody= 2.*dy
c
c..for random number generation
c..(better if moved into particle loops ?)
      iseed=irand()
      call srand(iseed)
c
c----------------------------------------------
      if (dx.gt.0.01 .and. dy.gt.0.01) then
c----------------------------------------------
c
c..want an uniform spread in x,y,z within a real cylinder,
c..i.e. usually an ellipse in the model grid
c..(ellipse code from DIANA diFieldEdit.cc FieldEdit::editWeight,
c.. and is probably more general than needed here (axis in x/y direction))
c
      if (dx.ge.dy) then
       e= sqrt(1.-(dy*dy)/(dx*dx))
      else
       e= sqrt(1.-(dx*dx)/(dy*dy))
      end if
      a= sqrt(dx*dx+dy*dy)
      b= a*sqrt(1.-e*e)
      c= 2.*sqrt(a*a-b*b)
      ecos= dx/a
      esin= dy/a
      en=   0.5*c/a
      en2=  en*en
      b2=   b*b
c
      do m=1,ncomp
c
       nprel= npart+nrel2(m,nrad)
c
        do while (npart.lt.nprel)
c
c..the rand function returns random real numbers between 0.0 and 1.0
          xrand=rand()
          yrand=rand()
          zrand=rand()
c
         dx= twodx*(xrand-0.5)
         dy= twody*(yrand-0.5)
c
          s= sqrt(dx*dx + dy*dy)
         gcos= dx/s
         gsin= dy/s
         rcos= gcos*ecos + gsin*esin
         smax= sqrt(b2/(1.-en2*rcos*rcos))
c
         if(s.le.smax) then
c
           h= hlower + (hupper-hlower)*zrand
           k= 2
           do while (k.lt.nk .and. h.gt.hlevel(k))
             k=k+1
           end do
           vlev= vlevel(k-1) + (vlevel(k)-vlevel(k-1))
     +                       *(h-hlevel(k-1))/(hlevel(k)-hlevel(k-1))
c
           npart=npart+1
           pdata(1,npart)= x+dx
           pdata(2,npart)= y+dy
           pdata(3,npart)= vlev
           pdata(9,npart)= pbq(m)
           icomp(npart)=   idefcomp(m)
c..an unique particle identifier (for testing...)
           nparnum=nparnum+1
           iparnum(npart)=nparnum
c
         end if
c
        end do
c
      end do
c
c----------------------------------------------
      else
c----------------------------------------------
c
c..distribution in a column (radius=0)
c
      do m=1,ncomp
c
       nprel= nrel2(m,nrad)
c
        do n=1,nprel
c
c..the rand function returns random real numbers between 0.0 and 1.0
          zrand=rand()
c
         h= hlower + (hupper-hlower)*zrand
         k= 2
         do while (k.lt.nk .and. h.gt.hlevel(k))
           k=k+1
         end do
         vlev= vlevel(k-1) + (vlevel(k)-vlevel(k-1))
     +                       *(h-hlevel(k-1))/(hlevel(k)-hlevel(k-1))
c
         npart=npart+1
         pdata(1,npart)= x
         pdata(2,npart)= y
         pdata(3,npart)= vlev
         pdata(9,npart)= pbq(m)
         icomp(npart)=   idefcomp(m)
c..an unique particle identifier (for testing...)
         nparnum=nparnum+1
         iparnum(npart)=nparnum
c
        end do
c
      end do
c
c----------------------------------------------
      end if
c----------------------------------------------
c
c++++++++++++++++++++++++++++++++++++++++++
c.....end do nrad=1,numradius
      end do
c++++++++++++++++++++++++++++++++++++++++++
c
      do n=1,ncomp
       m=idefcomp(n)
       totalbq(m)= totalbq(m) + pbq(n)*nrel(n)
       numtotal(m)=  numtotal(m) + nrel(n)
      end do
c
c################################################################
ccc   if(mod(istep,nsteph*3).eq.0) then
c      if(mod(istep,nsteph).eq.0) then
        do n=1,ncomp
         m=idefcomp(n)
ccc	  write(6,*) 'comp,m,totalbq,numtotal: ',
ccc  +			n,m,totalbq(m),numtotal(m)
         write(9,*) 'comp,m,totalbq,numtotal: ',
     +			n,m,totalbq(m),numtotal(m)
        end do
ccc	write(6,*) 'nparnum: ',nparnum
       write(9,*) 'nparnum: ',nparnum
c      end if
c################################################################
c
      nplume=nplume+1
      iplume(1,nplume)=npar1
      iplume(2,nplume)=npart
c
      write(9,*) '*RELEASE*  plumes,particles: ',nplume,npart
c#######################################################################
c     write(6,*) '*RELEASE*  plumes,particles: ',nplume,npart
c#######################################################################
c
      ierror= 0
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('RELEASE',1,ZHOOK_HANDLE)
#endif
      return
      end
