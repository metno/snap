      subroutine ensemble(icall,itime,tf1,tf2,tnow,istep,nstep,nsteph)
      use particleML
c
c  Purpose: Interpolate particle positions to ENSEMBLE grid,
c           and store data in this grid (and model levels),
c	    interpolation of concentrations to fixed heights.
c
c	icall=0 : initialize (after first fields read),
c            =1 : new fields read
c            =2 : before drydep
c            =3 : after drydep, before wetdep
c            =4 : after wetdep
c            =5 : after forwrd,rwalk
c            =6 : output etc...
c            =7 : final output of timeseries for each gridpoint
c
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
cxx   include 'snapfil.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snappar.inc'
      include 'snaptab.inc'
      include 'snapeps.inc'
c
c..input
      integer icall,istep,nstep,nsteph
      integer itime(5)
      real    tf1,tf2,tnow
c
c---------------------------------------------------------
ccccc parameter (nxep=151,nyep=91)
c
      integer nheights
      parameter (nheights=5)
c
      integer nepout
      parameter (nepout=nheights+4)
c
      real    heights(nheights)
c
      integer igridep, AllocStat
      real     gparep(6)
c
c nxep,nyep,mcomp arrays, (nxep,nyep,nk,mcomp in case of concep)
      real(kind=8), allocatable, save :: drydepep(:,:,:),
     + wetdepep(:,:,:), concsurfep(:,:,:), conc(:,:,:), concep(:,:,:,:)
c
c nxep,nyep,mk arrays
      real(kind=4), allocatable, save :: hlayer1ep(:,:,:),
     + hlayer2ep(:,:,:), hlevel1ep(:,:,:),hlevel2ep(:,:,:)
c nxep, nyep arrays
      real(kind=4), allocatable, save :: precipep(:,:), gareaep(:,:),
     + xmodel(:,:), ymodel(:,:), prectmp(:,:)
c
c mpart arrays
      real(kind=4), allocatable, save :: pbq1(:),pbq2(:),
     + xep(:),yep(:)
c
c nxep, nyep, nepout
      real(kind=4), allocatable, save :: epfield(:,:,:)
c
c nxep, nyep
      integer, allocatable, save :: inside(:,:)
c 2, nxep*nyep
      integer, allocatable, save :: ijlist(:,:)
c
c
c..using allocated memory for the final output (timeseries at each pos.)
c
      integer lepdata
      integer mbuffer
c buffer nepout, mbuffer, should be used after dallocation of other arrays
      real, allocatable :: buffer(:,:)
c      equivalence (drydepep(1,1,1),buffer(1,1))
c
c
      integer matimev
c 5,matimev
      integer, allocatable :: iatimev(:,:)
c
      real dxgr(5),dygr(5)
c
      integer itimerel(5)
      integer itimev(5)
c
      integer nsaveconc,ntoutput
c
      real    hh,dh,rt1,rt2,rk1,rk2
      real    hl(nk),cl(nk)
c
      integer i,j,k,l,m,mm,n,ierror,ninside,ivlvl,ihour,kk,npos,nc
      integer imc,iex,n1,n2,it,lrunit,iu,no,mtoutput
c
      real    undef,dxgridep,dygridep,x1,y1,x2,y2,hrstep,scale
      real    cscale,dscale,cavg,hmax,glon,glat,ttmax,ginv
c
      integer lenstr
c
      character*128 filename
c
      data heights/0.,200.,500.,1300.,3000./
      data igridep/2/
      data  gparep/-15.,30.,0.5,0.5,0.,0./
c
      data dxgr/-0.5,-0.5,+0.5,+0.5,0.0/
      data dygr/-0.5,+0.5,-0.5,+0.5,0.0/
c
      data itimerel/0,0,0,0,0/
      data nsaveconc/0/
      data ntoutput/0/
c
c---------------------------------------------------------
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('ENSEMBLE',0,ZHOOK_HANDLE)
#endif
c
      if (nxep.lt.2 .or. nyep.lt.2) return
c
c##################################################################
      write(6,*) '========== ENSEMBLE  icall= ',icall
      write(9,*) '========== ENSEMBLE  icall= ',icall
c##################################################################
c
      undef=+1.e+35
c
      if(icall.eq.0) then
c initialize
      ALLOCATE ( drydepep(nxep,nyep,mcomp), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( wetdepep(nxep,nyep,mcomp), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( concsurfep(nxep,nyep,mcomp), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( conc(nxep,nyep,mcomp), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( concep(nxep,nyep,nk,mcomp), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( hlayer1ep(nxep,nyep,nk), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hlayer2ep(nxep,nyep,nk), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hlevel1ep(nxep,nyep,nk), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( hlevel2ep(nxep,nyep,nk), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( precipep(nxep,nyep), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( gareaep(nxep,nyep), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( xmodel(nxep,nyep), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( ymodel(nxep,nyep), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( prectmp(nxep,nyep), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( pbq1(mpart), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( pbq2(mpart), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( xep(mpart), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      ALLOCATE ( yep(mpart), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( epfield(nxep,nyep,nepout), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( inside(nxep,nyep), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

      ALLOCATE ( ijlist(2, nxep*nyep), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
c
c


c
c..time of release
       do i=1,5
         itimerel(i)=itime(i)
       end do
c
c..compute map ratio
        call mapfield(1,0,igridep,gparep,
     +		      nxep,nyep,xmodel,ymodel,0.,
     +                dxgridep,dygridep,ierror)
        if(ierror.ne.0) then
          write(9,*) 'ensemble MAPFIELD ERROR. ierror= ',ierror
          write(6,*) 'ensemble MAPFIELD ERROR. ierror= ',ierror
          stop 255
        end if
c..size of each grid square (m**2)
       dxgridep=abs(dxgridep)
       dygridep=abs(dygridep)
        do j=1,nyep
          do i=1,nxep
            gareaep(i,j)= (dxgridep/xmodel(i,j))
     +			 *(dygridep/ymodel(i,j))
          end do
        end do
c
c..no output unless all corners of gridsquare are inside model grid
       do j=1,nyep
         do i=1,nxep
           inside(i,j)=1
         end do
       end do
       do n=1,5
         do j=1,nyep
           do i=1,nxep
             xmodel(i,j)=float(i)+dxgr(n)
             ymodel(i,j)=float(j)+dygr(n)
           end do
         end do
         call xyconvert(nxep*nyep,xmodel,ymodel,
     +		         igridep,gparep,igtype,gparam,ierror)
         if(ierror.ne.0) then
           write(6,*) 'ensemble XYCONVERT ERROR'
           write(9,*) 'ensemble XYCONVERT ERROR'
            stop 17
         end if
         x1=1.
         x2=nx
         y1=1.
         y2=ny
c##################################################################
         ninside=0
c##################################################################
         do j=1,nyep
           do i=1,nxep
             if(xmodel(i,j).lt.x1 .or. xmodel(i,j).gt.x2 .or.
     +           ymodel(i,j).lt.y1 .or. ymodel(i,j).gt.y2)
     +						     inside(i,j)=0
c##################################################################
             ninside=ninside+inside(i,j)
c##################################################################
           end do
         end do
       end do
c##################################################################
       write(6,*) 'nxep*nyep,ninside,noutside: ',
     +		    nxep*nyep,ninside,nxep*nyep-ninside
       write(9,*) 'nxep*nyep,ninside,noutside: ',
     +		    nxep*nyep,ninside,nxep*nyep-ninside
        call ftest('xmodel',1,1,nxep,nyep,1,xmodel,0)
        call ftest('ymodel',1,1,nxep,nyep,1,ymodel,0)
        call ftest('gareap',1,1,nxep,nyep,1,gareaep,0)
c##################################################################
c
c..output position sequence
       no=0
       do j=nyep,1,-1
         do i=1,nxep
           no=no+1
           ijlist(1,no)=i
           ijlist(2,no)=j
         end do
       end do
c
c..initialize
       do m=1,ncomp
         do j=1,nyep
           do i=1,nxep
             drydepep(i,j,m)=0.0d0
             wetdepep(i,j,m)=0.0d0
             concsurfep(i,j,m)=0.0d0
           end do
         end do
       end do
       do j=1,nyep
         do i=1,nxep
           precipep(i,j)=0.0
         end do
       end do
c
c..get machine dependant unit of recordlength in bytes
       call rlunit(lrunit)
c
       do m=1,ncomp
c
         mm= idefcomp(m)
c
         filename= 'ensemble.tmp.'//compnamemc(mm)
         open(60+m,file=filename,
     +		    access='direct',form='unformatted',
     +		    recl=(5+nxep*nyep*nepout)*4/lrunit,
     +		    status='unknown')
c
         filename= 'ensemble.test.'//compnamemc(mm)
         open(70+m,file=filename,
     +		    access='sequential',form='formatted',
     +		    status='unknown')
c
       end do
c
      end if
c
      if (icall.eq.1) then
c
       call copyfield(hlayer2ep,hlayer1ep,nxep,nyep,nk)
       call copyfield(hlevel2ep,hlevel1ep,nxep,nyep,nk)
c
      end if
c
      if (icall.eq.0 .or. icall.eq.1) then
c
c..height of model levels and thickness of model layers
c
       do k=1,nk
         call epinterp(nx,ny,hlevel2(1,1,k),
     +		        nxep*nyep,xmodel,ymodel,
     +			hlevel2ep(1,1,k),inside)
       end do
c
       do k=1,nk
         call epinterp(nx,ny,hlayer2(1,1,k),
     +		        nxep*nyep,xmodel,ymodel,
     +			hlayer2ep(1,1,k),inside)
       end do
c
c##################################################################
        call ftest('hlayer',nk,1,nxep,nyep,nk,hlayer2ep,1)
        call ftest('hlevel',nk,1,nxep,nyep,nk,hlevel2ep,1)
c##################################################################
c
c##################################################################
        i=nxep/2
       j=nyep/2
       do k=nk,1,-1
         write(9,fmt='(''    k,hlayer,hlevel:'',i3,2f8.0)')
     +			      k,hlayer2ep(i,j,k),hlevel2ep(i,j,k)
       end do
c##################################################################
      end if
c
      if(icall.eq.2) then
c
c..store Bq before drydep
       do n=1,npart
         pbq1(n)=pdata(n)%rad
       end do
c
      end if
c
      if(icall.eq.3) then
c
c..store Bq after drydep, before wetdep
       do n=1,npart
         pbq2(n)=pdata(n)%rad
       end do
c
      end if
c
      if(icall.eq.4 .or. icall.eq.5) then
c
c..convert positions to ensemble grid
       do n=1,npart
         xep(n)= pdata(n)%x
         yep(n)= pdata(n)%y
       end do
       call xyconvert(npart,xep,yep,igtype,gparam,
     +		       igridep,gparep,ierror)
       if(ierror.ne.0) then
         write(6,*) 'ensemble XYCONVERT ERROR'
         write(9,*) 'ensemble XYCONVERT ERROR'
          stop 17
       end if
c
      end if

      if(icall.eq.4) then
c
       do n=1,npart
         i=nint(xep(n))
         j=nint(yep(n))
         m=iruncomp(icomp(n))
         if(i.gt.0 .and. i.le.nxep .and.
     +       j.gt.0 .and. j.le.nyep) then
           drydepep(i,j,m)= drydepep(i,j,m) + dble(pbq1(n)-pbq2(n))
           wetdepep(i,j,m)= wetdepep(i,j,m) +
     +                      dble(pbq2(n)-pdata(n)%rad)
         end if
       end do
c
      end if
c
      if(icall.eq.5) then
c
c..for linear interpolation in time
        rt1=(tf2-tnow)/(tf2-tf1)
        rt2=(tnow-tf1)/(tf2-tf1)
        hrstep=1./float(nsteph)
c
       do m=1,ncomp
         do j=1,nyep
           do i=1,nxep
             conc(i,j,m)=0.0d0
           end do
         end do
       end do
c
       do n=1,npart
          ivlvl=pdata(n)%z*10000.
          k=ivlayer(ivlvl)
          if(k.eq.1) then
           i=nint(xep(n))
           j=nint(yep(n))
           if(i.gt.0 .and. i.le.nxep .and.
     +         j.gt.0 .and. j.le.nyep) then
             m=iruncomp(icomp(n))
              conc(i,j,m)= conc(i,j,m)+dble(pdata(n)%rad)
           end if
         end if
       end do
c
       do m=1,ncomp
         do j=1,nyep
           do i=1,nxep
             if(conc(i,j,m).gt.0.0d0) then
           dh= rt1*hlayer1ep(i,j,1)+rt2*hlayer2ep(i,j,1)
               concsurfep(i,j,m)= concsurfep(i,j,m)
     +				+conc(i,j,m)*hrstep/(dh*gareaep(i,j))
             end if
           end do
         end do
       end do
c
       if(nsaveconc.ge.0) then
         if(nsaveconc.eq.0) then
           do m=1,ncomp
             do k=1,nk
               do j=1,nyep
                 do i=1,nxep
                   concep(i,j,k,m)=0.0d0
                 end do
               end do
             end do
           end do
         end if
         nsaveconc=nsaveconc+1
c##################################################################
c	  minv=+999999
c	  maxv=-999999
c	  mink=+999999
c	  maxk=-999999
c##################################################################
         do n=1,npart
           i=nint(xep(n))
           j=nint(yep(n))
           if(i.gt.0 .and. i.le.nxep .and.
     +         j.gt.0 .and. j.le.nyep) then
              ivlvl=pdata(n)%z*10000.
              k=ivlayer(ivlvl)
             m=iruncomp(icomp(n))
c..in each sigma/eta (input model) layer
              concep(i,j,k,m)=concep(i,j,k,m)+dble(pdata(n)%rad)
c##################################################################
c	      minv=min(minv,ivlvl)
c	      maxv=max(maxv,ivlvl)
c	      mink=min(mink,k)
c	      maxk=max(maxk,k)
c##################################################################
           end if
         end do
c##################################################################
c	  write(9,*) 'minv,maxv,mink,maxk: ',minv,maxv,mink,maxk
c##################################################################
       end if
c
c..precipitation (from intensity per hour)
       scale= 1./float(nsteph)
       call epinterp(nx,ny,precip(1,1,iprecip),
     +		      nxep*nyep,xmodel,ymodel,prectmp,inside)
       do j=1,nyep
         do i=1,nxep
           if(inside(i,j).eq.1)
     +	      precipep(i,j)= precipep(i,j)+scale*prectmp(i,j)
         end do
       end do
c
      end if
c
      if(icall.eq.6) then
c
        write(6,*) 'istep,nstep,nsteph,itime,tf1,tf2,tnow:'
        write(9,*) 'istep,nstep,nsteph,itime,tf1,tf2,tnow:'
       write(6,900) istep,nstep,nsteph,itime,tf1,tf2,tnow
       write(9,900) istep,nstep,nsteph,itime,tf1,tf2,tnow
  900   format(1x,3i4,4x,i4,4i3,4x,3f9.1)
c
       do i=1,5
         itimev(i)=itime(i)
       end do
       call vtime(itimev,ierror)
c..minute
        itimev(5)=0
c
       ihour=itimev(4)
c
       if(mod(ihour,ensembleStepHours).eq.0) then
c
c..output
c################################################################
         write(6,*) 'out nsaveconc= ',nsaveconc
c################################################################
c
c..fixed base scaling for concentrations (unit 10**-12 g/m3 = 1 picog/m3)
ccc       cscale=10.**12
c
c..fixed base scaling for depositions (unit 10**-9 g/m2 = 1 nanog/m2)
ccc       dscale=10.**9
c
         cscale= 1.0
         dscale= 1.0
c
c..for linear interpolation in time
          rt1=(tf2-tnow)/(tf2-tf1)
          rt2=(tnow-tf1)/(tf2-tf1)
c
         cavg=float(nsaveconc)
c
         hmax=heights(nheights)+1.
c
         mtoutput= ntoutput
c
         do m=1,ncomp
c
         ntoutput= mtoutput
c
         do j=1,nyep
           do i=1,nxep
              glon= gparep(1)+gparep(3)*(i-1)
              glat= gparep(2)+gparep(4)*(j-1)
             if(inside(i,j).eq.1) then
               hh=-1.
               kk=0
               do while (hh.lt.hmax .and. kk.lt.nk)
                 kk=kk+1
             dh=rt1*hlayer1ep(i,j,kk)+rt2*hlayer2ep(i,j,kk)
             hh=rt1*hlevel1ep(i,j,kk)+rt2*hlevel2ep(i,j,kk)
             hl(kk)=hh
             cl(kk)=concep(i,j,kk,m)/(dh*gareaep(i,j)*cavg)
               end do
           k=2
               do l=1,nheights
             do while (heights(l).gt.hl(k) .and. k.lt.kk)
               k=k+1
             end do
             rk1=(hl(k)-heights(l))  /(hl(k)-hl(k-1))
             rk2=(heights(l)-hl(k-1))/(hl(k)-hl(k-1))
             epfield(i,j,l)= (rk1*cl(k-1)+rk2*cl(k))*cscale
           end do
           epfield(i,j,nheights+1)=concsurfep(i,j,m)*cscale
           epfield(i,j,nheights+2)=drydepep(i,j,1)*dscale
     +						       /gareaep(i,j)
           epfield(i,j,nheights+3)=wetdepep(i,j,1)*dscale
     +						       /gareaep(i,j)
c..precipitation unit 0.1 mm !!!!!!!!!!!!!!!!!!!!
           epfield(i,j,nheights+4)=precipep(i,j) * 10.
             else
           do l=1,nepout
             epfield(i,j,l)=-9.
           end do
             end if
c
ccc	      write(97,1001) (itimev(l),l=1,5),glon,glat,
ccc  +			     (epfield(i,j,l),l=1,nepout)
 1001	      format(i4.4,4i2.2,f10.5,1x,f10.5,9(1x,1pe11.4))
c################################################################
             ttmax=0.
             do l=1,nepout-1
           ttmax=max(ttmax,epfield(i,j,l))
             end do
             if(ttmax.gt.0.)
     +	        write(70+m,1001) (itimev(l),l=1,5),glon,glat,
     +			         (epfield(i,j,l),l=1,nepout)
c################################################################
           end do
         end do
c
         ntoutput=ntoutput+1
c
c..write temporary binary file
c################################################################
c	  write(6,*) 'WRITE unit,rec: ',60+m,ntoutput
c################################################################
         write(60+m,rec=ntoutput) itimev,epfield
c
         end do
c
       end if
c
c..check if one hour before next output
       nsaveconc=-1
       if(mod(ihour+1,ensembleStepHours).eq.0) nsaveconc=0
c################################################################
       write(6,*) 'set nsaveconc= ',nsaveconc
c################################################################
c
      end if
c
      if(icall.eq.7) then
c
      DEALLOCATE ( drydepep )
      DEALLOCATE ( wetdepep )
      DEALLOCATE ( concsurfep )
      DEALLOCATE ( conc )
      DEALLOCATE ( concep )
      DEALLOCATE ( hlayer1ep )
      DEALLOCATE ( hlayer2ep )
      DEALLOCATE ( hlevel1ep )
      DEALLOCATE ( hlevel2ep )
      DEALLOCATE ( precipep )
      DEALLOCATE ( gareaep )
      DEALLOCATE ( xmodel )
      DEALLOCATE ( ymodel )
      DEALLOCATE ( prectmp )
      DEALLOCATE ( pbq1 )
      DEALLOCATE ( pbq2 )
      DEALLOCATE ( xep )
      DEALLOCATE ( yep )

      DEALLOCATE ( inside )

      DEALLOCATE ( ijlist )

c reuse just deallocated memory
      lepdata=  nxep*nyep*(3+nk+1)*mcomp*2
     +                   + nxep*nyep*(1+nk*4+3+1)
     +       + mpart*4
      mbuffer= lepdata/nepout
      ALLOCATE ( buffer(nepout,mbuffer), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"
      matimev= 100 + nxep*nyep/5
      ALLOCATE ( iatimev(5,matimev), STAT = AllocStat)
      IF (AllocStat /= 0) STOP "*** Not enough memory ***"

       if (ntoutput.lt.1) return
c
       npos= mbuffer/ntoutput
c
       write(6,*) 'Final ENSEMBLE PROJECT output'
       write(6,*) 'lepdata,nepout:   ',lepdata,nepout
       write(6,*) 'mbuffer,ntoutput: ',mbuffer,ntoutput
       write(6,*) 'matimev:          ',matimev
       write(6,*) 'npos:             ',npos
       write(9,*) 'Final ENSEMBLE PROJECT output'
       write(9,*) 'lepdata,nepout:   ',lepdata,nepout
       write(9,*) 'mbuffer,ntoutput: ',mbuffer,ntoutput
       write(9,*) 'matimev:          ',matimev
       write(9,*) 'npos:             ',npos
       write(9,*) 'file: ',ensemblefile(1:lenstr(ensemblefile,1))
c
       if (npos.lt.1) stop 17
       if (ntoutput.gt.matimev) stop 17
c
       do m=1,ncomp
c
       mm= idefcomp(m)
c
       k=index(ensemblefile,' ')
       if (k.lt.0) k=len(ensemblefile)+1
       k=k-1
       filename=ensemblefile(1:k)//'_'//compnamemc(mm)

       open(98,file=filename,
     +		access='sequential',form='formatted',
     +		status='unknown')
c
       write(98,fmt='(i2.2)') ensembleparticipant
       write(98,fmt='(a7)')   ensembleRandomKey
       write(98,fmt='(a5)')   compnamemc(mm)
       write(98,fmt='(i2.2)') ntoutput
c..release start
       do i=1,5
         itimev(i)=itimerel(i)
       end do
       itimev(5)=00
       call vtime(itimev,ierror)
       write(98,fmt='(i4.4,4i2.2)') itimev
c..last analysis
       do i=1,5
         itimev(i)=itime(i)
       end do
       itimev(5)=00
       call vtime(itimev,ierror)
       write(98,fmt='(i4.4,4i2.2)') itimev
c
       do n1=1,nxep*nyep,npos
c
         n2=min(n1+npos-1,nxep*nyep)
c
         write(6,*) 'read/write  n1,n2,nxep*nyep: ',n1,n2,nxep*nyep
c
         do it=1,ntoutput
c
c################################################################
c	    write(6,*) 'READ unit,rec: ',60+m,it
c################################################################
           read(60+m,rec=it) itimev,epfield
c
           do i=1,5
             iatimev(i,it)=itimev(i)
           end do
c
           do n=n1,n2
             i=ijlist(1,n)
             j=ijlist(2,n)
             no=(n-n1)*ntoutput+it
             do k=1,nepout
           buffer(k,no)= epfield(i,j,k)
             end do
           end do
c
         end do
c
         do n=n1,n2
           i=ijlist(1,n)
           j=ijlist(2,n)
            glon= gparep(1)+gparep(3)*(i-1)
            glat= gparep(2)+gparep(4)*(j-1)
           write(98,fmt='(f10.5,1x,f10.5)') glon,glat
           no=(n-n1)*ntoutput
            do it=1,ntoutput
             no=no+1
             write(98,fmt='(i4.4,4i2.2,9(1x,1pe11.4))')
     +		       (iatimev(i,it),i=1,5),
     +		       (buffer(k,no),k=1,nepout)
            end do
          end do
c
       end do
c
       close(98)
       close(60+m)
       close(70+m)
c
       end do
c
      DEALLOCATE ( epfield )

      end if
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('ENSEMBLE',1,ZHOOK_HANDLE)
#endif
      return
      end
