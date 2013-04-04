      subroutine bldp
c
c  Purpose:  Compute boundary layer top and height
c
c  Method:   Computing Richardson no. and critical Richardson no.
c            as in the DNMI NWP LAM, NORLAM.
c            Critical Richardson number then modified ('ricfac')
c
c  Notes:
c    - sigma levels (norlam) or eta levels (hirlam,...)
c      defined by alevel and blevel
c    - horizontal wind components in unit m/s
c    - all wind components in non-staggered horizontal grid
c      and in the same levels
c    - lower model level is level 2
c
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
      include 'snaptab.inc'
c
      real    pih(nk),pif(nk),zh(nk),zf(nk),thh(nk)
c
      integer kbltop,kblbot,nkk,i,j,k,itab,ktop
      real    ginv,ricfac,psurf,pbltop,pblbot,p,p1,p2,vbltop,vblbot
      real    vbl,uhelp,vhelp,rtab,dz
      real    dv2min,dv2,dth,ri,ric,riu,ricu,dri,driu,hbl
c
c-test----------------------------------------------
      real    rri(4,nk)
      integer nrri(nk)
c-test----------------------------------------------
c######################################################################
c     real riri(nk),ricric(nk)
c######################################################################
c
      ginv=1./g
c
      ricfac=1.8
      write(9,*) '*BLDP*  ricfac = ',ricfac
c
c..lower model level is k=2
c
c..set valid range for boundary layer top
c
      psurf=1000.
      pbltop=600.
      pblbot=975.
      p=ahalf(nk-1)+bhalf(nk-1)*psurf
      if(pbltop.lt.p) pbltop=p
      p=ahalf(2)+bhalf(2)*psurf
      if(pblbot.gt.p) pblbot=p
      kbltop=2
      kblbot=2
      nkk=min(nk-2,nk-kadd)
      do k=2,nkk
        p=ahalf(k)+bhalf(k)*psurf
        if(p.gt.pbltop) kbltop=k+1
        if(p.ge.pblbot) kblbot=k
      end do
c..vlevel used for particles
      p2=ahalf(kbltop)  +bhalf(kbltop)  *psurf
      p1=ahalf(kbltop-1)+bhalf(kbltop-1)*psurf
      vbltop=vhalf(kbltop-1)+(vhalf(kbltop)-vhalf(kbltop-1))
     +                      *(pbltop-p1)/(p2-p1)
      p2=ahalf(kblbot+1)+bhalf(kblbot+1)*psurf
      p1=ahalf(kblbot)  +bhalf(kblbot)  *psurf
      vblbot=vhalf(kblbot)+(vhalf(kblbot+1)-vhalf(kblbot))
     +                    *(pblbot-p1)/(p2-p1)
c
c-test----------------------------------------------
      do k=1,nk
        nrri(k)=0
        rri(1,k)=+1.e+10
        rri(2,k)=-1.e+10
        rri(3,k)=+1.e+10
        rri(4,k)=-1.e+10
      end do
c-test----------------------------------------------
c
c######################################################################
c     write(9,*) 'k,ahalf,bhalf,alevel,blevel,phalf,pfull,phalf-pfull:'
c     do k=nk,1,-1
c	phalf=ahalf(k)+bhalf(k)*1000.
c	pfull=alevel(k)+blevel(k)*1000.
c	write(9,fmt='(1x,i2,7(1x,f9.3))')
c    +		k,ahalf(k),bhalf(k),alevel(k),blevel(k),phalf,pfull,
c    +	          phalf-pfull
c	write(9,*) '      vhalf= ',vhalf(k)
c     end do
c     write(9,*) 'kbltop,vbltop: ',kbltop,vbltop
c     write(9,*) 'kblbot,vblbot: ',kblbot,vblbot
c######################################################################
c
      do j=1,ny
c
        do i=1,nx
c######################################################################
c	  do k=1,nk
c	    riri(k)=0.
c	    ricric(k)=0.
c	    pih(k)=0.
c	    pif(k)=0.
c	    zh(k)=0.
c	    zf(k)=0.
c	    thh(k)=0.
c	  end do
c	  kstop=0
c######################################################################
c
c..set u=v=0 at surface (not using 10m wind)
          uhelp=u2(i,j,1)
          vhelp=v2(i,j,1)
          u2(i,j,1)=0.
          v2(i,j,1)=0.
c
c..pih: exner function in half (sigma1) levels
c..pif: exner function in full (sigma2) levels (u,v,th levels)
c..zh:  height of half levels
c..zf:  height of full levels    (linear interp. in exner func.)
c..thh: pot.temp. in half levels (linear interp. in exner func.)
c
          do k=1,2
            p=ahalf(k)+bhalf(k)*ps2(i,j)
            rtab=p*pmult
            itab=rtab
            pih(k)=pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
            p=alevel(k)+blevel(k)*ps2(i,j)
            rtab=p*pmult
            itab=rtab
            pif(k)=pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
          end do
c
          k=2
          zh(k-1)=0.
          zf(k-1)=0.
	  zh(k)=zh(k-1)+t2(i,j,k)*(pih(k-1)-pih(k))*ginv
          zf(k)=  zh(k-1)
     -          +(zh(k)-zh(k-1))*(pih(k-1)-pif(k))/(pih(k-1)-pih(k))
c
c..search for top of boundary layer
c
	  ktop=0
	  k=1
c
	  do while ((ktop.eq.0 .or. k.lt.kblbot) .and. k.lt.kbltop)
c
	    k=k+1
c
            p=ahalf(k+1)+bhalf(k+1)*ps2(i,j)
            rtab=p*pmult
            itab=rtab
            pih(k+1)=  pitab(itab)
     -               +(pitab(itab+1)-pitab(itab))*(rtab-itab)
c
            p=alevel(k+1)+blevel(k+1)*ps2(i,j)
            rtab=p*pmult
            itab=rtab
            pif(k+1)=  pitab(itab)
     -               +(pitab(itab+1)-pitab(itab))*(rtab-itab)
c
            thh(k)=  t2(i,j,k)
     -             +(t2(i,j,k+1)-t2(i,j,k))*(pif(k)-pih(k))
     -                                     /(pif(k)-pif(k+1))
c
            zh(k+1)=zh(k)+t2(i,j,k+1)*(pih(k)-pih(k+1))*ginv
c
            zf(k+1)=  zh(k)
     -              +(zh(k+1)-zh(k))*(pih(k)-pif(k+1))
     -                              /(pih(k)-pih(k+1))
c
	    if(ktop.eq.0) then
c
              dz=zf(k+1)-zf(k)
              dv2min=1.e-5*dz*dz
              dv2= (u2(i,j,k+1)-u2(i,j,k))*(u2(i,j,k+1)-u2(i,j,k))
     -            +(v2(i,j,k+1)-v2(i,j,k))*(v2(i,j,k+1)-v2(i,j,k))
              if(dv2.lt.dv2min) dv2=dv2min
              dth=t2(i,j,k+1)-t2(i,j,k)
c..Richardson no.
c............ ri=g*dth*dz/(thh(k)*dv2) ................. in NORLAM
              ri=cp*g*dth*dz/(thh(k)*pih(k)*dv2)
c..Critical Richardson no. (as in NORLAM)
              ric=0.115*(dz*100.)**0.175
c..and then modification
              ric=ric*ricfac
c-test----------------------------------------------
              nrri(k)=nrri(k)+1
              rri(1,k)=min(rri(1,k),ri)
              rri(2,k)=max(rri(2,k),ri)
              rri(3,k)=min(rri(3,k),ric)
              rri(4,k)=max(rri(4,k),ric)
c-test----------------------------------------------
c######################################################################
c	      riri(k)=ri
c	      ricric(k)=ric
c######################################################################
c
              if(ri.gt.ric) then
		ktop=k
	      else
                riu=ri
                ricu=ric
	      end if
c
	    end if
c######################################################################
c	    kstop=k
c######################################################################
c
          end do
c
          k=ktop
	  if(k.eq.0) k=kbltop
c
c..sigma/eta at top of boundary layer
          if(vhalf(k).ge.vblbot) then
            vbl=vblbot
            k=kblbot
          elseif(k.eq.kbltop .and. ri.le.ric) then
            vbl=vbltop
            k=kbltop-1
          else
c..interpolation, very simple method: ric-ri linear in sigma/eta
            k=k-1
            dri=ric-ri
            driu=ricu-riu
            vbl=vhalf(k)+(vhalf(k+1)-vhalf(k))*driu/(driu-dri)
            if(vbl.lt.vbltop) vbl=vbltop
            if(vbl.gt.vblbot) vbl=vblbot
          end if
c
c..height of boundary layer (linear interp. in 'vlevel')
c######################################################################
c	  write(9,*) 'kblbot,ktop,kstop= ',kblbot,ktop,kstop
c	  write(9,*) k,zh(k),zh(k+1),vbl,vhalf(k),vhalf(k+1)
c######################################################################
          hbl=  zh(k)
     -        +(zh(k+1)-zh(k))*(vhalf(k)-vbl)/(vhalf(k)-vhalf(k+1))
c
           bl2(i,j)=vbl
          hbl2(i,j)=hbl
c
c..reset wind
          u2(i,j,1)=uhelp
          v2(i,j,1)=vhelp
c######################################################################
c	  if(mod(i,20).eq.0 .and. mod(j,20).eq.0) then
c	    write(9,*) '---------- I,J: ',i,j
c	    write(9,*) 'vbl,hbl: ',vbl,hbl
c	    write(9,*) 'k,pih,pif,zh,zf,thh,ri,ric:'
c	    do k=kstop,1,-1
c	      write(9,fmt='(1x,i2,7(1x,f10.2))')
c    +		k,pih(k),pif(k),zh(k),zf(k),thh(k),riri(k),ricric(k)
c	    end do
c	  end if
c######################################################################
c
        end do
c
      end do
c
c-test----------------------------------------------
      write(9,*) '*BLDP*'
      call ftest('bl ',1,1,nx,ny,1, bl2,0)
      call ftest('hbl',1,1,nx,ny,1,hbl2,0)
c-test----------------------------------------------
c
c-test----------------------------------------------
      write(9,*) ' max. vlevel,k,vhalf(k): ',
     -                  vbltop,kbltop,vhalf(kbltop)
      write(9,*) ' min. vlevel,k,vhalf(k): ',
     -                  vblbot,kblbot,vhalf(kblbot)
      write(9,*) '   k   no.    ri_min    ri_max   ric_min   ric_max'
      do k=nk,1,-1
        if(nrri(k).gt.0)
     -    write(9,fmt='(3x,i2,i6,4(1x,f9.4))')
     -                     k,nrri(k),(rri(i,k),i=1,4)
      end do
c-test----------------------------------------------
c
      return
      end
