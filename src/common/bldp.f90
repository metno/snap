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

subroutine bldp
  USE snapdimML, only: nx,ny,nk, mprepro, mpretab
  USE snaptabML
  USE snapgrdML
  USE snapfldML
!  Purpose:  Compute boundary layer top and height

!  Method:   Computing Richardson no. and critical Richardson no.
!            as in the DNMI NWP LAM, NORLAM.
!            Critical Richardson number then modified ('ricfac')

!  Notes:
!    - sigma levels (norlam) or eta levels (hirlam,...)
!      defined by alevel and blevel
!    - horizontal wind components in unit m/s
!    - all wind components in non-staggered horizontal grid
!      and in the same levels
!    - lower model level is level 2


#if defined(DRHOOK)
  USE PARKIND1  ,ONLY : JPIM     ,JPRB
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
#endif
  implicit none
#if defined(DRHOOK)
  REAL(KIND=JPRB) :: ZHOOK_HANDLE ! Stack variable i.e. do not use SAVE
#endif

  real ::    pih(nk),pif(nk),zh(nk),zf(nk),thh(nk)

  integer :: kbltop,kblbot,nkk,i,j,k,itab,ktop
  real ::    ginv,ricfac,psurf,pbltop,pblbot,p,p1,p2,vbltop,vblbot
  real ::    vbl,uhelp,vhelp,rtab,dz
  real ::    dv2min,dv2,dth,ri,ric,riu,ricu,dri,driu,hbl

! test----------------------------------------------
  real ::    rri(4,nk)
  integer :: nrri(nk)

#if defined(DRHOOK)
! Before the very first statement
  IF (LHOOK) CALL DR_HOOK('BLDP',0,ZHOOK_HANDLE)
#endif

! test----------------------------------------------
!######################################################################
!     real riri(nk),ricric(nk)
!######################################################################

  ginv=1./g

  ricfac=1.8
  write(9,*) '*BLDP*  ricfac = ',ricfac

!..lower model level is k=2

!..set valid range for boundary layer top

  psurf=1000.
  pbltop=600.
  pblbot=975.
  p=ahalf(nk-1)+bhalf(nk-1)*psurf
  if(pbltop < p) pbltop=p
  p=ahalf(2)+bhalf(2)*psurf
  if(pblbot > p) pblbot=p
  kbltop=2
  kblbot=2
  nkk=min(nk-2,nk-kadd)
  do k=2,nkk
    p=ahalf(k)+bhalf(k)*psurf
    if(p > pbltop) kbltop=k+1
    if(p >= pblbot) kblbot=k
  end do
!..vlevel used for particles
  p2=ahalf(kbltop)  +bhalf(kbltop)  *psurf
  p1=ahalf(kbltop-1)+bhalf(kbltop-1)*psurf
  vbltop=vhalf(kbltop-1)+(vhalf(kbltop)-vhalf(kbltop-1)) &
  *(pbltop-p1)/(p2-p1)
  p2=ahalf(kblbot+1)+bhalf(kblbot+1)*psurf
  p1=ahalf(kblbot)  +bhalf(kblbot)  *psurf
  vblbot=vhalf(kblbot)+(vhalf(kblbot+1)-vhalf(kblbot)) &
  *(pblbot-p1)/(p2-p1)

! test----------------------------------------------
  do k=1,nk
    nrri(k)=0
    rri(1,k)=+1.e+10
    rri(2,k)=-1.e+10
    rri(3,k)=+1.e+10
    rri(4,k)=-1.e+10
  end do
! test----------------------------------------------

!######################################################################
!     write(9,*) 'k,ahalf,bhalf,alevel,blevel,phalf,pfull,phalf-pfull:'
!     do k=nk,1,-1
!	phalf=ahalf(k)+bhalf(k)*1000.
!	pfull=alevel(k)+blevel(k)*1000.
!	write(9,fmt='(1x,i2,7(1x,f9.3))')
!    +		k,ahalf(k),bhalf(k),alevel(k),blevel(k),phalf,pfull,
!    +	          phalf-pfull
!	write(9,*) '      vhalf= ',vhalf(k)
!     end do
!     write(9,*) 'kbltop,vbltop: ',kbltop,vbltop
!     write(9,*) 'kblbot,vblbot: ',kblbot,vblbot
!######################################################################

  do j=1,ny
  
    do i=1,nx
    !######################################################################
    !	  do k=1,nk
    !	    riri(k)=0.
    !	    ricric(k)=0.
    !	    pih(k)=0.
    !	    pif(k)=0.
    !	    zh(k)=0.
    !	    zf(k)=0.
    !	    thh(k)=0.
    !	  end do
    !	  kstop=0
    !######################################################################
    
    !..set u=v=0 at surface (not using 10m wind)
      uhelp=u2(i,j,1)
      vhelp=v2(i,j,1)
      u2(i,j,1)=0.
      v2(i,j,1)=0.
    
    !..pih: exner function in half (sigma1) levels
    !..pif: exner function in full (sigma2) levels (u,v,th levels)
    !..zh:  height of half levels
    !..zf:  height of full levels    (linear interp. in exner func.)
    !..thh: pot.temp. in half levels (linear interp. in exner func.)
    
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
    
      k=2
      zh(k-1)=0.
      zf(k-1)=0.
      zh(k)=zh(k-1)+t2(i,j,k)*(pih(k-1)-pih(k))*ginv
      zf(k)=  zh(k-1) &
      +(zh(k)-zh(k-1))*(pih(k-1)-pif(k))/(pih(k-1)-pih(k))
    
    !..search for top of boundary layer
    
      ktop=0
      k=1
    
      do while ((ktop == 0 .OR. k < kblbot) .AND. k < kbltop)
      
        k=k+1
      
        p=ahalf(k+1)+bhalf(k+1)*ps2(i,j)
        rtab=p*pmult
        itab=rtab
        pih(k+1)=  pitab(itab) &
        +(pitab(itab+1)-pitab(itab))*(rtab-itab)
      
        p=alevel(k+1)+blevel(k+1)*ps2(i,j)
        rtab=p*pmult
        itab=rtab
        pif(k+1)=  pitab(itab) &
        +(pitab(itab+1)-pitab(itab))*(rtab-itab)
      
        thh(k)=  t2(i,j,k) &
        +(t2(i,j,k+1)-t2(i,j,k))*(pif(k)-pih(k)) &
        /(pif(k)-pif(k+1))
      
        zh(k+1)=zh(k)+t2(i,j,k+1)*(pih(k)-pih(k+1))*ginv
      
        zf(k+1)=  zh(k) &
        +(zh(k+1)-zh(k))*(pih(k)-pif(k+1)) &
        /(pih(k)-pih(k+1))
      
        if(ktop == 0) then
        
          dz=zf(k+1)-zf(k)
          dv2min=1.e-5*dz*dz
          dv2= (u2(i,j,k+1)-u2(i,j,k))*(u2(i,j,k+1)-u2(i,j,k)) &
          +(v2(i,j,k+1)-v2(i,j,k))*(v2(i,j,k+1)-v2(i,j,k))
          if(dv2 < dv2min) dv2=dv2min
          dth=t2(i,j,k+1)-t2(i,j,k)
        !..Richardson no.
        !............ ri=g*dth*dz/(thh(k)*dv2) ................. in NORLAM
          ri=cp*g*dth*dz/(thh(k)*pih(k)*dv2)
        !..Critical Richardson no. (as in NORLAM)
          ric=0.115*(dz*100.)**0.175
        !..and then modification
          ric=ric*ricfac
        ! test----------------------------------------------
          nrri(k)=nrri(k)+1
          rri(1,k)=min(rri(1,k),ri)
          rri(2,k)=max(rri(2,k),ri)
          rri(3,k)=min(rri(3,k),ric)
          rri(4,k)=max(rri(4,k),ric)
        ! test----------------------------------------------
        !######################################################################
        !	      riri(k)=ri
        !	      ricric(k)=ric
        !######################################################################
        
          if(ri > ric) then
            ktop=k
          else
            riu=ri
            ricu=ric
          end if
        
        end if
      !######################################################################
      !	    kstop=k
      !######################################################################
      
      end do
    
      k=ktop
      if(k == 0) k=kbltop
    
    !..sigma/eta at top of boundary layer
      if(vhalf(k) >= vblbot) then
        vbl=vblbot
        k=kblbot
      elseif(k == kbltop .AND. ri <= ric) then
        vbl=vbltop
        k=kbltop-1
      else
      !..interpolation, very simple method: ric-ri linear in sigma/eta
        k=k-1
        dri=ric-ri
        driu=ricu-riu
        vbl=vhalf(k)+(vhalf(k+1)-vhalf(k))*driu/(driu-dri)
        if(vbl < vbltop) vbl=vbltop
        if(vbl > vblbot) vbl=vblbot
      end if
    
    !..height of boundary layer (linear interp. in 'vlevel')
    !######################################################################
    !	  write(9,*) 'kblbot,ktop,kstop= ',kblbot,ktop,kstop
    !	  write(9,*) k,zh(k),zh(k+1),vbl,vhalf(k),vhalf(k+1)
    !######################################################################
      hbl=  zh(k) &
      +(zh(k+1)-zh(k))*(vhalf(k)-vbl)/(vhalf(k)-vhalf(k+1))
    
      bl2(i,j)=vbl
      hbl2(i,j)=hbl
    
    !..reset wind
      u2(i,j,1)=uhelp
      v2(i,j,1)=vhelp
    !######################################################################
    !	  if(mod(i,20).eq.0 .and. mod(j,20).eq.0) then
    !	    write(9,*) '---------- I,J: ',i,j
    !	    write(9,*) 'vbl,hbl: ',vbl,hbl
    !	    write(9,*) 'k,pih,pif,zh,zf,thh,ri,ric:'
    !	    do k=kstop,1,-1
    !	      write(9,fmt='(1x,i2,7(1x,f10.2))')
    !    +		k,pih(k),pif(k),zh(k),zf(k),thh(k),riri(k),ricric(k)
    !	    end do
    !	  end if
    !######################################################################
    
    end do
  
  end do

! test----------------------------------------------
  write(9,*) '*BLDP*'
  call ftest('bl ',1,1,nx,ny,1, bl2,0)
  call ftest('hbl',1,1,nx,ny,1,hbl2,0)
! test----------------------------------------------

! test----------------------------------------------
  write(9,*) ' max. vlevel,k,vhalf(k): ', &
  vbltop,kbltop,vhalf(kbltop)
  write(9,*) ' min. vlevel,k,vhalf(k): ', &
  vblbot,kblbot,vhalf(kblbot)
  write(9,*) '   k   no.    ri_min    ri_max   ric_min   ric_max'
  do k=nk,1,-1
    if(nrri(k) > 0) &
    write(9,fmt='(3x,i2,i6,4(1x,f9.4))') &
    k,nrri(k),(rri(i,k),i=1,4)
  end do
! test----------------------------------------------

#if defined(DRHOOK)
!     before the return statement
  IF (LHOOK) CALL DR_HOOK('BLDP',1,ZHOOK_HANDLE)
#endif
  return
end subroutine bldp
