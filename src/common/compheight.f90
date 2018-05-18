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
      subroutine compheight
        USE snapgrdML
        USE snapfldML
        USE snaptabML
        USE snapdimML, only: nx,ny,nk
c
c  Purpose:  Compute height of model levels and thickness of model layers
c
c  Notes:
c    - sigma levels (norlam) or eta levels (hirlam,...)
c      defined by alevel and blevel
c    - lower model level is level 2
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
      
c
      integer i,j,k,itab
      real    ginv,rtab,p,pih,pif,h1,h2
      real    pihl(nx,ny),hlev(nx,ny)
c
#if defined(DRHOOK)
      ! Before the very first statement
      IF (LHOOK) CALL DR_HOOK('COMPHEIGHT',0,ZHOOK_HANDLE)
#endif
c
c##################################################################
c     real dz1min,dz1max,dz2min,dz2max,hhhmin,hhhmax,dzz
c##################################################################
c
c..compute height of model levels (in the model grid)
c
c##################################################################
c       write(9,*) 'nk: ',nk
c       write(9,*) 'k,alevel,blevel,vlevel,ahalf,bhalf,vhalf:'
c       do k=nk,1,-1
c         write(9,fmt='(1x,i2,'':'',2(f10.2,2f7.4))')
c    +		  k,alevel(k),blevel(k),vlevel(k),
c    +		    ahalf(k),bhalf(k),vhalf(k)
c       end do
c##################################################################
c
      ginv=1./g
c
      do j=1,ny
       do i=1,nx
         hlev(i,j)=0.
          rtab=ps2(i,j)*pmult
          itab=rtab
          pihl(i,j)=pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
         hlayer2(i,j,nk)=9999.
         hlevel2(i,j,1)=0.
       end do
      end do
c
      do k=2,nk
c##################################################################
c	dz1min=+1.e+35
c	dz1max=-1.e+35
c	dz2min=+1.e+35
c	dz2max=-1.e+35
c	hhhmin=+1.e+35
c	hhhmax=-1.e+35
c##################################################################
       do j=1,ny
         do i=1,nx
            p=ahalf(k)+bhalf(k)*ps2(i,j)
            rtab=p*pmult
            itab=rtab
            pih=pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
c
            p=alevel(k)+blevel(k)*ps2(i,j)
            rtab=p*pmult
            itab=rtab
            pif=pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
c
           h1=hlev(i,j)
            h2=h1 + t2(i,j,k)*(pihl(i,j)-pih)*ginv
c
           hlayer2(i,j,k-1)= h2-h1
            hlevel2(i,j,k)= h1 + (h2-h1)*(pihl(i,j)-pif)
     +                                  /(pihl(i,j)-pih)
c
           hlev(i,j)=h2
           pihl(i,j)=pih
c##################################################################
c	    dzz=h2-h1
c	    dz1min=min(dz1min,dzz)
c	    dz1max=max(dz1max,dzz)
c	    dzz=hlevel2(i,j,k)-hlevel2(i,j,k-1)
c	    dz2min=min(dz2min,dzz)
c	    dz2max=max(dz2max,dzz)
c	    hhhmin=min(hhhmin,hlevel2(i,j,k))
c	    hhhmax=max(hhhmax,hlevel2(i,j,k))
c##################################################################
         end do
       end do
c##################################################################
c	write(9,*) 'k,hhhmin,hhhmax: ',k,':',hhhmin,hhhmax
c	write(9,*) 'dz1min,dz1max,dz2min,dz2max:',
c    +              dz1min,dz1max,dz2min,dz2max
c##################################################################
      end do
c
c##################################################################
      call ftest('hlayer',nk,1,nx,ny,nk,hlayer2,1)
      call ftest('hlevel',nk,1,nx,ny,nk,hlevel2,1)
c##################################################################
c
c##################################################################
c     i=nx/2
c     j=ny/2
c     do k=nk,1,-1
c	write(9,fmt='(''    k,hlayer,hlevel:'',i3,2f8.0)')
c    +			    k,hlayer2(i,j,k),hlevel2(i,j,k)
c     end do
c##################################################################
c
#if defined(DRHOOK)
c     before the return statement
      IF (LHOOK) CALL DR_HOOK('COMPHEIGHT',1,ZHOOK_HANDLE)
#endif
      return
      end
