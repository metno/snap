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

module compheightML
  implicit none
  private

  public compheight

  contains

subroutine compheight
  USE snapgrdML
  USE snapfldML
  USE snaptabML
  USE snapdimML, only: nx,ny,nk
  USE ftestML, only: ftest

!  Purpose:  Compute height of model levels and thickness of model layers

!  Notes:
!    - sigma levels (norlam) or eta levels (hirlam,...)
!      defined by alevel and blevel
!    - lower model level is level 2


  implicit none

        

  integer :: i,j,k,itab
  real ::    ginv,rtab,p,pih,pif,h1,h2
  real ::    pihl(nx,ny),hlev(nx,ny)


!##################################################################
!     real dz1min,dz1max,dz2min,dz2max,hhhmin,hhhmax,dzz
!##################################################################

!..compute height of model levels (in the model grid)

!##################################################################
!       write(iulog,*) 'nk: ',nk
!       write(iulog,*) 'k,alevel,blevel,vlevel,ahalf,bhalf,vhalf:'
!       do k=nk,1,-1
!         write(iulog,fmt='(1x,i2,'':'',2(f10.2,2f7.4))')
!    +		  k,alevel(k),blevel(k),vlevel(k),
!    +		    ahalf(k),bhalf(k),vhalf(k)
!       end do
!##################################################################

  ginv=1./g

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

  do k=2,nk
  !##################################################################
  !	dz1min=+1.e+35
  !	dz1max=-1.e+35
  !	dz2min=+1.e+35
  !	dz2max=-1.e+35
  !	hhhmin=+1.e+35
  !	hhhmax=-1.e+35
  !##################################################################
    do j=1,ny
      do i=1,nx
        p=ahalf(k)+bhalf(k)*ps2(i,j)
        rtab=p*pmult
        itab=rtab
        pih=pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
      
        p=alevel(k)+blevel(k)*ps2(i,j)
        rtab=p*pmult
        itab=rtab
        pif=pitab(itab)+(pitab(itab+1)-pitab(itab))*(rtab-itab)
      
        h1=hlev(i,j)
        h2=h1 + t2(i,j,k)*(pihl(i,j)-pih)*ginv
      
        hlayer2(i,j,k-1)= h2-h1
        hlevel2(i,j,k)= h1 + (h2-h1)*(pihl(i,j)-pif) &
        /(pihl(i,j)-pih)
      
        hlev(i,j)=h2
        pihl(i,j)=pih
      !##################################################################
      !	    dzz=h2-h1
      !	    dz1min=min(dz1min,dzz)
      !	    dz1max=max(dz1max,dzz)
      !	    dzz=hlevel2(i,j,k)-hlevel2(i,j,k-1)
      !	    dz2min=min(dz2min,dzz)
      !	    dz2max=max(dz2max,dzz)
      !	    hhhmin=min(hhhmin,hlevel2(i,j,k))
      !	    hhhmax=max(hhhmax,hlevel2(i,j,k))
      !##################################################################
      end do
    end do
  !##################################################################
  !	write(iulog,*) 'k,hhhmin,hhhmax: ',k,':',hhhmin,hhhmax
  !	write(iulog,*) 'dz1min,dz1max,dz2min,dz2max:',
  !    +              dz1min,dz1max,dz2min,dz2max
  !##################################################################
  end do

!##################################################################
  call ftest('hlayer', hlayer2, contains_undef=.true., reverse_third_dim=.true.)
  call ftest('hlevel', hlevel2, contains_undef=.true., reverse_third_dim=.true.)
!##################################################################

!##################################################################
!     i=nx/2
!     j=ny/2
!     do k=nk,1,-1
!	write(iulog,fmt='(''    k,hlayer,hlevel:'',i3,2f8.0)')
!    +			    k,hlayer2(i,j,k),hlevel2(i,j,k)
!     end do
!##################################################################

  return
end subroutine compheight
end module compheightML
