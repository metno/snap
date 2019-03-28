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

module om2edotML
  implicit none
  private

  public om2edot

  contains

subroutine om2edot
  USE snapgrdML
  USE snapfldML
  USE snapdimML, only: nx,ny,nk
  USE edcompML, only: edcomp
  USE snapdebug, only: iulog
!  Purpose:  Convert omega (in model levels) to etadot,
!            (added in order to use ECMWF model data).

!  Method:   etadot = omega * d(eta)/d(p)
!	     WARNING: Comparisons using Hirlam etadot and omega output,
!		      does not show very good results.
!		      But the complete mixing of the boundary layer
!		      in Snap at least reduces the bad impact.
!            This method is used if less than half of the model levels
!            are present or if the upper level is missing.

!	     NOTE: As not all model levels may be present, it is not
!		   always possible to use the continuity equation and
!                  perhaps a better method as found in
!		   Jan Erik Haugen's Hirlam routine EDCOMP.
!                  If this was possible a mean of the method here
!		   and EDCOMP could be used.
!		   Comparisons using Hirlam etadot,u,v,ps output,
!		   does not show very good results.
!		   If you try to use EDCOMP, be aware of the level
!		   order and surface level here!

!          UPDATE: A simplified version of EDCOMP is used if all model
!                  levels are present.
!                  The used etadot values are then the mean of the two
!                  methods.

!          UPDATE: Even using EDCOMP when some levels are missing,
!                  More than half of the model levels and the upper
!                  level are required.

  implicit none


  real :: xmd2h(nx,ny),ymd2h(nx,ny)

  integer :: i,j,k,kk,km
  real ::    deta,omega,p1,p2,d2hx,d2hy

  write(iulog,*) 'OM2EDOT'

  do k=2,nk-kadd
  
    deta=vhalf(k-1)-vhalf(k)
  
    do j=1,ny
      do i=1,nx
        omega=w2(i,j,k)
        p1=ahalf(k)+bhalf(k)*ps2(i,j)
        p2=ahalf(k-1)+bhalf(k-1)*ps2(i,j)
        w2(i,j,k)=omega*deta/(p2-p1)
      end do
    end do
  
  end do

!..check if required model levels are present for subr. edcomp

!..no. of present levels
  kk=(nk-kadd)-1
!..assuming that the lower model level is always present above surface level
  km=klevel(2)

  if(kk > km/2 .AND. kadd == 0 .AND. klevel(nk) == 1) then
  
    d2hx=1./(gparam(7)*2.)
    d2hy=1./(gparam(8)*2.)
    do j=1,ny
      do i=1,nx
        xmd2h(i,j)=xm(i,j)*d2hx
        ymd2h(i,j)=ym(i,j)*d2hy
      end do
    end do
  
    write(iulog,*) 'OM2EDOT call EDCOMP'
  
    k=2
    call edcomp(nx,ny,kk,u2(:,:,k),v2(:,:,k),w2(:,:,k),ps2(:,:), &
    xmd2h,ymd2h,ahalf(:),bhalf(:),vhalf(:), &
    field1,field2,field3,field4)
  
  end if

  return
end subroutine om2edot
end module om2edotML
