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

!> Purpose:  Convert omega (in model levels) to etadot,
!>           (added in order to use ECMWF model data).
!>
!> Method:   etadot = omega * d(eta)/d(p)
!>
!>     WARNING: Comparisons using Hirlam etadot and omega output,
!>	      does not show very good results.
!>	      But the complete mixing of the boundary layer
!>	      in Snap at least reduces the bad impact.
!>           This method is used if less than half of the model levels
!>           are present or if the upper level is missing.
!>
!>     NOTE: As not all model levels may be present, it is not
!>	   always possible to use the continuity equation and
!>                 perhaps a better method as found in
!>	   Jan Erik Haugen's Hirlam routine EDCOMP.
!>                 If this was possible a mean of the method here
!>	   and EDCOMP could be used.
!>	   Comparisons using Hirlam etadot,u,v,ps output,
!>	   does not show very good results.
!>	   If you try to use EDCOMP, be aware of the level
!>	   order and surface level here!
!>
!>         UPDATE: A simplified version of EDCOMP is used if all model
!>                 levels are present.
!>                 The used etadot values are then the mean of the two
!>                 methods.
!>
!>         UPDATE: Even using EDCOMP when some levels are missing,
!>                 More than half of the model levels and the upper
!>                 level are required.
subroutine om2edot
  USE snapgrdML, only: ahalf, bhalf, vhalf, klevel, gparam
  USE snapfldML, only: xm, ym, ps2, u2, v2, w2, field1, field2, field3, field4
  USE snapdimML, only: nx,ny,nk
  USE snapdebug, only: iulog

  real :: xmd2h(nx,ny),ymd2h(nx,ny)

  integer :: i,j,k,kk,km
  real ::    deta,omega,p1,p2,d2hx,d2hy

  write(iulog,*) 'OM2EDOT'

  do k=2,nk

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
  kk=(nk)-1
!..assuming that the lower model level is always present above surface level
  km=klevel(2)

  if(kk > km/2 .AND. klevel(nk) == 1) then

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
end subroutine om2edot

!> compute etadot (in full levels) from u,v and ps
subroutine edcomp(nx,ny,nz,u,v,edot,ps,xmd2h,ymd2h, &
    ahalf,bhalf,vhalf, &
    uu,vv,dpsdt,edoth)

  integer :: nx,ny,nz
  real ::    u(nx,ny,nz),v(nx,ny,nz),edot(nx,ny,nz)
  real ::    ps(nx,ny),xmd2h(nx,ny),ymd2h(nx,ny)
  real ::    ahalf(nz+1),bhalf(nz+1),vhalf(nz+1)
  real ::    uu(nx,ny),vv(nx,ny),dpsdt(nx,ny),edoth(nx,ny)

  integer :: i,j,k
  real ::    da,db,dp,deta,div,edothu,etadot

!..computation of surface pressure tendency.............................

  uu = 0.0
  vv = 0.0

  do k=1,nz

    da=ahalf(k)-ahalf(k+1)
    db=bhalf(k)-bhalf(k+1)

    do j=1,ny
      do i=1,nx
        dp=da+db*ps(i,j)
        uu(i,j) = uu(i,j) - u(i,j,k)*dp
        vv(i,j) = vv(i,j) - v(i,j,k)*dp
      end do
    end do

  !.......end do k=1,nz
  end do

  do j=2,ny-1
    do i=2,nx-1
      dpsdt(i,j) = (uu(i+1,j)-uu(i-1,j))*xmd2h(i,j) &
          +(vv(i,j+1)-vv(i,j-1))*ymd2h(i,j)
    end do
  end do

!..vertical integration of the continuity equation......................

  edoth = 0.0

  do k=1,nz-1

    da = ahalf(k) - ahalf(k+1)
    db = bhalf(k) - bhalf(k+1)
    deta = vhalf(k) - vhalf(k+1)

    do j=1,ny
      do i=1,nx
        dp = da + db*ps(i,j)
        uu(i,j) = dp*u(i,j,k)
        vv(i,j) = dp*v(i,j,k)
      end do
    end do

    do j=2,ny-1
      do i=2,nx-1
      !..divergence
        div= (uu(i+1,j)-uu(i-1,j))*xmd2h(i,j) &
            +(vv(i,j+1)-vv(i,j-1))*ymd2h(i,j)
      !..etadot in half level below full level k
        edothu = edoth(i,j)
      !..etadot in half level above full level k
        edoth(i,j) = edoth(i,j) + db*dpsdt(i,j) + div
      !..etadot in full level k (mean of the two previous)
        etadot = (edothu+edoth(i,j))*0.5
      !..etadot * dp/deta -> etadot
        dp = da + db*ps(i,j)
        etadot = etadot*deta/dp
      !..and then mean with the input value
        edot(i,j,k) = (edot(i,j,k) + etadot)*0.5
      end do
    end do

  !.......end do k=nz,2,-1
  end do

!..etadot in upper full level
  da = ahalf(nz) - ahalf(nz+1)
  db = bhalf(nz) - bhalf(nz+1)
  deta = vhalf(nz) - vhalf(nz+1)
  do j=2,ny-1
    do i=2,nx-1
      dp = da + db*ps(i,j)
      edot(i,j,nz) = (edot(i,j,nz)+0.5*edoth(i,j)*deta/dp)*0.5
    end do
  end do

  do k=1,nz
    ! Halving the edges
    edot(:,1,k) = edot(:,1,k)*0.5
    edot(:,ny,k) = edot(:,ny,k)*0.5

    ! Skipping the corners
    edot(1,2:ny-1,k) = edot(1,2:ny-1,k)*0.5
    edot(nx,2:ny-1,k) = edot(nx,2:ny-1,k)*0.5
  end do

end subroutine edcomp
end module om2edotML
