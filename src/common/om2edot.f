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
      subroutine om2edot
c
c  Purpose:  Convert omega (in model levels) to etadot,
c            (added in order to use ECMWF model data).
c
c  Method:   etadot = omega * d(eta)/d(p)
c	     WARNING: Comparisons using Hirlam etadot and omega output,
c		      does not show very good results.
c		      But the complete mixing of the boundary layer
c		      in Snap at least reduces the bad impact.
c            This method is used if less than half of the model levels
c            are present or if the upper level is missing.
c
c	     NOTE: As not all model levels may be present, it is not
c		   always possible to use the continuity equation and
c                  perhaps a better method as found in
c		   Jan Erik Haugen's Hirlam routine EDCOMP.
c                  If this was possible a mean of the method here
c		   and EDCOMP could be used.
c		   Comparisons using Hirlam etadot,u,v,ps output,
c		   does not show very good results.
c		   If you try to use EDCOMP, be aware of the level
c		   order and surface level here!
c
c          UPDATE: A simplified version of EDCOMP is used if all model
c                  levels are present.
c                  The used etadot values are then the mean of the two
c                  methods.
c
c          UPDATE: Even using EDCOMP when some levels are missing,
c                  More than half of the model levels and the upper
c                  level are required.
c
      implicit none
c
      include 'snapdim.inc'
      include 'snapgrd.inc'
      include 'snapfld.inc'
c
      real xmd2h(nx,ny),ymd2h(nx,ny)
c
      integer i,j,k,kk,km
      real    deta,omega,p1,p2,d2hx,d2hy
c
      write(9,*) 'OM2EDOT'
c
      do k=2,nk-kadd
c
       deta=vhalf(k-1)-vhalf(k)
c
       do j=1,ny
         do i=1,nx
           omega=w2(i,j,k)
           p1=ahalf(k)+bhalf(k)*ps2(i,j)
           p2=ahalf(k-1)+bhalf(k-1)*ps2(i,j)
           w2(i,j,k)=omega*deta/(p2-p1)
         end do
       end do
c
      end do
c
c..check if required model levels are present for subr. edcomp
c
c..no. of present levels
      kk=(nk-kadd)-1
c..assuming that the lower model level is always present above surface level
      km=klevel(2)
c
      if(kk.gt.km/2 .and. kadd.eq.0 .and. klevel(nk).eq.1) then
c
       d2hx=1./(gparam(7)*2.)
       d2hy=1./(gparam(8)*2.)
       do j=1,ny
         do i=1,nx
           xmd2h(i,j)=xm(i,j)*d2hx
           ymd2h(i,j)=ym(i,j)*d2hy
         end do
       end do
c
       write(9,*) 'OM2EDOT call EDCOMP'
c
        k=2
       call edcomp(nx,ny,kk,u2(1,1,k),v2(1,1,k),w2(1,1,k),ps2(1,1),
     +		    xmd2h,ymd2h,ahalf(1),bhalf(1),vhalf(1),
     +		    field1,field2,field3,field4)
c
      end if
c
      return
      end
