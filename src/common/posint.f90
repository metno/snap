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

module posintML
  implicit none
  private

  public posint, posint_init

  real, save :: vminprec = -1.
  real, parameter :: precmin = 0.01

  contains

!> initialisation function for ::posint
subroutine posint_init()
  USE snapgrdML, only: alevel, blevel, vlevel
  USE snapdimML, only: nk
  USE snapdebug, only: iulog

  integer :: k
  real :: p1,p2
  real, parameter :: plim = 550.0

  if(vminprec < 0.) then
    p2 = 1000.
    k = 1
    do while (p2 > plim .AND. k < nk)
      k = k+1
      p1 = p2
      p2 = alevel(k) + blevel(k)*1000.
    end do
    if(k > 1) then
      vminprec = vlevel(k-1) &
          + (vlevel(k)-vlevel(k-1))*(p1-plim)/(p1-p2)
    else
      vminprec = vlevel(nk)
    end if
    write(iulog,*) 'POSINT. precmin,vminprec: ',precmin,vminprec
  end if
end subroutine

!> Purpose:  Interpolation of boundary layer top and height
!>           and precipitation intensity to particle positions
!>
!> must call ::posint_init first
subroutine posint(part,tf1,tf2,tnow,pextra)
  USE particleML, only: Particle, extraParticle
  USE snapgrdML, only: gparam
  USE snapfldML, only: xm, ym, bl1, bl2, hbl1, hbl2, precip

!> particle
!> (with all particles at the same horizontal position)
  type(Particle), intent(inout) :: part
!> time in seconds for field set 1 (e.g. 0.)
  real, intent(in) ::    tf1
!> time in seconds for field set 2 (e.g. 21600, if 6 hours)
  real, intent(in) :: tf2
!> time in seconds for current paricle positions
  real, intent(in) :: tnow
!> extra information interpolated to the particle
!> position, mainly rmx/rmy/prc
  type(extraParticle), intent(out) :: pextra

  integer :: i,j
  real :: rt1,rt2,dxgrid,dygrid,dx,dy,c1,c2,c3,c4,bl,hbl,rmx,rmy
  real :: pr

  if (part%is_inactive()) then
    ! No need to compute any properties on the particle
    return
  endif

!..for linear interpolation in time
  rt1=(tf2-tnow)/(tf2-tf1)
  rt2=(tnow-tf1)/(tf2-tf1)

  dxgrid=gparam(7)
  dygrid=gparam(8)

  !..for horizontal interpolations
  i=part%x
  j=part%y
  dx=part%x-i
  dy=part%y-j
  c1=(1.-dy)*(1.-dx)
  c2=(1.-dy)*dx
  c3=dy*(1.-dx)
  c4=dy*dx

 !..interpolation

  !..top of boundary layer
  bl= rt1*(c1*bl1(i,j)  +c2*bl1(i+1,j) &
      +c3*bl1(i,j+1)+c4*bl1(i+1,j+1)) &
      +rt2*(c1*bl2(i,j)  +c2*bl2(i+1,j) &
      +c3*bl2(i,j+1)+c4*bl2(i+1,j+1))
  !..height of boundary layer
  hbl= rt1*(c1*hbl1(i,j)  +c2*hbl1(i+1,j) &
      +c3*hbl1(i,j+1)+c4*hbl1(i+1,j+1)) &
      +rt2*(c1*hbl2(i,j)  +c2*hbl2(i+1,j) &
      +c3*hbl2(i,j+1)+c4*hbl2(i+1,j+1))

  !..map ratio
  rmx= c1*xm(i,j)  +c2*xm(i+1,j) &
      +c3*xm(i,j+1)+c4*xm(i+1,j+1)
  rmy= c1*ym(i,j)  +c2*ym(i+1,j) &
      +c3*ym(i,j+1)+c4*ym(i+1,j+1)

  !..precipitation intensity (mm/hour)
  pr= c1*precip(i,j)+c2*precip(i+1,j) &
      +c3*precip(i,j+1)+c4*precip(i+1,j+1)

  !..update boundary layer top and height, map ratio and precipitation
  part%tbl=bl
  part%hbl=hbl
  pextra%rmx=rmx/dxgrid
  pextra%rmy=rmy/dygrid
  pextra%prc=pr

  !..reset precipitation to zero if pressure less than approx. 550 hPa.
  !..and if less than a minimum precipitation intensity (mm/hour)
  if(part%z < vminprec .OR. &
      pextra%prc < precmin) then
      pextra%prc=0.
  endif

  return
end subroutine posint
end module posintML
