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

module epinterpML
  implicit none
  private

  public epinterp

  contains

!> simple bilinear interpolation
subroutine epinterp(nxf,nyf,field,npos,xpos,ypos,zpos,inside)
  integer, intent(in) :: nxf,nyf,npos
  real, intent(in) :: field(nxf,nyf)
  real, intent(in) :: xpos(npos),ypos(npos)
  real, intent(out) :: zpos(npos)
  integer, intent(in) :: inside(npos)

  real, parameter :: undef = +1.0e+35

  integer :: n,i,j
  real :: dx,dy,c1,c2,c3,c4

  do n=1,npos
    if(inside(n) == 1) then
      i = int(xpos(n))
      j = int(ypos(n))
      dx = xpos(n) - i
      dy = ypos(n) - j
      c1 = (1.0 - dy)*(1.0 - dx)
      c2 = (1.0 - dy)*dx
      c3 = dy*(1.0 - dx)
      c4 = dy*dx
      zpos(n) = c1*field(i,j) + c2*field(i+1,j) &
          + c3*field(i,j+1) + c4*field(i+1,j+1)
    else
      zpos(n) = undef
    end if
  end do

  return
end subroutine epinterp
end module epinterpML
