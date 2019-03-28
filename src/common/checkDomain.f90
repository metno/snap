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

module checkDomainML
  implicit none
  private

  public checkDomain

  contains

subroutine checkDomain(np)
  USE snapgrdML
  USE snapdimML, only: nx,ny,nk
! Purpose:
!    check if particle is inside domain (set active = .false. if outside)
!    move particle to lowest (highest) level if below lowest (highest) level

  use particleML
  implicit none

  INTEGER, INTENT(IN) :: np
  REAL :: vmin, vmax


  if (pdata(np)%x < 1. .OR. pdata(np)%y < 1. .OR. &
  pdata(np)%x > nx .OR. pdata(np)%y > ny) then
    pdata(np)%active = .FALSE. 
  else
    vmin=vlevel(nk)
    vmax=vlevel( 1)
    if (pdata(np)%z > vmax) then
      pdata(np)%z = vmax
    else if (pdata(np)%z < vmin) then
      pdata(np)%z = vmin
    end if
  end if
  return

end subroutine checkDomain
end module checkDomainML
