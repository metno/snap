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

module snapposML
  implicit none
  private

!> max. no. of release positions available (in list)
  integer, parameter :: mrelpos = 30
!> the chosen release position
  integer, save, public :: irelpos = 0
!> no. of release positions (in list)
  integer, save, public :: nrelpos = 0

  type, public :: release_position
!> geographic latitude for release position
    real :: geo_latitude
!> geographic longitude for release position
    real :: geo_longitude
!> grid x coordinate for release position
    real :: grid_x
!> grid y coordinate for release positions
    real :: grid_y
!> name of release positions
    character(len=40) :: name
  end type

  type(release_position), save, public :: release_positions(mrelpos)

end module snapposML
