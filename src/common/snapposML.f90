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
  integer, save, public :: irelpos
!> no. of release positions (in list)
  integer, save, public :: nrelpos

!> relpos(1,n): geographic latitude  for release positions
!>
!> relpos(2,n): geographic longitude for release positions
!>
!> relpos(3,n): grid x coordinat for release positions
!>
!> relpos(4,n): grid y coordinat for release positions
  real, save, public :: relpos(4,mrelpos)

!> name of release positions
  character(len=40), save, public :: relnam(mrelpos)
!> name of selected release position
  character(len=40), save, public :: srelnam

end module snapposML
