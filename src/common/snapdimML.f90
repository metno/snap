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

!> all parameter statements (model dimensions etc.)
module snapdimML
    implicit none
    private

!> default size of ::nx (when input.gridsize not given)
    integer, parameter :: nxpre = 864
!> default size of ::ny (when input.gridsize not given)
    integer, parameter :: nypre = 698
!> default size of ::nk (when input.levels not given)
    integer, parameter :: nkpre = 61

!> horizontal field dimensions (for computations)
    integer, save, public :: nx = nxpre
!> horizontal field dimensions (for computations)
    integer, save, public :: ny = nypre
!> number of levels
    integer, save, public :: nk = nkpre

!> max no. of hourly precipitation fields
!>
!> (and then the maximum hours between field input...)
    integer, parameter, public :: mprecip = 12

!> max. input field size (possibly larger than nx*ny)
    integer, save, public :: maxsiz
!> length of buffer for field input/output
    integer, save, public :: ldata


!> SSV ARGOS
!>
!> disable SSV ARGOS output (set to 1)
!>
!> enable SSV ARGOS output, set maximum grid sizes (set to nx*ny)
    integer, parameter, public :: mxyargos = 1


!> max no. of components used in one run
!>
!> (keep as small as "possible", it dimensions 2d/3d output fields)
    integer, parameter, public :: mcomp=51

!> max. no. of available timesteps with data
    integer, parameter, public :: mavail = 8192

end module snapdimML
